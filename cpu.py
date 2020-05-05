####### IMPORTS ########

import pygame
import sys
from random import randint
from config import MAX_MEMORY, PROGRAM_COUNTER_START, WIDTH, HEIGHT, SCALED_HEIGHT, SCALED_WIDTH, SCALE_FACTOR, KEY_MAPPINGS

####### CONSTANTS ########

# ROM_PATH = sys.argv[1]
FONTS = [0xF0, 0x90, 0x90, 0x90, 0xF0, # 0
         0x20, 0x60, 0x20, 0x20, 0x70, # 1
         0xF0, 0x10, 0xF0, 0x80, 0xF0, # 2
         0xF0, 0x10, 0xF0, 0x10, 0xF0, # 3
         0x90, 0x90, 0xF0, 0x10, 0x10, # 4
         0xF0, 0x80, 0xF0, 0x10, 0xF0, # 5
         0xF0, 0x80, 0xF0, 0x90, 0xF0, # 6
         0xF0, 0x10, 0x20, 0x40, 0x40, # 7
         0xF0, 0x90, 0xF0, 0x90, 0xF0, # 8
         0xF0, 0x90, 0xF0, 0x10, 0xF0, # 9
         0xF0, 0x90, 0xF0, 0x90, 0x90, # A
         0xE0, 0x90, 0xE0, 0x90, 0xE0, # B
         0xF0, 0x80, 0x80, 0x80, 0xF0, # C
         0xE0, 0x90, 0x90, 0x90, 0xE0, # D
         0xF0, 0x80, 0xF0, 0x80, 0xF0, # E
         0xF0, 0x80, 0xF0, 0x80, 0x80  # F
        ]

####### CLASSES ########

class Chip8CPU(object):

    """
    A class to emulate the Chip8 CPU.

    RESOURCE = Cowgod's legit documentation - http://devernay.free.fr/hacks/chip8/C8TECH10.HTM
    
    The Chip8 CPU consists:

        * 16 x 8-bit general purpose registers (V0 - VF)
        * 16 x 16-bit stack implemented as an array
        * 1 x 16-bit index register (I)
        * 1 x 16-bit stack pointer (SP)
        * 1 x 16-bit program counter (PC)
        * 1 x 8-bit delay timer (DT)
        * 1 x 8-bit sound timer (ST)

        Program Counter - A program counter is a register in a computer processor that contains the address (location) of the instruction being executed at the current time.
        As each instruction gets fetched, the program counter increases its stored value by 1.

        Source - http://craigthomas.ca/blog/2014/07/17/writing-a-chip-8-emulator-part-2/
        In the case of Chip8 the program counter (PC) is incremented by 2 after reading each byte from the memory.
        For example, let’s say that the program counter points at memory location 0x0200, and that the data 0x1100 is stored there
        (technically, 0x11 is stored in location 0x0200, and 0x00 is stored in location 0x0201)
        and hence the program counter (PC) is incremnted by 2.
    """

    def __init__(self, screen):

        """
        Initialize the Chip8 CPU with all the registers, program counter, stack pointer and timers.

        There are two 60 hertz timer registers, one for sound and one that is general
        purpose known as the delay timer. The timers are loaded with a value
        and then decremented 60 times per second.
        """
        self.memory = bytearray(MAX_MEMORY)
        self.registers = [0] * 16
        self.index = 0
        self.stack = [0] * 16
        self.sp = 0
        self.pc = 0
        # self.display_buffer = [0] * SCALED_WIDTH * SCALED_HEIGHT
        self.screen = screen
        self.operand = 0
        self.delay = 0
        self.sound = 0
        self.running = True

        # OPCODE/Operation Code lookup table:
        # The operation lookup table is executed based on the most significant byte of the opcode.
        # Example: 5st0 would call the skip_if_reg_equal_reg function since the most significant byte is 5.
        # Note: s and t are short for source and target. It is referred as x and y in Cowgod's documentation.
        
        self.operation_lookup = {
            0x0: self.clear_return,                  # 0nnn - SYS  nnn, has subfunctions
            0x1: self.jump_to_address,               # 1nnn - JUMP nnn
            0x2: self.jump_to_subroutine,            # 2nnn - CALL nnn
            0x3: self.skip_if_reg_equal_val,         # 3snn - SKE  Vs, nn
            0x4: self.skip_if_reg_not_equal_val,     # 4snn - SKNE Vs, nn
            0x5: self.skip_if_reg_equal_reg,         # 5st0 - SKE  Vs, Vt
            0x6: self.move_value_to_reg,             # 6snn - LOAD Vs, nn
            0x7: self.add_value_to_reg,              # 7snn - ADD  Vs, nn
            0x8: self.execute_logical_instruction,   # see subfunctions below
            0x9: self.skip_if_reg_not_equal_reg,     # 9st0 - SKNE Vs, Vt
            0xA: self.load_index_reg_with_value,     # Annn - LOAD I, nnn
            0xB: self.jump_to_index_plus_value,      # Bnnn - JUMP [I] + nnn
            0xC: self.generate_random_number,        # Ctnn - RAND Vt, nn
            0xD: self.draw_sprite,                   # Dstn - DRAW Vs, Vy, n
            0xE: self.keyboard_routines,             # see subfunctions below
            0xF: self.misc_routines,                 # see subfunctions below
        }

        # This set of operations is invoked when the operand loaded into the
        # CPU starts with 8
        
        self.logical_operation_lookup = {
            0x0: self.move_reg_into_reg,             # 8st0 - LOAD Vs, Vt
            0x1: self.logical_or,                    # 8st1 - OR   Vs, Vt
            0x2: self.logical_and,                   # 8st2 - AND  Vs, Vt
            0x3: self.exclusive_or,                  # 8st3 - XOR  Vs, Vt
            0x4: self.add_reg_to_reg,                # 8st4 - ADD  Vs, Vt
            0x5: self.subtract_reg_from_reg,         # 8st5 - SUB  Vs, Vt
            0x6: self.right_shift_reg,               # 8st6 - SHR  Vs
            0x7: self.subtract_reg_from_reg1,        # 8st7 - SUBN Vs, Vt
            0xE: self.left_shift_reg,                # 8stE - SHL  Vs
        }

        # This set of operations is invoked when the operand loaded into the
        # CPU starts with F

        self.misc_routine_lookup = {
            0x07: self.move_delay_timer_into_reg,            # Ft07 - LOAD Vt, DELAY
            0x0A: self.wait_for_keypress,                    # Ft0A - KEYD Vt
            0x15: self.move_reg_into_delay_timer,            # Fs15 - LOAD DELAY, Vs
            0x18: self.move_reg_into_sound_timer,            # Fs18 - LOAD SOUND, Vs
            0x1E: self.add_reg_into_index,                   # Fs1E - ADD  I, Vs
            0x29: self.load_index_with_reg_sprite,           # Fs29 - LOAD I, Vs
            # 0x30: self.load_index_with_extended_reg_sprite,  # Fs30 - LOAD I, Vs
            0x33: self.store_bcd_in_memory,                  # Fs33 - BCD
            0x55: self.store_regs_in_memory,                 # Fs55 - STOR [I], Vs
            0x65: self.read_regs_from_memory,                # Fs65 - LOAD Vs, [I]
            # 0x75: self.store_regs_in_rpl,                  # Fs75 - SRPL Vs
            # 0x85: self.read_regs_from_rpl,                 # Fs85 - LRPL Vs
        }
        self.reset()

    def execute_opcode(self):

        # Extract the first nibble from the OP code and look it up in the operation_tookup table.

        # self.operand = (self.memory[self.pc] << 8) #| (self.memory[self.pc+1])
        # self.operand += self.memory[self.pc+1]
        self.operand = int(self.memory[self.pc])
        self.operand = self.operand << 8
        self.operand += int(self.memory[self.pc + 1])
        self.pc += 2
        operation = (self.operand & 0xF000) >> 12
        try:
            self.operation_lookup[operation]()
        except KeyError:
            print('Unknown OPcode: {:4X}'.format(self.operand))

    def execute_logical_instruction(self):

        # OP codes starting with 8 are directed here
        
        operation = self.operand & 0x000F
        try:
            self.logical_operation_lookup[operation]()
        except KeyError:
            print('Unknown OPcode: {:4X}'.format(self.operand))

    def misc_routines(self):

        # OP codes starting with F are directed here

        operation = self.operand & 0x00FF
        try:
            self.misc_routine_lookup[operation]()
        except KeyError:
            print('Unknown OPcode: {:4X}'.format(self.operand))
            
    def clear_return(self):
        """
        0nnn - Jump to machine code at nnn (ignored).
        00E0 - Clear the display.
        00EE - Return from sun-routine - The PC is pointed to the address at the top of the stackand the value of stack pointer is decreased by 1.
        """
        if self.operand & 0x00F0 == 0x00E0:
            # self.display_buffer = [0] * HEIGHT * WIDTH
            self.screen.clear_screen()

        elif self.operand & 0x00FF == 0x00EE:
            print("Clear return; Flag")
            # self.pc = self.stack[-1]
            # self.sp -= 1

            # self.sp -= 1
            # self.pc = self.memory[self.sp] << 8
            # self.sp -= 1
            # self.pc += self.memory[self.sp]

            self.pc = self.stack.pop()

    def jump_to_address(self):
        """
        1nnn - Jump to address nnn

        Sets the value of PC to the address nnn.
        """
        self.pc = self.operand & 0x0FFF

    def jump_to_subroutine(self):
        """
        2nnn - Call subroutine at address nnn

        The current value of PC is stored on the top of the stack and the value of PC is set to nnn.
        """
        print("Jump to subroutine; Flag")
        # self.sp += 1
        # self.stack[self.sp] = self.pc
        # self.pc = self.operand & 0x0FFF

        # self.memory[self.sp] = self.pc & 0x00FF
        # self.sp += 1
        # self.memory[self.sp] = (self.pc & 0xFF00) >> 8
        # self.sp += 1
        # self.pc = self.operand & 0x0FFF

        self.stack.append(self.pc)
        self.sp += 1
        self.pc = self.operand & 0x0FFF
    
    def skip_if_reg_equal_val(self):
        """
        3skk - Skips the next OP code if Vs equals kk

        The value of Vs and kk are compared and if they are equal the value of PC is incremented by 2.
        """
        source = (self.operand & 0x0F00) >> 8
        if self.registers[source] == (self.operand & 0x00FF):
            self.pc += 2
        
    def skip_if_reg_not_equal_val(self):
        """
        4skk - Skips the next OP code if Vs and kk are not equal

        The value of Vs and kk are compared and if they are not equal the value of PC is incremented by 2.
        """
        source = (self.operand & 0x0F00) >> 8
        if self.registers[source] != (self.operand & 0x00FF):
            self.pc += 2
    
    def skip_if_reg_equal_reg(self):
        """
        5st0 - Skips the next OP code if Vs and Vt are equal

        The value of PC is increamented by 2 if the value of Vs and Vt are equal.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        if self.registers[source] == self.registers[target]:
            self.pc += 2

    def move_value_to_reg(self):
        """
        6skk - Set Vs = kk

        The value of kk is assigned to Vs.
        """
        source = (self.operand & 0x0F00) >> 8
        self.registers[source] = self.operand & 0x00FF

    def add_value_to_reg(self):
        """
        7skk - Sets Vs = Vs + kk

        Adds the value of kk to Vs and stores the resust in Vs
        """
        source = (self.operand & 0x0F00) >> 8
        self.registers[source] += (self.operand & 0x00FF)

    def move_reg_into_reg(self):
        """ 
        8ts0 - Set Vs = Vt

        Stores the value of Vs in Vt.
        """
        target = (self.operand & 0x0F00) >> 8
        source = (self.operand & 0x00F0) >> 4
        self.registers[target] = self.registers[source]

    def logical_or(self):
        """
        8ts1 - Set Vs = Vs OR Vt

        Performs a bitwise OR on Vs and Vt and stores the result in Vs.
        """
        target = (self.operand & 0x0F00) >> 8
        source = (self.operand & 0x00F0) >> 4
        self.registers[target] |= self.registers[source]

    def logical_and(self):
        """
        8ts2 - Set Vs = Vs AND Vt

        Performs a bitwise AND on Vs and Vt and stores the result in Vs.
        """
        target = (self.operand & 0x0F00) >> 8
        source = (self.operand & 0x00F0) >> 4
        self.registers[target] &= self.registers[source]

    def exclusive_or(self):
        """
        8ts3 - Set Vs = Vs XOR Vt

        Performs a bitwise exclusive OR on Vs and Vt and stores the result in Vs.
        """
        target = (self.operand & 0x0F00) >> 8
        source = (self.operand & 0x00F0) >> 4
        self.registers[target] ^= self.registers[source]

    def add_reg_to_reg(self):
        """
        8st4 - Set Vs = Vs + Vt, set VF = carry

        The values of Vx and Vy are added together. If the result is greater than 8 bits (i.e., > 255,)
        VF is set to 1, otherwise 0. Only the lowest 8 bits of the result are kept, and stored in Vx.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        temp = self.registers[source] + self.registers[target]
        if temp > 255:
            self.registers[source] = temp - 256
            self.registers[0xF] = 1
        else:
            self.registers[source] = temp
            self.registers[0xF] = 0

    def subtract_reg_from_reg(self):
        """
        8st5 - Set Vs = Vs - Vt, set VF = NOT borrow

        If Vs > Vt, then VF is set to 1, otherwise 0. Then Vt is subtracted from Vs, and the results stored in Vs.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        temp = self.registers[source] - self.registers[target]
        if self.registers[source] > self.registers[target]:
            self.registers[0xF] = 1
        else:
            self.registers[0xF] = 0
        self.registers[source] = temp
        

    def right_shift_reg(self):
        """
        8st6 - Set Vs = Vs SHR 1

        If the least-significant bit of Vs is 1, then VF is set to 1, otherwise 0. Then Vs is divided by 2.
        Here the least significant bit is the 1st bit from right(value - 1).
        Right shifting a number dicides it by 2 and left shifting it would multiply it by 2.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        least_bit = source & 0x1
        self.registers[0xF] = least_bit
        self.registers[source] = self.registers[source] >> 1

    def subtract_reg_from_reg1(self):
        """
        8st7 - Set Vs = Vt - Vs, set VF = NOT borrow

        If Vt > Vs, then VF is set to 1, otherwise 0. Then Vs is subtracted from Vt, and the results stored in Vs.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4.
        temp = self.registers[target] - self.registers[source]
        if self.registers[target] > self.registers[source]:
            self.registers[0xF] = 1
        else:
            self.registers[0xF] = 0
        self.registers[source] = temp

    def left_shift_reg(self):
        """
        8stE - Set Vs = Vs SHL 1

        If the most-significant bit of Vs is 1, then VF is set to 1, otherwise 0. Then Vs is multiplied by 2.
        Here the most significant bit is the 7th bit from right(value - 128). Vs is 8bits long.
        Right shifting a number dicides it by 2 and left shifting it would multiply it by 2.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        most_bit = (source & 0x80) >> 8
        self.registers[0xF] = most_bit
        self.registers[source] = self.registers[source] << 1

    def skip_if_reg_not_equal_reg(self):
        """
        9st0 - Skip the instruction if Vs != Vt

        The values of Vs and Vt are compared, and if they are not equal, the PC is increased by 2.
        """
        source = (self.operand & 0x0F00) >> 8
        target = (self.operand & 0x00F0) >> 4
        if self.registers[source] != self.registers[target]:
            self.pc += 2
        
    def load_index_reg_with_value(self):
        """
        Annn - Set I = nnn

        The value of register I is set to nnn. 
        """
        self.index = self.operand & 0x0FFF

    def jump_to_index_plus_value(self):
        """
        Bnnn - Jump to location nnn + V0

        The PC is set to nnn + the value of V0.
        """
        print("Jump to index plus value")
        self.pc = (self.operand & 0x0FFF) + self.registers[0]

    def generate_random_number(self):
        """
        Ctkk - Set Vt = random byte AND kk

        A random number between 0 and 255 is generated. The number is then ANDed with the
        constant value passed in the operand. The result is stored in the target register.
        """
        target = (self.operand & 0x0F00) >> 8
        constant = (self.operand & 0x00FF)
        self.registers[target] = constant & randint(0, 255)

    def draw_sprite(self):
        """
        Dxyn - Display n-byte sprite starting at memory location I at (Vx, Vy), set VF = collision

        Draws the sprite pointed to in the index register at the specified x and y coordinates.
        Drawing is done via an XOR routine, meaning that if the target pixel is already turned on,
        and a pixel is set to be turned on at that same location via the draw, then the pixel is
        turned off. The routine will wrap the pixels if they are drawn off the edge of the screen.
        Each sprite is 8 bits (1 byte) wide. The num_bytes parameter sets how tall the sprite is. 
        Consecutive bytes in the memory pointed to by the index register make up the bytes of the sprite. 
        Each bit in the sprite byte determines whether a pixel is turned on (1) or turned off (0).
        """
        # TODO - Implement this method and the next method for keyboard routines(E)
        x_source = (self.operand & 0x0F00) >> 8
        y_source = (self.operand & 0x00F0) >> 4
        x_pos = self.registers[x_source]
        y_pos = self.registers[y_source]
        num_bytes = self.operand & 0x000F
        self.registers[0xF] = 0
        self.draw_normal(x_pos, y_pos, num_bytes)

    def draw_normal(self, x_pos, y_pos, num_bytes):
        """
        Draws a sprite on the screen while in NORMAL mode.
        
        :param x_pos: the X position of the sprite
        :param y_pos: the Y position of the sprite
        :param num_bytes: the number of bytes to draw
        """
        # TODO
        for y_index in range(num_bytes):

            color_byte = bin(self.memory[self.index + y_index])
            color_byte = color_byte[2:].zfill(8)
            y_coord = y_pos + y_index
            y_coord = y_coord % self.screen.get_height()

            for x_index in range(8):

                x_coord = x_pos + x_index
                x_coord = x_coord % self.screen.get_width()

                color = int(color_byte[x_index])
                current_color = self.screen.get_pixel(x_coord, y_coord)

                if color == 1 and current_color == 1:
                    self.registers[0xF] = self.registers[0xF] | 1
                    color = 0

                elif color == 0 and current_color == 1:
                    color = 1

                self.screen.draw_pixel(x_coord, y_coord, color)

        self.screen.update()

    def keyboard_routines(self):
        """
        Run the specified keyboard routine based upon the operand. These
        operations are:

            Es9E - SKPR Vs
            EsA1 - SKUP Vs

        0x9E will check to see if the key specified in the source register is
        pressed, and if it is, skips the next instruction. Operation 0xA1 will
        again check for the specified keypress in the source register, and
        if it is NOT pressed, will skip the next instruction.
        """
        operation = self.operand & 0x00FF
        source = (self.operand & 0x0F00) >> 8

        key_to_check = self.registers[source]
        keys_pressed = pygame.key.get_pressed()

        # Skip if the key specified in the source register is pressed
        if operation == 0x9E:
            if keys_pressed[KEY_MAPPINGS[key_to_check]]:
                self.pc += 2

        # Skip if the key specified in the source register is not pressed
        if operation == 0xA1:
            if not keys_pressed[KEY_MAPPINGS[key_to_check]]:
                self.pc += 2

    def move_delay_timer_into_reg(self):
        """
        Ft07 - Set Vt = delay timer value

        The value of DT is placed into Vt.
        """
        target = (self.operand & 0x0F00) >> 8
        self.registers[target] = self.delay

    def wait_for_keypress(self):
        """
        Ft0A - Wait for a key press, store the value of the key in Vx

        All execution stops until a key is pressed, then the value of that key is stored in Vt.
        """
        target = (self.operand & 0x0F00) >> 8
        key_pressed = False
        while not key_pressed:
            event = pygame.event.wait()
            if event.type == pygame.KEYDOWN:
                keys_pressed = key.get_pressed()
                for keyval, lookup_key in KEY_MAPPINGS.items():
                    if keys_pressed[lookup_key]:
                        self.registers['v'][target] = keyval
                        key_pressed = True
                        break

        # TODO
        
    def move_reg_into_delay_timer(self):
        """
        Fs15 - Set delay timer = Vx

        DT is set equal to the value of Vx.
        """
        source = (self.operand & 0x0F00) >> 8
        self.delay = self.registers[source]

    def move_reg_into_sound_timer(self):
        """
        Fs18 - Set sound timer = Vs

        ST is set equal to the value of Vx.
        """
        source = (self.operand & 0x0F00) >> 8
        self.sound = self.registers[source]

    def add_reg_into_index(self):
        """
        Fs1E - Set I = I + Vs

        The values of I and Vx are added, and the results are stored in I.
        """
        source = (self.operand & 0x0F00) >> 8
        self.index += self.registers[source]

    def load_index_with_reg_sprite(self):
        """
        Fs29 - Set I = location of sprite for digit Vs

        The value of I is set to the location for the hexadecimal sprite corresponding to the value of Vx.
        All sprites are 5 bytes long, so the location of the specified sprite is its index multiplied by 5.
        """
        source = (self.operand & 0x0F00) >> 8
        self.index = self.registers[source] * 5
    
    def store_bcd_in_memory(self):
        """
        Fs33 - Store BCD representation of Vs in memory locations I, I+1, and I+2

        Stores the Binary-coded decimal(BCD) representation of Vs, with the
        most significant of three digits at the address in I, the middle
        digit at I plus 1, and the least significant digit at I plus 2.

        For example, if the value is 123, then the following values will be
        placed at the specified locations:

             1 -> self.memory[index]
             2 -> self.memory[index + 1]
             3 -> self.memory[index + 2]
        """
        source = (self.operand & 0x0F00) >> 8
        self.memory[self.index] = int(int(source) / 100)
        self.memory[self.index + 1] = int((int(source) / 10) % 10)
        self.memory[self.index + 2] = int(int(source) % 10)

    def store_regs_in_memory(self):
        """
        Fs55 - Store registers V0 through Vx in memory starting at location I

        Copies the values of registers V0 through Vx into memory, starting at the address in I.
        The source register contains the number of V registers to store. For example, to store 
        all of the V registers, the source register would contain the value 'F'.
        """
        source = (self.operand & 0x0F00) >> 8
        for i in range(source + 1):
            self.memory[self.index+i] = self.registers[i]

    def read_regs_from_memory(self):
        """
        Fs65 - Read registers V0 through Vx from memory starting at location I

        Reads values from memory starting at location I into registers V0 through Vs. The source register contains 
        the number of V registers to load. For example, to load all of the V registers, the source register would
        contain the value 'F'.
        """
        source = (self.operand & 0x0F00) >> 8
        for i in range(source + 1):
            self.registers[i] = self.memory[self.index+i]

    def load_rom(self,ROM_PATH):
        """
        Function to load the ROM_FILE to the memory.
        Starts loading from 0x200 (512 bytes).
        https://www.reddit.com/r/learnpython/comments/b1o0en/typeerror_ord_expected_string_of_length_1_but_int/ein23hi?utm_source=share&utm_medium=web2x
        """
        # ROM_PATH = 'D:\\Code\\Chip8 Emulator\\INVADERS'
        rom_file = open(ROM_PATH, 'rb').read()
        for i in range(len(rom_file)):
            self.memory[i+0x200] = rom_file[i]

    def decrement_timers(self):
        """
        Decrements both the sound and delay timers.
        """
        if self.delay != 0:
            self.delay-=1

        if self.sound != 0:
            self.sound -= 1

    def reset(self):
        """
        Function to reset and wipe clean all the registers, stack pointer
        and program counter and load the fonts to the CPU.
        """
        self.memory = bytearray(MAX_MEMORY)
        self.memory[0:80] = FONTS[0:80]        # Loads the font to the CPU memory
        self.registers = [0] * 16
        # self.display_buffer = [0] * SCALED_WIDTH * SCALED_HEIGHT
        self.index = 0
        self.pc = PROGRAM_COUNTER_START
        self.sp = 0
        # self.load_rom()
