####### IMPORTS ########
import argparse
import pygame

from config import DELAY_INTERVAL
from cpu import Chip8CPU
from screen import Chip8Screen

# C O N S T A N T S ###########################################################

# A simple timer event used for the delay and sound timers
TIMER = pygame.USEREVENT + 1

# F U N C T I O N S  ##########################################################


def parse_arguments():
    """
    Parses the command-line arguments passed to the emulator.

    :return: the parsed command-line arguments
    """
    parser = argparse.ArgumentParser(
        description="Starts a simple Chip 8 "
        "emulator. See README.md for more information, and LICENSE for "
        "terms of use.")
    parser.add_argument(
        "rom", help="the ROM file to load on startup")
    parser.add_argument(
        "-s", help="the scale factor to apply to the display "
        "(default is 5)", type=int, default=5, dest="scale")
    parser.add_argument(
        "-d", help="sets the CPU operation to take at least "
        "the specified number of milliseconds to execute (default is 1)",
        type=int, default=1, dest="op_delay")
    return parser.parse_args()


def main_loop(args):
    """
    Runs the main emulator loop with the specified arguments.

    :param args: the parsed command-line arguments
    """
    screen = Chip8Screen()
    screen.init_display()
    cpu = Chip8CPU(screen)
    cpu.load_rom(args.rom)
    pygame.time.set_timer(TIMER, DELAY_INTERVAL)

    while cpu.running:
        pygame.time.wait(args.op_delay)
        cpu.execute_opcode()

        # Check for events
        for event in pygame.event.get():
            if event.type == TIMER:
                cpu.decrement_timers()
            if event.type == pygame.QUIT:
                cpu.running = False
            if event.type == pygame.KEYDOWN:
                keys_pressed = pygame.key.get_pressed()
                if keys_pressed[pygame.K_ESCAPE]:
                    cpu.running = False




if __name__ == "__main__":
    main_loop(parse_arguments())
