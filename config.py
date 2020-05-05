####### IMPORTS ########

import pygame

####### CONSTANTS ########

MAX_MEMORY = 4096  # 4KB

PROGRAM_COUNTER_START = 0x200  # 512th byte

WIDTH = 64
HEIGHT = 32

SCALE_FACTOR = 5
SCALED_WIDTH = WIDTH * SCALE_FACTOR
SCALED_HEIGHT = HEIGHT * SCALE_FACTOR

KEY_MAPPINGS = {
    0x0: pygame.K_g,
    0x1: pygame.K_4,
    0x2: pygame.K_5,
    0x3: pygame.K_6,
    0x4: pygame.K_7,
    0x5: pygame.K_r,
    0x6: pygame.K_t,
    0x7: pygame.K_y,
    0x8: pygame.K_u,
    0x9: pygame.K_f,
    0xA: pygame.K_h,
    0xB: pygame.K_j,
    0xC: pygame.K_v,
    0xD: pygame.K_b,
    0xE: pygame.K_n,
    0xF: pygame.K_m,
}

DELAY_INTERVAL = 17