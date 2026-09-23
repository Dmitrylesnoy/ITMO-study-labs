#include "keyboard.h"

void initKeyboard(void) {
  // R1-R4: PB7, PB6, PA10, PB3 - выходы с открытым стоком
  GPIOB->MODER = (GPIOB->MODER & ~(3U << (7 * 2))) | (1U << (7 * 2));
  GPIOB->MODER = (GPIOB->MODER & ~(3U << (6 * 2))) | (1U << (6 * 2));
  GPIOA->MODER = (GPIOA->MODER & ~(3U << (10 * 2))) | (1U << (10 * 2));
  GPIOB->MODER = (GPIOB->MODER & ~(3U << (3 * 2))) | (1U << (3 * 2));

  GPIOB->OTYPER |= (1U << 7) | (1U << 6) | (1U << 3);
  GPIOA->OTYPER |= (1U << 10);

  // Строки в неактивное состояние (лог. 1)
  GPIOB->BSRR = (1U << 7) | (1U << 6) | (1U << 3);
  GPIOA->BSRR = (1U << 10);

  // C1-C3: PB10, PB4, PB5 - входы с pull-up
  GPIOB->MODER &= ~((3U << (10 * 2)) | (3U << (4 * 2)) | (3U << (5 * 2)));
  GPIOB->PUPDR =
      (GPIOB->PUPDR & ~((3U << (10 * 2)) | (3U << (4 * 2)) | (3U << (5 * 2)))) |
      (1U << (10 * 2)) | (1U << (4 * 2)) | (1U << (5 * 2));

  // C4: PA15 - вход с pull-up
  GPIOA->MODER &= ~(3U << (15 * 2));
  GPIOA->PUPDR = (GPIOA->PUPDR & ~(3U << (15 * 2))) | (1U << (15 * 2));

  lastKey = '\0';
  lastScanTime = 0;
}

char readKey(void) {
  // Слева направо, сверху вниз как в diagram.json:
  // 1 2 3 +
  // 4 5 6 -
  // 7 8 9 *
  // C 0 = /
  static const char keymap[4][4] = {
    {'1', '2', '3', '+'},
    {'4', '5', '6', '-'},
    {'7', '8', '9', '*'},
    {'C', '0', '=', '/'}
  };

  for (uint8_t row = 0; row < 4; row++) {
    // Активируем одну строку (лог. 0)
    switch (row) {
      case 0: GPIOB->BRR = (1U << 7);  break; // R1 PB7
      case 1: GPIOB->BRR = (1U << 6);  break; // R2 PB6
      case 2: GPIOA->BRR = (1U << 10); break; // R3 PA10
      case 3: GPIOB->BRR = (1U << 3);  break; // R4 PB3
    }

    for (volatile int d = 0; d < 100; d++);

    // C1 PB10
    if ((GPIOB->IDR & (1U << 10)) == 0) {
      switch (row) {
        case 0: GPIOB->BSRR = (1U << 7); break;
        case 1: GPIOB->BSRR = (1U << 6); break;
        case 2: GPIOA->BSRR = (1U << 10); break;
        case 3: GPIOB->BSRR = (1U << 3); break;
      }
      return keymap[row][0];
    }

    // C2 PB4
    if ((GPIOB->IDR & (1U << 4)) == 0) {
      switch (row) {
        case 0: GPIOB->BSRR = (1U << 7); break;
        case 1: GPIOB->BSRR = (1U << 6); break;
        case 2: GPIOA->BSRR = (1U << 10); break;
        case 3: GPIOB->BSRR = (1U << 3); break;
      }
      return keymap[row][1];
    }

    // C3 PB5
    if ((GPIOB->IDR & (1U << 5)) == 0) {
      switch (row) {
        case 0: GPIOB->BSRR = (1U << 7); break;
        case 1: GPIOB->BSRR = (1U << 6); break;
        case 2: GPIOA->BSRR = (1U << 10); break;
        case 3: GPIOB->BSRR = (1U << 3); break;
      }
      return keymap[row][2];
    }

    // C4 PA15
    if ((GPIOA->IDR & (1U << 15)) == 0) {
      switch (row) {
        case 0: GPIOB->BSRR = (1U << 7); break;
        case 1: GPIOB->BSRR = (1U << 6); break;
        case 2: GPIOA->BSRR = (1U << 10); break;
        case 3: GPIOB->BSRR = (1U << 3); break;
      }
      return keymap[row][3];
    }

    // Деактивируем строку
    switch (row) {
      case 0: GPIOB->BSRR = (1U << 7);  break;
      case 1: GPIOB->BSRR = (1U << 6);  break;
      case 2: GPIOA->BSRR = (1U << 10); break;
      case 3: GPIOB->BSRR = (1U << 3);  break;
    }
  }

  return '\0';
}

void scanKeyboard(void) {
  // 50 мс достаточно для опроса и подавления дребезга
  if ((tickCount - lastScanTime) >= 50) {
    lastScanTime = tickCount;
    char currentKey = readKey();

    if (currentKey != '\0' && currentKey != lastKey) {
      lastKey = currentKey;
      printf("Pressed: %c\n", currentKey);
    } else if (currentKey == '\0') {
      lastKey = '\0';
    }
  }
}
