#include "main.h"
#include "tm1637.h"
#include <stdio.h>
#include <stdbool.h>

/* Глобальные переменные для библиотек */
volatile uint32_t tickCount = 0;
uint32_t last_display_update = 0;
uint16_t counter = 0;

char lastKey = 0;
uint32_t lastScanTime = 0;

/* Переменные калькулятора */
typedef enum {
  STATE_ENTER_NUM1,
  STATE_ENTER_NUM2,
  STATE_SHOW_RESULT
} CalcState;

CalcState currentState = STATE_ENTER_NUM1;
int num1 = 0;
int num2 = 0;
bool isNum1Started = false;
bool isNum2Started = false;

int opIndex = 0; // 0: '+', 1: '-', 2: '*', 3: '/'
const char opSymbols[] = {'+', '-', '*', '/'};
char prevProcessedKey = 0;

void osSystickHandler(void) {
  tickCount++;
}

void initGPIO(void) {
  RCC->AHBENR |= RCC_AHBENR_GPIOAEN | RCC_AHBENR_GPIOBEN;

  // Индикация статуса на PA5
  GPIOA->MODER = (GPIOA->MODER & ~(3U << (5 * 2))) | (1U << (5 * 2));
  GPIOA->ODR &= ~(1U << 5);
}

void initUSART2(void) {
  RCC->APB1ENR |= RCC_APB1ENR_USART2EN;

  GPIOA->MODER = (GPIOA->MODER & ~(0xF << 4)) | (0xA << 4);
  GPIOA->AFR[0] = (GPIOA->AFR[0] & ~(0xFF << 8)) | (1 << 8) | (1 << 12);

  USART2->BRR = 417; // 48MHz / 115200
  USART2->CR1 = USART_CR1_TE | USART_CR1_UE;
}

void initSysTick(void) {
  SysTick->LOAD = 47999; // 1 мс при 48 МГц
  SysTick->VAL = 0;
  SysTick->CTRL = (1 << 2) | (1 << 1) | (1 << 0);
}

int _write(int file, uint8_t *ptr, int len) {
  for (int i = 0; i < len; i++) {
    while (!(USART2->ISR & USART_ISR_TXE));
    USART2->TDR = ptr[i];
  }
  return len;
}

/* Логика ввода трехзначных чисел */
void processKey(char key) {
  if (key >= '0' && key <= '9') {
    int digit = key - '0';

    if (currentState == STATE_SHOW_RESULT) {
      // Сброс и ввод первого числа заново после вычисления
      num1 = digit;
      isNum1Started = true;
      num2 = 0;
      isNum2Started = false;
      opIndex = 0;
      currentState = STATE_ENTER_NUM1;
      counter = num1;
      printf("\n[Step 1] Num1 = %d\n", num1);
    }
    else if (currentState == STATE_ENTER_NUM1) {
      if (!isNum1Started) {
        num1 = digit;
        isNum1Started = true;
      } else {
        // Ограничение: не более 3 цифр (до 999)
        if (num1 < 100) {
          num1 = (num1 * 10) + digit;
        }
      }
      counter = num1;
      printf("[Step 1] Num1 = %d\n", num1);
    } 
    else if (currentState == STATE_ENTER_NUM2) {
      if (!isNum2Started) {
        num2 = digit;
        isNum2Started = true;
      } else {
        // Ограничение: не более 3 цифр (до 999)
        if (num2 < 100) {
          num2 = (num2 * 10) + digit;
        }
      }
      counter = num2;
      printf("[Step 3] Num2 = %d\n", num2);
    }
  } 
  else if (key == '*') {
    if (currentState == STATE_ENTER_NUM1 || currentState == STATE_ENTER_NUM2) {
      if (currentState == STATE_ENTER_NUM1) {
        opIndex = 0; // По умолчанию '+'
        currentState = STATE_ENTER_NUM2;
        isNum2Started = false;
      } else if (!isNum2Started) {
        // Переключение операции пока не начат ввод второго числа
        opIndex = (opIndex + 1) % 4;
      }
      printf("[Step 2] Selected OP: '%c'\n", opSymbols[opIndex]);
    }
  } 
  else if (key == '#') {
    if (currentState == STATE_ENTER_NUM2 && isNum2Started) {
      int res = 0;
      bool isError = false;

      switch (opSymbols[opIndex]) {
        case '+': res = num1 + num2; break;
        case '-': res = num1 - num2; break;
        case '*': res = num1 * num2; break;
        case '/': 
          if (num2 != 0) {
            res = num1 / num2;
          } else {
            isError = true;
          }
          break;
      }

      currentState = STATE_SHOW_RESULT;

      if (isError) {
        printf("[Result] ERROR: Division by zero!\n");
        counter = 9999; // Код ошибки 'Err'
      } else {
        printf("[Result] %d %c %d = %d\n", num1, opSymbols[opIndex], num2, res);
        counter = res;
      }
    }
  }
}

void handleKeyboardInput(void) {
  if (lastKey != prevProcessedKey) {
    if (lastKey != 0) {
      processKey(lastKey);
    }
    prevProcessedKey = lastKey;
  }
}

int main(void) {
  initGPIO();
  initUSART2();
  initSysTick();
  initKeyboard();
  tm1637_init();

  printf("=== 3-Digit Calculator App Started ===\n");
  printf("Usage: [0-999] -> [* to choose op (+,-,*,/)] -> [0-999] -> [# for result]\n");
  counter = 0;

  while (1) {
    scanKeyboard();
    handleKeyboardInput();
    tm1637_update();
  }

  return 0;
}