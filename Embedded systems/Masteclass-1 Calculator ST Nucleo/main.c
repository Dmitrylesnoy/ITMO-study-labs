#include "main.h"
#include "keyboard.h"
#include "tm1637.h"
#include <stdbool.h>

volatile uint32_t tickCount = 0;
uint32_t last_display_update = 0;
int counter = 0;

char lastKey = '\0';
uint32_t lastScanTime = 0;
static char prevProcessedKey = '\0';

typedef enum {
  STATE_ENTER_NUM1,
  STATE_ENTER_NUM2,
  STATE_SHOW_RESULT
} CalcState;

static CalcState currentState = STATE_ENTER_NUM1;
static int num1 = 0;
static int num2 = 0;
static bool isNum1Started = false;
static bool isNum2Started = false;
static char operation = '\0';

void osSystickHandler(void) {
  tickCount++;
}

void initGPIO(void) {
  RCC->AHBENR |= RCC_AHBENR_GPIOAEN | RCC_AHBENR_GPIOBEN;

  GPIOA->MODER = (GPIOA->MODER & ~(3U << (5 * 2))) | (1U << (5 * 2));
  GPIOA->OTYPER &= ~(1U << 5);
  GPIOA->ODR &= ~(1U << 5);
}

void initUSART2(void) {
  RCC->APB1ENR |= RCC_APB1ENR_USART2EN;

  GPIOA->MODER = (GPIOA->MODER & ~(0xFU << 4)) | (0xAU << 4);
  GPIOA->AFR[0] =
      (GPIOA->AFR[0] & ~(0xFFU << 8)) | (1U << 8) | (1U << 12);

  USART2->BRR = 417;
  USART2->CR1 = USART_CR1_TE | USART_CR1_UE;
}

void initSysTick(void) {
  SysTick->LOAD = 47999; // 1 мс при 48 МГц
  SysTick->VAL = 0;
  SysTick->CTRL = (1U << 2) | (1U << 1) | (1U << 0);
}

int _write(int file, uint8_t *ptr, int len) {
  (void)file;
  for (int i = 0; i < len; i++) {
    while (!(USART2->ISR & USART_ISR_TXE));
    USART2->TDR = ptr[i];
  }
  return len;
}

static void resetCalculator(void) {
  num1 = 0;
  num2 = 0;
  isNum1Started = false;
  isNum2Started = false;
  operation = '\0';
  currentState = STATE_ENTER_NUM1;
  counter = 0;
  tm1637_display_number(0);
  printf("[Clear]\n");
}

static void processKey(char key) {
  if (key == 'C') {
    resetCalculator();
    return;
  }

  // Цифры
  if (key >= '0' && key <= '9') {
    int digit = key - '0';

    // После результата новая цифра начинает новое выражение
    if (currentState == STATE_SHOW_RESULT) {
      resetCalculator();
    }

    if (currentState == STATE_ENTER_NUM1) {
      if (!isNum1Started) {
        num1 = digit;
        isNum1Started = true;
      } else if (num1 < 100) {
        num1 = num1 * 10 + digit; // максимум 3 цифры
      }

      counter = num1;
      tm1637_display_number(num1);
      printf("Num1 = %d\n", num1);
    } else if (currentState == STATE_ENTER_NUM2) {
      if (!isNum2Started) {
        num2 = digit;
        isNum2Started = true;
      } else if (num2 < 100) {
        num2 = num2 * 10 + digit; // максимум 3 цифры
      }

      counter = num2;
      tm1637_display_number(num2);
      printf("Num2 = %d\n", num2);
    }
    return;
  }

  // Операция выбирается отдельной кнопкой + - * /
  if (key == '+' || key == '-' || key == '*' || key == '/') {
    if (isNum1Started) {
      operation = key;
      currentState = STATE_ENTER_NUM2;
      num2 = 0;
      isNum2Started = false;
      printf("Operation = %c\n", operation);
    }
    return;
  }

  // Вычисление по =
  if (key == '=') {
    if (currentState != STATE_ENTER_NUM2 ||
        !isNum1Started || !isNum2Started || operation == '\0') {
      return;
    }

    int result = 0;
    bool error = false;

    switch (operation) {
      case '+':
        result = num1 + num2;
        break;
      case '-':
        result = num1 - num2;
        break;
      case '*':
        result = num1 * num2;
        break;
      case '/':
        if (num2 == 0) {
          error = true;
        } else {
          result = num1 / num2; // целочисленное деление
        }
        break;
    }

    if (error) {
      printf("ERROR: division by zero\n");
      tm1637_display_error();
    } else if (result < -999 || result > 9999) {
      printf("ERROR: result does not fit display: %d\n", result);
      tm1637_display_error();
    } else {
      counter = result;
      tm1637_display_number(result);
      printf("%d %c %d = %d\n", num1, operation, num2, result);
    }

    currentState = STATE_SHOW_RESULT;
  }
}

static void handleKeyboardInput(void) {
  if (lastKey != prevProcessedKey) {
    if (lastKey != '\0') {
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

  printf("=== Calculator started ===\n");
  printf("Keys: 0-9, +, -, *, /, C, =\n");

  while (1) {
    scanKeyboard();
    handleKeyboardInput();
  }

  return 0;
}
