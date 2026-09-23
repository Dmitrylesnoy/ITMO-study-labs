#include "tm1637.h"

/*
 * Драйвер TM1637 для STM32 Nucleo.
 * CLK = PA6, DIO = PA7.
 *
 * В калькуляторе дисплей обновляется по событию ввода, поэтому
 * периодический tm1637_update() и counter драйверу не нужны.
 */

static const uint8_t digit_codes[10] = {
  0x3F, // 0
  0x06, // 1
  0x5B, // 2
  0x4F, // 3
  0x66, // 4
  0x6D, // 5
  0x7D, // 6
  0x07, // 7
  0x7F, // 8
  0x6F  // 9
};

void delay_us(uint32_t us) {
  // При 48 МГц. Для Wokwi достаточно программной задержки.
  volatile uint32_t cycles = us * 24U;
  while (cycles-- > 0U) {
    __asm__("nop");
  }
}

void tm1637_start(void) {
  // START: при CLK=1 перевести DIO 1 -> 0
  GPIOA->BSRR = (1U << TM1637_DIO_PIN) | (1U << TM1637_CLK_PIN);
  delay_us(2);

  GPIOA->BRR = (1U << TM1637_DIO_PIN);
  delay_us(2);

  GPIOA->BRR = (1U << TM1637_CLK_PIN);
  delay_us(2);
}

void tm1637_stop(void) {
  // STOP: при CLK=1 перевести DIO 0 -> 1
  GPIOA->BRR = (1U << TM1637_CLK_PIN);
  GPIOA->BRR = (1U << TM1637_DIO_PIN);
  delay_us(2);

  GPIOA->BSRR = (1U << TM1637_CLK_PIN);
  delay_us(2);

  GPIOA->BSRR = (1U << TM1637_DIO_PIN);
  delay_us(2);
}

void tm1637_write_byte(uint8_t byte) {
  // TM1637 принимает младший бит первым.
  for (uint8_t i = 0; i < 8; i++) {
    GPIOA->BRR = (1U << TM1637_CLK_PIN);

    if (byte & 0x01U) {
      GPIOA->BSRR = (1U << TM1637_DIO_PIN);
    } else {
      GPIOA->BRR = (1U << TM1637_DIO_PIN);
    }

    delay_us(2);

    GPIOA->BSRR = (1U << TM1637_CLK_PIN);
    delay_us(2);

    byte >>= 1;
  }

  /*
   * Так как DIO настроен как open-drain, запись 1 отпускает линию.
   * На девятом такте TM1637 формирует ACK.
   * Для данного проекта ACK не считывается.
   */
  GPIOA->BRR = (1U << TM1637_CLK_PIN);
  GPIOA->BSRR = (1U << TM1637_DIO_PIN);
  delay_us(2);

  GPIOA->BSRR = (1U << TM1637_CLK_PIN);
  delay_us(2);

  GPIOA->BRR = (1U << TM1637_CLK_PIN);
  delay_us(2);
}

static void tm1637_write_segments(const uint8_t segments[4]) {
  // Режим автоматического увеличения адреса
  tm1637_start();
  tm1637_write_byte(TM1637_CMD_DATA_AUTO);
  tm1637_stop();

  // Запись четырёх разрядов начиная с адреса 0
  tm1637_start();
  tm1637_write_byte(TM1637_CMD_ADDR);

  for (uint8_t i = 0; i < 4; i++) {
    tm1637_write_byte(segments[i]);
  }

  tm1637_stop();

  // Включить дисплей, максимальная яркость
  tm1637_start();
  tm1637_write_byte(TM1637_CMD_DISPLAY);
  tm1637_stop();
}

void tm1637_clear(void) {
  const uint8_t empty[4] = {0, 0, 0, 0};
  tm1637_write_segments(empty);
}

void tm1637_init(void) {
  /*
   * PA6 и PA7: general-purpose output, open-drain.
   * Тактирование GPIOA включается в initGPIO() до вызова этой функции.
   */
  GPIOA->MODER &= ~((3U << (TM1637_CLK_PIN * 2)) |
                    (3U << (TM1637_DIO_PIN * 2)));
  GPIOA->MODER |=  ((1U << (TM1637_CLK_PIN * 2)) |
                    (1U << (TM1637_DIO_PIN * 2)));

  GPIOA->OTYPER |= (1U << TM1637_CLK_PIN) |
                    (1U << TM1637_DIO_PIN);

  GPIOA->OSPEEDR |= (3U << (TM1637_CLK_PIN * 2)) |
                     (3U << (TM1637_DIO_PIN * 2));

  // Отпускаем обе линии.
  GPIOA->BSRR = (1U << TM1637_CLK_PIN) |
                (1U << TM1637_DIO_PIN);

  tm1637_clear();
  tm1637_display_number(0);
}

void tm1637_display_number(int number) {
  uint8_t segments[4] = {0, 0, 0, 0};
  uint32_t value;
  uint8_t pos = 0;
  uint8_t used_digits = 1;

  /*
   * На четырёх разрядах:
   *  0 ... 9999
   * -1 ... -999
   */
  if (number > 9999 || number < -999) {
    tm1637_display_error();
    return;
  }

  if (number < 0) {
    value = (uint32_t)(-number);
  } else {
    value = (uint32_t)number;
  }

  // TM1637 address 0 соответствует левому разряду.
  // Сначала формируем число справа налево в segments[].
  segments[3] = digit_codes[value % 10U];
  value /= 10U;

  while (value > 0U && used_digits < 4U) {
    segments[3U - used_digits] = digit_codes[value % 10U];
    value /= 10U;
    used_digits++;
  }

  if (number < 0) {
    pos = (uint8_t)(3U - used_digits);
    segments[pos] = 0x40; // '-'
  }

  tm1637_write_segments(segments);
}

void tm1637_display_error(void) {
  /*
   * Приближённая надпись "Err":
   * E = 0x79, r = 0x50.
   * Первый разряд оставляем пустым.
   */
  const uint8_t error_segments[4] = {
    0x00, 0x79, 0x50, 0x50
  };

  tm1637_write_segments(error_segments);
}
