/* USER CODE BEGIN Header */
/**
  ******************************************************************************
  * @file           : main.c
  * @brief          : Timer for SDK-1.1M
  ******************************************************************************
  */
/* USER CODE END Header */

/* Includes ------------------------------------------------------------------*/
#include "main.h"
#include "i2c.h"
#include "usart.h"
#include "gpio.h"
#include "tim.h"

/* Private includes ----------------------------------------------------------*/
/* USER CODE BEGIN Includes */
#include "kb.h"
#include "oled.h"
#include "fonts.h"
#include "buzzer.h"
#include <stdio.h>
/* USER CODE END Includes */

/* Private typedef -----------------------------------------------------------*/
/* USER CODE BEGIN PTD */
typedef enum
{
    TIMER_INPUT = 0,
    TIMER_RUNNING,
    TIMER_ALARM
} TimerState;
/* USER CODE END PTD */

/* Private define ------------------------------------------------------------*/
/* USER CODE BEGIN PD */
#define ALARM_TIME_MS       3000U
#define KEYBOARD_DELAY_MS     20U
#define MAX_SET_TIME       5999U
/* USER CODE END PD */

/* Private variables ---------------------------------------------------------*/
/* USER CODE BEGIN PV */
static TimerState timerState = TIMER_INPUT;

static uint32_t setTime = 0;
static uint32_t remainingTime = 0;
static uint32_t lastSecondTick = 0;
static uint32_t alarmStartTick = 0;

/* USER CODE END PV */

/* Private function prototypes -----------------------------------------------*/
void SystemClock_Config(void);

/* USER CODE BEGIN PFP */
static void ShowKeyboardLegend(void);
static void ShowInputScreen(void);
static void ShowTimerScreen(void);
static void ShowAlarmScreen(void);

static char ReadKeyboard(void);
static void ProcessKeyboard(void);

/* USER CODE END PFP */

/* Private user code ---------------------------------------------------------*/
/* USER CODE BEGIN 0 */

/*
 * Physical keyboard legend:
 *
 *      1  2  3
 *      4  5  6
 *      7  8  9
 *      *  0  #
 *
 * '*' - reset entered value
 * '#' - start timer
 *
 * ROW1 is the physical upper row of the keyboard and ROW4 is the lower row.
 * This makes the key mapping match the legend shown on the OLED:
 * 1 2 3 / 4 5 6 / 7 8 9 / * 0 #.
 */
static char ReadKeyboard(void)
{
    static const uint8_t rows[4] = {ROW1, ROW2, ROW3, ROW4};

    static const char keys[4][3] =
    {
        {'1', '2', '3'},
        {'4', '5', '6'},
        {'7', '8', '9'},
        {'*', '0', '#'}
    };

    static char lastKey = 0;
    char currentKey = 0;

    for (uint8_t row = 0; row < 4; row++)
    {
        uint8_t key = Check_Row(rows[row]);

        if (key == 0x04)
        {
            currentKey = keys[row][0];
            break;
        }
        else if (key == 0x02)
        {
            currentKey = keys[row][1];
            break;
        }
        else if (key == 0x01)
        {
            currentKey = keys[row][2];
            break;
        }
    }

    /*
     * Return a key only once per physical press.
     * A new key can be registered after all keys are released.
     */
    if (currentKey != 0)
    {
        if (lastKey == 0)
        {
            lastKey = currentKey;
            return currentKey;
        }

        return 0;
    }

    lastKey = 0;
    return 0;
}


static void ShowKeyboardLegend(void)
{
    oled_Fill(Black);

    oled_SetCursor(0, 0);
    oled_WriteString("TIMER KEYS", Font_7x10, White);

    oled_SetCursor(36, 12);
    oled_WriteString("1 2 3", Font_7x10, White);

    oled_SetCursor(36, 22);
    oled_WriteString("4 5 6", Font_7x10, White);

    oled_SetCursor(36, 32);
    oled_WriteString("7 8 9", Font_7x10, White);

    oled_SetCursor(36, 42);
    oled_WriteString("* 0 #", Font_7x10, White);

    oled_SetCursor(0, 54);
    oled_WriteString("*=CLR  #=START", Font_7x10, White);

    oled_UpdateScreen();
}


static void ShowInputScreen(void)
{
    char buffer[24];

    oled_Fill(Black);

    oled_SetCursor(0, 0);
    oled_WriteString("SET TIMER (SEC)", Font_7x10, White);

    sprintf(buffer, "%lu", (unsigned long)setTime);

    oled_SetCursor(0, 20);
    oled_WriteString(buffer, Font_11x18, White);

    oled_SetCursor(0, 44);
    oled_WriteString("* = CLEAR", Font_7x10, White);

    oled_SetCursor(0, 54);
    oled_WriteString("# = START", Font_7x10, White);

    oled_UpdateScreen();
}


static void ShowTimerScreen(void)
{
    char buffer[16];

    uint32_t minutes = remainingTime / 60U;
    uint32_t seconds = remainingTime % 60U;

    oled_Fill(Black);

    oled_SetCursor(0, 0);
    oled_WriteString("TIME LEFT", Font_7x10, White);

    sprintf(buffer,
            "%02lu:%02lu",
            (unsigned long)minutes,
            (unsigned long)seconds);

    oled_SetCursor(20, 22);
    oled_WriteString(buffer, Font_11x18, White);

    oled_UpdateScreen();
}


static void ShowAlarmScreen(void)
{
    oled_Fill(Black);

    oled_SetCursor(15, 18);
    oled_WriteString("TIME OUT!", Font_11x18, White);

    oled_SetCursor(39, 44);
    oled_WriteString("ALARM", Font_7x10, White);

    oled_UpdateScreen();
}


static void ProcessKeyboard(void)
{
    char key = ReadKeyboard();

    if (key == 0 || timerState != TIMER_INPUT)
        return;

    if (key >= '0' && key <= '9')
    {
        uint32_t digit = (uint32_t)(key - '0');

        if (setTime <= (MAX_SET_TIME - digit) / 10U)
        {
            setTime = setTime * 10U + digit;
            ShowInputScreen();
        }
    }
    else if (key == '*')
    {
        setTime = 0;
        ShowInputScreen();
    }
    else if (key == '#')
    {
        if (setTime > 0)
        {
            remainingTime = setTime;
            lastSecondTick = HAL_GetTick();
            timerState = TIMER_RUNNING;
            ShowTimerScreen();
        }
    }
}


/* USER CODE END 0 */

/**
  * @brief  The application entry point.
  * @retval int
  */
int main(void)
{
    /* MCU Configuration--------------------------------------------------------*/

    HAL_Init();

    /* Configure the system clock */
    SystemClock_Config();

    /* Initialize all configured peripherals */
    MX_GPIO_Init();
    MX_I2C1_Init();
    MX_USART6_UART_Init();
    MX_TIM2_Init();

    /* USER CODE BEGIN 2 */
    oled_Init();

    /* Buzzer initialization from the original SDK driver. */
    Buzzer_Init();
    Buzzer_Set_Volume(BUZZER_VOLUME_MUTE);

    /*
     * For the first 5 seconds show the keyboard legend.
     * The physical keyboard itself has no labels.
     */
    ShowKeyboardLegend();
    HAL_Delay(5000);

    setTime = 0;
    remainingTime = 0;
    timerState = TIMER_INPUT;

    ShowInputScreen();
    /* USER CODE END 2 */

    /* Infinite loop */
    /* USER CODE BEGIN WHILE */
    while (1)
    {
        ProcessKeyboard();

        if (timerState == TIMER_RUNNING)
        {
            uint32_t now = HAL_GetTick();

            /*
             * "while" instead of "if" also compensates if one iteration
             * of the main loop takes longer than one second.
             */
            while ((uint32_t)(now - lastSecondTick) >= 1000U &&
                   timerState == TIMER_RUNNING)
            {
                lastSecondTick += 1000U;

                if (remainingTime > 0)
                {
                    remainingTime--;
                }

                ShowTimerScreen();

                if (remainingTime == 0)
                {
                    timerState = TIMER_ALARM;
                    alarmStartTick = HAL_GetTick();

                    ShowAlarmScreen();

                    /* Continuous alarm tone using the original buzzer.c API. */
                    Buzzer_Set_Freq(N_A5);  /* 880 Hz */
                    Buzzer_Set_Volume(BUZZER_VOLUME_MAX);
                }
            }
        }
        else if (timerState == TIMER_ALARM)
        {
            if ((uint32_t)(HAL_GetTick() - alarmStartTick) >= ALARM_TIME_MS)
            {
                Buzzer_Set_Volume(BUZZER_VOLUME_MUTE);

                setTime = 0;
                remainingTime = 0;
                timerState = TIMER_INPUT;

                ShowInputScreen();
            }
        }

        HAL_Delay(KEYBOARD_DELAY_MS);
    }
    /* USER CODE END WHILE */
}


/**
  * @brief System Clock Configuration
  * @retval None
  */
void SystemClock_Config(void)
{
    RCC_OscInitTypeDef RCC_OscInitStruct = {0};
    RCC_ClkInitTypeDef RCC_ClkInitStruct = {0};

    /** Configure the main internal regulator output voltage */
    __HAL_RCC_PWR_CLK_ENABLE();
    __HAL_PWR_VOLTAGESCALING_CONFIG(PWR_REGULATOR_VOLTAGE_SCALE1);

    /** Initializes the CPU, AHB and APB busses clocks */
    RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_HSE;
    RCC_OscInitStruct.HSEState = RCC_HSE_ON;
    RCC_OscInitStruct.PLL.PLLState = RCC_PLL_ON;
    RCC_OscInitStruct.PLL.PLLSource = RCC_PLLSOURCE_HSE;
    RCC_OscInitStruct.PLL.PLLM = 25;
    RCC_OscInitStruct.PLL.PLLN = 336;
    RCC_OscInitStruct.PLL.PLLP = RCC_PLLP_DIV2;
    RCC_OscInitStruct.PLL.PLLQ = 4;

    if (HAL_RCC_OscConfig(&RCC_OscInitStruct) != HAL_OK)
    {
        Error_Handler();
    }

    /** Initializes the CPU, AHB and APB busses clocks */
    RCC_ClkInitStruct.ClockType = RCC_CLOCKTYPE_HCLK |
                                  RCC_CLOCKTYPE_SYSCLK |
                                  RCC_CLOCKTYPE_PCLK1 |
                                  RCC_CLOCKTYPE_PCLK2;

    RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_PLLCLK;
    RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
    RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV4;
    RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV2;

    if (HAL_RCC_ClockConfig(&RCC_ClkInitStruct, FLASH_LATENCY_5) != HAL_OK)
    {
        Error_Handler();
    }
}


/**
  * @brief  This function is executed in case of error occurrence.
  * @retval None
  */
void Error_Handler(void)
{
    __disable_irq();

    while (1)
    {
    }
}


#ifdef USE_FULL_ASSERT
/**
  * @brief Reports the name of the source file and the source line number
  *        where the assert_param error has occurred.
  */
void assert_failed(uint8_t *file, uint32_t line)
{
    (void)file;
    (void)line;
}
#endif /* USE_FULL_ASSERT */
