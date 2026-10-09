/* USER CODE BEGIN Header */
/**
  ******************************************************************************
  * File Name          : freertos.c
  * Description        : Tic-Tac-Toe FreeRTOS application for SDK-1.1M
  ******************************************************************************
  */
/* USER CODE END Header */

/* Includes ------------------------------------------------------------------*/
#include "FreeRTOS.h"
#include "task.h"
#include "main.h"
#include "cmsis_os.h"

/* Private includes ----------------------------------------------------------*/
/* USER CODE BEGIN Includes */
#include "kb.h"
#include "oled.h"
#include "fonts.h"
#include <stdint.h>
/* USER CODE END Includes */

/* Private typedef -----------------------------------------------------------*/
/* USER CODE BEGIN PTD */

typedef enum
{
  CELL_EMPTY = 0,
  CELL_X,
  CELL_O
} Cell;

typedef enum
{
  GAME_RUNNING = 0,
  GAME_X_WON,
  GAME_O_WON,
  GAME_DRAW
} GameState;

/* USER CODE END PTD */

/* Private define ------------------------------------------------------------*/
/* USER CODE BEGIN PD */

#define KEYBOARD_SCAN_PERIOD_MS   20U
#define KEY_QUEUE_LENGTH           8U

#define BOARD_SIZE                63U
#define CELL_SIZE                 21U

/* USER CODE END PD */

/* Private macro -------------------------------------------------------------*/
/* USER CODE BEGIN PM */
/* USER CODE END PM */

/* Private variables ---------------------------------------------------------*/
/* USER CODE BEGIN Variables */

static osMessageQueueId_t keyQueueHandle;
static osMutexId_t i2cMutexHandle;

static Cell board[9];
static Cell currentPlayer = CELL_X;
static GameState gameState = GAME_RUNNING;

/* USER CODE END Variables */
/* Definitions for defaultTask */
osThreadId_t defaultTaskHandle;
const osThreadAttr_t defaultTask_attributes = {
  .name = "defaultTask",
  .stack_size = 384 * 4,
  .priority = (osPriority_t) osPriorityNormal,
};
/* Definitions for myTask02 */
osThreadId_t myTask02Handle;
const osThreadAttr_t myTask02_attributes = {
  .name = "myTask02",
  .stack_size = 256 * 4,
  .priority = (osPriority_t) osPriorityAboveNormal,
};

/* Private function prototypes -----------------------------------------------*/
/* USER CODE BEGIN FunctionPrototypes */

static char ReadKeyboard(void);

static void Game_Reset(void);
static void Game_ProcessKey(char key);
static GameState Game_Evaluate(void);

static void RenderSplash(void);
static void RenderGame(void);
static void DrawBoard(void);
static void DrawX(uint8_t cellX, uint8_t cellY);
static void DrawO(uint8_t cellX, uint8_t cellY);
static void DrawCirclePoint(uint8_t cx, uint8_t cy, int8_t x, int8_t y);

/* USER CODE END FunctionPrototypes */

void StartDefaultTask(void *argument);
void StartTask02(void *argument);

void MX_FREERTOS_Init(void); /* (MISRA C 2004 rule 8.1) */

/**
  * @brief  FreeRTOS initialization
  * @param  None
  * @retval None
  */
void MX_FREERTOS_Init(void) {
  /* USER CODE BEGIN Init */
  /* USER CODE END Init */

  /* USER CODE BEGIN RTOS_MUTEX */
  /*
   * The OLED and the keypad both use I2C1.
   * Protect the HAL I2C driver from simultaneous access by two tasks.
   */
  i2cMutexHandle = osMutexNew(NULL);
  if (i2cMutexHandle == NULL)
  {
    Error_Handler();
  }
  /* USER CODE END RTOS_MUTEX */

  /* USER CODE BEGIN RTOS_SEMAPHORES */
  /* USER CODE END RTOS_SEMAPHORES */

  /* USER CODE BEGIN RTOS_TIMERS */
  /* USER CODE END RTOS_TIMERS */

  /* USER CODE BEGIN RTOS_QUEUES */
  /*
   * KeyboardTask sends one-byte key events.
   * GameTask blocks on this queue and wakes only when a key is pressed.
   */
  keyQueueHandle = osMessageQueueNew(KEY_QUEUE_LENGTH, sizeof(uint8_t), NULL);
  if (keyQueueHandle == NULL)
  {
    Error_Handler();
  }
  /* USER CODE END RTOS_QUEUES */

  /* Create the thread(s) */
  /* creation of defaultTask */
  defaultTaskHandle = osThreadNew(StartDefaultTask, NULL, &defaultTask_attributes);

  /* creation of myTask02 */
  myTask02Handle = osThreadNew(StartTask02, NULL, &myTask02_attributes);

  /* USER CODE BEGIN RTOS_THREADS */
  if ((defaultTaskHandle == NULL) || (myTask02Handle == NULL))
  {
    Error_Handler();
  }
  /* USER CODE END RTOS_THREADS */

  /* USER CODE BEGIN RTOS_EVENTS */
  /* add events, ... */
  /* USER CODE END RTOS_EVENTS */

}

/* USER CODE BEGIN Header_StartDefaultTask */
/**
  * @brief  Function implementing the defaultTask thread.
  * @param  argument: Not used
  * @retval None
  */
/* USER CODE END Header_StartDefaultTask */
void StartDefaultTask(void *argument)
{
  /* USER CODE BEGIN StartDefaultTask */
  (void)argument;

  RenderSplash();
  osDelay(1200);

  Game_Reset();
  RenderGame();

  for (;;)
  {
    uint8_t key = 0U;

    if (osMessageQueueGet(keyQueueHandle, &key, NULL, osWaitForever) == osOK)
    {
      Game_ProcessKey((char)key);
      RenderGame();
    }
  }
  /* USER CODE END StartDefaultTask */
}

/* USER CODE BEGIN Header_StartTask02 */
/**
* @brief Function implementing the myTask02 thread.
* @param argument: Not used
* @retval None
*/
/* USER CODE END Header_StartTask02 */
void StartTask02(void *argument)
{
  /* USER CODE BEGIN StartTask02 */
  (void)argument;

  for (;;)
  {
    char key = 0;

    if (osMutexAcquire(i2cMutexHandle, osWaitForever) == osOK)
    {
      key = ReadKeyboard();
      osMutexRelease(i2cMutexHandle);
    }

    if (key != 0)
    {
      uint8_t event = (uint8_t)key;
      (void)osMessageQueuePut(keyQueueHandle, &event, 0U, 0U);
    }

    osDelay(KEYBOARD_SCAN_PERIOD_MS);
  }
  /* USER CODE END StartTask02 */
}

/* Private application code --------------------------------------------------*/
/* USER CODE BEGIN Application */

static char ReadKeyboard(void)
{
  static const uint8_t rows[4] = {ROW1, ROW2, ROW3, ROW4};
  static const char keys[4][3] = {
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
    if (key == 0x04) { currentKey = keys[row][0]; break; }
    if (key == 0x02) { currentKey = keys[row][1]; break; }
    if (key == 0x01) { currentKey = keys[row][2]; break; }
  }

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

static void Game_Reset(void)
{
  for (uint8_t i = 0; i < 9; i++) board[i] = CELL_EMPTY;
  currentPlayer = CELL_X;
  gameState = GAME_RUNNING;
}

static GameState Game_Evaluate(void)
{
  static const uint8_t winLines[8][3] = {
    {0,1,2},{3,4,5},{6,7,8},
    {0,3,6},{1,4,7},{2,5,8},
    {0,4,8},{2,4,6}
  };

  for (uint8_t i = 0; i < 8; i++)
  {
    uint8_t a = winLines[i][0];
    uint8_t b = winLines[i][1];
    uint8_t c = winLines[i][2];
    if ((board[a] != CELL_EMPTY) && (board[a] == board[b]) && (board[a] == board[c]))
      return (board[a] == CELL_X) ? GAME_X_WON : GAME_O_WON;
  }

  for (uint8_t i = 0; i < 9; i++)
    if (board[i] == CELL_EMPTY) return GAME_RUNNING;

  return GAME_DRAW;
}

static void Game_ProcessKey(char key)
{
  if (key == '*') { Game_Reset(); return; }
  if ((key == '#') && (gameState != GAME_RUNNING)) { Game_Reset(); return; }
  if (gameState != GAME_RUNNING) return;
  if ((key < '1') || (key > '9')) return;

  uint8_t cell = (uint8_t)(key - '1');
  if (board[cell] != CELL_EMPTY) return;

  board[cell] = currentPlayer;
  gameState = Game_Evaluate();
  if (gameState == GAME_RUNNING)
    currentPlayer = (currentPlayer == CELL_X) ? CELL_O : CELL_X;
}

static void DrawX(uint8_t cellX, uint8_t cellY)
{
  uint8_t x0 = (uint8_t)(cellX + 4U);
  uint8_t y0 = (uint8_t)(cellY + 4U);
  uint8_t x1 = (uint8_t)(cellX + 16U);

  for (uint8_t i = 0; i <= 12U; i++)
  {
    uint8_t y = (uint8_t)(y0 + i);
    oled_DrawPixel((uint8_t)(x0 + i), y, White);
    oled_DrawPixel((uint8_t)(x1 - i), y, White);
    oled_DrawPixel((uint8_t)(x0 + i), (uint8_t)(y + 1U), White);
    oled_DrawPixel((uint8_t)(x1 - i), (uint8_t)(y + 1U), White);
  }
}

static void DrawCirclePoint(uint8_t cx, uint8_t cy, int8_t x, int8_t y)
{
  oled_DrawPixel((uint8_t)(cx + x), (uint8_t)(cy + y), White);
  oled_DrawPixel((uint8_t)(cx - x), (uint8_t)(cy + y), White);
  oled_DrawPixel((uint8_t)(cx + x), (uint8_t)(cy - y), White);
  oled_DrawPixel((uint8_t)(cx - x), (uint8_t)(cy - y), White);
  oled_DrawPixel((uint8_t)(cx + y), (uint8_t)(cy + x), White);
  oled_DrawPixel((uint8_t)(cx - y), (uint8_t)(cy + x), White);
  oled_DrawPixel((uint8_t)(cx + y), (uint8_t)(cy - x), White);
  oled_DrawPixel((uint8_t)(cx - y), (uint8_t)(cy - x), White);
}

static void DrawO(uint8_t cellX, uint8_t cellY)
{
  uint8_t cx = (uint8_t)(cellX + 10U);
  uint8_t cy = (uint8_t)(cellY + 10U);
  int8_t x = 7, y = 0, err = 1 - 7;

  while (x >= y)
  {
    DrawCirclePoint(cx, cy, x, y);
    y++;
    if (err < 0) err = (int8_t)(err + (2 * y) + 1);
    else { x--; err = (int8_t)(err + (2 * (y - x)) + 1); }
  }
}

static void DrawBoard(void)
{
  oled_DrawVLine(0, 62, 21, White);
  oled_DrawVLine(0, 62, 42, White);
  oled_DrawHLine(0, 62, 21, White);
  oled_DrawHLine(0, 62, 42, White);

  for (uint8_t i = 0; i < 9; i++)
  {
    uint8_t row = (uint8_t)(i / 3U);
    uint8_t col = (uint8_t)(i % 3U);
    uint8_t cellX = (uint8_t)(col * CELL_SIZE);
    uint8_t cellY = (uint8_t)(row * CELL_SIZE);

    if (board[i] == CELL_X) DrawX(cellX, cellY);
    else if (board[i] == CELL_O) DrawO(cellX, cellY);
    else
    {
      char number = (char)('1' + i);
      oled_SetCursor((uint8_t)(cellX + 7U), (uint8_t)(cellY + 5U));
      oled_WriteChar(number, Font_7x10, White);
    }
  }
}

static void RenderSplash(void)
{
  if (osMutexAcquire(i2cMutexHandle, osWaitForever) != osOK) return;

  oled_Fill(Black);
  oled_SetCursor(25, 7);
  oled_WriteString("TIC TAC TOE", Font_7x10, White);
  oled_SetCursor(25, 27);
  oled_WriteString("1-9 = MOVE", Font_7x10, White);
  oled_SetCursor(32, 45);
  oled_WriteString("* = NEW", Font_7x10, White);
  oled_UpdateScreen();

  osMutexRelease(i2cMutexHandle);
}

static void RenderGame(void)
{
  if (osMutexAcquire(i2cMutexHandle, osWaitForever) != osOK) return;

  oled_Fill(Black);
  DrawBoard();

  if (gameState == GAME_RUNNING)
  {
    oled_SetCursor(68, 2);
    oled_WriteString("TURN", Font_7x10, White);
    oled_SetCursor(84, 14);
    oled_WriteString((currentPlayer == CELL_X) ? "X" : "O", Font_11x18, White);
    oled_SetCursor(68, 38);
    oled_WriteString("1-9 MOVE", Font_7x10, White);
    oled_SetCursor(68, 51);
    oled_WriteString("*=NEW", Font_7x10, White);
  }
  else
  {
    oled_SetCursor(68, 4);
    if (gameState == GAME_X_WON) oled_WriteString("X WINS", Font_7x10, White);
    else if (gameState == GAME_O_WON) oled_WriteString("O WINS", Font_7x10, White);
    else oled_WriteString("DRAW", Font_7x10, White);

    oled_SetCursor(68, 24);
    oled_WriteString("PRESS *", Font_7x10, White);
    oled_SetCursor(68, 38);
    oled_WriteString("OR #", Font_7x10, White);
    oled_SetCursor(68, 52);
    oled_WriteString("NEW GAME", Font_7x10, White);
  }

  oled_UpdateScreen();
  osMutexRelease(i2cMutexHandle);
}

/* USER CODE END Application */

