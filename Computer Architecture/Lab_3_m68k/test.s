.data
input_addr:      .word  0x80
output_addr:     .word  0x84
stack_top:       .word  0x200
const_endl:      .byte  10
const_space:     .byte  32
const_overflow:  .word  0xCCCCCCCC

    .org 0x300
token_addr:      .word  0x400  ; Буфер для хранения распарсенных токенов

    .text
    .org     0x100
_start:
    movea.l  stack_top, A7
    movea.l  (A7), A7        ; Установка SP
    movea.l  input_addr, A0
    movea.l  (A0), A0        ; A0 -> порт ввода
    movea.l  output_addr, A1
    movea.l  (A1), A1        ; A1 -> порт вывода
    movea.l  token_addr, A2
    movea.l  (A2), A2        ; A2 -> начало списка токенов
    
    clr.l    D3              ; Флаг: 1 если мы в процессе сборки числа
    clr.l    D4              ; Текущее собираемое число

read_char:
    move.b   (A0), D0        ; Читаем 1 байт из ввода
    cmp.b    const_endl, D0  ; Проверка на конец строки (\n)
    beq      finalize_and_main
    cmp.b    const_space, D0 ; Проверка на пробел
    beq      handle_space

    ; Проверка, не оператор ли это (+, -, *, /)
    cmp.b    43, D0          ; '+'
    beq      save_op
    cmp.b    45, D0          ; '-'
    beq      save_op
    cmp.b    42, D0          ; '*'
    beq      save_op
    cmp.b    47, D0          ; '/'
    beq      save_op

    ; Если цифра (парсинг числа)
    sub.b    48, D0          ; ASCII '0' = 48. Переводим в цифру 0-9
    mul.l    10, D4
    add.l    D0, D4
    move.l   1, D3           ; Ставим флаг "собираем число"
    jmp      read_char

handle_space:
    cmp.l    1, D3           ; Если до этого собирали число, сохраняем его
    bne      read_char
    move.l   D4, (A2)+       ; Сохраняем число в токены
    clr.l    D4              ; Сброс сборщика
    clr.l    D3
    jmp      read_char

save_op:
    move.l   D0, (A2)+       ; Сохраняем ASCII код оператора как токен
    jmp      read_char

finalize_and_main:
    cmp.l    1, D3           ; Добавляем последнее число, если оно было
    bne      start_calc
    move.l   D4, (A2)+

start_calc:
    move.l   0, (A2)         ; Маркер конца токенов (NULL)
    movea.l  token_addr, A3
    movea.l  (A3), A3        ; A3 итерирует по токенам
    movea.l  A7, A6          ; Используем A6 как указатель стека калькулятора

main_loop:
    move.l   (A3)+, D0       ; Берем токен
    cmp.l    0, D0           ; Конец?
    beq      main_loop_end

    ; Проверяем, оператор это (ASCII < 48) или число
    cmp.l    48, D0
    bge      push_num

    ; Это оператор. Нужно минимум 2 числа в стеке.
    ; (A7 - A6) / 4 — количество элементов.
    move.l   A7, D1
    sub.l    A6, D1
    cmp.l    8, D1           ; Меньше 8 байт (2-х лонгов)?
    blt      return_err

    move.l   -(A6), D2       ; b
    move.l   -(A6), D1       ; a

    cmp.l    43, D0          ; '+'
    beq      do_add
    cmp.l    45, D0          ; '-'
    beq      do_sub
    cmp.l    42, D0          ; '*'
    beq      do_mul
    cmp.l    47, D0          ; '/'
    beq      do_div
    jmp      return_err      ; Неизвестный символ

do_add:
    add.l    D1, D2
    jmp      push_res
do_sub:
    sub.l    D2, D1
    move.l   D1, D2
    jmp      push_res
do_mul:
    mul.l    D1, D2
    jmp      push_res
do_div:
    cmp.l    0, D2           ; Деление на 0
    beq      return_err
    div.l    D2, D1
    move.l   D1, D2

push_res:
    move.l   D2, (A6)+
    jmp      main_loop

push_num:
    move.l   D0, (A6)+
    jmp      main_loop

main_loop_end:
    ; В стеке должен остаться ровно один результат
    move.l   A6, D1
    sub.l    A7, D1
    cmp.l    4, D1           ; Должно быть ровно 4 байта (1 число)
    bne      return_err

    move.l   -(A6), (A1)     ; Вывод результата
    halt

return_err:
    move.l   -1, (A1)
    halt

return_overflow:
    move.l   const_overflow, (A1)
    halt