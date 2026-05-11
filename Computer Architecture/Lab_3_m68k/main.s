    .text
    .org     0x100
_start:
    movea.l  input_addr, A3

    movea.l  input_addr, A0
    movea.l  (A0), A0        ; A0 -> порт ввода
    movea.l  output_addr, A1
    movea.l  (A1), A1        ; A1 -> порт вывода

    clr.l    D3              ; Флаг: 1 если мы в процессе сборки числа
    clr.l    D4              ; Текущее собираемое число

read_char:
    move.b   0, D0
    move.b   (A0), D0       ; Читаем 1 байт из ввода
    cmp.b    0x0A, D0       ; Проверка на конец строки
    beq      start_calc
    cmp.b    ' ', D0        ; Проверка на пробел
    beq      handle_space

    ; Если это оператор, то сохраняем его как токен
    cmp.b    '+', D0          ; '+'
    beq      save_op
    cmp.b    '-', D0          ; '-'
    beq      save_op
    cmp.b    '*', D0          ; '*'
    beq      save_op
    cmp.b    '/', D0          ; '/'
    beq      save_op

    ; Если цифра
    sub.b    '0', D0         ; Переводим в цифру 0-9
    mul.l    10, D4
    add.l    D0, D4
    cmp.l    1073741824, D4
    bgt      return_overflow
    cmp.l    -1073741824, D4
    blt      return_overflow
    move.l   1, D3           ; Ставим флаг "собираем число"
    jmp      read_char

handle_space:
    cmp.l    1, D3           ; Если до этого собирали число, сохраняем его
    bne      read_char

    ; Сохраняем число в токены
    add.l    4, 24(A3)
    movea.l  24(A3), A2
    move.l   D4, (A2)

    clr.l    D4              ; Сброс сборщика
    clr.l    D3              ; Сброс флага
    jmp      read_char

save_op:
    ; Сохраняем ASCII код оператора как токен
    add.l    4, 24(A3)
    movea.l  24(A3), A2
    move.l   D0, (A2)
    jmp      read_char

start_calc:
    move.l  24(A3), D0
    sub.l   8(A3), D0
    beq     return_err
    cmp.l   12, D0
    blt     return_err

    move.l  24(A3), 28(A3) ; обвновляем адрес конца списка токенов
    move.l   8(A3), 24(A3)  ; устанавливаем указатель на начало списка токенов

    move.l  12(A3), 16(A3) ; обновляем указатель стека наверх

main_loop:
    move.l  24(A3), D0
    sub.l   28(A3), D0
    beq     main_loop_end

    add.l    4, 24(A3)
    movea.l  24(A3), A2
    move.l   (A2), D0

    cmp.l    '+', D0           ; '+'
    beq      check_nums_stack
    cmp.l    '-', D0           ; '-'
    beq      check_nums_stack
    cmp.l    '*', D0           ; '*'
    beq      check_nums_stack
    cmp.l    '/', D0           ; '/'
    beq      check_nums_stack
    jmp      push_num

check_nums_stack:
    move.l   16(A3), D1
    sub.l    12(A3), D1
    cmp.l    8, D1           ; Меньше 2 чисел?
    blt      return_err

    movea.l  16(A3), A2
    move.l   (A2), D2        ; b
    sub.l    4, 16(A3)

    movea.l  16(A3), A2
    move.l   (A2), D1        ; a
    sub.l    4, 16(A3)

    cmp.l    '+', D0           ; '+'
    beq      do_add
    cmp.l    '-', D0           ; '-'
    beq      do_sub
    cmp.l    '*', D0           ; '*'
    beq      do_mul
    cmp.l    '/', D0           ; '/'
    beq      do_div

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
    add.l    4, 16(A3)
    movea.l  16(A3), A2
    move.l   D2, (A2)
    jmp      main_loop

push_num:
    add.l    4, 16(A3)
    movea.l  16(A3), A2
    move.l   D0, (A2)
    jmp      main_loop

main_loop_end:
    move.l   16(A3), D1
    sub.l    12(A3), D1
    cmp.l    4, D1           ; Должно быть ровно 4 байта
    bne      return_err

    ; Вывод результата
    movea.l  16(A3), A2
    move.l   (A2), D1
    movea.l  4(A3), A2
    move.l   D1, (A2)
    halt

return_err:
    move.l   -1, (A1)
    halt

return_overflow:
    movea.l   const_overflow, A2
    move.l    (A2), D1
    move.l    D1, (A1)
    halt

    .data
    .org 0x400
input_addr:      .word  0x80    ; +0
output_addr:     .word  0x84    ; +4
token_addr:      .word  0x500  ; +8
stack_top:       .word  0x600   ; +12
stack_ptr:       .word  0x600   ; +16
stack_acc_prt:   .word  0x0   ; +20
token_ptr:       .word  0x500  ; +24
token_end:       .word  0x500   ; +28

const_endl:      .byte  10      ; +24
const_space:     .byte  32      ; +28
const_overflow:  .word  0xCCCCCCCC ; +32
const_num_char:  .byte  48      ; +36

const_plus:      .byte  43      ; +40
const_minus:     .byte  45      ; +44
const_mul:       .byte  42      ; +48
const_div:       .byte  47      ; +52
