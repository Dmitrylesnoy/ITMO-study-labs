    .data
input_addr:      .word  0x80
output_addr:     .word  0x84
stack_top:       .word  0x200
inp_num_buff:    .byte  0
const_endl:      .byte  10
const_space:     .byte  20
const_overflow:  .word  0xCCCCCCCC
strip_flag:      .byte  0

    .org 0x300
token_addr:      .word  0x300

    .text
    .org     0x100
_start:
    movea.l  stack_top, A7
    movea.l  (A7), A7
    movea.l  input_addr, A0
    movea.l  (A0), A0
    movea.l  output_addr, A1
    movea.l  (A1), A1
    movea.l  token_addr, A2
    move.l     0, D3

next_token:
    not.l      D3
    cmp.l      D3, 0x0 
    beq      return_err
    cmp.l      D3, 0x2B
    beq      input_loop
    cmp.l      D3, 0x2D
    beq      input_loop
    cmp.l      D3, 0x2A
    beq      input_loop
    cmp.l      D3, 0x2F
    beq      input_loop
    
    move.l   (A2)+, D2
    move.l   0, (A2)
input_loop:
    move.l   (A0), D0
    cmp.l    D0, const_space
    beq      next_token
    cmp.l    D0, const_endl
    beq      main                   ; make a Strip str format check

    move.l   (A2), D1
    mul.l    10, D1
    sub.b    30, D0
    add.l    D0, D1
    move.l   D1, (A2)
    move     1, D3

    jmp input_loop

main:
    movea.l  token_addr, A3         ; token iters in tokens
    move.l   (A2)+, D0
    movea.l  A2, A6              ; stack addr init

main_loop:
    move.l  (A3)+, D0
    cmp.l   0, D0
    beq     main_loop_end
    cmp.l   30, D0
    bge     else_num

    movea.l A6, D4
    sub.l   A2, D4
    ble     return_err
    move.l  -(A6), D1       ; b
    move.l  -(A6), D2       ; a

    cmp      D0, 0x2B
    beq      calc_sum
    cmp      D3, 0x2D
    beq      calc_sub
    cmp      D3, 0x2A
    beq      calc_mul
    cmp      D3, 0x2F
    beq      calc_div

calc_sum:
    add.l   D1, D2
    jmp     add_stack
calc_sub:
    sub.l   D1, D2
    jmp     add_stack
calc_mul:
    mul.l   D1, D2
    jmp     add_stack
calc_div:
    cmp.l   0, D1
    beq     return_err
    div.l   D1, D2
add_stack:
    move.l  D2, (A6)+
    jmp     main_loop

else_num:
    move.l  (A3), (A6)+
    jmp     main_loop

output:
    move.l   D0, (A1)

    halt

main_loop_end:
    movea.l A6, D4
    sub.l   A2, D4
    ble     return_err
    movea.l  -(A6), (A1)

return_err:
    move.l  -1, (A1)
    hlt

    
return_err:
    move.l  const_overflow, (A1)
    hlt
