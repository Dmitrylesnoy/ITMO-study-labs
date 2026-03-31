    .data
buffer:             .byte '___________________________________'
addr_inpt:          .word 0x80
arrd_out:           .word 0x84
ptr_right:          .word 0
ptr_left:           .word 0
ptr_buff:           .word 0
char_l:             .word 0
char_r:             .word 0
mask_1char:         .word 0x000000ff
mask_write:         .word 0xffffff00
const_1:            .word 1
const_endl:         .word 10
const_32:           .word 32
iter_count:         .word 0
length:             .word 0

    .text           
    .org    0x100
_start:             
    load_imm        buffer
    store           ptr_right
    store           ptr_left
   
input_cycle: 
    load            addr_inpt
    load_acc
    and             mask_1char
    store           char_l
    beqz            save_current_ptr

input_continue:
    load            char_l
    xor             const_endl
    beqz            set_null_char
    
    load            ptr_right
    load_acc
    and             mask_write
    add             char_l
    store_ind       ptr_right
    
    load            ptr_right
    add             const_1
    store           ptr_right
    
    load            iter_count
    add             const_1
    store           iter_count
    xor             const_32
    beqz            err_overflow
    jmp             input_cycle

save_current_ptr:
    load            ptr_right
    store           ptr_buff
    load            iter_count
    store           length
    jmp             input_continue
    
set_null_char:
    load            ptr_right
    load_acc
    and             mask_write      
    store_ind       ptr_right
    jmp             check_ptr

check_ptr:
    load            ptr_buff
    beqz            start_reverse
    store           ptr_right
    
start_reverse:
    load            ptr_right
    sub             const_1
    store           ptr_right
    
    load            length
    beqz            calc_iters
    store           iter_count
    
calc_iters:
    load            iter_count
    shiftr          const_1           
    store           iter_count
    
    load_imm        buffer
    store           ptr_left
    
swap_loop:   
    load            iter_count
    beqz            output_result
    sub             const_1
    store           iter_count
    
    load            ptr_right
    load_acc
    and             mask_1char
    store           char_l
    
    load            ptr_left
    load_acc
    and             mask_1char
    store           char_r
    
    load            ptr_left
    load_acc
    and             mask_write
    add             char_l
    store_ind       ptr_left
    
    load            ptr_right
    load_acc
    and             mask_write
    add             char_r
    store_ind       ptr_right
    
    load            ptr_right
    sub             const_1
    store           ptr_right
    
    load            ptr_left
    add             const_1
    store           ptr_left

    jmp             swap_loop
    
output_result:
    load_imm        buffer
    store           ptr_right

output_while:
    load            ptr_right
    load_acc
    and             mask_1char
    beqz            finish
    store_ind       arrd_out
    
    load            ptr_right
    add             const_1
    store           ptr_right
    
    jmp             output_while
    
err_overflow:
    load_imm        0xcccccccc
    store_ind       arrd_out    
    
finish:
    halt