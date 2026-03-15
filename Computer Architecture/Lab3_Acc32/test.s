    .data
    .org 0x100
buffer:          .byte  '________'         ; buffer for 2symbols
input_addr:      .word  0x80               ; Input address where input string is stored
output_addr:     .word  0x84               ; Output address where the result string should be stored
i:               .word  0x80               ; iterator for symbols in input
j:               .word  0x84               ; iterator for symbols in result
buffer_ptr:      .word  0x00               ; pointer to 1 symbol in string
mask_1char:      .word  0x000000FF         ; mask to fet little symbol
const_endl:      .word  10                 ; constant for new line symb
const_1:         .word  1                  ; Constant 1
const_32:        .word  32                 ; const for overflow string memory cell
char_temp:       .word  0                  ; 1 symb buff
str_len:         .word  0                  ; string length

    .text
    .org 0x200
_start:
    load_imm     buffer
    store        buffer_ptr
    load         input_addr
    load_acc

finding_end:
    load         i   
    load_acc                  
    and          mask_1char               ; get little char    
    store        char_temp                ; saving lisstle symbol alone
    
    load         char_temp
    sub          const_endl                   
    beqz         reverse_loop             ; if (mem[i+1]=='\n') goto reverse_loop
    
    load         char_temp
    store_ind    buffer_ptr               

    load         buffer_ptr
    add          const_1
    store        buffer_ptr               ; buffer_ptr++

    load         i                         
    add          const_1                   
    store        i                         ; i++
    
    load         str_len
    add          const_1
    store        str_len                    ; length++
    sub          const_32
    bgt          reverse_overflow          ; if(length>32) goto overflow

    jmp          finding_end

reverse_loop:
    load         buffer_ptr
    sub          const_1
    store        buffer_ptr                ; buffer_ptr--
    
    load_imm     buffer
    sub          buffer_ptr
    ble          reverse_end               ; if (went out of buffer address diaposont
                                           ;  goto reverse_end

    load         buffer_ptr
    load_acc
    and          mask_1char
    store_ind    output_addr                         ; mem[mem[j]]=mem[mem[i]]

    load         j 
    add          const_1                   
    store        j                         ; j++
    jmp          reverse_loop 

reverse_end:
    halt

reverse_overflow:
    load_imm     0xCCCC_CCCC
    store_ind    output_addr                 ; mem[mem[output_addr]] = 0xCCCC_CCCC
    halt