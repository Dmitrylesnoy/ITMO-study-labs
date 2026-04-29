    .data

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text

_start:
    lui      t0, %hi(input_addr)
    addi     t0, t0, %lo(input_addr)
    lw       t0, 0(t0)
    
    lw       t1, 0(t0)                     ; t1 = n
    addi     t2, zero, 0                   ; count = 0
    addi     t3, zero, 1                   ; constant 1
    
ones_loop:
    beqz     t1, ones_done
    
    and      t4, t1, t3                    ; t4 = n & 1
    add      t2, t2, t4
    
    srl      t1, t1, t3                    ; n >>= 1 
    j        ones_loop
    
ones_done:
    lui      t0, %hi(output_addr)
    addi     t0, t0, %lo(output_addr)
    lw       t0, 0(t0)
    
    sw       t2, 0(t0)
    halt