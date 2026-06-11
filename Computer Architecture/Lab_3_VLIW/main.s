.data
fnv_prime_val:   .word  0x01000193
fnv_init_hash:   .word  0x811C9DC5

input_addr:      .word  0x80
output_addr:     .word  0x84

    .text
    .org 0x100
_start:
    lui a1, %hi(input_addr)         / lui a2, %hi(output_addr)        / nop          / nop
    lui t1, %hi(fnv_prime_val)      / lui t2, %hi(fnv_init_hash)      / nop          / nop

    addi a1, a1, %lo(input_addr)    / addi a2, a2, %lo(output_addr)   / nop          / nop
    addi t1, t1, %lo(fnv_prime_val) / addi t2, t2, %lo(fnv_init_hash) / nop          / nop
    
    nop                             / nop                             / lw a1, 0(a1) / nop
    nop                             / nop                             / lw a2, 0(a2) / nop
    nop                             / nop                             / lw t1, 0(t1) / nop
    nop                             / nop                             / lw t2, 0(t2) / nop

hash_loop:
    nop                             / nop                             / lb t3, 0(a1) / beqz t3, end_loop
    mul t2, t2, t1                  / nop                             / nop          / nop
    xor t2, t2, t3                  / nop                             / nop          / j hash_loop

end_loop:
    nop                             / nop                             / sw t2, 0(a2) / nop
    nop                             / nop                             / nop          / halt