ORG 0x584
ARD0: WORD 0

ORG 0x28C
POINTER: WORD 0x584
ITER: WORD 0

START:  CLA

INPLEN: CALL READ
        INC
        ST ITER
        DEC
MAIN:
        ST (POINTER)+
        LOOP ITER
        JUMP FIRSTWORD
        HLT
FIRSTWORD:
        LD ITER
        DEC
        ST ITER
        BEQ ENDWORD
        CALL READ
        SWAB
        ST (POINTER)
        CALL READ
        JUMP MAIN

ENDWORD:
        LD ITER
        INC
        ST ITER
        LD (POINTER)
        CALL READ
        SWAB
        JUMP MAIN

READ:   IN 7
        AND #0x40
        BEQ READ
        LD (POINTER)
        IN 6
        RET