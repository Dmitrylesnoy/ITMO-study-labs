ORG 0x584
ARD0: WORD 0

; ВУ-3: DR(R/W)      #6
;       SR(R), RM(W) #7

ORG 0x28C
POINTER: WORD 0x584
ITER: WORD 0

START:  CLA

INPLEN: CALL READ
        ST ITER
MAIN:
        ST (POINTER)+
        LOOP ITER
        JUMP FIRSTWORD
        HLT
FIRSTWORD:
        LD ITER
        BEQ SECONDWORD
        DEC
        ST ITER
        CALL READ
        SWAB
        ST (POINTER)
SECONDWORD:
        CALL READ
        JUMP MAIN

READ:   IN 7
        AND #0x40
        BEQ READ
        LD (POINTER)
        IN 6
        RET