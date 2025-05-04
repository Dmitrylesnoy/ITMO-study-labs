ORG 0x584
ITER: WORD 0x01
LEN: WORD 0

; ВУ-3: DR(R/W)      #6
;       SR(R), RM(W) #7

ORG 0x28C
START:  CLA

INPLEN: CALL READ
        POP
MAIN:
        ST (ITER)+
        LOOP (LEN)
        JUMP FIRSTWORD
        HLT
FIRSTWORD:
        DEC
        BEQ SECONDWORD
        INC
        CALL READ
        POP
        SWAB
SECONDWORD:
        INC
        CALL READ
        POP
        JUMP MAIN

READ:   IN 7
        AND #0x40
        BEQ READ
        IN 6
        PUSH
        RET