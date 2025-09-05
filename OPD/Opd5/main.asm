ORG     0x28C
POINTER:        WORD    0x584   ; 584 - len
                                ; 585 - stop symbol
ITER:           WORD    0
FLAG_SYM:       WORD    0xFFFF
START_LIST:     WORD    0x585

START:  CLA
INPUT_LEN:
        CALL    READ3
        ST      (POINTER)+
        INC
        ST      ITER
        CLA
        CALL    READ3
        JUMP    MAIN
FIRST_S:
        CALL    READ8
        CMP     0x585
        BEQ     SECOND_P
        ST      (POINTER)
SECOND_S:
        LD      ITER
        DEC
        ST      ITER
        BEQ     FIRST_P
        CLA
        CALL    READ8
        CMP     0x585
        BEQ     FIRST_P
        SWAB
        ADD     (POINTER)
MAIN:
        ST      (POINTER)+
        LOOP    ITER
        JUMP    FIRST_S
        JUMP    PRINT_LOOP


SECOND_P:
        LD      -(POINTER)
        JUMP    SHOW
FIRST_P:
        LD      (POINTER)
        SWAB
        JUMP    SHOW
SHOW:
        ASR
        ASR
        ASR
        ASR
        ASR
        ASR
        ASR
        ASR
        CMP     #0x30
        BLT     PRINT_LOOP
        CMP     #0x40
        BGE     PRINT_LOOP

        PUSH
        CALL    PRINT
        POP
PRINT_LOOP:
        LD      POINTER
        CMP     START_LIST
        BEQ     EXIT
        LD      FLAG_SYM
        NOT
        ST      FLAG_SYM
        BEQ     SECOND_P
        JUMP    FIRST_P

EXIT:   HLT

READ3:
        IN      7
        AND     #0x40
        BEQ     READ3
        IN      6
        RET

READ8:
        IN      0x19
        AND     #0x40
        BEQ     READ8
        IN      0x18
        RET

PRINT:
        IN      0xD
        AND     #0x40
        BEQ     PRINT
        LD      &1
	OUT     0xC
	RET