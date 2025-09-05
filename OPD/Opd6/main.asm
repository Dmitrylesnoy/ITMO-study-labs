ORG 0x00
V0:     WORD    $DEFAULT,   0x180
V1:     WORD    $INT1,      0x180
V2:     WORD    $DEFAULT,   0x180
V3:     WORD    $INT3,      0x180
V4:     WORD    $DEFAULT,   0x180
V5:     WORD    $DEFAULT,   0x180
V6:     WORD    $DEFAULT,   0x180
V7:     WORD    $DEFAULT,   0x180
DEFAULT:IRET

ORG     0x047
X:      WORD    0x0
MIN:    WORD    0xFFF3
MAX:    WORD    0x0015

INT1:           ; OUTPUT RESULT
        LD      X
        HLT
        ASL
        ASL
        ADD     X
        ADD     X
        SUB     #0x9
        OUT     0x2
        IRET
INT3:           ; CHANGE SIGN
        LD      X
        HLT
        IN      0x6
        ; SXTB
        NEG
        HLT
        CALL    CHECK
        HLT
        ST      X
        IRET

START:
        DI
        CLA
        OUT     0x1
        OUT     0x5
        OUT     0xB
        OUT     0xE
        OUT     0x12
        OUT     0x16
        OUT     0x1A
        OUT     0x1E

        LD      #0x9
        OUT     0x3

        LD      #0xB
        OUT     0x7
        CLA
        EI
        JUMP    MAIN
MAIN:
        DI
        LD      X
        SUB     #3
        CALL    CHECK
        ST      X
        EI
        JUMP    MAIN

CHECK:
CHECK_MIN:
        CMP     MIN
        BPL     CHECK_MAX
        JUMP    SET_MAX
CHECK_MAX:
        CMP     MAX
        BLT     CHECK_RETURN
SET_MAX:
        LD      MAX
CHECK_RETURN:   RET