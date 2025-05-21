ORG 0x00
V0:     WORD    $DEFAULT,   0x180
V1:     WORD    $INT1,      0x180
V2:     WORD    $DEFAULT,   0x180
V3:     WORD    $INT2,      0x180
V4:     WORD    $DEFAULT,   0x180
V5:     WORD    $DEFAULT,   0x180
V6:     WORD    $DEFAULT,   0x180
V7:     WORD    $DEFAULT,   0x180
DEFAULT:IRET

ORG     0x047
X:      WORD    0x0

INT1:           ; OUTPUT RESULT
        LD      X
        HLT
        CLA
        ASL
        ASL
        ADD     X
        ADD     X
        SUB     #0x9
        OUT     0x2
        IRET
INT2:           ; CHANGE SIGN
        LD      X
        HLT
        IN      0x4
        SXTB
        NEG
        HLT
        CALL    CHECK
        HLT
        ST      X
        IRET

MIN:    WORD    0xFFED
MAX:    WORD    0x0015

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

        LD      #0xA
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
        CMP     MAX
        BPL     CHECK_MAX
        LD      MAX
        ST      X
CHECK_MAX:
        CMP     MIN
        BLT     CHECK_RETURN
        LD      MAX
        ST      X
CHECK_RETURN:   RET