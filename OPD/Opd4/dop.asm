ORG 0x458
  Z: WORD 0xDA8D
  Y: WORD 0xFDD5
  X: WORD 0x003B
  ANS: WORD 0
  ADDR_ZY: WORD 0x0458
  ADDR_X: WORD 0x045A

ORG 0x734
  ARG: WORD 0

ORG 0x743
  J: WORD 0xF65F
  K: WORD 0x00B8
  I: WORD 0x0743

ORG 0x43C
PROG:   ST #0x1                         ; прямая загрузка
        ADD $ADDR_X
        ST ADDR_X
        CLA
        ST ANS
        LD -(ADDR_X)                    ; косвенная автодекрементная
        DEC
        ST $ARG
        CALL $DOP_PROGRAM_1 ;---
        LD $ARG
        INC
        SUB ANS
        ST ANS
        LD (ADDR_ZY)+                    ; косвенная автоинкрементная
        ST $ARG
        CALL $DOP_PROGRAM_1 ;---
        LD $ARG
        DEC
        ADD ANS
        ST ANS
        LD (ADDR_ZY)                     ; косвенная относительная
        INC
        ST $ARG
        CALL $DOP_PROGRAM_1
        LD $ARG
        ADD ANS
        ST ANS
        HLT

ORG 0x735
DOP_PROGRAM_1:
        LD $ARG                         ; прямая абсолютная
        BPL ADDR_LD_J
        SUB J                           ; прямая относительная
        BMI ADDR_LD_J
        BEQ ADDR_LD_Jл
        ADD J
        ASL
        ASL
        ADD ARG
        ADD K
        JUMP ADDR_JUMP_ONE
        ADDR_LD_J:
          LD J
        ADDR_JUMP_ONE:
          ST $ARG
        RET
