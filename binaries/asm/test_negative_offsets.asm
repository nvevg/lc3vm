        .ORIG x3000
		LEA R0, MSG
		PUTS

        ; Check LDR/STR with negative offset
        LEA R1, BUF
        ADD R1, R1, #2
        AND R2, R2, #0
        ADD R2, R2, #9
        STR R2, R1, #-1
        LDR R3, R1, #-1
        ADD R3, R3, #-9
        BRz OK2
        BRnzp FAIL

OK1
        ; Check LDI/STI
        LDI R4, P1
        STI R4, P2
        LD  R5, DST
        NOT R5, R5
        ADD R5, R5, #1
        ADD R5, R5, R4
        BRz OK2
        BRnzp FAIL

OK2
        LD R0, CHAR_O
        TRAP x21
        TRAP x25

FAIL
        LD R0, CHAR_F
        TRAP x21
        TRAP x25

MSG     .stringz "This test checks LDR/STR with negative offset and LDI/STI; Output: O - success, F - fail\n"
BUF     .BLKW 3
VAL     .FILL x3456
DST     .BLKW 1
P1      .FILL VAL
P2      .FILL DST
CHAR_O  .FILL x004F
CHAR_F  .FILL x0046

        .END