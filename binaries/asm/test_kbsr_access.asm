        .ORIG x3000
		LEA R0, MSG
		PUTS

        LD   R1, KBSR_ADDR
        LD   R2, KBDR_ADDR

POLL    LDR  R3, R1, #0      ; R3 = Mem[KBSR]
        BRzp POLL            ; if bits15 == 0, poll more

        LDR  R0, R2, #0      ; R0 = Mem[KBDR]
        TRAP x21             ; OUT
        TRAP x25             ; HALT
MSG .stringz "This test polls KBSR memory mapped register, and then echoes the symbol from KBDR\n"
KBSR_ADDR .FILL xFE00
KBDR_ADDR .FILL xFE02

        .END