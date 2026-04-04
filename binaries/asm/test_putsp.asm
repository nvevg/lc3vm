.ORIG x3000

LEA R0, DESCR
TRAP x24

LEA R0, MSG
TRAP x24
LEA R0, MSG2
TRAP x24
TRAP x25

DESCR .stringz "This tests outputs ABCABCD using PUTSP trap\n"

MSG     .FILL x4241     ; 'A' 'B'
        .FILL x0043     ; 'C' '\0'
        .FILL x0000

MSG2     .FILL x4241     ; 'A' 'B'
        .FILL x4443     ; 'D' 'C' '\0'
        .FILL x0000

.END