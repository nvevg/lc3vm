.ORIG x3000

LEA R0, MSG
PUTS

AND R0, R0, #0
GETC
OUT
HALT

MSG .stringz "This test echoes the symbol which was read with GETC\n"

.END