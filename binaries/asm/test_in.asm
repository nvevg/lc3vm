.ORIG x3000

LEA R0, MSG
PUTS

AND R0, R0, #0
IN
OUT
HALT

MSG .stringz "This gets a symbol from trap IN then echoes"

.END