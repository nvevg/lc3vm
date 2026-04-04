.ORIG x3000

LEA R0, MSG
TRAP 0x22

HALT

MSG .STRINGZ " I am a testing string for PUTS directive\n!"

.END