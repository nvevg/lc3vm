.ORIG x3000

LD R0, CHAR_F
TRAP 0x21 ; OUT

LD R0, CHAR_B
TRAP 0x21 ; OUT

HALT

CHAR_F  .FILL x0046      ; 'F'
CHAR_B  .FILL x0146 ; should be printed as 'F' too

.END