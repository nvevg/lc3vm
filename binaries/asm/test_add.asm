.ORIG 3000

START
        AND R0, R0, #0          ; R0 = 0
        AND R1, R1, #0          ; R1 = 0
        AND R2, R2, #0          ; R2 = 0
        AND R3, R3, #0          ; R3 = 0
        AND R4, R4, #0          ; R4 = 0
        AND R5, R5, #0          ; R5 = 0
        AND R6, R6, #0          ; R6 = 0
        AND R7, R7, #0          ; R7 = 0

TEST
        ADD R7, R7, #7
        ADD R7, R7, #-7
        BRz OK
        BRnp FAIL

OK
        LEA R0, MSG_OK
        PUTS
        HALT

FAIL
        LEA R0, MSG_FAIL
        PUTS
        HALT

MSG_OK .stringz "OK\n"
MSG_FAIL .stringz "FAIL\n"

.END

