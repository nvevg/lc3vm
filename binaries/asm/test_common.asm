        .ORIG x3000

; -------------------------------------------------
; 0. Initialization
; -------------------------------------------------
START
        AND R0, R0, #0          ; R0 = 0
        AND R1, R1, #0          ; R1 = 0
        AND R2, R2, #0          ; R2 = 0
        AND R3, R3, #0          ; R3 = 0
        AND R4, R4, #0          ; R4 = 0
        AND R5, R5, #0          ; R5 = 0
        AND R6, R6, #0          ; R6 = 0
        AND R7, R7, #0          ; R7 = 0

; -------------------------------------------------
; 1. ADD / AND / NOT / condition flags
; -------------------------------------------------
        ADD R0, R0, #5          ; R0 = 5
        ADD R1, R0, #3          ; R1 = 8
        ADD R2, R0, R1          ; R2 = 13

        AND R3, R2, #10         ; R3 = 13 & 10 = 8
        NOT R3, R3              ; R3 = ~8 = xFFF7
        ADD R3, R3, #1          ; R3 = xFFF8 = -8, N=1

        BRzp FAIL               ; shouldn't branch
        BRn  NEG_OK             ; should branch
        BRnzp FAIL

NEG_OK
        ADD R3, R3, #8          ; R3 = 0, Z=1
        BRnp FAIL               ; shouldn't branch
        BRz  ZERO_OK            ; should branch
        BRnzp FAIL

ZERO_OK
        ADD R3, R3, #1          ; R3 = 1, P=1
        BRnz FAIL               ; shouldn't branch
        BRp  POS_OK             ; should branch
        BRnzp FAIL

POS_OK
; -------------------------------------------------
; 2. LD / ST
; -------------------------------------------------
        LD  R4, CONST_A         ; R4 = x1234
        ST  R4, SAVE_A          ; MEM[SAVE_A] = x1234

; -------------------------------------------------
; 3. LEA / LDR / STR
; -------------------------------------------------
        LEA R5, BUFFER          ; R5 = &BUFFER
        ADD R6, R6, #7          ; R6 = 7
        STR R6, R5, #0          ; BUFFER[0] = 7
        LDR R7, R5, #0          ; R7 = 7

        ADD R7, R7, #-7         ; R7 = 0
        BRz LDRSTR_OK
        BRnzp FAIL

LDRSTR_OK
; -------------------------------------------------
; 4. LDI / STI
; -------------------------------------------------
        LDI R0, PTR_TO_CONST_B  ; R0 = MEM[ MEM[PTR_TO_CONST_B] ] = CONST_B = x0041
        STI R0, PTR_TO_SAVE_B   ; MEM[ MEM[PTR_TO_SAVE_B] ] = x0041

; -------------------------------------------------
; 5. JSR / RET
; -------------------------------------------------
        JSR SUB1                ; R0 = 'O' after return
        ADD R1, R0, #0
        LD  R0, CHAR_K
        OUT                     ; prints 'K'
        ADD R0, R1, #0
        OUT                     ; prints 'O'

; -------------------------------------------------
; 6. JSRR / JMP
; -------------------------------------------------
        LEA R4, SUB2
        JSRR R4                 ; R0 = '!' after returning
        OUT                     ; prints '!'

        LEA R4, MSG
        PUTS                    ; prints " TEST"
        BRnzp DONE

; -------------------------------------------------
; FAIL
; -------------------------------------------------
FAIL
        LD  R0, CHAR_F
        OUT
        HALT

; -------------------------------------------------
; DONE
; -------------------------------------------------
DONE
        HALT

; -------------------------------------------------
; Subroutines
; -------------------------------------------------
SUB1
        LD  R0, CHAR_O
        RET

SUB2
        LD  R0, CHAR_EXCL
        JMP R7                  ; alternative to RET, but JMP

; -------------------------------------------------
; Data
; -------------------------------------------------
CONST_A        .FILL x1234
CONST_B        .FILL x0041      ; 'A'

PTR_TO_CONST_B .FILL CONST_B
PTR_TO_SAVE_B  .FILL SAVE_B

CHAR_F         .FILL x0046      ; 'F'
CHAR_K         .FILL x004B      ; 'K'
CHAR_O         .FILL x004F      ; 'O'
CHAR_EXCL      .FILL x0021      ; '!'

MSG            .STRINGZ " TEST"

SAVE_A         .BLKW 1
SAVE_B         .BLKW 1
BUFFER         .BLKW 1

        .END