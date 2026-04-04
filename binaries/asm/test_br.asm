        .ORIG x3000

; -----------------------------
; 1. Z flag + branch forward
; -----------------------------
        AND R0, R0, #0          ; R0 = 0, Z=1
        BRz Z_OK
        BRnzp FAIL

Z_OK
; -----------------------------
; 2. P flag + branch forward
; -----------------------------
        ADD R0, R0, #1          ; R0 = 1, P=1
        BRp P_OK
        BRnzp FAIL

P_OK
; -----------------------------
; 3. N flag + branch forward
; -----------------------------
        ADD R0, R0, #-2         ; R0 = -1, N=1
        BRn N_OK
        BRnzp FAIL

N_OK
; -----------------------------
; 4. negative offset branch back
;    countdown loop: 3 -> 2 -> 1 -> 0
; -----------------------------
        AND R1, R1, #0
        ADD R1, R1, #3          ; R1 = 3

LOOP1   ADD R1, R1, #-1         ; R1--
        BRp LOOP1               ; negative PCoffset9 backward
        BRz LOOP1_DONE
        BRnzp FAIL

LOOP1_DONE
; тут R1 должен быть 0

; -----------------------------
; 5. unconditional backward jump
; -----------------------------
        AND R2, R2, #0
        ADD R2, R2, #2

LOOP2   ADD R2, R2, #-1
        BRz LOOP2_DONE
        BRnzp LOOP2             ; unconditional backward branch
        BRnzp FAIL

LOOP2_DONE
; тут R2 должен быть 0

; -----------------------------
; 6. check branch base is PC+1
; -----------------------------
        BRnzp SKIP_FAIL
        BRnzp FAIL              ; должно быть пропущено
SKIP_FAIL

; -----------------------------
; success
; -----------------------------
        LD R0, CHAR_O
        TRAP x21
        TRAP x25

FAIL
        LD R0, CHAR_F
        TRAP x21
        TRAP x25

CHAR_O  .FILL x004F
CHAR_F  .FILL x0046

        .END