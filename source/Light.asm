; LIGHT Subroutine. Lights KIM 7-segment dis-
;      plays with character-codes contained
;      in table WINDO.  On return, key from
;      keyboard is in A-reg (else A-reg=$15).
;      Y-reg is preserved.
LIGHT:  STY YSAV     ; Save Y-register
        LDY #0
        LDA #$7F     ; Set directional
        STA PADD     ;    registers.
        LDX #9       ; Start with leftmost
LITELP: STY TEMP     ;    character.
        LDA WINDO,Y  ; Get char to be shown.
        JSR CONVD+6  ; Use KIM monitor subr.
        INY          ; Next char on right ...
        CPY #6       ; Done all six yet?
        BCC LITELP   ; Not yet. Continue.
;
KBG:    JSR KEYS     ; Before return, sample
        JSR GETKEY   ;    the KIM keyboard.
        LDY YSAV     ; Restore Y-register.
        RTS
