RAM     = $0000
.OUTPUT RAM,ENDPRG

; Start locations for page 1 and page 2
; to avoid use of top of stack.
; Page 2 must be a page boundary to ensure
; messages in following page are read correctly

PART1 = $0200
PART2 = $0300
PART3 = $0500
PART4 = $0600
PART5 = $0700

; These zero page locations are overwritten
;    by the scoring program.
MOVMSH = OBMSAD
BCDLSH = OBMSAD+1
BCDMSH = OBMSAD+2
OBCELR = OBMSAD+3

include "KIM-Locations.asm"

; KIM-VENTURE © Copyright R.C.Leedom 1979
;
** = $0000
include "Light.asm"
include "ZeroPage.asm"

** = PART1

include "Game.asm"

** = PART1
; Add code to restore the values that are
;    used by the scoring program
        JSR INITZP

** = PART3

include "Extra.asm"
; Put zero page restore at end of Extra code
INITZP: LDA #$BE
        STA OBMSAD
        LDA #$DC
        STA OBMSAD+1
        LDA #$43
        STA OBMSAD+2
        LDA #$E4
        STA OBMSAD+3
        CLD            ; Original start code
        LDA LOCNUM     ; Start at preloaded loc.
        RTS

include "Scoring.asm"

ENDPRG = **-1
