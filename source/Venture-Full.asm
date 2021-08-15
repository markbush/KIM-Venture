; Start locations for page 1 and page 2
; to avoid use of top of stack.
; Page 2 must be a page boundary to ensure
; messages in following page are read correctly

PART1 = $0200
PART2 = $0300
PART3 = $0500
PART4 = $0600
PART5 = $0700

RAM     = PART1
.OUTPUT RAM,ENDPRG

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
INITZP: LDA #$89
        STA UINMAD+1
        LDA #$B5
        STA UINMAD+2
        LDA #$E7
        STA UINMAD+3
        LDA #$D7
        STA UINMAD+4
        CLD            ; Original start code
        LDA LOCNUM     ; Start at preloaded loc.
        RTS

; These zero page locations are overwritten
;    by the scoring program.
MOVMSH = UINMAD+1
BCDLSH = UINMAD+2
BCDMSH = UINMAD+3
OBCELR = UINMAD+4

include "Scoring.asm"

ENDPRG = **-1
