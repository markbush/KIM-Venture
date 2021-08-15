; Start locations for page 1 and page 2
; to avoid use of top of stack.
; Page 2 must be a page boundary to ensure
; messages in following page are read correctly

PART1 = $0100
PART2 = $0200
PART3 = $1780
PART4 = $0100
PART5 = $0200

include "KIM-Locations.asm"

; KIM-VENTURE © Copyright R.C.Leedom 1979
;
** = $0000
include "Light.asm"
include "ZeroPage.asm"

; Game needed for references to messages
** = PART1

include "Game.asm"

RAM     = PART1
.OUTPUT RAM,ENDPRG
** = RAM

; These zero page locations are overwritten
MOVMSH = $60
BCDLSH = $61
BCDMSH = $62
OBCELR = $63

include "Scoring.asm"

ENDPRG = **-1
;  Need this routine
** = $02E9
DONFIL: LDY #$C0       ; **($02EA)=Display speed**
DSPSPD = **-1          ; Save into symbol table.
SHMSG:  JSR LIGHT      ; Make several calls to
        JSR LIGHT      ;    the display/keyboard
        DEY            ;    subroutines.
        BNE SHMSG
        LDX DISNXM     ; Display another word?
        BNE FLM1       ; Yes.  Go do it.
        RTS            ; No.  Return with key (if
        ;                   any) in A-reg.  If no
        ;                   key hit, A = $15.

** = PART3

include "Extra.asm"
