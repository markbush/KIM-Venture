; Start locations for page 1 and page 2
; to avoid use of top of stack.
; Page 2 must be a page boundary to ensure
; messages in following page are read correctly

PART1 = $0200
PART2 = $0300
PART3 = $0500

RAM     = $0000
.OUTPUT RAM,ENDPRG
** = RAM

include "KIM-Locations.asm"

; KIM-VENTURE © Copyright R.C.Leedom 1979
;
include "Light.asm"
include "ZeroPage.asm"

ENDPRG = EOCM

include "Game.asm"

** = PART3

include "Extra.asm"
