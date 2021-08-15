; Start locations for page 1 and page 2
; to avoid use of top of stack.
; Page 2 must be a page boundary to ensure
; messages in following page are read correctly

PART1 = $0100
PART2 = $0200
PART3 = $1780

include "KIM-Locations.asm"

; KIM-VENTURE © Copyright R.C.Leedom 1979
;
** = $0000
include "Light.asm"
include "ZeroPage.asm"

** = PART1
include "Game.asm"

RAM     = PART3
.OUTPUT RAM,ENDPRG
** = RAM

include "Extra.asm"

ENDPRG = **-1
