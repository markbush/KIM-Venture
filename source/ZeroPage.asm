** = $0021
;
; Character look-up table.  Frequently-used
;      characters stored as 4-bit pointers
;      into part of this table (FUTBL). In-
;      frequently-used characters stored as
;      8-bit pointers into the other part
;      (IUTBL).  Note that the last two en-
;      tries are variables, used for special
;      program-controllable characters.
;
;                  Index Character
FUTBL:  .BYTE $77 ;   2     A
        .BYTE $39 ;   3     C
        .BYTE $5E ;   4     D
        .BYTE $79 ;   5     E
        .BYTE $76 ;   6     H
        .BYTE $06 ;   7     I
        .BYTE $38 ;   8     L
        .BYTE $54 ;   9     N
        .BYTE $5C ;   A     O
        .BYTE $50 ;   B     R
        .BYTE $6D ;   C     S
        .BYTE $78 ;   D     T
        .BYTE $1C ;   E     U
        .BYTE $00 ;   F     (blank)
IUTBL:  .BYTE $40 ;  10     - (dash)
        .BYTE $7C ;  11     B
        .BYTE $71 ;  12     F
        .BYTE $3D ;  13     G
;                  (continuation of IUTBL)
        .BYTE $1E ;  14     J
        .BYTE $37 ;  15     M
        .BYTE $73 ;  16     P
        .BYTE $3E ;  17     W
        .BYTE $6E ;  18     Y
        .BYTE $53 ;  19     ?
        .BYTE $08 ;  1A     . (period)
        .BYTE $5B ;  1B     2
SGNPST: .BYTE $00 ;  1C     Variable (signpost)
MBCODE: .BYTE $00 ;  1D     Variable (? or magic
;                                     button)
;
; Program variables (except for a very few
;      located elsewhere)
NMBUTS: .BYTE $00 ; No. of magic button uses
BURDEN: .BYTE $00 ; Bit #n set if carrying object #n.
DLOBAD: .BYTE $00 ; Abs address of obj to be deleted.
DRAGON: .BYTE $FF ; FF=hungry; 0=dead; 1=full.
EGOLAD: .BYTE $EC ; Current address of EGO file.
LOCAD:  .BYTE $00 ; Addr of current location file.
LINTAX: .BYTE $00 ; Pointer: 0=EGOLAD; 1=LOCAD
LOBJAD: .BYTE $00 ; Addr of last obj in file, or
                 ;      of object of interest.
LOCNUM: .BYTE $0B ; Number of current location.
MBUT:   .BYTE $FF ; Current magic button (0 - F valid).
NOBCRY: .BYTE $00 ; No. of objects carried (0 - 4).
NOBS:   .BYTE $00 ; No. of objs to be displayed (0 - 7).
OBJ:    .BYTE $00 ; Object identifier (0 - 7).
POINTR: .BYTE $00 ; ADL of message
        .BYTE $03 ; ADH of message (Constant!)
MOVES:  .BYTE $00 ; L.S. Half of number of moves.
;
; Program constants
;
OBJMSK: .BYTE %00000010 ; (1) Bird    When obj is
        .BYTE %00000100 ; (2) Rope   picked up (or is
        .BYTE %00001000 ; (3) Rod    dropped), the
        .BYTE %00010000 ; (4) File   proper bit is
        .BYTE %00100000 ; (5) Cage   ORed into (or is
        .BYTE %01000000 ; (6) Pearls NANDed out of)
        .BYTE %10000000 ; (7) Gold   BURDEN.
;
; Message addresses.  These are the ADL's of
;      the messages, all of which are assumed
;      to reside in page 3 (see POINTR+1).
;      Order of this table is paramount!
;      There is a variable thrown in here to
;      separate ADOPGR and ADBRDG ...
ADOPGR: .BYTE $34 ; Open Grate
SCDU:   .BYTE $00 ; -1,0,1,2 : Browse, Carry,
                 ;            Drop, Use
ADBRDG: .BYTE $2B ; Bridge Across Gully
OBMSAD: .BYTE $BE ; (0) Dragon
        .BYTE $DC ; (1) Bird
        .BYTE $43 ; (2) Rope
        .BYTE $E4 ; (3) Rod
        .BYTE $25 ; (4) File
        .BYTE $22 ; (5) Cage
        .BYTE $1C ; (6) Pearls
        .BYTE $46 ; (7) Gold
UINMAD: .BYTE $8C ; You Are In
        .BYTE $89 ; You Are At
        .BYTE $B5 ; I See -
CYMSAD: .BYTE $E7 ; Carry -
        .BYTE $D7 ; Drop -
        .BYTE $AE ; Use -
LNAMAD: .BYTE $06 ; Cellar         0
        .BYTE $09 ; Purple Oracle  1
        .BYTE $11 ; Red Room       2
ADSSM:  .BYTE $18 ; Stone Steps    3
        .BYTE $D3 ; Blue Den       4
ADGRM:  .BYTE $3C ; Steel Grate    5
        .BYTE $4F ; Hole           6
ADGYM:  .BYTE $38 ; Gully          7
ADRHM:  .BYTE $7B ; Royal Hall     8
        .BYTE $AB ; House          9
        .BYTE $DB ; Bird Room      A
        .BYTE $70 ; Stream         B
ADTSM:  .BYTE $52 ; Tight Shaft    C
        .BYTE $93 ; N. Pit         D
        .BYTE $EF ; Grotto         E
        .BYTE $6A ; Oyster-bed     F
        .BYTE $28 ; Chute         10
        .BYTE $73 ; E. Pit        11
        .BYTE $9B ; Attic         12
        .BYTE $EB ; S. Pit        13
        .BYTE $56 ; Tunnel        14
        .BYTE $5D ; 2-Inch Slit   15
        .BYTE $82 ; Glen          16
        .BYTE $F3 ; Forest        17
;                        CAVE MAP
; Format for file for each location in caves is as follows:
;
;  Word #                           Contents
;    0      Bit 7 = 1, bit 6 = 0. Bit 5 = 1 if location has
;             been visited during the game.  Bits 4 - 0
;             contain the location number of this file.
;
;    1      Bit 7 = 0.  Bit 6 = 1 if magic button works
;              in this location.  bits 5,4,3,2,1,0 = 1 if
;              you can leave this location in the D,U,W,S,
;              E,N direction, respectively.  This word is
;              used as the "signpost" in the Cue message.
;
; next (up  Bit 7 = 0, bit 6 = 1.  Bits 5 - 0 specify a
; to) six      location to which you may move from this
;  words       location.
;              The first of these words specifies the
;              destination for the lowest-numbered bit
;              which is set in word 1; the second specifies
;              the destination for the next-lowest bit set
;              in word 1, etc.
;              Therefore, there must be one of these words
;              for each of the first six bits (5 - 0) set
;              in word 1 of this file.
;
; next (up  Bit 7 = 0, bit 6 = 0.  Bits 5 - 0 specify the
; to) eight    object number of an object at this location.
;  words       There may be as many as eight of these words,
;              or there may be none at all.
;
RYHALL = $08
SOCM:   .BYTE $88 ; LOCNUM = 8            Royal Hall
        .BYTE $3F ; Directions: N,E,S,W,U,D
        .BYTE $4E ; N to Grotto (E)
        .BYTE $43 ; E to Stone Steps (3)
        .BYTE $4C ; S to Tight Shaft  (C)
        .BYTE $4D ; W to N. Pit (D)
        .BYTE $52 ; U to Attic (12)
        .BYTE $50 ; D to Chute (10)
DRAGAD: .BYTE $00 ; Dragon
;
        .BYTE $94 ; LOCNUM = 14           Tunnel
        .BYTE $11 ; Directions: N,U
        .BYTE $4A ; N to Bird Room (A)
        .BYTE $45 ; U to Steel Grate (5)
        .BYTE $03 ; Rod
;
        .BYTE $95 ; LOCNUM = 15           2-Inch Slit
        .BYTE $05 ; Directions: N,S
        .BYTE $4B ; N to Stream (B)
        .BYTE $45 ; S to Steel Grate (5)
;
        .BYTE $96 ; LOCNUM = 16           Glen
        .BYTE $02 ; Directions: E
        .BYTE $4B ; E to Stream (B)
;
        .BYTE $8F ; LOCNUM = F            Oyster-Bed
        .BYTE $10 ; Directions: U
        .BYTE $44 ; U to Blue Den (4)
        .BYTE $06 ; Pearls
;
        .BYTE $80 ; LOCNUM = 0            Cellar
        .BYTE $50 ; Directions: U, Magic
                  ;  (Magic to Stone Steps)
        .BYTE $49 ; U to (at) House (9)
        .BYTE $05 ; Cage
        .BYTE $02 ; Rope
        .BYTE $04 ; File
;
        .BYTE $81 ; LOCNUM = 1            Purple Oracle
        .BYTE $0A ; Directions: W,E
        .BYTE $4A ; E to Bird Room (A)
        .BYTE $43 ; W to Stone Steps (3)
;
        .BYTE $82 ; LOCNUM = 2            Red Room
        .BYTE $52 ; Directions: E,U,Magic
                  ;  (Magic to Cellar)
        .BYTE $47 ; E to Gully (7)
        .BYTE $52 ; U to Attic (12)
        .BYTE $07 ; Gold
;
        .BYTE $83 ; LOCNUM = 3            Stone Steps
        .BYTE $70 ; Directions: U,D,Magic
                  ;  (Magic to Cellar)
        .BYTE $41 ; U to Purple Oracle (1)
        .BYTE $48 ; D to Royal Hall (8)
;
        .BYTE $84 ; LOCNUM = 4            Blue Den
        .BYTE $61 ; Directions: N,D,Magic
                  ;  (Magic to Cellar)
        .BYTE $46 ; N to Hole (6)
        .BYTE $4F ; D to Oyster-Bed (F)
;
STGRAT = $05
        .BYTE $85 ; LOCNUM = 5            Steel Grate
        .BYTE $21 ; Directions: N,D
        .BYTE $55 ; N to 2-Inch Slit (15)
        .BYTE $54 ; D to Tunnel (14)
;
HOLE = $06
        .BYTE $86 ; LOCNUM = 6            Hole
        .BYTE $00 ; Directions: None!
;
GULLY = $07
        .BYTE $87 ; LOCNUM = 7            Gully
        .BYTE $0C ; Directions: S,W
        .BYTE $4E ; S to Grotto (E)
        .BYTE $42 ; W to Red Room (2)
;
        .BYTE $89 ; LOCNUM = 9            House
        .BYTE $2E ; Directions: E,S,W,D
        .BYTE $57 ; E to Forest (17)
        .BYTE $4B ; S to Stream (B)
        .BYTE $56 ; W to Glen (16)
        .BYTE $40 ; D to Cellar (0)
;
        .BYTE $8A ; LOCNUM = A            Bird Room
        .BYTE $0C ; Directions: S,W
        .BYTE $54 ; S to Tunnel (14)
        .BYTE $41 ; W to Purple Oracle (1)
        .BYTE $01 ; Bird
;
        .BYTE $8B ; LOCNUM = B            Stream
        .BYTE $0F ; Directions: N,E,S,W
        .BYTE $49 ; N to House (9)
        .BYTE $57 ; E to Forest (17)
        .BYTE $55 ; S to 2-Inch Slit (15)
        .BYTE $56 ; W to Glen (16)
;
        .BYTE $8C ; LOCNUM = C            Tight Shaft
        .BYTE $30 ; Directions: U,D
        .BYTE $52 ; U to Attic (12)
        .BYTE $53 ; D to S. Pit (13)
;
NPIT = $0D
        .BYTE $8D ; LOCNUM = D            N. Pit
        .BYTE $2A ; Directions: E,W,D
        .BYTE $51 ; E to E. Pit (11)
        .BYTE $50 ; W to Chute (10)
        .BYTE $46 ; D to Hole (6)
;
        .BYTE $8E ; LOCNUM = E            Grotto
        .BYTE $0D ; Directions: N,S,W
        .BYTE $43 ; N to Stone Steps (3)
        .BYTE $4D ; S to N. Pit (D)
        .BYTE $47 ; W to Gully (7)
;
        .BYTE $97 ; LOCNUM = 17           Forest
        .BYTE $09 ; Directions: N,W
        .BYTE $49 ; N to House (9)
        .BYTE $4B ; W to Stream (B)
;
        .BYTE $90 ; LOCNUM = 10           Chute
        .BYTE $20 ; Directions: D
        .BYTE $4C ; D to Tight Shaft (C)
;
        .BYTE $91 ; LOCNUM = 11           E. Pit
        .BYTE $35 ; Directions: N,S,U,D
        .BYTE $4D ; N to N. Pit (D)
        .BYTE $53 ; S to S. Pit (13)
        .BYTE $4C ; U to Tight Shaft (C)
        .BYTE $46 ; D to Hole (6)
;
        .BYTE $92 ; LOCNUM = 12           Attic
        .BYTE $20 ; Directions: D
        .BYTE $48 ; D to Royal Hall (8)
;
        .BYTE $93 ; LOCNUM = 13           S. Pit
        .BYTE $33 ; Directions: N,E,U,D
        .BYTE $4D ; N to N. Pit (D)
        .BYTE $51 ; E to E. Pit (11)
        .BYTE $52 ; U to Attic (12)
        .BYTE $44 ; D to Blue Den (4)
;
; EGO File ("File of the self")
;       Behaves like any other location, except that the
;       "Directions" word is used for the Most Significant
;       Half of the double precision MOVES counter.  This
;       file is initially empty; objects picked up by the
;       adventurer are placed here until they are dropped.
;
        .BYTE $9F ; LOCNUM = 1F           EGO File
        .BYTE $00 ; M.S.H. of MOVES
EOCM:   .BYTE $9F ; End Of Cave Map Flag (a constant)
;
; KIM monitor locations used by KIM-VENTURE
;
YSAV:   .BYTE $00 ; Used by LIGHT S/R to save Y-Reg.
                  ;     This location is destroyed each
                  ;     time ADDOBJ is called -- EOCM
                  ;     gets written here.
WINDO:  .BYTE $00,$00,$00,$00,$00,$00,$00
                  ; Display window for LIGHT S/R. Really
                  ;     only need six, but for the fact
                  ;     that FILMSG keeps unpacking msg's
                  ;     till it ends on a whole byte --
                  ;     thus clobbering 1 or 2 extras...
DIR:    .BYTE $00 ; Direction moved. 0=N,....,5=D.
;
** = $00FC
TEMP:   .BYTE $00 ; Used by LIGHT and monitor together.
LCTR:   .BYTE $00 ; Letter-counter for FILMSG.
DISNXM: .BYTE $00 ; Display-next-message flag.  If nonzero,
                  ;     FILMSG will add DISNXM to ADL of
                  ;     message (POINTR) and start over.
