; START segment.  Begin here using keys
;       AD , 0100,  GO .
;
** = PART1
START:  CLD            ; PROGRAM START POINT.
        LDA LOCNUM     ; Start at preloaded loc.
;
; NEWLOC segment.  Program comes here any
;      time a location is entered.
;
NEWLOC: STA LOCNUM     ; New location entry.
        LDX #SOCM-2    ; Start-of-cave-map is
                       ;    used as starting
                       ;    point for file search.
CKLNUM: INX
CKLNLP: INX
        LDA 0,X        ; Is this a start-of file?
        BPL CKLNLP     ; No. Keep looking.
        STX LOCAD      ; Yes, save file address,
        AND #$1F       ;    and see if it's the
        CMP LOCNUM     ;    one he moved to ...
        BNE CKLNUM     ; No.  Look for next file.
        ORA #$A0       ; Yes. Indicate "visiting
        STA 0,X        ;    here" for scoring.
        LDY 1,X        ; Get "Directions" word.
        AND #1         ; Set X-reg for "IN" if
        TAX            ;    LOCNUM even; else "AT".
        STY SGNPST     ; Signpost = "Directions".
        LDY UINMAD,X   ; Show "You are in"
        JSR FILMSG     ;    or "You are at".
        LDX LOCNUM
        LDY LNAMAD,X
        JSR FILMSG     ; Show location name.
        LDX LOCNUM
        DEX            ; At Purple Oracle?
        BNE MVTOBH     ; No, move to obj-handler.
        LDA TIMER      ; Yes, so pick
        AND #$F        ;    a random magic button.
        TAX
        STA MBUT       ; Save the button
        LDA DIGCOD,X   ;    and the display-code
        STA MBCODE     ;   (from monitor) for it.
        LDY #<ASSMAD    ; Show "A Sign Says
        JSR FILMSG     ;    Magic Button Is *"
MVTOBH: LDA #$B        ; Set up for "I See" in
OBHLNK: JMP OBHNDL     ;    Object-Handler.
; MNMVLP (Main Move Loop).  Program comes here
;      after each move and stays here till next.
;
MNMVLP: LDX EGOLAD     ; Enter here after each move.
        INC MOVES      ; Move count.  Overflow?
        BNE MNLOOP     ; No.
        INC 1,X        ; Bump MSH of MOVES.
MNLOOP: LDY #<CUEMAD    ; Loop here till he moves.
        JSR FILMSG     ; Show "?", Signpost.
        CMP #6         ; Key = 0 - 5? (Dir?)
        BCS MNLCON
        JMP SPROC      ; Yes. Do Special Proc.
MNLCON: CMP #$B        ; Key = 6 - A? (No-op?)
        BCC MNMVLP     ; Yes. Count as a move.
STLINK: BEQ START      ; Key = B.  To Browse, act
                       ;     as if just moved here.
        CMP #$F        ; Key = C,D, or E?
        BCC OBHLNK     ; Yes. Handle objects. (Go
                       ;     via NEWLOC.)
        BNE MNLOOP     ; If key is none of the
                       ;     above, and not F,
                       ;     do nothing.
        ; F key has been hit.  Magic Processing.
        LDA #$53        ; Insert "?" to ask what
        STA MBCODE     ;     Magic Button is.
        LDY #<MBIMAD
        JSR FILMSG     ; Ask the question.
        LDY #$E1       ; TODO: Coded as #CUEMAD
        CMP MBUT       ; Did he hit the right one?
        BNE MNMVLP     ; No. Count as a move.
        LDA #3         ; Yes, so magic might work.
        LDX LOCNUM     ; In the Cellar?
        BEQ NEWLNK     ; Yes. To Stone Steps now.
        LDA #0         ; Is location number
        CPX #5         ;    higher than 4?
        BCS NOJMSG     ; Yes. Spell won't work.
        DEX            ; At Purple Oracle?
        BEQ STLINK     ; Yes. Spell not only won't
                       ; work, it changes!
        INC NMBUTS     ; OK.  At Stone Steps, Red
                       ;     Room, or Blue Den.
                       ;     Bump M.B. count, and
        NOP            ; Original used absolute
                       ;    addressing for previous
                       ;    INC but ZP available.
                       ;    Use NOP to match addresses.
        BNE NEWLNK     ;     go to Cellar (via
                       ;     MOVER).
;
; MOVER   Provesses direction commands (if you
;      made it through SPROC).
;
MOVER:  LDX LOCAD      ; Address current file,
        LDA 1,X        ;    pick up "Directions",
        LDY #$FF       ;    and init check count.
CKNDIR: INY
        LSR            ; This direction OK?
        BCC CKDLP      ; No.  See if done.
        INX            ; Yes. Bump pointer, and
        CPY DIR        ;    see if this is the
                       ;    desired direction.
        BEQ DIROK      ; It is.  Go do it.
CKDLP:  CPY #5         ; Isn't. Tried all dir's?
        BNE CKNDIR     ;        No, keep on...
        LDY #<CNTMAD    ; Show "Cannot" and
MVMSML: JMP MSGAML     ;    return to main loop.
        ;
DIROK:  LDA 1,X        ; Pick up new location
        AND #$1F       ;    number, get LS 5
                       ;    bits for LOCNUM, and
NEWLNK: JMP NEWLOC     ; Go to new location.
;
; OBUSE (Object Use, or Employment)
;
OBUSE:  LDY OBJ
        LDA LOCNUM
        DEY            ; Is object Bird?
        BEQ OBUBRD     ; Yes.  Go use it.
        DEY            ; Is object Rope?
        BEQ OBUROP     ; Yes.  Go use it.
        LDX #GULLY
        DEY            ; Is object Rod?
        BEQ OFLROD     ; Yes.  Go to File/Rod use.
        LDX #STGRAT
        DEY            ; Is object File?
        BEQ OFLROD     ; Yes.  Go to File/Rod use.
NOJMSG: LDY #<NOJMAD    ; Show
        BNE MVMSML     ;   "No Joy" (via MOVER).
;
OBUBRD: CMP #RYHALL    ; Use Bird at Royal Hall?
        BNE NOJMSG     ; No -- nothing happens.
        LDA DRAGON     ; Yes.  Dragon hungry?
        BEQ NOJMSG     ; No, dead.  No effect.
        INY            ; Yes! "Using" Bird is
        STY DRAGON     ;    Like "feeding him to
        LDY #<ADDGMS    ;    Dragon!" Show
        JSR FILMSG     ;   "Dragon Eats Bird".
        JMP OBDELE     ; Go delete Bird.
;
OBUROP: CMP #HOLE      ; Used Rope in Hole?
        BNE NOJMSG     ; No.  No effect.
        LDA #NPIT      ; Yes, so got out to
        BPL NEWLNK     ;    N. Pit (via MOVER).
;
OFLROD: CPX LOCNUM     ; Used File at Grate or
                       ;     Rod at Gully?
        BNE NOJMSG     ; No.  No effect.
        LDA ADOPGR-5,X ; Yes.  Is Grate open or
        CMP LNAMAD,X   ;   is Bridge made?
        BEQ NOJMSG     ; Yes. No effect.
        STA LNAMAD,X   ; No.  Open Grate or
                       ;   make the Bridge.
        TXA            ; Show the new state
        BPL NEWLNK     ;    of this location.
;
;
; DELOBJ (Delete Object) Sunroutine.
;      Call with DLOBAD = page zero address
;      of the object to be deleted from file.
;
DELOBJ: LDX DLOBAD     ; Point to obj to delete.
DOBLP:  LDA 1,X        ; Move all files down
        STA 0,X        ;    one location until
        INX            ;    obj is overwritten.
        CPX #EOCM+1    ; Done yet?
        BNE DOBLP      ; No, continue.
        RTS            ; Yes, return.
;
;
; OBHNDL (Object-Handler) segment.  Entered
;      with A-reg filled with either of key-
;      depressions B,C,D, or E.  (Arrival at
;      a location looks like a B-keyin.)
;      B=Browse. Produces list of objects,
;         with no action allowed.
;      C,D,E = Carry, Drop, Employ.  Each
;         produces object list, but during
;         list, any key causes action on
;         object currently displayed.
;
** = PART2
OBHNDL: SEC            ; Change B,C,D, or E to
        SBC #$D        ;    -2,-1,0, or 1.
        TAX
        LSR            ; Set up Y-reg for LOBSCH:
        AND #1         ;    Y=1 (current loc)-B,C.
        TAY            ;    Y=0 (EGO file)-D,E.
        EOR #1         ; Flip state to get "loc-
        STA LINTAX     ;    of-interest-adr-index".
        INX            ; Change B,C,D, or E to
        STX SCDU       ;    -1,0,1, or 2 for SCDU.
        JSR LOBSCH     ; Get LOBJAD, no. of obj's.
        STY NOBS       ; Save no. of obj's for loop.
        BEQ MLLINK     ; If nothing here, done!
        ;
        ; Begin object-handling processing...
        LDX SCDU       ; "Carry" command?
        BNE OBHMDS     ; No.  Continue.
        LDA NOBCRY     ; Yes, but is he already
        CMP #4         ;    carrying four things?
        BNE OBHMDS     ; No.  Continue.
HOWMSG: LDY #<HOWMAD    ; Show "How ? ".
MSGAML: JSR FILMSG     ; Display the message.
MLLINK: JMP MNMVLP     ; Return to Main Move Loop.
        ;
OBHMDS: LDY CYMSAD,X   ; Show "I See-", "Carry-",
        JSR FILMSG     ;    "Drop-", or "Use - ".
        ;
OBNEXD: DEC NOBS       ; Showed all obj's yet?
        BMI MLLINK     ; Yes.  nothing else to do.
        LDY LOBJAD     ; Save addr of this object
        STY DLOBAD     ;    in case it's to be
                       ;    deleted from the file.
        LDX 0,Y        ; Save the
        STX OBJ        ;    object number.
        LDY OBMSAD,X   ; Show the
        JSR FILMSG     ;    object's name.
        LDY SCDU       ; Just looking?
        BMI OBN        ;    Yes, display next one.
        CMP #$15       ; Carry/Drop/Use this obj?
        BNE OBHXQT     ; Yes.  Execute obj-handle.
OBN:    DEC LOBJAD     ; Point to next object,
        BNE OBNEXD     ;    and show it.
        ;
        ; Execution of object-handling begins:
OBHXQT: DEY
        BMI OBCARY     ; Go Carry object.
        BEQ OBDROP     ; Go Drop object.
        JMP OBUSE      ; Go Use object.
        ;
        ; OBCARY (Object-Carrying) segment.
        ;
OBCARY: LDY #<CNTMAD
        LDX OBJ
        DEX            ; Is object Bird?
        BNE OBCDCK     ; No, see if Dragon.
        LDA BURDEN     ; Yes.  Is he carrying
        AND #$28       ;    the cage and
        CMP #$20       ;    not the rod?
        BNE HOWMSG     ; No. "How carry Bird?"
        ;
OBCDCK: TXA            ; Is obj Dragon (X=$FF)?
        BMI MSGAML     ; Yes.  Show "Cannot".
        ;
        ; Finally ready to carry the
        ;   indicated object...
OBOKCY: INC NOBCRY     ; OK to carry object.
        LDA BURDEN     ;    Bump carry count,
        ORA OBJMSK,X   ;    and indicate
        STA BURDEN     ;    what's being carried.
        JSR ADDOBJ     ; Add obj to EGO file.
OBDELL: DEC EGOLAD     ; Move everything down 1,
        JSR DELOBJ     ;    and delete object
        ;                   from location file.
        ;
DONMSG: LDY #<DONE
        BNE MSGAML     ; Show "Done" message
        ;                   and return to
        ;                   Main Move Loop.
        ;
        ;
        ; OBDROP (Object-Dropping) segment.
        ;
OBDROP: INC EGOLAD     ; Move everything up 1,
        JSR ADDOBJ     ;    and add object to
        INC DLOBAD     ;    location file.
OBDELE: DEC NOBCRY     ; Delete object from
        LDX OBJ        ;    EGO file,
        LDA BURDEN     ;    indicate one less
        SEC            ;    object carried,
        SBC OBJMSK-1,X ;    and remove
        STA BURDEN     ;    "object-flag" from
        JSR DELOBJ     ;    Burden list.
        ;
        LDA DRAGON     ; Is Dragon alive&hungry?
        BPL DONMSG     ; No.  All done.
        ;                TODO: BPL recorded as 1C in hex dump
        LDX OBJ        ; Was Bird
        DEX            ;    just dropped?
        BNE DONMSG
        LDA LOCNUM     ; Yes, are we
        CMP #8         ;    at Royal Hall?
        BNE DONMSG
        LDA #DRAGAD    ; Yes, so Dragon is
        STA DLOBAD     ;    scared off.
        STX DRAGON
        LDA #5         ; Change msg length
        STA ADDGMS     ;    so proper msg
        LDY #<ADDGMS    ;    is shown, and
        JSR FILMSG     ;    go show it.
        BEQ OBDELL     ; Delete Dragon.
; FILMSG (Fill WINDO, display message) Sub-
;      routine.  Unpacks and displays a word
;      or series of words, starting at ADL
;      specified by Y-reg at time of call.
;      Message is in page specified by con-
;      tents of POINTR+1.  Calls LIGHT S/R.
FLM1:   CLC
        TXA            ; To display next word,
        ADC POINTR     ;    add DISNXM to POINTR,
        TAY            ;    place in Y-reg, and
        ;                   call S/R again...
FILMSG: STY POINTR     ; S/R ENTRY POINT *******
                            ;  Save msg ADL.
        LDX #0         ; Clear letter-counter.
        LDY #0         ; Clear byte pointer.
        STX DISNXM     ; Clear "continue" flag.
MFLOOP: STX LCTR       ; Save letter-counter.
        CLC            ; C=0 to address FUTBL.
MFLAP:  LDA (POINTR),Y ; Get next byte, and
        PHA            ;    save a copy.
        ROR            ; Shift in CARRY bit,
        LSR            ;    then move CARRY+MSH
        LSR            ;    to lower part
        LSR            ;    of the byte.
        BEQ MSHRPT     ; MSH=0 means LSH is a
                       ;    repeat pointer.
        CMP #1         ; MSH=1 means LSH is an
        BEQ IUBYT      ;    index to IUTBL.
        TAX            ; MSH>=2, so use C + MSH
        LDA FUTBL-2,X  ;    to point to char-code.
        ;
STMSH:  LDX LCTR       ; Use letter-counter to
        STA WINDO,X    ;    put code in window.
        PLA            ; Get copy of current byte.
        INX            ; Increment and
        STX LCTR       ;    save the letter count.
                       ; (At this point, FILMSG
                       ; could be done, and a
                       ; check should be made
                       ; for "Done 6?".  To
                       ; save 4 bytes, I let it
                       ; run till ending on a
                       ; byte boundary....RCL)
        AND #$F        ; Extract LSH of the byte.
        CMP #1         ; If =1, next byte is IU
        BEQ IUNXWD     ;    letter code.
        ;
STLSH:  TAX            ; Use this byte's LSH as
        LDA FUTBL-2,X  ;    FUTBL pointer.
        LDX LCTR       ; Use letter-counter to
        STA WINDO,X    ;    put code in window.
        INY            ; Bump both pointers
        INX
        CPX #6         ; Done yet?
        BCC MFLOOP     ; No.  Continue.
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
MSHRPT: PLA            ; The current byte is an
        STA DISNXM     ;    offset to next msg.
        INY            ;    Save it, point to
        BPL MFLOOP     ;    next byte, continue.
        ;
IUBYT:  PLA            ; The current byte is an
        BPL STLSH      ;    IUTBL pointer. Use it.
        ;
IUNXWD: INY            ; Point to next byte.
        SEC            ; C=1 will add 16 to FUTBL
        BCS MFLAP      ;    pointer; thus we have
        ;                   an IUTBL pointer.
;
; Message.  Starting at a point in page 3
;      specified by POINTR, the FILMS S/R
;      examines this data a half byte at a
;      time to extract a 6-character message.
;      Each half-byte may be one of the
;      following:
;  0, meaning "Save the next half-byte. When
;      the current display is done, use that value to advance POINTR, and go through
;      FILMSG again for a new display."
;  1, meaning "Use the value of the next
;      half-byte as a pointer into IUTBL."
;  2 - F, meaning "Use this value as a pointer
;      into FUTBL."
;      (See IUTBL and FUTBL at $002F, $0021.)
; (Key to character-codes is at the end of this table.)
        .BYTE $35,$88,$2B                     ; CELLAR  CE LL AR
        .BYTE $05,$16,$EB,$16,$85,$AB,$23,$85 ; PURPLO  #5 Px UR Px LE OR AC LE
        .BYTE $03,$FB,$54,$FF,$BA,$A1,$5F     ; REDRM   #3  R ED    RO OM x*
        .BYTE $07,$CD,$A9,$5F                 ; STSTPS  #7 ST ON E
        .BYTE $16,$52,$B8                     ; PEARLS  Px EA RL
        .BYTE $CD,$51,$6C                     ;         ST EP xS
        .BYTE $F3,$21,$35                     ; CAGE     C AG xE
        .BYTE $F1,$27,$85                     ; FILE     F xI LE
        .BYTE $F3,$6E,$D5                     ; CHUTE    C HU TE
        .BYTE $05,$11,$B7,$41,$35             ; BRAGM   #5 Bx RI DG xE
        .BYTE $08,$23,$BA,$CC                 ;         #8 AC RO SS
        .BYTE $0C,$FA,$16,$59                 ; OPNGRM  #C  O Px EN
        .BYTE $F1,$3E,$88,$18                 ; GULLY    G xU LL Yx
        .BYTE $04,$CD,$55,$8F                 ; STGRAT  #4 ST EE L
        .BYTE $13,$B2,$D5                     ;         Gx RA TE
        .BYTE $FB,$A1,$65                     ; ROPE     R OP xE
        .BYTE $F1,$3A,$84                     ; GOLD     G xO LD
        .BYTE $F3,$21,$35                     ; CAGE     C AG xE
DONE:   .BYTE $F4,$A9,$5F                     ; DONMAD   D ON E
        .BYTE $2F,$6A,$85                     ; HOLE    A  HO LE
        .BYTE $07,$FD,$71,$36                 ; TSHAFT  #7  T IG xH
        .BYTE $DE,$99,$58                     ; TUNNEL  TU NN EL
        .BYTE $FC,$62,$12,$DF                 ;          S HA Fx T*
        .BYTE $05,$1B,$10,$79,$36             ; SLIT    #5 2x -x IN CH
        .BYTE $04,$FC,$87,$DF                 ;         #4  S LI T
        .BYTE $09,$79,$FD,$65                 ;         #9 IN  T HE
        .BYTE $0D,$A1,$8C,$D5,$BF             ; OYSTRB  #D OY xS TE R*
        .BYTE $08                             ;         #08
        .BYTE $CD,$B5,$21                     ; STREAM  ST RE AM
        .BYTE $51,$AF,$16,$7D                 ; EPIT    E. x  Px IT
        .BYTE $F1,$01,$15,$4F                 ;          - xB xE D
        .BYTE $04,$BA,$18,$28,$F6,$28,$8F     ; RYHALL  #4 RO Yx AL  H AL L
        .BYTE $2F,$13,$85                     ; GLEN    A  Gx LE
NOJMAD: .BYTE $9A,$F1,$4A,$18                 ; NOJOY   NO  J xO Yx
        .BYTE $EF,$BF,$2D                     ; URAT    U  R  AT
        .BYTE $EF,$BF,$79                     ; URIN    U  R  IN
ASSMAD: .BYTE $08,$2F,$C7,$13                 ; ASSMAD  #8 A  SI Gx
        .BYTE $91,$AF,$16,$7D                 ; NPIT    N. x  Px IT
        .BYTE $07,$FC,$21,$8C                 ;         #7  S AY xS
        .BYTE $F2,$DD,$73                     ; ATTIC    A TT IC
MBIMAD: .BYTE $05,$15,$21,$37,$3F             ; MBISAD  #5 Mx AG xI C
        .BYTE $05,$11,$ED,$DA,$9F             ;         #5 Bx UT TO N*
        .BYTE $F7,$CF,$1D                     ;          I S  @x
        .BYTE $F6,$AE,$C5                     ; HOUSE    H OU SE
        .BYTE $FE,$C5,$F1                     ; USE      U SE  -
HBDMS:  .BYTE $07,$62,$8D,$54                 ; HBDRGN  #7 HA LT ED
        .BYTE $7F,$C5,$51                     ; ISEE    I  SE E-
        .BYTE $06,$11,$18,$FD,$65             ;         Bx Yx  T HE
ADDGMS: .BYTE $0E                             ; ADDGMS  #E (Change to #5 for
                                              ;            Scared Out...)
        .BYTE $4B,$21,$3A,$9F                 ; ADDRAG  DR AG xO N*
        .BYTE $04,$C3,$2B,$54                 ;         #4 SC AR ED
        .BYTE $09,$AE,$DF,$11,$18             ;         #9 OU T  Bx Yx
        .BYTE $04,$F5,$2D,$CF                 ;         #4  E AT S
        .BYTE $0D,$87,$DD,$85                 ;         #D LI TT LE
        .BYTE $0F,$F1,$18,$E5                 ; BLUDEN  #F  B xL UE
        .BYTE $F4,$BA,$16,$10                 ; DROP     D RO Px -x
        .BYTE $04                             ; BIRDRM  #4
        .BYTE $F1,$17,$B4                     ; BIRD     B xI RD
        .BYTE $FB,$AA,$15                     ;          R OO Mx
        .BYTE $F4,$59                         ;          D EN
        .BYTE $FF,$BA,$4F                     ; ROD        RO D
        .BYTE $32,$BB,$18,$10                 ; CARRY   CA RR Yx -x
        .BYTE $C1,$AF,$16,$7D                 ; SPIT    S. x  Px IT
        .BYTE $13,$BA,$DD,$AF                 ; GROTTO  Gx RO TT O*
        .BYTE $12,$AB,$5C,$DF                 ; FOREST  Fx OR ES T*
CNTMAD: .BYTE $32,$99,$AD                     ; CNTMAD  CA NN OT
HOWMAD: .BYTE $6A,$17                         ; HOWMAD  HO Wx
CUEMAD: .BYTE $F1,$9F,$FF,$1C                 ; CUEMAD   ? x     &x
;
; Key to characters used in right-hand column
;      of above table:
;    Letter or space -- the FUTBL 4-bit code
;          for that letter or space.
;
;    Letter, dash, "?", ".", or 2; followed
;    by "x" -- the IUTBL 8-bit code for that
;          character.
;
;    "@x" -- the IUTBL 8-bit code for the
;          character stored in IUTBL (by the
;          NEWLOC and MNMVLP segments) as
;          part of the Magic Button message.
;    "&x" -- the 8-bit code for the Signpost
;          character stored in IUTBL (by the
;          NEWLOC segment) as part of CUE msg.
;
;    #n -- the number, n, of bytes (in hex) to
;          advance POINTR in order to point to
;          the next successive message.
;
;    * -- a "wasted" half-byte
;
; ............................................
