; K - V SCORE   © Copyright R.C.Leedom 1979
;      The K-V SCORE program is to be loaded
;      immediately following a KIM-VENTURE
;      game, and run starting at location
;      $100.  K-V SCORE will provide a rating
;      (which may be from Class A all the
;      way down to J, or -- at the bottom --
;      Class O), and a count of the moves
;      made by the player (up to 9999).
** = PART4
KVSCOR: JMP BGNSCR
;
; LOCSCH (Location Search) Subroutine.
;      Created from KIM-VENTURE's NEWLOC,
;      this S/R (when called with A-reg =
;      location number) will search for the
;      location file and return LOCAD
;      in the X-register.
LOCSCH: STA LOCNUM     ; Save location number.
        LDX #SOCM-2    ; Start-of-cave-map is
                       ;    used as starting
                       ;    point for file search.
CKLNUMS: INX
CKLNLPS: INX
        LDA 0,X        ; Is this a start-of-file?
        BPL CKLNLPS    ; No.  Keep looking.
        STX LOCAD      ; Yes, save file address,
        AND #$1F       ;    and see if it's the
        CMP LOCNUM     ;    one we want ...
        BNE CKLNUMS    ; No. Look for next file.
        RTS            ; Yes.  Done, so return.
;
; VISCHK (Visit Check) Subroutine.  Call with
;      A-reg = location number.  S/R will re-
;      turn A-reg <0 if location was visited,
;      else A-reg >0.
VISCHK: JSR LOCSCH     ; Go get LOCAD in X-reg.
        LDA 0,X        ; Now we get header word of
        ASL            ; location file, shifted
        ASL            ; to show "visit" bit,
        RTS            ; and return.
;
SCRMSG: .BYTE $6D,$58,$5C ; Data for "SCORE " msg.
        .BYTE $50,$79,$00
;
; BGNSCR (Begin Scoring) segment. (Main prog.)
;
BGNSCR: LDX #6         ; Display the six
SCMLP:  LDA SCRMSG-1,X ;   characters of the
        STA WINDO-1,X  ;   score message:
        DEX            ;   save them in the
        BNE SCMLP      ;   window, indicate
        STX DISNXM     ;   "no more displays",
        JSR DONFIL     ;   and call a few LIGHTs.
; MVCONV (Move Conversion) segment.  Converts
;      the double precision move counter to
;      a decimal number (up to four digits).
MVCONV: CLC
        SED            ; Set decimal mode.
        LDA #0         ; Clear Binary Coded
        STA BCDLSH     ;   Decimal, Least and
        STA BCDMSH     ;   Most Signif. Halves.
        LDA MOVES      ; Is LSH = 0?
        BEQ MSADD      ; Yes, go add up MSH.
        ;
LSADD:  LDA BCDLSH     ; For LSH, a double
        ADC #1         ;   precision add
        STA BCDLSH     ;   of one count
        LDA BCDMSH     ;   for each unit
        ADC #0         ;   of the LSH of
        STA BCDMSH     ;   the move counter.
        DEC MOVES
        BNE LSADD
        ;
MSADD:  LDX EGOLAD     ; Get MSH of the move
        LDA 1,X        ;   counter, save it,
        STA MOVMSH     ;   and if zero, we
        BEQ DSPFIL     ;   are done...
        ;
MSAD1:  LDA BCDLSH     ; For MSH, a double
        ADC #$56       ;   precision add of
        STA BCDLSH     ;   256 counts
        LDA BCDMSH     ;   for each unit
        ADC #2         ;   of the MSH of
        STA BCDMSH     ;   the move counter.
        DEC 1,X
        BNE MSAD1
;
; DSPFIL (Display Fill) segment.  Fills the
;      display window with digits corresponding
;      to score, and blanks (up to) two leading
;      zeroes.
DSPFIL: CLD
        LDA BCDLSH     ; For LSH of score,
        AND #$F        ;   get lower digit and
        TAX            ;   corresponding segment
        LDA DIGCOD,X   ;   code from monitor, and
        STA WINDO+5    ;   put in display window.
        LDA BCDLSH     ; Similarly, get upper
        LSR            ;   digit of LSH of
        LSR
        LSR
        LSR            ;   score, use to get
        TAX            ;   segment code from
        LDA DIGCOD,X   ;   KIM monitor, and put
        STA WINDO+4    ;   in display window.
        LDA BCDMSH     ; For MSH of score,
        LSR            ;   first get upper digit.
        LSR
        LSR
        LSR
        TAY            ; Is it zero?
        BEQ LZ1BNK     ; Yes, blank it.
        ;
        TAX            ; Use nonzero upper digit
        LDA DIGCOD,X   ;   to get segment code.
LZ1BNK: STA WINDO+2    ; Put MS digit in window.
        LDA BCDMSH     ; Get next most signif.
        AND #$F        ;   digit and
        TAX            ;   save it.
        TYA            ; Was MS digit zero?
        BNE NOBNK2     ; No, so don't blank
        ;                  this digit.
        TXA            ; Yes, is this one zero?
        BEQ LZBNK2     ; Yes -- both zero!  Go
        ;                  blank this one too.
NOBNK2: LDA DIGCOD,X   ; Get code for 2nd MSD.
LZBNK2: STA WINDO+3    ; Fill the remaining
        ;                  slot of the window.
; CLASS segment.  The remaining code
;      determines the player's classificatino
;      based on what was accomplished in the
;      course of the game.
CLASS:  LDX #$71       ; Class F if
        LDA LOCNUM
        CMP #6         ;    in the hole
        BNE CELSCH
        LDA BURDEN
        AND #4         ;    without the rope.
        BEQ WSLNK      ;    Go show "F".
        ;
CELSCH: LDA #0         ; Cellar search to see
        STA OBCELR     ;    if any treasures
        JSR LOCSCH     ;    have been left
        LDY #1         ;    here...
        JSR LOBSCH
        STY NOBS       ; Anything here?
        BEQ CVLINK     ; No, check cave visits.
OBCSET: LDY 0,X        ; Yes, so set up
        LDA OBCELR     ;    OBCELR which will
        ORA OBJMSK-1,Y ;    have a bit set for
        STA OBCELR     ;    each object left
        DEX            ;    in the cellar.
        DEC NOBS
        BNE OBCSET
        LDX #$39       ; Class C if only one
        ASL            ;    treasure here.
        BMI PICLNK     ; Pearls. See if Gold too.
        BCS WSLNK      ; No Pearls, Gold only.
CVLINK: JMP CAVIS      ; Neither, check cave visits.
WSLNK:  JMP WINSET     ; Go set window with class.
PICLNK: JMP PIC        ; Continue cellar check.

; Note that 1D9 - 1FF not used.  Cellar
;      check continues in Page 2...  At
;      this point we have verified that
;      the Pearls are there and are
;      testing for Gold.  A-reg has been
;      preloaded with Class C.
** = PART5
PIC:    BCC WSLNK      ; Pearls only. Class C.
        LDX #$7C       ; Have placed both
        LDA BCDMSH     ;    treasures in the cellar,
        BNE WSLNK      ;    but unless done in
        LDA #$40       ;    less than 41 moves,
        CMP BCDLSH     ;    this is only
        BCC WSLNK      ;    Class B.
        LDX #$77       ; Class A for both in
        BNE WSLNK      ;    cellar, moves <= 40!
        ;
CAVIS:  LDA #2         ; No treasures returned.
        JSR VISCHK     ; Visited Red Room?
        BPL DRAGCK     ;    No.
        LDA #$F
        JSR VISCHK     ; Visited Oyster-Bed?
        BPL DRAGCK     ;    No.
        ;
        ; Have found (but not recovered) both
        ;     treasures, so at least Class E.
        ;     See if visited all rooms of
        ;     caverns to earn Class D....
        LDY #$12
VISCLP: TYA
        JSR VISCHK     ; Visited this one?
        BMI NXVCLP     ; Yes, keep checking.
        LDX #$79       ; No, missed one, so
        BNE WSLNK      ;    show Class E.
        ;
NXVCLP: DEY            ; Checked 0 thru $12?
        BPL VISCLP     ; Not yet.
        LDX #$5E       ; Yes, and all were visited,
        BNE WSLNK      ;    so show Class D.
        ;
        ; In the code below, no qualifications
        ;     have yet been met, so we'll
        ;     first see if Class G has been
        ;     earned either by scaring off the
        ;     Dragon or by using the F-key....
DRAGCK: LDX #$3D
        LDA DRAGON
        BEQ WSLNK      ; Dragon is gone!
        LDA NMBUTS
        BNE WSLNK      ; F-key used correctly!
        ;
        ; Continuing, let's see if he at
        ;     least got into the caverns....
TUNCK:  LDA #$14
        JSR VISCHK
        LDX #$76
        TAY            ; Visited Tunnel?
        BMI WSLNK      ; Yes, show Class H.
        ;
        ; Well, did he even get into the
        ;     cellar of the house....?
        LDA #0
        JSR VISCHK
        LDX #6
        TAY            ; Visited Cellar?
        BMI WSLNK      ; Yes, show Class I.
        ;
        ; OK, maybe he forgot he could use
        ;     up and down as directions.
        ;     But did he do all the exploring
        ;     possible with just N,E,S, and W?
        LDY #4
ABOVLP: LDA VISTBL,Y   ; Visited House, Glen,
        JSR VISCHK     ;   Slit, Forest, Grate?
        BMI ABVCON
        LDX #$3F       ; No, missed one --
        BNE WINSET     ;   show Class O.
        ;
ABVCON: DEY            ; Checked all 5 yet?
        BPL ABOVLP     ; No, keep checking.
        LDX #$1E       ; Yes, show Class J.
        ;
        ;
WINSET: STX WINDO      ; Put class in window,
        LDA #$40       ;    put dash after
        STA WINDO+1    ;    that, and
END:    JSR LIGHT      ;    endlessly show
        JMP END        ;    Class & Moves.
;
;
VISTBL: .BYTE $05,$09,$15 ; Table of places to
        .BYTE $16,$17     ;    visit above-ground.
