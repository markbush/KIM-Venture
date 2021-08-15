;
; LOBSCH (Last Object Search) subrouting.
;      Finds, and saves in LOBJAD, the address
;      of the last object in a file; also
;      counts, and returns in Y-reg, the number
;      of objects in the file.  File to search
;      is EGO file if called with Y=0; is
;      file at LOCAD if called with Y=1.
;
LOBSCH: LDX EGOLAD,Y   ; Get pointer to file.
        LDY #$FF       ; Init object-count.
        ;
OBFIND: INX
        STX LOBJAD     ; Save addr of last obj.
        LDA 1,X        ; Set up to test bits
        ASL            ;   7 & 6 of each location.
        BMI OBFIND     ; b6=1.  Not an object.
        INY            ; Bump object-count.
        BCC OBFIND     ; b7=0. An object. Continue.
        RTS            ; b7=1. End of file. Done.
;
;
;
; ADDOBJ (Add Object) subroutine.  Called to
;      add a dropped object to a location file,
;      or a picked-up object to EGO file.
;      LINTAX is the pointer to the address of
;      the location of interest: 0 for EGO,
;      1 for file specified by LOCAD.  Calls
;      LOBSCH subroutine.  Object to be added
;      is specified by contents of OBJ.
;
ADDOBJ: LDY LINTAX     ; Point to file of interest.
        JSR LOBSCH     ; Find last obj's address.
        LDX #EOCM      ; Start at End of Cave Map.
AOBLP:  LDA 0,X        ; Move all files up one
        STA 1,X        ;    location to make room
        DEX            ;    for the object.
        CPX LOBJAD     ; Done yet?
        BNE AOBLP      ; No.  Keep moving.
        LDA OBJ        ; Yes, store object just
        STA 1,X        ;    above last object in
        RTS            ;    the file; return.
;
;
; SPROC (Special Processing) segment.
;      Entered from Main Move Loop (MNMVLP)
;      following a "direction" command, this
;      code takes care of any special pro-
;      hibitions against moving in the com-
;      manded direction. (Examples -- can't
;      go through a steel grate, or past a
;      dragon.)  Possible exits from a SPROC
;      are: to MOVER, if no problems with
;                the command direction,
;           to HOWMSG, if "How ? " is to be
;                shown to indicate improper
;                conditions for the move, or
;           to MSGAML, showing "Halted By
;                The Dragon", if appropriate.
;
SPROC:  TAY
        STY DIR        ; Save direction for MOVER.
        LDX LOCNUM
        LDA LNAMAD,X   ; If at grate (or gully),
        CMP ADOPGR-5,X ;    is grate open (or is
        ;                   bridge made)?
        BEQ SPATS      ; Yes, move is OK.
        CPX #[ADGRM-LNAMAD] ; No. At closed
        ;                           grate?
        BEQ SPCHKD     ; Yes, disallow Down.
        CPX #[ADGYM-LNAMAD] ; At bridgeless
        ;                           gully?
        BEQ SPCHKW     ; Yes, disallow West.
SPATS:  CPX #[ADTSM-LNAMAD] ; At shaft?
        BNE SPATSS
        LDA NOBCRY     ; Yes, carrying anything?
        BNE SPCHKD     ; Yes, disallow Down.
        ;
SPATSS: CPX #[ADSSM-LNAMAD] ; At steps?
        BNE SPATRH
        LDA BURDEN     ; Yes, carrying Gold?
        BMI SPCHKU     ; Yes, disallow Up.
        ;
SPATRH: CPX #[ADRHM-LNAMAD] ; At Royal Hall?
        BNE SPCONT
        LDA DRAGON     ; Yes, is Dragon there?
        BEQ SPCONT
        DEY            ; Yes, but going East
        BEQ SPCONT     ;    is OK.  Continue.
        LDY #<HBDMS     ; All other directions,
        JMP MSGAML     ;    "Halted by Dragon."
        ;
SPCHKD: DEY            ; Check for Down,
SPCHKU: DEY            ;       for Up, or
SPCHKW: CPY #3         ;       for West.
        BNE SPCONT     ; Other directions are OK.
        JMP HOWMSG     ; Disallowed direction
        ;                   produces "How ? ".
        ;
SPCONT: JMP MOVER      ; Continue Move process.
        ;
        ;
        ;
        ;
;  These three bytes
;  are left spare for
;  user expansion....
