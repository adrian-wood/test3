      SUBROUTINE CREXDAT(DESCR,STRING,VALUES,NAMES,ND,DSPLAY,
     &        MAXDES,MAXVAL,CHECK_DIGIT,IRC)

! ------------------------------------------------------------------- !
!                                                                     !
! program       : CREXDAT                                             !
!                                                                     !
!               : ANSI standard except for '!' used for comments,     !
!               : ENDDO statements, variable lengths greater than 6   !
!               : characters.                                         !
!                                                                     !
! purpose       : to expand one report from a CREX message,           !
!               : given the descriptor sequence in BUFR form.         !
!                                                                     !
! description   : The general approach is as for BUFR: a single scan  !
!               : through a descriptor string whose still unscanned   !
!               : part will grow as sequences or replications are     !
!               : expanded.                                           !
!               :   Each element descriptor in the expanded sequence  !
!               : corresponds to one value in the array.  Characters  !
!               : are put in a string with pointers in the number     !
!               : array & the corresponding descriptors flagged.      !
!               :   If the display flag is set, values are printed    !
!               : as soon as they are decoded. Code figures are       !
!               : looked up, but the figure itself is printed if      !
!               : no description can be found. Flag tables are        !
!               : displayed bit by bit.                               !
!                                                                     !
! called by     : CREXDEC                                             !
!                                                                     !
! calls         : CREXVAL: to get a value from the data section       !
!               : CREXELM: to find scale & number of figures in value !
!               : CODE   : to look up code figs or bits in flag tbls  !
!               : TABLED : to expand a sequence                       !
!               : TABLEB : only for BUFR width to display flag table  !
!               : DESFXY : to convert descriptors from TABLED         !
!                                                                     !
! parameters    : (1) descriptors in 16-bit BUFR form; on return   !1.2
!                      the sequence is expanded & descriptors with    !
!                      character values have 131072 (2**17) added, !1.2
!                      but the unexpanded sequence is returned too,
!                      at the end of the array, in reverse order,
!                      in case it's needed for reencoding.       (i/o)!
!                 (2) character string                            (i) !
!                 (3) array for output values                     (o) !
!                 (4) string for output characters                (o) !
!                      (missing values blank)                         !
!                 (5) number of descriptors (to be adjusted)     (i/o)!
!                 (6) flag set if display required                (i) !
!                 (7) maximum length of descriptor array          (i) !
!                 (8) maximum length of value array               (i) !
!                 (9) true if check digits used                   (i) !
!                (10) return code (8 if error, 0 otherwise)       (o) !
!                                                                     !
!      error return if:                                               !
!       - no entry for descriptor in table B or table D               !
!       - operator descriptor (F=2), but no operation defined         !
!       - no room in one of the arrays passed by the user             !
!       - check digits used, but digits out of step                   !
!      error return points ND to descriptor in expanded string        !
!                                                                     !
! Revision Info:                                                      !
! $Revision: 1$                                                        !
! $Date: 30/01/2006 20:21:54$
! $Source: /home/us0400/mdb/op/lib/source/RCS/crexdat.f,v $
!                                                                     !
! CHANGE RECORD:                                                      !
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:54    Sheila Needham  
! $
! Revision 1.2  2004/04/05 11:10:46  usmdb
! 19 April 2004    C Long
! 1.2   Change to 16-bit descriptors as in BUFR.
!       SEQ(999) for consistency with BUFRSEQ & ENCODE.
!
c Revision 1.1  2003/05/02  14:38:02  14:38:02  usmdb (MetDB account c/o usjh)
c Initial revision
c
!                                                                     !
!----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.         !
!                                                                     !
! Met Office, United Kingdom                                          !
!                                                                     !
! The use, duplication and disclosure of this code is strictly        !
! prohibited without the permission of The Meteorological Database    !
! Team at the above address.                                          !
!                                                                     !
! ---------------------------------------------------------------------
      IMPLICIT NONE
      SAVE

      INTEGER   BIT       ! =1 if flag table bit is set, =0 if not
      INTEGER   BUFWID    ! BUFR width (bits) of flag table
      INTEGER   CHEKFIG   ! check digit at start of next group
      INTEGER   CREXVAL   ! function to get value from data section
      INTEGER   DESCR(*)  ! descriptors (in 16-bit form)            !1.2
      INTEGER   F         ! F in FXXYYY (descriptors are in BUFR form)
      INTEGER   FLCHAR    ! to flag descriptor with character value
      INTEGER   FLDEL     ! to flag descriptor for deletion at end
      INTEGER   I         ! loop variable
      INTEGER   ICODEF    ! integer code figure to look up & display
      INTEGER   INAM      ! number of characters already in NAMES
      INTEGER   INTV      ! integer value of flag table figures
      INTEGER   IRC       ! return code (argument 10)
      INTEGER   ISCALE    ! scale change from operator
      INTEGER   IV        ! used to convert flag combinations from octal
      INTEGER   IVAL      ! number of values currently in value array
      INTEGER   IWIDTH    ! width change from operator
      INTEGER   JF        ! F in descriptor from Table D sequence
      INTEGER   JX        ! X in descriptor from Table D sequence
      INTEGER   JY        ! Y in descriptor from Table D sequence
      INTEGER   MAXDES    ! argument 7
      INTEGER   MAXVAL    ! argument 8
      INTEGER   MOUT      ! number of descriptors after final deletions
      INTEGER   N         ! number of current descriptor in main loop
      INTEGER   NBEFOR    ! number of characters before current value
!                            (but see CREXVAL for precise description!)
      INTEGER   NBIT      ! bit number in flag table (to call CODE)
      INTEGER   NBUNCH    ! number of descriptors to replicate
      INTEGER   NCHECK    ! current check figure is MOD(NCHECK,10)
      INTEGER   ND        ! total number of descriptors
      INTEGER   NDEL      ! number of delayed replications in sequence
      INTEGER   NEXTRA    ! number of extra descriptors to expand repl
      INTEGER   NSEQ      ! number of dsecriptors in Tabe D sequence
      INTEGER   NTIMES    ! number of times to replicate
      INTEGER   REFVAL    ! only for TABLEB call to get BUFWID
      INTEGER   SCALE     ! from Table B
      INTEGER   SEQ(999)  ! sequence from Table D                   !1.2
      INTEGER   WIDTH     ! from Table B, in characters
      INTEGER   X         ! XX in FXXYYY
      INTEGER   Y         ! YYY in FXXYYY

      REAL      MISSIN    ! -9999999
      REAL      VALUES(*) ! output array (argument 3)
      REAL      V         ! value on the way to VALUES( )

      LOGICAL   DSPLAY    ! argument 6
      LOGICAL   NAMED     ! set until name printed with first flag set
      LOGICAL CHECK_DIGIT ! argument 9

      CHARACTER STRING*(*)! argument 2
      CHARACTER NAMES*(*) ! argument 4
      CHARACTER NAME*60   ! element name from Table B
      CHARACTER UNITS*24  ! element units from Table B
      CHARACTER FORMAT*1  ! only for TABLEB call to get BUFWID
      CHARACTER*12 WORD   ! code figure description for display
      CHARACTER*24 FORMO(5)    ! format statements (see below)

      DATA MISSIN/-9999999./
      DATA FLCHAR/131072/,FLDEL/262144/   ! 2**17 & 2**18           !1.2

! The following format statements print different numbers of figures
! after the decimal point.  Each is to print name, units & value.

      DATA FORMO/'(1X,A47,A10,F11.1)','(1X,A47,A10,F11.2)',
     &           '(1X,A47,A10,F11.3)','(1X,A47,A10,F11.4)',
     &           '(1X,A47,A10,F11.5)'/

      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET/.FALSE./

      IF (.NOT. HEADSET) THEN
        HEAD='$RCSfile: crexdat.f,v $ '//
     &   '$Revision: 1$ $Date: 30/01/2006 20:21:54$ '
        HEADSET = .TRUE.
      ENDIF

! Loop round the descriptors, doing each operation as it comes.
! (This loop CAN'T be expressed as DO N=1,ND because ND may be changed.)

      IWIDTH=-9999999
      ISCALE=-9999999
      INAM=0
      NBEFOR=0
      NCHECK=0
      IVAL=1
      N=1

! Express descriptor as F,X,Y (packed as F*100000+X*1000+Y in CREXDEC)

      DO WHILE (N.LE.ND)
        Y=MOD(DESCR(N),256)                                         !1.2
        X=MOD(DESCR(N)/256,64)                                      !1.2
        F=MOD(DESCR(N)/16384,4)                                     !1.2

! ---------------------------------------------------------------------
! Element descriptor
! ---------------------------------------------------------------------

! Check the check digit if there is one.  (CREXVAL skips any blanks
! before the next group.)  It should be equal to the units figure
! of the group number counting from zero.

        IF (F.EQ.0) THEN
          IF (CHECK_DIGIT) THEN
            CHEKFIG=CREXVAL(STRING,1,NBEFOR)
            IF (CHEKFIG.NE.MOD(NCHECK,10)) THEN
              print *,NCHECK,'-th value has wrong check figure:',
     &                STRING(NBEFOR-1:NBEFOR+30)
              GO TO 999
            ENDIF
            NCHECK=NCHECK+1
          ENDIF

! Look up scale etc in table

          CALL CREXELM(X,Y,SCALE,WIDTH,NAME,UNITS)

          IF (WIDTH.EQ.0) THEN
            PRINT *,N,'-th descriptor not in Table B',X*1000+Y      !1.2
            GO TO 999
          ENDIF

! Check for enough characters in the data section to return a value.

          IF (NBEFOR+WIDTH.GT.LEN(STRING)) THEN
            print *,'Value string too short: no value for',X*1000+Y !1.2
            GO TO 999
          ENDIF

! Check subscript to avoid overwriting

          IF (IVAL+1.GT.MAXVAL) THEN
            PRINT *,'Value array not big enough'
            GO TO 999
          ENDIF

! Change field width and/or scale if there's a change for this value

          IF (IWIDTH.NE.-9999999) WIDTH=IWIDTH
          IF (ISCALE.NE.-9999999) SCALE=ISCALE

!---------------------!
! Character values    !
!---------------------!

          IF (INDEX(UNITS,'CHARACTER').GT.0) THEN

! Check room left in names array as well as values (already checked)

            IF (INAM+WIDTH.GT.LEN(NAMES)) THEN
              PRINT *,'Names array not big enough'
              GO TO 999
            ENDIF

! Flag descriptor to indicate that value will be pointer to characters.
! Skip any spaces (or end of line) & take next WIDTH characters.

            DO WHILE (STRING(NBEFOR:NBEFOR).LE.' ')
              NBEFOR=NBEFOR+1
            ENDDO
            NAMES(INAM+1:INAM+WIDTH)=STRING(NBEFOR:NBEFOR+WIDTH-1)
            NBEFOR=NBEFOR+WIDTH
            DESCR(N)=DESCR(N)+FLCHAR
            IF (DSPLAY) PRINT *,NAME,NAMES(INAM+1:INAM+WIDTH)

! If the string is all slashes, return blanks for consistency with BUFR.

            I=1
            DO WHILE (STRING(NBEFOR-I:NBEFOR-I).EQ.'/' .AND. I.LE.WIDTH)
              I=I+1
            ENDDO
            IF (I.GT.WIDTH) NAMES(INAM+1:INAM+WIDTH)=' '

! Set the corresponding value to a combination of string length and
! displacement in output string.  (*65536 for consistency with BUFR)

            VALUES(IVAL)=WIDTH*65536+INAM+1
            INAM=INAM+WIDTH
            IVAL=IVAL+1
          ELSE

!---------------------!
! Numerical values    !
!---------------------!

! Find value of element.

            V=CREXVAL(STRING,WIDTH,NBEFOR)

! Divide real value by 10.0**SCALE.  (Or multiply by 10.0**-SCALE if
! scale is negative - this avoids some rounding problems!)
! Flag tables are represented by octal (rather than decimal) digits:
! reconvert such a value to represent bits as in BUFR.

            IF (V.NE.MISSIN) THEN
              IF (INDEX(UNITS,' TABLE ').EQ.0) THEN
                IF (SCALE.GT.0) V=V/10.0**SCALE
                IF (SCALE.LT.0) V=V*10.0**(-SCALE)
              ELSE IF (INDEX(UNITS,'FLAG TABLE').GT.0) THEN
                IV=0
                INTV=V
                DO I=1,WIDTH
                  IV=IV+MOD(INTV,10)*8**(I-1)
                  INTV=INTV/10
                ENDDO
                V=IV
              ENDIF
            ENDIF
            VALUES(IVAL)=V
            IVAL=IVAL+1

! A value has now been found. The rest is concerned with display.
! If the element is a code figure, look it up in the tables.
! (CODE expects a descriptor in BUFR form & an integer value.)

            IF (DSPLAY) THEN
              IF (INDEX(UNITS,'CODE TABLE').GT.0) THEN
                ICODEF=V
                WORD=' '
                IF (ICODEF.GE.0) CALL CODE(X*256+Y,ICODEF,WORD)

! If no description found, print the code figure itself.

                IF (WORD.EQ.' ') THEN
                  WORD='         ...'
                  WRITE (WORD(10:12),'(I3)') ICODEF
                ENDIF
                WRITE (*,'(1X,A47,A10,A12)') NAME,UNITS,WORD

! If it's a flag table rather than a single code figure, break the
! number up into separate flags and print a line for each flag set.
! Otherwise print the flag combination as an integer.
! Look up the bits from right to left, skipping the right-hand bit,
! which is only set for missing data.  Only print the name once.
! (Wrong way round?  Better to print higher-numbered bits first?)

              ELSE IF (INDEX(UNITS,'FLAG TABLE').GT.0) THEN
                IV=V/2           ! ignore last bit
                CALL TABLEB(X,Y,SCALE,REFVAL,BUFWID,FORMAT,NAME,UNITS)
                NBIT=BUFWID-1    ! -1 because last bit ignored
                NAMED=.FALSE.

                DO WHILE (IV.GT.0)
                  BIT=MOD(IV,2)
                  IF (BIT.EQ.1) THEN
                    WORD=' '
                    CALL CODE(X*256+Y,NBIT,WORD)
                    IF (.NOT.NAMED) THEN
                      WRITE (*,'(1X,A47,10X,A12)') NAME,WORD
                      NAMED=.TRUE.
                    ELSE
                      WRITE (*,'(58X,A12)') WORD
                    ENDIF
                  ENDIF
                  IV=IV/2
                  NBIT=NBIT-1
                ENDDO

! If it's a whole number (scale zero or negative), first convert it
! to a character string ending with a decimal point (F12.0), then
! output the figures omitting the point (A11).
! If scale>0, print that many figures after the decimal point.

              ELSE
                IF (SCALE.LE.0) THEN
                  WRITE (WORD,'(F12.0)') V
                  WRITE (*,'(1X,A47,A10,A11)') NAME,UNITS,WORD
                ELSE IF (SCALE.GT.0) THEN
                  WRITE (*,FORMO(SCALE)) NAME,UNITS,V
                ENDIF
              ENDIF
            ENDIF
          ENDIF

! Finally move pointer on to next descriptor, cancelling any operation
! that only applies to the value following the operator.

          N=N+1
          IWIDTH=-9999999
          ISCALE=-9999999

! ---------------------------------------------------------------------
! Replication descriptor
! ---------------------------------------------------------------------

        ELSE IF (F.EQ.1) THEN
          NBUNCH=X

! If the replication count is not in the descriptor, look in the data.
! It must have 4 figures (plus a check figure if other groups have).
! Put count in data & count descriptor in descriptor array (replacing
! operator) even if count was in operator.

          IF (Y.NE.0) THEN
            NTIMES=Y
          ELSE
            IF (CHECK_DIGIT) THEN
              CHEKFIG=CREXVAL(STRING,1,NBEFOR)
              IF (CHEKFIG.NE.MOD(NCHECK,10)) THEN
                print *,'Replication count has wrong check figure.'
                print *,NCHECK,'-th group: ',STRING(NBEFOR-1:NBEFOR+30)
                GO TO 999
              ENDIF
              NCHECK=NCHECK+1
            ENDIF

            IF (NBEFOR+4.GT.LEN(STRING)) THEN
              print *,'String too short for replication count'
              GO TO 999
            ENDIF

            NTIMES=CREXVAL(STRING,4,NBEFOR)

            DESCR(N)=31*256+1   ! put 031001 in descriptor array    !1.2
            VALUES(IVAL)=NTIMES ! & corresponding count in value array
            IVAL=IVAL+1
          ENDIF

! Work out how many extra descriptors, move the rest down to make room
! (working from right to left to avoid repetition!), and repeat (from
! left to right!) to fill the empty slot.

          IF (NTIMES.GT.1) THEN
            NEXTRA=NBUNCH*(NTIMES-1)
            IF (ND+NEXTRA.GT.MAXDES) THEN
              PRINT *,' No room to replicate descriptors',NEXTRA,NTIMES
              GO TO 999
            ENDIF

! First make room

            DO I=ND,N+NBUNCH+1,-1
              DESCR(I+NEXTRA)=DESCR(I)
            ENDDO

! Then repeat (bunch will recur at intervals of NBUNCH)

            DO I=1,NEXTRA
              DESCR(N+NBUNCH+I)=DESCR(N+I)
            ENDDO
            ND=ND+NEXTRA

! If NTIMES=1, no action needed here: just skip the replication count
! & go on to look at the descriptors in the scope of the replication.
! If NTIMES=0, flag (for deletion) the descriptors that would have been
! repeated if NTIMES>1, and move the pointer past them all.

          ELSE IF (NTIMES.EQ.0) THEN
            DO I=N+1,N+NBUNCH
              IF (MOD(DESCR(I)/FLDEL,2).NE.1) DESCR(I)=DESCR(I)+FLDEL
            ENDDO
            N=N+NBUNCH
          ENDIF

! Move past the count descriptor which has now replaced the replication
! operator.

          N=N+1

! ---------------------------------------------------------------------
! Operator descriptor          (flag it for deletion & move past it)
! ---------------------------------------------------------------------

        ELSE IF (F.EQ.2) THEN
          IF (MOD(DESCR(N)/FLDEL,2).NE.1) DESCR(N)=DESCR(N)+FLDEL
          N=N+1

! Width & scale changes (Y replaces Table B value for next value only)
! Scale can be <0, packed in CREXDEC by adding 128, hence -128 here.

          IF (X.EQ.1) THEN
            IWIDTH=Y

          ELSE IF (X.EQ.2) THEN
            ISCALE=Y-128
            if (dsplay) print *,iscale,'is scale for next value'

! Inserted characters

          ELSE IF (X.EQ.5 .OR. X.EQ.60) THEN
            V=CREXVAL(STRING,Y,NBEFOR)
            NAMES(INAM+1:INAM+Y)=STRING(NBEFOR-Y:NBEFOR-1)
            PRINT *,NAMES(INAM+1:INAM+Y)
            INAM=INAM+Y

! Change of units (can only issue warning until table set up!)

          ELSE IF (X.EQ.7) THEN
            IF (DSPLAY) PRINT *,'Change of units for next value!!!',
     &                          Y,'is code figure in Table C-6'

! Stop if operation not recognised, printing bad descriptor.
! (DESCR(N-1) is flagged for deletion, so reconstruct it from X & Y.)

          ELSE
            PRINT *,200000+X*1000+Y,' is not a valid operation'
            GO TO 999
          ENDIF

! ---------------------------------------------------------------------
! Sequence descriptor
! -------------------
! Sequences can't be expanded at the start, because replication
! counts would have to be adjusted.
! N stays pointing to the first descriptor in the expansion
! (so the sequence descriptor itself is overwritten).
! ---------------------------------------------------------------------

        ELSE IF (F.EQ.3) THEN
          CALL TABLED(X,Y,SEQ,NSEQ)

          IF (NSEQ.EQ.0) THEN
            PRINT *,N,'-th descriptor is for sequence not in Table D'
            GO TO 999
          ENDIF

          IF (ND+NSEQ-1.GT.MAXDES) THEN
            PRINT *,' No room to expand sequence in descriptor array'
            GO TO 999
          ENDIF

! Convert 16-bit descriptors from TABLED to F*100000+X*1000+Y as used
! here, deleting any BUFR counts for delayed replication (class 31).
! (This allows BUFR & CREX to use the same Table D input.)

          NDEL=0
          DO I=1,NSEQ
            CALL DESFXY(SEQ(I),JF,JX,JY)
            IF (JF.EQ.0 .AND. JX.EQ.31) THEN   ! if 031yyy,
              NDEL=NDEL+1                      ! skip it
            ELSE
              SEQ(I-NDEL)=SEQ(I)
            ENDIF
          ENDDO
          NSEQ=NSEQ-NDEL

! Insert descriptor sequence after moving the rest down. Adjust total.

          DO I=ND,N+1,-1
            DESCR(I+NSEQ-1)=DESCR(I)
          ENDDO

          DO I=1,NSEQ
            DESCR(N+I-1)=SEQ(I)
          ENDDO

          ND=ND+NSEQ-1
        ENDIF
      ENDDO

! Delete all descriptors flagged for deletion, adjusting the count.
! This should leave only element descriptors.  Leave them in readable
! 6-figure form (not the 16-bit form needed for BUFR reencoding).

      MOUT=0
      DO I=1,ND
        IF (MOD(DESCR(I)/FLDEL,2).NE.1) THEN
          MOUT=MOUT+1
          DESCR(MOUT)=DESCR(I)
        ENDIF
      ENDDO
      ND=MOUT
      IRC=0
      RETURN

! Come here if there's an error.  Point ND to last descriptor handled.

  999 ND=N
      IRC=8
      RETURN
      END
