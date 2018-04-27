      SUBROUTINE ENCODI(DESCR,VALUES,ND,NELEM,NOBS,NAMES,STRING,CMPRES,
     & L4)

! ------------------------------------------------------------------- !
!                                                                     !
! program       : encodi      (formerly bufr4)           (integer)    !
!                                                                     !
!               : ANSI standard except for '!' used for comments,     !
!               : variable lengths greater than 6 characters.         !
!                                                                     !
! purpose       : to make a BUFR bit string from arrays of values &   !
!               : corresponding descriptors, compressing if required. !
!               : n.b. bufr4 makes only the data section (#4) of a    !
!               : bufr message.                                       !
!                                                                     !
! called by     : bufr1 (or user who wants only section 4)            !
!                                                                     !
! calls         : tableb, tabled, valout, desfxy                      !
!                                                                     !
! parameters    : (1) descriptors (elements & associated fields only) !
!               : (2) values to be coded (nobs*nelem array)           !
!               : (3) number of descriptors (before expansion)        !
!               : (4) number of elements in descriptor string (nelem) !
!               : (5) number of reports (nobs)                        !
!               : (6) any character values (with pointers in array)   !
!               : (7) output string (for section 4)                   !
!               : (8) flag set if compression required                !
!               : (9) length of section (return zero if error)        !
!                                                                     !
!Y2K  16.06.1997  ENCODI is Year 2000 compliant.
!                                                                     !
! change record :                                                     !
!                                                                     !
! dec 89: check each value against field width, not just base value   !
! aug 90: delete f=2 descriptors to fit in with input from decode     !
! apr 91: subroutine name changed from bufr4 to encodi for shell      !
! jun 91: correct changes of field width, scale & reference value     !
!                                                                     !
! jun 95: S.Cox - changes to allow code to work on a HP-UX machine.   !
!         SAVE statement added, and changes to non-portable code.     !
!                                                                     !
! ------------------------------------------------------------------- !
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:12:34    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from BUFR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:22:21    Sheila Needham  
! $
! Revision 1.2  2000/11/07 12:17:06  usmdb
! Initialise INAM - S.Cox
!
! Revision 1.1  97/06/19  13:40:18  13:40:18  uspm (Pat McCormack)
! Initial revision
!
! ------------------------------------------------------------------- !

      SAVE

      INTEGER DESCR(*),VALUES(NOBS,NELEM)
      INTEGER F,X,Y, REFVAL,WIDTH,VALUE, SEQ(20)
      INTEGER NEWREF(20),REFDES(20)
      INTEGER EMBED, WID, OCTET, TWOTO(0:30)
      LOGICAL CMPRES,MSFLAG,ASSOC
      CHARACTER STRING*(*),NAMES*(*), FORMAT*1,NAME*60,UNITS*24
      CHARACTER HEAD*132

! ----------------------------------------------------------------------
! two missing data indicators are recognised, -32768 from the sdb &
! minus nines from a bufr decode (because we may need to re-encode!)
! ----------------------------------------------------------------------

      DATA MS/-9999999/, MIS/-32768/
      DATA MSFLAG/.FALSE./, ASSOC/.FALSE./

      DATA TWOTO/1,2,4,8,16,32,64,128,256,512,1024,2048,
     & 4096,8192,16384,32768,65536,131072,262144,524288,1048576,
     & 2097152,4194304,8388608,16777216,33554432,67108864,134217728,
     & 268435456,536870912,1073741824/

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/encodi.F,v $
     &'//'$ $Date: 26/11/2007 11:12:34$ $Revision: 2$'

      L4=0

! ----------------------------------------------------------------------
! initialise number of observation (column number in values array).
! nob will be incremented if there are several obs but no compression.
! initialise count of bits in string, leaving room for length at start.
! ----------------------------------------------------------------------

      NOB=1
      INAM=1                                                        !1.2
      IBEFOR=32

! ----------------------------------------------------------------------
!***********   expand descriptor string   *****************************
!
!  n: subscript in descriptor array.     k: row number in value array.
!       k is incremented after coding the value(s) of an element;
!       n is incremented then and after handling an f=2 descriptor
!    (when f=1 or f=3 the descriptor is removed & n left unchanged)
! ----------------------------------------------------------------------

   10 N=1
      K=1
      IWIDTH=0
      ISCALE=0
      NREF=0
      LASSOC=0
   20 CALL DESFXY(DESCR(N),F,X,Y)
      ID=F*100000+X*1000+Y

! ----------------------------------------------------------------------
!  if replication (f=1), repeat descriptors, finding count in data if
!  replication is delayed.
! ----------------------------------------------------------------------

      IF (F.EQ.1) THEN

! ----------------------------------------------------------------------
! if the replication count isn't in the descriptor, get it from the data
! (for data repetition - 031011 or 031012 - there is only one value (or
! none) in the input, so we don't need more than one descriptor)
! ----------------------------------------------------------------------

        IF (Y.EQ.0) THEN
          Y=VALUES(1,K)
          IF (DESCR(N+1).EQ.IDES(031011) .AND. Y.GT.1) Y=1
          EMBED=1
        ELSE
          EMBED=0
        ENDIF

! ----------------------------------------------------------------------
! work out how many extra descriptors, move the rest down to make room
! (working from right to left to avoid repetition!), & repeat from left
! to right to fill the empty slot.
! ----------------------------------------------------------------------

        NEXTRA=X*(Y-1)
        IF (Y.GE.1) THEN

! ----------------------------------------------------------------------
! first make room
! ----------------------------------------------------------------------

          DO 110 I=ND,N+EMBED+X+1,-1
  110      DESCR(I+NEXTRA)=DESCR(I)

! ----------------------------------------------------------------------
! then repeat (bunch will recur at intervals of x) & adjust counts
! ----------------------------------------------------------------------

          DO 120 I=1,NEXTRA
  120      DESCR(N+EMBED+X+I)=DESCR(N+EMBED+I)

! ----------------------------------------------------------------------
! if y=1, do nothing; if y=0, delete the descriptors that would
! otherwise be replicated  (nextra=-x)
! ----------------------------------------------------------------------

        ELSE IF (Y.EQ.0) THEN
          DO 130 I=N+EMBED+1,ND-X
  130      DESCR(I)=DESCR(I+X)
        ENDIF

! ----------------------------------------------------------------------
! delete replication descriptor to make sequence usable for next report
! (any embedded count must be left for the value to be coded)
! ----------------------------------------------------------------------

        DO 140 I=N+1,ND+NEXTRA
  140    DESCR(I-1)=DESCR(I)
        ND=ND+NEXTRA-1

! ----------------------------------------------------------------------
! code all values for descriptors between 203yyy & 203000 in y bits.
! changed reference values are assumed not to be missing and not to
! vary from report to report if data is compressed.
! characters to be inserted (x=5) are assumed to follow the previous
! character values in names, no pointer in values needed.
!                     ~~~~~                ~~~~~~
! first of all delete the descriptor to fit in with the array of values
! ----------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN
        DO 200 I=N+1,ND
  200    DESCR(I-1)=DESCR(I)
        ND=ND-1

        IF (X.EQ.3) THEN
          IF (Y.EQ.0) THEN
            NREF=0
          ELSE
  230       IF (DESCR(N).NE.IDES(203255)) THEN
              INVAL=VALUES(NOB,K)

! ----------------------------------------------------------------------
! keep changed ref values to use later
! ----------------------------------------------------------------------

              NEWREF(NREF)=INVAL
              REFDES(NREF)=DESCR(N)
              NREF=NREF+1

! ----------------------------------------------------------------------
! code negative value by setting sign bit
! ----------------------------------------------------------------------

              IF (INVAL.GE.0) THEN
                CALL VALOUT(STRING,IBEFOR,Y,INVAL)
              ELSE
                CALL VALOUT(STRING,IBEFOR,1,1)
                CALL VALOUT(STRING,IBEFOR,Y-1,-INVAL)
              ENDIF

! ----------------------------------------------------------------------
! if compressed, add zero increment width
! ----------------------------------------------------------------------

              IF (CMPRES) CALL VALOUT(STRING,IBEFOR,6,0)
              N=N+1
              K=K+1
              GO TO 230
            ENDIF
          ENDIF
        ENDIF

        IF (X.EQ.5) THEN
          CALL EB2ASC(Y,NAMES(INAM:))
          DO 250 I=0,Y-1
           OCTET=ICHAR(NAMES(INAM+I:INAM+I))
           CALL VALOUT (STRING,IBEFOR,8,OCTET)
  250     CONTINUE
        ENDIF

        IF (X.EQ.1 .AND. Y.GT.0) IWIDTH=Y-128
        IF (X.EQ.1 .AND. Y.EQ.0) IWIDTH=0

        IF (X.EQ.2 .AND. Y.GT.0) ISCALE=Y-128
        IF (X.EQ.2 .AND. Y.EQ.0) ISCALE=0

        IF (X.EQ.4) LASSOC=Y

! ----------------------------------------------------------------------
! look up a sequence  (expansion will overwrite sequence descriptor)
! ----------------------------------------------------------------------

      ELSE IF (F.EQ.3) THEN
        CALL TABLED(X,Y,SEQ,NSEQ)
        IF (NSEQ.EQ.0) THEN
          PRINT *,N,'-TH SEQUENCE DESCRIPTOR',ID,'NOT IN TABLE D'
          PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'
          GO TO 999
        ENDIF

! ----------------------------------------------------------------------
! insert sequence of descriptors, moving the rest down. adjust total.
! ----------------------------------------------------------------------

        DO 310 I=ND,N+1,-1
  310    DESCR(I+NSEQ-1)=DESCR(I)

        DO 320 I=1,NSEQ
  320    DESCR(N+I-1)=SEQ(I)

        ND=ND+NSEQ-1

!***********************************************************************
!******      when an element descriptor is reached, encode      ********
!******      its value(s) - one row of the input array or       ********
!******      2 rows if there's an added q/c field first.        ********
!***********************************************************************

      ELSE IF (F.EQ.0) THEN
        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)
        IF (WIDTH.EQ.0) THEN
          PRINT *,N,'-TH ELEMENT DESCRIPTOR',ID,'NOT IN TABLE B'
          PRINT *,ND,'IS N(DESCRIPTORS)',NELEM,'IS N(ELEMENTS)'
          GO TO 999
        ENDIF

! ----------------------------------------------------------------------
! change field width (if not code or flag table; scale not used here)
! & reference value (for any element) if changes are in force.
! ----------------------------------------------------------------------

        IF (FORMAT.NE.'F') WIDTH=WIDTH+IWIDTH
        DO 401 I=1,NREF
         IF (DESCR(N).EQ.REFDES(I)) REFVAL=NEWREF(I)
  401   CONTINUE

!***********
!          *
! numbers  *    if there's an associated field, code it first, then the
!          *                                      value(s) themselves.
!***********

        IF (X.NE.31 .AND. LASSOC.GT.0) ASSOC=.TRUE.
        IF (FORMAT.NE.'C') THEN
  400     IF (ASSOC) THEN
            REF=0
            WID=LASSOC
          ELSE
            REF=REFVAL
            WID=WIDTH
          ENDIF

          IF (.NOT.CMPRES) THEN
            IF (VALUES(NOB,K).EQ.MS .OR. VALUES(NOB,K).EQ.MIS) THEN
              VALUE=TWOTO(WID)-1
            ELSE
              VALUE=VALUES(NOB,K)-REF
              IF (VALUE.GE.TWOTO(WID) .OR. VALUE.LT.0) THEN
                PRINT *,K,'-TH ELEMENT HAS VALUE TOO BIG FOR BITS'
                PRINT *,VALUE,' IS VALUE',WID,' IS FIELD WIDTH'
                PRINT *,' MISSING DATA WILL BE CODED'
                VALUE=TWOTO(WID)-1
              ENDIF
            ENDIF

            CALL VALOUT(STRING,IBEFOR,WID,VALUE)
          ELSE
            DO 410 J=1,NOBS

! ----------------------------------------------------------------------
! set any value too large for the field width to missing
! so as not to interfere with working out the base value.
! ----------------------------------------------------------------------

            VALUE=VALUES(J,K)
            IF (VALUE.NE.MS .AND. VALUE.NE.MIS) THEN
              VALUE=VALUES(J,K)-REF
              IF (VALUE.GE.TWOTO(WID) .OR. VALUE.LT.0) THEN
                WRITE (6,'('' VALUE'',I4,'' (OUT OF'',I4,
     &           '') OF ELEMENT'',I3,'' WAS'',I11)') J,NOBS,K,VALUE
                WRITE (6,'(''  SET TO MISSING AS TOO BIG FOR '',
     &           ''FIELD WIDTH OF'',I3,'' BITS'')') WID
                VALUE=MS
              ENDIF
              VALUES(J,K)=VALUE
            ENDIF

            IF (J.EQ.1) THEN
              MIN=VALUE
              MAX=VALUE
            ELSE
              IF (VALUE.NE.MS .AND. VALUE.NE.MIS) THEN
                IF (VALUE.LT.MIN) MIN=VALUE
                IF (MIN.EQ.MS .OR. MIN.EQ.MIS) MIN=VALUE
                IF (VALUE.GT.MAX) MAX=VALUE
              ELSE
                MSFLAG=.TRUE.
              ENDIF
            ENDIF
  410       CONTINUE

! ----------------------------------------------------------------------
! work out the greatest increment.  because all ones means missing data,
! the range of values is one more than max-min unless all the values are
! the same, when no increments will be coded.  but max-min=0 if some
! values are missing but the rest are the same, so check msflag, which
! is set if there are missing data.
! ----------------------------------------------------------------------

            IF (MAX.GT.MIN) THEN
              MAXDIF=MAX-MIN+1
            ELSE IF (MIN.NE.MS .AND. MIN.NE.MIS .AND. MSFLAG) THEN
              MAXDIF=1
            ELSE
              MAXDIF=0
            ENDIF
            MSFLAG=.FALSE.

! ----------------------------------------------------------------------
! find out how many bits needed to code increments (none if all values
! same, one if some missing but others all the same, etc)
! ----------------------------------------------------------------------

            DO 420 J=0,30
             IF (MAXDIF.LT.TWOTO(J)) GO TO 421
  420       CONTINUE
  421       INCWID=J

! ----------------------------------------------------------------------
! encode the values of the given element.  for nobs reports there are
! nobs+2 values to go in the bit string, the first and second being
! the minimum and the width of the increments.
! if the minimum is missing, set the value to all ones, as many ones as
! fill the field.
! ----------------------------------------------------------------------

            IF (INCWID.GT.0) THEN
              NINCR=NOBS
            ELSE
              NINCR=0
            ENDIF

            DO 430 J=-1,NINCR
            IF (J.EQ.-1) THEN
              IF (MIN.EQ.MS .OR. MIN.EQ.MIS) THEN
                VALUE=TWOTO(WID)-1
              ELSE
                VALUE=MIN
              ENDIF
            ELSE IF (J.EQ.0) THEN
              VALUE=INCWID
              WID=6
            ELSE
              IF (VALUES(J,K).EQ.MS .OR. VALUES(J,K).EQ.MIS) THEN
                VALUE=TWOTO(INCWID)-1
              ELSE
                VALUE=VALUES(J,K)-MIN
              ENDIF
              WID=INCWID
            ENDIF

            CALL VALOUT(STRING,IBEFOR,WID,VALUE)
  430       CONTINUE
          ENDIF
          K=K+1

! ----------------------------------------------------------------------
! if added field has just been coded, now do the values themselves.
! ----------------------------------------------------------------------

          IF (ASSOC) THEN
            ASSOC=.FALSE.
            GO TO 400
          ENDIF

!**************  true compression of characters is unlikely to be use-
!             *  ful.  so code a zero base value & then the names them-
! characters  *  selves, unchanged - but if they're all the same, code
!             *  one name and a zero increment width.
!**************

        ELSE IF (FORMAT.EQ.'C') THEN
          IF (.NOT.CMPRES) THEN
            NCHARS=WIDTH/8
            IN=VALUES(NOB,K)
            CALL EB2ASC(NCHARS,NAMES(IN:))
            DO 510 J=0,NCHARS-1
             OCTET=ICHAR(NAMES(IN+J:IN+J))
             CALL VALOUT(STRING,IBEFOR,8,OCTET)
  510       CONTINUE
          ELSE
            NCHARS=WIDTH/8
            IN=VALUES(1,K)

! ----------------------------------------------------------------------
! compare names till a different one reached
! ----------------------------------------------------------------------

            J=2
  520       IJ=VALUES(J,K)
            IF (NAMES(IJ:IJ+NCHARS).EQ.NAMES(IN:IN+NCHARS)) THEN
              J=J+1
              IF (J.LT.NOBS) THEN
                GO TO 520
              ELSE

! ----------------------------------------------------------------------
! all the names are the same,
! ----------------------------------------------------------------------

                CALL EB2ASC(NCHARS,NAMES(IN:))

! ----------------------------------------------------------------------
! so encode one name (translated)
! ----------------------------------------------------------------------

                DO 530 J=0,NCHARS-1
                 OCTET=ICHAR(NAMES(IN+J:IN+J))
                 CALL VALOUT(STRING,IBEFOR,8,OCTET)
  530           CONTINUE

! ----------------------------------------------------------------------
! & a zero increment width.
! ----------------------------------------------------------------------

                INCWID=0
                CALL VALOUT(STRING,IBEFOR,6,INCWID)
              ENDIF
            ELSE

! ----------------------------------------------------------------------
! names aren't all the same,
! ----------------------------------------------------------------------

              DO 540 J=1,NCHARS
               OCTET=0
               CALL VALOUT(STRING,IBEFOR,8,OCTET)
  540         CONTINUE

! ----------------------------------------------------------------------
! so encode zero base & basic width
! ----------------------------------------------------------------------

              INCWID=NCHARS
              CALL VALOUT(STRING,IBEFOR,6,INCWID)

! ----------------------------------------------------------------------
! & all the names as increments.
! ----------------------------------------------------------------------

              DO 550 I=1,NOBS
               IN=VALUES(I,K)
               CALL EB2ASC(NCHARS,NAMES(IN:))
               DO 560 J=0,NCHARS-1
                OCTET=ICHAR(NAMES(IN+J:IN+J))
                CALL VALOUT(STRING,IBEFOR,8,OCTET)
  560          CONTINUE
  550         CONTINUE
            ENDIF
          ENDIF
          K=K+1
        ENDIF
        N=N+1
      ENDIF

! ----------------------------------------------------------------------
! n is incremented at the end of the f=0 section; k is incremented
! in the character & number subsections of f=0, because with q/c
! fields it must be incremented twice for numbers, n only once.
!
! loop round the descriptors, if there are any left.  if not, there
! may be a further set of data for the same descriptor sequence,
! which is reusable in expanded form (only f=0 & f=2 descriptors).
! ----------------------------------------------------------------------

      IF (N.LE.ND) GO TO 20

! ----------------------------------------------------------------------
! if several sets of data, but no compression, go round again.
! ----------------------------------------------------------------------

      IF (.NOT.CMPRES .AND. NOB.LT.NOBS) THEN
        NOB=NOB+1
        GO TO 10
      ENDIF

! ----------------------------------------------------------------------
! finally store length of bit string, rounded up to nearest halfword.
! ----------------------------------------------------------------------

  999 L=(IBEFOR+7)/8
      IF (MOD(L,2).EQ.1) L=L+1
      L4=L
      IBEFOR=0
      CALL VALOUT(STRING,IBEFOR,24,L)

      RETURN
      END
