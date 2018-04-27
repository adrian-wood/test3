      SUBROUTINE DECODI(DESCR,VALUES,NAMES,ND,NOBS,STRING,CMPRES,
     & DSPLAY,*)

! ------------------------------------------------------------------- !
!                                                                     !
! program       : decodi    (formerly decode)            (integer)    !
!                                                                     !
!               : ANSI standard except for '!' used for comments,     !
!               : ENDDO statements and variable lengths greater than  !
!               : 6 characters.                                       !
!                                                                     !
! purpose       : to expand a BUFR bit string, expanding the sequence !
!                 of descriptors & looking up tables B & D.           !
!                                                                     !
! description   : the general approach is that proposed by ECMWF: a   !
!                 single scan through a descriptor string whose still !
!                 unscanned part will grow as sequence or replication !
!                 descriptors are met.  each descriptor in the        !
!                 expanded string corresponds to nobs values in the   !
!                 array of numbers; a zero descriptor corresponds to  !
!                 "associated" (QC) fields for the element which      !
!                 follows.  character fields are returned in the      !
!                 character string, with pointers rather than values  !
!                 in the array of numbers; descriptors in the expanded!
!                 string are flagged if they correspond to characters !
!                 & hence to pointers.  flags are also set for        !
!                 repeated coordinate increments to which no values   !
!                 correspond in the array.  if the display flag is    !
!                 set, code figures are looked up in table b; but the !
!                 code figure itself is displayed if a description    !
!                 can't be found.  plain language is just skipped     !
!                 unless a display is requested.                      !
!                                                                     !
! calls         : DESFXY : to expand descriptr; value, tableb, tabled !
!                 CODE   : to look up code figs or bits in flag tbls  !
!                 ASC2EB : to translate any characters to EBCDIC      !
!                                                                     !
! parameters    : (1) string of descriptors (copied to fullword  (nd) !
!                     array to leave room for expansion & flags)      !
!                 (2) array for output values   ((nobs,nd), i.e. all  !
!                     values of an element together)                  !
!                 (3) string for output characters                    !
!                 (4) number of descriptors (to be adjusted)       nd !
!                 (5) number of reports                          nobs !
!                 (6) bit string                                      !
!                 (7) flag set if data compressed                     !
!                 (8) flag set if display required                    !
!                                                                     !
!      error return if bufr rules broken in one of the ways below:    !
!       - no entry for descriptor in table b or table d               !
!       - operator descriptor (f=2), but no operation defined (x>5)   !
!       - base value missing, but non-zero increment length           !
!       - delayed replication (y=0), but no count in data             !
!       - delayed replication with compression, but counts vary       !
!       - new reference value(s), but end of definition not marked    !
!       - new reference values with compression, but values vary (?)  !
!       - all values missing with compression, but incr width nonzero !
!       - data repetition with more than one descriptor  (invalid?)   !
!       - data repetition but compression  (not allowed?)             !
!      error return points nd to descriptor in expanded string        !
!                                                                     !
!Y2K  16.06.1997  DECODI is Year 2000 compliant.
!                                                                     !
! change record :                                                     !
!                                                                     !
!   jul 90 : flag repl descr to delete at end (one line of code)  !a  !
!   apr 91 : subroutine name changed to decodi for shell              !
!   jun 91 : allow change of reference value for any element          !
!   jun 95 : changes made to make code more portable (s.cox)          !
!                                                                     !
! ------------------------------------------------------------------- !
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:13:09    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from BUFR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:22:03    Sheila Needham  
! $
! Revision 1.3  2000/11/07 12:16:52  usmdb
! Replace a TAB by spaces as f90 compile on HP was
! failing - S.Cox
!
! Revision 1.2  97/09/22  09:47:52  09:47:52  uspm (Pat McCormack)
! Change all labelled statements to be CONTINUE
!
! Revision 1.1  1997/06/20 10:16:03  uspm
! Initial revision
!
! ------------------------------------------------------------------- !

      SAVE

! ---------------------------------------------------------------------
! declare interface variables
! ---------------------------------------------------------------------

      INTEGER DESCR(*),VALUES(*)
      CHARACTER STRING*(*),NAMES*(*)
      LOGICAL CMPRES,DSPLAY

! ---------------------------------------------------------------------
! declare parameters and arrays using parameters
! ---------------------------------------------------------------------

      PARAMETER (NARRAY=20)
      INTEGER NEWREF(NARRAY),REFDES(NARRAY),SEQ(NARRAY)
      INTEGER STACK(NARRAY),STEP(NARRAY)

! ---------------------------------------------------------------------
! allow up to 64 characters in a field - this is the longest increment
! ---------------------------------------------------------------------

      INTEGER BASE(64),INCREM(64),FIG,CARRY, JOUT(6)
      INTEGER F,X,Y, SCALE,REFVAL,WIDTH,VALUE,V,BIT
      INTEGER MISSIN

      REAL VOUT(6)

      LOGICAL RHFLAG

      INTEGER TENTO(0:9)
      INTEGER TWOTO(0:30)                                               02700079

      INTEGER FLCHAR,FLINCR,FLRUN,FLQC,FLDEL
      EQUIVALENCE (FLCHAR,TWOTO(17))
      EQUIVALENCE (FLINCR,TWOTO(18))
      EQUIVALENCE (FLRUN,TWOTO(19))
      EQUIVALENCE (FLQC,TWOTO(29))
      EQUIVALENCE (FLDEL,TWOTO(30))

      CHARACTER NAME*60,UNITS*24,FORMAT*1
      CHARACTER WORD(6)*12, FORMT(5)*9, FORMO(5)*18
      CHARACTER HEAD*132

      DATA FORMT/'(6F12.1)','(6F12.2)','(6F12.3)','(6F12.4)','(6F12.5)'/
      DATA FORMO/'(1X,A55,A12,F11.1)','(1X,A55,A12,F11.2)',
     &           '(1X,A55,A12,F11.3)','(1X,A55,A12,F11.4)',
     &           '(1X,A55,A12,F11.5)'/

      DATA MISSIN/-9999999/
      DATA TENTO/1,10,100,1000,10000,100000,1000000,10000000,
     &           100000000,1000000000/
      DATA TWOTO/1,2,4,8,16,32,64,128,256,512,1024,2048,
     & 4096,8192,16384,32768,65536,131072,262144,524288,1048576,
     & 2097152,4194304,8388608,16777216,33554432,67108864,134217728,
     & 268435456,536870912,1073741824/

! ---------------------------------------------------------------------
! declare statement function
! ---------------------------------------------------------------------
      LOGICAL  FLAGON
      FLAGON(ID,JFLAG) = MOD(ID/JFLAG,2).EQ.1

!
! ---------------------------------------------------------------------
! Declare revision information
! ---------------------------------------------------------------------
!
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/decodi.F,v $
     &'//'$ $Date: 26/11/2007 11:13:09$ $Revision: 2$'

! ---------------------------------------------------------------------
! loop round the descriptors, doing each operation as it comes.
! this loop can't be expressed as  do n=1,nd  because nd may be changed
! ---------------------------------------------------------------------

      INAM=0
      IBEFOR=32
      IWIDTH=0
      ISCALE=0
      IASSOC=0
      NREF=0
      NOB=1
      NSTEPS=0
   11 CONTINUE
      IVAL=NOB
      N=1

! ---------------------------------------------------------------------
! express descriptor as f, x and y.
! ---------------------------------------------------------------------

    1 CONTINUE
      CALL DESFXY(DESCR(N),F,X,Y)

! ---------------------------------------------------------------------
!                            (do nothing if it's
! element descriptor         flagged as a further
!                            coordinate increment)
! ---------------------------------------------------------------------

      IF (F.EQ.0) THEN
        IF (.NOT.FLAGON(DESCR(N),FLINCR)) THEN

! ---------------------------------------------------------------------
! look up scale &c in table b
! ---------------------------------------------------------------------

          CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)
          IF (WIDTH.EQ.0) THEN
            PRINT *,N,'-TH DESCRIPTOR NOT IN TABLE B'
            GO TO 999
          ENDIF

! ---------------------------------------------------------------------
! adjust width & refval if nec
! ---------------------------------------------------------------------

          IF (FORMAT.NE.'F') WIDTH=WIDTH+IWIDTH
          DO 10 I=1,NREF
           IF (DESCR(N).EQ.REFDES(I)) REFVAL=NEWREF(I)
   10     CONTINUE

! ---------------------------------------------------------------------
! element: characters
!                                                     width is in bits,
!                                                      nchars in bytes.
! ---------------------------------------------------------------------

          IF (FORMAT.EQ.'C') THEN
            NCHARS=WIDTH/8

! ---------------------------------------------------------------------
! flag descriptor (if first time round): value will be pointer to chars
! ---------------------------------------------------------------------

            IF (NOB.EQ.1) DESCR(N)=DESCR(N)+FLCHAR

! ---------------------------------------------------------------------
! get the characters one at a time, keeping the integer values to add
! increments character by character if the data is compressed.
! ---------------------------------------------------------------------

            DO 20 I=1,NCHARS
             BASE(I)=VALUE(STRING,IBEFOR,8)
             NAMES(INAM+I:INAM+I)=CHAR(BASE(I))
   20       CONTINUE

! ---------------------------------------------------------------------
! if data not compressed, translate characters & set pointer
! ---------------------------------------------------------------------

            IF (.NOT.CMPRES) THEN
              CALL ASC2EB(NCHARS,NAMES(INAM+1:INAM+NCHARS))
              IF (BASE(1).EQ.255) NAMES(INAM+1:INAM+NCHARS)=' '
              IF (DSPLAY) PRINT *,NAME,NAMES(INAM+1:INAM+NCHARS)
              VALUES(IVAL)=NCHARS*65536+INAM+1
              INAM=INAM+NCHARS
              IVAL=IVAL+NOBS
            ELSE

! ---------------------------------------------------------------------
! if data is compressed, add increments. first get increment width.
! ---------------------------------------------------------------------

              NCREM=VALUE(STRING,IBEFOR,6)
              IF (BASE(1).EQ.255 .AND. NCREM.NE.0) THEN
                PRINT *,' BASE VALUE MISSING BUT NCREM=',NCREM
                PRINT *,' AFTER',IBEFOR,' BITS'
                GO TO 999
              ENDIF

              DO 50 NIN=1,NOBS

! ---------------------------------------------------------------------
! copy name to next slot before changed by increments or translation
! ---------------------------------------------------------------------

              INEXT=INAM+NCHARS
              NAMES(INEXT+1:INEXT+NCHARS)=NAMES(INAM+1:INAM+NCHARS)
              IF (NCREM.GT.0) THEN

! ---------------------------------------------------------------------
! get increment: if ncrem<nchars, increment left-hand characters only.
! ---------------------------------------------------------------------

                DO 30 I=1,NCREM
                 INCREM(I)=VALUE(STRING,IBEFOR,8)
   30           CONTINUE

! ---------------------------------------------------------------------
! add increment byte by byte, from right, carrying if necessary.
!(characters in bufr are 8-bit fields, so get a character at a time)
! ---------------------------------------------------------------------

                CARRY=0
                DO 40 I=NCREM,1,-1
                 FIG=BASE(I)+INCREM(I)+CARRY
                 IF (FIG.GE.256) THEN
                   NAMES(INAM+I:INAM+I)=CHAR(FIG-256)
                   CARRY=1
                 ELSE
                   NAMES(INAM+I:INAM+I)=CHAR(FIG)
                   CARRY=0
                 ENDIF
   40           CONTINUE
              ENDIF
              CALL ASC2EB(NCREM,NAMES(INAM+1:INAM+NCREM))
              IF (INCREM(1).EQ.255) NAMES(INAM+1:INAM+NCHARS)=' '

! ---------------------------------------------------------------------
! finally set pointer & move on for next value.
! ---------------------------------------------------------------------

              VALUES(IVAL)=NCHARS*65536+INAM+1
              INAM=INAM+NCHARS
              IVAL=IVAL+1
   50         CONTINUE

              IF (DSPLAY) THEN
                WRITE (*,'(A55//(6(1X,A12)))') NAME,
     &          (NAMES(INAM-NCHARS*J+1:INAM-NCHARS*(J-1)),J=NOBS,1,-1)
              ENDIF
            ENDIF
          ELSE

! ---------------------------------------------------------------------
! element: numbers
! ----------------
!
!  find value(s) of any associated field
! ---------------------------------------------------------------------

            IF (X.NE.31) THEN
              IF (IASSOC.NE.0) THEN
                VALUES(IVAL)=VALUE(STRING,IBEFOR,IASSOC)
                IF (CMPRES) THEN
                  V=VALUES(IVAL)
                  NCREM=VALUE(STRING,IBEFOR,6)

                  DO 60 I=1,NOBS
                  IF (NCREM.GT.0) THEN
                    VALUES(IVAL)=VALUE(STRING,IBEFOR,NCREM)+V
                  ELSE
                    VALUES(IVAL)=V
                  ENDIF
                  IVAL=IVAL+1
   60             CONTINUE

                ELSE
                  IVAL=IVAL+1
                ENDIF

! ---------------------------------------------------------------------
! set flag to move all the descriptors down one (at the end)
! & leave a zero descriptor corresponding to the associated field.
! ---------------------------------------------------------------------

                DESCR(N)=DESCR(N)+FLQC
              ENDIF
            ENDIF

! ---------------------------------------------------------------------
!  find value(s) of element itself
! ---------------------------------------------------------------------

            V=VALUE(STRING,IBEFOR,WIDTH)

            IF (V.EQ.TWOTO(WIDTH)-1) THEN
              V=MISSIN
            ELSE

! ---------------------------------------------------------------------
! first add refval, then cope with any adjustment to the normal scale
! (n.b. if refval is nonzero, any scale change must be accompanied by
! a change of refval, except in the awkward case of rainfall & snow!)
! ---------------------------------------------------------------------

              IF (FORMAT.NE.'F') THEN
                V=V+REFVAL
                IF (ISCALE.GT.0) THEN
                  V=V/TENTO(ISCALE)
                ELSE IF (ISCALE.LT.0) THEN
                  V=V*TENTO(-ISCALE)
                ENDIF
              ENDIF
            ENDIF

            IF (CMPRES) THEN
              NCREM=VALUE(STRING,IBEFOR,6)
              IF (V.EQ.MISSIN .AND. NCREM.NE.0) THEN
                PRINT *,NCREM,'IS INCREMENT LEN BUT MISSING BASE VALUE'
                PRINT *,IBEFOR,'BITS TO END OF INCREMENT LENGTH'
                PRINT *,' GOING ON TO NEXT ELEMENT'
              ENDIF
              DO 70 I=1,NOBS
              IF (NCREM.GT.0 .AND. V.NE.MISSIN) THEN
                INCR=VALUE(STRING,IBEFOR,NCREM)
                IF (INCR.EQ.TWOTO(NCREM)-1) THEN
                  VALUES(IVAL)=MISSIN
                ELSE
                  IF (FORMAT.NE.'F') THEN
                    IF (ISCALE.GT.0) THEN
                      INCR=INCR/TENTO(ISCALE)
                    ELSE IF (ISCALE.LT.0) THEN
                      INCR=INCR*TENTO(-ISCALE)
                    ENDIF
                  ENDIF
                  VALUES(IVAL)=V+INCR
                ENDIF
              ELSE
                VALUES(IVAL)=V
              ENDIF
              IVAL=IVAL+1
   70         CONTINUE
            ELSE
              VALUES(IVAL)=V
              IVAL=IVAL+NOBS
            ENDIF

! ---------------------------------------------------------------------
! if the replication count implied data repetition, i.e if an element
! descriptor with the run flag set has been repeated, then repeat the
! value to go with it.   assume no more than one value thus repeated;
! but it may have a q/c field, in which case leave a zero descriptor.
! ---------------------------------------------------------------------

   77       CONTINUE
            IF (FLAGON(DESCR(N),FLRUN).AND.DESCR(N+1).EQ.DESCR(N)) THEN
              IF (CMPRES) THEN
                PRINT *,' DATA REPETITION, BUT MORE THAN ONE ELEMENT'
                GO TO 999
              ENDIF
              IF (IASSOC.NE.0) THEN
                VALUES(IVAL+1)=VALUES(IVAL-1)
                VALUES(IVAL+2)=VALUES(IVAL)
                IVAL=IVAL+2
                DESCR(N)=DESCR(N)+FLQC
              ELSE
                VALUES(IVAL+1)=VALUES(IVAL)
                IVAL=IVAL+1
              ENDIF
              DESCR(N)=DESCR(N)-FLRUN
              N=N+1
              GO TO 77
            ENDIF

! ---------------------------------------------------------------------
! the values have now been found. the rest is concerned with display
! ---------------------------------------------------------------------

            IF (DSPLAY .AND. V.NE.MISSIN) THEN
              LEFT=NOBS
              ISTART=IVAL-NOBS

! ---------------------------------------------------------------------
! from number of values left get number on this line (six per line)
! ---------------------------------------------------------------------

   90         CONTINUE
              IF (LEFT.GE.6) THEN
                LINE=6
              ELSE
                LINE=LEFT
              ENDIF

! ---------------------------------------------------------------------
! if the element is a code figure, look it up in the tables.
! if it's a flag table, look up the bits one by one.
! ---------------------------------------------------------------------

              IF (FORMAT.EQ.'F') THEN
                IF (UNITS(1:4).EQ.'CODE') THEN
                  DO I=1,LINE
                   CALL CODE(DESCR(N),VALUES(ISTART+I-1),WORD(I))
                   IF (WORD(I).EQ.' ') THEN
                     WORD(I)='         ...'
                     WRITE (WORD(I)(10:12),'(I3)') VALUES(ISTART+I-1)
                   ENDIF
                  ENDDO

                  IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN
                    WRITE (*,'(1X,A55,A12,A12)') NAME,UNITS,WORD(1)
                  ELSE
                    IF (LEFT.EQ.NOBS) WRITE (*,'(1X,A55)') NAME
                    WRITE (*,'(6(1X,A12))') (WORD(I),I=1,LINE)
                  ENDIF

! ---------------------------------------------------------------------
! if flag table rather than single code figure, break the table up into
! separate flags if all the values are the same, & print a line for each
! flag that is set; otherwise print the flag combinations as integers.
! look up the bits from right to left,skipping the right-hand bit,
! which is only set for missing data.
! ---------------------------------------------------------------------

                ELSE IF (UNITS(1:4).EQ.'FLAG') THEN
                  IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN
                    V=V/2
                    NBIT=WIDTH-1
                    RHFLAG=.TRUE.

   80               CONTINUE
                    BIT=MOD(V,2)
                    IF (BIT.EQ.1) THEN
                      CALL CODE(DESCR(N),NBIT-1,WORD)
                      IF (RHFLAG) THEN
                        RHFLAG=.FALSE.
                        WRITE (*,'(1X,A55,12X,A12)') NAME,WORD(1)
                      ELSE
                        WRITE (*,'(68X,A12)') WORD(1)
                      ENDIF
                    ENDIF
                    V=V/2
                    NBIT=NBIT-1
                    IF (V.NE.0) GO TO 80
                  ELSE
                    IF (LEFT.EQ.NOBS) WRITE(*,'(1X,A55,A12)') NAME,UNITS
                    WRITE (*,'(6I12)') (VALUES(ISTART+I-1),I=1,LINE)
                  ENDIF
                ENDIF

! ---------------------------------------------------------------------
! display whole-number values as integers.  if division by 10**scale is
! needed, display scale figs after decimal point.  pressures in hpa(mb)
! ---------------------------------------------------------------------

              ELSE IF (FORMAT.NE.'F') THEN
                IF (UNITS.EQ.'PA') THEN
                  UNITS='HPA'
                  SCALE=SCALE+2
                ENDIF

! ---------------------------------------------------------------------
! if scaling puts zero(s) on end, display values as integers
! ---------------------------------------------------------------------

                IF (SCALE.LE.0) THEN
                  DO I=1,LINE
                   IF (VALUES(ISTART+I-1).NE.MISSIN) THEN
                     JOUT(I)=(VALUES(ISTART+I-1))*TENTO(-SCALE)
                   ELSE
                     JOUT(I)=MISSIN
                   ENDIF
                  ENDDO

                  IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN
                    IF (LEFT.EQ.NOBS) THEN
                      WRITE (*,'(1X,A55,A12,I12)') NAME,UNITS,JOUT(1)
                    ENDIF
                  ELSE
                    IF (LEFT.EQ.NOBS) WRITE(*,'(1X,A55,A12)') NAME,UNITS
                    WRITE (*,'(6I12)') (JOUT(I),I=1,LINE)
                  ENDIF

! ---------------------------------------------------------------------
! if scaling inserts decimal point, display values as real.
! ---------------------------------------------------------------------

                ELSE IF (SCALE.GT.0) THEN
                  DO I=1,LINE
                   IF (VALUES(ISTART+I-1).NE.MISSIN) THEN
                     VOUT(I)=VALUES(ISTART+I-1)
                     VOUT(I)=VOUT(I)/TENTO(SCALE)
                   ELSE
                     VOUT(I)=-999999999.
                   ENDIF
                  ENDDO

                  IF (NOBS.EQ.1 .OR. NCREM.EQ.0) THEN
                    IF (LEFT.EQ.NOBS) THEN
                      WRITE (*,FORMO(SCALE)) NAME,UNITS,VOUT(1)
                    ENDIF
                  ELSE
                    IF (LEFT.EQ.NOBS) WRITE(*,'(1X,A55,A12)') NAME,UNITS
                    WRITE (*,FORMT(SCALE)) (VOUT(I),I=1,LINE)
                  ENDIF
                ENDIF
              ENDIF

              LEFT=LEFT-6
              IF (LEFT.GT.0) THEN
                ISTART=ISTART+6
                GO TO 90
              ENDIF

            ENDIF
          ENDIF
        ELSE IF (FLAGON(DESCR(N),FLINCR)) THEN
          DO 95 IS=NSTEPS,1,-1
           IF (DESCR(N).EQ.STACK(IS)) THEN
             DESCR(N)=DESCR(N)-FLINCR
             IF (CMPRES) THEN
               DO 97 I=IVAL,IVAL+NOBS
                VALUES(I)=STEP(IS)
   97          CONTINUE
               IVAL=IVAL+NOBS
             ELSE
               VALUES(IVAL)=STEP(IS)
               IVAL=IVAL+1
             ENDIF
             GO TO 99
           ENDIF
   95     CONTINUE
        ENDIF

! ---------------------------------------------------------------------
! move pointer on to next descriptor
! ---------------------------------------------------------------------

   99   CONTINUE
        N=N+1

! ---------------------------------------------------------------------
! replication descriptor
! ---------------------------------------------------------------------

      ELSE IF (F.EQ.1) THEN
        NBUNCH=X

! ---------------------------------------------------------------------
! delete replication descriptor at end of decode (can't delete before
! end because of replicated increments: two increments before different
! replications could come together if a replicator were deleted and the
! first be wrongly replicated with the second replication)
! ---------------------------------------------------------------------

        DESCR(N)=DESCR(N)+FLDEL                                     !a

! ---------------------------------------------------------------------
! if the replication count is not in the descriptor, look in the data.
! (if the data is compressed, all the counts must be the same)
! ---------------------------------------------------------------------

        IF (Y.NE.0) THEN
          NTIMES=Y
          IMBED=0
        ELSE
          IMBED=1

! ---------------------------------------------------------------------
! next descriptor must be for embedded count
! ---------------------------------------------------------------------

          CALL DESFXY(DESCR(N+1),F,X,Y)
          IF (F.NE.0 .OR. X.NE.31) THEN
            PRINT *,' DELAYED REPLICATION, BUT NO COUNT DESCRIPTOR'
            GO TO 999
          ENDIF

          CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

! ---------------------------------------------------------------------
! do usual adjustments to get value of count
! ---------------------------------------------------------------------

          WIDTH=WIDTH+IWIDTH
          DO 110 I=1,NREF
           IF (DESCR(N+1).EQ.REFDES(I)) REFVAL=NEWREF(I)
  110     CONTINUE
          K=VALUE(STRING,IBEFOR,WIDTH)

! ---------------------------------------------------------------------
! if compressed, must be zero incre
! ---------------------------------------------------------------------

          IF (CMPRES) THEN
            KINC=VALUE(STRING,IBEFOR,6)
            IF (KINC.NE.0) THEN
              PRINT *,' DELAYED REPLICATION WITH COMPRESSION,'
              PRINT *,' BUT COUNTS VARY FROM REPORT TO REPORT'
              GO TO 999
            ENDIF
          ENDIF

! ---------------------------------------------------------------------
! a count could conceivably be scaled, but only in one direction.
! ---------------------------------------------------------------------

          NTIMES=(K+REFVAL)*TENTO(-SCALE-ISCALE)

! ---------------------------------------------------------------------
! set run-length encoding flag on element descriptor if y=11 or 12
! ---------------------------------------------------------------------

          IF (Y.EQ.11 .OR. Y.EQ.12) THEN
            IF (NBUNCH.GT.1) THEN
              PRINT *,' DATA REPETITION, BUT MORE THAN ONE DESCRIPTOR'
              GO TO 999
            ELSE
              DESCR(N+2)=DESCR(N+2)+FLRUN
            ENDIF
          ENDIF
        ENDIF

! ---------------------------------------------------------------------
! if there are increments immediately before the replication, copy them
! at the end of the string to be repeated and flag them so that they're
! interpreted as having no more corresponding fields in the bit string.
! first find how many increments there are (insert).
! ---------------------------------------------------------------------

        INSERT=0
  120   CONTINUE
        LASTEL=N-1-INSERT
        CALL DESFXY(DESCR(LASTEL),F,X,Y)
        IF (X.GE.4.AND.X.LE.7 .AND. Y.GE.11.AND.Y.LE.20) THEN
          IF (.NOT.FLAGON(DESCR(LASTEL),FLINCR)) THEN
            INSERT=INSERT+1
            NSTEPS=NSTEPS+1
            STACK(NSTEPS)=DESCR(LASTEL)+FLINCR
            STEP(NSTEPS)=VALUES(IVAL-INSERT)
            GO TO 120
          ENDIF
        ENDIF

        IF (NTIMES.GT.1) THEN

! ---------------------------------------------------------------------
! work out how many extra descriptors, move the rest down to make room
! (working from right to left to avoid repetition!), add in any coord.
! increments and repeat (left to right!) to fill the empty slot.
! ---------------------------------------------------------------------

          NEXTRA=(NBUNCH+INSERT)*(NTIMES-1)

! ---------------------------------------------------------------------
! first make room
! ---------------------------------------------------------------------

          DO 130 I=ND,N+IMBED+NBUNCH+1,-1
           DESCR(I+NEXTRA)=DESCR(I)
  130     CONTINUE

! ---------------------------------------------------------------------
! put coordinate increments at end of bunch to be repeated (repeated all
! but the last time)
! ---------------------------------------------------------------------

          DO 140 I=1,INSERT
           DESCR(N+IMBED+NBUNCH+I)=DESCR(N-INSERT+I-1)+FLINCR
  140     CONTINUE

! ---------------------------------------------------------------------
! & repeat (bunch will recur at intervals of nbunch+insert)
! ---------------------------------------------------------------------

          DO 150 I=1,NEXTRA-INSERT
           DESCR(N+IMBED+NBUNCH+INSERT+I)=DESCR(N+IMBED+I)
  150     CONTINUE

          ND=ND+NEXTRA

! ---------------------------------------------------------------------
! flag count descriptor for deletion
! ---------------------------------------------------------------------

          IF (IMBED.EQ.1) DESCR(N+1)=DESCR(N+1)+FLDEL

! ---------------------------------------------------------------------
! & move pointer past it & replication descr
! ---------------------------------------------------------------------

          N=N+IMBED+1

! ---------------------------------------------------------------------
! if ntimes=1, just flag any count descriptor for deletion;
! if ntimes=0, flag for deletion also the descriptors
! that would be replicated if ntimes>1 (incl. any increments).
! ---------------------------------------------------------------------

        ELSE IF (NTIMES.EQ.1) THEN
          IF (IMBED.EQ.1) DESCR(N+1)=DESCR(N+1)+FLDEL
          N=N+IMBED+1

        ELSE IF (NTIMES.EQ.0) THEN
          DO 160 I=-INSERT,IMBED+NBUNCH
           DESCR(N+I)=DESCR(N+I)+FLDEL
  160     CONTINUE

! ---------------------------------------------------------------------
! & move pointer past them all
! ---------------------------------------------------------------------

          N=N+IMBED+NBUNCH+1
        ENDIF

! ---------------------------------------------------------------------
! operator descriptor
! ---------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN
        IF (X.GT.5) THEN
          PRINT *,' OPERATION NOT DEFINED (F=2 BUT Y>5)'
          GO TO 999
        ENDIF
        DESCR(N)=DESCR(N)+FLDEL
        N=N+1

        IF (X.EQ.1 .AND. Y.GT.0) IWIDTH=Y-128
        IF (X.EQ.1 .AND. Y.EQ.0) IWIDTH=0

        IF (X.EQ.2 .AND. Y.GT.0) ISCALE=Y-128
        IF (X.EQ.2 .AND. Y.EQ.0) ISCALE=0

        IF (X.EQ.4) IASSOC=Y

! ---------------------------------------------------------------------
! keep a list of any changed reference values.  empty the list (set the
! count to zero) when the operation lapses.  add lines to the table so
! that the fields which define the new reference values can be skipped.
! ---------------------------------------------------------------------

        IF (X.EQ.3) THEN
          IF (Y.EQ.0) THEN
            NREF=0
          ELSE
            WIDTH=Y
  210       CONTINUE                                               !1.3
            CALL DESFXY(DESCR(N),F,X,Y)
            DESCR(N)=DESCR(N)+FLDEL
            N=N+1
            IF (F.EQ.0) THEN
              NREF=NREF+1
              NEWREF(NREF)=VALUE(STRING,IBEFOR,WIDTH)
              IF (CMPRES) THEN

! ---------------------------------------------------------------------
!  not obvious how to cope if new values vary from report to report,
!  so error.
! ---------------------------------------------------------------------

                KINC=VALUE(STRING,IBEFOR,6)
                IF (KINC.NE.0) THEN
                  PRINT *,' NEW REFERENCE VALUES DEFINED, BUT VARY FROM'
                  PRINT *,' REPORT TO REPORT IN COMPRESSED DATA'
                  GO TO 999
                ENDIF
              ENDIF
              IF (NEWREF(NREF).GE.TWOTO(Y-1)) THEN
                NEWREF(NREF)=TWOTO(Y-1)-NEWREF(NREF)
              ENDIF
              REFDES(NREF)=DESCR(N)
              GO TO 210
            ELSE

! ---------------------------------------------------------------------
! end of redefinition must be marked by descriptor 203255
! ---------------------------------------------------------------------

              IF (F.NE.2 .OR. X.NE.3 .OR. Y.NE.255) THEN
                PRINT *,' NEW REFERENCE VALUES, BUT END OF DEFINITION'
                PRINT *,' NOT MARKED BY REQUIRED DESCRIPTOR'
                GO TO 999
              ENDIF
            ENDIF
          ENDIF
        ENDIF

! ---------------------------------------------------------------------
! if characters inserted in bit string, skip them (& any increments)
! ---------------------------------------------------------------------

        IF (X.EQ.5) THEN
          IBEFOR=IBEFOR+Y*8

! ---------------------------------------------------------------------
! (length of char field in descr is in bytes)
! ---------------------------------------------------------------------

          IF (CMPRES) THEN
            INCWID=VALUE(STRING,IBEFOR,6)
            IBEFOR=IBEFOR+NOBS*INCWID*8
          ENDIF

! ---------------------------------------------------------------------
! ? print out characters if display ?
! ---------------------------------------------------------------------

        ENDIF

! ---------------------------------------------------------------------
! sequence descriptor
!
! (sequences can't be expanded at the start, because replication
! counts would have to be adjusted.)
! n stays pointing to the first descriptor in the expansion (so the
! sequence descriptor itself is overwritten).
! ---------------------------------------------------------------------

      ELSE IF (F.EQ.3) THEN
        CALL TABLED(X,Y,SEQ,NSEQ)
        IF (NSEQ.EQ.0) THEN
          PRINT *,N,'-TH DESCRIPTOR HAS F=3, BUT NOT IN TABLE D'
          GO TO 999
        ENDIF

! ---------------------------------------------------------------------
! insert sequence of descriptors, moving the rest down. adjust total.
! ---------------------------------------------------------------------

        DO 310 I=ND,N+1,-1
         DESCR(I+NSEQ-1)=DESCR(I)
  310   CONTINUE

        DO 320 I=1,NSEQ
         DESCR(N+I-1)=SEQ(I)
  320   CONTINUE

        ND=ND+NSEQ-1
      ENDIF

! ---------------------------------------------------------------------
! carry on round loop if there are descriptors left.  otherwise
! do the deletions and insertions indicated by flags set above.
! first do the deletions (moving left), counting the insertions.
! then, moving right, insert zero descriptors for q/c fields.
! ---------------------------------------------------------------------

      IF (N.LE.ND) THEN
        GO TO 1
      ELSE
        MOUT=0
        MQC=0
        DO 400 MIN=1,ND
         IF (.NOT.FLAGON(DESCR(MIN),FLDEL)) THEN
           MOUT=MOUT+1
           DESCR(MOUT)=DESCR(MIN)
           IF (FLAGON(DESCR(MIN),FLQC)) MQC=MQC+1
         ENDIF
  400   CONTINUE

        ND=MOUT

        IF (MQC.GT.0) THEN
          MADD=MQC
          I=ND
  410     CONTINUE
          DESCR(I+MADD)=DESCR(I)
          IF (FLAGON(DESCR(I),FLQC)) THEN
            DESCR(I+MADD)=DESCR(I+MADD)-FLQC
            MADD=MADD-1
            DESCR(I+MADD)=0
          ENDIF
          I=I-1
          IF (I.GT.0 .AND. MADD.GT.0) GO TO 410

          ND=ND+MQC
        ENDIF

        IF (.NOT.CMPRES .AND. NOB.LT.NOBS) THEN

! ---------------------------------------------------------------------
! if there is more than one set of data without compression, the next
! value of each element comes after the corres value in the last set.
! ---------------------------------------------------------------------

          NOB=NOB+1
          GO TO 11
        ELSE
          RETURN
        ENDIF
      ENDIF

  999 CONTINUE
      ND=N

      RETURN1
      END
