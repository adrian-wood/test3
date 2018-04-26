//T12SCBFI JOB (M12,SC,WDS0BF),SDBTEAM.6955,PRTY=??,MSGCLASS=Q
// EXEC FORT2CLG,FPARMS='NOFIPS',TIME.GO=(0,2)
//FORT.SYSIN DD *

!-----------------------------------------------------------------------
!
! Program       : MKBUFIDX
!
! Purpose       : generate array index from BUFR descriptor sequence.
!
! Calls         : LOCALD, DESFXY, TABLEB, TABLED                    !1.4
!
! Change record : S.Needham written
!
! 04-12-1996 !A : Add operator check for 323YYY descriptors - S.Cox
!
! 18-06-1997 !B : Add call to LOCALB for local table B elements and
!               : now read sequence from unit 1 and write bitindex to
!               : to unit 2. Can also read in LOCALSEQ's. Also write
!               : message structure segements to separate lines - S.Cox
!
! 19-06-1997 !C : Allow associated QC bits to be cancelled on finding
!               : descriptor 204000 - S.Cox
!
! 08-10-1997 !D : Only set ADD back to 0, if QC set back to 0, as
!               : change !C wasn't quite right - S.Cox
!
! 28-05-1998 !E : Change code to cope with 031031 - S.Cox
!
! 13-07-1998 !F : Can now read in LOCALSEQ's - S.Cox
!
! 08-03-1999 !G : Skip line of revision information if present when
!               : reading BUFR sequence from dataset - S.Cox
!
!-----------------------------------------------------------------------

      PROGRAM MKBUFIDX

      CHARACTER*80     INSEQ(999)
      CHARACTER*80     REVISION                                       !G
      CHARACTER        NAME*64,UNITS*24,FORMAT*1
      CHARACTER*60     FORM
      INTEGER          F,X,Y,DES(9999),SEQ(9999),SCALE,REFVAL,WIDTH
      INTEGER          LSEG,CQ,ADD
      INTEGER          LSEGD,LSEGV
      INTEGER          DSEG(99),VSEG(99),SEGT(99)
      INTEGER          DUMMY1                                         !F
      INTEGER          INUM

      LSEGD=0          ! length of descriptors in segment
      LSEGV=0          ! length of values in segment
      ISEG=1           ! segment number
      SEGT(ISEG)=0     ! type of segment (0=Mand, 8=Optional)
      INUM=0

!-----------------------------------------------------------------------
! read in local sequence, put it in local table d, set first descriptor
!-----------------------------------------------------------------------

      OPEN (1,FORM='UNFORMATTED')                                     !B
      READ (1) REVISION                                               !G
      IF (INDEX(REVISION,'$Revision:') .LE.0 ) REWIND (1)             !G
      I=1
   20 READ (1,END=21) INSEQ(I)                                        !B
      I=I+1
      GO TO 20
   21 INSEQ(I+1)=' END '
      PRINT*,' LOCAL SEQ READ. LENGTH=',I

      CALL LOCALD(X,Y,Z,Q,INSEQ,'ADD')    ! ONLY LAST 2 ARGUMENTS USED
      READ (INSEQ(1)(1:6),*) N
      WRITE(2,'(A6)')INSEQ(1)(1:6)                                    !B
      DES(1)=IDES(N)
      PRINT*,' LOCAL TABLE D NUMBER ',N,DES(1)

!-----------------------------------------------------------------------
! put LOCALSEQ descriptors into memory
!-----------------------------------------------------------------------

      CALL LOCALD(0,0,Z,DUMMY1,' ','ADD')                             !F

!-----------------------------------------------------------------------
! initialise loop variables
!-----------------------------------------------------------------------

      QC=0    ! BECOMES 1 IF A 204XXX OPERATOR FOUND
      ADD=0   ! BECOMES 1 WHEN A 031021 OPERATOR FOUND
      ND=1
      N=1

!-----------------------------------------------------------------------
! loop over descriptors
! convert to F,X,Y
!-----------------------------------------------------------------------

  100 CONTINUE
      CALL DESFXY(DES(N),F,X,Y)
      WRITE(6,1000)N,F,X,Y
1000  FORMAT(' NEXT N ',I3,1X,I1,I2.2,I3.3)

!-----------------------------------------------------------------------
! check for flag at end of replicated section. start a new segment if
! this is not the last descriptor.
!-----------------------------------------------------------------------

      IF (DES(N).EQ.-1) THEN

!-----------------------------------------------------------------------
! save previous segment counts
!-----------------------------------------------------------------------

        DSEG(ISEG)=LSEGD      ! DESCRIPTOR COUNT
        VSEG(ISEG)=LSEGV      ! VALUES COUNT
        LSEGD=0
        LSEGV=0

!-----------------------------------------------------------------------
! increment segment number
!-----------------------------------------------------------------------

        IF(N.LT.ND)THEN
          ISEG=ISEG+1
          SEGT(ISEG)=0
        ENDIF
        N=N+1 ! NEXT DESCRIPTOR

!-----------------------------------------------------------------------
! move replicated descriptors up to overwrite the operator, put a marker
! at the end; increment segment number etc above (but not if that has
! just been done at the end of the previous replication)
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.1) THEN

        IF(LSEGV.GT.0)THEN

!-----------------------------------------------------------------------
! start of a new segment
!-----------------------------------------------------------------------

          DSEG(ISEG)=LSEGD
          VSEG(ISEG)=LSEGV
          LSEGD=0
          LSEGV=0

!-----------------------------------------------------------------------
! increment segment number
!-----------------------------------------------------------------------

          ISEG=ISEG+1
        ENDIF
        SEGT(ISEG)=8     !OPTIONAL SEGMENT TYPE
        LSEGD=LSEGD+1

!-----------------------------------------------------------------------
! assume these are the only descriptors used for delayed replication
!-----------------------------------------------------------------------

        IF (DES(N+1).EQ.IDES(031001) .OR. DES(N+1).EQ.IDES(031002)) THEN
          N=N+1
        ENDIF

        DO I=1,X
          DES(N+I-1)=DES(N+I)
        ENDDO
        DES(N+X)=-1
        INUM=INUM+1

!-----------------------------------------------------------------------
! in debufr output, the count comes from the descriptor not the data
! so use value 0 to indicate this
!-----------------------------------------------------------------------

        WRITE (2,2) INUM,ISEG,0,LSEGD                                 !B
    2   FORMAT (' REPLICATION COUNT               ',I6,',',I3,',',
     &       I4,',',I4)

!-----------------------------------------------------------------------
! increment counts for different operators
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN

!-----------------------------------------------------------------------
! appears in the descriptors but not the data
!-----------------------------------------------------------------------

        IF (X.EQ.2 .OR. X.EQ.23) THEN                                 !A
          LSEGD=LSEGD+1
        ENDIF

!-----------------------------------------------------------------------
! adds qc descriptor and value to every other element descriptor
!-----------------------------------------------------------------------

        IF (X.EQ.4) THEN
          IF (Y.NE.0) THEN                                            !C
            QC=1
          ELSE                                                        !C
            QC=0                                                      !C
            ADD=0                                                     !D
          ENDIF                                                       !C
        ENDIF
        N=N+1

!-----------------------------------------------------------------------
! insert sequence of descriptors, moving the rest down. adjust total.
! (the sequence descriptor itself is overwritten, so no n+n+1.)
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.3) THEN
        CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
        IF (NSEQ.EQ.0) CALL TABLED(X,Y,SEQ,NSEQ)

        DO I=ND,N+1,-1
          DES(I+NSEQ-1)=DES(I)
        ENDDO

        DO I=1,NSEQ
          DES(N+I-1)=SEQ(I)
        ENDDO

        ND=ND+NSEQ-1

!-----------------------------------------------------------------------
! print element details, adjusting table b scale etc if necessary
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.0) THEN

        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)         !B

        ITABLE=INDEX(UNITS,'TABLE')

!-----------------------------------------------------------------------
! assume this is the only qc descriptor we use to give meaning to bits
! added by 204xxx
!-----------------------------------------------------------------------

        IF(X.EQ.31.AND.Y.EQ.21)THEN
          ADD=1
        ELSEIF ((X.NE.31) .OR. (X.EQ.31 .AND. Y.EQ.31)) THEN          !E
          LSEGV=LSEGV+QC+ADD+1
          LSEGD=LSEGD+QC+ADD+1
          INUM=INUM+1

          WRITE (2,1) NAME(1:33),INUM,ISEG,LSEGV,LSEGD                !B
    1     FORMAT (A33, I6,',',I3,',',I4,',',I4)
        ENDIF
        N=N+1
      ENDIF
      IF (N.LE.ND) GO TO 100

!-----------------------------------------------------------------------
! counts for last segment
!-----------------------------------------------------------------------

      IF(LSEGV.GT.0)THEN
        DSEG(ISEG)=LSEGD      ! DESCRIPTOR COUNT
        VSEG(ISEG)=LSEGV      ! VALUES COUNT
      ENDIF

      WRITE(2,'(A16)')'MESTRUCT_FOLLOWS'                              !B
      WRITE(2,'(I4)')ISEG                                             !B

      DO I=1,ISEG                                                     !B
        WRITE(2,'(I4,'','',I4,'','',I4)')SEGT(I),VSEG(I),DSEG(I)      !B
      ENDDO                                                           !B

      STOP
      END
/*
//LKED.DB DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN DD *
 INCLUDE DB(BUFRSHEL)
/*
//* -----------------------------------------------------------------
//* FT01F001 : is the input sequence to make a BUFINDEX from.
//* LOCALSEQ : MUST be the same as FT01F001 unless there are "real"
//*          : local sequences to read in.
//* FT02F001 : is the output BUFINDEX.
//* -----------------------------------------------------------------
//*
//GO.TABLEB   DD DSN=SDB.BUFR.TABLEB,DISP=SHR
//GO.TABLED   DD DSN=SDB.BUFR.TABLED,DISP=SHR
//GO.LOCALSEQ DD DSN=MCC3.SCJCLLIB.CNTL(ESASEQ),DISP=SHR,
//  LABEL=(,,,IN)
//GO.FT01F001 DD DSN=MCC3.SCJCLLIB.CNTL(ESASEQ),DISP=SHR,
//  LABEL=(,,,IN)
//GO.FT02F001 DD DSN=MCC3.SCJCLLIB.CNTL(ESABIT),DISP=SHR
//
