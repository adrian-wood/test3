//T12SCIN1 JOB (M12,SC,WDS0BF),MDBTEAM.6955,PRTY=8,MSGCLASS=Q
// EXEC FORT2CLG,FPARMS='NOFIPS'

!-----------------------------------------------------------------------
!
! Program       : MKBITIDX
!
! Purpose       : Generate bit index (for fast retrieval) from bufr
!               : descriptor sequence (input as in sdb.bufr.localseq).
!               : no entities or elements outside the data section
!               : are included, and the element names are from
!               : TABLE B rather than NEONS.
!
! Calls         : LOCALD, DESFXY, TABLEB, TABLED                    !1.3
!
! Change record : operational from 27-08-96
!
! 12-07-96      : Output a count to serve as a place holder for the
!               : element number. Element numbers will need editing
!               : by hand when the full list is completed (header
!               : elements etc.) Names are still output as well for
!               : readability, but they will not be transferred to the
!               : final index - C.Long
!
! 26-03-97  !A  : Add 'MESTRUCT_FOLLOWS' line - S.Cox
!
! 16-05-97  !B  : Add call to LOCALB for local table B elements - S.Cox
!
! 27-05-97  !C  : Now read sequence from unit 1 and write bitindex to
!               : to unit 2. Can also read in LOCALSEQ's - S.Cox
!
! 23-06-97  !D  : Can now read in LOCALSEQ's - S.Cox
!
! 08-03-99  !E  : Skip line of revision information if present when
!               : reading BUFR sequence from dataset - S.Cox
!
!-----------------------------------------------------------------------

      PROGRAM MKBITIDX

      CHARACTER*80 INSEQ(200)
      CHARACTER*80 REVISION                                           !E
      CHARACTER    NAME*64,UNITS*24,FORMAT*1
      INTEGER      F,X,Y,DES(999),SEQ(999), SCALE,REFVAL,WIDTH
      INTEGER      DUMMY1                                             !D
      REAL         Z                                                  !B

!-----------------------------------------------------------------------
! read in sequence, put it in local table d, set first descriptor
!-----------------------------------------------------------------------

      OPEN (1,FORM='UNFORMATTED')                                     !C
      READ (1) REVISION                                               !E
      IF (INDEX(REVISION,'$Revision:') .LE.0 ) REWIND (1)             !E
      I=1
   20 READ (1,END=21) INSEQ(I)                                        !C
      I=I+1
      GO TO 20
   21 INSEQ(I+1)=' END '

      CALL LOCALD(X,Y,Z,Q,INSEQ,'ADD')    ! only last 2 arguments used

      READ (INSEQ(1)(1:6),*) N
      DES(1)=IDES(N)
      WRITE(2,'(A6)')INSEQ(1)(1:6)                                    !C

!-----------------------------------------------------------------------
! put LOCALSEQ descriptors into memory
!-----------------------------------------------------------------------

      CALL LOCALD(0,0,Z,DUMMY1,' ','ADD')                             !D

!-----------------------------------------------------------------------
! initialise loop variables & bufr scale change etc
!-----------------------------------------------------------------------

      ND=1
      N=1
      ISEG=1
      IBEFOR=32        ! START IN DATA SECTION AFTER LENGTH BITS
      IBEFOR0=-256     ! SEGMENT 1 STARTS AT 'BUFR', SO ADD (4+18+10)*8

      ISCALE=0
      IWIDTH=0
      IASSOC=0

!-----------------------------------------------------------------------
! convert descriptor to f, x & y & check for marker after replication
!-----------------------------------------------------------------------

  100 CALL DESFXY(DES(N),F,X,Y)

      IF (DES(N).EQ.-1) THEN
        ISEG=ISEG+1
        IBEFOR0=IBEFOR
        N=N+1

!-----------------------------------------------------------------------
! move replicated descriptors up to overwrite the operator, put a marker
! at the end; increment segment number & reset bit count as above (but
! not if that has just been done at the end of the previous replication)
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.1) THEN
        IF (IBEFOR.GT.IBEFOR0) THEN
          ISEG=ISEG+1
          IBEFOR0=IBEFOR
        ENDIF
                                              ! ASSUME ONLY 031001 USED
        IF (DES(N+1).EQ.IDES(031001)) THEN    !  AS REPLICATION COUNT!
          IBEFOR0=IBEFOR0+8                   ! - SO LENGTH IS ALWAYS 8.
          IBEFOR=IBEFOR+8
          WRITE (2,2) N,ISEG
    2     FORMAT ('  REPLICATION COUNT              ',
     &      I6,',',I3,',  -8,  8,   0,          0')
          N=N+1
        ENDIF

        DO I=1,X            ! REPLICATED FOLLOW IMMEDIATELY.
          DES(N+I-1)=DES(N+I)
        ENDDO
        DES(N+X)=-1         ! PUT MARKER AFTER DESCRIPTORS MOVED UP.

!-----------------------------------------------------------------------
! keep any scale & field width changes & associated field width if set.
! assume that our q/c sequence is only for adding model values to data
! which will be compressed and therefore not need fast retrieval, and
! that any other operators don't occur in our data.
!-----------------------------------------------------------------------

      ELSE IF (F.EQ.2) THEN
        IF (X.EQ.1 .AND. Y.GT.0) IWIDTH=Y-128
        IF (X.EQ.1 .AND. Y.EQ.0) IWIDTH=0
        IF (X.EQ.2 .AND. Y.GT.0) ISCALE=Y-128
        IF (X.EQ.2 .AND. Y.EQ.0) ISCALE=0
        IF (X.EQ.4) IASSOC=Y
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

        CALL TABLEB(X,Y,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

        ITABLE=INDEX(UNITS,'TABLE')
        IF (X.NE.31) THEN         ! NO OPERATOR APPLIES TO CLASS 31.
          IF (ITABLE.EQ.0) THEN   ! SCALE & WIDTH CHANGES DON'T APPLY
            SCALE=SCALE+ISCALE    !  TO CODE OR FLAG TABLES.
            WIDTH=WIDTH+IWIDTH
          ENDIF
          IBEFOR=IBEFOR+IASSOC
        ENDIF

        WRITE (2,1) NAME(1:33),N,ISEG,IBEFOR-IBEFOR0,WIDTH,           !C
     &              SCALE,REFVAL
  1     FORMAT (A33, I6,',',I3,',', I4,',', I3,',', I4,',', I11)
        IBEFOR=IBEFOR+WIDTH
        N=N+1
      ENDIF
      IF (N.LE.ND) GO TO 100

      WRITE(2,'(A16)')'MESTRUCT_FOLLOWS'                            !C!A

      STOP
      END

/*
//LKED.DB    DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN DD *
 INCLUDE DB(BUFRSHEL)
/*
//* -----------------------------------------------------------------
//* FT01F001 : is the input sequence to make a BITINDEX from.
//* LOCALSEQ : MUST be the same as FT01F001 unless there are "real"
//*          : local sequences to read in.
//* FT02F001 : is the output BITINDEX.
//* -----------------------------------------------------------------
//*
//GO.TABLEB   DD DSN=SDB.BUFR.TABLEB,DISP=SHR
//GO.TABLED   DD DSN=SDB.BUFR.TABLED,DISP=SHR
//GO.LOCALSEQ DD DSN=MDB.MERGE.SEQUENCE(UPRAIRCL),DISP=SHR
//GO.FT01F001 DD DSN=MCC3.SC.MERGE(UPRSEQ),DISP=SHR
//GO.FT02F001 DD DSN=MCC3.SC.MERGE(UPRBIT),DISP=SHR
