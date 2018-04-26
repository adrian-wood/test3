//M12DBFTP JOB (M12,CL,WDS0BF),C.LONG.X6953,PRTY=8,MSGCLASS=Q
//*
//* to display a 1-minute bulletin data set
//* (decoding any BUFR message, which may produce a lot of output:
//*  comment out DEBUFR call if BUFR data not wanted)
//*
// EXEC FORT2C
//SYSIN DD DSN=MET.SRCELIB(IBMISO8),DISP=SHR
// EXEC FORT2CLG,FPARMS='CHARLEN(32000),NOFIPS',TIME.GO=(0,3)
      CHARACTER*28672 BLOCKS   ! 7 blocks of 4096
      CHARACTER*4 SOH/Z010D0D0A/,BUFR/Z42554652/,CS*3333
      INTEGER BULSTR,BULLEN,BLEND,NBUL/0/
      INTEGER DESC(50000)
      REAL OUT(50000)

! Read the first 7 blocks (if there are that many)

      DO I=0,6
        READ (1,IOSTAT=IRC) BLOCKS(I*4096+1:I*4096+4096)
        IF (IRC.NE.0) THEN
          BLEND=I*4096
          GO TO 5
        ENDIF
      ENDDO
      BLEND=7*4096

! Look for start of bulletin.  Length is before X'01'.

    5 BULSTR=INDEX(BLOCKS(1:BLEND),SOH)
      IXSOH=1
   10 IF (IXSOH.GT.0) THEN
        BULSTR=BULSTR+IXSOH-1
        READ(BLOCKS(BULSTR-4:BULSTR-1),'(A4)') BULLEN
        PRINT *,' '
        PRINT *,BULLEN,'-byte bulletin:'

! Read in more data if length goes beyond what is now in BLOCKS,
! moving the block where this bulletin starts to the start of BLOCKS.

        IF (BULSTR+BULLEN.GT.7*4096) THEN
          NB=BULSTR/4096     ! number of blocks which are finished with
          DO I=1,7-NB        ! move up blocks to overwrite them
            BLOCKS((I-1)*4096+1:I*4096)=
     &      BLOCKS((I-1+NB)*4096+1:(I+NB)*4096)
          ENDDO
          DO I=7-NB+1,7      ! & read in new blocks to make up the 7
            READ (1,IOSTAT=IRC) BLOCKS((I-1)*4096+1:I*4096)
            IF (IRC.NE.0) THEN
              BLEND=(I-1)*4096
              GO TO 15
            ENDIF
          ENDDO
          BLEND=7*4096
   15     BULSTR=BULSTR-NB*4096  ! adjust pointer for blocks removed
        ENDIF

! If BUFR, print one line & decode; otherwise delimit lines by CRCRLF

        IXBUFR=INDEX(BLOCKS(BULSTR:BULSTR+BULLEN),BUFR)
        IF (IXBUFR.GT.0) THEN
          CALL ASC2EB(IXBUFR-2,BLOCKS(BULSTR+1:BULSTR+IXBUFR-2))
          PRINT *,BLOCKS(BULSTR+1:BULSTR+80)
          ND=50000
          NOB=50000
          CALL DEBUFR(DESC,OUT,CS,ND,NOB,BLOCKS(BULSTR+1:),.TRUE.)
        ELSE
          LINSTR=1
   20     LINEND=INDEX(BLOCKS(BULSTR+LINSTR+3:BULSTR+BULLEN),SOH(2:4))
          IF (LINEND.GT.0) THEN
            CALL ASC2EB(LINEND,BLOCKS(BULSTR+LINSTR+3:))
            PRINT *,BLOCKS(BULSTR+LINSTR+3:BULSTR+LINSTR+LINEND+1)
            LINSTR=LINSTR+LINEND+2
            GO TO 20
          ENDIF
        ENDIF

! Move on to start of next bulletin

        BULSTR=BULSTR+BULLEN-IXSOH+1
        NBUL=NBUL+1
        IXSOH=INDEX(BLOCKS(BULSTR:BLEND),SOH)
        GO TO 10
      ENDIF
      print *,' '
      print *,NBUL,'bulletins in this data set'
      STOP
      END
//LKED.DB DD DSN=SYS1.SDBLOAD,DISP=SHR
    INCLUDE DB(BUFR)
//GO.TABLEB DD DSN=MCC3.DBBUFR.TABLEB,DISP=SHR                          00090000
//GO.TABLED DD DSN=MCC3.DBBUFR.TABLED,DISP=SHR                          00100000
//GO.CODEFIG DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR                          00100000
//GO.FT01F001 DD DSN=MHSP.R2D00???.T??????.SDB1.S000,
//   DISP=SHR,LABEL=(,,,IN)
