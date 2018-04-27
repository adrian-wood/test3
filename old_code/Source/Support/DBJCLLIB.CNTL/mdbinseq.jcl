//M12DBSEQ JOB (M12,CL,WDS0BF),C.LONG.6953,PRTY=8,MSGCLASS=Q
//*
//* TO COPY AN MDB DATA SET PUTTING A BUFR SEQUENCE IN THE SECOND BLOCK
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS'
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
*Y2K  16.06.1997  MDBINSEQ is Year 2000 compliant.
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
      INTEGER*2 BLOCKS,XBLOKS,XHOURS,AMSTAR, NOFLOW,INDENT
                                              ! ****************
      PARAMETER (LKSIZE=23476)                ! ** Set blocksize
                                              ! ****************
      CHARACTER BLOCK*(LKSIZE),MAP*(LKSIZE-12)
*
* GET BLOCK COUNT TO FIND WHERE TO SET FLAG FOR SEQUENCE BLOCK
*
      READ (1) BLOCKS,XBLOKS,XHOURS,AMSTAR, MAP, NOFLOW,INDENT
      MAP(BLOCKS:BLOCKS)=CHAR(2)               ! SEQUENCE IN BLOCK 2
      IF (NOFLOW.GT.0) THEN                    ! IF THERE ARE OVERFLOWS,
        NOFLOW=NOFLOW-1                        ! THERE'S ONE LESS NOW.
      ENDIF
      WRITE (2) BLOCKS,XBLOKS,XHOURS,AMSTAR, MAP, NOFLOW,INDENT
*
* READ IN LOCAL SEQUENCE (80-BYTE RECORDS) & WRITE IT TO SECOND BLOCK
*
      L=0
   10 READ (3,END=11) BLOCK(L*80+1:L*80+80)
      L=L+1
      IF (L*80+5.LE.LKSIZE) THEN
        GO TO 10
      ELSE
        PRINT *,' LOCAL SEQUENCE TOO LONG FOR BLOCK '
      ENDIF
   11 BLOCK(L*80+1:L*80+5)=' END '
      WRITE (2) BLOCK
*
* COPY REMAINING BLOCKS FROM INPUT (ASSUMING LAST BLOCK NOT USED!)
*
      DO 12 J=2,BLOCKS-1
      READ (1) BLOCK                           ! READ J-TH BLOCK
   12 WRITE (2) BLOCK                          ! WRITE (J+1)-TH
      STOP
      END
//GO.FT03F001 DD DSN=SDB.BUFR.LOCALSEQ(????????),DISP=SHR
//*            ***************
//*            ** Input DSN **
//FT01F001 DD DSN=MDB.??????,DISP=SHR,LABEL=(,,,IN)    INPUT
//*             ****************
//*             ** Output DSN **
//FT02F001 DD DSN=MDB.??????????,DISP=SHR             OUTPUT(SAME SIZE)
