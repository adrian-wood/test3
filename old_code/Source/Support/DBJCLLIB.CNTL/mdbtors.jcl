//M12DBTOR JOB (M00,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* PRINTS DISTRIBUTION OF REPORTS BY TIME OF RECEIPT FOR MDB DATA SET
//* (SET BLOCKSIZE IN OPEN; DOESN'T COPE WITH SEQUENCE IN SECOND BLOCK)
//*                         ******************************************
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS',TIME.GO=(0,3)
//FORT.SYSIN DD *
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
*
*Y2K  16.06.1997  MDBTORS is Year 2000 compliant.
*
*   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *
      CHARACTER*15476 INDEX
      INTEGER*2 BLOCKS,XBLOKS
      INTEGER HOURMES(25),HOUROB(25) ! FOR DISTRIBUTION BY T.O.R.

!                                  ***** *******************
      OPEN (1,ACCESS='DIRECT',RECL=15476) !** Set Blocksize **!
!                                  ***** *******************

      READ (1,REC=1) BLOCKS,XBLOKS   ! GET NUMBER OF INDEX BLOCKS
*
      DO 100 IND=1,XBLOKS            ! LOOP ROUND INDEX BLOCKS
      READ (1,REC=1+IND) INDEX
      INDAY=ICHAR(INDEX(1:1))        ! DAY OF INDEX BLOCK
      INDHR=ICHAR(INDEX(2:2))        ! HOUR OF INDEX BLOCK
      NMESS=ICHAR(INDEX(3:3))*256+ICHAR(INDEX(4:4))
*
      DO 11 I=1,25
      HOURMES(I)=0
   11 HOUROB(I)=0
*
      NOBS=0                         ! TO COUNT REPORTS IN MESSAGES
      DO 10 I=1,NMESS                ! LOOP ROUND MESSAGES
       IT=6+(I-1)*23                 ! 6 BYTES AT START, 23-BYTE ENTRIES
      NOB=ICHAR(INDEX(IT+12:IT+12))
      NOBS=NOBS+NOB
*
      ITOR=ICHAR(INDEX(IT+18:IT+18))*256+ICHAR(INDEX(IT+19:IT+19))
      ITORHOR=ITOR/60
      IF (ITORHOR.GT.24) ITORHOR=24
      HOURMES(ITORHOR+1)=HOURMES(ITORHOR+1)+1
      HOUROB(ITORHOR+1)=HOUROB(ITORHOR+1)+NOB
   10 CONTINUE
*
      WRITE (*,1) INDAY,INDHR,NMESS,NOBS
    1 FORMAT (I3,'/',I2.2,'Z',I7,' MESSAGES',I7,' REPORTS')
      WRITE (*,'(25I4)') HOURMES
      WRITE (*,'(25I4)') HOUROB
  100 CONTINUE
      STOP
      END
/*
//*                 ****************
//*                 ** Set DsName **
//GO.FT01F001 DD DSN=MDB.????????,DISP=SHR,LABEL=(,,,IN)
