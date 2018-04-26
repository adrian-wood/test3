//M12DBZAP JOB (M00,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* TO CHANGE BYTES IN AN MDB DATA BASE.  GIVE THE CURRENT (BAD) STRING,
//* TO BE FOUND BEFORE ANY CHANGE IS MADE, AS WELL AS THE CORRECTION, &
//* SET DSN, BLOCKSIZE, BLOCK NUMBER, IN=START OF STRING TO BE CHANGED,
//* LEN=LENGTH OF STRING
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS'
      INTEGER BLKSIZ

!         ************************************************************
!         ** Set block no, Start and Length of string and Blocksize **
      PARAMETER (IRECNO=??,IN=?????,LEN=??,BLKSIZ=27998)
!                       **    *****     *******

      CHARACTER*(BLKSIZ) RECORD
      CHARACTER*(LEN) OLD,NEW

!               ****************          ***************************
      DATA OLD/Z03FCFDFDFDFDFD09/        !** Set current BAD string
      DATA NEW/Z0300000000000009/        !** Set new CORRECTED string
!               ****************          ***************************
*
      OPEN (1,ACCESS='DIRECT',RECL=BLKSIZ)
      READ (1,REC=IRECNO) RECORD
      IF (RECORD(IN:IN+LEN-1).EQ.OLD) THEN
        RECORD(IN:IN+LEN-1)=NEW
        WRITE (1,REC=IRECNO) RECORD
      ELSE
        PRINT *,'CHANGE NOT MADE. STRING FOUND WAS ',RECORD(IN:IN+LEN-1)
      ENDIF
      STOP
      END
/*
//*               ****************
//*               ** Set DsName **
//GO.FT01F001 DD DSN=MDB.??????,DISP=SHR
//
