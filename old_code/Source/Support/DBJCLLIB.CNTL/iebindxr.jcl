//T12DBUPE JOB (M12,JL,WDS0BF),JON.6954,PRTY=8,
//      NOTIFY=T12JL,MSGCLASS=Q
//*
//REGRESS   EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=MDB.DBBAKUP.ELEMENT.INDEX,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=MDB.ELEMENT.INDEX,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//
