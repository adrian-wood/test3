//T12DBUPE JOB (OP1,DB,WDS0BF),MDBTEAM.6954,PRTY=10,
//      NOTIFY=&SYSUID,MSGCLASS=Q
//*
//SAVEOP    EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=MDB.ELEMENT.INDEX,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=MDB.DBBAKUP.ELEMENT.INDEX,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//UPDATE    EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=MCC3.BCELIDX.N170700,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=MDB.ELEMENT.INDEX,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//