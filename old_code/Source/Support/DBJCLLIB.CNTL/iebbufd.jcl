//T12DBUPD JOB (OP1,DB,WDS0BF),SI.6955,CLASS=I,
//      NOTIFY=T12DB,MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* TO UPDATE SDB.BUFR.TABLED
//*
//* $Revision: 1$ $Date: 28/02/2006 12:18:29$
//*---------------------------------------------------------------------
//*
//SAVETD    EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.TABLED,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.TABLED.OLD,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//UPDTD     EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.TABLED.NEW0317,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.TABLED,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//
