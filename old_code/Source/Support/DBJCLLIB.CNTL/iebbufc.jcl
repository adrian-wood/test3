//T12DBUPC JOB (OP1,DB,WDS0BF),SI.6955,CLASS=I,
//      NOTIFY=T12DB,MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* TO UPDATE SDB.BUFR.CODEFIGS
//*
//* $Revision: 1$ $Date: 28/02/2006 12:18:29$
//*---------------------------------------------------------------------
//*
//SAVETC    EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.CODEFIGS,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.CODEFIGS.OLD,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//UPDTC     EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.CODEFIGS.NEW0317,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.CODEFIGS,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//
