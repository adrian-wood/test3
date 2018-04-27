//T12DBUPB JOB (OP1,DB,WDS0BF),SI.6955,CLASS=I,
//      NOTIFY=T12DB,MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* TO UPDATE SDB.BUFR.TABLEB
//*
//* $Revision: 1$ $Date: 28/02/2006 12:18:29$
//*---------------------------------------------------------------------
//*
//SAVETB    EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.TABLEB,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.TABLEB.OLD,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//UPDTB     EXEC PGM=IEBGENER
//SYSPRINT  DD  SYSOUT=Q
//SYSUT1    DD  DSN=SDB.BUFR.TABLEB.NEW0317,DISP=SHR,UNIT=DISK
//SYSUT2    DD  DSN=SDB.BUFR.TABLEB,DISP=SHR,UNIT=DISK
//SYSIN     DD  DUMMY
//*
//
