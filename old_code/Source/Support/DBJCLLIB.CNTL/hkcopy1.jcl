//T12DBHK1 JOB (M12,JH,WDS0BF),DATABANKTEAM.6953,CLASS=I,
// NOTIFY=&SYSUID,MSGCLASS=Q,MSGLEVEL=(1,1)
//*
//*  TO COPY SDB.HKDS TO DATAPARK FOR SNAPSHOT OF CURRENT OPERATIONAL
//*  DATASET AND FOR EDITING BEFORE UPDATING CURRENT OPERATIONAL.
//*  RUN AFTER STORAGE HAS BEEN STOPPED.
//*
//   EXEC PGM=IEBGENER
//SYSPRINT DD DUMMY
//SYSUT1   DD DSN=SDB.HKDS,UNIT=DISK,DISP=SHR,LABEL=(,,,IN),
//         VOL=SER=OPR103
//SYSUT2   DD DSN=MCC3.DBHKOLD,DISP=(NEW,CATLG),
//         STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//         RECFM=F,BLKSIZE=100,LRECL=100,SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//   EXEC PGM=IEBGENER
//SYSPRINT DD DUMMY
//SYSUT1   DD DSN=SDB.HKDS,UNIT=DISK,DISP=SHR,LABEL=(,,,IN),
//         VOL=SER=OPR103
//SYSUT2   DD DSN=MCC3.DBHKNEW,DISP=(NEW,CATLG),
//         STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//         RECFM=F,BLKSIZE=100,LRECL=100,SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//*
//*    PRINTS OUT DETAILS OF HOUSEKEEPING DATASETS
//*
//*EXEC   SDBHKDPR,NAME='SDB.HKDS',VOL=OPR103,UN=DISK
//*
//