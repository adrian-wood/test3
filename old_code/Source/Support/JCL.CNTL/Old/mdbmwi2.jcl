//MDBMWI2 JOB (OP1,DB,WDS0BF),MDBTEAM.6953,CLASS=N,MSGCLASS=Q,
//     MSGLEVEL=(1,1)
//*    THIS TASK IS SUBMITTED BY CF (NWP) WHEN ERS-2 MWI DATA
//*    ARE TO BE STORED IN THE METDB
//*
// EXEC PGM=BUFRERS,TIME=(0,10),REGION=4096K
//STEPLIB DD DSN=SDB.LOADLIB,DISP=SHR
//FT01F001 DD DSN=SDB.MWI2.BUFR,DISP=SHR
//FT03F001 DD DSN=MDB.ERSMWI,DISP=SHR
//FT10F001 DD DSN=SDB.MWILOCK,DISP=OLD
//TABLEB DD DSN=SDB.BUFR.TABLEB,DISP=SHR,LABEL=(,,,IN)
//TABLED DD DSN=SDB.BUFR.TABLED,DISP=SHR,LABEL=(,,,IN)
//CODEFIG DD DSN=SDB.BUFR.CODEFIGS,DISP=SHR
//SYSUDUMP DD SYSOUT=M
//*
//