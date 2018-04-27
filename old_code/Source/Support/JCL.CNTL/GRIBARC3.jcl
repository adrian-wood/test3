//MDBDBGJ3 JOB (OP1,DB,WDS0BF),METDB,CLASS=H,USER=T12DB,
//  MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* Job to generate and submit JCL to archive RADOP2 data sets.
//* Submitted by script MDB.REXX.EXEC(GRIBARC)
//*---------------------------------------------------------------------
//* $Workfile: GRIBARC3.jcl$ $Folder: JCL.CNTL$
//* $Revision: 1$ $Date: 07/12/2012 16:30:03$
//*
//GARSUB EXEC PGM=GARCSUB
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//FT06F001 DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=151,BLKSIZE=1510)
//FT11F001 DD DSN=MDB.JCL.CNTL(GARCSKL3),DISP=SHR
//FT13F001 DD DSN=MDB.GRIBARC.ARCHIVE3,DISP=SHR
//GO.INTOUT DD SYSOUT=(A,INTRDR)
//*
//
