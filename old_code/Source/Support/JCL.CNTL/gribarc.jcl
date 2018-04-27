//MDBDBGAJ JOB (OP1,DB,WDS0BF),METDB,CLASS=H,USER=T12DB,
//  MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* Job to generate and submit JCL to archive GRIB data sets.
//* Submitted by script MDB.REXX.EXEC(GRIBARC)
//*---------------------------------------------------------------------
//* $Workfile: gribarc.jcl$ $Folder: JCL.CNTL$
//* $Revision: 2$ $Date: 25/08/2009 13:18:16$
//*
//GARSUB EXEC PGM=GARCSUB
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//FT06F001 DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=151,BLKSIZE=1510)
//FT11F001 DD DSN=MDB.JCL.CNTL(GARCSKEL),DISP=SHR
//FT13F001 DD DSN=MDB.GRIBARC.ARCHIVE,DISP=SHR
//GO.INTOUT DD SYSOUT=(A,INTRDR)
//*
//
