//MDBDBGAR JOB (OP1,DB,WDS0BF),METDB,CLASS=J,USER=T12DB,
//  MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* Archive GRIB data sets into MASS-R  (datatype RADOP)
//*
//* Job generated from skeleton JCL MDB.JCL.CNTL(GARCDSKEL)
//* and submitted by MDB.JCL.CNTL(GARCDSUB)
//*---------------------------------------------------------------------
//* $Workfile: gardskel.jcl$ $Folder: JCL.CNTL$
//* $Revision: 2$ $Date: 21/07/2010 11:33:15$
//*
//JCL JCLLIB ORDER=(DSM.STORAGE.JCL)
//*
//* MDBMOOSE PROC STREAM='%STREM',FILE=
//* LINEONE EXEC PGM=LPUT,
//* PARM=' -p  //&FILE'
//* CTRL DD DSN=&&TEMP,DISP=(NEW,PASS),RECFM=FB,LRECL=80,BLKSIZE=800
//* LINETWO  EXEC PGM=LPUT,
//* PARM='moose:/misc/metdb/&STREAM..xmit.file/&FILE'
//* CTRL DD DISP=(MOD,PASS),DSN=&&TEMP
//*
//* MOO EXEC MOODATA
//* PARMS DD DSN=&&TEMP,DISP=(OLD,DELETE)
//* PEND
//*
//*  - + - + - + - + - + - + - + - + - + Repeatable JCL  - + - + - + -
//* ANN EXEC MDBMOOSE,FILE='%ARCFLE'
//*  - + - + - + - + - + - + - + - + - +  Do not alter + - + - + - + -
//*
//*---------------------------------------------------------------------
//* SCRATCH DATASETS IF RC FROM BACKUP IS 0
//*---------------------------------------------------------------------
//*
//* SCRATCH IF (RC EQ 0) THEN
//DELETE EXEC PGM=IDCAMS                                           !ST2
//SYSPRINT DD SYSOUT=Q                                             !ST2
//SYSIN   DD DSN=TDB.GRIBARC.SCRATCH,DISP=SHR                      !ST2
//*  ENDIF
//
