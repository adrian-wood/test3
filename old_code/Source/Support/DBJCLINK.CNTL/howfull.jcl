//T12DBFUL JOB (OP1,DB,WDS0BF),MDBTEAM.X6675,MSGCLASS=N,PRTY=8
//*
//*=====================================================================
//*                  MCC3.DBJCLINK.CNTL(HOWFULL)
//*                  ---------------------------
//*      JOB TO BUILD LOAD MODULE FOR "MDB.LOADLIB(HOWFULL)".
//*
//* REVISION INFO:
//* --------------
//* $Workfile: howfull.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 4$ $Date: 20/05/2008 09:36:43$
//*
//* $Log:
//*  4    Met_DB_Project 1.3         20/05/2008 09:36:43    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  3    Met_DB_Project 1.2         19/12/2007 14:55:37    Brian Barwell
//*       Source pre-processing step added.
//*  2    Met_DB_Project 1.1         06/03/2006 15:10:09    Brian Barwell
//*       Account on JOB statement changed from TCDS1B to WDS0BF.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:47    Sheila Needham  
//* $
//*=====================================================================
//*        PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS           !3
//*        -----------------------------------------------           !3
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !4
//*   names like 'name.F' rather than 'name.f', e.g. 'howfull.F'.)   !4
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !4
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !4
//*
//FT06F001 DD SYSOUT=Q                                               !2
//FT10F001 DD DSN=MDB.UTILITY.SRCE(HOWFULL),DISP=SHR                 !4
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(1,1)),              !4
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
//HOWFULL EXEC FORT2CL,FPARMS='CHARLEN(28000),NOFIPS,DC(*),OPT(3)'
//*
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !4
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !4
//             DSN=MCC3.DBLOAD.Dyymmdd(HOWFULL)                      !4
//
