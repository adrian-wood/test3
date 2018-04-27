//T12DBSMT JOB (OP1,DB,WDS0BF),METDB.TEAM,MSGCLASS=Q,PRTY=8        !2
//*
//*=====================================================================
//*       BUILDS MET.D.B. STATISTICS MONITOR MODULE ("STATSMET")
//*       ------------------------------------------------------
//*     Makes load module STATSBUF for generating daily statistics
//*     for selected data types received as alphanumeric bulletins.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: statsmet.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 3$ $Date: 20/05/2008 09:40:29$
//*
//* $Log:
//*  3    Met_DB_Project 1.2         20/05/2008 09:40:29    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  2    Met_DB_Project 1.1         25/10/2007 10:18:38    Brian Barwell
//*       Pre-compiler step added to process subroutine STATSMET.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:54    Sheila Needham  
//* $
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !3
//*   names like 'name.F' rather than 'name.f', e.g. 'statsmet.F'.)  !3
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !3
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !3
//*
//FT06F001 DD SYSOUT=Q                                               !3
//FT10F001 DD DSN=MDB.UTILITY.SRCE(STATSMET),DISP=SHR                !3
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(1,1)),              !3
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
//STATSMET EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(27998),DC(*)',        !3
//         COND=(4,LT)                                               !3
//*
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !3
//           DD DSN=MDB.SOURCE(DT2HRS),DISP=SHR                      !2
//           DD DSN=MDB.SOURCE(HRS2DT),DISP=SHR
//           DD DSN=MDB.SOURCE(SORTCH),DISP=SHR
//           DD DSN=MDB.UTILITY.SRCE(STATSUB),DISP=SHR
/*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !3
//             DSN=MCC3.DBLOAD.Dyymmdd(STATSMET)                     !3
//*
//LKED.DATE    DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN   DD *
  INCLUDE DATE(ZPDATE)
/*
//
