//M12SNSOB JOB (OP1,DB,WDS0BF),SHEILA.NEEDHAM,MSGCLASS=Q,PRTY=??
//*
//*=====================================================================
//*       BUILDS MET.D.B. STATISTICS MONITOR MODULE ("STATSBUF")
//*       ------------------------------------------------------
//*     Makes load module STATSBUF for generating daily statistics
//*     for selected data types received as alphanumeric bulletins.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: statsbuf.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 2$ $Date: 06/01/2009 14:05:42$
//*
//* $Log:
//*  2    Met_DB_Project 1.1         06/01/2009 14:05:42    Sheila Needham
//*       Added pre-processing step
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:54    Sheila Needham  
//* $
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS
//*         -----------------------------------------------
//*  (source in the FT10F001 dd statement is held in starteam with
//*   names like 'name.F' rather than 'name.f', e.g. 'statsbuf.F'.)
//*=====================================================================
//PREPRO EXEC PGM=PREPRO
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//*
//FT06F001 DD SYSOUT=Q
//FT10F001 DD DSN=MDB.UTILITY.SRCE(STATSBUF),DISP=SHR
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(1,1)),
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
//*
//STATSBUF EXEC FORT2CL,LPARMS='XREF,MAP',
//  FPARMS='NOFIPS,CHARLEN(27998),XREF,MAP,DC(*)',
//      COND=(4,LT)
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)
//           DD DSN=MDB.SOURCE(HRS2DT),DISP=SHR
//           DD DSN=MDB.SOURCE(DT2HRS),DISP=SHR
/*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D######(STATSBUF),DISP=SHR,
//  SPACE=,UNIT=DISK
//LKED.DATE    DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN   DD *
  INCLUDE DATE(ZPDATE)
/*
//
