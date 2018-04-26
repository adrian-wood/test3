//T12DBCRX JOB (OP1,DB,WDS0BF),MDBTEAM.6675,PRTY=8,MSGCLASS=Q
//*
//*=====================================================================
//*
//* TITLE        : CREX
//*
//* PURPOSE      : To build a CREX load module (decode only).
//*                TABLED includes sequences given under both BUFR and
//*                CREX in the MANUAL on Codes. TABLEB has CREX columns
//*                (read by CREXELM), but the BUFR program is called if
//*                a flag table is displayed.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: crex.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 2$ $Date: 20/05/2008 09:36:04$
//*
//* $Log:
//*  2    Met_DB_Project 1.1         20/05/2008 09:36:04    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:45    Sheila Needham  
//* $
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !2
//*   names like 'name.F' rather than 'name.f', e.g. 'code.F'.)      !2
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !2
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !2
//*
//FT06F001 DD SYSOUT=Q                                               !2
//FT10F001 DD DSN=MDB.SOURCE(CODE),DISP=SHR                          !2
//         DD DSN=MDB.SOURCE(TABLEB),DISP=SHR                        !2
//         DD DSN=MDB.SOURCE(TABLED),DISP=SHR                        !2
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(3,1)),              !2
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
//CREX EXEC FORT2CL,FPARMS='MAP,CHARLEN(27998),NOFIPS,DC(*)',        !2
//     LMAP=MAP,LXREF=XREF,REGION=3000K,OUTPUT=Q,COND=(4,LT)         !2
//*
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !2
//           DD DSN=MDB.SOURCE(CREX),DISP=SHR     start of message
//           DD DSN=MDB.SOURCE(CREXDAT),DISP=SHR  data section
//           DD DSN=MDB.SOURCE(CREXELM),DISP=SHR  look up Table B
//           DD DSN=MDB.SOURCE(CREXVAL),DISP=SHR  return a value
//           DD DSN=MDB.SOURCE(DESFXY),DISP=SHR      for Table D seqs
//           DD DSN=MDB.SOURCE(READCF),DISP=SHR      only for display
//           DD DSN=MDB.SOURCE(VALUE),DISP=SHR       used by CODE
//           DD DSN=MDB.SOURCE(IVALUE),DISP=SHR      used by CREXVAL
//           DD DSN=MDB.SOURCE(PRINDT),DISP=SHR      for error message
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !2
//             DSN=MCC3.DBLOAD.Dyymmdd(CREX)                         !2
//LKED.SYSIN   DD *
 ENTRY CREX
/*
//
