//T12DBPAO JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*
//*=====================================================================
//*     BUILDS MET.D.B. STORAGE MODULE "PAOBDAT" FOR PAOBS DATA
//*     -------------------------------------------------------
//*    Makes load module PAOBDAT for storage in the MetDB of
//*   Australian bogus ("PAOBS") data from FROST via PAOB stream.
//*
//* REVISION INFO:
//* -------------
//* $Revision: 2$
//* $Date: 20/05/2008 09:39:56$
//*
//* $Log:
//*  2    Met_DB_Project 1.1         20/05/2008 09:39:56    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:53    Sheila Needham  
//* $
//*  18/03/03:  Include EBCDIC for TROPICS to FROST change.        !1.7
//*  30/03/00:  Changes for new storage routines (opnl. 19/6/00, B.R.B.)
//*
//*=====================================================================
//*                    ASSEMBLER ROUTINES
//*=====================================================================
//DYNALLOC EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(DYNALLOC),DISP=SHR
//*
//REPLYT EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(REPLYT),DISP=SHR
//*
//SECSOUT EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(SECSOUT),DISP=SHR
//*
//TELLOPS EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(TELLOPS),DISP=SHR
//*
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !2
//*   names like 'name.F' rather than 'name.f', e.g. 'nchtst.F'.)    !2
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !2
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !2
//*
//FT06F001 DD SYSOUT=Q                                               !2
//FT10F001 DD DSN=MDB.SOURCE(NCHTST),DISP=SHR                        !2
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(1,1)),              !2
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//*=====================================================================
//*              ASSEMBLER AND FORTRAN COMPILATION
//*=====================================================================
//PAOBDAT EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(28672),OPT(3),DC(*)',  !2
//        LPARMS='AC=1,RMODE=24',REGION=2000K,OUTPUT=Q,COND=(4,LT)   !2
//*
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !2
//           DD DSN=MDB.SOURCE(AIRSTO),DISP=SHR
//           DD DSN=MDB.SOURCE(BULLED),DISP=SHR
//           DD DSN=MDB.SOURCE(CENTURY),DISP=SHR
//           DD DSN=MDB.SOURCE(DREG),DISP=SHR
//           DD DSN=MDB.SOURCE(DSINFO),DISP=SHR
//           DD DSN=MDB.SOURCE(DSOPEN),DISP=SHR
//           DD DSN=MDB.SOURCE(EBCDIC),DISP=SHR                     1.7
//           DD DSN=MDB.SOURCE(FINDJSR),DISP=SHR
//           DD DSN=MDB.SOURCE(FINDMHS),DISP=SHR
//           DD DSN=MDB.SOURCE(INITSTOR),DISP=SHR
//           DD DSN=MDB.SOURCE(LATBOX),DISP=SHR
//           DD DSN=MDB.SOURCE(NEXTFILE),DISP=SHR
//           DD DSN=MDB.SOURCE(NEXTMSG),DISP=SHR
//           DD DSN=MDB.SOURCE(PAOBDAT),DISP=SHR
//           DD DSN=MDB.SOURCE(PAOBIND),DISP=SHR
//           DD DSN=MDB.SOURCE(PAOBUL),DISP=SHR
//           DD DSN=MDB.SOURCE(READ4K),DISP=SHR
//           DD DSN=MDB.SOURCE(SATYPE),DISP=SHR
//           DD DSN=MDB.SOURCE(SUMMARY),DISP=SHR
//*
//           DD DSN=MET.SRCELIB(IBMISO8),DISP=SHR
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !2
//             DSN=MCC3.DBLOAD.Dyymmdd(PAOBDAT)                      !2
//*
//LKED.METPROG DD DSN=MET.PROGLIB,DISP=SHR
//LKED.MHSFTP  DD DSN=MHS.FTP.LOAD,DISP=SHR
//LKED.SDBLOAD DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN   DD *
 INCLUDE METPROG(ZPDATE)
 INCLUDE MHSFTP(MHSIFF)
 INCLUDE SDBLOAD(BUFRSHEL)
 ENTRY PAOBDAT
/*
//
