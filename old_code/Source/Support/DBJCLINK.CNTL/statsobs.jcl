//M12SNSOB JOB (OP1,DB,WDS0BF),SHEILA.NEEDHAM,MSGCLASS=Q,PRTY=??
//*
//*---------------------------------------------------------------------
//*                 MCC3.DBJCLINK.CNTL(STATSOBS)
//*                 ----------------------------
//*       JOB TO BUILD LOAD MODULE FOR "MDB.LOADLIB(STATSOBS)"
//*
//* REVISION INFO :
//*
//* $Revision: 2$
//* $Date: 06/01/2009 14:10:27$
//*---------------------------------------------------------------------
//*
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !2
//*   names like 'name.F' rather than 'name.f', e.g. 'statsmet.F'.)  !2
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !2
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !2
//*
//FT06F001 DD SYSOUT=Q                                               !2
//FT10F001 DD DSN=MDB.UTILITY.SRCE(STATSOBS),DISP=SHR                !2
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(1,1)),              !2
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//DYNALLOC EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(DYNALLOC),DISP=SHR
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
// EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(27998),OPT(3)',
//      COND=(4,LT)
//*                                                              Source
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)
//           DD DSN=MDB.SOURCE(DSINFO),DISP=SHR
//           DD DSN=MDB.SOURCE(DSOPEN),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR3),DISP=SHR
//           DD DSN=MDB.SOURCE(SATYPE),DISP=SHR
//*                                                         Load module
//LKED.SYSLMOD DD UNIT=,SPACE=,DCB=,DISP=SHR,
//             DSN=MCC3.DBLOAD.D######(STATSOBS)
//*
//LKED.PROGLIB DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE PROGLIB(ZPDATE)
/*
//
