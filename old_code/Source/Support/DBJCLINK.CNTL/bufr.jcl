//T12DBUFR JOB (OP1,DB,WDS0BF),MDBTEAM.6675,PRTY=8,MSGCLASS=Q
//*
//*=====================================================================
//*
//* TITLE        : BUFR
//*
//* PURPOSE      : To build BUFR module completely from source for
//*                later use via BUFRSHEL in MDB applications.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: bufr.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 5$ $Date: 20/05/2008 09:35:02$
//*
//* $Log:
//*  5    Met_DB_Project 1.4         20/05/2008 09:35:02    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  4    Met_DB_Project 1.3         25/10/2007 10:13:24    Brian Barwell   JCL
//*        for DBUFRI, DECODI, ENCODI and NBUFRI deleted. Dummy routines added
//*        to avoid changing BUFRSHEL.
//*  3    Met_DB_Project 1.2         25/08/2006 12:03:48    Alison Weir
//*       ENCODE added to preprocessing step. Also COND= added.
//*  2    Met_DB_Project 1.1         30/06/2006 10:32:06    Stan Kellett
//*       Needed to change BUFR build job to allow for preprocessing on GPCS
//*       system
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:44    Sheila Needham  
//* $
//*=====================================================================
//*                      ASSEMBLER ROUTINES
//*=====================================================================
//BUFR EXEC ASMAC                                                    !5
//ASM.SYSIN DD DSN=MDB.SOURCE(BUFR),DISP=SHR
//*
//ASMVALUE EXEC ASMAC                                                !5
//ASM.SYSIN DD DSN=MDB.SOURCE(ASMVALUE),DISP=SHR
//*
//ASMVALOU EXEC ASMAC                                                !5
//ASM.SYSIN DD DSN=MDB.SOURCE(ASMVALOU),DISP=SHR
//*
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !5
//*   names like 'name.F' rather than 'name.f', e.g. 'bufdata.F'.)   !5
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !5
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !5
//*
//FT06F001 DD SYSOUT=Q                                               !5
//FT10F001 DD DSN=MDB.SOURCE(BUFDATA),DISP=SHR                       !5
//         DD DSN=MDB.SOURCE(BUFDCHR),DISP=SHR                       !5
//         DD DSN=MDB.SOURCE(CODE),DISP=SHR                          !5
//         DD DSN=MDB.SOURCE(ENBUFV2),DISP=SHR                       !5
//         DD DSN=MDB.SOURCE(ENCODE),DISP=SHR                        !3
//         DD DSN=MDB.SOURCE(LOCALD),DISP=SHR                        !5
//         DD DSN=MDB.SOURCE(TABLEB),DISP=SHR                        !5
//         DD DSN=MDB.SOURCE(TABLED),DISP=SHR                        !5
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(10,5)),             !5
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !5
//*
//*=====================================================================
//*                      FORTRAN COMPILATION
//*=====================================================================
// EXEC FORT2C,COND=(4,LT)  dummy routines to avoid changing shell   !5
      SUBROUTINE DBUFRI                                              !4
      RETURN                                                         !4
      END                                                            !4
      SUBROUTINE DECODI                                              !4
      RETURN                                                         !4
      END                                                            !4
      SUBROUTINE ENCODI                                              !4
      RETURN                                                         !4
      END                                                            !4
      SUBROUTINE LOCALB !(to resolve reference in subroutine BUFR)!1.10
      RETURN                                                      !1.10
      END                                                         !1.10
      SUBROUTINE NBUFRI                                              !4
      RETURN                                                         !4
      END                                                            !4
//*
//BUFR EXEC FORT2CL,FPARMS='MAP,CHARLEN(27998),NOFIPS,DC(*)',        !5
//        LMAP=MAP,LXREF=XREF,REGION=3000K,OUTPUT=Q,COND=(4,LT)      !5
//*
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !5
//           DD DSN=MDB.SOURCE(BUFDASS),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFDELT),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFDMAP),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFDRIP),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFDRPL),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFDSPL),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFD203),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFD206),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFR207),DISP=SHR                !1.6
//           DD DSN=MDB.SOURCE(BUFRQEL),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFRQOP),DISP=SHR                !1.4
//           DD DSN=MDB.SOURCE(BUFRSEQ),DISP=SHR                !1.9
//           DD DSN=MDB.SOURCE(BUFV2),DISP=SHR
//           DD DSN=MDB.SOURCE(DEBUFR),DISP=SHR
//           DD DSN=MDB.SOURCE(DECORD),DISP=SHR
//           DD DSN=MDB.SOURCE(DESFXY),DISP=SHR
//           DD DSN=MDB.SOURCE(ELMENT),DISP=SHR                 !1.3
//           DD DSN=MDB.SOURCE(ENBUFR),DISP=SHR
//           DD DSN=MDB.SOURCE(IDES),DISP=SHR
//           DD DSN=MDB.SOURCE(READCF),DISP=SHR                 !1.9
//           DD DSN=MDB.SOURCE(SCRIPT),DISP=SHR
//           DD DSN=MDB.SOURCE(SORTN),DISP=SHR
//* DD DSN=MDB.SOURCE(VALOUT),DISP=SHR  Fortran not operational!
//* DD DSN=MDB.SOURCE(VALUE),DISP=SHR   Fortran not operational!
//*
//           DD DSN=MET.SRCELIB(IBMISO8),DISP=SHR
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !5
//             DSN=MCC3.DBLOAD.Dyymmdd(BUFR)                         !5
//LKED.SYSIN   DD *
 ENTRY BUFR
/*
//
