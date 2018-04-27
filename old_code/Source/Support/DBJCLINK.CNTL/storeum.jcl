//T12DBRTC JOB (M12,DB,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the STOREUM load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: storeum.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 4$ $Date: 31/01/2012 08:22:49$
//*
//* $Log:
//*  4    Met_DB_Project 1.3         31/01/2012 08:22:49    Sheila Needham  Fix
//*        typo.
//*  3    Met_DB_Project 1.2         30/01/2012 10:08:40    Sheila Needham
//*       Updated following refresh
//*  2    Met_DB_Project 1.1         08/10/2008 09:58:52    Sheila Needham
//*       Added subroutines to handle ARGO data
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:56    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D111121
//INLIB    DD  PATH='/PROD/u/os/t12db/dbrevsrc/STOREUM',
//         PATHOPTS=(ORDONLY)
//ASM      DD  DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  mhsiffc_(MHSIFFC)
      CHANGE  secsout_(SECSOUT)
      CHANGE  replyt_(REPLYT)
      CHANGE  tellops_(TELLOPS)
      INCLUDE INLIB
      INCLUDE ASM(MHSIFFC,SECSOUT,REPLYT,TELLOPS)
      NAME STOREUM(R)
/*
