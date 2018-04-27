//T12DBRTE JOB (M12,DB,WDS0BF),BARWELL,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the STORAMD load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: STAMDBD.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 2$ $Date: 04/03/2013 18:17:17$
//*
//* $Log:
//*  2    Met_DB_Project 1.1         04/03/2013 18:17:17    Sheila Needham
//*       Corrected module name
//*  1    Met_DB_Project 1.0         13/02/2013 17:06:35    Brian Barwell   JCL
//*        to re-link load modules and resolve external references.
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D******
//INLIB    DD  PATH='/PROD/u/os/t12db/dbrevsrc/STORAMD',
//         PATHOPTS=(ORDONLY)
//ASM      DD DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  tellops_(TELLOPS)
      CHANGE  replyt_(REPLYT)
      INCLUDE INLIB
      INCLUDE ASM(TELLOPS,REPLYT)
      NAME STORAMD(R)
/*
