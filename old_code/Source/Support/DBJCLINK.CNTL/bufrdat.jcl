//T12DBRTB JOB (M12,DB,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the BUFRDAT load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: bufrdat.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 2$ $Date: 30/01/2012 10:05:00$
//*
//* $Log:
//*  2    Met_DB_Project 1.1         30/01/2012 10:05:00    Sheila Needham
//*       Updated following refresh
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:44    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D******
//INLIB    DD  PATH='/u/os/t12db/dbrevsrc/BUFRDAT',
//         PATHOPTS=(ORDONLY)
//ASM      DD  DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  mhsiffc_(MHSIFFC)
      CHANGE  secsout_(SECSOUT)
      CHANGE  tellops_(TELLOPS)
      CHANGE  replyt_(REPLYT)
      INCLUDE INLIB
      INCLUDE ASM(MHSIFFC,SECSOUT,TELLOPS,REPLYT)
      NAME BUFRDAT(R)
/*
