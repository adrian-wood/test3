//T12DBRTD JOB (M12,DB,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the MONITOR load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: monitor.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 3$ $Date: 30/01/2012 10:07:33$
//*
//* $Log:
//*  3    Met_DB_Project 1.2         30/01/2012 10:07:33    Sheila Needham
//*       Updated following refresh
//*  2    Met_DB_Project 1.1         20/05/2008 09:39:23    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:52    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D******
//INLIB    DD  PATH='/PROD/u/os/t12db/dbrevsrc/MONITOR',
//         PATHOPTS=(ORDONLY)
//ASM      DD  DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  mhsiffc_(MHSIFFC)
      CHANGE  secsout_(SECSOUT)
      CHANGE  tellops_(TELLOPS)
      CHANGE  replytu_(REPLYTU)
      INCLUDE INLIB
      INCLUDE ASM(MHSIFFC,SECSOUT,TELLOPS,REPLYTU)
      NAME MONITOR(R)
/*
