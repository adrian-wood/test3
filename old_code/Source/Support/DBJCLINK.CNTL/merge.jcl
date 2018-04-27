//T12SNRTM JOB (M12,SN,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the MERGE load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: merge.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 6$ $Date: 30/01/2012 10:06:38$
//*
//* $Log:
//*  6    Met_DB_Project 1.5         30/01/2012 10:06:38    Sheila Needham
//*       Updated following refresh
//*  5    Met_DB_Project 1.4         29/03/2010 16:58:11    Brian Barwell
//*       Change SYS1.VSF6COMP to SYS1.VSF2COMP.
//*  4    Met_DB_Project 1.3         20/05/2008 09:38:56    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  3    Met_DB_Project 1.2         01/05/2007 17:18:49    Brian Barwell
//*       Correction to error in location of MERGE source (introduced by the
//*       last change).
//*  2    Met_DB_Project 1.1         23/02/2007 15:51:29    Brian Barwell
//*       Pre-processor step added for MERGE routine.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:51    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED,DYNAM(DLL)'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D******
//INLIB    DD  PATH='/u/os/t12db/dbrevsrc/MERGE',
//         PATHOPTS=(ORDONLY)
//SIDE     DD  PATH='/u/os/t12db/dbrevsrc/MDBDLL.x',
//         PATHOPTS=(ORDONLY)
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      INCLUDE INLIB
      INCLUDE SIDE
      NAME MERGE(R)
/*
