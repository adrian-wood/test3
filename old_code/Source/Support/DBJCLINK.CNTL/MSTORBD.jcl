//T12SNRTA JOB (M12,SN,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the MDBSTOR load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* This job must be submitted from the TEST system.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: MSTORBD.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 1$ $Date: 22/03/2011 10:47:53$
//*
//* $Log:
//*  1    MetDB_Refresh 1.0         22/03/2011 10:47:53    Sheila Needham  JCL
//*       to re-link load modules and resolve external references.
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=TSTMDB.DBLOAD.DINTBLD
//INLIB    DD  PATH='/PROD/u/os/t12sn/port/mdbstor/MDBSTOR',
//         PATHOPTS=(ORDONLY)
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      INCLUDE INLIB
      NAME MDBSTOR(R)
/*
