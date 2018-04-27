//T12DBSSH JOB (M12,DB,WDS0BF),SHEILA,PRTY=8,MSGCLASS=Q,
// MSGLEVEL=(1,1)
//*
//* Re-link the SALTSSH load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: saltssh.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 3$ $Date: 08/06/2012 17:53:29$
//*
//* $Log:
//*  3    Met_DB_Project 1.2         08/06/2012 17:53:29    Sheila Needham  JCL
//*        to complete the F95 build
//*  2    Met_DB_Project 1.1         19/12/2006 15:14:06    Stan Kellett
//*       removed assembler routine SECSOUT as not used anymore.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:53    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED,DYNAM(DLL)'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D*****
//INLIB    DD  PATH='/u/os/t12db/dbopsrce/SALTSSH',
//         PATHOPTS=(ORDONLY)
//SIDE     DD  PATH='/u/os/t12db/dbopsrce/BUFRDLL.x',
//         PATHOPTS=(ORDONLY)
//MHS      DD  DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  mhsiffc_(MHSIFFC)
      INCLUDE INLIB
      INCLUDE SIDE
      INCLUDE MHS(MHSIFFC)
      NAME SALTSSH(R)
/*
