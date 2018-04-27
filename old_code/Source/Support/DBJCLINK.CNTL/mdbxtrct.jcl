//T12DBRTB JOB (M12,DB,WDS0BF),METDB.TEAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Build the MDBXTRCT module. Run build_mdbxtrct.sh first.
//*
//* $Workfile: mdbxtrct.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 3$ $Date: 09/03/2012 12:03:52$
//*
//* $Log:
//*  3    Met_DB_Project 1.2         09/03/2012 12:03:52    Sheila Needham
//*       Updated JCL
//*  2    Met_DB_Project 1.1         14/02/2012 09:38:40    Sheila Needham
//*       Updated for F90 build.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:51    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.Dyymmdd
//INLIB    DD  PATH='/u/os/t12sn/mdbxtrct/MDBXTRCT',
//         PATHOPTS=(ORDONLY)
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      INCLUDE INLIB
      NAME MDBXTRCT(R)
/*
