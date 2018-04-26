//T12DBRTA JOB (M12,DB,WDS0BF),NEEDHAM,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the MDBSTOR load module built under USS to resolve
//* external references.
//*
//* Check the SYSLMOD step for output load libary.
//* Check the INLIB step for source of partial build.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: mdbstor.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 11$ $Date: 30/01/2012 10:05:49$
//*
//* $Log:
//*  11   Met_DB_Project 1.10        30/01/2012 10:05:49    Sheila Needham
//*       Updated following refresh
//*  10   Met_DB_Project 1.9         01/10/2010 16:44:08    Brian Barwell
//*       MDB.SOURCE(ABC) removed from build job.
//*  9    Met_DB_Project 1.8         20/05/2008 09:37:49    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  8    Met_DB_Project 1.7         22/04/2008 14:34:52    Brian Barwell
//*       Delete JCL for SFLOC routine. Add JCL for MOBSYN routine. Move JCL
//*       for CCCODE and SYNEXP into pre-processing section and move SYNBUL
//*       out.
//*  7    Met_DB_Project 1.6         25/10/2007 10:16:08    Brian Barwell
//*       Subroutines PLAINOB, TOVIND, TOVREP, TOVSEQ and TRNPOSEI removed.
//*       JCL for HRRBUL commented out.
//*  6    Met_DB_Project 1.5         01/05/2007 17:16:06    Brian Barwell
//*       SYNBUL added to pre-processor step.
//*  5    Met_DB_Project 1.4         10/07/2006 15:34:24    Brian Barwell   Add
//*        modified SYNOB to pre-processing step.
//*  4    Met_DB_Project 1.3         06/06/2006 10:13:18    Sheila Needham
//*       Pre-compile AIRBUL
//*  3    Met_DB_Project 1.2         10/05/2006 11:10:15    Sheila Needham
//*       Remove references to development libraries
//*  2    Met_DB_Project 1.1         04/05/2006 09:30:35    Sheila Needham
//*       Change JCL to include pre-processing for member synob.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:50    Sheila Needham  
//* $
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.D111017
//INLIB    DD  PATH='/PROD/u/os/t12db/dbrevsrc/MDBSTOR',
//         PATHOPTS=(ORDONLY)
//ASM      DD DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  tellops_(TELLOPS)
      CHANGE  replyt_(REPLYT)
      INCLUDE INLIB
      INCLUDE ASM(TELLOPS,REPLYT)
      NAME MDBSTOR(R)
/*
