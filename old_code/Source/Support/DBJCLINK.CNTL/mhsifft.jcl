//T12SNCMP JOB (M12,SN,WDS0BF),SHEILA.NEEDHAM,MSGCLASS=Q,               00001000
//  PRTY=8,MSGLEVEL=(1,1),REGION=220M                                   00002000
//*=====================================================================00002100
//*        BUILDS MHSIFFT                                               00002200
//*        ---------------                                              00002300
//*   Local version of MHS handling routine for processing a fixed      00002400
//*   list of datasets without renaming.                                00002500
//*   Check the SYSLIN and SYSLMOD DD in both steps.                    00002600
//*                                                                     00002700
//* REVISION INFO:                                                      00002800
//* --------------                                                      00002900
//* $Workfile: mhsifft.jcl$ $Folder: DBJCLINK.CNTL$                                             00003000
//* $Revision: 1$ $Date: 27/09/2011 11:03:53$                                              00003100
//*                                                                     00003200
//* $Log:
//*  1    Met_DB_Project 1.0         27/09/2011 11:03:53    Sheila Needham  New
//*        build JCL for local C version of MHSIFFC routine
//* $                                                             00003300
//*                                                                     00003400
//COMPILE    EXEC  PGM=CCNDRVR,REGION=220M,                             00003800
// PARM='/SEARCH(''CEE.SCEEH.+'') NOOPT SO OBJ'                         00003900
//STEPLIB  DD  DSN=SYS1.SCEERUN,DISP=SHR                                00004000
//         DD  DSN=SYS1.SCEERUN2,DISP=SHR                               00004100
//         DD  DSN=SYS1.SCCNCMP,DISP=SHR                                00004200
//SYSIN    DD  DSNAME=MDB.SOURCE(MHSIFFT),DISP=SHR                      00004300
//SYSLIB   DD  DSNAME=SYS1.SCEEH.H,DISP=SHR                             00004400
//         DD  DSNAME=SYS1.SCEEH.SYS.H,DISP=SHR                         00004500
//SYSLIN   DD  DSNAME=MCC3.SNLIB.OBJ(MHSIFFT),UNIT=DISK,                00005000
//             DISP=SHR                                                 00006000
//SYSPRINT DD  SYSOUT=*                                                 00007000
//SYSOUT   DD  SYSOUT=*                                                 00008000
//SYSCPRT  DD  SYSOUT=*                                                 00009000
//* COMMENT:                                                            00010000
//*                                                                     00020000
//BIND   EXEC PGM=IEWL,REGION=220M,                                     00030000
//   PARM='AMODE=31,MAP,RENT,CASE=MIXED,COMPAT=CURR'                    00041000
//SYSLIB   DD  DSN=SYS1.SCEELKEX,DISP=SHR                               00060000
//         DD  DSN=SYS1.SCEELKED,DISP=SHR                               00070000
//         DD  DSN=SYS1.LINKLIB,DISP=SHR                                00071000
//SYSPRINT DD  SYSOUT=*                                                 00080000
//SYSLIN   DD DISP=SHR,DSN=MCC3.SNLIB.OBJ(MHSIFFT)                      00090000
//         DD  DDNAME=SYSIN                                             00100000
//SYSLMOD  DD DISP=SHR,DSN=MCC3.DB.LOAD(MHSIFFT)                        00110000
//SYSDEFSD DD  DUMMY                                                    00120000
//SYSIN    DD  DUMMY                                                    00130000
