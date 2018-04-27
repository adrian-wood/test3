//T12DBREX JOB (OP1,DB,WDS0BF),MDB.6953,MSGCLASS=Q,                     00010003
//    MSGLEVEL=(1,1),NOTIFY=T12DB,CLASS=N                               00020003
//*                                                                     00030001
//*                                                                     00040001
//PERIOD1  EXEC PGM=IKJEFT01                                            00050001
//SYSEXEC  DD   DSN=MDB.REXX.EXEC,DISP=SHR                              00060002
//         DD   DSN=MET.EXECFLIB,DISP=SHR                               00070001
//SYSPRINT DD   SYSOUT=*                                                00080001
//SYSTSPRT DD   SYSOUT=*                                                00090001
//JOBDSN   DD   DSN=MCC3.DB.NWP.RESTORE,DISP=(,CATLG),SPACE=(TRK,5),    00100004
//         RECFM=FB,LRECL=80,BLKSIZE=27920,STORCLAS=SCUSER              00101001
//SYSTSIN  DD   *                                                       00110001
  %NWPRESTR 26/11/99 02/12/99                                           00120001
//*                                                                     00130001
//PERIOD2  EXEC PGM=IKJEFT01                                            00140001
//SYSEXEC  DD   DSN=MDB.REXX.EXEC,DISP=SHR                              00150002
//         DD   DSN=MET.EXECFLIB,DISP=SHR                               00160001
//SYSPRINT DD   SYSOUT=*                                                00170001
//SYSTSPRT DD   SYSOUT=*                                                00180001
//JOBDSN   DD   DSN=MCC3.DB.NWP.RESTORE1,DISP=(,CATLG),SPACE=(TRK,5),   00191004
//         RECFM=FB,LRECL=80,BLKSIZE=27920,STORCLAS=SCUSER              00192001
//SYSTSIN  DD   *                                                       00200001
  %NWPRESTR 05/07/00 11/07/00                                           00210001
//*                                                                     00220001
