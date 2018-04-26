//T12JHCLN JOB (M12,JH,WDS0BF),METDB.6953,CLASS=N,MSGCLASS=N,       *   00010007
//  MSGLEVEL=(1,1),TIME=20,PRTY=8,NOTIFY=&SYSUID                        00020009
//*-------------------------------------------------------------------- 00030000
//*                                                                     00040000
//* JOB     :                                                           00050002
//* PURPOSE : CLEANUP OF SGMDBPRK                                       00060001
//*           THIS JOB IS ONLY TO BE RUN IF THE NORMAL DISK SPACE       00070001
//*           MANAGEMENT DOESN'T FREE UP SUFFICIENT SPACE.              00080001
//* CREATED : 29 OCT 1998, CHRIS LOVE                                   00090000
//*                                                                     00100000
//*-------------------------------------------------------------------- 00110000
//*                                                                     00120000
//SCRCLM    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00130000
//SYSPRINT  DD   SYSOUT=*                                               00140000
//SYSPRIN1  DD   SYSOUT=*                                               00150000
//SYSUDUMP  DD   SYSOUT=*                                               00160000
//ABRMAP    DD   SYSOUT=*                                               00170000
//TAPE1     DD   DUMMY                                                  00180000
//SYSIN     DD   *                                                      00190000
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,                      00200010
  SELTERR=NO,EXPIRED,MAXCARDS=10000                                     00210012
  MOUNT STORGRP=SGMDBPRK                                                00220012
* CHOOSE ADAYS FOR LAST ACCESSED CRDAYS FOR CREATED
* SELECT CATDSN=PUBLIC.MDB.**,CRDAYS=1                                  00230012
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=2                                   00230012
//* SET  ADAYS TO SELECT DATA TO DELETE THAT HAS NOT BEEN USED FOR
//* ADAYS DAYS
/*
