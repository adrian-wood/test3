//MDBCLNX  JOB (OP1,DB,WDS0BF),METDBTEAM.6953,CLASS=N,MSGCLASS=Q,       00010007
//  MSGLEVEL=(1,1),NOTIFY=T12DB,USER=T12DB                              00020007
//*-------------------------------------------------------------------- 00030000
//*                                                                     00040000
//* JOB     : MDBCLNX                                                   00050001
//* PURPOSE : CLEANUP OF SGMDBPRK, THE METDB DATAPARK.                  00060000
//*           THIS JOB IS ONLY TO BE RUN IF THE NORMAL DISK SPACE       00070000
//*           MANAGEMENT DOESN'T FREE UP SUFFICIENT SPACE.              00080000
//* CREATED : 29 OCT 1998, CHRIS LOVE                                   00090000
//*                                                                     00100000
//*-------------------------------------------------------------------- 00110000
//*                                                                     00120000
//*                                                                     00451000
//*-------------------------------------------------------------------- 00451200
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 4 DAY AGO IF THE 00451306
//* DISK IS STILL OVER 80% FULL                                         00451402
//*-------------------------------------------------------------------- 00451500
//ADAYS4    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00452006
//SYSPRINT  DD   SYSOUT=*                                               00453000
//SYSPRIN1  DD   SYSOUT=*                                               00454000
//SYSUDUMP  DD   SYSOUT=*                                               00455000
//ABRMAP    DD   SYSOUT=*                                               00456000
//TAPE1     DD   DUMMY                                                  00457000
//SYSIN     DD   *                                                      00458000
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00460001
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00461000
  MOUNT STORGRP=SGMDBPRK                                                00462000
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=4                                   00463006
/*                                                                      00469600
//*                                                                     00479605
//*-------------------------------------------------------------------- 00479705
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 3 DAY AGO IF THE 00479806
//* DISK IS STILL OVER 80% FULL                                         00479906
//*-------------------------------------------------------------------- 00480005
//ADAYS3    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00480105
//SYSPRINT  DD   SYSOUT=*                                               00480205
//SYSPRIN1  DD   SYSOUT=*                                               00480305
//SYSUDUMP  DD   SYSOUT=*                                               00480405
//ABRMAP    DD   SYSOUT=*                                               00480505
//TAPE1     DD   DUMMY                                                  00480605
//SYSIN     DD   *                                                      00480705
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00480805
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00480905
  MOUNT STORGRP=SGMDBPRK                                                00481005
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=3                                   00481105
/*                                                                      00481205
//*                                                                     00481306
//*-------------------------------------------------------------------- 00481406
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 2 DAYS AGO IF    00481506
//* THE DISK IS STILL OVER 80% FULL                                     00481606
//*-------------------------------------------------------------------- 00481706
//ADAYS2    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00481806
//SYSPRINT  DD   SYSOUT=*                                               00481906
//SYSPRIN1  DD   SYSOUT=*                                               00482006
//SYSUDUMP  DD   SYSOUT=*                                               00482106
//ABRMAP    DD   SYSOUT=*                                               00482206
//TAPE1     DD   DUMMY                                                  00482306
//SYSIN     DD   *                                                      00482406
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00482506
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00482606
  MOUNT STORGRP=SGMDBPRK                                                00482706
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=2                                   00482806
/*                                                                      00482906
//*                                                                     00483006
//*-------------------------------------------------------------------- 00483106
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 1 DAY AGO IF     00483206
//* THE DISK IS STILL OVER 80% FULL                                     00483306
//*-------------------------------------------------------------------- 00483406
//ADAYS1    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00483506
//SYSPRINT  DD   SYSOUT=*                                               00483606
//SYSPRIN1  DD   SYSOUT=*                                               00483706
//SYSUDUMP  DD   SYSOUT=*                                               00483806
//ABRMAP    DD   SYSOUT=*                                               00484004
//TAPE1     DD   DUMMY                                                  00485004
//SYSIN     DD   *                                                      00486004
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00487004
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00488004
  MOUNT STORGRP=SGMDBPRK                                                00489004
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=1                                   00489105
/*                                                                      00489204
//*                                                                     00489306
//*-------------------------------------------------------------------- 00489406
//* DELETE ALL DATASETS ON SGMDBPRK LARGER THAN 100 CYLINDERS IN SIZE   00489506
//* IF THE THE DISK IS STILL OVER 80% FULL                              00489606
//*-------------------------------------------------------------------- 00489706
//ADAYS1    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00489806
//SYSPRINT  DD   SYSOUT=*                                               00489906
//SYSPRIN1  DD   SYSOUT=*                                               00490006
//SYSUDUMP  DD   SYSOUT=*                                               00490106
//ABRMAP    DD   SYSOUT=*                                               00491006
//TAPE1     DD   DUMMY                                                  00492006
//SYSIN     DD   *                                                      00493006
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00494006
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00495006
  MOUNT STORGRP=SGMDBPRK                                                00496006
  SELECT CATDSN=PUBLIC.MDB.**,SIZE=1500                                 00497006
/*                                                                      00498006
//                                                                      00500006
