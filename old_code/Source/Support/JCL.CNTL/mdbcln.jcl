//MDBCLN   JOB (OP1,DB,TCDB0B),METDBTEAM.6953,CLASS=N,MSGCLASS=Q,       00010007
//  MSGLEVEL=(1,1),NOTIFY=T12DB,USER=T12DB                              00020007
//*-------------------------------------------------------------------- 00030000
//*                                                                     00040000
//* JOB     : MDBCLN                                                    00050001
//* PURPOSE : CLEANUP OF SGMDBPRK, THE METDB DATAPARK.                  00060001
//*           THIS JOB IS ONLY TO BE RUN IF THE NORMAL DISK SPACE       00070000
//*           MANAGEMENT DOESN'T FREE UP SUFFICIENT SPACE.              00080000
//* CREATED : 29 OCT 1998, CHRIS LOVE                                   00090000
//*                                                                     00100000
//*-------------------------------------------------------------------- 00110000
//*                                                                     00120000
//*-------------------------------------------------------------------- 00121303
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 5 DAYS AGO AND   00121406
//* LARGER THAN 100 CYLINDERS IN SIZE                                   00121503
//*-------------------------------------------------------------------- 00121603
//ADAYS5S   EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00130006
//SYSPRINT  DD   SYSOUT=*                                               00140000
//SYSPRIN1  DD   SYSOUT=*                                               00150000
//SYSUDUMP  DD   SYSOUT=*                                               00160000
//ABRMAP    DD   SYSOUT=*                                               00170000
//TAPE1     DD   DUMMY                                                  00180000
//SYSIN     DD   *                                                      00190000
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,                      00200001
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00430001
  MOUNT STORGRP=SGMDBPRK                                                00440001
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=5,SIZE=1500                         00450005
/*                                                                      00450101
//*                                                                     00451000
//*-------------------------------------------------------------------- 00451103
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 4 DAYS AGO       00451206
//*-------------------------------------------------------------------- 00451403
//ADAYS4    EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00452006
//SYSPRINT  DD   SYSOUT=*                                               00453000
//SYSPRIN1  DD   SYSOUT=*                                               00454000
//SYSUDUMP  DD   SYSOUT=*                                               00455000
//ABRMAP    DD   SYSOUT=*                                               00456000
//TAPE1     DD   DUMMY                                                  00457000
//SYSIN     DD   *                                                      00458000
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00460002
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00461001
  MOUNT STORGRP=SGMDBPRK                                                00462001
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=4                                   00463006
/*                                                                      00469600
//*-------------------------------------------------------------------- 00469704
//* DELETE ALL DATASETS ON SGMDBPRK LAST ACCESSED OVER 1 DAY AGO AND    00469804
//* OVER 100 CYLINDERS IN SIZE                                          00469904
//*-------------------------------------------------------------------- 00470004
//ADAYS1S   EXEC PGM=FDRABR,TIME=8,REGION=4000K                         00470104
//SYSPRINT  DD   SYSOUT=*                                               00470204
//SYSPRIN1  DD   SYSOUT=*                                               00470304
//SYSUDUMP  DD   SYSOUT=*                                               00470404
//ABRMAP    DD   SYSOUT=*                                               00470504
//TAPE1     DD   DUMMY                                                  00470604
//SYSIN     DD   *                                                      00470704
  DUMP TYPE=SCR,DSNENQ=USE,SCRATCH,ICFCORE=260000,THRESHOLD=80,         00470804
  SELTERR=NO,EXPIRED,MAXCARDS=5000                                      00470904
  MOUNT STORGRP=SGMDBPRK                                                00471004
  SELECT CATDSN=PUBLIC.MDB.**,ADAYS=1,SIZE=1500                         00471104
/*                                                                      00472004
//                                                                      00480000
