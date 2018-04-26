//T12DBU JOB (OP1,SC,WDS0BF),METDB,                                     00001003
// PRTY=8,NOTIFY=T12DB,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=8M,CLASS=L      00002003
//*                                                                     00003000
//* MDB test RPC broker server - IBM BPRD                               00004003
//*                                                                     00005000
//MDBROKER EXEC PGM=BPXBATCH,REGION=8M,                                 00006000
//  DYNAMNBR=50,TIME=9,COND=EVEN,                                       00007000
//  PARM='SH /usr/local/mdb/test/bin/broker_test'                       00008102
//SYSTSIN  DD DUMMY                                                     00009000
//STDIN    DD DUMMY                                                     00010000
//STDOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00020000
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH)                       00030000
//STDERR   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00040000
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH)                       00050000
//                                                                      00060000
//T12DBV JOB (OP1,SC,WDS0BF),METDB,                                     00070003
// PRTY=8,NOTIFY=T12DB,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=8M,CLASS=L      00080003
//*                                                                     00080100
//* MDB test RPC freepn server - IBM BPRD                               00080203
//*                                                                     00080300
//MDBROKER EXEC PGM=BPXBATCH,REGION=8M,                                 00080400
//  DYNAMNBR=50,TIME=9,COND=EVEN,                                       00080500
//  PARM='SH /usr/local/mdb/test/bin/freepn_test'                       00080602
//SYSTSIN  DD DUMMY                                                     00080700
//STDIN    DD DUMMY                                                     00080800
//STDOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00080900
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH)                       00081000
//STDERR   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00082000
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH)                       00083000
//                                                                      00084000
