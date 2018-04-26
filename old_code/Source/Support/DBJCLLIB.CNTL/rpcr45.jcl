//T12DBR JOB (OP1,DB,WDS0BF),METDB,                                     00001009
// CLASS=I,NOTIFY=T12DB,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=8M             00002009
//*                                                                     00040000
//* MDB test RPC broker server - IBM PROD                               00050009
//*                                                                     00060000
//MDBROKER EXEC PGM=BPXBATCH,REGION=8M,                                 00070300
//  DYNAMNBR=50,TIME=9,COND=EVEN,                                       00071003
//  PARM='SH /usr/local/mdb/test/bin/broker_test'                       00072000
//SYSTSIN  DD DUMMY                                                     00075000
//STDIN    DD DUMMY                                                     00076000
//SYSOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00077008
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00078008
//SYSTSPRT DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00078108
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00078208
//STDOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00078308
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00078408
//STDERR   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00079001
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00080008
//                                                                      00080100
//T12DBF JOB (OP1,DB,WDS0BF),METDB,                                     00080209
// CLASS=I,NOTIFY=T12DB,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=8M             00080309
//*                                                                     00080400
//* MDB test RPC freepn server - IBM PROD                               00080509
//*                                                                     00080600
//MDBROKER EXEC PGM=BPXBATCH,REGION=8M,                                 00080700
//  DYNAMNBR=50,TIME=9,COND=EVEN,                                       00080803
//  PARM='SH /usr/local/mdb/test/bin/freepn_test'                       00080900
//SYSTSIN  DD DUMMY                                                     00081000
//STDIN    DD DUMMY                                                     00082000
//SYSOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00083008
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00084008
//SYSTSPRT DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00084108
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00084208
//STDOUT   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00084308
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00084408
//STDERR   DD PATH='/tmp/mdbtst.stdout',PATHOPTS=(OWRONLY,OCREAT),      00085001
//  PATHMODE=(SIRUSR,SIWUSR,SIRGRP,SIWGRP,SIROTH,SIWOTH)                00086008
//                                                                      00087000
