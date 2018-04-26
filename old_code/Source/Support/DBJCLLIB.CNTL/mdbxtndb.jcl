//M12DBEXT JOB (M12,DB,WDS0BF),MDB.TEAM,MSGCLASS=Q,PRTY=8
//*
//*---------------------------------------------------------------------
//*                 MCC3.DBJCLLIB.CNTL(MDBXTNDB)
//*                 ============================
//*
//*  Copies BUFR data stored with BUFREP from one storage data set to
//*  another. New data set must be pre-created using MDBINITB.
//*
//*  Before submitting,
//*    - select whether datasets are on MVS or ZFS,
//*    - insert data set names in 'OLD' & 'NEW' DD statements,
//*    - add record lengths of old and new datasets at INPUT namelist,
//*    - check job card details.
//*---------------------------------------------------------------------
//*
//EXTEND EXEC PGM=MDBXTNDB
//STEPLIB DD DSN=MCC3.DB.LOAD,DISP=SHR
//CODEFIG DD DSN=SDB.BUFR.CODEFIGS,LABEL=(,,,IN),DISP=SHR
//TABLEB  DD DSN=SDB.BUFR.TABLEB,LABEL=(,,,IN),DISP=SHR
//TABLED  DD DSN=SDB.BUFR.TABLED,LABEL=(,,,IN),DISP=SHR
//*
//INPUT DD *
 &INPUT  LENREC1=27998, LENREC2=27998 /
/*
//* -----------------------------------------------------------
//* For MVS datasets:                     **********************
//*                                       ** SET ORIGINAL DSN **
//* OLD     DD DISP=SHR,LABEL=(,,,IN),DSN=MDB.AATSR
//* NEW     DD DISP=SHR,DSN=MDB.AATSR.NEW
//*                         ** SET NEW DSN **
//*                         *****************
//* -----------------------------------------------------------
//* For ZFS datasets:         ***************************
//*                           ** SET ORIGINAL DSN PATH **
//* OLD     DD PATH='/BPRD/var/mdb/data/temp/MDB.AIRSWF',
//*            PATHOPTS=(ORDONLY)
//* NEW     DD PATH='/BPRD/var/mdb/data/AIRSWF/MDB.AIRSWF',
//*            PATHOPTS=(ORDWR)     ** SET NEW DSN PATH **
//*                               **********************
//* -----------------------------------------------------------
//
