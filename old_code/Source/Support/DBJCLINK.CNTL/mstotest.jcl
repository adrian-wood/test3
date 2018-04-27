//T12DBRTD JOB (M12,DB,WDS0BF),METDB,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the MDBSTOR load module built under USS to build an
//* integration test version.
//* Check:-
//* SYSLMOD points to the output library
//* INLIB points to the partial build on USS to be tested
//*
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.Dxxxxxx
//INLIB    DD  PATH='/usr/local/mdb/mdb_refresh/MDBSTOR',
//         PATHOPTS=(ORDONLY)
//OBJ      DD DSN=MCC3.DB.OBJ,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      INCLUDE INLIB
      INCLUDE OBJ(REPLYT,TELLOPS)
      NAME MDBSTOR(R)
/*
//* For Info: the CHANGE statement allows unresolved F90 references
//*  to be resolved by a system module
//* TELLOPS and REPLYT are the "silent" F90 versions
//
