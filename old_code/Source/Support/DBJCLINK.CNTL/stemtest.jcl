//T12DBRTD JOB (M12,DB,WDS0BF),METDB,PRTY=8,MSGCLASS=Q,TIME=(,5)
//* ------------------------------------------------------------------
//* Re-link the STOREUM load module built under USS to build an
//* integration test version.
//* Check:-
//* SYSLMOD points to the output library
//* INLIB points to the partial load module built under USS
//*
//* ------------------------------------------------------------------
//LINK     EXEC PGM=IEWBLINK,REGION=2M,
//         PARM='LET,CASE=MIXED'
//SYSLIB   DD DISP=SHR,DSN=SYS1.SAFHFORT
//         DD DISP=SHR,DSN=SYS1.SCEELKED
//SYSPRINT DD  SYSOUT=*
//SYSLMOD  DD  DISP=SHR,DSN=MCC3.DBLOAD.Dxxxxxx
//INLIB    DD  PATH='/usr/local/mdb/mdb_refresh/STOREUM',
//         PATHOPTS=(ORDONLY)
//OBJ      DD DSN=MCC3.DB.OBJ,DISP=SHR
//LOAD     DD DSN=MCC3.DB.LOAD,DISP=SHR
//SYSLIN   DD  *
      ENTRY CEESTART
      CHANGE  sysabn_(SYSABN)
      CHANGE  mhsiffc_(MHSIFFC)
      CHANGE  secsout_(SECSOUT)
      INCLUDE INLIB
      INCLUDE OBJ(TELLOPS,REPLYT)
      INCLUDE LOAD(MHSIFFT,SECSOUT)
      NAME STOREUM(R)
/*
//* For Info: the CHANGE statements allow unresolved F90 references
//*  to be resolved by system or MVS load modules
//* TELLOPS and REPLYTU are the "silent" F90 versions
//* MHSIFFT is the integration test version that generates a list of
//*  MHSP datasets on the first call, loops through without renaming
//*  and returns 0 datasets when they ahve all been processed.
//* SECSOUT is the assembler version because the F90 version needs
//*  POSIX(ON) which conflicts with MHSIFFT
//
