//T12DBMKL JOB (M12,DB,WDS0BF),METDB.TEAM,                              00000107
// PRTY=8,NOTIFY=T12DB,MSGCLASS=Q,TIME=1                                00000200
//*
//* --------------------------------------------------------------------
//* MetDB GARCSUB - build from scratch.
//*
//* $Revision: 1$ $Date: 25/08/2009 09:55:21$
//* --------------------------------------------------------------------
//*
// EXEC FORT2CL,
//      FPARMS='NOFIPS',FPARMS2='CHARLEN(27998),XREF,MAP,DC(*)',
//      LMAP=MAP
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(GARCSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(LENSTR),DISP=SHR
//           DD DSN=MDB.SOURCE(SUBMIT),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(GARCSUB),DISP=SHR,SPACE=
//
