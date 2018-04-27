//T12RLLCA JOB (M12,RL,WDS0BF),METDB.X6955,NOTIFY=T12RL,PRTY=8,         00000107
//  MSGCLASS=Q,TIME=(,10)                                               00000200
//*
//* --------------------------------------------------------------------
//* MetDB CARTSUB - build from scratch.
//*
//* --------------------------------------------------------------------
//*
// EXEC FORT2CL,
//      FPARMS='NOFIPS',FPARMS2='CHARLEN(27998),XREF,MAP,DC(*)',
//      LMAP=MAP
//*
//FORT.SYSIN DD DSN=MCC3.DBSRCLIB(CARTSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(SUBMIT),DISP=SHR
//           DD DSN=MCC3.DBSRCLIB(COLENG),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D310511(CARTSUB),DISP=SHR,SPACE=
//LKED.OBJMOD1 DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN   DD  *
  INCLUDE OBJMOD1(ZPDATE)
/*
//
