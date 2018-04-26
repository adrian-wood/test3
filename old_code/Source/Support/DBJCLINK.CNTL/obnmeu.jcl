//T12SCOBN JOB (M12,SC,WDS0BF),SIMON.ROOMG08.X6955,
// PRTY=??,NOTIFY=T12SC,MSGCLASS=Q
// EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(29999),ICA'
//*
//* --------------------------------------------------------------------
//* JCL to build load module OBNMEU from scratch
//* --------------------------------------------------------------------
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(OBNMEU),DISP=SHR
//           DD DSN=MDB.SOURCE(DELSPCE),DISP=SHR
//           DD DSN=MDB.SOURCE(IVALUE),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(OBNMEU),DISP=SHR
//LKED.OBJMOD1 DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.OBJMOD2 DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE OBJMOD1(MDB)
  INCLUDE OBJMOD2(REXXVAR,ZPDATE)
/*
//
