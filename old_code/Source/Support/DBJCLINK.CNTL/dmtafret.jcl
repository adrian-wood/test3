//T12DBMDB JOB (M12,DB,WDS0BF),METDB.TEAM.X6953,PRTY=8,MSGCLASS=Q
//*
//* --------------------------------------------------------------------
//* TO REMAKE DMTAFRET
//* --------------------------------------------------------------------
//*
//CLG EXEC FORT2CL,LPARMS='XREF,MAP',
//         FPARMS='NOFIPS',FPARMS2='CHARLEN(27998),XREF,MAP,DC(*)'
//*
//FORT.SYSIN DD DSN=MCC3.DBSRCLIB(DMTFRTSC),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.SCLOAD(DMTAFRET),DISP=SHR,SPACE=,UNIT=DISK
//*
//LKED.L1 DD DSN=MET.PROGLIB,DISP=SHR
//LKED.L2 DD DSN=SYS1.ISPF.LINKLIB,DISP=SHR
//LKED.L3 DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE L1(ZPDATE)
  INCLUDE L2(ISPLNK)
  INCLUDE L3(MDB)
/*
//