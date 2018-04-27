//M12DBLOK JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* PRINT A BLOCK OF AN MDB DATA SET (23-BYTE INDEX, CHAINED, TRAILERS)
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIMEG=(0,3),GOREGN=6M,
//  PARM.GO='???,27998/' Block (numbered from start of d/s), blocksize
//*          *** *****   *********************************************
//*
//SYSIN DD DSN=MDB.UTILITY.SRCE(MDBBLOCK),DISP=SHR
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
//LKED.CL DD DSN=MDB.LOADLIB,DISP=SHR
 INCLUDE PR(CLPARM,ZPDATE,LPL82)
 INCLUDE CL(MDBSTOR)
/*
//*               ****************
//*               ** Set DsName **
//GO.FT01F001 DD DISP=SHR,LABEL=(,,,IN),DSN=MDB.??????
