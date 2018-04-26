//M12DBIND JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* Print index of MDB data set entry by entry (more detail than stats)
//* (12-byte index entries, i.e. only satellite data; set blocksize of
//*  23476 or 27998 in PARM and DSN on last line.)
//*
//* Dec 2000: PARM.GO added, CLPARM included
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIME.GO=(,3),PARM.GO='23476'
//SYSIN DD DSN=MDB.UTILITY.SRCE(MDBINDEX),DISP=SHR
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE PR(CLPARM)
//*             ****************
//*             ** Set DsName **
//GO.SAT120 DD DISP=SHR,LABEL=(,,,IN),DSN=MDB.??????
