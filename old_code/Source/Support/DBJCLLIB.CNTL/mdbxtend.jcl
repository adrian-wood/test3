//T12DBEXT JOB (M12,DB,WDS0BF),MDBTEAM.6953,PRTY=8,MSGCLASS=Q,
//     REGION=5M
//*---------------------------------------------------------------------
//* No general program exists for remaking an MDB data set with extra
//* index blocks. MDB.UTILITY.SRCE(MDBPRINT) stores data again calling
//* TAFREP (MODE='S') or AIRSTO (MODE='A')
//* Set 1st block & N(blocks) to 0, to copy whole dataset.
//*---------------------------------------------------------------------
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000),NOFIPS',TIMEG=(0,10),GOREGN=5M,
//  PARM.GO='??,??,27998,''A''/' ** 1st block,N(blocks),blocksize **
//*                             ***********************************
//SYSIN DD DSN=MDB.UTILITY.SRCE(MDBPRINT),DISP=SHR ! COPY TO CHANGE IT!
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
//LKED.DB DD DSN=MDB.LOADLIB,DISP=SHR
 INCLUDE PR(CLPARM,ZPDATE,LPL82)
 INCLUDE DB(MDBSTOR)
/*
//*      *************************
//*      ** Set original DsName **
//GO.FT01F001 DD DSN=MDB.???????,DISP=SHR,LABEL=(,,,IN)
//GO.FT02F001 DD DSN=MDB.???????.NEW,DISP=SHR
//*           ** Set New DsName **
//*           ********************
//
