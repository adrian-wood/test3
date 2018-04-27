//M12DBOBS JOB (M12,DB,WDS0BF),SDBTEAM.6953,PRTY=8,MSGCLASS=Q
//*
//* TO PRINT CHAINED OBS (SORTED BY CALL SIGN) IN A RANGE OF MDB INDEX
//* BLOCKS.  (SET INDEX BLOCK NUMBER (<100), DDHH OR 0 FOR ALL BLOCKS;
//* BLOCK NUMBER SIMPLY FROM START OF DATA SET.  MODE IS 'P' OR 'N',
//* 'P' MAYBE BETTER FOR TAFS & METARS, 'N' BETTER FOR SYNOPS & U/A.)
//*
// EXEC FORT2CLG,FPARMS='CHARLEN(28000)',TIMEG=(0,3),GOREGN=6M,
//  PARM.GO='????,1,27998,''N''/'  ** 1st block,N(blocks),blocksize **
//*          **** * *****          ** (or time) **********************
//*
//SYSIN DD DSN=MDB.UTILITY.SRCE(MDBPRINT),DISP=SHR
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
//LKED.CL DD DSN=MDB.LOADLIB,DISP=SHR
 INCLUDE PR(CLPARM,ZPDATE,LPL82)
 INCLUDE CL(MDBSTOR)
/*
//*                                     ****************
//*                                     ** Set DsName **
//GO.FT01F001 DD DISP=SHR,LABEL=(,,,IN),DSN=MDB.??????
