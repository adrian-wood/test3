//T12SCTSQ JOB (M12,SC,WDS0BF),C.LONG.X6953,PRTY=8,MSGCLASS=Q
//*
//* TO CHECK DESCRIPTORS IN MERGE TABLE AGAINST SEQUENCE IN LIBRARY
//* (STORAGE DATA SET NEEDS DIFFERENT READ!)
//*
// EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(28000)',
//      TIME.GO=(0,3),REGION.GO=2M
//SYSIN DD DSN=MDB.UTILITY.SRCE(MERTABSQ),DISP=SHR
//LKED.DB DD DSN=SYS1.SDBLOAD,DISP=SHR
    INCLUDE DB(BUFR)
//* GO.FT01F001 DD DISP=SHR,LABEL=(,,,IN),DSN=MDB.GLOSS.OPER.GAIR
//GO.FT01F001 DD DSN=MDB.MERGE.SEQUENCE(ESAHRWSC),DISP=SHR
//GO.FT02F001 DD DSN=MDB.MERGE.TABLES(ESAHRWSC),DISP=SHR,LABEL=(,,,IN)
