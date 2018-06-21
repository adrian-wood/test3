//T12SCTMS JOB (M12,SC,WDS0BF),S.COX.X6955,PRTY=8,MSGCLASS=Q
//*
//* TO CHECK DESCRIPTORS IN MODEL FILE AGAINST MERGE TABLE
//* (INPUT LINE SAYS WHICH COLUMN THE SUBSCRIPTS ARE IN:
//*  A - ANALYSIS, B - BACKGROUND, M - MODEL LEVELS)
//*
// EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(27998)',TIME.GO=(0,3),
// REGION.GO=2M
//SYSIN DD DSN=MDB.UTILITY.SRCE(MERTABMS),DISP=SHR
//      DD DSN=MDB.MERGE.SRCE(MODELB),DISP=SHR
//LKED.DB DD DSN=SYS1.SDBLOAD,DISP=SHR
    INCLUDE DB(BUFR)
//GO.FT01F001 DD DSN=MCC3.SCADDAT.SSMI,DISP=SHR,LABEL=(,,,IN)
//GO.FT02F001 DD DSN=MDB.MERGE.TABLES(SSMI),DISP=SHR,LABEL=(,,,IN)
BB