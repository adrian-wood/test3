//T12DHWM3 JOB (M12,DH,WDS0BF),D.HIRST.X6955,
//  MSGCLASS=Q,TIME=(2,00),NOTIFY=T12DH,PRTY=??
//*
//* SIX MONTHLY UPDATE OF STNMAS.MAIN FROM STATION.INDEX
//*
//SAVEM    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=Q
//SYSUT1   DD DSN=SDB.STNMAS.MAINB,DISP=SHR
//SYSUT2   DD DSN=SDB.STNMAS.MAINO,DISP=SHR
//SYSIN    DD DUMMY
//*
//SAVEI    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=Q
//SYSUT1   DD DSN=SDB.STNMAS.INDEXB,DISP=SHR
//SYSUT2   DD DSN=SDB.STNMAS.INDEXO,DISP=SHR
//SYSIN    DD DUMMY
//*
//*  TO REPOPULATE STATIONMASTER MAIN AND INDEX DEVELOPMENT DATASETS
//*  IN HIPERBATCH WITH THE UP TO DATE DATA FROM THE DATASETS ON DISK
//*
//HPOP   EXEC PGM=STNHWRT,REGION=1500K
//STEPLIB DD DSN=SDB.LOADLIB,DISP=SHR
//FT06F001 DD SYSOUT=Q,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1463)
//FT20F001 DD DSN=SDB.STNMAS.MAINB,DISP=SHR
//FT25F001 DD DSN=MCC3.STNMAS.MAIN,DISP=SHR
//FT30F001 DD DSN=SDB.STNMAS.INDEXB,DISP=SHR
//FT35F001 DD DSN=MCC3.STNMAS.INDEX,DISP=SHR
//*
//UPDATE EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(1000)',
//           TIMEG=2,GOREGN=1500K
//FORT.SYSPRINT DD SYSOUT=*
//FORT.SYSIN DD DSN=SDB.DBCLMSRC(STNUPDT),DISP=SHR
//           DD DSN=SDB.DBCLMSRC(STNINDX),DISP=SHR
//           DD DSN=SDB.DBCLMSRC(STNFIN),DISP=SHR
//           DD DSN=SDB.DBCLMSRC(REGSUB),DISP=SHR
//           DD DSN=SDB.DBCLMSRC(STNRITE),DISP=SHR
//*
//GO.FT06F001 DD SYSOUT=Q,DCB=(RECFM=FBA,LRECL=133,BLKSIZE=1463)
//GO.FT15F001 DD DSN=MCC3.STATION.INDEX,DISP=SHR,LABEL=(,,,IN)
//GO.FT20F001 DD DSN=SDB.STNMAS.LOG,DISP=SHR,LABEL=(,,,IN)
//GO.FT25F001 DD DSN=SDB.STNMAS.NAME,DISP=SHR,LABEL=(,,,IN)
//GO.FT88F001 DD DSN=MCC3.STNMAS.MAIN,DISP=SHR
//GO.FT89F001 DD DSN=MCC3.STNMAS.INDEX,DISP=SHR
//GO.SYSUDUMP DD SYSOUT=Q
//
