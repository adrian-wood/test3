//MDBDBAJB JOB (OP1,DB,WDS0BF),METDB.TEAM.6955,MSGCLASS=Q,
//  TIME=(0,30),REGION=4096K,PRTY=8
//*
//* CREATE DATASET CONTAINING METDB ARCHIVING SUITE JOBS.
//*
//MDBARC EXEC PGM=CARTSUB
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//GO.FT06F001 DD SYSOUT=*
//GO.FT10F001 DD DSN=MDB.ARCHIVE.LOOKUP,
//            DISP=SHR,LABEL=(,,,IN)
//GO.FT20F001 DD DSN=MDB.ARCHIVE.JOBS,
//            DISP=SHR,LABEL=(,,,IN)
//GO.FT31F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.A01,
//            DISP=SHR
//GO.FT32F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.B01,
//            DISP=SHR
//GO.FT33F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.C01,
//            DISP=SHR
//GO.FT34F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.D01,
//            DISP=SHR
//GO.FT35F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.E01,
//            DISP=SHR
//GO.FT36F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.A10,
//            DISP=SHR
//GO.FT37F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.B10,
//            DISP=SHR
//GO.FT38F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.C10,
//            DISP=SHR
//GO.FT39F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.D10,
//            DISP=SHR
//GO.FT40F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.E10,
//            DISP=SHR
//GO.FT41F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.F10,
//            DISP=SHR
//GO.FT42F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.G10,
//            DISP=SHR
//GO.FT43F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.H10,
//            DISP=SHR
//GO.FT44F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.I10,
//            DISP=SHR
//GO.FT45F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.J10,
//            DISP=SHR
//GO.FT46F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.K10,
//            DISP=SHR
//GO.FT47F001 DD DSN=MDB.ARCHIVE.DATAPARK.COPY.L10,
//            DISP=SHR
//GO.FT51F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.A01,
//            DISP=SHR
//GO.FT52F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.B01,
//            DISP=SHR
//GO.FT53F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.C01,
//            DISP=SHR
//GO.FT54F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.D01,
//            DISP=SHR
//GO.FT55F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.E01,
//            DISP=SHR
//GO.FT56F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.A10,
//            DISP=SHR
//GO.FT57F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.B10,
//            DISP=SHR
//GO.FT58F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.C10,
//            DISP=SHR
//GO.FT59F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.D10,
//            DISP=SHR
//GO.FT60F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.E10,
//            DISP=SHR
//GO.FT61F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.F10,
//            DISP=SHR
//GO.FT62F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.G10,
//            DISP=SHR
//GO.FT63F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.H10,
//            DISP=SHR
//GO.FT64F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.I10,
//            DISP=SHR
//GO.FT65F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.J10,
//            DISP=SHR
//GO.FT66F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.K10,
//            DISP=SHR
//GO.FT67F001 DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.L10,
//            DISP=SHR
//GO.INTOUT DD DSN=MCC3.DBARCH.JOBS.CNTL,DISP=(MOD,CATLG),
//    STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,SPACE=(TRK,(1,1)),
//    DCB=(RECFM=FB,LRECL=80,BLKSIZE=27920)
//
