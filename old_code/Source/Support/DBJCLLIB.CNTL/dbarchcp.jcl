//T12DBCAR JOB (M22,JN,WDS0BF),NORTON.EXT.6928,PRTY=??,
// MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12DB
//*   MAIN DEADLINE=(1630,A,070697)
//*               HHMM   MMDDYY
//*
//*-----------------------------------------------------------------*   00094400
//* MAKE SURE CONTROL DATASET IS COPIED FOR 10Z RUN                     00094500
//*-----------------------------------------------------------------*   00094600
//*                                                                     00030300
//*                                                                     00030300
//COPYA01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.A01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.A01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYB01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.B01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.B01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYC01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.C01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.C01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYD01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.D01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.D01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYE01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.E01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.E01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYA10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.A10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.A10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYB10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.B10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.B10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYC10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.C10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.C10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYD10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.D10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.D10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYE10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.E10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.E10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYF10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.F10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.F10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYG10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.G10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.G10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYH10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.H10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.H10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYI10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.I10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.I10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYJ10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.J10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.J10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYK10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.K10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.K10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//COPYL10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.COPY.L10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.COPY.L10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//*                                                                     00030300
//*                                                                     00030300
//*                                                                     00030300
//SCRHA01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.A01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.A01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHB01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.B01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.B01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHC01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.C01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.C01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHD01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.D01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.D01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHE01  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.E01,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.E01,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHA10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.A10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.A10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHB10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.B10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.B10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHC10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.C10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.C10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHD10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.D10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.D10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHE10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.E10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.E10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHF10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.F10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.F10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHG10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.G10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.G10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHH10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.H10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.H10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHI10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.I10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.I10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHJ10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.J10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.J10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHK10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.K10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.K10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//SCRHL10  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSIN    DD DUMMY
//SYSUT1   DD DSN=MDB.ARCHIVE.DATAPARK.SCRATCH.L10,DISP=SHR
//SYSUT2   DD DSN=MCC3.ARCHIVE.DATAPARK.SCRATCH.L10,DISP=(MOD,CATLG),
//  STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  SPACE=(TRK,(1,1),RLSE),DCB=(RECFM=FB,LRECL=80)
//*                                                                     00030300
//*                                                                     00030300
//*                                                                     00030300
//*                                                                     00030300
//
