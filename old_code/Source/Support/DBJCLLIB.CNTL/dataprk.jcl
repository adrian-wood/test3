//MDBDBMET JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBARK
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.METARS
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  DISP=(,CATLG),SPACE=(CYL,0030,RLSE),
//  DCB=(RECFM=F,BLKSIZE=23476),
//            DSN=MDB.DAMETARS.D040615
//FT05F001 DD *
20040615/1200Z,20040616/1159Z,23476,23,TAFMET
/*
//
//MDBDBSA5 JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBARK
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.SATOB.OPER.MAIRUK
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  DISP=(,CATLG),SPACE=(CYL,0010,RLSE),
//  DCB=(RECFM=F,BLKSIZE=27998),
//            DSN=MDB.DASATOB.MAIRUK.D040621
//FT05F001 DD *
20040621/2100Z,20040622/2059Z,27998,23,TRAILER,BUFR
/*
//
//MDBDBSS3 JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBARK
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.SSTSUPOB.OPER.GSST
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,
//  DISP=(,CATLG),SPACE=(CYL,0001,RLSE),
//  DCB=(RECFM=F,BLKSIZE=27998),
//            DSN=MDB.DASSTSUP.GSST.D040622
//FT05F001 DD *
20040622/0000Z,20040622/2359Z,27998,23,TRAILER,BUFR
/*
//
//MDBDBGB1 JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBXTRCT
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.GOESBUFR.OPER.GAIR
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,DISP=(,CATLG),
//  SPACE=(CYL,0040,RLSE),DCB=(RECFM=F,BLKSIZE=27998),
//       DSN=MDB.DAGOESBF.GAIR.D040622
//FT05F001 DD *
20040622/0000Z,20040622/2359Z,27998,
/*
//
//MDBDBMO1 JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBXTRCT
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.MODIS.OPER.GAIR
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,DISP=(,CATLG),
//  SPACE=(CYL,0030,RLSE),DCB=(RECFM=F,BLKSIZE=27998),
//       DSN=MDB.DAMODIS.GAIR.D040622
//FT05F001 DD *
20040622/0000Z,20040622/2359Z,27998,
/*
//
//MDBDBSW1 JOB (OP1,DB,WDS0BF),METDB.6955,CLASS=I,MSGCLASS=Q       !1.7
//STEP1 EXEC PGM=MDBXTRCT
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR
//OLD DD DISP=SHR,LABEL=(,,,IN),
//       DSN=MDB.SEAWINDS.OPER.GAIR
//NEW DD STORCLAS=SCDATPRK,MGMTCLAS=MCSNC4,DISP=(,CATLG),
//  SPACE=(CYL,0500,RLSE),DCB=(RECFM=F,BLKSIZE=27998),
//       DSN=MDB.DASEAWIN.GAIR.D040622
//FT05F001 DD *
20040622/0000Z,20040622/2359Z,27998,
/*
//