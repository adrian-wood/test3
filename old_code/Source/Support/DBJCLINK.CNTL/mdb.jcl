//T12DBMDB JOB (OP1,DB,WDS0BF),MDBTEAM.X6675,PRTY=8,MSGCLASS=Q
//*
//*=====================================================================
//*             BUILDS MET.D.B. RETRIEVAL MODULE ("MDB")
//*             ----------------------------------------
//*    Makes load module MDB for retrieval of data from the MetDB.
//*
//* REVISION INFO:
//* --------------
//* $Workfile: mdb.jcl$ $Folder: DBJCLINK.CNTL$
//* $Revision: 5$ $Date: 29/03/2010 16:57:47$
//*
//* $Log:
//*  5    Met_DB_Project 1.4         29/03/2010 16:57:47    Brian Barwell
//*       Change SYS1.VSF6COMP to SYS1.VSF2COMP.
//*  4    Met_DB_Project 1.3         14/09/2009 11:17:35    Sheila Needham
//*       Changed location of MDBRSN load module
//*  3    Met_DB_Project 1.2         18/11/2008 13:59:50    Richard Weedon
//*       TAFCNL ADDED TO BUILD
//*  2    Met_DB_Project 1.1         20/05/2008 09:37:19    Brian Barwell
//*       Modified to use new MetDB source pre-processor.
//*  1    Met_DB_Project 1.0         30/01/2006 17:55:48    Sheila Needham  
//* $
//*=====================================================================
//*                      ASSEMBLER ROUTINE
//*=====================================================================
//DYNALLOC EXEC ASMAC                                                !2
//SYSIN DD DSN=MDB.SOURCE(DYNALLOC),DISP=SHR
//*
//*=====================================================================
//*         PRE-COMPILER STEP TO SELECT ONLY MVS STATEMENTS          !2
//*         -----------------------------------------------          !2
//*  (Source in the FT10F001 DD statement is held in StarTeam with   !2
//*   names like 'name.F' rather than 'name.f', e.g. 'getlocd.F'.)   !2
//*=====================================================================
//PREPRO EXEC PGM=PREPRO                                             !2
//STEPLIB DD DSN=MDB.LOADLIB,DISP=SHR                                !2
//*
//FT06F001 DD SYSOUT=Q                                               !2
//FT10F001 DD DSN=MDB.SOURCE(GETLOCD),DISP=SHR                       !2
//         DD DSN=MDB.SOURCE(GRIBINDX),DISP=SHR                      !2
//         DD DSN=MDB.SOURCE(GRIBOPEN),DISP=SHR                      !2
//         DD DSN=MDB.SOURCE(MDB),DISP=SHR                           !2
//         DD DSN=MDB.SOURCE(MDBALC),DISP=SHR                        !2
//         DD DSN=MDB.SOURCE(READABRV),DISP=SHR                      !2
//         DD DSN=MDB.SOURCE(READIDX),DISP=SHR                       !2
//         DD DSN=MDB.SOURCE(READLIST),DISP=SHR                      !2
//         DD DSN=MDB.SOURCE(RTABLE),DISP=SHR                        !2
//         DD DSN=MDB.SOURCE(STNRET),DISP=SHR                        !2
//FT20F001 DD DSN=&PREPRO,UNIT=SYSDA,SPACE=(TRK,(10,5)),             !2
//            DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200),DISP=(MOD,PASS)   !2
//*
//*=====================================================================
//*                    FORTRAN COMPILATION
//*=====================================================================
// EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(27998),DC(*),PCP(256),OPT(2)'
//*
//FORT.STEPLIB DD DISP=SHR,DSN=MET.FORT.P65473.LOADLIB.V4
//             DD DISP=SHR,DSN=SYS1.VSF2COMP                         !5
//FORT.SYSIN DD DSN=&PREPRO,DISP=(OLD,DELETE)                        !2
//           DD DSN=MDB.SOURCE(ALSELM),DISP=SHR                      !2
//           DD DSN=MDB.SOURCE(ARRINDX),DISP=SHR
//           DD DSN=MDB.SOURCE(ARRSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(BITINDX),DISP=SHR
//           DD DSN=MDB.SOURCE(BUFINDX),DISP=SHR
//           DD DSN=MDB.SOURCE(BUFRET),DISP=SHR
//           DD DSN=MDB.SOURCE(BUSRET),DISP=SHR
//           DD DSN=MDB.SOURCE(CENTURY),DISP=SHR
//           DD DSN=MDB.SOURCE(CHKMODID),DISP=SHR
//           DD DSN=MDB.SOURCE(CHK4TW),DISP=SHR
//           DD DSN=MDB.SOURCE(CONDES),DISP=SHR
//           DD DSN=MDB.SOURCE(DATCHK),DISP=SHR
//           DD DSN=MDB.SOURCE(DELSPCE),DISP=SHR
//           DD DSN=MDB.SOURCE(DT2HRS),DISP=SHR
//           DD DSN=MDB.SOURCE(EXPELM),DISP=SHR
//           DD DSN=MDB.SOURCE(EXPREQ),DISP=SHR
//           DD DSN=MDB.SOURCE(GETARE),DISP=SHR
//           DD DSN=MDB.SOURCE(GETDAT),DISP=SHR
//           DD DSN=MDB.SOURCE(GETKEY),DISP=SHR
//           DD DSN=MDB.SOURCE(GETREQ),DISP=SHR
//           DD DSN=MDB.SOURCE(GETSTN),DISP=SHR
//           DD DSN=MDB.SOURCE(GETSTR),DISP=SHR
//           DD DSN=MDB.SOURCE(GRIBFIND),DISP=SHR
//           DD DSN=MDB.SOURCE(GRIBREAD),DISP=SHR
//           DD DSN=MDB.SOURCE(GRIBRET),DISP=SHR
//           DD DSN=MDB.SOURCE(GTGRP),DISP=SHR
//           DD DSN=MDB.SOURCE(HRS2DT),DISP=SHR
//           DD DSN=MDB.SOURCE(ICEINT),DISP=SHR
//           DD DSN=MDB.SOURCE(ICERET),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR3),DISP=SHR
//           DD DSN=MDB.SOURCE(IDMTCH),DISP=SHR
//           DD DSN=MDB.SOURCE(INTCON),DISP=SHR
//           DD DSN=MDB.SOURCE(INXKEY),DISP=SHR
//           DD DSN=MDB.SOURCE(ITERAT),DISP=SHR
//           DD DSN=MDB.SOURCE(IVALUE),DISP=SHR
//           DD DSN=MDB.SOURCE(KEYS),DISP=SHR
//           DD DSN=MDB.SOURCE(MAPELM),DISP=SHR
//           DD DSN=MDB.SOURCE(MAPRD),DISP=SHR
//           DD DSN=MDB.SOURCE(MSGDSP),DISP=SHR
//           DD DSN=MDB.SOURCE(MSGDSPN),DISP=SHR                  !1.17
//           DD DSN=MDB.SOURCE(MSGSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(MSGSUBN),DISP=SHR                  !1.17
//           DD DSN=MDB.SOURCE(MTRCCC),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRCLD),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRDDF),DISP=SHR
//           DD DSN=MDB.SOURCE(MTREXP),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRGRP),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRINT),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRLOC),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRPER),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRRWY),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRSHE),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRSKY),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRTRD),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRVHT),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRWND),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRWTR),DISP=SHR
//           DD DSN=MDB.SOURCE(MTRWWR),DISP=SHR
//           DD DSN=MDB.SOURCE(NCMEXP),DISP=SHR
//           DD DSN=MDB.SOURCE(NCMINT),DISP=SHR
//           DD DSN=MDB.SOURCE(NMSTEM),DISP=SHR
//           DD DSN=MDB.SOURCE(NXTKEY),DISP=SHR
//           DD DSN=MDB.SOURCE(OFIGTS),DISP=SHR
//           DD DSN=MDB.SOURCE(PARSE),DISP=SHR
//           DD DSN=MDB.SOURCE(POP),DISP=SHR
//           DD DSN=MDB.SOURCE(PUSH),DISP=SHR
//           DD DSN=MDB.SOURCE(RAINEXP),DISP=SHR
//           DD DSN=MDB.SOURCE(RDBITAB),DISP=SHR
//           DD DSN=MDB.SOURCE(ROTAREA),DISP=SHR
//           DD DSN=MDB.SOURCE(SETDEF),DISP=SHR
//           DD DSN=MDB.SOURCE(SETHED),DISP=SHR
//           DD DSN=MDB.SOURCE(SKLARR),DISP=SHR
//           DD DSN=MDB.SOURCE(SKLDSP),DISP=SHR
//           DD DSN=MDB.SOURCE(SKLDSPN),DISP=SHR                  !1.17
//           DD DSN=MDB.SOURCE(SKLSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(SKLSUBN),DISP=SHR                  !1.17
//           DD DSN=MDB.SOURCE(SORTCH),DISP=SHR
//           DD DSN=MDB.SOURCE(SORTCHR),DISP=SHR                  !1.18
//           DD DSN=MDB.SOURCE(SORTN),DISP=SHR
//           DD DSN=MDB.SOURCE(SORTR),DISP=SHR
//           DD DSN=MDB.SOURCE(SRWEXP),DISP=SHR
//           DD DSN=MDB.SOURCE(SRWINT),DISP=SHR
//           DD DSN=MDB.SOURCE(SSMRET),DISP=SHR
//           DD DSN=MDB.SOURCE(STMRET),DISP=SHR
//           DD DSN=MDB.SOURCE(STNARY),DISP=SHR
//           DD DSN=MDB.SOURCE(STNFIN),DISP=SHR
//           DD DSN=MDB.SOURCE(STNINDX),DISP=SHR
//           DD DSN=MDB.SOURCE(SUBPER),DISP=SHR
//           DD DSN=MDB.SOURCE(SUBSLOCD),DISP=SHR
//           DD DSN=MDB.SOURCE(SYNRET),DISP=SHR
//           DD DSN=MDB.SOURCE(TAFCNL),DISP=SHR               !3.0
//           DD DSN=MDB.SOURCE(TAFEXP),DISP=SHR
//           DD DSN=MDB.SOURCE(TAFINT),DISP=SHR
//           DD DSN=MDB.SOURCE(TAFSECT),DISP=SHR               !1.13
//           DD DSN=MDB.SOURCE(TAFVISM),DISP=SHR               !1.13
//           DD DSN=MDB.SOURCE(TEMPEXP),DISP=SHR
//*
//* TFMRET also contains function EOCHN
//*
//           DD DSN=MDB.SOURCE(TFMRET),DISP=SHR
//           DD DSN=MDB.SOURCE(TRNSFR),DISP=SHR
//           DD DSN=MDB.SOURCE(TRPINT),DISP=SHR
//           DD DSN=MDB.SOURCE(UPAIND),DISP=SHR
//           DD DSN=MDB.SOURCE(UPREPID),DISP=SHR
//*
//* UPRPARTS also contains subroutines STNDELEM, SGNFELEM, SWAPARRAY
//*
//           DD DSN=MDB.SOURCE(UPRPARTS),DISP=SHR
//*
//* UPRRET also contains subroutine CHECKPOSITIONS
//*
//           DD DSN=MDB.SOURCE(UPRRET),DISP=SHR
//           DD DSN=MDB.SOURCE(UPRWINDS),DISP=SHR
//           DD DSN=MDB.SOURCE(VALAREA),DISP=SHR
//           DD DSN=MDB.SOURCE(VALARR),DISP=SHR
//           DD DSN=MDB.SOURCE(VALDAT),DISP=SHR
//           DD DSN=MDB.SOURCE(VALREC),DISP=SHR
//           DD DSN=MDB.SOURCE(VALREQ),DISP=SHR
//           DD DSN=MDB.SOURCE(VALUE),DISP=SHR
//           DD DSN=MDB.SOURCE(VALUSR),DISP=SHR
//*
//           DD DSN=MET.SRCELIB(IBMISO8),DISP=SHR
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,                        !2
//             DSN=MCC3.DBLOAD.Dyymmdd(MDB)                          !2
//*
//LKED.OBJMOD1 DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.OBJMOD2 DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN   DD  *
  INCLUDE OBJMOD1(BUFRSHEL,MDBRSN)
  INCLUDE OBJMOD2(ZPDATE)
  ENTRY MDB
/*
//
