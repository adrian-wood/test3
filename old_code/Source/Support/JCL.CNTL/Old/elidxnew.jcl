//T12SCMIX JOB (M12,DB,WDS0BF),MDBTEAM.6951,PRTY=8,MSGCLASS=Q
//*
//*--------------------------------------------------------------------
//* ELIDXNEW
//*
//* $Revision: 2$ $Date: 25/06/2007 09:54:03$
//*--------------------------------------------------------------------
//*
//ELIDX EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(27998),DC(*)'
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(ELIDXNEW),DISP=SHR
//*
//GO.FT02F001 DD DSN=MDB.ELEMENT.INDEX.NEW????,
//   UNIT=DISK,VOL=SER=OPR202,SPACE=(TRK,(45,1),RLSE),
//   DCB=(RECFM=FB,LRECL=10000,BLKSIZE=10000),DISP=(NEW,CATLG)
//GO.FT05F001 DD *

NUMSUBTYPES=053                                                   !1.17

AATSR                                                             !1.16
MDB.ELEMENTS.INDEX(AATSR)                                         !1.16

AIREPS
MDB.ELEMENTS.INDEX(AIREPS)

AIRS                                                              !1.16
MDB.ELEMENTS.INDEX(AIRS)                                          !1.16

AMDARS
MDB.ELEMENTS.INDEX(AMDARS)

AMSUB
MDB.ELEMENTS.INDEX(AMSUB)

ATOVSG
MDB.ELEMENTS.INDEX(ATOVS)

ATOVSL
MDB.ELEMENTS.INDEX(ATOVS)

AVHRRG
MDB.ELEMENTS.INDEX(AVHRRG)

BATHY
MDB.ELEMENTS.INDEX(BATHY)

BOGUS
MDB.ELEMENTS.INDEX(BOGUS)

BUOY
MDB.ELEMENTS.INDEX(BUOY)

BUOYPROF
MDB.ELEMENTS.INDEX(BUOY)

DRIFTR
MDB.ELEMENTS.INDEX(BUOY)

DROPSOND
MDB.ELEMENTS.INDEX(UPPERAIR)

ESACMW
MDB.ELEMENTS.INDEX(ESAUWIND)

ESACSR
MDB.ELEMENTS.INDEX(ESACSR)

ESACSWVW
MDB.ELEMENTS.INDEX(ESAUWIND)

ESAHRVW
MDB.ELEMENTS.INDEX(ESAUWIND)

ESAHRWVW
MDB.ELEMENTS.INDEX(ESAUWIND)

ESAURA
MDB.ELEMENTS.INDEX(ESAURA)                                        !1.19

ESAUWA
MDB.ELEMENTS.INDEX(ESAUWA)                                        !1.19

ESAUWI
MDB.ELEMENTS.INDEX(ESAUWI)                                        !1.19

ESAWS
MDB.ELEMENTS.INDEX(ESAWS)

FOAMSST
MDB.ELEMENTS.INDEX(UKMOSST)

GLOSS
MDB.ELEMENTS.INDEX(GLOSS)

GOESAMW
MDB.ELEMENTS.INDEX(SATOB)

GOESBUFR                                                          !1.16
MDB.ELEMENTS.INDEX(GOESBUFR)                                      !1.16

GOESVIS
MDB.ELEMENTS.INDEX(SATOB)

GOESWV
MDB.ELEMENTS.INDEX(SATOB)

GPSIWV
MDB.ELEMENTS.INDEX(GPSIWV)

GPSRO                                                             !1.14
MDB.ELEMENTS.INDEX(GPSRO)                                         !1.14

LNDSYN
MDB.ELEMENTS.INDEX(LNDSYN)                                        !1.19

MODIS                                                             !1.17
MDB.ELEMENTS.INDEX(MODIS)                                         !1.18

OZONESAT                                                          !1.14
MDB.ELEMENTS.INDEX(OZONESAT)                                      !1.14

PAOBS
MDB.ELEMENTS.INDEX(PAOBS)

PILOT
MDB.ELEMENTS.INDEX(UPPERAIR)

RASS
MDB.ELEMENTS.INDEX(RASS)

SALTSSH                                                           !1.13
MDB.ELEMENTS.INDEX(SALTSSH)                                       !1.13

SATOB
MDB.ELEMENTS.INDEX(SATOB)

SAT120
MDB.ELEMENTS.INDEX(SAT120)

SEAWINDS
MDB.ELEMENTS.INDEX(SEAWINDS)

SFERICS
MDB.ELEMENTS.INDEX(SFERICS)

SFLOC
MDB.ELEMENTS.INDEX(SFLOC)

SHPSYN
MDB.ELEMENTS.INDEX(SHPSYN)                                        !1.19

SSMI
MDB.ELEMENTS.INDEX(SSMI)

SSMIS                                                             !1.14
MDB.ELEMENTS.INDEX(SSMIS)                                         !1.14

TEMP
MDB.ELEMENTS.INDEX(UPPERAIR)

TESAC
MDB.ELEMENTS.INDEX(TESAC)

TRACKOB
MDB.ELEMENTS.INDEX(TRACKOB)

TROPCYCL                                                          !1.15
MDB.ELEMENTS.INDEX(TROPCYCL)                                      !1.15

UKMMWI
MDB.ELEMENTS.INDEX(UKMMWI)

UKMOSST
MDB.ELEMENTS.INDEX(UKMOSST)

WINPRO
MDB.ELEMENTS.INDEX(WINPRO)

/*
//
