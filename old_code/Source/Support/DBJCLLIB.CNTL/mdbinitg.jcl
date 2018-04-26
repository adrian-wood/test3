//T12BCING JOB (OP1,DB,WDS0BF),MDBTEAM.G05.6951,PRTY=??,MSGCLASS=Q
//*
//*---------------------------------------------------------------------
//* TO INITIALISE AN MDB INDEX DATA SET FOR DATA TYPES STORED IN GRIB.
//*
//* The following parameters should be coded on the EXEC statement:
//*
//*    DSN     Name of index data set (in quotes),
//*    DISK    Disk where index data set is to be located,
//*    TRKS    Number of tracks allocated to index data set,
//*    BLOCK   Block size for index data set (9306 recommended),
//*    YEARS   Data retention period in years,
//*    GRIB    GRIB edition number of fields to be stored,          !1.3
//*    MSGS    Number of GRIB messages each data time (usually 1),  !1.3
//*    SKEL    Skeleton data set name for GRIB data sets (in quotes).
//*
//* Notes:  Number of records must be at least (YEARS+4). With a block
//* ------  size of 9306, five records fit on a track on a 3390 disk.
//*
//*         Skeleton data set name should contain 'YY', 'MM', 'DD' and
//*         'HH' for year, month, day and hour; e.g. for TCRTEMP,
//*
//*                   SKEL='MDB.DATCRTMP.DYYMMDD.THH'
//*
//*         If 'MSGS' is >1, the skeleton name should also contain  !1.3
//*         '##' (e.g. ending in 'THH##') to be replaced with a     !1.3
//*         message number taken from the last 2 characters of MHS  !1.3
//*         data set name.                                          !1.3
//*
//*---------------------------------------------------------------------
//*                                COMPILE AND LINK SOURCE FOR MDBINITG
//*
//INITGRIB EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(28000)'
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(MDBINITG),DISP=SHR
//LKED.MP    DD DSN=MET.PROGLIB,DISP=SHR
 INCLUDE MP(CLPARM)
/*
//*---------------------------------------------------------------------
//*                                       PROCEDURE TO EXECUTE MDBINITG
//INITG PROC TRKS=
// EXEC FORT2G,PARM='&TRKS,&BLOCK,&YEARS,&GRIB,&MSGS,''&SKEL'''    !1.3
//GO.FT01F001 DD DSN=&DSN,DISP=(,CATLG),UNIT=DISK,VOL=SER=&DISK,
//   DCB=(RECFM=F,BLKSIZE=&BLOCK),SPACE=(TRK,&TRKS)
// PEND
//*
//*---------------------------------------------------------------------
//*                              RUN PROCEDURE TO CREATE INDEX DATA SET
//*
//MFSST   EXEC INITG,DSN='MDB.MFSST.INDEX',DISK=OPR100,
//        TRKS=3,BLOCK=9306,YEARS=10,GRIB=1,MSGS=3,
//        SKEL='MDB.DAMFSST.DYYMMDD.THH##'
//*---------------------------------------------------------------------
//
