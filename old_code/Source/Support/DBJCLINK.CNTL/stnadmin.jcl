//T12DBSTA JOB (M12,DB,WDS0BF),METDB.TEAM.6953,PRTY=??,
//         MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12DH
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : STNADMIN
//*
//* PURPOSE       : To rebuild STNADMIN StationMaster update pgm
//*                 from source. NB. UNDER DEVELOPMENT - DH USE ONLY
//*
//* INTRODUCED    : May 1998 by Dick Hirst
//*
//* CHANGE RECORD :
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//S1 EXEC FORT2CL,FPARMS='CHARLEN(692),NOFIPS,DC(*)',
//        LMAP=MAP,LXREF=XREF,INCLIB1='MCC3.DHSTNLIB.FORT'
//*
//FORT.SYSIN DD DSN=MCC3.DHSTNLIB.FORT(STNADMIN),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(STNDISPA),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(STNTOPAN),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(STNFRPAN),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(WRITBAK),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(WRITBAK2),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(STNINDX),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(WRITINDX),DISP=SHR
//           DD DSN=MCC3.DHSTNLIB.FORT(STOPAN02),DISP=SHR
//           DD DSN=MDB.SOURCE(STNFIN),DISP=SHR
//           DD DSN=MDB.UTILITY.SRCE(UPDLOG),DISP=SHR
//*           DD DSN=MDB.UTILITY.SRCE(STCPAN03),DISP=SHR
//           DD DSN=MDB.UTILITY.SRCE(STNNAME),DISP=SHR
//           DD DSN=MDB.UTILITY.SRCE(REGSUB),DISP=SHR
//           DD DSN=MDB.SOURCE(IVALUE),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(STNADDEV),DISP=OLD,
//  SPACE=,UNIT=
//*
//LKED.L1 DD DSN=SYS1.ISP.LINKLIB,DISP=SHR
//LKED.SYSIN DD *
    INCLUDE L1(ISPLNK,ISPEX)
/*
//
