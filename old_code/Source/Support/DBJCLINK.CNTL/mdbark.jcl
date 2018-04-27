//T12??ARK JOB (M12,??,WDS0BF),METDB.????.X6953,PRTY=??,
//     MSGCLASS=Q,MSGLEVEL=(1,1)
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : MDBARK
//*
//* PURPOSE       : Build MDBARK load module from source.  (Program
//*                  to copy data for given period from one MDB set
//*                  to another.)
//*
//* INTRODUCED    : Cloned June 1997 by Jim Arnott (CC3) from
//*                 pre-existing original.
//*
//* CHANGE RECORD :
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//*
// EXEC FORT2CL,
//      FPARMS='NOFIPS',FPARMS2='CHARLEN(27998),XREF,MAP,DC(*)',
//      LMAP=MAP
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(MDBARK),DISP=SHR
//           DD DSN=MDB.SOURCE(BLKCHK),DISP=SHR
//           DD DSN=MDB.SOURCE(CHAR2),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//*
//           DD DSN=MDB.SOURCE(DT2HRS),DISP=SHR
//           DD DSN=MDB.SOURCE(HRS2DT),DISP=SHR
//*
//           DD DSN=MET.SRCELIB(IBMISO8),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MDBARK),DISP=SHR,SPACE=
//*
//LKED.METPL DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
   INCLUDE METPL(ZPDATE,CLPARM)
/*
//
