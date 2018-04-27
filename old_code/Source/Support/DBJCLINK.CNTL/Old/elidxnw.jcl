//T12??ELD JOB (M12,??,WDS0BF),METDB.????.X6953,PRTY=??,
//   MSGCLASS=Q,MSGLEVEL=(1,1)
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : ELIDXNEW
//*
//* PURPOSE       : To construct the element index dataset from source.
//*
//* INTRODUCED    : Cloned June 1997 by Jim Arnott (CC3) from
//*                 original by Simon Cox.
//*
//* CHANGE RECORD :
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
// EXEC FORT2CL,
//    FPARMS='NOFIPS',FPARMS2='CHARLEN(27998),XREF,MAP,DC(*)',
//    LPARMS='MAP'
//*
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(ELIDXNEW),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.??LOAD(ELIDXNEW),DISP=SHR
//
