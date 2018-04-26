//T12SCSHL JOB (M12,SC,WDS0BF),METDB.SCOX.X6955,PRTY=??,
//     MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12SC
//*
//* --------------------------------------------------------------------
//*
//* TITLE         : MDBSHELL
//*
//* PURPOSE       : To remake the MDB shell (including a small.
//*               : fortran function for ease of use by one
//*               : unused to Assembler - it has no function).
//*
//* INTRODUCED    : Cloned June 1997 by Jim Arnott (CC3) from
//*               : original by Alan Montgomery.
//*
//* CHANGE RECORD :
//*
//* --------------------------------------------------------------------
//*
//*
//ASM EXEC ASMAC
//SYSIN DD DSN=MDB.SOURCE(MDBSHELL),DISP=SHR
//*
//*
//FORT EXEC FORT2CL,LPARMS='XREF,MAP'
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(GETCHR),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.SCLOAD.N46210(MDB),DISP=SHR,SPACE=
//
