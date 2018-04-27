//T12DBMKL JOB (M12,DB,WDS0BF),METDB.TEAM,
// PRTY=8,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=128M
//*
//* ====================================================================
//* JCL to make the MetDB RPC multi-user server - S.Cox
//*
//* COMPILE.SYSIN   : Source to compile
//* COMPILE.USERLIB : Directory containing user include files
//* LKED.SYSLMOD    : Final executable
//*
//* LKED.SYSLIB     : SYS1.VSF6LOAD needed for fortran routines.
//*                 : SDB.LOADLIB contains assembler load ASMRPC
//* ====================================================================
//*
//S1 EXEC EDCCPL,
//   CPARM='RENT,SOURCE,MARGINS(1,*),MEMORY,LONGNAME,NOSEARCH'
//*
//COMPILE.SYSIN   DD DSN=MDB.RETRIEVL.CSRCE(MSRPC),DISP=SHR
//COMPILE.USERLIB DD DSN=MDB.RETRIEVL.HSRCE,DISP=SHR
//LKED.SYSLMOD    DD DSN=MCC3.DBLOAD.RPC(MSRPC),DISP=SHR,UNIT=
//*
//* Link Step: Note the TCPIP and &LIBPRFX libraries will soon be
//*            part of the EDCCPL procedure, so won't be needed here
//*
//LKED.SYSLIB     DD DSN=SYS1.SEZALNK2,DISP=SHR
//                DD DSN=SYS1.SEZARPCL,DISP=SHR
//                DD DSN=SYS1.SEZACMTX,DISP=SHR
//                DD DSN=&LIBPRFX..SCEELKED,DISP=SHR
//                DD DSN=SYS1.VSF2LOAD,DISP=SHR
//                DD DSN=SDB.LOADLIB,DISP=SHR
//
