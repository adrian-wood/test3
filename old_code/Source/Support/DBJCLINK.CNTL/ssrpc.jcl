//T12SCMKL JOB (M12,SC,WDS0BF),SIMON.ROOMG08.X6955,
// PRTY=8,NOTIFY=T12SC,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=4M
//*
//* ====================================================================
//* JCL to make the MetDB RPC single-user server - S.Cox
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
//* debug compile step below.
//*
//* S1 EXEC EDCCPL,
//*   CPARM='RENT,SOURCE,MARGINS(1,*),MEMORY,LONGNAME',
//*   CPARM2='DEFINE(DEBUG=)'
//*
//COMPILE.SYSIN   DD DSN=MDB.RETRIEVL.CSRCE(SSRPC),DISP=SHR
//COMPILE.USERLIB DD DSN=MDB.RETRIEVL.HSRCE,DISP=SHR
//LKED.SYSLMOD    DD DSN=MCC3.SCLOAD(RPCTEST),DISP=SHR
//*
//* Link Step: Note the TCPIP and &LIBPRFX libraries will soon be
//*            part of the EDCCPL procedure, so won't be needed here
//*
//LKED.SYSLIB     DD DSN=TCPIP.SEZALINK,DISP=SHR
//                DD DSN=TCPIP.SEZARPCL,DISP=SHR
//                DD DSN=TCPIP.SEZACMTX,DISP=SHR
//                DD DSN=&LIBPRFX..SCEELKED,DISP=SHR
//                DD DSN=SYS1.VSF6LOAD,DISP=SHR
//                DD DSN=SDB.LOADLIB,DISP=SHR
//
