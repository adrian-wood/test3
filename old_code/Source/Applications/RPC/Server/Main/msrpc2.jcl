//T12SNMKL JOB (M12,SN,WDS0BF),SHEILA.NEEDHAM,
// PRTY=8,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=0M
//*
//*
//* NB. Need to increase default region in your profile to run
//* this with 64M.  T12SN and T12DB have this increase.
//*
//* ====================================================================
//* JCL to make Version 3.0 of the RPC server
//* N.B we have two versions running together so check you're building
//* the right one and make the changes to COMPILE.USERLIB below.
//*
//* COMPILE.SYSIN   : Source to compile
//* COMPILE.USERLIB : Directory containing user include files
//* LKED.SYSLMOD    : Final executable
//*
//* LKED.SYSLIB     : SYS1.VSF2LOAD needed for fortran routines.
//*                 : SDB.LOADLIB contains assembler load ASMRPC
//* ====================================================================
//*
//S1 EXEC EDCCPL,CREGSIZ='64M',REGION=0M,
//   CPARM='RENT,SOURCE,MARGINS(1,*),MEMORY,LONGNAME,NOSEARCH,DLL(CBA)'
//*
//COMPILE.SYSIN   DD DSN=MCC3.DBSRCLIB(MSRPC2),DISP=SHR
//                DD DSN=MDB.RETRIEVL.CSRCE(PRINTCT),DISP=SHR
//                DD DSN=MDB.RETRIEVL.CSRCE(RPCLOG),DISP=SHR
//                DD DSN=MCC3.DBSRCLIB(CHKAUTH),DISP=SHR
//* The .h file must be called MSRPC, but there are two versions and
//* this build needs MSRPC2.  Copy and rename MSRPC2 to MSRPC on a
//* local library before submitting this job
//*
//* MPILE.USERLIB DD DSN=MDB.RETRIEVL.HSRCE,DISP=SHR
//COMPILE.USERLIB DD DSN=MCC3.SNRPC.H,DISP=SHR
//PLKED.SYSIN2    DD DSN=MCC3.SNPORT.CNTL(MDBDLLX),DISP=SHR
//LKED.SYSLMOD    DD DSN=TSTMDB.LOADLIB(MSRPC2),DISP=SHR,UNIT=
//LKED.SYSLIB     DD DSN=SYS1.SEZALNK2,DISP=SHR
//                DD DSN=SYS1.SEZARPCL,DISP=SHR
//                DD DSN=SYS1.SEZACMTX,DISP=SHR
//                DD DSN=SYS1.SAFHFORT,DISP=SHR
//                DD DSN=SYS1.SCEELKED,DISP=SHR
//
