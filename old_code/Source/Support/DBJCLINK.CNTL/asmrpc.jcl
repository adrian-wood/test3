//T12SCMKL JOB (M12,SC,WDS0BF),SIMON.ROOMG08.X6955,
// PRTY=8,NOTIFY=T12SC,MSGCLASS=Q,MSGLEVEL=(1,1),REGION=4M
//*
//* ====================================================================
//* JCL to make the MetDB RPC assembler load ASMRPC - S.Cox
//*
//* Library CEE.SCEEMAC is needed for MACROS EDCPRLG, EDCEPIL
//*
//* SYSIN           : Source to compile
//* SYSLIB          : Directory containing MACROS (see above). The
//*                 : empty DD is required!!
//* LKED.SYSLMOD    : Final executable
//*
//* LKED.MDB        : library to link in MDB shell from
//* LKED.SYSIN      : MDB shell
//* ====================================================================
//*
// EXEC ASMACL
//SYSIN  DD DSN=MDB.SOURCE(ASMRPC),DISP=SHR
//SYSLIB DD
//       DD DSN=CEE.SCEEMAC,DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.RPC(ASMRPC),DISP=SHR,SPACE=,
//  UNIT=DISK
//LKED.MDB     DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN   DD *
  INCLUDE MDB(MDB)
  ENTRY MDB
/*
//
