//MDBMDBX JOB (M12,DB,WDS0BF),METDB.6675,PRTY=8,MSGCLASS=Q
//*---------------------------------------------------------------------
//*                    MCC3.DBJCLINK.CNTL(MDBX)
//*                    ~~~~~~~~~~~~~~~~~~~~~~~~
//* Job to build MDBX load module. Operational module is on SDB.LOADLIB.
//* @Revision: @ @Date: @
//*---------------------------------------------------------------------
// EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(2000),DC(*)'                !1.3
//SYSIN DD DSN=MDB.MERGE.SRCE(MDBX),DISP=SHR
//      DD DSN=MDB.MERGE.SRCE(MDBXREQ),DISP=SHR
//      DD DSN=MDB.MERGE.SRCE(MDBXDAY),DISP=SHR
//      DD DSN=MDB.MERGE.SRCE(ONEHOUR),DISP=SHR
//      DD DSN=MDB.SOURCE(IVALUE),DISP=SHR
//      DD DSN=MDB.SOURCE(SORTR),DISP=SHR
//      DD DSN=MDB.SOURCE(SORTN),DISP=SHR
//LKED.SYSLMOD DD DSN=???????????(MDBX),DISP=SHR,SPACE=,UNIT=
//LKED.DB DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.PR DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *                                                  !1.3
    INCLUDE DB(MDB)
    INCLUDE PR(ZPDATE)
    ENTRY MDBX
/*
//
