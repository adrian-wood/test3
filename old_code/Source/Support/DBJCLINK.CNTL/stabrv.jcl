//T12DHBLD JOB (OP1,DH,TCDS1B),HIRST.6955,PRTY=8,
//         MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12DH
//*
//*---------------------------------------------------------------------
//* PURPOSE       : TO REBUILD STABRV FROM SOURCE.
//*
//* $Revision: 1$
//* $Date: 30/01/2006 17:55:54$
//*---------------------------------------------------------------------
//*
//S1 EXEC FORT2CL,FPARMS='NOFIPS,DC(*)',
//        LPARMS='MAP,XREF,'
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(STABRV),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(STABRV),DISP=SHR,SPACE=,UNIT=
//*
//LKED.SDBLOAD DD DSN=SDB.LOADLIB,DISP=SHR
//LKED.SYSIN   DD *
         INCLUDE SDBLOAD(MDB)
/*
//
