//T12JNCHG JOB (M12,DB,WDS0BF),METDB.TEAM.6953,PRTY=??,
//         MSGCLASS=Q,MSGLEVEL=(1,1),NOTIFY=T12JN
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//* TITLE         : CHGJCL
//*
//* PURPOSE       : TO REBUILD CHGJCL APPLICATION FROM SOURCE.
//*                  (MDB CHASER JCL EDITING)
//*
//* INTRODUCED    : SEPTEMBER 1998 BY JOHN NORTON (CC3)
//*
//* CHANGE RECORD :
//*
//*  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
//*
//*
//S1 EXEC FORT2CL,FPARMS='CHARLEN(28000),NOFIPS,DC(*)',
//        LMAP=MAP,LXREF=XREF
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(CHGJCL),DISP=SHR
//*
//LKED.SYSLMOD DD DSN=MCC3.**.LOAD(CHGJCL),DISP=OLD,SPACE=,UNIT=
//*
//METPL DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
   INCLUDE METPL(ZPDATE)
   ENTRY   CHGJCL
/*
//
