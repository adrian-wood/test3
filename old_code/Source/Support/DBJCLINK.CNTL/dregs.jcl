//M12BCDRG JOB (OP1,BC,WDS0BF),MDB.TEAM.6951,MSGCLASS=Q,PRTY=??
//*
//*---------------------------------------------------------------------
//*               BUILDS MET.D.B. LOAD MODULE ("DREGS")
//*               -------------------------------------
//*    MAKES LOAD MODULE "DREGS" FOR COLLECTING DREGS MESSAGES FROM
//*   INDIVIDUAL STORAGE JOBS INTO THE MAIN DREGS DATA SET.
//*
//* BEFORE RUNNING:
//* ---------------
//*      1.  CHECK DETAILS OF "LKED.SYSLMOD" DD STATEMENT.
//*      2.  CHECK JOB STATEMENT DETAILS.
//*
//*                           BRIAN BARWELL,  IT(DD),  30 AUGUST 2000.
//*---------------------------------------------------------------------
//*
//DYNALLOC EXEC ASMAC
//ASM.SYSIN DD DSN=MDB.SOURCE(DYNALLOC),DISP=SHR
//*
//DREGS EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(950),OPT(3)',OUTPUT=Q
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(DREGS),DISP=SHR
//*
//LKED.SYSLMOD DD DISP=SHR,UNIT=,SPACE=,DCB=,
//             DSN=MCC3.DBLOAD.D180900(DREGS)
//
