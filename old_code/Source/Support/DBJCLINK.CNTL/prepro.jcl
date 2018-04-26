//M12DBPRE JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*
//*=====================================================================
//*                BUILD JOB FOR MDB.LOADLIB(PREPRO)
//*                ---------------------------------
//*
//* DESCRIPTION   : Makes load module PREPRO for pre-processing MetDB
//*                 source code.
//*
//* SOURCE        : MDB.UTILITY.SRCE(PREPRO)
//*
//* REVISION INFO : $Workfile: prepro.jcl$ $Folder: DBJCLINK.CNTL$
//*                 $Revision: 1$ $Date: 20/05/2008 10:30:11$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         20/05/2008 10:30:11    Brian Barwell
//*       Build job for new MetDB source pre-processor.
//* $
//*=====================================================================
//*
//PREPRO EXEC FORT2CL,FPARMS='NOFIPS,OPT(3)',OUTPUT=Q
//*
//FORT.SYSIN DD DSN=MDB.SOURCE(PREPRO),DISP=SHR
//*
//*                                Insert load module destination below
//*
//LKED.SYSLMOD DD DISP=OLD,UNIT=,SPACE=,DCB=,
//             DSN=MCC3.DBLOAD.Dyymmdd(PREPRO)
//
