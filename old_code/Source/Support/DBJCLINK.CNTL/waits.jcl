//M12DBWAI JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*        BUILD JOB FOR MCC3.DB.LOAD MODULES WAIT3 AND WAIT5
//*        --------------------------------------------------
//*
//* DESCRIPTION   : Makes load modules WAIT3 and WAIT5 for use in
//*                 integration tests. These are FORTRAN versions of
//*                 REPLYTU (as in module SILENT) but with a delay of
//*                 3 seconds (WAIT3) or 5 seconds (WAIT5).
//*
//* SOURCE        : MDB.UTILITY.SRCE members WAIT3 and WAIT5.
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 15:14:24$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: waits.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 15:14:24    Brian Barwell   
//* $
//*--------------------------------------------------------------------
//* (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
//*
//* Met Office, United Kingdom.
//*
//* The use, duplication and disclosure of this code is strictly
//* prohibited without the permission of The Meteorological Database
//* Team at the above address.
//*---------------------------------------------------------------------
//WAIT3      EXEC FORT2CL,FPARMS='NOFIPS,OPT(3)',LPARMS=NCAL,OUTPUT=Q
//FORT.SYSIN   DD DSN=MDB.UTILITY.SRCE(WAIT3),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(WAIT3),
//             UNIT=,DCB=,SPACE=,DISP=SHR
//*--------------------------------------------------------------------
//WAIT5      EXEC FORT2CL,FPARMS='NOFIPS,OPT(3)',LPARMS=NCAL,OUTPUT=Q
//FORT.SYSIN   DD DSN=MDB.UTILITY.SRCE(WAIT5),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(WAIT5),
//             UNIT=,DCB=,SPACE=,DISP=SHR
//
