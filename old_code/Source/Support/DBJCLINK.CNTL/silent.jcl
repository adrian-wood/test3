//M12DBSIL JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(SILENT)
//*               ----------------------------------
//*
//* DESCRIPTION   : Makes load module SILENT consisting of FORTRAN
//*                 versions of routines TELLOPS, REPLYT and REPLYTU
//*                 which don't communicate with operators.
//*
//* SOURCE        : MDB.UTILITY.SRCE(SILENT)
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 15:05:48$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: silent.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 15:05:48    Brian Barwell   
//* $
//*--------------------------------------------------------------------
//* (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
//*
//* Met Office, United Kingdom.
//*
//* The use, duplication and disclosure of this code is strictly
//* prohibited without the permission of The Meteorological Database
//* Team at the above address.
//*--------------------------------------------------------------------
//*
//SILENT     EXEC FORT2CL,FPARMS='NOFIPS,OPT(3)',LPARMS=LET,OUTPUT=Q
//FORT.SYSIN   DD DSN=MDB.UTILITY.SRCE(SILENT),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(SILENT),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
