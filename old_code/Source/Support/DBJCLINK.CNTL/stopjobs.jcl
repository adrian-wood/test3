//M12DBSTP JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(STOPJOBS)
//*               ------------------------------------
//*
//* DESCRIPTION   : Makes load module STOPJOBS to terminate a test
//*                 MetDB storage run. See MetDB Technical Note 14,
//*                 section 5.6 for details.
//*
//* SOURCE        : MDB.UTILITY.SRCE(STOPJOBS)
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 15:09:43$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: stopjobs.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 15:09:43    Brian Barwell   
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
//STOPJOBS EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(950),DC(*),OPT(3)'
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(STOPJOBS),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(STOPJOBS),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
