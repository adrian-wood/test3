//M12DBIF2 JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(MHSIFF2)
//*               -----------------------------------
//*
//* DESCRIPTION   : Builds load module MHSIFF2, a FORTRAN version of
//*                 MHS routine MHSIFF for use in MetDB storage jobs
//*                 running with 'MODE=2'. See MetDB Technical Note 14,
//*                 section 5.3.1 for details.
//*
//* SOURCE        : MDB.UTILITY.SRCE(MHSIFF2)
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 15:02:12$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: mhsiff2.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 15:02:12    Brian Barwell   
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
//MHSIFF EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(950),OPT(3)'
//FORT.SYSIN   DD DSN=MDB.UTILITY.SRCE(MHSIFF2),DISP=SHR
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MHSIFF2),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
