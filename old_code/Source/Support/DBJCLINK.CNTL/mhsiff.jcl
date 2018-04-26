//M12DBIFF JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(MHSIFF)
//*               ----------------------------------
//*
//* DESCRIPTION   : Makes load module for version 3.0 of MHS routine
//*                 MHSIFF (preserved on MCC3.DB.LOAD because it does
//*                 not need to be run from an 'authorised' library).
//*                 This build job is based on MHS.FTP.CNTL(ASMIFF)
//*                 (May 2007) but does not include module MHSCSI.
//*
//* SOURCE        : MDB.UTILITY.SRCE(MHSIFF)
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 14:55:35$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: mhsiff.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 14:55:35    Brian Barwell   
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
//ASMHSIFF    EXEC ASMACL,
//   ASMPARM='TEST,XREF(SHORT),SYSPARM(215)',
//   LNKPARM='LIST,LET,MAP,XREF,NCAL,AC=1,AMODE=31,RMODE=24'
//ASM.SYSIN  DD DISP=SHR,DSN=MDB.UTILITY.SRCE(MHSIFF)
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MHSIFF),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
