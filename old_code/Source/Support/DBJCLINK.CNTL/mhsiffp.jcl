//M12DBIFP JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(MHSIFFP)
//*               -----------------------------------
//*
//* DESCRIPTION   : Builds load module for MHSIFFP, a modification of
//*                 version 3.0 of MHS routine MHSIFF changed to look
//*                 for data sets starting MHSP rather than MHSR. The
//*                 'delete' option is a dummy call which does nothing.
//*                 It does not need to be run from an 'authorised'
//*                 library.
//*
//*                 This build job is based on MHS.FTP.CNTL(ASMIFF)
//*                 (May 2007) but does not include module MHSCSI.
//*
//* SOURCE        : MDB.UTILITY.SRCE(MHSIFFP)
//*
//* REVISION INFO : $Revision: 1$ $Date: 08/06/2007 15:03:23$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: mhsiffp.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  1    Met_DB_Project 1.0         08/06/2007 15:03:23    Brian Barwell   
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
//ASM.SYSIN  DD DISP=SHR,DSN=MDB.UTILITY.SRCE(MHSIFFP)
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MHSIFFP),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
