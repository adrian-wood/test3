//M12DBUFC JOB (M12,DB,WDS0BF),METDB.6675,MSGCLASS=Q,PRTY=8
//*
//*---------------------------------------------------------------------
//*                  MCC3.DBJCLLIB.CNTL(MDBUFCHK)
//*                  ----------------------------
//*  Skeleton job to check the format and consistency of a set of BUFR
//* tables (i.e. Table B, Table D and Code & Flag Table) for MetDB use.
//* See comments in MDB.UTILITY.SRCE(CHEKTABL) for a summary of checks
//* done.
//*
//*  Before submitting, uncomment one of the sets of DD statements for
//* BUFR tables below and complete the data set name details (but don't
//* save your edits here!).
//*
//* REVISION INFORMATION
//*
//* $Workfile: mdbufchk.jcl$ $Folder: DBJCLLIB.CNTL$ $Author: Brian Barwell$
//* $Revision: 1$ $Date: 05/03/2008 09:36:33$
//*
//*--------------------------------------------------------------------
//* (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
//*
//* Met Office, United Kingdom.
//*
//* The use, duplication and disclosure of this code is strictly
//* prohibited without the permission of The Meteorological Database
//* Team at the above address.
//*--------------------------------------------------------------------
//*
//CHEKTABL EXEC FORT2CLG,FPARMS=NOFIPS,OUTPUT=Q
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(CHEKTABL),DISP=SHR
//*
//*   Uncomment ONE set of JCL below and supply full data set names
//*   ----------===------------------------------------------------
//*
//*    (1) Operational BUFR tables
//*
//*GO.TABLEB  DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.TABLEB
//*GO.TABLED  DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.TABLED
//*GO.CODEFIG DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.CODEFIGS
//*
//*    (2) New operational BUFR tables (specify dates in names)
//*
//*GO.TABLEB  DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.TABLEB.NEW?????
//*GO.TABLED  DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.TABLED.NEW?????
//*GO.CODEFIG DD LABEL=(,,,IN),DISP=SHR,DSN=SDB.BUFR.CODEFIG.NEW?????
//*
//*    (3) Your own BUFR tables (specify full data set names)
//*
//*GO.TABLEB  DD LABEL=(,,,IN),DISP=SHR,DSN= your Table B
//*GO.TABLED  DD LABEL=(,,,IN),DISP=SHR,DSN= your Table D
//*GO.CODEFIG DD LABEL=(,,,IN),DISP=SHR,DSN= your Codes/Flags
//
