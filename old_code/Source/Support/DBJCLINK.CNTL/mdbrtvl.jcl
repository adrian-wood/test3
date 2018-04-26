//M12DBRTV JOB (OP1,DB,WDS0BF),MDB.TEAM.6675,MSGCLASS=Q,PRTY=8
//*---------------------------------------------------------------------
//*               BUILD JOB FOR MCC3.DB.LOAD(MDBRTVL)
//*               -----------------------------------
//*
//* DESCRIPTION   : Makes load module MDBRTVL for retrieval of data
//*                 from the MetDB and display in tabulated form.
//*
//* SOURCE        : MDB.UTILITY.SRCE(MDBRTVL)
//*
//* REVISION INFO : $Revision: 2$ $Date: 21/04/2010 14:11:54$
//*                 $Folder: DBJCLINK.CNTL$ $Workfile: mdbrtvl.jcl$
//*
//* CHANGE RECORD :
//*
//* $Log:
//*  2    Met_DB_Project 1.1         21/04/2010 14:11:54    Brian Barwell   Add
//*        new subroutine GETNAME.
//*  1    Met_DB_Project 1.0         08/06/2007 14:53:29    Brian Barwell   
//* $
//*--------------------------------------------------------------------
//* (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
//*
//* Met Office, United Kingdom.
//*
//* The use, duplication and disclosure of this code is strictly
//* prohibited without the permission of The Meteorological Database
//* Team at the above address.
//*--------------------------------------------------------------------
//*
//MDBRTVL EXEC FORT2CL,FPARMS='NOFIPS,CHARLEN(3034),DC(*),OPT(3)',
//        LPARMS=NCAL,OUTPUT=Q
//*
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(MDBRTVL),DISP=SHR
//           DD DSN=MDB.UTILITY.SRCE(GETNAME),DISP=SHR               !2
//*
//LKED.SYSLMOD DD DSN=MCC3.DBLOAD.D??????(MDBRTVL),
//             UNIT=,SPACE=,DCB=,DISP=OLD
//
