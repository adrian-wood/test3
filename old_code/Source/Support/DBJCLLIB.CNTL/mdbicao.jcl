//M12DBICO JOB (M12,DB,WDS0BF),METDB.6675,MSGCLASS=Q,PRTY=8
//*
//*---------------------------------------------------------------------
//*                  MCC3.DBJCLLIB.CNTL(MDBICAO)
//*                  ----------------------------
//*  Skeleton job to check an ICAO list for entries out of alphabetical
//* order or duplicated. ICAO list must be in the new format introduced
//* in October 2010.
//*
//*  Before submitting, supply the name of your ICAO list on the DD
//* statement "//GO.STNICAO" (but don't save your edits here!).
//*
//* REVISION INFORMATION
//*
//* $Workfile: mdbicao.jcl$ $Folder: DBJCLLIB.CNTL$
//* $Revision: 1$ $Date: 07/10/2010 12:22:03$
//*
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
//CHEKICAO EXEC FORT2CLG,FPARMS=NOFIPS
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(CHEKICAO),LABEL=(,,,IN),DISP=SHR
//*
//*       Enter the name of your ICAO list data set below
//*
//*                                        <--------------- ... >
//GO.STNICAO DD LABEL=(,,,IN),DISP=SHR,DSN= your ICAO list
//
