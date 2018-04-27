//M12DBLST JOB (M12,DB,WDS0BF),MDBTEAM.X6955,MSGCLASS=Q,PRTY=8
//*
//*---------------------------------------------------------------------
//*       PRINTS INDEX ENTRY DETAILS FOR AN MDB STORAGE DATA SET
//*       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//*   The storage data set must be in the old format in use before 2000
//*  (see MetDB Technical Note 2).
//*
//* Instructions for use:
//* ~~~~~~~~~~~~~~~~~~~~~
//*  (1) Specify the name of the data set on the "//GO.MDBSTORE DD"
//*      statement below.
//*
//*  (2) Change job card details if necessary and submit the job.
//*
//*  (3) CANCEL rather than SAVE your changes to this member.
//*
//* Notes
//* ~~~~~
//*  - The printed times of receipt are in minutes after the index time.
//*
//*  - Some data sets have large numbers of index entries resulting
//*    in a correspondingly large amount of output. E.g. running this
//*    job on "MDB.AMDARS" produces over a million lines of output.
//*
//*  - For more details, see comments in "MDB.UTILITY.SRCE(MDBXLIST)".
//*
//*---------------------------------------------------------------------
//* $Workfile: mdbxlist.jcl$ $Folder: DBJCLLIB.CNTL$
//* $Revision: 1$ $Date: 25/04/2008 16:24:12$
//*
//* $Log:
//*  1    Met_DB_Project 1.0         25/04/2008 16:24:12    Brian Barwell   Job
//*        to print index entries in old-format storage data set.
//* $
//*---------------------------------------------------------------------
//*
//INDEXLST EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(27998)',
//         TIME=(0,10),OUTPUT=Q
//*
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(MDBXLIST),DISP=SHR
//           DD DSN=MDB.SOURCE(ICHAR2),DISP=SHR
//*
//GO.FT06F001 DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=133,BLKSIZE=10640)
//*
//*                                                    Storage data set
//GO.MDBSTORE DD LABEL=(,,,IN),DISP=SHR,DSN=your data set
//
