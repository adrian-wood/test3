//M12DBNDX JOB (M12,DB,WDS0BF),MDBTEAM.X6955,MSGCLASS=Q,PRTY=8
//*
//*---------------------------------------------------------------------
//*       PRINTS INDEX ENTRY DETAILS FOR AN MDB STORAGE DATA SET
//*       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//*   The storage data set must be in the new format devised for
//*  satellite data in 2000 (see Appendix C of MetDB Technical Note 14).
//*  (These data sets would have '2' coded in the 'LIST' column of a
//*  Retrieval Table.)
//*
//* Instructions for use:
//* ~~~~~~~~~~~~~~~~~~~~~
//*  (1) Specify the name of the data set on the "//MDBSTORE DD"
//*      statement below. Two forms are given; for MVS or ZFS d/s
//*
//*  (2) Specify the record length of the dataset in the &INPUT
//*      namelist below.
//*
//*  (3) Change job card details if necessary and submit the job.
//*
//*  (3) CANCEL rather than SAVE your changes to this member.
//*
//* Notes
//* ~~~~~
//*  - The printed times of receipt are in minutes after the index time.
//*
//*  - Some data sets have large numbers of index entries resulting in
//*    a correspondingly large amount of output. E.g. "MDB.SSMI"
//*    produces about 330,000 lines of output.
//*
//*  - For more details, see comments in "MDB.UTILITY.SRCE(MDBXPRNT)".
//*
//*---------------------------------------------------------------------
//* $Revision: 3$ $Date: 21/11/2011 09:39:44$
//*---------------------------------------------------------------------
//*
//INDEXPRT EXEC PGM=MDBXPRNT
//STEPLIB DD DSN=MCC3.DB.LOAD,DISP=SHR
//FT06F001 DD SYSOUT=Q,DCB=(RECFM=FB,LRECL=133,BLKSIZE=10640)
//INPUT DD *
 &INPUT LENREC=27998 /
/*
//*                                MVS Storage data set
//MDBSTORE DD LABEL=(,,,IN),DISP=SHR,DSN=MDB.AATSR
//*                                ZFS Storage data set
//* MDBSTORE DD PATH='/BPRD/var/mdb/data/AIRSWF/MDB.AIRSWF',
//*          PATHOPTS=(ORDONLY)
//
