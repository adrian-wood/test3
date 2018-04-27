//M12DBCLR JOB (M12,DB,WDS0BF),MDBTEAM.X6955,MSGCLASS=Q,PRTY=??
//*
//*---------------------------------------------------------------------
//*      CLEARS DATA FROM INDEX PERIODS IN AN MDB STORAGE DATA SET
//*      ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//*   The storage data set must be in the new format devised for
//*  satellite data in 2000 (see Appendix C of MetDB Technical Note 14).
//*  (These data sets would have '2' coded in the 'LIST' column of a
//*  Retrieval Table.)
//*
//* Instructions for use:
//* ~~~~~~~~~~~~~~~~~~~~~
//*  (1) Browse the data set in hex and note the record number of the
//*      first (or only) index record to be cleared and its index time.
//*
//*  (2) If clearing a range of consecutive index periods, repeat (1)
//*      for the last index record to be cleared.
//*
//*  (3) Terminate any storage jobs using the data set you want to
//*      clear data from.
//*
//*  (4) Specify the name of the data set on the "//GO.MDBSTORE DD"
//*      statement below.
//*
//*  (5) In the "/GO.SYSIN DD" input below, fill in the record number
//*      and time from (1) in the variables IREC1 and ITIME1.
//*
//*  (6) If you are clearing a range of index periods, do the same with
//*      variables IREC2 and ITIME2 using data from (2); otherwise
//*      delete the line containing IREC2 and ITIME2.
//*
//*  (7) Change job card details if necessary and submit the job.
//*
//*  (8) CANCEL rather than SAVE your changes to this member.
//*
//* Notes
//* ~~~~~
//*  - Specifying both record numbers and index times is just to
//*    prevent typing errors from clearing the wrong data.
//*
//*  - IREC2 may be less than IREC1.
//*
//*  - If any problem or inconsistency is found, the job prints an
//*    error message and abandons processing. Otherwise it prints a
//*    one-line message for each index period processed saying
//*    whether it was cleared or not.
//*
//*  - For more details, see comments in "MDB.UTILITY.SRCE(MDBXCLR)".
//*
//*---------------------------------------------------------------------
//* $Revision: 1$ $Date: 28/02/2006 12:18:40$
//*---------------------------------------------------------------------
//*
// EXEC FORT2CLG,FPARMS='NOFIPS,CHARLEN(27998),DC(*)',OUTPUT=Q
//*
//FORT.SYSIN DD DSN=MDB.UTILITY.SRCE(MDBXCLR),DISP=SHR
//           DD DSN=MDB.SOURCE(CHAR2),DISP=SHR        (Used by CLRMAP)
//           DD DSN=MDB.SOURCE(CLRMAP),DISP=SHR
//           DD DSN=MDB.SOURCE(IOBLKS),DISP=SHR
//*
//LKED.METPROG DD DSN=MET.PROGLIB,DISP=SHR
//LKED.SYSIN DD *
  INCLUDE METPROG(ZPDATE)
/*
//*                                                    Storage data set
//GO.MDBSTORE DD DISP=SHR,DSN=your.data.set
//*                                                      NAMELIST input
//GO.SYSIN DD *
 &CLEAR IREC1 = ??,  ITIME1 = yyyy, mm, dd, hh, mm,
        IREC2 = ??,  ITIME2 = yyyy, mm, dd, hh, mm,
 &END
/*
//
