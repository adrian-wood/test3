      SUBROUTINE RTABLE (CTYPE, IRM, IAREA, RNAME, LTEST, IETIME,
     &                   CDAT, IDATA, LIST, IERR, CERR, MSTREAM,   !2.4b
     &                   ELIST)                                    !2.6b

!-----------------------------------------------------------------------
!
! ROUTINE     : RTABLE
!
! PURPOSE     : To look up an entry in the retrieval table based on
!               the user's retrieval request and return details of a
!               suitable storage (or other) data set to access.
!
! DESCRIPTION : RTABLE looks up the retrieval table for a data set or
!               archive series matching the data subtype and model area
!               specified by the user.  (The retrieval table is read in
!               if this has not already been done.) It checks the start
!               and end times of each entry estimating suitable times
!               for rolling data sets, and selects one which includes
!               the users start time (or, if not available, one with
!               the earliest data after this time).  If it selects an
!               archive series, it will identify the first data set to
!               look at; then it updates the user's time window if
!               necessary and returns all the information needed about
!               the data set.
!
!               Some table entries are for data sets other than storage
!               data sets: for these, no selection is required other
!               than 'subtype' and the user's time window is not used.
!
! USAGE       : CALL RTABLE (CTYPE, IRM, IAREA, RNAME, LTEST, IETIME,
!                            CDAT, IDATA, LIST, IERR, CERR, MSTREAM,
!                            ELIST)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!               NAME   I/O TYPE        DESCRIPTION
!               -----  --- ----        -----------
!               CTYPE   I  C*8   Data type code to look up in table.
!               IRM     I   I    Preferred data (1=raw, 2=merged).
!               IAREA   I   I    Required model/area (e.g. 1=global,
!                                6=strat., 9=SST, 15=mesoscale).
!               RNAME   I C*(*)  User's retrieval table name ('DDICT').
!               LTEST   I   L    Flag for diagnostic printout.
!               IETIME I/O  I    (9-element array) User's time window
!                                (start y,m,d,h, end y,m,d,h, increment)
!               CDAT    O C*(*)  Data set name from retrieval table.
!               IDATA   O   I    (5-element array) Data set details:
!                                   1  length of data set name,
!                                   2  data set record length,
!                                   3  required ft number (=84),
!                                   4  no. of associated data set list,
!                                   5  medium (1=disk, 2=tape).
!               LIST    O  C*8   Member name of list of element names
!               IERR    O   I    Error code (0=last batch of data, 4=
!                                more data, 8=no data, 16=fatal error)
!               CERR    O C*(*)  Error message text (up to 40 chars.)
!               MSTREAM O C*3    MASS stream. (minus the MDB prefix)
!               ELIST   O  C*8   Member name of element index.
!
! CALLED BY   : MDB, MDBALC, READLIST
!
! CALLS       : DATE13, DATE31, DATIM
!
! HISTORY     : Original version by Brian Barwell, April 2002.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/rtable.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:03    Sheila Needham  
! $
! Revision 2.8  2005/03/03 09:20:15  usmdb
! 2.8.  21 March 2005.  Brian Barwell.
! 'LMAX' increased from 500 to 600 to allow bigger arrays.
!
! Revision 2.7  2005/02/07 15:25:03 15:25:03 usmdb (MetDB account c/o J C Ward)
! 2.7.  21 February 2005.  Brian Barwell.  Remedy INC133929.
! Modify start time of on-line unmerged data set even if the
! only archived data is from the merged data set.
!
! Revision 2.6  2003/05/06 07:20:28 07:20:28 usmdb (MetDB account c/o J C Ward)
! 2.6b Read the element index member name from the retrieval table
! and pass it back to the calling program - S.Cox.
!
! Revision 2.5  2003/04/10 13:33:26  usmdb
! call MDB_DATIM on unix platforms - S.Cox
!
! Revision 2.4  2003/02/03  15:19:37  15:19:37  usmdb (MetDB account c/o usjh)
! 2.4  Allow the retrieval table location to be specified in the
!      environment variable METDB_RTABLE. Changed the layout of
!      the 1 /non-1  code - S.Cox
! 2.4b Changes made to read the MASS stream from the retrieval
!      table and pass it back to the calling program - S.Cox
!
! Revision 2.3  2002/06/10  15:06:59  15:06:59  usmdb (MetDB account c/o usjh)
! 2.3.  17 June 2002.  Brian Barwell.  Change 42/02.
! Correct upper limit of search for entry with required model data.
!
! Revision 2.2  2002/05/23  08:11:50  08:11:50  usmdb (Generic MetDB account)
! Correction made so that off-line retrievals work for those
! 24-hr datasets not starting at 0000Z - B.Barwell
!
! Revision 2.1  2002/05/21 12:33:56  usmdb
! Ensure non-zero time interval - B.Barwell
!
! Revision 2.0  2002/05/07 09:06:24  usmdb
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters
      INTEGER LDSN         ! Maximum length of data set name
      INTEGER LMAX         ! Maximum number of lines of data in table
      PARAMETER (LDSN=120) ! Big enough for HP
      PARAMETER (LMAX=600) ! Increased from 500 for NAE merges      !2.8

!-----------------------------------------------------------------------
!     Variables other than retrieval table data
!-----------------------------------------------------------------------

      INTEGER I             ! Position in skeleton data set name
      INTEGER IAREA         ! Indicator for model area
      INTEGER IAREA1,IAREA2 ! Range of lines in table for CTYPE & IAREA
      INTEGER IATIM1(2)     ! Times of oldest on-line raw & merged data
      INTEGER IATIM2(2)     ! Times of latest off-line raw & merged data
      INTEGER IBEST         ! Best ISCORE so far
      INTEGER IDATA(5)      ! Data set information (see above)
      INTEGER IDAY          ! Day for skeleton data set name        !2.2
      INTEGER IDMIN         ! Data set time period in minutes
      INTEGER IDTIM1,IDTIM2 ! Data set start & end times (century mins.)
      INTEGER IERR          ! Error code (see above)
      INTEGER IETIME(9)     ! Amended user's start & end times
      INTEGER IMONTH        ! Month for skeleton data set name      !2.2
      INTEGER INEW          ! Local storage of period end time
      INTEGER IOLD          ! Local storage of period start time
      INTEGER IRM           ! Preferred data (1=raw, 2=merged)
      INTEGER ISCORE        ! Score for matching dataset periods
      INTEGER ISTAT         ! Status code from I/O operation
      INTEGER IUTIM1,IUTIM2 ! User's start & end times (century mins.)
      INTEGER IXTIM         ! Period expiry time (century mins.)
      INTEGER IYEAR         ! Year for skeleton data set name       !2.2
      INTEGER J             ! General loop variable
      INTEGER LASTAREA      ! Value of IAREA from last call
      INTEGER LBEST         ! Best data available (line number in table)
      INTEGER LENDSN        ! Length of data set name
      INTEGER LINE          ! Pointer to line in data set table
      INTEGER LINES         ! Number of lines of data in data set table
      INTEGER LINE1, LINE2  ! Range of lines in data table for CTYPE
      INTEGER MINUTES       ! Time in century minutes
      INTEGER MWANT         ! Type of data wanted (raw=1, merged=2)
      INTEGER NDAY          ! Century day number
      INTEGER NOW(8)        ! Current date and time (from DATIM)
      INTEGER NOWLINE(2)    ! Line numbers for on-line raw & merged data
      INTEGER NOWMIN        ! Current date and time (century minutes)

      LOGICAL FIRST         ! .TRUE. if first call to subroutine
#if defined (MVS)
      LOGICAL FOUND         ! .TRUE. if data set found by INQUIRE
#endif
      LOGICAL LTEST         ! .TRUE. if diagnostic printout required
      LOGICAL PREFER        ! .TRUE. if preferred period

      CHARACTER*(*)    CDAT     ! Data set name from retrieval table
      CHARACTER*(*)    CERR     ! Text of error or warning message
      CHARACTER*8      CTYPE    ! Data type code to look up in table
      CHARACTER*1      MPREFER  ! Preferred data ('R'=Raw, 'M'=Merged)
      CHARACTER*(LDSN) RDEFAULT ! Default retrieval table name
      CHARACTER*(*)    RNAME    ! User's retrieval table name ('DDICT')
      CHARACTER*80     FORMAT   ! Format for reading retrieval table
      CHARACTER*132    HEAD     ! Revision information
      CHARACTER*8      LASTTYPE ! Value of CTYPE from last call
      CHARACTER*8      LIST     ! Member name of list of element names
      CHARACTER*2      RM       ! 'RM' (Raw and Merged data flags)
      CHARACTER*3      MSTREAM  ! MASS stream (minus MDB prefix)   !2.4b
      CHARACTER*8      ELIST    ! Member name of element index     !2.6b

#if ! defined (MVS)
      LOGICAL       FEXIST          ! .TRUE. if data set exists     !2.4
      CHARACTER*200 METDB_BASE_DIR  ! METDB_BASE_DIR PATH
      CHARACTER*200 METDB_RTABLE    ! RTABLE PATH                   !2.4
#endif

!-----------------------------------------------------------------------
!     Variables holding data read in from retrieval table
!-----------------------------------------------------------------------

      INTEGER IPAREA(LMAX)  ! Data area code for table entry
      INTEGER IPTIM1(LMAX)  ! Data period start time (century minutes)
      INTEGER IPTIM2(LMAX)  ! Data period end time (century minutes)
      INTEGER IT1(5),IT2(5) ! Period start & end times (y,m,d,h,m)
      INTEGER KEEPDAYS(LMAX)! Data retention periods (days)
      INTEGER LENREC(LMAX)  ! Data set record lengths (=block sizes)
      INTEGER LISTDS(LMAX)  ! Code number for associated data set list
      INTEGER MEDIUM(LMAX)  ! Data medium (disk=1, tape=2)
      INTEGER NDXMIN(LMAX)  ! Numbers of munutes per index period
      INTEGER NDXREC(LMAX)  ! Numbers of index periods per data set

      LOGICAL SKELFLAG(LMAX) ! .TRUE. if data set name is a skeleton

      CHARACTER*(LDSN) DSN(LMAX) ! Data set names
      CHARACTER*8 ELEMLIST(LMAX) ! Locations of lists of element names
      CHARACTER*1    MFLAG(LMAX) ! Raw/Merged ('R'/'M') data indicators
      CHARACTER*8     TYPE(LMAX) ! Data type names (e.g. 'ATOVSG')
      CHARACTER*3   STREAM(LMAX) ! MASS stream (minus MDB prefix)  !2.4b
      CHARACTER*8 ELEMINDX(LMAX) ! Locations of lists of elemidx   !2.6b

!-----------------------------------------------------------------------
! COMMON, SAVE and DATA statements
!-----------------------------------------------------------------------

!                               COMMON block (for dynamical allocation)
      COMMON /COMTBL/
     &  IPAREA, IPTIM1, IPTIM2, KEEPDAYS, LENREC, LISTDS, MEDIUM,
     &  NDXMIN, NDXREC, DSN, ELEMLIST, TYPE, MFLAG, SKELFLAG,
     &  STREAM, ELEMINDX                                           !2.6b

!                                                       Saved variables
      SAVE FIRST, LINES, LINE1, LINE2, IAREA1,IAREA2
      SAVE LASTAREA, LASTTYPE, RM
!                                                   Data initialisation
      DATA FIRST/.TRUE./, RM/'RM'/, FORMAT/' '/
#if defined (MVS)
      DATA RDEFAULT/'/MDB.RETRIEVL.TABLE'/
#else
      DATA RDEFAULT/'MDB.RETRIEVL.TABLE.HPMDB'/
#endif
      DATA LASTTYPE/' '/, LASTAREA/-1/

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

      IERR     = 0     ! No errors yet
      CERR     = ' '   ! No error message yet
      CDAT     = ' '   ! No data set name found yet
      IDATA(1) = 0     ! Data set name length not yet available
      IDATA(4) = 0     ! Associated data set code not yet available

      IF (LTEST) WRITE (*,'(/A/A)') ' In MetDB subroutine RTABLE',
     &                              ' =========================='

!-----------------------------------------------------------------------
! Get current date & time (NOW). Compute current century minute (NOWMIN)
!-----------------------------------------------------------------------

#if defined (MVS)
      CALL DATIM (NOW)
#else
      CALL METDB_DATIM (NOW)                                        !2.5
#endif
      CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
      NOWMIN = 1440*(NDAY-1) + 60*NOW(5) + NOW(4)

!=======================================================================
!   IF THIS IS THE FIRST CALL, READ THE RETRIEVAL DATA SET DIRECTORY
!=======================================================================

      IF (FIRST) THEN
!                                              Set revision information
        HEAD = '$RCSfile: rtable.F,v $ ' //
     &         '$Revision: 1$ $Date: 30/01/2006 20:24:03$'

#if defined (MVS)
!-----------------------------------------------------------------------
! Open the retrieval data set table: 1  code.                      !2.4
! There are 3 options with regards to opening the retrieval table:
! 1) User Specified via DDICT keyword
! 2) Specified in JCL as FT80F001
! 3) Not specified at all - open default retrieval table.
!-----------------------------------------------------------------------

        IF (RNAME(2:2).NE.' ') THEN   !  User-supplied via DDICT keyword
          OPEN (80, FILE=RNAME(1:), IOSTAT=ISTAT, ACTION='READ')
        ELSE
          INQUIRE (FILE='FT80F001', EXIST=FOUND)
          IF (FOUND) THEN      ! User-supplied via FT80F001 DD statement
            OPEN (80, IOSTAT=ISTAT, ACTION='READ')
          ELSE                          ! Not user-supplied, use default
            OPEN (80, FILE=RDEFAULT(1:), IOSTAT=ISTAT, ACTION='READ')
          ENDIF
        ENDIF

#else
!-----------------------------------------------------------------------
! Open the retrieval data set table: non-MVS code.                  !2.4
! There are 3 options with regards to opening the retrieval table:
! 1) User Specified in environemnt variable METDB_RTABLE
! 2) User Specified via DDICT keyword
! 3) Not specified at all - open default retrieval table.
!-----------------------------------------------------------------------

! 1) User Specified in environemnt variable METDB_RTABLE

        CALL METDB_GETENV('METDB_RTABLE',METDB_RTABLE,ISTAT)
        IF (ISTAT.EQ.0) THEN    ! User-supplied via environment variable
          J=LEN(METDB_RTABLE)   ! Find the length of METDB_RTABLE
          DO WHILE (METDB_RTABLE(J:J).EQ.' ')
            J=J-1
          ENDDO
          INQUIRE (FILE=METDB_RTABLE(1:J),EXIST=FEXIST)
          IF (.NOT.FEXIST) THEN
            WRITE(6,*)'RTABLE: ERROR: File not found: ',
     &      METDB_RTABLE(1:J)
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF
          OPEN (80,FILE=METDB_RTABLE(1:J),IOSTAT=ISTAT)
          IF (ISTAT.NE.0) THEN
            WRITE(6,*)'RTABLE: ERROR: Could not open file ',
     &                METDB_RTABLE(1:J)
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF

! 2) User Specified via DDICT keyword

        ELSEIF (RNAME(2:2).NE.' ') THEN    ! User-supplied DDICT keyword
          J=LEN(RNAME)   ! Find the length of RNAME
          DO WHILE (RNAME(J:J).EQ.' ')
            J=J-1
          ENDDO
          INQUIRE (FILE=RNAME(2:J),EXIST=FEXIST)
          IF (.NOT.FEXIST) THEN
            WRITE(6,*)'RTABLE: ERROR: File not found: ',RNAME(2:J)
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF
          OPEN (80,FILE=RNAME(2:J),IOSTAT=ISTAT)
          IF (ISTAT.NE.0) THEN
            WRITE(6,*)'RTABLE: ERROR: Could not open file ',RNAME(2:J)
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF

! 3) Not specified at all - open default retrieval table.

        ELSE                            ! Not user-supplied, use default
          METDB_BASE_DIR=' '
          CALL METDB_GETENV('METDB_BASE_DIR',METDB_BASE_DIR,ISTAT)
          IF (ISTAT.NE.0) THEN
            WRITE(6,*)'RTABLE: ERROR: Environment variable ',
     &                'METDB_BASE_DIR not set'
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF
          J=LEN(METDB_BASE_DIR)  ! Find the length of METDB_BASE_DIR
          DO WHILE (METDB_BASE_DIR(J:J).EQ.' ')
            J=J-1
          ENDDO
          OPEN (80,FILE=METDB_BASE_DIR(1:J)//'/data/'//
     &    RDEFAULT,IOSTAT=ISTAT)
          IF (ISTAT.NE.0) THEN
            WRITE(6,*)'RTABLE: ERROR: Could not open file ',
     &                METDB_BASE_DIR(1:J)//'/data/'//RDEFAULT
            CERR = 'SEE ERROR ABOVE'
            IERR = 16
            RETURN
          ENDIF
        ENDIF
#endif

!-----------------------------------------------------------------------
! Read the retrieval data set table and store data in arrays
!-----------------------------------------------------------------------

        READ (80,'(//A80////)') FORMAT
!                                              Loop over lines in table
        DO LINE=1,LMAX
          DSN(LINE) = ' '
!                                             Read next line from table
          READ (80,FORMAT,IOSTAT=ISTAT)
     &      TYPE(LINE),   IPAREA(LINE), MFLAG(LINE),    IT1, IT2,
     &      MEDIUM(LINE), LISTDS(LINE), ELEMLIST(LINE), ELEMINDX(LINE),
     &      KEEPDAYS(LINE), NDXREC(LINE), NDXMIN(LINE), LENREC(LINE),
     &      SKELFLAG(LINE), STREAM(LINE), DSN(LINE)                !2.6b
!                                     Stop at end of data or blank line

          IF (ISTAT.NE.0 .OR. TYPE(LINE).EQ.' ') GO TO 1
          LINES = LINE

!-----------------------------------------------------------------------
! If not a storage data set, set data period limits to zero
!-----------------------------------------------------------------------

          IF (IPAREA(LINE).LE.0) THEN
            IPTIM1(LINE) = 0
            IPTIM2(LINE) = 0

!-----------------------------------------------------------------------
! Convert period start and end times to century minutes
!-----------------------------------------------------------------------

          ELSE
!                                             Start time (IPTIM1(LINE))
            IF (IT1(1).GT.0) THEN
              CALL DATE31 (IT1(3), IT1(2), IT1(1), NDAY)
              IPTIM1(LINE) = 1440*(NDAY-1) + 60*IT1(4) + IT1(5)
            ELSE
              IPTIM1(LINE) = 0
            END IF
!                                               End time (IPTIM2(LINE))
            IF (IT2(1).GT.0) THEN
              CALL DATE31 (IT2(3), IT2(2), IT2(1), NDAY)
              IPTIM2(LINE) = 1440*(NDAY-1) + 60*IT2(4) + IT2(5)
            ELSE
              IPTIM2(LINE) = 0
            END IF
          END IF
        END DO

!-----------------------------------------------------------------------
! Close the directory and check that data has been read
!-----------------------------------------------------------------------

    1   CONTINUE
        CLOSE (80)
!                            Error message if retrieval table not found
        IF (LINES.EQ.0) THEN
          CERR = 'RETRIEVAL TABLE NOT FOUND'
          IERR = 16
          RETURN
        END IF

        IF (LTEST) WRITE (*,*) ' RTABLE:  Retrieval table read - ',
     &                         'number of entries is', LINES
        FIRST=.FALSE.
      END IF

!=======================================================================
!    LOCATE DIRECTORY ENTRIES FOR TYPE 'CTYPE' (IF NOT ALREADY DONE)
!=======================================================================

      IF (CTYPE.NE.LASTTYPE) THEN  ! Not already done
        LASTTYPE = ' '  ! Forget last subtype
        LASTAREA = -1   ! Forget last area

!                      Find first line in directory for CTYPE ('LINE1')
        LINE1 = 1
        DO WHILE (LINE1.LE.LINES .AND. TYPE(LINE1).NE.CTYPE)
          LINE1 = LINE1 + 1
        END DO
!                       Find last line in directory for CTYPE ('LINE2')
        LINE2 = LINE1
        DO WHILE (LINE2.LT.LINES .AND. TYPE(LINE2+1).EQ.CTYPE)
          LINE2 = LINE2 + 1
        END DO
!                         Return with error message if no entries found

        IF (LINE1.GT.LINES) THEN
          CERR = 'SUBTYPE NOT FOUND IN RETRIEVAL TABLE'
          IERR = 16
          RETURN
        END IF

        LASTTYPE = CTYPE  ! Save new type
      END IF                                                        !2.6

!-----------------------------------------------------------------------
! Skip further searching of data in table if not a storage data set
!-----------------------------------------------------------------------

      IF (IPAREA(LINE1).LE.0) THEN ! Not a storage data set
        LBEST = LINE1  ! Best (only!) type entry
        GO TO 90       ! To set IDATA array
      END IF
!                                                            Diagnostic
      IF (LTEST) WRITE (*,*) ' RTABLE:  Entries for ',
     &           CTYPE, ' cover the range', LINE1, ' to', LINE2

!-----------------------------------------------------------------------
! If retrieving GRIB fields (LISTDS=10) assume only 1 period for GRIB
! data types. Returned data set name (CDAT) may be a skeleton.
!-----------------------------------------------------------------------

      IF (LISTDS(LINE1).EQ.10) THEN  ! GRIB fields
        LBEST = LINE1  ! Best (only!) subtype entry
        GO TO 90       ! To set IDATA array
      END IF

!=======================================================================
!  LOCATE 'CTYPE' ENTRIES FOR REQUIRED MODEL AREA (IF NOT ALREADY DONE)
!=======================================================================

      IF (IAREA.NE.LASTAREA) THEN   ! Not already done
        LASTAREA = -1  ! Forget last area

!                          Find first CTYPE line in directory for IAREA
        IAREA1 = LINE1
        DO WHILE (IAREA1.LE.LINE2 .AND. IPAREA(IAREA1).NE.IAREA)    !2.3
          IAREA1 = IAREA1 + 1
        END DO
!                           Find last CTYPE line in directory for IAREA
        IAREA2 = IAREA1
        DO WHILE (IAREA2.LT.LINE2 .AND. IPAREA(IAREA2+1).EQ.IAREA)
          IAREA2 = IAREA2 + 1
        END DO
!                         Return with error message if no entries found

        IF (IAREA1.GT.LINE2) THEN                                   !2.3
          CERR = 'REQUIRED AREA NOT FOUND FOR THIS SUBTYPE'
          IERR = 16
          RETURN
        END IF

        LASTAREA = IAREA   ! Save new area
      END IF
!                                                            Diagnostic
      IF (LTEST) WRITE (*,*) ' RTABLE:  Entries for area ',
     &           IAREA, ' cover the range', IAREA1, ' to', IAREA2

!=======================================================================
! CHOOSE CUT-OFF TIMES FOR OLDEST ON-LINE AND MOST RECENT ARCHIVED DATA
!=======================================================================
! Loop over raw data (J=1) and merged data (J=2) putting oldest data
! not yet archived in IATIM1 and most recent archived data in IATIM2
!-----------------------------------------------------------------------

      DO J=1,2
        NOWLINE(J) = 0  ! No on-line data found yet
        IOLD = 0        ! Not yet set
        INEW = 0        ! Not yet set

!-----------------------------------------------------------------------
! Search though lines in directory looking for an on-line (MEDIUM=1)
! rolling (IPTIM2=0) data set with the right merge flag (MFLAG). Set
! IOLD to (current time) minus (data set period), i.e. IOLD will be
! just before start of data stored. Also store line number (NOWLINE).
!-----------------------------------------------------------------------

        DO LINE=IAREA1,IAREA2
          IF (MEDIUM(LINE).EQ.1 .AND. IPTIM2(LINE).EQ.0 .AND.
     &        MFLAG(LINE).EQ.RM(J:J)) THEN
            IOLD = NOWMIN - NDXREC(LINE)*NDXMIN(LINE)
            NOWLINE(J) = LINE
          END IF
        END DO

!-----------------------------------------------------------------------
! If on-line data was found, search remaining lines looking for an
! off-line (MEDIUM not 1) rolling (IPTIM2=0) data set with the right
! merge flag (MFLAG).  Increment the start time of the on-line data
! (IOLD) by one index period get into the range of data still stored.
! Set a preliminary end time for the off-line data (INEW) to IOLD plus
! one data set period, and then round this down to the to nearest time
! corresponding to the end of a data set.
!-----------------------------------------------------------------------

        IF (NOWLINE(J).GT.0) THEN
          DO LINE=NOWLINE(J)+1,IAREA2
            IF (MEDIUM(LINE).NE.1 .AND. IPTIM2(LINE).EQ.0 .AND.
     &          MFLAG(LINE).EQ.RM(J:J)) THEN
              IOLD = IOLD + NDXMIN(NOWLINE(J))
              IDMIN = NDXREC(LINE)*NDXMIN(LINE)  ! Data set time period
              INEW = IOLD + IDMIN
              INEW = IPTIM1(LINE) +
     &               IDMIN*((INEW-IPTIM1(LINE))/IDMIN) ! Round down

!-----------------------------------------------------------------------
! If no archive of unmerged data exists and unmerged data older than
! that in the on-line data set is required, the retrieval will look for
! it in archived merged data if available. However, in this case the
! start time of the on-line unmerged data will still be earlier than
! the first data available because the change to IOLD above wouldn't
! have been done for J=1. So for merged data (J=2) we need to check
! for the need to increment the start time for unmerged data (J=1) if
! it hasn't already been done. We can test whether it has been done by
! checking IATIM1(1) which will still be zero if it hasn't.
!-----------------------------------------------------------------------

              IF (J.EQ.2 .AND. IATIM2(1).EQ.0)                      !2.7
     &            IATIM1(1) = IATIM1(1) + NDXMIN(NOWLINE(1))        !2.7
            END IF
          END DO
        END IF

!-----------------------------------------------------------------------
! Put start of on-line data period in IATIM1(J) and end of off-line
! data period in IATIM2(J).
!-----------------------------------------------------------------------

        IATIM1(J) = IOLD
        IATIM2(J) = INEW
      END DO
!                                                   Diagnostic printout
      IF (LTEST) THEN
        WRITE (*,*) ' RTABLE:  On- & off-line cut-off times for ',
     &              'raw data:  ', IATIM1(1), IATIM2(1)
        WRITE (*,*) ' RTABLE:  On- & off-line cut-off times for ',
     &              'merge data:', IATIM1(2), IATIM2(2)
      END IF

!=======================================================================
!        CONVERT USER'S START AND END TIMES TO CENTURY MINUTES
!=======================================================================
! Notes:
!
! - The user's end time is specified as the start of the last minute of
!   the period, e.g. a period ending at midnight would be specified as
!   up to 2359Z. However, the end 'century minute' time is for the true
!   end of the period, so 1 is effectively added to IETIME(8).
!
! - Elements 4 and 8 of IETIME contain time of day in 'hhmm' format
!   (i.e. 100*hours + minutes) but to compute century minutes we need
!   60*hours + minutes, so we subtract 40*hours (= 40*('hhmm'/100)).
!-----------------------------------------------------------------------

!                                            User's start time (IUTIM1)
      IF (IETIME(1).NE.0) THEN
        CALL DATE31 (IETIME(3), IETIME(2), IETIME(1), NDAY)
        IUTIM1 = 1440*(NDAY-1) + IETIME(4) - 40*(IETIME(4)/100)

!                                              User's end time (IUTIM2)
        IF (IETIME(5).NE.0) THEN
          CALL DATE31 (IETIME(7), IETIME(6), IETIME(5), NDAY)
          IUTIM2 = 1440*(NDAY-1) + IETIME(8) - 40*(IETIME(8)/100) + 1

!-----------------------------------------------------------------------
! If no end time is specified, assume 1 minute after start time
!-----------------------------------------------------------------------

        ELSE
          IUTIM2 = IUTIM1 + 1
        END IF

!-----------------------------------------------------------------------
! If start time is not specified, set times to zero
!-----------------------------------------------------------------------

      ELSE
        IUTIM1 = 0
        IUTIM2 = 0
      END IF
!                                                   Diagnostic printout
      IF (LTEST) WRITE (*,*)
     &  ' RTABLE:  User''s start & end times -', IUTIM1, IUTIM2

!=======================================================================
! LOOP OVER PERIODS FOR REQUIRED TYPE & AREA TO FIND ONE INCLUDING THE
! USER'S START TIME (OR, FAILING THIS, THE NEXT PERIOD AFTER THIS TIME)
!=======================================================================

      IBEST  = 0               ! Best score so far
      IDTIM1 = 0               ! No data set start time yet
      MPREFER = RM(IRM:IRM)    ! Preferred data ('R' or 'M')

!-----------------------------------------------------------------------
! If no start time is given, look at the on-line rolling data set only
!-----------------------------------------------------------------------

      IF (IUTIM1.EQ.0) THEN
        IAREA1 = NOWLINE(1)  ! On-line rolling data set
        IAREA2 = IAREA1
        IUTIM1 = NOWMIN      ! Current time
        IUTIM2 = NOWMIN+1    ! Ensure non-zero time interval        !2.1
      END IF
!                                                     Loop over periods
      DO LINE=IAREA1,IAREA2

!-----------------------------------------------------------------------
! Modify start time if oldest data in period has time-expired.
! Compute the expiry time in century minutes (IXTIM). To get time of
! oldest kept data, round this down to the nearest data set start time.
!-----------------------------------------------------------------------

        IF (KEEPDAYS(LINE).NE.0) THEN
          IXTIM = NOWMIN - 1440*KEEPDAYS(LINE) ! Current expiry time

!                                                Check for expired data
          IF (IXTIM.GT.IPTIM1(LINE)) THEN
            IDMIN = NDXREC(LINE)*NDXMIN(LINE)  ! Data set time period
            IPTIM1(LINE) = IPTIM1(LINE) +      ! Adjust period start
     &                     IDMIN*((IXTIM-IPTIM1(LINE))/IDMIN)
          END IF
        END IF

!-----------------------------------------------------------------------
! Store start and end of period in IOLD and INEW
!-----------------------------------------------------------------------

        IOLD = IPTIM1(LINE)
        INEW = IPTIM2(LINE)

!-----------------------------------------------------------------------
! Set start and/or end times if rolling data set (INEW=0)
!-----------------------------------------------------------------------

        IF (INEW.EQ.0) THEN
          MWANT = INDEX(RM,MFLAG(LINE))  ! 1 or 2 for R(aw) or M(erged)

!                                  On-line (MEDIUM=1) rolling data set:
!                                                        Get start time
          IF (MEDIUM(LINE).EQ.1 .AND. IOLD.EQ.0) THEN
            IOLD = IATIM1(MWANT)

!                          Off-line (MEDIUM not 1) open-ended data set:
!                                                          Get end time
          ELSE IF (MEDIUM(LINE).NE.1) THEN
            INEW = IATIM2(MWANT)
          END IF
        END IF
!                          If end time still not set, set it to current
!                                time (or user's end time if in future)

        IF (INEW.EQ.0) INEW = MAX0(IUTIM2,NOWMIN)

!=======================================================================
! USE A SCORING SYSTEM TO SELECT PERIOD MATCHING THE USER'S TIME WINDOW
!=======================================================================

        PREFER = .FALSE.  ! Default

!-----------------------------------------------------------------------
! Score zero if period lies entirely outside user's time window
!-----------------------------------------------------------------------

        IF (INEW.LE.IUTIM1 .OR. IOLD.GE.IUTIM2) THEN
          ISCORE = 0

!-----------------------------------------------------------------------
! If the period includes the start of the user's window, score 8 plus
!    4 if data is of the preferred type (raw or merged) and
!    2 if the data set is on-line.
!-----------------------------------------------------------------------

        ELSE IF (IOLD.LE.IUTIM1) THEN ! Period includes user start time
          ISCORE = 8
          IF (MFLAG(LINE).EQ.MPREFER) ISCORE = ISCORE + 4
          IF (MEDIUM(LINE).EQ.1) ISCORE = ISCORE + 2

!-----------------------------------------------------------------------
! If the period starts after the start of the user's window, score 4 but
! give preference to one with a start time earlier than the best found
! so far.  If the earliest start time is shared by two or more periods,
! give preference to the one with the desired data type (raw or merged).
!-----------------------------------------------------------------------

        ELSE                ! User start time is before start of period
          ISCORE = 4
          IF (IOLD.NE.IDTIM1) THEN
            PREFER = IOLD.LT.IDTIM1
          ELSE
            PREFER = MFLAG(LINE).EQ.MPREFER
          END IF
        END IF

!-----------------------------------------------------------------------
! Don't accept on-line raw data if you want merged data
!-----------------------------------------------------------------------

        IF (MEDIUM(LINE).EQ.1 .AND. MPREFER.EQ.'M' .AND.
     &      MFLAG(LINE).NE.MPREFER) ISCORE = 0

!-----------------------------------------------------------------------
! Keep period details if score is above highest found so far or if the
! score is the same but the new period is preferred
!-----------------------------------------------------------------------

        IF (ISCORE.GT.IBEST .OR.
     &     (ISCORE.EQ.IBEST .AND. PREFER)) THEN
!                                                        Update details
          IBEST  = ISCORE  ! Best score so far
          IDTIM1 = IOLD    ! Period start time
          IDTIM2 = INEW    ! Period end time
          LBEST  = LINE    ! Line number in table
        END IF
      END DO
!                                  Error condition if no data available
      IF (IBEST.LE.2) THEN
        WRITE(6,*)'RTABLE: ',CTYPE(1:8),' - DATE/TIME NOT AVAILABLE'
        WRITE(6,'(A,2(I4,2I2.2,''/'',I4.4,''Z'',3X))')
     &  '        REQUEST START,END TIMES = ',(IETIME(J),J=1,8)
        IERR = 8
        CERR = 'SEE MESSAGE ABOVE'
        RETURN
      END IF

      IF (LTEST) WRITE (*,*)' RTABLE:  Table entry number selected',
     &                        LBEST, '.  Score =', IBEST

!=======================================================================
! IF THE DATA SET NAME IS A SKELETON AND THE PERIOD INCLUDES THE USER'S
! START TIME, FIND THE START & END TIMES OF DATA SET HOLDING THIS TIME
!=======================================================================

      IF (SKELFLAG(LBEST)) THEN
        IDMIN = NDXREC(LBEST)*NDXMIN(LBEST) ! Data set time range

!-----------------------------------------------------------------------
! Convert IDTIM1 to a data set start time by adding the largest multiple
! of the data set time range which does not pass the user's start time
!-----------------------------------------------------------------------

        IF (IUTIM1.GT.IDTIM1)         ! User's start time is in period
     &    IDTIM1 = IDTIM1 + IDMIN*((IUTIM1-IDTIM1)/IDMIN)

!-----------------------------------------------------------------------
! Convert IDTIM2 to the data set end time (= IDTIM1 + data set period)
! but check against end time of period in case of overshoot
!-----------------------------------------------------------------------

        IDTIM2 = MIN0(IDTIM2,IDTIM1+IDMIN)
      END IF

!=======================================================================
! ADJUST THE RETRIEVAL TIME WINDOW IF THE DATA SET STARTS AFTER THE
! START OF THE USER'S WINDOW OR FINISHES BEFORE THE END OF THE WINDOW
!=======================================================================
! Note:
!       In this section we need the time of day in 'hhmm' format (i.e.
!   100*hours + minutes) for elements 4 and 8 of IETIME but MINUTES
!   (= minutes since 00Z) is 60*hours + minutes, so we need to add
!   40*hours (= 40*(MINUTES/60)).
!-----------------------------------------------------------------------

!                           Data set starts after retrieval start time:
!                                           Change start time to IDTIM1
      IF (IUTIM1.LT.IDTIM1) THEN
        MINUTES = IDTIM1
        NDAY = MINUTES/1440 + 1
        CALL DATE13 (NDAY, IETIME(3), IETIME(2), IETIME(1))
        MINUTES = MOD(MINUTES,1440)  ! Minutes since 00Z
        IETIME(4) = MINUTES + 40*(MINUTES/60)
!                                                    Diagnostic message
        IF (LTEST) WRITE (*,*)
     &    ' RTABLE:  User start time updated to', (IETIME(J),J=1,4)
      END IF
!                              Data set ends before retrieval end time:
!                                             Change end time to IDTIM2
      IF (IUTIM2.GT.IDTIM2) THEN
        MINUTES = IDTIM2 - 1
        NDAY = MINUTES/1440 + 1
        CALL DATE13 (NDAY, IETIME(7), IETIME(6), IETIME(5))
        MINUTES = MOD(MINUTES,1440)  ! Minutes since 00Z
        IETIME(8) = MINUTES + 40*(MINUTES/60)
        IERR = 4  ! More data to come
!                                                    Diagnostic message
        IF (LTEST) WRITE (*,*)
     &    ' RTABLE:  User end time updated to  ', (IETIME(J),J=5,8)
      END IF

!=======================================================================
!        GET DATA SET NAME, DERIVING FROM SKELETON IF NECESSARY
!=======================================================================

  90  CONTINUE
      CDAT = DSN(LBEST)                   ! DSN from directory
      LENDSN = INDEX(CDAT,' ') - 1        ! Length of name
      IF (LENDSN.LT.0) LENDSN = LEN(DSN(LBEST))

!                           Make data set name if skeleton and not GRIB

      IF (SKELFLAG(LBEST) .AND. LISTDS(LBEST).NE.10) THEN
!                                                           Diagnostics
        IF (LTEST) WRITE (*,*)
     &           ' RTABLE:  Skeleton name is ', CDAT(1:LENDSN)

!                               Get year, month & day for data set name
        NDAY = IDTIM1/1440 + 1                                      !2.2
        CALL DATE13 (NDAY, IDAY, IMONTH, IYEAR)                     !2.2

!                                        Look for 'YY' in skeleton name
        J = 1  ! Position in CDAT
        I = INDEX(CDAT(J:LENDSN),'YY')

!                                  If 'YYYY', replace with 4-digit year
        IF (I.GT.0) THEN
          IF (CDAT(I+2:I+3).EQ.'YY') THEN
            WRITE (CDAT(I:I+3),'(I4.4)') IYEAR                      !2.2
            J = I + 3
!                               If just 'YY', replace with 2-digit year
          ELSE
            IYEAR = MOD(IYEAR,100)                                  !2.2
            WRITE (CDAT(I:I+1),'(I2.2)') IYEAR                      !2.2
            J = I + 1
          END IF
        END IF
!                          Look for 'MM' in name and replace with month

        I = INDEX(CDAT(J:LENDSN),'MM')
        IF (I.GT.0) THEN
          J = J + I - 1
          WRITE (CDAT(J:J+1),'(I2.2)') IMONTH                       !2.2
        END IF
!                            Look for 'DD' in name and replace with day

        I = INDEX(CDAT(J:LENDSN),'DD')
        IF (I.GT.0) THEN
          J = J + I - 1
          WRITE (CDAT(J:J+1),'(I2.2)') IDAY                         !2.2
        END IF
      END IF

      IF (LTEST) WRITE (*,*)
     &         ' RTABLE:  Data set name is ', CDAT(1:LENDSN)

!=======================================================================
!             SET UP 5-ELEMENT ARRAY 'IDATA' AND RETURN
!=======================================================================

      IDATA(1) = LENDSN         ! Length of data set name (CDAT)
      IDATA(2) = LENREC(LBEST)  ! Record length of data set
      IDATA(3) = 84             ! Unit number for data set
      IDATA(4) = LISTDS(LBEST)  ! Number of associated data set list
      IDATA(5) = MEDIUM(LBEST)  ! Medium (1=disk ,2=tape)

      IRM = INDEX(RM,MFLAG(LBEST))  ! 1 or 2 for raw or merged data
      LIST = ELEMLIST(LBEST)        ! Member containing elements list
      ELIST = ELEMINDX(LBEST)       ! Member containing elemidx    !2.6b
      MSTREAM = STREAM(LBEST)       ! MASS stream                  !2.4b

      IF (LTEST) THEN
        WRITE (*,*) ' RTABLE:  "IDATA" array is', IDATA
        WRITE (*,*) ' RTABLE:  Data type (1=raw, 2=merged)', IRM
        WRITE (*,*) ' RTABLE:  Element names list is ', LIST
        WRITE (*,*) ' RTABLE:  Element index list is ', ELIST      !2.6b
        WRITE (*,*) ' RTABLE:  MASS stream is ', MSTREAM           !2.4b
      END IF

      RETURN
      END
