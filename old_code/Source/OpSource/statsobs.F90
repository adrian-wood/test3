PROGRAM STATSOBS

!-----------------------------------------------------------------------
!
! PROGRAM       : STATSOBS
!
! PURPOSE       : Print observation totals in a 24-hour period.
!
! DESCRIPTION   : STATSOBS computes statistics of observations stored in
!                 the MetDB for various observation types as listed in
!                 the input on unit 5. Observation type codes should be
!                 as in MDB.DATASETS and should be listed one to a line
!                 starting in column 1. Entries after the blank line are
!                 ignored.
!
!                 The output consists of observation totals, first&last
!                 receipt times and latitude & longitude limits for each
!                 hour for the day specified in the NAMELIST input (see
!                 below). Also the total number of observations for the
!                 whole day is given.
!
!                 If "DSN" is specified in the NAMELIST, the tabulated
!                 output will be added to a statistics data set if one
!                 exists (see below for naming convention).
!
!                 STATSOBS should not be used for any data type for
!                 which the storage data set has overflow blocks or uses
!                 trailers and/or 'chaining'.
!
!                 STATSOBS reads index blocks in the storage data sets
!                 but not data blocks. It can handle old format 12- or
!                 23-byte index entries or new format index entry
!                 versions 1 and 2.
!
! NAMELIST      : INSTAT  (Unit 2).  Contents as follows:
!
!                 Variable Type        Description              Default
!                 -------- ----        -----------              -------
!                  NDAY    I*4   Day of month for which stats     -1
!                                are required (or, if negative,
!                                days before 'today').
!
!                  DSN     C*15  First two levels of stats      Blanks
!                                data sets (or
!                                blanks if no data sets).
!                 Notes:
!                   - If NDAY is greater then the current day, it is
!                     assumed to apply for last month.
!                   - Statistics data sets are assumed to have names of
!                     the form "DSN.datatype" where "DSN" is as
!                     specified (e.g. MDB.STATS) and datatype is the
!                     qualifier of the name of the storage data set.
!
! CALLS         : DATE13, DATE31, DATIM, ICHAR2, ICHAR3, INQUIRE
!
! HISTORY       : Original version by Brian Barwell, May 2001.
!
! REVISION INFO :
!
! $Workfile: statsobs.F90$ $Folder: OpSource$
! $Revision: 8$ $Date: 22/06/2011 15:35:20$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         22/06/2011 15:35:20    Sheila Needham
!       initialise stats output dataset name
!  7    MetDB_Refresh 1.6         21/06/2011 18:59:45    Sheila Needham  Fix to
!        opening MVs datasets
!  6    MetDB_Refresh 1.5         10/06/2011 09:32:51    Sheila Needham
!       Reinstate changes from revision 4
!  5    MetDB_Refresh 1.4         09/06/2011 17:42:31    Rosemary Lavery Use
!       c-routines for ZFS storage data sets
!  4    MetDB_Refresh 1.3         08/06/2011 17:18:25    Brian Barwell   Delete
!        obsolete ERS check and bypass ACFT check if new-format storage data
!       set.
!  3    MetDB_Refresh 1.2         03/05/2011 13:13:25    Alison Weir     Change
!        to c i/o for STATS dataset
!  2    MetDB_Refresh 1.1         28/04/2011 11:01:00    Alison Weir     Ported
!        to F95, plus a couple of small tweaks to deal with longer STATS and
!       ZFS dataset names.
!  1    MetDB_Refresh 1.0         15/04/2011 16:39:42    Alison Weir
!       Initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE datim_mod
USE dsinfo_mod
USE ichar2_mod
USE ichar3_mod
USE inquire_mod
USE zpdate_mod

IMPLICIT NONE

INTEGER  ::  I, J        ! General variables for local use
INTEGER  ::  IBOX        ! Offset for start of lat/lon box in entry
INTEGER  ::  ICD         ! Century day for statistics date
INTEGER  ::  IDT(8)      ! Current date and time (from DATIM)
INTEGER  ::  IFIRST(0:23)! Receipt time for earliest ob. each hour
INTEGER  ::  IHHMM1      ! Earliest observation time in 'hhmm' format
INTEGER  ::  IHHMM2      ! Latest observation time in 'hhmm' format
INTEGER  ::  IHOUR       ! Offset for hour in index entry
INTEGER  ::  ILAST(0:23) ! Receipt time for latest ob. each hour
INTEGER  ::  IOBS        ! Number of observations (from index entry)
INTEGER  ::  IOPEN       ! Mode for opening data set (0=read-only)  !4
INTEGER  ::  IOFLAG      ! i/o return code from COPEN               !4
INTEGER  ::  IOS         ! Status from I/O statements
INTEGER  ::  IPBOX       ! Location of lat/lon info in current index
INTEGER  ::  IPT         ! Pointer to end of last index entry processed
INTEGER  ::  IPTIM1      ! First index time to look at
INTEGER  ::  IPTIM2      ! Last index time to look at
INTEGER  ::  IRC         ! Return code from i/o stmt
INTEGER  ::  ISIZE       ! Amount of data read/written from stats ds
INTEGER  ::  IT(5)       ! Index time (year, month, day, hour, minute)
INTEGER  ::  ITIM1,ITIM2 ! Century mins for 0000Z & 2359Z on stats date
INTEGER  ::  ITOR        ! Receipt time mins (from index time, then 0Z)
INTEGER  ::  IUNIT       ! Unit number for storage data set
INTEGER  ::  IVERSN      ! Storage d/s. version nos. (new format only)
INTEGER  ::  IWKDAY      ! Day of week (1=Sunday etc.) for stats date
INTEGER  ::  IXBLK       ! Current index block number
INTEGER  ::  IY, IM, ID  ! Year, month and day of statistics date
INTEGER  ::  JENTRY      ! Loop variable for loop over index entries
INTEGER  ::  JHOUR       ! Loop variable for loop over hours (0-23)
INTEGER  ::  JPTIM       ! Loop variable for index periods (cent.mins.)
INTEGER  ::  KODE        ! Return code from DSINFO
INTEGER  ::  LATN, LATS  ! North and south limits for index entry
INTEGER  ::  LENQ        ! Length of first two levels of stats dsn
INTEGER  ::  LENREC      ! Record length of storage data set
INTEGER  ::  LENTRY      ! Index entry length for storage data set
INTEGER  ::  LINE        ! Subscript for CDATA
INTEGER  ::  LONE, LONW  ! East and west limits for index entry
INTEGER  ::  MAXLAT(0:23)! Maximum observation latitude for each hour
INTEGER  ::  MAXLON(0:23)! Maximum observation longitude for each hour
INTEGER  ::  MAXNDX      ! Maximum number of entries in 1 index block
INTEGER  ::  NBLKS       ! Total number of blocks in storage data set
INTEGER  ::  MINLAT(0:23)! Minimum observation latitude for each hour
INTEGER  ::  MINLON(0:23)! Minimum observation longitude for each hour
INTEGER  ::  NHOUR       ! Observation century hour
INTEGER  ::  NSQ         ! Offset for block numbering (old format only)
INTEGER  ::  NTOTAL      ! Number of observations (all hours)
INTEGER  ::  NTRIES      ! Number of entries in current index block
INTEGER  ::  NUMMAP      ! Number of map blocks in storage data set
INTEGER  ::  NUMOBS(0:23)! Number of observations for each hour
INTEGER  ::  NUMR        ! Record number in storage data set        !4
INTEGER  ::  NXBLKS      ! Number of index blocks in storage data set
INTEGER  ::  NXHEAD      ! Index block header length
INTEGER  ::  NXMIN       ! Index period in minutes
INTEGER  ::  NX00Z       ! Index period offset from 00Z
INTEGER  ::  NX1         ! Block number of first index block
#if ! defined (MVS)
INTEGER  ::  NFT
INTEGER  ::  DTPATHLEN
#endif

LOGICAL  ::  ACFT           ! Flag for aircraft (AIREP or AMDAR) data
LOGICAL  ::  GOOD           ! 'So far so good' flag
LOGICAL  ::  LATE           ! Flag for T.O.R. outside limit in index
LOGICAL  ::  LATEOB(2,0:23) ! Flag for unreliable IFIRST and ILAST
LOGICAL  ::  OLDFMT         ! Flag for storage data set in old format
LOGICAL  ::  STATDSOK       ! Flag to indicate if stats d/s exists

CHARACTER(LEN=80)   ::  CDATA(672) ! Stats data set contents after CHEAD
CHARACTER(LEN=80)   ::  CHEAD(4)   ! First 4 lines of stats data set
CHARACTER(LEN=3)    ::  CMONTH(12) ! Abbreviated names of months
CHARACTER(LEN=44)   ::  DSSTOR     ! Name of storage d/s from DSINFO
CHARACTER(LEN=5)    ::  INDXTAG    ! 5-char tag time from index block
CHARACTER(LEN=27998) ::  RECORD    ! Buffer to hold 1 rec of storage d/s
CHARACTER(LEN=200)  ::  STATNAME   ! Name of statistics data set
CHARACTER(LEN=200)  ::  STORNAME   ! Name of storage data set
CHARACTER(LEN=5)    ::  TIMTAG     ! 5-character index tag time (ymdhm)
CHARACTER(LEN=8)    ::  TYPE       ! MetDB code for data type
CHARACTER(LEN=9)    ::  WKDAY(7)   ! Names of days of the week
#if ! defined (MVS)
CHARACTER(LEN=8)  ::  DT
#endif
!
!                                                              NAMELIST
!
INTEGER  ::  NDAY        ! Day of month for which stats are required
                         ! (or, if negative, days before 'today')
CHARACTER(LEN=15) :: DSN ! First two levels of stats dataset name
NAMELIST /INSTAT/ NDAY, DSN
DATA  NDAY, DSN /  -1,  ' '/
!                                                   Data initialisation
!
DATA WKDAY  /'Sunday   ', 'Monday   ', 'Tuesday  ', 'Wednesday', &
             'Thursday ', 'Friday   ', 'Saturday '/
DATA CMONTH /'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',           &
             'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'/
DATA IVERSN /0/
!
!
!-----------------------------------------------------------------------
! Read high level qualifier of statistics data set and date from
! namelist. (Negative dates refer to days before today.)
!-----------------------------------------------------------------------

GOOD = INQUIRE ('FT02F001', 'DDN')
IF (GOOD) THEN
#if defined (MVS)
  OPEN (2,FILE='DD:FT02F001',IOSTAT=IOS, ACTION='READ')
#else
  OPEN (2,FILE='FT02F001',IOSTAT=IOS)
#endif
  READ (2, INSTAT, IOSTAT=IOS)
  CLOSE (2)
END IF

LENQ = INDEX(DSN//' ',' ') - 1    ! Length of DSN

!-----------------------------------------------------------------------
! Get century day of date for which statistics are required
!-----------------------------------------------------------------------

CALL DATIM (IDT)  ! Today's date

!                                              NDAY = days before today
IFLABEL1: &
IF (NDAY <= 0) THEN
  CALL DATE31 (IDT(6), IDT(7), IDT(8), ICD)
  ICD = ICD + NDAY
  CALL DATE13 (ICD, ID, IM, IY)
!                                            NDAY = actual day of month
ELSE
  IM = IDT(7)
  IY = IDT(8)
  IF (NDAY > IDT(6)) THEN    ! Must be last month
    IM = IM - 1
    IF (IM == 0) THEN    ! Must be last year
      IM = 12
      IY = IY - 1
    END IF
  END IF
  ID = NDAY
  CALL DATE31 (ID, IM, IY, ICD)
END IF  IFLABEL1
!                                                       Get day of week
IWKDAY = MOD(ICD,7) + 1

!=======================================================================
!                    LOOP OVER MET.D.B. DATA TYPES
!=======================================================================
!
TYPE = 'NOTBLANK'
DO_TYPE: &
DO WHILE (TYPE /= ' ')
  GOOD = .TRUE.
!                                             Read data type from input
#if defined (MVS)
  OPEN (5,FILE='DD:FT05F001',IOSTAT=IOS,ACTION='READ',FORM='FORMATTED')
#endif
  READ (5,'(A)',IOSTAT=IOS) TYPE
  IF (IOS /= 0) TYPE = ' '
  IF (TYPE == ' ') GOOD = .FALSE.
!
!                           Locate storage data set & check return code
IFLABEL2: &
  IF (GOOD) THEN
#if defined (MVS)
    IUNIT = 10                                                      !4
    IOPEN=0                            ! Info only - does not open  !4
    CALL DSINFO (TYPE, IOPEN, IUNIT, LENREC, KODE, DSSTOR)          !4
    IF (KODE /= 1) THEN ! Problem
      WRITE (6,'(/T3,3A,I3)') 'STATSOBS:  Data type ',TYPE,  &
               ' Return code from DSINFO is', KODE
      GOOD = .FALSE.
    END IF
    STORNAME=DSSTOR
!
! If the file is MVS then add //'...' around the dataset name
!
    IF (INDEX(DSSTOR(1:LEN(DSSTOR)),'/') <= 0) &
      DSSTOR='//'''//TRIM(DSSTOR)//''''
    CALL METDB_COPEN(IUNIT,TRIM(DSSTOR)//CHAR(0),IOPEN,IOFLAG)
    IF (IOFLAG /= 0) THEN                                           !4
      WRITE (6,'(/T3,3A,I3)') 'STATSOBS:  Data type ',TYPE,  &
               ' Error opening file, status=', IOFLAG               !4
      GOOD = .FALSE.
    END IF
#else
    OPEN(15,FILE='HPMDB_datasets',FORM='FORMATTED')
    READ(15,*)
    READ(15,*)
    IOS = 0
    DO WHILE (IOS == 0)
      READ(15,'(A8,2X,I6,2(2X,I3),2X,A100)')DT,LENREC,NFT,  &
      DTPATHLEN,STORNAME
      IF (DT == TYPE) THEN
        OPEN(10,FILE=STORNAME(1:DTPATHLEN),ACCESS='DIRECT', &
             RECL=LENREC,IOSTAT=IOS)
        IF (IOS /= 0) THEN
          WRITE(6,*)'STATSJOB: ERROR OPENING DATASET ',     &
          STORNAME(1:DTPATHLEN)
          GOOD = .FALSE.
        END IF
        IOS = 100  !- subtype matched. Exit loop.
      END IF
      IF (DT == 'END') IOS = 200  !- stop at end of file.
    END DO
    CLOSE(15)
#endif
  END IF  IFLABEL2
!                                               Look for stats data set
  STATDSOK = .FALSE.
IFLABEL3: &
  IF (GOOD) THEN
IFLABEL4: &
    IF (DSN(1:1) /= ' ') THEN
!                                               Get stats data set name
      STATNAME = DSN

#if defined (MVS)
      J = INDEX(STORNAME,'.')            ! 1st dot in STORNAME
      IF (J > 0) THEN
        STATNAME = DSN(1:LENQ) // STORNAME(J:44)
!
!                                               Check whether it exists
!
        STATDSOK = INQUIRE(STATNAME,'DSN')
      END IF
#else
      STATNAME = DSN(1:LENQ) // '.' // TYPE
      STATDSOK = INQUIRE(STATNAME,'DSN')
#endif
    END IF  IFLABEL4
!                                               Look for stats data set
!                                          If stats data set exists ...
IFLABEL5: &
    IF (STATDSOK) THEN
!                                                           ... open it
#if defined (MVS)
      CALL METDB_COPEN(20,"//'"//TRIM(STATNAME)//"'"//CHAR(0),0,IOS)
#else
      CALL METDB_COPEN(20,TRIM(STATNAME)//CHAR(0),0,IOS)
#endif
!                                                     Check status code
      IF (IOS /= 0) THEN
        WRITE (6,'(/T3,4A)') 'STATSOBS:  Data type ',TYPE,  &
               ' Error opening ', STATNAME
        GOOD = .FALSE.
!                                                  If OK, read the data
      ELSE
        DO I=1,4
          CALL METDB_CREAD(20, CHEAD(I), ISIZE)
        END DO
        DO I=33,672
          CALL METDB_CREAD(20, CDATA(I), ISIZE)
        END DO
        CALL METDB_CCLOSE(20)
      END IF
    ELSE
!                                 Make titles if data set doesn't exist
      I = INDEX(TYPE,' ')
      IF (I == 0) I = 9
      CHEAD(1) = ' '
      WRITE (CHEAD(1),'(T19,23A1)') ('=', J=1,I+14)
      WRITE (CHEAD(2),'(T19,2A)') 'MDB TOTALS FOR ', TYPE
      CHEAD(3) = CHEAD(1)
      CHEAD(4) = ' '
    END IF  IFLABEL5
  END IF  IFLABEL3
!
IFLABEL6: &
  IF (GOOD) THEN
!                                         Table headings for statistics
    CDATA(1) = &
       '========================================================='
    WRITE (CDATA(2)(:),'(A,I3.2,A4,I5.4)')  &
           WKDAY(IWKDAY), ID, CMONTH(IM), IY
    J = INDEX(STORNAME,' ')
#if defined (MVS)
    I = 1
! If dataset name is too big for space in table, truncate start.
    IF (J > 35) THEN
      I = J - 32
      CDATA(2)(55+I-J:58) = '...'//STORNAME(I:J)
    ELSE
      CDATA(2)(59-J:58) = STORNAME(I:J)
    END IF
#else
    CDATA(2)(50:) = TYPE
#endif
    CDATA(3) =   &
       '---------------------------------------------------------'
    CDATA(4) =   &
       'Hours   Observations   Receipt (Z)   Latitude   Longitude'
    CDATA(5) =   &
       '                       First  Last   Min  Max    Min  Max'
    CDATA(6) = CDATA(3)
    CDATA(32)= CDATA(1)
!
!-----------------------------------------------------------------------
! Read first block of storage data set
!-----------------------------------------------------------------------
!
! **READ (10,REC=1) RECORD(1:LENREC)

    NUMR=1                                                         !4
    CALL METDB_CREAD_DIR (IUNIT,RECORD,LENREC,NUMR,IRC)            !4

    I = ICHAR2(RECORD(1:2))
!                                                 I=0: New format
IFLABEL7: &
    IF (I == 0) THEN
      IVERSN = TRANSFER(RECORD( 1: 4),IVERSN)
      NBLKS  = TRANSFER(RECORD( 5: 8),NBLKS)
      NUMMAP = TRANSFER(RECORD( 9:12),NUMMAP)
      NXBLKS = TRANSFER(RECORD(13:16),NXBLKS)
      NXMIN  = TRANSFER(RECORD(17:20),NXMIN)
      NX00Z  = TRANSFER(RECORD(21:24),NX00Z)
      LENTRY = TRANSFER(RECORD(25:28),LENTRY)
!
      IF (IVERSN == 257) THEN      ! Index entry version 1
        IHOUR = 3                  ! Index byte containing hour
        IBOX = 9                   ! Offset for lat/lon box
      ELSE IF (IVERSN == 258) THEN ! Index entry version 2
        IHOUR = 10                 ! Index byte containing hour
        IBOX = 16                  ! Offset for lat/lon box
      ELSE
        WRITE (6,'(/T5,A,I6)') 'INVALID VERSION NUMBER -', I
        GOOD = .FALSE.
      END IF
!
      OLDFMT = .FALSE.             ! Not old data set format
      NXHEAD = 10                  ! Index block header length
      NX1 =  NUMMAP + 3            ! Number of first index block
      MAXNDX = (LENREC-NXHEAD)/LENTRY  ! Max. entries per block
!
!                                                 I>0: Old format
    ELSE
      NBLKS  = I                         ! Total no. of blocks
      NXBLKS = ICHAR2(RECORD(3:4))       ! No. of index blocks
      NXMIN  = ICHAR2(RECORD(5:6))*60    ! Index period (minutes)
      NX00Z  = MOD(ICHAR2(RECORD(7:8))*60,NXMIN) ! Index offset
      LENTRY = ICHAR2(RECORD(LENREC-1:LENREC))   ! Entry length
!
!                                       Get number of first index block
!                             (=3 if sequence record exists, =2 if not)
!
      IF ((NBLKS <= 12000 .AND.                      &
           ICHAR(RECORD(NBLKS+8:NBLKS+8)) > 0) .OR. &
          (NBLKS > 12000 .AND. ICHAR(RECORD(9:9)) > 0)) THEN
        NX1 = 3
      ELSE
        NX1 = 2
      END IF

      NSQ = NX1 - 2          ! Offset for block numbers in index
      OLDFMT = .TRUE.        ! Old data set format
      NXHEAD = 6             ! Index block header length
      IF (LENTRY == 12) THEN
        IHOUR = 2            ! Index byte containing hour
        IBOX = 5             ! Offset for start of lat/lon box
      ELSE
        IHOUR = 1            ! Index byte containing hour
        IBOX = 13            ! Offset for start of lat/lon box
      END IF
      MAXNDX = (LENREC-NXHEAD-2)/LENTRY  ! Index block capacity
    END IF  IFLABEL7
  END IF  IFLABEL6
!
IFLABEL8: &
  IF (GOOD) THEN
!                            AIREP & AMDAR need special treatment later
!
    ACFT = TYPE(1:5) == 'AIREP' .OR. TYPE(1:5) == 'AMDAR'
!
!-----------------------------------------------------------------------
! Initialise hourly arrays and index times for loop
!-----------------------------------------------------------------------
!
    DO JHOUR=0,23
      NUMOBS(JHOUR) =  0        ! Number of observations
      IFIRST(JHOUR) =  9999     ! Earliest receipt time
      ILAST(JHOUR)  = -9999     ! Latest receipt time
      MINLAT(JHOUR) =  9999     ! Minimum latitude
      MAXLAT(JHOUR) = -9999     ! Maximum latitude
      MINLON(JHOUR) =  9999     ! Minimum longitude
      MAXLON(JHOUR) = -9999     ! Maximum longitude
      LATEOB(1,JHOUR) = .FALSE. ! Flag for unreliable IFIRST
      LATEOB(2,JHOUR) = .FALSE. ! Flag for unreliable ILAST
    END DO
    NTOTAL = 0                  ! Total number of observations
!
    ITIM1 = 1440*(ICD-1)   ! Century minutes for 0000Z
    ITIM2 = ITIM1 + 1439   ! Century minutes for 2359Z
!
    IPTIM1 = ITIM1 - MOD((ITIM1-NX00Z),NXMIN)  ! First index time
    IPTIM2 = ITIM2 - MOD((ITIM2-NX00Z),NXMIN)  ! Last index time
!
!=======================================================================
!         LOOP OVER INDEX PERIODS COVERING STATISTICS DATE
!=======================================================================
!
DO_JPTIM: &
    DO JPTIM=IPTIM1,IPTIM2,NXMIN
!                                                   Index date and time
!
      CALL DATE13 (JPTIM/1440+1, IT(3), IT(2), IT(1))
      IT(4) = MOD(JPTIM/60,24)  ! Hour
      IT(5) = MOD(JPTIM,60)     ! Minute
!                                                  5-character time tag
      TIMTAG = CHAR(MOD(IT(1),100)) //  &
         CHAR(IT(2)) // CHAR(IT(3)) // CHAR(IT(4)) // CHAR(IT(5))
!
!                                 Find base index block for this period
!
      IXBLK = MOD((JPTIM-NX00Z)/NXMIN,NXBLKS) + NX1
!
!                     Check for old data (IXBLK=0 stops any processing)
!
      NUMR=IXBLK                                                    !4
      CALL METDB_CREAD_DIR (IUNIT,RECORD,LENREC,NUMR,IRC)           !4
      INDXTAG = RECORD(1:5)

      IF ((.NOT.OLDFMT.AND.INDXTAG /= TIMTAG) .OR.  &
          (OLDFMT.AND.INDXTAG(1:2) /= TIMTAG(3:4))) IXBLK = 0
!
!=======================================================================
!           LOOP OVER INDEX BLOCKS FOR THIS INDEX PERIOD
!=======================================================================
!
!
DO_IXBLK: &
      DO WHILE (IXBLK > 0)
!                                                  Read new index block
        NUMR=IXBLK                                                  !4
        CALL METDB_CREAD_DIR (IUNIT,RECORD,LENREC,NUMR,IRC)         !4

!                                                  Read new index block
!                                                 Get number of entries
        IF (OLDFMT) THEN
          I = NXHEAD + MAXNDX*LENTRY
          IF (ICHAR2(RECORD(I+1:I+2)) /= 0) THEN ! Block full
            NTRIES = MAXNDX
          ELSE
            NTRIES = MOD(ICHAR2(RECORD(3:4))-1,MAXNDX)+1
          END IF
        ELSE
          NTRIES = ICHAR2(RECORD(6:7))
        END IF
!
!=======================================================================
!             LOOP OVER INDEX ENTRIES IN INDEX BLOCK
!=======================================================================
!
DO_JENTRY: &
        DO JENTRY=1,NTRIES
          IPT = NXHEAD + (JENTRY-1)*LENTRY ! End of previous entry
!                                          Get observation century hour
          IF (OLDFMT) THEN
            NHOUR = MOD(ICHAR(RECORD(IPT+IHOUR:IPT+IHOUR)),32) + &
                    JPTIM/60
          ELSE
!
            NHOUR = ICHAR(RECORD(IPT+IHOUR:IPT+IHOUR))
            IF (NHOUR < IT(4)) NHOUR = NHOUR + 24
            NHOUR = 24*(JPTIM/1440) + NHOUR
          END IF
!                                                Convert to actual hour
          NHOUR = NHOUR - 24*(ICD-1)
!
!                    Special treatment for aircraft data in old format:
!                                Skip entry if zero data record pointer
          IF (OLDFMT .AND. ACFT) THEN
            IF (ICHAR2(RECORD(IPT+22:IPT+23)) == 0) NHOUR = -1
          END IF
!                                          Check for hour in range 0-23
!
IFLABEL9: &
          IF (NHOUR >= 0 .AND. NHOUR <= 23) THEN
!
!                               Extract number of obs. and receipt time
            LATE = .FALSE.
!                                           Old format
IFLABEL10: &
            IF (OLDFMT) THEN
IFLABEL11: &
              IF (LENTRY == 12) THEN
                IOBS = MOD(ICHAR2(RECORD(IPT+3:IPT+4)),1024)
                ITOR = 3*ICHAR(RECORD(IPT+9:IPT+9))
                IF (ITOR >= 765) LATE = .TRUE.    ! (765=3*255)
              ELSE
                IOBS = ICHAR (RECORD(IPT+12:IPT+12))
                ITOR = ICHAR2(RECORD(IPT+18:IPT+19))
                IF (ITOR > 50000) ITOR = 65536 - ITOR
!
!                             SATOBS: Number of obs can be >255, so can
!                                  overflow into byte 11 of index entry
!
                IF (TYPE(1:5) == 'SATOB') THEN
                  IOBS = IOBS +   &
                         256*MOD(ICHAR(RECORD(IPT+11:IPT+11)),16)
!
!                          AIREPs and AMDARs: count only 1 ob per entry
!
                ELSE IF (ACFT) THEN
                  IOBS = 1
                END IF
              END IF  IFLABEL11
!                                           New format
            ELSE ! New format
              IF (IVERSN == 257) THEN
                IOBS = ICHAR2(RECORD(IPT+13:IPT+14))
                ITOR = ICHAR2(RECORD(IPT+18:IPT+19))
              ELSE
                IOBS = ICHAR2(RECORD(IPT+20:IPT+21))
                ITOR = ICHAR2(RECORD(IPT+25:IPT+26))
              END IF
            END IF  IFLABEL10
!                                                 Update number of obs.
!
            NUMOBS(NHOUR) = NUMOBS(NHOUR) + IOBS
            NTOTAL = NTOTAL + IOBS
!                                            Update receipt time limits
!
            ITOR = ITOR + JPTIM - ITIM1   ! Minutes since 00Z
!
            IF (ITOR < IFIRST(NHOUR)) THEN   ! Earliest so far
              IFIRST(NHOUR) = ITOR
              IF (LATE) LATEOB(1,NHOUR) = .TRUE.
            END IF
!
            IF (ITOR > ILAST(NHOUR)) THEN    ! Latest so far
              ILAST(NHOUR) = ITOR
              IF (LATE) LATEOB(2,NHOUR) = .TRUE.
            END IF
!                                        Extract lat/lon box boundaries
            IPBOX = IPT + IBOX
IFLABEL12: &
            IF ((OLDFMT .AND. LENTRY == 23 .AND.              &
                (IOBS == 1 .AND. TYPE(1:6) /= 'SFERIC')) .OR. &
                (IVERSN == 258 .AND. IOBS == 1)) THEN ! 1 Lat/lon
              LATS = ICHAR2(RECORD(IPBOX  :IPBOX+1))
              LONW = ICHAR2(RECORD(IPBOX+2:IPBOX+3))
!
!             Allow for funny missing data (Hex 3700 = 14080) in SFLOCs
!
              IF (TYPE(1:5) == 'SFLOC' .AND.   &
                  (LATS == 14080 .OR. LONW == 14080)) THEN
                LATS = -32768
                LONW = -32768
              END IF
!
              IF (LATS >= 32768) LATS = LATS - 65536
              IF (LONW >= 32768) LONW = LONW - 65536
              LATN = (LATS+ 9099)/100 -  90
              LATS = (LATS+ 9000)/100 -  90
              LONE = (LONW+18099)/100 - 180
              LONW = (LONW+18000)/100 - 180
            ELSE                                    ! lat/lon box
              LATS =   ICHAR(RECORD(IPBOX  :IPBOX  )) -  90
              LATN =   ICHAR(RECORD(IPBOX+1:IPBOX+1)) -  90
              LONW = 2*ICHAR(RECORD(IPBOX+2:IPBOX+2)) - 180
              LONE = 2*ICHAR(RECORD(IPBOX+3:IPBOX+3)) - 180
            END IF   IFLABEL12
!                                     Check for missing lat and/or long
!                            (-32768 in entry converts to -326 OR -327)
!
            IF (LATN /= -326 .AND. LATS /= -327 .AND.  &
                LONE /= -326 .AND. LONW /= -327) THEN ! OK
!
!                                           Check for dateline crossing
              IF (LONW > LONE) THEN
                LONW = -180
                LONE = 180
              END IF
!                             Update current max and min lats and longs
!
              MAXLAT(NHOUR) = MAX0(MAXLAT(NHOUR),LATN)
              MINLAT(NHOUR) = MIN0(MINLAT(NHOUR),LATS)
              MAXLON(NHOUR) = MAX0(MAXLON(NHOUR),LONE)
              MINLON(NHOUR) = MIN0(MINLON(NHOUR),LONW)
            END IF
          END IF  IFLABEL9
        END DO  DO_JENTRY  ! End of loop over index entries
!
!                                                  Get next index block
        IF (NTRIES == MAXNDX) THEN
          IF (OLDFMT) THEN
            I = NXHEAD + MAXNDX*LENTRY
            IXBLK = ICHAR2(RECORD(I+1:I+2))
            IF (IXBLK > 0) IXBLK = IXBLK + NSQ
          ELSE
            IXBLK = ICHAR3(RECORD(8:10))
          END IF
        ELSE
          IXBLK = 0
        END IF
      END DO  DO_IXBLK   ! End of loop over index blocks for this period
    END DO   DO_JPTIM   ! End of loop over index periods
!
!                                                Close storage data set
    CALL METDB_CCLOSE (IUNIT)                                       !4
!
!=======================================================================
!         GENERATE PRINTED OUTPUT AND UPDATE STATS DATA SET
!=======================================================================
!
!                                                       Loop over hours
DO_JHOUR: &
    DO JHOUR=0,23
      LINE = JHOUR + 7
      CDATA(LINE) = ' '
!                                                 No data for this hour
IFLABEL13: &
      IF (NUMOBS(JHOUR) == 0) THEN
        WRITE (CDATA(LINE),'(I4.2,I12)') JHOUR, NUMOBS(JHOUR)
      ELSE
!                                               Some data for this hour
        I = MOD(IFIRST(JHOUR),1440)
        IHHMM1 = I + 40*(I/60)        ! Earliest T.O.R.
        I = MOD(ILAST(JHOUR),1440)
        IHHMM2 = I + 40*(I/60)        ! Latest T.O.R.
!
        WRITE (CDATA(LINE),'(I4.2,I12,4X,2(3X,I4.4),I6,I5,I7,I5)') &
               JHOUR, NUMOBS(JHOUR), IHHMM1, IHHMM2,  &
               MINLAT(JHOUR), MAXLAT(JHOUR),          &
               MINLON(JHOUR), MAXLON(JHOUR)
!
        IF (IABS(MINLAT(JHOUR)) > 90) CDATA(LINE)(37:40) = ' ---'
        IF (IABS(MAXLAT(JHOUR)) > 90) CDATA(LINE)(42:45) = ' ---'
        IF (IABS(MINLON(JHOUR)) > 180)CDATA(LINE)(49:52) = ' ---'
        IF (IABS(MAXLON(JHOUR)) > 180)CDATA(LINE)(54:57) = ' ---'
!
        IF (LATEOB(1,JHOUR)) CDATA(LINE)(23:23) = '>'
        IF (LATEOB(2,JHOUR)) CDATA(LINE)(30:30) = '>'
      END IF  IFLABEL13
!
      WRITE (CDATA(31),'(A,I11,64X)') 'Total', NTOTAL
    END DO  DO_JHOUR   ! End loop over hours
!                                                 Update stats data set
    IF (STATDSOK) THEN
#if defined (MVS)
      CALL METDB_COPEN(20,"//'"//TRIM(STATNAME)//"'"//CHAR(0),3,IOS)
#else
      CALL METDB_COPEN(20,TRIM(STATNAME)//CHAR(0),3,IOS)
#endif
      DO I=1,4
        CALL METDB_CWRITE(20,CHEAD(I),ISIZE)
      END DO
      DO I=1,672
        CALL METDB_CWRITE(20,CDATA(I),ISIZE)
      END DO
    END IF
!                                                        Printed output
!
    WRITE (6,'(///(T2,A80))') CHEAD, (CDATA(LINE),LINE=2,31)
  END IF  IFLABEL8
!                     Close stats data set and end loop over data types
!
  IF (STATDSOK) CALL METDB_CCLOSE(20)
END DO  DO_TYPE

!                                                        All finished !
STOP
END PROGRAM STATSOBS
