SUBROUTINE INDEX2 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  INDEX2
!
! PURPOSE:     To make an index entry for a BUFR message using index
!              format 2.
!
! DESCRIPTION: 'INDEX2' creates the main part of the index entry for
!              a BUFR message in index format 2 (new format developed
!              for non-satellite data in Spring 2001).  Only details
!              of time of receipt and location in storage need to be
!              added later.
!
!              The BUFR message is partially decoded (in subroutine
!              GETVALS) to provide some details for the entry.
!
!              The ID descriptor(s) looked for are determined by
!              element 9 of ITEMS. If you add a new value, check
!              whether you need to increase the value if MAXCODE.
!
! USAGE:       CALL INDEX2 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)
!
! PARAMETERS:  ('I'=Input, 'O'=Output)
!
!              MESSAGE (I)  (CHARACTER*(*)) BUFR message to be
!                           decoded (starting from "BUFR").
!              FLAGS   (I)  Processing flags (from headers data set).
!              ITEMS   (I)  Processing data (from headers data set).
!              NTIME   (O)  Integer array dimension (6,2) giving
!                           year, month, day, hour, minute and second
!                           of earliest and latest observations.
!              ENTRY   (O)  33-byte index entry, complete except for
!                           time of receipt and location in storage
!                           data set.
!              KODE    (O)  Integer return code as follows:
!
!                              0  Successsful completion
!                             21  VALUES array too small
!                             41  Vital BUFR descriptor not found
!                             42  BUFR bulletin failed to decode
!                             44  Couldn't make index entry
!
! CALLED BY:   BUFREP
!
! CALLS:       CHAR2, GETVALS, LATBOX, ONLAND, TRANGE, INT2CH
!
! HISTORY:     Original version by Brian Barwell, April 2001.
!
! REVISION INFO:
!
! $Workfile: index2.f90$ $Folder: OpSource$
! $Revision: 13$ $Date: 26/06/2012 17:04:10$
!
! CHANGE RECORD:
!
! $Log:
!  13   MetDB_Refresh 1.12        26/06/2012 17:04:10    Brian Barwell   New
!       identifier codes 4, 15 & 16.
!  12   MetDB_Refresh 1.11        25/10/2011 15:36:03    Brian Barwell
!       Parameter MAXOBS increased from 300 to 1000 (for GPSIWV data).
!  11   MetDB_Refresh 1.10        10/10/2011 11:50:27    Brian Barwell   Allow
!       for missing data in integer identifier.
!  10   MetDB_Refresh 1.9         28/09/2011 09:51:51    Brian Barwell   Use
!       IDMAKE for character IDs in index entries.
!  9    MetDB_Refresh 1.8         24/05/2011 15:19:53    Brian Barwell
!       Increase MAXVALS to 10000, delete COMMON block and make VALUES
!       allocatable.
!  8    MetDB_Refresh 1.7         17/03/2011 15:17:22    Sheila Needham
!       Initialise byte 24 of entry.
!  7    MetDB_Refresh 1.6         17/03/2011 10:06:41    Sheila Needham  Split
!       IF test into separate tests and replace logical operators
!  6    MetDB_Refresh 1.5         28/01/2011 13:34:30    Alison Weir     CHAR
!       replaced with INT2CH
!  5    MetDB_Refresh 1.4         28/01/2011 12:49:55    Alison Weir     Minor
!       changes following review - initialise out arguments
!  4    MetDB_Refresh 1.3         28/01/2011 10:01:06    Brian Barwell
!       Modifications for WINDFARM data.
!  3    MetDB_Refresh 1.2         27/01/2011 16:00:05    Richard Weedon  refs
!       to funcs removed
!  2    MetDB_Refresh 1.1         27/01/2011 13:08:22    Richard Weedon
!       Variable declarations and arguments correctly labeled
!  1    MetDB_Refresh 1.0         27/01/2011 13:02:55    Richard Weedon  passes
!        intial compilation test. 
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE char2_mod
USE getvals_mod
USE idmake_mod                                                      !10
USE latbox_mod
USE onland_mod
USE trange_mod
USE int2ch_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN)   ::   MESSAGE   ! BUFR message
LOGICAL,INTENT(IN)            ::   FLAGS(9)  ! Processing flags
                                             ! (from headers data set)
INTEGER,INTENT(IN)            ::   ITEMS(12) ! Processing data
                                             ! (from headers data set)
INTEGER,INTENT(OUT)           ::   NTIME(6,2)! Yr,mon,day,hr,
                                             ! min,sec of ob.time window
CHARACTER(LEN=26),INTENT(OUT) ::   ENTRY     ! Index entry for BUFR mess
INTEGER,INTENT(OUT)           ::   KODE      ! Return code

! Parameters

INTEGER,PARAMETER  ::   MAXVALS=100000    ! Size of VALUES array
INTEGER,PARAMETER  ::   NDES=12           ! Num of descr to look for
INTEGER,PARAMETER  ::   LCSTR=20000       ! Length of CSTR string

! Variables

INTEGER   ::  IDCODE          ! Code for type of ID in message
INTEGER   ::  ILAT            ! Pointer to lats in VALUES array
INTEGER   ::  ILON            ! Pointer to lons in VALUES array
INTEGER   ::  IVAL            ! Pointer to VALUES array
INTEGER   ::  J               ! General loop variable
INTEGER   ::  J1              ! 1st pointer to station number in VALUES
INTEGER   ::  J2              ! 2nd pointer to station number in VALUES
INTEGER   ::  J3              ! 3rd pointer to station number in VALUES
INTEGER   ::  JOB             ! Loop variable for observations
INTEGER   ::  LASTCODE        ! IDCODE value used by last call
INTEGER   ::  LASTLIST        ! NUMLIST value used by last call
INTEGER   ::  LISTDES(NDES,2) ! Lists of descriptors to be decoded
INTEGER   ::  LISTITEM        ! Counter for items in parameter list
INTEGER   ::  MAXCODE = 16    ! Highest ID code (except for '99')   !13
INTEGER   ::  MAXLAT          ! Highest latitude of obs in message
INTEGER   ::  MAXLON          ! Highest longitude of obs in message
INTEGER   ::  MDES            ! Number of descriptors to look for
INTEGER   ::  MINLAT          ! Lowest latitude of obs in message
INTEGER   ::  MINLON          ! Lowest longitude of obs in message
INTEGER   ::  NCHAR           ! Number of characters in identifier
INTEGER   ::  NPOS(NDES)      ! Location of params in BUFR sequence
INTEGER   ::  NSKIP           ! Number of ID characters to skip
INTEGER   ::  NSTN            ! Station or other platform number
INTEGER   ::  NUMLIST         ! Number of list to be used for decode
INTEGER   ::  NUMOBS          ! Number of observations in BUFR message

REAL      ::  BOX(4)    ! Boundaries of lat/long box enclosing obs
REAL, ALLOCATABLE :: VALUES(:)  ! Decoded values from BUFR message

LOGICAL   ::   FIRST    ! .TRUE. if first call to this subroutine
LOGICAL   ::   FOUND    ! Flag for parameter location found
LOGICAL   ::   MUSTHAVE(NDES)  ! Flags for values needed for index
LOGICAL   ::   SEA             ! Flag for at least one ob. over the sea

CHARACTER(LEN=LCSTR)   ::   CSTR  ! Character elements from GETVALS
CHARACTER(LEN=8)       ::   ID    ! Platform identification for ob  !10
CHARACTER(LEN=8)       ::   PLAT  ! Platform identification for index

!                                                       Saved variables

SAVE FIRST, LISTDES, MUSTHAVE, LASTLIST, LASTCODE, MDES, NCHAR, VALUES

!                                                   Data initialisation
DATA LISTDES &
!        YEAR  MNTH   DAY  HOUR   MIN   SEC   LAT  LONG  SURF  IDs
       / 1025, 1026, 1027, 1028, 1029, 1030, 1282, 1538, 2060, 3*0,&
!        SAME AS ABOVE BUT WITH HIGH ACCURACY LATITUDE & LONGITUDE
         1025, 1026, 1027, 1028, 1029, 1030, 1281, 1537, 2060, 3*0/

DATA MUSTHAVE /5*.TRUE., .FALSE., 2*.TRUE., .FALSE., 3*.TRUE./
DATA FIRST /.TRUE./

IF (FIRST) THEN
!                     Set variables for storing data from previous call
  LASTCODE = -1
  LASTLIST = -1
!                                  Allocate work space for BUFR decodes
  ALLOCATE (VALUES(1:MAXVALS))
  FIRST = .FALSE.
END IF

ENTRY = ' '
KODE  = 0

!-----------------------------------------------------------------------
!     GET LIST OF WANTED PARAMETERS FROM BUFR MESSAGE
!-----------------------------------------------------------------------
!                          Use list with correct lat & long descriptors
IF (FLAGS(5)) THEN
  NUMLIST = 2      ! High res. lat & long
ELSE
  NUMLIST = 1      ! Coarse lat & long
END IF
!                          Check code for type of identifier in message
IDCODE = ITEMS(9)
!                            Reset LISTDES & MUSTHAVE if new code value
!
!  Details of descriptor used for ID (and some other details) are
!  dependent on the value of IDCODE obtained from the headers data
!  set as follows:
!
!    IDCODE  Descriptor      ID type            Data type(s)
!    ------  ----------      -------            ------------
!       1    001001/2    WMO block & station    OZONEPRF, RASS
!       2    001005      Buoy/platform ID       WAVENET
!       3    001087      Marine platform ID     ARGO
!       4   001003/20/5  WMO regn./sub-area/ID  TRIAXYS             !13
!      10    001008      Aircraft ID            AMDAR, TAMDAR
!      11    001015      Site name (bytes 5+)   GPSIWV
!      12    001200      Road site ID           OPENROAD
!      13    001216      GEMS station ID        AIRQAL
!      14    001019      Long site name         Wind farm data
!      15    001011      Ship/mobile stn. ID.   Ship/mobile TEMPs   !13
!      16    001006      Aircraft flight no.    Dropsondes          !13
!      99     None                              Radar rainfall
!
!  Codes 1-9 are integers; the rest are character codes.

if_constr1 : &
IF (IDCODE /= LASTCODE .OR. NUMLIST /=LASTLIST) THEN

  IF (LASTCODE == 12) THEN
    MUSTHAVE(7) = .TRUE.
    MUSTHAVE(8) = .TRUE.
  END IF
  MDES = 10      ! Usually 10 descriptors to look for
  NCHAR = 8      ! Most IDs have 8 characters

  if_constr2 : &
  IF (IDCODE == 1) THEN         ! OZONEPRF, RASS
    LISTDES(10,NUMLIST) = 257   ! 001001 - WMO block no.
    LISTDES(11,NUMLIST) = 258   ! 001002 - WMO station no.
    MDES = 11

  ELSE IF (IDCODE == 2) THEN    ! WAVENET
    LISTDES(10,NUMLIST) = 261   ! 001005 - Buoy/platform ID

  ELSE IF (IDCODE == 3) THEN    ! ARGO
    LISTDES(10,NUMLIST) = 343   ! 001087 - Marine platform ID

  ELSE IF (IDCODE == 4) THEN    ! TRIAXYS                           !13
    LISTDES(10,NUMLIST) = 259   ! 001003 - WMO region number        !13
    LISTDES(11,NUMLIST) = 276   ! 001020 - WMO region sub-area      !13
    LISTDES(12,NUMLIST) = 261   ! 001005 - Buoy/platform ID         !13
    MDES = 12                                                       !13

  ELSE IF (IDCODE == 10) THEN   ! TAMDAR
    LISTDES(10,NUMLIST) = 264   ! 001008 - Aircraft ID

  ELSE IF (IDCODE == 11) THEN   ! GPSIWV
    LISTDES(10,NUMLIST) = 271   ! 001015 - Site name
    NCHAR = 4                   ! Keep only 4 characters

  ELSE IF (IDCODE == 12) THEN   ! OPENROAD
    LISTDES(10,NUMLIST) = 456   ! 001200 - Road site ID
    MUSTHAVE(7) = .FALSE.       ! No latitude ...
    MUSTHAVE(8) = .FALSE.       ! or longitude

  ELSE IF (IDCODE == 13) THEN   ! AIRQAL
    LISTDES(10,NUMLIST) = 472   ! 001216 - GEMS station ID
    NCHAR = 6                   ! 6-character IDs

  ELSE IF (IDCODE == 14) THEN   ! Wind Farm data
    LISTDES(10,NUMLIST) = 275   ! 001019 - Long site name

  ELSE IF (IDCODE == 15) THEN   ! Ship & mobile TEMPS               !13
    LISTDES(10,NUMLIST) = 267   ! 001011 - Station identifier       !13

  ELSE IF (IDCODE == 16) THEN   ! Dropsondes                        !13
    LISTDES(10,NUMLIST) = 262   ! 001006 - Aircraft flight number   !13

  ELSE      ! (e.g. IDCODE=99)  ! Radar rainfall
    MDES = 9                    ! No identifier for index
  END IF if_constr2
!                                              Store data for next call
  LASTCODE = IDCODE
  LASTLIST = NUMLIST
END IF if_constr1
!                                        Partial decode of BUFR message

CALL GETVALS (LISTDES(1,NUMLIST), MDES, MESSAGE, MAXVALS, &
              VALUES, CSTR, NPOS, NUMOBS, KODE)
if_constr3 : &
IF (KODE == 0) THEN
!                          Check that essential parameters were located
  LISTITEM = 0
  FOUND = .TRUE.
  DO WHILE (FOUND .AND. LISTITEM.LT.MDES)
    LISTITEM = LISTITEM + 1
    IF (MUSTHAVE(LISTITEM)) FOUND = NPOS(LISTITEM).GT.0
  END DO
!                                  Warning if something vital not found
  if_constr4 : &
  IF (.NOT.FOUND) THEN
    WRITE (6,'(T5,A,T15,A,I3,A)') 'INDEX2:', 'Vital descriptor',&
                    LISTITEM, ' not found in BUFR sequence.'
    KODE = 41
!                             Check for expected number of observations
!                                  (but store anyway even if different)
  ELSE
    IF (ITEMS(8) > 0 .AND. NUMOBS /= ITEMS(8))&
            WRITE (6,'(T5,A,T15,A,I5,A,I5,A)') 'INDEX2:',           &
                     'Number of observations in message =', NUMOBS, &
                     ' instead of', ITEMS(8), ' expected.'

!-----------------------------------------------------------------------
!     SET LAND/SEA FLAG (SET TO 'SEA' IF AT LEAST ONE SEA POINT).
!     IF SURFACE TYPE IS NOT AVAILABLE, CALL "ONLAND".
!-----------------------------------------------------------------------

    if_constr5 : &
    IF (NPOS(7) > 0) THEN
      ILAT = (NPOS(7)-1)*NUMOBS
      ILON = (NPOS(8)-1)*NUMOBS
      IF (NPOS(9) > 0) THEN
        IVAL = (NPOS(9)-1)*NUMOBS
      ELSE
        IVAL = -1
      END IF
      SEA = .FALSE.
!                                    Loop over observations in bulletin
      JOB = 1
      do_constr1 : &
      DO WHILE (.NOT.SEA .AND. JOB <= NUMOBS)

!                             Check land/sea flag '008012' if available
        IF (IVAL >= 0) THEN
          IF (VALUES(IVAL+JOB) >= 0.0) THEN
            SEA = NINT(VALUES(IVAL+JOB)) == 1
          END IF
!                                 Otherwise check 1-degree land/sea map

        ELSE IF (ABS(VALUES(ILAT+JOB)) <= 90.0 .AND.&
                       ABS(VALUES(ILON+JOB)) <= 180.0) THEN
          SEA = .NOT.ONLAND(VALUES(ILAT+JOB),VALUES(ILON+JOB))
        END IF
!                                Increment counter for next observation
        JOB = JOB + 1
      END DO do_constr1
!                              If no lat/long available, assume on land
    ELSE
      SEA = .FALSE.
    END IF if_constr5

!-----------------------------------------------------------------------
!     MAKE PLATFORM IDENTIFIER ('PLAT') FOR INDEX ENTRY
!-----------------------------------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (a) Integer identifiers - numerical value is NSTN
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    if_constr6 : &
    IF (IDCODE < 10) THEN     ! Integer ID
      PLAT = ' '                                                    !10
      J1 = (NPOS(10)-1)*NUMOBS                                      !10
      IF (MDES >= 11) J2 = (NPOS(11)-1)*NUMOBS                      !13
      IF (MDES >= 12) J3 = (NPOS(12)-1)*NUMOBS                      !13
!                                                         Loop over obs
      DO JOB=1,NUMOBS                                               !10
        NSTN = NINT(VALUES(J1+JOB))                                 !10
!                                      Convert station ID to characters

        IF (NSTN < 0) THEN                       ! Missing ID       !11
          ID = ' '                                                  !11
        ELSE IF (IDCODE == 1) THEN               ! WMO blk/stn      !11
          NSTN = 1000*NSTN + NINT(VALUES(J2+JOB))                   !10
          WRITE (ID,'(I5.5,3X)') NSTN                               !10
        ELSE IF (IDCODE == 2) THEN               ! 5 digits         !10
          WRITE (ID,'(I5.5,3X)') NSTN                               !10
        ELSE IF (IDCODE == 4) THEN               ! Regn/sub-area/ID !13
          NSTN = 1000000*NSTN + &                                   !13
                 100000*NINT(VALUES(J2+JOB)) + NINT(VALUES(J3+JOB)) !13
          WRITE (ID,'(I7.7,1X)') NSTN                               !13
        ELSE                                     ! 8 digits         !10
          WRITE (ID,'(I8.8)') NSTN                                  !10
        END IF                                                      !10
!                                            Loop over characters in ID
!                                 Put '*' in IDNDX if characters differ
        IF (JOB > 1) THEN                                           !10
          DO J=1,8                                                  !10
            IF (ID(J:J) /= PLAT(J:J)) PLAT(J:J) = '*'               !10
          END DO                                                    !10
        ELSE                                                        !10
          PLAT = ID                                                 !10
        END IF                                                      !10
      END DO                                                        !10

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (b) Character identifiers
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ELSE IF (IDCODE >= 10 .AND. IDCODE <= MAXCODE) THEN             !13
      IF (IDCODE == 11) THEN

! The next statement is used by GPSIWV data which has a 'station or
! site name' instead of a WMO station number. The coding is 'cccc-pppp'
! for the first 9 characters (the other 11 being unused) where 'cccc'
! is the IGS station name and 'pppp' is string of up to 4 characters
! (followed by blanks if necessary) representng the processing centre.
! A message can contain data from different stations but they should
! all have the same 'pppp', so this will be put in the index entry.

        NSKIP = 5   ! Skip 5 characters of ID
      ELSE
        NSKIP = 0   ! Don't skip any characters
      END IF
!                                               Make ID for index entry
      IVAL = (NPOS(10)-1)*NUMOBS + 1                                !10
      CALL IDMAKE (VALUES(IVAL), CSTR, NUMOBS, NSKIP, PLAT)         !10

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (c) No identifier suitable for indexing
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    ELSE          ! (e.g. IDCODE=99)
      PLAT = ' '
    END IF if_constr6

!-----------------------------------------------------------------------
!     GET DATE & TIME FOR OLDEST AND MOST RECENT OBSERVATIONS
!     CHECK DATE/TIME FOR MISSING DATA (BIG NEGATIVE VALUE)
!-----------------------------------------------------------------------

    CALL TRANGE (VALUES, NUMOBS, NPOS(1), NTIME)

    DO J=1,6
      IF (NTIME(J,1).LT.0 .OR. NTIME(J,2).LT.0) KODE = 44
    END DO ! J

!-----------------------------------------------------------------------
!     GET BOUNDARIES OF LAT/LONG BOX ENCLOSING OBSERVATIONS (OR ACTUAL
!     LAT & LONG IF ONLY 1 OB.). ALSO CHECK VALUES FOR MISSING DATA
!-----------------------------------------------------------------------

    if_constr9 : &
    IF (NPOS(7) > 0) THEN
      if_constr10 : &
      IF (NUMOBS == 1) THEN
!                                                             Only 1 ob
        MINLAT = NINT(100.0*VALUES(ILAT+1))
        MINLON = NINT(100.0*VALUES(ILON+1))
!                                                    Check for bad data
        IF (IABS(MINLAT) > 9000 .OR.&
                  IABS(MINLON) > 18000) KODE = 44

!                                Modify negative values for index entry

        IF (MINLAT < 0) MINLAT = MINLAT + 65536
        IF (MINLON < 0) MINLON = MINLON + 65536
      ELSE
!                                   More than 1 ob (Calculation done in
!                                integers to control round-offs on HP.)

        CALL LATBOX (VALUES, NUMOBS, NPOS(7), BOX)

        IF (ABS(BOX(1)) >  90.0 .OR. ABS(BOX(2)) >  90.0 .OR. &
            ABS(BOX(3)) > 180.0 .OR. ABS(BOX(4)) > 180.0) THEN
          KODE = 44
        ELSE
          MINLAT = (NINT(1.0E5*BOX(1))+ 9000000)/100000
          MAXLAT = (NINT(1.0E5*BOX(2))+ 9099999)/100000
          MINLON = (NINT(1.0E5*BOX(3))+18000000)/200000
          MAXLON = (NINT(1.0E5*BOX(4))+18199999)/200000
        END IF
      END IF if_constr10
    ELSE
      MINLAT = 0
      MINLON = 0
      MAXLAT = 0
      MAXLON = 0
    END IF if_constr9

!-----------------------------------------------------------------------
!     IF ALL OK, MAKE UP AS MUCH OF THE INDEX ENTRY AS POSSIBLE
!-----------------------------------------------------------------------
    if_constr11 : &
    IF (KODE <= 10) THEN
      IVAL = 0
      IF (SEA) IVAL = 128

      ENTRY(1:1)   = INT2CH(IVAL)       ! Land/sea flag
      ENTRY(2:9)   = PLAT               ! Platform ID
      ENTRY(10:10) = INT2CH(NTIME(4,1)) ! Start hour
      ENTRY(11:11) = INT2CH(NTIME(5,1)) ! Start minute
      ENTRY(12:12) = INT2CH(NTIME(6,1)) ! Start second
      ENTRY(13:13) = INT2CH(NTIME(4,2)) ! End hour
      ENTRY(14:14) = INT2CH(NTIME(5,2)) ! End minute
      ENTRY(15:15) = INT2CH(NTIME(6,2)) ! End second
!
      IF (NUMOBS == 1) THEN
        ENTRY(16:17) = CHAR2(MINLAT)    ! Latitude (1 ob.)
        ENTRY(18:19) = CHAR2(MINLON)    ! Longitude (1 ob.)
      ELSE
        ENTRY(16:16) = INT2CH(MINLAT)   ! Minimum latitude
        ENTRY(17:17) = INT2CH(MAXLAT)   ! Maximum latitude
        ENTRY(18:18) = INT2CH(MINLON)   ! Minimum longitude
        ENTRY(19:19) = INT2CH(MAXLON)   ! Maximum longitude
      END IF

      ENTRY(20:21) = CHAR2(NUMOBS)      ! Number of obs.
      ENTRY(22:22) = INT2CH(ITEMS(3))   ! Index entry byte
      ENTRY(23:23) = CHAR(0)            ! Spare
      ENTRY(24:24) = CHAR(0)            ! Default = no data overflows
!     ENTRY(25:33)   Can't be set yet
    END IF if_constr11
  END IF if_constr4
END IF if_constr3
!                                             Return to calling program
RETURN
END
