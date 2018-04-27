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
! CALLS:       CHAR2, GETVALS, LATBOX, ONLAND, TRANGE
!
! HISTORY:     Original version by Brian Barwell, April 2001.
!
! REVISION INFO:
!
! $Workfile: index2.f$ $Folder: pre_refresh$
! $Revision: 7$ $Date: 28/04/2009 11:03:38$
!
! CHANGE RECORD:
!
! $Log:
!  7    Met_DB_Project 1.6         28/04/2009 11:03:38    Brian Barwell
!       Section dealing with platform identifiers rewritten.
!  6    Met_DB_Project 1.5         03/02/2009 12:13:32    Brian Barwell
!       Replace "END IF" which was accidentally deleted last time.
!  5    Met_DB_Project 1.4         03/02/2009 09:39:14    Richard Weedon  GEMS
!       Platform Identifier added to write statement, NESDIS snow cover
!       construct removed
!  4    Met_DB_Project 1.3         30/01/2009 09:57:51    Richard Weedon  GEMS
!       Identifier added for AQ Obs
!  3    Met_DB_Project 1.2         14/10/2008 12:19:35    Sheila Needham
!       Marine platform identifiers added
!  2    Met_DB_Project 1.1         23/05/2007 11:34:54    Alison Weir
!       Amended for Open Road data.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:59    Sheila Needham  
! $
! Revision 2.5  2004/12/06 12:16:33  usmdb
! 2.5.  20 December 2004.  Brian Barwell.  Remedy CHG009245.
! Add new section to look for 008001 (aircraft ID number - for
! TAMDARs) and set ID to blanks in index if there is no identifier
! in the BUFR message (for SNOW).
!
! Revision 2.4  2004/01/06 11:12:19  usmdb
! 2.4.  19 January 2004.  Brian Barwell.  Change 115/03.
! Modify for use with data having WMO station number (BUFR 001005).
!
! Revision 2.3  2002/10/07  16:01:17  16:01:17  usmdb (Generic MetDB account)
! Repositioned lat/long check to prevent very large numbers being
! generated when there was missing lat/long. These large numbers
! caused memory problems in the HPMDB - S.Cox
!
! Revision 2.2  2001/10/03  15:08:20  15:08:20  usmdb (Generic MetDB account)
! 2.2.  15 October 2001.  Brian Barwell.  Change 127/01.
! Modify calculation of lat/long box to avoid out-of-bounds errors
! on HP.  Also correct bug in setting of land/sea flag.
!
! Revision 2.1  2001/07/18  10:03:11  10:03:11  usmdb (Generic MetDB account)
! 17 July 2001.  Brian Barwell.  Change 100/01.
! Size of CSTR increased and common block renamed.
!
! Revision 2.0  2001/06/06  10:19:52  10:19:52  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                            Parameters
      INTEGER    MAXVALS      ! Size of VALUES array
      INTEGER    MAXOBS       ! Maximum number of observations
      INTEGER    NDES         ! Number of descriptors to look for   !2.5
      INTEGER    LCSTR        ! Length of CSTR string               !2.1
      PARAMETER (MAXVALS=20000)    ! Up to 20000 values
      PARAMETER (MAXOBS=300)       ! Up to 300 obs                  !2.1
      PARAMETER (NDES=12)          ! Max. descriptors to look for    !7
      PARAMETER (LCSTR=20*MAXOBS)  ! Length of CSTR string          !2.1
!                                                             Variables
      INTEGER KODE            ! Return code
      INTEGER IDCODE          ! Code for type of ID in message       !7
      INTEGER ILAT, ILON      ! Pointer to lats & lons in VALUES array
      INTEGER ITEMS(12)       ! Processing data (from headers data set)
      INTEGER IVAL            ! Pointer to VALUES array
      INTEGER J               ! General loop variable
      INTEGER J1, J2          ! Byte numbers for CSTR array          !7
      INTEGER JOB             ! Loop variable for observations
      INTEGER LASTCODE        ! IDCODE value used by last call       !7
      INTEGER LASTLIST        ! NUMLIST value used by last call      !7
      INTEGER LISTDES(NDES,2) ! Lists of descriptors to be decoded  !2.5
      INTEGER LISTITEM        ! Counter for items in parameter list
      INTEGER MAXLAT          ! Highest latitude of obs in message
      INTEGER MAXLON          ! Highest longitude of obs in message
      INTEGER MDES            ! Number of descriptors to look for    !7
      INTEGER MINLAT          ! Lowest latitude of obs in message
      INTEGER MINLON          ! Lowest longitude of obs in message
      INTEGER NCHAR           ! Number of characters in identifier   !7
      INTEGER NPOS(NDES)      ! Location of params in BUFR sequence !2.5
      INTEGER NSKIP           ! Number of ID characters to skip      !7
      INTEGER NSTN            ! WMO station number
      INTEGER NTIME(6,2)      ! Yr,mon,day,hr,min,sec of ob. time window
      INTEGER NUMLIST         ! Number of list to be used for decode
      INTEGER NUMOBS          ! Number of observations in BUFR message

      REAL BOX(4)             ! Boundaries of lat/long box enclosing obs
      REAL STN                ! WMO blk or stn number from BUFR message
      REAL VALUES(MAXVALS)    ! Decoded values from BUFR message

      LOGICAL FIRST           ! .TRUE. if first call to this subroutine
      LOGICAL FLAGS(9)        ! Processing flags (from headers data set)
      LOGICAL FOUND           ! Flag for parameter location found
      LOGICAL MUSTHAVE(NDES)  ! Flags for values needed for index   !2.5
      LOGICAL SAME            ! Flag for obs having same station id
      LOGICAL SEA             ! Flag for at least one ob. over the sea

      CHARACTER*(LCSTR) CSTR  ! Character elements from GETVALS     !2.1
      CHARACTER*26  ENTRY     ! Index entry for BUFR message
      CHARACTER*80  HEAD      ! For revision information             !4
      CHARACTER*(*) MESSAGE   ! BUFR message
      CHARACTER*8   PLAT      ! Platform identification             !2.5

!                                 Common block (for dynamic allocation)
      COMMON /COMV2/ VALUES, CSTR                                   !2.1
!                                                    External functions

      LOGICAL     ONLAND     ! Function to set land sea flag
      CHARACTER*2 CHAR2      ! I*4 to C*2 conversion routine

!                                                       Saved variables

      SAVE FIRST, LISTDES, MUSTHAVE, LASTLIST, LASTCODE, MDES, NCHAR !7

!                                                   Data initialisation
      DATA LISTDES                                                  !2.4
!        YEAR  MNTH   DAY  HOUR   MIN   SEC   LAT  LONG  SURF  IDs   !7
     & / 1025, 1026, 1027, 1028, 1029, 1030, 1282, 1538, 2060, 3*0,  !7
!        SAME AS ABOVE BUT WITH HIGH ACCURACY LATITUDE & LONGITUDE   !7
     &   1025, 1026, 1027, 1028, 1029, 1030, 1281, 1537, 2060, 3*0/  !7

      DATA MUSTHAVE /5*.TRUE., .FALSE., 2*.TRUE., .FALSE., 3*.TRUE./ !7
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
        HEAD = '$Workfile: index2.f$ ' //
     &         '$Revision: 7$ $Date: 28/04/2009 11:03:38$'
        FIRST = .FALSE.
!                     Set variables for storing data from previous call
        LASTCODE = -1                                                !7
        LASTLIST = -1                                                !7
      END IF

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
      IDCODE = ITEMS(9)                                              !7
!                            Reset LISTDES & MUSTHAVE if new code value

!  Details of descriptor used for ID (and some other details) are    !7
!  dependent on the value of IDCODE obtained from the headers data   !7
!  set as follows:                                                   !7
!                                                                    !7
!    IDCODE  Descriptor      ID type            Data type(s)         !7
!    ------  ----------      -------            ------------         !7
!       1    001001/2    WMO block & station    OZONEPRF, RASS       !7
!       2    001005      Buoy/platform ID       WAVENET              !7
!       3    001087      Marine platform ID     ARGO                 !7
!      10    001008      Aircraft ID            TAMDAR               !7
!      11    001015      Short site name        GPSIWV               !7
!      12    001200      Road site ID           OPENROAD             !7
!      13    001216      GEMS station ID        AIRQAL               !7
!      99     None                              Radar rainfall       !7
!                                                                    !7
!  Codes 1-9 are integers; the rest are character codes.             !7

      IF (IDCODE.NE.LASTCODE .OR. NUMLIST.NE.LASTLIST) THEN          !7

        IF (LASTCODE.EQ.12) THEN                                     !7
          MUSTHAVE(7) = .TRUE.                                       !7
          MUSTHAVE(8) = .TRUE.                                       !7
        END IF                                                       !7
        MDES = 10      ! Usually 10 descriptors to look for          !7
        NCHAR = 8      ! Most IDs have 8 characters                  !7

        IF (IDCODE.EQ.1) THEN         ! OZONEPRF, RASS               !7
          LISTDES(10,NUMLIST) = 257   ! 001001 - WMO block no.       !7
          LISTDES(11,NUMLIST) = 258   ! 001002 - WMO station no.     !7
          MDES = 11                                                  !7

        ELSE IF (IDCODE.EQ.2) THEN    ! WAVENET                      !7
          LISTDES(10,NUMLIST) = 261   ! 001005 - Buoy/platform ID    !7

        ELSE IF (IDCODE.EQ.3) THEN    ! ARGO                         !7
          LISTDES(10,NUMLIST) = 343   ! 001087 - Marine platform ID  !7

        ELSE IF (IDCODE.EQ.10) THEN   ! TAMDAR                       !7
          LISTDES(10,NUMLIST) = 264   ! 001008 - Aircraft ID         !7

        ELSE IF (IDCODE.EQ.11) THEN   ! GPSIWV                       !7
          LISTDES(10,NUMLIST) = 271   ! 001015 - Short site name     !7
          NCHAR = 4                   ! Keep only 4 characters       !7

        ELSE IF (IDCODE.EQ.12) THEN   ! OPENROAD                     !7
          LISTDES(10,NUMLIST) = 456   ! 001200 - Road site ID        !7
          MUSTHAVE(7) = .FALSE.       ! No latitude ...              !7
          MUSTHAVE(8) = .FALSE.       ! or longitude                 !7

        ELSE IF (IDCODE.EQ.13) THEN   ! AIRQAL                       !7
          LISTDES(10,NUMLIST) = 472   ! 001216 - GEMS station ID     !7
          NCHAR = 6                   ! 6-character IDs              !7

        ELSE      ! (e.g. IDCODE=99)  ! Radar rainfall               !7
          MDES = 9                    ! No identifier for index      !7
        END IF                                                       !7
!                                              Store data for next call
        LASTCODE = IDCODE                                            !7
        LASTLIST = NUMLIST                                           !7
      END IF                                                         !7
!                                        Partial decode of BUFR message

      CALL GETVALS (LISTDES(1,NUMLIST), MDES, MESSAGE, MAXVALS,      !7
     &              VALUES, CSTR, NPOS, NUMOBS, KODE)

      IF (KODE.EQ.0) THEN
!                          Check that essential parameters were located
        LISTITEM = 0
        FOUND = .TRUE.
        DO WHILE (FOUND .AND. LISTITEM.LT.MDES)                      !7
          LISTITEM = LISTITEM + 1
          IF (MUSTHAVE(LISTITEM)) FOUND = NPOS(LISTITEM).GT.0
        END DO
!                                  Warning if something vital not found
        IF (.NOT.FOUND) THEN
          WRITE (6,'(T5,A,T15,A,I3,A)') 'INDEX2:', 'Vital descriptor',
     &              LISTITEM, ' not found in BUFR sequence.'
          KODE = 41
!                             Check for expected number of observations
!                                  (but store anyway even if different)
        ELSE
          IF (ITEMS(8).GT.0 .AND. NUMOBS.NE.ITEMS(8))
     &      WRITE (6,'(T5,A,T15,A,I5,A,I5,A)') 'INDEX2:',
     &               'Number of observations in message =', NUMOBS,
     &               ' instead of', ITEMS(8), ' expected.'

!-----------------------------------------------------------------------
!     SET LAND/SEA FLAG (SET TO 'SEA' IF AT LEAST ONE SEA POINT).
!     IF SURFACE TYPE IS NOT AVAILABLE, CALL "ONLAND".
!-----------------------------------------------------------------------

          IF (NPOS(7).GT.0) THEN                                     !7
            ILAT = (NPOS(7)-1)*NUMOBS                                !7
            ILON = (NPOS(8)-1)*NUMOBS                                !7
            IF (NPOS(9).GT.0) THEN                                   !7
              IVAL = (NPOS(9)-1)*NUMOBS                              !7
            ELSE
              IVAL = -1
            END IF
            SEA = .FALSE.
!                                    Loop over observations in bulletin
            JOB = 1
            DO WHILE (.NOT.SEA .AND. JOB.LE.NUMOBS)

!                             Check land/sea flag '008012' if available

              IF (IVAL.GE.0 .AND. VALUES(IVAL+JOB).GE.0.0) THEN
                SEA = NINT(VALUES(IVAL+JOB)).EQ.1

!                                 Otherwise check 1-degree land/sea map

              ELSE IF (ABS(VALUES(ILAT+JOB)).LE.90.0 .AND.
     &                 ABS(VALUES(ILON+JOB)).LE.180.0) THEN
                SEA = .NOT.ONLAND(VALUES(ILAT+JOB),VALUES(ILON+JOB))
              END IF
!                                Increment counter for next observation
              JOB = JOB + 1
            END DO
!                              If no lat/long available, assume on land
          ELSE                                                       !2
            SEA = .FALSE.                                            !2
          END IF                                                     !2

!-----------------------------------------------------------------------
!     CHECK THAT ALL OBS ARE FOR THE SAME STATION AND GET ITS ID.
!-----------------------------------------------------------------------

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (a) Integer identifiers                                        !7
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          IF (IDCODE.LT.10) THEN     ! Integer ID                    !7
            NSTN = 0                                                 !7
            SAME = .TRUE.                                            !7
!                                  Loop over ID descriptors (may be >1)
            DO J=10,MDES                                             !7
              IVAL = (NPOS(J)-1)*NUMOBS                              !7
              STN = VALUES(IVAL+1)                                   !7
              JOB = 2                                                !7
!                                                 Check the station IDs
              DO WHILE (SAME .AND. JOB.LE.NUMOBS)                    !7
                SAME = VALUES(IVAL+JOB) .EQ. STN                     !7
                JOB = JOB + 1                                        !7
              END DO                                                 !7
!                                   Make station number for index entry

              NSTN = 1000*NSTN + NINT(STN)                           !7
            END DO                                                   !7
!                                      Problem if more than one station
            IF (.NOT.SAME) THEN                                      !7
              WRITE (6,'(T5,A,T15,A,I8)') 'INDEX2:',                 !7
     &                 'Station numbers not all the same.', IDCODE   !7
              PLAT = ' >1 '                                          !7
!                                       Convert to character station ID

            ELSE IF (NSTN.LT.0) THEN    ! Bad or missing ID          !7
              PLAT = ' '                                             !7
            ELSE IF (IDCODE.EQ.1 .OR. IDCODE.EQ.2) THEN  ! 5 digits  !7
              WRITE (PLAT,'(I5.5,3X)') NSTN                          !7
            ELSE                                         ! 8 digits  !7
              WRITE (PLAT,'(I8.8)') NSTN                             !7
            END IF                                                   !7

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (b) Character identifiers                                      !7
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          ELSE IF (IDCODE.GE.10 .AND. IDCODE.LE.13) THEN             !7
            IF (IDCODE.EQ.11) THEN                                   !7

! The next statement is used by GPSIWV data which has a 'station or
! site name' instead of a WMO station number. The coding is 'cccc-pppp'
! for the first 9 characters (the other 11 being unused) where 'cccc'
! is the IGS station name and 'pppp' is string of up to 4 characters
! (followed by blanks if necessary) representng the processing centre.
! A message can contain data from different stations but they should
! all have the same 'pppp', so this will be put in the index entry.

              NSKIP = 5   ! Skip 5 characters of ID                  !7
            ELSE                                                     !7
              NSKIP = 0   ! Don't skip any characters                !7
            END IF                                                   !7
!                                           Get ID of first observation
            IVAL = (NPOS(10)-1)*NUMOBS                               !7
            J1 = MOD(NINT(VALUES(IVAL+1)),65536) + NSKIP             !7
            J2 = J1 + NCHAR - 1                                      !7
            PLAT = CSTR(J1:J2)         ! First Id                    !7
            SAME = .TRUE.                                            !7
            JOB = 2                                                  !7
!                                               Check with other ob IDs
            DO WHILE (SAME .AND. JOB.LE.NUMOBS)                      !7
              J1 = MOD(NINT(VALUES(IVAL+JOB)),65536) + NSKIP         !7
              J2 = J1 + NCHAR - 1                                    !7
              SAME = CSTR(J1:J2) .EQ. PLAT(1:NCHAR)                  !7
              JOB = JOB + 1                                          !7
            END DO                                                   !7
!                                           Problem if not all the same
            IF (.NOT.SAME) THEN                                      !7
              PLAT = ' >1 '                                          !7
              WRITE (6,'(T5,A,T15,A,I8)') 'INDEX2:',                 !7
     &                 'Platform IDs not all the same.', IDCODE      !7
            END IF                                                   !7

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     (c) No identifier suitable for indexing                        !7
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          ELSE          ! (e.g. IDCODE=99)                           !7
            PLAT = ' '                                               !7
          END IF                                                     !7

!-----------------------------------------------------------------------
!     GET DATE & TIME FOR OLDEST AND MOST RECENT OBSERVATIONS
!     CHECK DATE/TIME FOR MISSING DATA (BIG NEGATIVE VALUE)
!-----------------------------------------------------------------------

          CALL TRANGE (VALUES, NUMOBS, NPOS(1), NTIME)               !7

          DO J=1,6
            IF (NTIME(J,1).LT.0 .OR. NTIME(J,2).LT.0) KODE = 44
          END DO ! J

!-----------------------------------------------------------------------
!     GET BOUNDARIES OF LAT/LONG BOX ENCLOSING OBSERVATIONS (OR ACTUAL
!     LAT & LONG IF ONLY 1 OB.). ALSO CHECK VALUES FOR MISSING DATA
!-----------------------------------------------------------------------

          IF (NPOS(7).GT.0) THEN                                     !7
            IF (NUMOBS.EQ.1) THEN
!                                                             Only 1 ob
              MINLAT = NINT(100.0*VALUES(ILAT+1))
              MINLON = NINT(100.0*VALUES(ILON+1))
!                                                    Check for bad data
              IF (IABS(MINLAT).GT.9000 .OR.
     &            IABS(MINLON).GT.18000) KODE = 44

!                                Modify negative values for index entry

              IF (MINLAT.LT.0) MINLAT = MINLAT + 65536
              IF (MINLON.LT.0) MINLON = MINLON + 65536
            ELSE
!                              More than 1 ob (Calculation done in  !2.2
!                           integers to control round-offs on HP.)  !2.2

              CALL LATBOX (VALUES, NUMOBS, NPOS(7), BOX)             !7

              IF (ABS(BOX(1)).GT.90.0 .OR.                          !2.3
     &            ABS(BOX(2)).GT.90.0 .OR.                          !2.3
     &            ABS(BOX(3)).GT.180.0 .OR.                         !2.3
     &            ABS(BOX(4)).GT.180.0) THEN                        !2.3
                KODE = 44                                           !2.3
              ELSE                                                  !2.3
                MINLAT = (NINT(1.0E5*BOX(1))+ 9000000)/100000       !2.2
                MAXLAT = (NINT(1.0E5*BOX(2))+ 9099999)/100000       !2.2
                MINLON = (NINT(1.0E5*BOX(3))+18000000)/200000       !2.2
                MAXLON = (NINT(1.0E5*BOX(4))+18199999)/200000       !2.2
              ENDIF                                                 !2.3
            END IF
          ELSE                                                       !2
            MINLAT = 0                                               !2
            MINLON = 0                                               !2
            MAXLAT = 0                                               !2
            MAXLON = 0                                               !2
          END IF                                                     !2

!-----------------------------------------------------------------------
!     IF ALL OK, MAKE UP AS MUCH OF THE INDEX ENTRY AS POSSIBLE
!-----------------------------------------------------------------------

          IF (KODE.LE.10) THEN
            IVAL = 0
            IF (SEA) IVAL = 128
            ENTRY(1:1)   = CHAR(IVAL)         ! Land/sea flag       !2.2
            ENTRY(2:9)   = PLAT               ! Platform ID          !7
            ENTRY(10:10) = CHAR(NTIME(4,1))   ! Start hour
            ENTRY(11:11) = CHAR(NTIME(5,1))   ! Start minute
            ENTRY(12:12) = CHAR(NTIME(6,1))   ! Start second
            ENTRY(13:13) = CHAR(NTIME(4,2))   ! End hour
            ENTRY(14:14) = CHAR(NTIME(5,2))   ! End minute
            ENTRY(15:15) = CHAR(NTIME(6,2))   ! End second
!
            IF (NUMOBS.EQ.1) THEN
              ENTRY(16:17) = CHAR2(MINLAT)    ! Latitude (1 ob.)
              ENTRY(18:19) = CHAR2(MINLON)    ! Longitude (1 ob.)
            ELSE
              ENTRY(16:16) = CHAR(MINLAT)     ! Minimum latitude
              ENTRY(17:17) = CHAR(MAXLAT)     ! Maximum latitude
              ENTRY(18:18) = CHAR(MINLON)     ! Minimum longitude
              ENTRY(19:19) = CHAR(MAXLON)     ! Maximum longitude
            END IF

            ENTRY(20:21) = CHAR2(NUMOBS)      ! Number of obs.
            ENTRY(22:22) = CHAR(0)            ! Spare
            ENTRY(23:23) = CHAR(0)            ! Spare
            ENTRY(24:24) = CHAR(0)            ! Spare
!           ENTRY(25:33)   Can't be set yet
          END IF
        END IF
      END IF
!                                             Return to calling program
      RETURN
      END
