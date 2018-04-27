SUBROUTINE INDEX1 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  INDEX1
!
! PURPOSE:     To make an index entry for a BUFR message using index
!              format 1.
!
! DESCRIPTION: 'INDEX1' creates the main part of the index entry for
!              a BUFR message in index format 1 (new format developed
!              for satellite data in late 2000).  Only details of
!              time of receipt and location in storage need to be
!              added later.
!
!              The BUFR message is partially decoded (in subroutine
!              GETVALS) to provide some details for the entry.
!
! USAGE:       CALL INDEX1 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)
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
!              ENTRY   (O)  26-byte index entry, complete except for
!                           time of receipt and location in storage
!                           data set.
!              KODE    (O)  Integer return code as follows:
!
!                              0  Successsful completion
!                             21  VALUES array too small
!                             41  Vital bufr descriptor not found
!                             42  BUFR bulletin failed to decode
!                             44  Couldn't make index entry
!
! CALLED BY:   BUFREP
!
! CALLS:       CHAR2, GETVALS, LATBOX, ONLAND, TRANGE, INT2CH
!
! HISTORY:     Original version by Brian Barwell, October 2000.
!
! REVISION INFO:
!
! $Workfile: index1.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 27/01/2012 16:54:42$
!
! CHANGE RECORD:
!
! $Log:
!  9    MetDB_Refresh 1.8         27/01/2012 16:54:42    Brian Barwell
!       Increase MAXVALS TO 300000.
!  8    MetDB_Refresh 1.7         24/05/2011 15:19:32    Brian Barwell
!       Increase MAXVALS to 10000, delete COMMON block and make VALUES
!       allocatable.
!  7    MetDB_Refresh 1.6         16/03/2011 09:45:29    Sheila Needham  Split
!       an IF test into separate tests
!  6    MetDB_Refresh 1.5         28/01/2011 13:34:30    Alison Weir     CHAR
!       replaced with INT2CH
!  5    MetDB_Refresh 1.4         28/01/2011 12:49:55    Alison Weir     Minor
!       changes following review - initialise out arguments
!  4    MetDB_Refresh 1.3         27/01/2011 15:57:14    Richard Weedon  change
!        to copyright date
!  3    MetDB_Refresh 1.2         27/01/2011 10:58:20    Richard Weedon
!       Updated for if and do construct labelling
!  2    MetDB_Refresh 1.1         27/01/2011 10:50:10    Richard Weedon  passed
!        compilation test with module files
!  1    MetDB_Refresh 1.0         21/01/2011 16:15:32    Richard Weedon  Intial
!        draft , still needs more work as will not compile without mod files
!       for char2 & onland
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
USE latbox_mod
USE onland_mod
USE trange_mod
USE int2ch_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN)   :: MESSAGE       ! BUFR message
LOGICAL,INTENT(IN)            :: FLAGS(9)      ! Processing flags
                                               ! (from headers data set)
INTEGER,INTENT(IN)            :: ITEMS(12)     ! Processing data
                                               ! (from headers data set)
INTEGER,INTENT(OUT)           :: NTIME(6,2)    ! Yr,mon,day,hr,mi
                                               ! of ob. time window
CHARACTER(LEN=26),INTENT(OUT) :: ENTRY         ! Index entry for
INTEGER,INTENT(OUT)           :: KODE          ! Return code

! Local Variables

INTEGER,PARAMETER :: LCSTR=6000    ! Size of CSTR string
INTEGER,PARAMETER :: MAXVALS=300000! Size of VALUES array            !9

INTEGER           :: ILAT          ! Pointer to lats in VALUES array
INTEGER           :: ILON          ! Pointer to lons in VALUES array
INTEGER           :: IVAL          ! Pointer to VALUES array
INTEGER           :: J             ! General loop variable
INTEGER           :: JOB           ! Loop variable for observations
INTEGER           :: LISTDES(10,2) ! Lists of descriptors to be decoded
INTEGER           :: LISTITEM      ! Number of an item in parameter list
INTEGER           :: MAXLAT        ! Highest latitude of obs in message
INTEGER           :: MAXLON        ! Highest longitude of obs in message
INTEGER           :: MINLAT        ! Lowest latitude of obs in message
INTEGER           :: MINLON        ! Lowest longitude of obs in message
INTEGER           :: NPOS(10)      ! Loc of parameters in BUFR sequence
INTEGER           :: NSAT          ! Satellite id. for index entry
INTEGER           :: NUMLIST       ! No. of list to be used for decode
INTEGER           :: NUMOBS        ! No. of obs in BUFR message

REAL              :: BOX(4)        ! Boundaries of lat/long obs. box
REAL              :: SATID         ! Sat id decoded from BUFR message
REAL, ALLOCATABLE :: VALUES(:)     ! Decoded values from BUFR message

LOGICAL           :: FIRST =.TRUE. ! Flag for first call to INDEX1
LOGICAL           :: FOUND         ! Flag for parameter location found
LOGICAL           :: MUSTHAVE(10)  ! Flags for params. needed for index
LOGICAL           :: SAME          ! Flag for obs having same sat. id
LOGICAL           :: SEA           ! Flag for at least one ob. over sea

CHARACTER(LEN=LCSTR)  :: CSTR      ! Char elements from GETVALS

!                                                       Saved variables
SAVE LISTDES, MUSTHAVE, VALUES
!                                                   Data initialisation
DATA LISTDES &
!         SAT  LAT  LON YEAR MNTH  DAY HOUR  MIN  SEC SURF   (LAT/LON)
        / 263,1282,1538,1025,1026,1027,1028,1029,1030,2060,&!  COARSE
          263,1281,1537,1025,1026,1027,1028,1029,1030,2060/ !  HI-RES
DATA MUSTHAVE /8*.TRUE., 2*.FALSE./
!                                         Allocate VALUES if first call
IF (FIRST) THEN
  ALLOCATE (VALUES(1:MAXVALS))
  FIRST = .FALSE.
END IF
!                                                 Other initialisations
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
!                                        Partial decode of BUFR message

CALL GETVALS (LISTDES(1,NUMLIST), 10, MESSAGE, MAXVALS,  &
              VALUES, CSTR, NPOS, NUMOBS, KODE)

ifconstr1 : &
IF (KODE == 0) THEN
!                     Check that essential parameters have been located
   LISTITEM = 0
   FOUND = .TRUE.
   DO WHILE (FOUND .AND. LISTITEM < 10)
      LISTITEM = LISTITEM + 1
      IF (MUSTHAVE(LISTITEM)) FOUND = NPOS(LISTITEM) > 0
   END DO
!                                  Warning if something vital not found
   ifconstr2 : &
   IF (.NOT.FOUND) THEN
      WRITE (6,'(T5,A,T15,A,I3,A)') 'INDEX1:', 'Vital descriptor',&
                  LISTITEM, ' not found in BUFR sequence.'
      KODE = 41
!                             Check for expected number of observations
!                                  (but store anyway even if different)
   ELSE
      IF (ITEMS(8) > 0 .AND. NUMOBS /= ITEMS(8))       &
       WRITE (6,'(T5,A,T15,A,I5,A,I5,A)') 'INDEX1:',   &
       'Number of observations in message =', NUMOBS,  &
       ' instead of', ITEMS(8), ' expected.'

!-----------------------------------------------------------------------
!     SET LAND/SEA FLAG (SET TO 'SEA' IF AT LEAST ONE SEA POINT).
!     IF SURFACE TYPE IS NOT AVAILABLE, CALL "ONLAND".
!-----------------------------------------------------------------------

      ILAT = (NPOS(2)-1)*NUMOBS
      ILON = (NPOS(3)-1)*NUMOBS
      IF (NPOS(10) > 0) THEN
         IVAL = (NPOS(10)-1)*NUMOBS
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

!-----------------------------------------------------------------------
!     CHECK THAT ALL OBS ARE FOR THE SAME SATELLITE AND GET ITS ID.
!-----------------------------------------------------------------------

      IVAL = (NPOS(1)-1)*NUMOBS
      SATID = VALUES(IVAL+1)   ! First satellite identifier
      SAME = .TRUE.
      JOB = 2
!                                              Check for same satellite
      DO WHILE (SAME .AND. JOB <= NUMOBS)
         SAME = VALUES(IVAL+JOB) == SATID
         JOB = JOB + 1
      END DO ! JOB
!                              Set satellite identifier for index entry

      IF (SAME .AND. SATID > 0.0) THEN
         NSAT = NINT(SATID)
      ELSE
         NSAT = 1023  ! MISSING DATA
         WRITE (6,'(T5,A,T15,A)') 'INDEX1:',&
                   'Satellite identifiers missing or not all the same.'
      END IF

!-----------------------------------------------------------------------
!     GET DATE & TIME FOR OLDEST AND MOST RECENT OBSERVATIONS
!     CHECK DATE/TIME FOR MISSING DATA (BIG NEGATIVE VALUE)
!-----------------------------------------------------------------------

      CALL TRANGE (VALUES, NUMOBS, NPOS(4), NTIME)

      DO J=1,6
         IF (NTIME(J,1) < 0 .OR. NTIME(J,2) < 0) KODE = 44
      END DO ! J

!-----------------------------------------------------------------------
!     GET BOUNDARIES OF LAT/LONG BOX ENCLOSING OBSERVATIONS
!     (Calculation done in integers to control round-offs on HP.)
!-----------------------------------------------------------------------

      CALL LATBOX (VALUES, NUMOBS, NPOS(2), BOX)

      IF (ABS(BOX(1)) > 90.0 .OR. ABS(BOX(2)) > 90.0 .OR.&
         ABS(BOX(3)) > 180.0 .OR. ABS(BOX(4)) > 180.0) THEN
        KODE = 44
      ELSE
        MINLAT = (NINT(1.0E5*BOX(1)) +  9000000) / 100000
        MAXLAT = (NINT(1.0E5*BOX(2)) +  9099999) / 100000
        MINLON = (NINT(1.0E5*BOX(3)) + 18000000) / 200000
        MAXLON = (NINT(1.0E5*BOX(4)) + 18199999) / 200000
      END IF

!-----------------------------------------------------------------------
!     IF ALL OK, MAKE UP AS MUCH OF THE INDEX ENTRY AS POSSIBLE
!-----------------------------------------------------------------------

      ifconstr3 : &
      IF (KODE <= 10) THEN
         IVAL = NSAT/256
         IF (SEA) IVAL = IVAL + 128

         ENTRY(1:1)   = INT2CH(IVAL)          ! Land/sea flag & ..
         ENTRY(2:2)   = INT2CH(MOD(NSAT,256)) ! .. Satellite ID
         ENTRY(3:3)   = INT2CH(NTIME(4,1))    ! Start hour
         ENTRY(4:4)   = INT2CH(NTIME(5,1))    ! Start minute
         ENTRY(5:5)   = INT2CH(NTIME(6,1))    ! Start second
         ENTRY(6:6)   = INT2CH(NTIME(4,2))    ! End hour
         ENTRY(7:7)   = INT2CH(NTIME(5,2))    ! End minute
         ENTRY(8:8)   = INT2CH(NTIME(6,2))    ! End second
         ENTRY(9:9)   = INT2CH(MINLAT)        ! Minimum latitude
         ENTRY(10:10) = INT2CH(MAXLAT)        ! Maximum latitude
         ENTRY(11:11) = INT2CH(MINLON)        ! Minimum longitude
         ENTRY(12:12) = INT2CH(MAXLON)        ! Maximum longitude
         ENTRY(13:14) = CHAR2(NUMOBS)         ! Number of obs.
         ENTRY(15:15) = INT2CH(ITEMS(3))      ! Index entry byte
         ENTRY(16:16) = INT2CH(0)             ! Spare
!        ENTRY(17:26)   Can't be set yet
      END IF ifconstr3
   END IF ifconstr2
END IF ifconstr1
!                                             Return to calling program
RETURN
END SUBROUTINE INDEX1
