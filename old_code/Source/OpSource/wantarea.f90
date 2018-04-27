SUBROUTINE WANTAREA (ENTRY, NVER, AREA, RPOLE, KEEP)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  WANTAREA
!
!    To compare the user's area as specified in an MDB request string
!    with that covered by observations in an index entry.
!
! DESCRIPTION:
!
!    WANTAREA checks the requested area in a user's MDB request string
!    against the lat/long box in an index entry which indicates the
!    observation coverage.  The result is indicated by the output
!    variable KEEP.
!
!    If the lat/long box lies entirely outside the user's area, none
!    of the observations are wanted and KEEP is set to -1.
!
!    If the lat/long box lies entirely within the user's area, all of
!    the observations are wanted and KEEP is set to +1.
!
!    If there is a pertial overlap between the user's area and the
!    lat/long box, it is not possible to say which observations are
!    wanted without decoding the BUFR message and checking observation
!    locations individually. In this case KEEP is set to zero.
!
!    If the index entry is in format 2 (non-satellite data) and there
!    is only one observation, the entry contains the observation
!    latitude and longitude instead of a lat/long box so these can be
!    checked directly against the user's area.  In this case the check
!    is done even if the user's area is on a rotated grid (element 1
!    of the AREA array = 1.0); otherwise an area on a rotated grid
!    will result in a KEEP value of zero so that observation locations
!    will need to be checked individually after BUFR decoding.
!
! USAGE:  CALL WANTAREA (ENTRY, NVER, AREA, RPOLE, KEEP)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    ENTRY    I   C*(*)  Index entry from MetDB storage data set
!    NVER     I   I*4    Index entry version number (1 or 2)
!    AREA     I   R*4    (Array size 5) User's requested area: Flag, N,
!                          W, S, E boundaries (Flag=1 for rotated pole)
!    RPOLE    I   R*4    (Array size 2) True latitude and longitide of
!                          'rotated' N. pole (for rotated grids)
!    KEEP     O   i*4    'Wanted' flag, -1, 0 or +1 (see Description)
!
! CALLS:  ROTAREA
!
! REVISION INFORMATION:
!
!    $Workfile: wantarea.f90$ $Folder: OpSource$
!    $Revision: 3$ $Date: 25/11/2010 17:09:17$
!
! CHANGE RECORD:
!
! $Log:
!  3    MetDB_Refresh 1.2         25/11/2010 17:09:17    Alison Weir
!       Comment in USE statement
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE rotarea_mod

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)        ::  ENTRY ! Index entry
INTEGER, INTENT(IN)             ::  NVER ! Index entry version number (1 or 2)
REAL, INTENT(IN)                ::  AREA(5) ! User's area (Flag, N, W, S, E)
REAL, INTENT(IN)                ::  RPOLE(2) ! Latitude and longitude of rotated pole
INTEGER, INTENT(OUT)            ::  KEEP ! 'Keep' flag from area checking

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  I  ! Pointer to byte in index entry
INTEGER      ::  ICHAR2 ! Function to convert C*2 to I*4
INTEGER      ::  IPLL(2) ! Start bytes of lat/lon for index versions
INTEGER      ::  IX, IY ! 'Keep' flags for longitude & latitude
INTEGER      ::  LAT ! Latitude in 0.01 deg. from index entry
INTEGER      ::  LON ! Longitude in 0.01 deg. from index entry

REAL         ::  RLATN ! Northern boundary of observation box
REAL         ::  RLATS ! Southern boundary of observation box
REAL         ::  RLONE ! Eastern boundary of observation box
REAL         ::  RLONW ! Western boundary of observation box

LOGICAL      ::  LONER ! Flag for index version 2 with single ob

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA IPLL /9,16/    ! Index lat/lon locations for versions 1 & 2

I = IPLL(NVER)                     ! First byte of area info.
LONER = NVER == 2 .AND. &
        ICHAR2(ENTRY(20:21)) == 1  ! Version 2 with single ob

!=======================================================================
!     INDEX CONTAINS LAT & LONG: CHECK FOR OBSERVATION IN USER'S AREA
!=======================================================================

IFLABEL1: &
IF (LONER) then
  KEEP = 1       ! First guess
!                                                      Decode lat & lon
  LAT = ICHAR2(ENTRY( I :I+1))  ! Latitude
  LON = ICHAR2(ENTRY(I+2:I+3))  ! Longitude
!                                                   Check for values <0
  IF (LAT >= 32768) LAT = LAT - 65536
  IF (LON >= 32768) LON = LON - 65536
!                                                Convert to real values
  RLATN = 0.01*LAT
  RLONE = 0.01*LON

!-----------------------------------------------------------------------
!  (1)    Rotated pole: convert to lat & long to rotated co-ordinates
!-----------------------------------------------------------------------

  IF (AREA(1) == 1.0) THEN       ! Rotated pole
    RLATS = RLATN
    RLONW = RLONE
    CALL ROTAREA (RLATS, RLONW, RLATN, RLONE, RPOLE)
  END IF

!-----------------------------------------------------------------------
!  (2)    Check for observation outside range of user's latitudes
!-----------------------------------------------------------------------

!             Ob too far N          Ob too far S
IFLABEL2: &
  IF (RLATN > AREA(2) .OR. RLATN < AREA(4)) THEN
    KEEP = -1

!-----------------------------------------------------------------------
!  (3)  Check for observation outside range of user's longitudes
!-----------------------------------------------------------------------

  ELSE

!-----------------------------------------------------------------------
!  (3A)   User's box doesn't cross 180W
!-----------------------------------------------------------------------

IFLABEL3: &
    IF (AREA(3) <= AREA(5)) THEN

!                 Ob too far W          Ob too far E
      IF (RLONE < AREA(3) .OR. RLONE > AREA(5)) KEEP = -1

!-----------------------------------------------------------------------
!  (3A)   User's box crosses 180W
!-----------------------------------------------------------------------

    ELSE
!                 Ob too far W           Ob too far E
      IF (RLONE < AREA(3) .AND. RLONE > AREA(5)) KEEP = -1
    END IF IFLABEL3
  END IF IFLABEL2

!=======================================================================
!         CHECK FOR OVERLAP BETWEEN OBSERVATION BOX AND USER'S AREA
!=======================================================================
ELSE IF (AREA(1) /= 1.0) THEN   ! Not rotated pole
!                                                       Initialisations
  IX = 0
  IY = 0
!                                    Find boundaries of observation box

  RLATS =   ICHAR(ENTRY( I : I )) - 90  ! South
  RLATN =   ICHAR(ENTRY(I+1:I+1)) - 90  ! North
  RLONW = 2*ICHAR(ENTRY(I+2:I+2)) - 180 ! West
  RLONE = 2*ICHAR(ENTRY(I+3:I+3)) - 180 ! East

!-----------------------------------------------------------------------
!  (1)    Check for no overlap in latitude ranges
!-----------------------------------------------------------------------

!           Ob box too far N      Ob box too far S
  IF (RLATS > AREA(2) .OR. RLATN < AREA(4)) THEN
    IY = -1

!-----------------------------------------------------------------------
!  (1A)   Check for ob. latitudes lying entirely within user's range
!-----------------------------------------------------------------------

!               Obs not too far S       Obs not too far N
  ELSE IF (RLATS >= AREA(4) .AND. RLATN <= AREA(2)) THEN
    IY = 1
  END IF

!-----------------------------------------------------------------------
!  (2)    Check for no overlap in longitude ranges
!-----------------------------------------------------------------------

IFLABEL4: &
  IF (IY >= 0) THEN

!-----------------------------------------------------------------------
!  (2A)   Check for neither ob. nor user's box crossing 180W
!-----------------------------------------------------------------------

!             Ob box doesn't       User's box doesn't
IFLABEL5: &
    IF (RLONW <= RLONE .AND. AREA(3) <= AREA(5)) THEN

!-----------------------------------------------------------------------
!  (2Aa)  Check for no overlap in longitude ranges
!-----------------------------------------------------------------------

!               Ob box too far W      Ob box too far E
      IF (RLONW > AREA(5) .OR. RLONE < AREA(3)) THEN
        IX = -1

!-----------------------------------------------------------------------
!  (2Ab)  Check for ob. longitudes lying entirely within user's range
!-----------------------------------------------------------------------

!                   Obs not too far W       Obs not too far E
      ELSE IF (RLONW >= AREA(3) .AND. RLONE <= AREA(5)) THEN
        IX = 1
      END IF

!-----------------------------------------------------------------------
!  (2B)   Check for both ob. and user's boxes crossing 180W
!         (Boxes must overlap so there's no need to test for IX = -1.)
!-----------------------------------------------------------------------

!                   Ob box does          User's box does
    ELSE IF (RLONW > RLONE .AND. AREA(3) > AREA(5)) THEN

!-----------------------------------------------------------------------
!  (2Ba)  Check for ob. longitudes lying entirely within user's range
!-----------------------------------------------------------------------

!              Obs not too far E      Obs not too far W
      IF (RLONW >= AREA(3) .OR. RLONE <= AREA(5)) IX = 1

!-----------------------------------------------------------------------
!  (2C)   Remaining case - one box crosses 180W but not both
!-----------------------------------------------------------------------

    ELSE

!-----------------------------------------------------------------------
!  (2Ca)  Check for no overlap in longitude ranges.
!         (The test is the same whichever box crosses 180W.)
!-----------------------------------------------------------------------

      IF (RLONW > AREA(5) .AND. RLONE < AREA(3)) THEN
        IX = -1

!-----------------------------------------------------------------------
!  (2Cb)  If the user's box is the one which crosses 180W, check for
!         observation longitudes lying entirely within user's range.
!-----------------------------------------------------------------------

      ELSE IF (AREA(3) > AREA(5) .AND. &
              (RLONW >= AREA(3) .OR. RLONE <= AREA(5))) THEN
        IX = 1
      END IF
    END IF IFLABEL5
  END IF IFLABEL4
!                                     Set 'keep' flag for area checking
  KEEP = MIN0(IX,IY)

ELSE        ! Rotated pole - check obs individually later
  KEEP = 0
END IF IFLABEL1

RETURN
END SUBROUTINE WANTAREA
