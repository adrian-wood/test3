SUBROUTINE LATBOX(A,NOBS,NLAT,BOX)

!-----------------------------------------------------------------------
!
! PROGRAM       : LATBOX
!
! PURPOSE       : FIND LAT-LONG BOX ENCLOSING SET OF LAT/LONGS
!                 (N.B. THIS IS NOT AS SIMPLE AS IT MIGHT SEEM,
!                  BECAUSE THE BOX MIGHT CROSS 180 DEGREES!)
!
! CALLED BY     : ERSIND, INDEX1, INDEX2, MERGE, PAOBIND,
!                 SALTSSH, SFERICS, STBIND
!
! ARGUMENTS     : (1) ARRAY OF VALUES
!                 (2) NUMBER OF VALUES OF EACH ELEMENT
!                 (3) SUBSCRIPT OF LATITUDE (ASSUME LONGITUDE NEXT)
!                 (4) LAT-LONG BOX (MIN LAT, MAX LAT, MIN LONG...)
!
! REVISION INFO :
!
!
! $Workfile: latbox.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 12/01/2011 14:13:03$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         12/01/2011 14:13:03    Rosemary Lavery
!       Initial Port
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

IMPLICIT NONE

! Interface Arguments

REAL,    INTENT(IN)   :: A(:)       ! (a1) Array of decoded BUFR data values
INTEGER, INTENT(IN)   :: NOBS       ! (a2) Number of obs. in bulletin
INTEGER, INTENT(IN)   :: NLAT       ! (a3) Latitude subscript for A array
REAL,    INTENT(OUT)  :: BOX(4)     ! (a4) Limits of lat/long box (output)

! Local Parameters

INTEGER, PARAMETER  :: IXNEG = -9999999   ! Large negative integer
REAL, PARAMETER     :: RXPOS = +9999999.  ! Large positive decimal
REAL, PARAMETER     :: RXNEG=  -9999999.  ! Large negative decimal

! Local Variables

INTEGER  :: NGOOD   ! Number of good lat/longs
INTEGER  :: NO      ! Loop variable for loop over obs.

REAL     :: ALAT    ! Latitude
REAL     :: ALON    ! Longitude
REAL     :: MINLAT  ! Minimum latitude in bulletin
REAL     :: MAXLAT  ! Maximum latitude in bulletin
REAL     :: MINLON  ! Minimum longitude in bulletin
REAL     :: MAXLON  ! Maximum longitude in bulletin
REAL     :: MINPOS  ! Minimum positive longitude
REAL     :: MAXPOS  ! Maximum positive longitude
REAL     :: MINNEG  ! Minimum negative longitude
REAL     :: MAXNEG  ! Maximum negative longitude

! --------------------------------------------------------------------

!initialize variables
MINLAT=RXPOS
MAXLAT=RXNEG
MINPOS=RXPOS
MAXPOS=RXNEG
MINNEG=RXNEG
MAXNEG=RXPOS

! Keep max & min latitudes for index entry ignoring missing data.
! Longitudes are less simple because range can cross 180: keep 4
! numbers, max & min pos & neg, to work out range at end.

NGOOD = 0

DO_OBS: &
DO NO=1,NOBS

! *** ALAT = A(NO,NLAT)
! *** ALON = A(NO,NLAT+1)

  ALAT = A( (NLAT-1)*NOBS + NO)
  ALON = A( (NLAT)*NOBS + NO)

IF_LOCATE: &
  IF (ABS(ALAT) <= 90.0 .AND. ABS(ALON) <= 180.0) THEN
    NGOOD = NGOOD + 1
    IF (ALAT > MAXLAT) MAXLAT = ALAT
    IF (ALAT < MINLAT) MINLAT = ALAT

    IF (ALON >= 0) THEN
      IF (ALON > MAXPOS) MAXPOS = ALON
      IF (ALON < MINPOS) MINPOS = ALON
    ELSE
      IF (ALON < MAXNEG) MAXNEG = ALON
      IF (ALON > MINNEG) MINNEG = ALON
    END IF
  END IF IF_LOCATE
END DO DO_OBS

! Find max & min longitude from max & min pos & neg values

IF_CHKLOC: &
IF (NGOOD == 0) THEN                 ! NO GOOD LAT/LONGS
  MINLAT = RXNEG
  MINLON = RXNEG
  MAXLON = RXNEG
ELSE IF (MINNEG == IXNEG) THEN       ! NO NEGATIVE LONGITUDES
  MINLON=MINPOS
  MAXLON=MAXPOS
ELSE IF (MAXPOS == IXNEG) THEN       ! NO POSITIVE LONGITUDES
  MINLON=MAXNEG
  MAXLON=MINNEG
ELSE IF (MINPOS-MINNEG <= 180) THEN  ! 0 IN RANGE
  MINLON=MAXNEG
  MAXLON=MAXPOS
ELSE IF (MINPOS-MINNEG > 180) THEN   ! 180 IN RANGE
  MINLON=MINPOS
  MAXLON=MINNEG
END IF IF_CHKLOC

BOX(1)=MINLAT                          !Set up Lat/Long box
BOX(2)=MAXLAT                          !in single dimension array
BOX(3)=MINLON                          !box - to be passed back
BOX(4)=MAXLON                          !to calling routine.

RETURN
END SUBROUTINE LATBOX
