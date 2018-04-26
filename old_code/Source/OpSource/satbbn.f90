SUBROUTINE SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)

!-----------------------------------------------------------------------

! PROGRAM       : SATBBN
!
! PURPOSE       : DECODES BBBNN GROUP IN A SATOB SECTION 2,3,4,5 OR 7
!
! CALLED BY     : SATOB2, SATOB3, SATOB4, SATOB5, SATOB7
!
! CALLS         : NIL
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) CRUDELAT LATITUDE  OF A TEN DEGREE SQUARE
!                 (3) CRUDELON LONGITUDE OF A TEN DEGREE SQUARE
!                 (4) NORTH    SET TO TRUE IF NORTHERN HEMISPHERE
!                 (5) EAST     SET TO TRUE IF EAST OF GREENWICH MERID
!                 (6) NN       NUMBER OF REPORTS FROM 10 DEGREE SQUARE
!                 (7) BULL     REPORT DATA
!
! REVISION INFO :
!
!
! $Workfile: satbbn.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/01/2011 16:51:16$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/01/2011 16:51:16    Rosemary Lavery ref to
!        module IVALUE added
!  1    MetDB_Refresh 1.0         12/01/2011 14:12:35    Rosemary Lavery
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

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN        ! Set constant missing data value

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)               :: POINT
INTEGER, INTENT(OUT)              :: CRUDELAT
INTEGER, INTENT(OUT)              :: CRUDELON
LOGICAL, INTENT(OUT)              :: NORTH
LOGICAL, INTENT(OUT)              :: EAST
INTEGER, INTENT(OUT)              :: NN
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

! Local Variables

INTEGER  :: B1
INTEGER  :: B2
INTEGER  :: B3

! ---------------------------------------------------------------------

NN = IVALUE(BULL(POINT+3:POINT+4))

CRUDELAT = 0
CRUDELON = 0

B1 = IVALUE(BULL(POINT:POINT))     ! OCTANT

IF (B1 == 4 .OR. B1 == 9 .OR. B1 == MISSIN) THEN
   CRUDELAT = MISSIN
   CRUDELON = MISSIN
   RETURN
END IF

B2 = IVALUE(BULL(POINT+1:POINT+1))     ! TENS FIGURE OF LAT
IF (B2 == 9 .OR. B2 == MISSIN) CRUDELAT = MISSIN

B3 = IVALUE(BULL(POINT+2:POINT+2))     ! TENS FIGURE OF LON
IF (             B3 == MISSIN) CRUDELON = MISSIN
IF (CRUDELAT  /=  MISSIN) THEN
   CRUDELAT = B2*10
   IF (B1 >= 5) THEN
      CRUDELAT = 0-CRUDELAT
      NORTH    = .FALSE.
   ELSE
      NORTH    = .TRUE.
   END IF
END IF

IF (CRUDELON  /=  MISSIN) THEN
   IF ((B1 == 1).OR.(B1 == 2).OR.(B1 == 6).OR.(B1 == 7)) THEN
      IF (B3 /= 9) B3 = B3+10
   END IF
   CRUDELON = B3*10
   IF ((B1 == 0).OR.(B1 == 1).OR.(B1 == 5).OR.(B1 == 6)) THEN
      CRUDELON = 0-CRUDELON
      EAST     = .FALSE.
   ELSE
      EAST     = .TRUE.
   END IF
END IF

BULL(POINT:POINT+2) = 'BBB'  ! ##############

RETURN
END SUBROUTINE SATBBN
