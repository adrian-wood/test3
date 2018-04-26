SUBROUTINE SATOB3(POINT,KNOTS,DCVALS,NOBS,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SATOB3
!
! PURPOSE       : TO DECODE A SATOB SECTION 3
!
! CALLED BY     : SATOB1
!
! CALLS         : SATBBN
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) KNOTS    TRUE IF WIND SPEED IN KNOTS
!                 (3) DCVALS   REAL ARRAY TO HOLD DECODED VALUES
!                 (4) NOBS     NUMBER OF 'OBSERVATIONS' IN SECTION
!                 (5) BULL     REPORT DATA
!
! REVISION INFO :
!
!
! $Workfile: satob3.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/01/2011 16:51:16$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/01/2011 16:51:16    Rosemary Lavery ref to
!        module IVALUE added
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

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN, KTS2MPS
USE SATBBN_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT
LOGICAL, INTENT(IN)               :: KNOTS
REAL, INTENT(OUT)                 :: DCVALS(:)
INTEGER, INTENT(IN)               :: NOBS
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

! Local Variables

INTEGER  :: CRUDELAT
INTEGER  :: CRUDELON
INTEGER  :: DDD
INTEGER  :: FFF
INTEGER  :: I
INTEGER  :: J
INTEGER  :: LATUNIT
INTEGER  :: LONUNIT
INTEGER  :: NN
INTEGER  :: NNTOT
INTEGER  :: PRESSURE

LOGICAL  :: CALM
LOGICAL  :: EAST
LOGICAL  :: NORTH

! ---------------------------------------------------------------------

I = NOBS*9 + 1  ! NOBS*9 FROM SECTION ONE
NNTOT = 0

   10 CONTINUE

CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)

POINT = POINT+6
NNTOT = NNTOT+NN

DOBLOCK1: &
DO J = 0,NN-1
   LATUNIT = IVALUE(BULL(POINT + J*12:POINT + J*12))
   IF (CRUDELAT == MISSIN .OR. LATUNIT == MISSIN) THEN
      DCVALS(I + J) = MISSIN
   ELSE IF (NORTH) THEN
      DCVALS(I + J) = CRUDELAT+LATUNIT
   ELSE
      DCVALS(I + J) = CRUDELAT-LATUNIT
   END IF

   LONUNIT = IVALUE(BULL(POINT + J*12 + 1:POINT + J*12 +1))
   IF (CRUDELON == MISSIN .OR. LONUNIT == MISSIN) THEN
      DCVALS(I + NOBS + J) = MISSIN
   ELSE IF (EAST) THEN
      DCVALS(I + NOBS + J) = CRUDELON+LONUNIT
   ELSE
      DCVALS(I + NOBS + J) = CRUDELON-LONUNIT
   END IF

   PRESSURE = IVALUE(BULL(POINT + J*12 + 2:POINT + J*12 + 3))
   IF (PRESSURE /= MISSIN) PRESSURE = PRESSURE*1000
   DCVALS(I + NOBS*2 + J) = PRESSURE ! PRESSURE IN PASCALS!

   CALM = .FALSE.
   DDD = IVALUE(BULL(POINT + J*12 + 6:POINT + J*12 +  7))
   FFF = IVALUE(BULL(POINT + J*12 + 8:POINT + J*12 + 10))
   IF (DDD /= MISSIN) DDD = DDD*10
   IF (FFF >= 500) THEN
      FFF = FFF-500
      IF (DDD /= MISSIN) DDD = DDD+5
   END IF
   IF (FFF == 0) CALM = .TRUE.
   DCVALS(I + NOBS*5 + J) = FFF ! WIND SPEED M/S OR KNOTS
   IF (KNOTS .AND. FFF /= MISSIN) &
      DCVALS(I + NOBS*5 + J) = DCVALS(I + NOBS*5 + J)*KTS2MPS
   IF (CALM) DCVALS(I + NOBS*5 + J) = 0.0
   FFF = DCVALS(I + NOBS*5 + J)
   IF (FFF > 200) THEN
      FFF = MISSIN
      DCVALS(I + NOBS*5 + J) = MISSIN
   END IF

   IF (FFF == MISSIN) DDD = MISSIN
   IF (DDD == 0) DDD = 360
   IF (DDD > 360) DDD = MISSIN
   IF (CALM) DDD = 0
   DCVALS(I + NOBS*4 + J) = DDD ! WIND DIRN DEGREES TRUE
   IF (DDD == MISSIN) THEN
      FFF = MISSIN
      DCVALS(I + NOBS*5 + J) = MISSIN
   END IF
END DO DOBLOCK1

POINT = POINT + NN*12
IF (NNTOT < NOBS) THEN
   I = I + NN
   GOTO 10
END IF

RETURN
END SUBROUTINE SATOB3
