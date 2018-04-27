SUBROUTINE SATOB5(POINT,DCVALS,NOBS,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SATOB5
!
! PURPOSE       : TO DECODE A SATOB SECTION 5
!
! CALLED BY     : SATOB1
!
! CALLS         : SATBBN
!
! ARGUMENTS     : (1) POINT    POINTER TO WHERE TO START IN BULLETIN
!                 (2) DCVALS   REAL ARRAY TO HOLD DECODED VALUES
!                 (3) NOBS     NUMBER OF 'OBSERVATIONS' IN SECTION
!                 (4) BULL     REPORT DATA
!
! REVISION INFO :
!
!
! $Workfile: satob5.f90$ $Folder: OpSource$
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
USE METDB_COM_mod, ONLY: MISSIN, TCONV
USE SATBBN_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT
REAL, INTENT(OUT)                 :: DCVALS(:)
INTEGER, INTENT(IN)               :: NOBS
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

! Local Variables

INTEGER  :: CLOUDCVR
INTEGER  :: CRUDELAT
INTEGER  :: CRUDELON
INTEGER  :: I
INTEGER  :: J
INTEGER  :: LATUNIT
INTEGER  :: LONUNIT
INTEGER  :: NN
INTEGER  :: NNTOT
INTEGER  :: PRESSURE
INTEGER  :: TEMPRCH
INTEGER  :: TEMPSIGN

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
   CLOUDCVR = IVALUE(BULL(POINT + J*12 + 6:POINT + J*12 + 7))
   DCVALS(I + NOBS*6 + J) = CLOUDCVR
   TEMPRCH = IVALUE(BULL(POINT + J*12 + 8:POINT + J*12 + 10))
   IF (TEMPRCH /= MISSIN) THEN
      TEMPSIGN = IVALUE(BULL(POINT +J*12 +10:POINT +J*12 + 10))
      IF (MOD(TEMPSIGN, 2)  ==  1) TEMPRCH = 0-TEMPRCH
      TEMPRCH = TEMPRCH + TCONV*10.  ! temperature in tenths
   END IF
   DCVALS(I + NOBS*3 + J) = TEMPRCH  ! TEMPERATURE OF SURFACE
   IF (TEMPRCH /= MISSIN) &
   DCVALS(I + NOBS*3 + J) = DCVALS(I + NOBS*3 + J)/10.0 ! DEG K
END DO DOBLOCK1

POINT = POINT + NN*12
IF (NNTOT < NOBS) THEN
   I = I + NN
   GOTO 10
END IF

RETURN
END SUBROUTINE SATOB5
