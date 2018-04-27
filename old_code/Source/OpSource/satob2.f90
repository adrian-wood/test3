SUBROUTINE SATOB2(POINT,KNOTS,DCVALS,NOBS,BULL,SLASHES)

!-----------------------------------------------------------------------
!
! PROGRAM       : SATOB2
!
! PURPOSE       : TO DECODE A SATOB SECTION 2
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
!                 (6) SLASHES  LOGICAL FOR '/////' PADDING
!
! REVISION INFO :
!
!
! $Workfile: satob2.f90$ $Folder: OpSource$
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
USE METDB_COM_mod, ONLY: MISSIN, KTS2MPS, TCONV
USE SATBBN_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT
LOGICAL, INTENT(IN)               :: KNOTS
REAL, INTENT(OUT)                 :: DCVALS(:)
INTEGER, INTENT(IN)               :: NOBS
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL
LOGICAL, INTENT(IN)               :: SLASHES

! Local Variables

INTEGER  :: CRUDELAT
INTEGER  :: CRUDELON
INTEGER  :: DDD
INTEGER  :: FFF
INTEGER  :: I
INTEGER  :: J
INTEGER  :: K
INTEGER  :: L
INTEGER  :: LATUNIT
INTEGER  :: LONUNIT
INTEGER  :: NN
INTEGER  :: NNTOT
INTEGER  :: PAIRTOT
INTEGER  :: PRESSURE
INTEGER  :: TEMPRCH
INTEGER  :: TEMPSIGN

LOGICAL  :: CALM
LOGICAL  :: EAST
LOGICAL  :: NORTH
LOGICAL  :: ODDTOT

! --------------------------------------------------------------------

I = NOBS*9 + 1  ! NOBS*9 FROM SECTION ONE
NNTOT = 0

   10 CONTINUE
CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)
IF (MOD(NN,2) == 0) THEN
   PAIRTOT = NN/2
   ODDTOT  = .FALSE.
ELSE
   PAIRTOT = (NN+1) / 2
   ODDTOT  = .TRUE.
END IF
POINT = POINT+6
NNTOT = NNTOT+NN
J     = -2

DOBLOCK1: &
DO K = 1,PAIRTOT
   J = J+2

DOBLOCK2: &
   DO L = 0,1
      IF (K == PAIRTOT .AND. L == 1 .AND. ODDTOT) GOTO 20
! NO POINT IN DECODING AND STORING SOLIDI (IF PRESENT)
      IF (L == 0) THEN
        LATUNIT = IVALUE(BULL(POINT + J*15    :POINT + J*15    ))
      ELSE
        LATUNIT = IVALUE(BULL(POINT + J*15 + 2:POINT + J*15 + 2))
      END IF
      IF (CRUDELAT == MISSIN .OR. LATUNIT == MISSIN) THEN
         DCVALS(I + J + L) = MISSIN
      ELSE IF (NORTH) THEN
         DCVALS(I + J + L) = CRUDELAT+LATUNIT
      ELSE
         DCVALS(I + J + L) = CRUDELAT-LATUNIT
      END IF

      IF (L == 0) THEN
        LONUNIT = IVALUE(BULL(POINT + J*15 + 1:POINT + J*15 + 1))
      ELSE
        LONUNIT = IVALUE(BULL(POINT + J*15 + 3:POINT + J*15 + 3))
      END IF
      IF (CRUDELON == MISSIN .OR. LONUNIT == MISSIN) THEN
         DCVALS(I + NOBS + J + L) = MISSIN
      ELSE IF (EAST) THEN
         DCVALS(I + NOBS + J + L) = CRUDELON+LONUNIT
      ELSE
         DCVALS(I + NOBS + J + L) = CRUDELON-LONUNIT
      END IF

      IF (L == 0) THEN
     PRESSURE = IVALUE(BULL(POINT + J*15 +  6:POINT + J*15 +  7))
      ELSE
     PRESSURE = IVALUE(BULL(POINT + J*15 + 18:POINT + J*15 + 19))
      END IF
      IF (PRESSURE /= MISSIN) PRESSURE = PRESSURE*1000
     DCVALS(I + NOBS*2 + J + L) = PRESSURE ! PRESSURE IN PASCALS!

      IF (L == 0) THEN
      TEMPRCH = IVALUE(BULL(POINT + J*15 +  8:POINT + J*15 + 10))
      ELSE
      TEMPRCH = IVALUE(BULL(POINT + J*15 + 20:POINT + J*15 + 22))
      END IF
      IF (TEMPRCH /= MISSIN) THEN
         IF (L == 0) THEN
        TEMPSIGN = IVALUE(BULL(POINT +J*15 +10:POINT +J*15 + 10))
         ELSE
        TEMPSIGN = IVALUE(BULL(POINT +J*15 +22:POINT +J*15 + 22))
         END IF
         IF (MOD(TEMPSIGN, 2) == 1) TEMPRCH = 0-TEMPRCH
         TEMPRCH = TEMPRCH + TCONV*10.     ! temperature in tenths
      END IF
      DCVALS(I + NOBS*3 + J + L) = TEMPRCH ! SURFACE TEMPERATURE
      IF (TEMPRCH /= MISSIN) &
     DCVALS(I + NOBS*3 + J + L) = DCVALS(I + NOBS*3 + J + L)/10.0

      CALM = .FALSE.
      IF (L == 0) THEN
         DDD = IVALUE(BULL(POINT + J*15 + 12:POINT + J*15 + 13))
         FFF = IVALUE(BULL(POINT + J*15 + 14:POINT + J*15 + 16))
      ELSE
         DDD = IVALUE(BULL(POINT + J*15 + 24:POINT + J*15 + 25))
         FFF = IVALUE(BULL(POINT + J*15 + 26:POINT + J*15 + 28))
      END IF
      IF (DDD /= MISSIN) DDD = DDD*10
      IF (FFF >= 500) THEN
         FFF = FFF-500
         IF (DDD /= MISSIN) DDD = DDD+5
      END IF
      IF (FFF == 0) CALM = .TRUE.
      DCVALS(I + NOBS*5 + J + L) = FFF ! WIND SPEED M/S OR KNOTS
      IF (KNOTS .AND. FFF /= MISSIN) &
       DCVALS(I + NOBS*5 + J + L) = DCVALS(I + NOBS*5 + J + L)*KTS2MPS
      IF (CALM) DCVALS(I + NOBS*5 + J + L) = 0.0
      FFF = DCVALS(I + NOBS*5 + J + L)
      IF (FFF > 200) THEN
         FFF = MISSIN
         DCVALS(I + NOBS*5 + J + L) = MISSIN
      END IF

      IF (FFF == MISSIN) DDD = MISSIN
      IF (DDD == 0) DDD = 360
      IF (DDD > 360) DDD = MISSIN
      IF (CALM) DDD = 0
      DCVALS(I + NOBS*4 + J + L) = DDD ! WIND DIRN DEGREES TRUE
      IF (DDD == MISSIN) THEN
         FFF = MISSIN
         DCVALS(I + NOBS*5 + J + L) = MISSIN
      END IF
   END DO DOBLOCK2
END DO DOBLOCK1

   20 CONTINUE
IF (ODDTOT .AND. SLASHES) THEN
   POINT = POINT + PAIRTOT*6 + (NN+1)*12
ELSE
   POINT = POINT + PAIRTOT*6 + NN*12
END IF
IF (NNTOT < NOBS) THEN
   I = I + NN
   GOTO 10
END IF

RETURN
END SUBROUTINE SATOB2
