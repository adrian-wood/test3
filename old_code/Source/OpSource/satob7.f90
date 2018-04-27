SUBROUTINE SATOB7(POINT,DCVALS,NOBS,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : SATOB7
!
! PURPOSE       : TO DECODE A SATOB SECTION 7
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
! $Workfile: satob7.f90$ $Folder: OpSource$
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
USE METDB_COM_mod, ONLY: MISSIN
USE SATBBN_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT
REAL, INTENT(OUT)                 :: DCVALS(:)
INTEGER, INTENT(IN)               :: NOBS
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL

! Local Variables

INTEGER  :: CRUDELAT
INTEGER  :: CRUDELON
INTEGER  :: I
INTEGER  :: J
INTEGER  :: K
INTEGER  :: L
INTEGER  :: LASTFEW
INTEGER  :: LATUNIT
INTEGER  :: LONUNIT
INTEGER  :: M
INTEGER  :: NN
INTEGER  :: NNTOT
INTEGER  :: PBASE
INTEGER  :: PENTTOT
INTEGER  :: PTOP
INTEGER  :: RELHUM

LOGICAL  :: EAST
LOGICAL  :: NORTH

CHARACTER (LEN=15)  :: LLHUMS

! ---------------------------------------------------------------------

I = NOBS*6 + 1  ! NOBS*6 FROM SECTION ONE
NNTOT = 0

   10 CONTINUE

IF (BULL(POINT+2:POINT+4)  ==  '///') THEN
   PBASE = IVALUE(BULL(POINT:POINT+1))
   POINT = POINT+6
   IF (PBASE == MISSIN) THEN
      PTOP  = MISSIN
   ELSE
      PBASE = PBASE*1000
      PTOP  = 0  ! ASSUME TROPOPAUSE PRESSURE IS ZERO
   END IF
END IF

CALL SATBBN(POINT,CRUDELAT,CRUDELON,NORTH,EAST,NN,BULL)

POINT   = POINT+6
NNTOT   = NNTOT+NN
PENTTOT = NN/5
LASTFEW = MOD(NN,5)
L = 0

DOBLOCK1: &
DO J = 0,PENTTOT-1
   LLHUMS( 1: 2) = BULL(POINT  :POINT+ 1)
   LLHUMS( 4: 5) = BULL(POINT+2:POINT+ 3)
   LLHUMS( 7: 7) = BULL(POINT+4:POINT+ 4)
   LLHUMS( 8: 8) = BULL(POINT+6:POINT+ 6)
   LLHUMS(10:11) = BULL(POINT+7:POINT+ 8)
   LLHUMS(13:14) = BULL(POINT+9:POINT+10)
   DO K = 1,5
      LLHUMS(K*3:K*3) = BULL(POINT+11+K:POINT+11+K)
   END DO

DOBLOCK2: &
   DO K = 0,14,3
      LATUNIT = IVALUE(LLHUMS(K+1:K+1))
      IF (CRUDELAT == MISSIN .OR. LATUNIT == MISSIN) THEN
         DCVALS(I + J*5 + K/3) = MISSIN
      ELSE IF (NORTH) THEN
         DCVALS(I + J*5 + K/3) = CRUDELAT+LATUNIT
      ELSE
         DCVALS(I + J*5 + K/3) = CRUDELAT-LATUNIT
      END IF
      LONUNIT = IVALUE(LLHUMS(K+2:K+2))
      IF (CRUDELON == MISSIN .OR. LONUNIT == MISSIN) THEN
         DCVALS(I + NOBS + J*5 + K/3) = MISSIN
      ELSE IF (EAST) THEN
         DCVALS(I + NOBS + J*5 + K/3) = CRUDELON+LONUNIT
      ELSE
         DCVALS(I + NOBS + J*5 + K/3) = CRUDELON-LONUNIT
      END IF
      DCVALS(I + NOBS*2 + J*5 + K/3) = PBASE
      DCVALS(I + NOBS*3 + J*5 + K/3) = PTOP
      RELHUM = IVALUE(LLHUMS(K+3:K+3))
      IF (RELHUM /= MISSIN) RELHUM = RELHUM*10
      DCVALS(I + NOBS*4 + J*5 + K/3) = RELHUM
      L = J*5 + K/3 + 1
   END DO DOBLOCK2
   POINT = POINT+18
END DO DOBLOCK1

LLHUMS( 1: 2) = BULL(POINT  :POINT+ 1)
LLHUMS( 4: 5) = BULL(POINT+2:POINT+ 3)
LLHUMS( 7: 7) = BULL(POINT+4:POINT+ 4)
LLHUMS( 8: 8) = BULL(POINT+6:POINT+ 6)
LLHUMS(10:11) = BULL(POINT+7:POINT+ 8)
LLHUMS(13:14) = BULL(POINT+9:POINT+10)
DO M = 1,5
   LLHUMS(M*3:M*3) = BULL(POINT+11+M:POINT+11+M)
END DO
DO M = 0, LASTFEW*3 - 1, 3
   LATUNIT = IVALUE(LLHUMS(M+1:M+1))
   IF (CRUDELAT == MISSIN .OR. LATUNIT == MISSIN) THEN
      DCVALS(I + L+ M/3) = MISSIN
   ELSE IF (NORTH) THEN
      DCVALS(I + L+ M/3) = CRUDELAT+LATUNIT
   ELSE
      DCVALS(I + L+ M/3) = CRUDELAT-LATUNIT
   END IF
   LONUNIT = IVALUE(LLHUMS(M+2:M+2))
   IF (CRUDELON == MISSIN .OR. LONUNIT == MISSIN) THEN
      DCVALS(I + NOBS + L+ M/3) = MISSIN
   ELSE IF (EAST) THEN
      DCVALS(I + NOBS + L+ M/3) = CRUDELON+LONUNIT
   ELSE
      DCVALS(I + NOBS + L+ M/3) = CRUDELON-LONUNIT
   END IF
   DCVALS(I + NOBS*2 + L+ M/3) = PBASE
   DCVALS(I + NOBS*3 + L+ M/3) = PTOP
   RELHUM = IVALUE(LLHUMS(M+3:M+3))
   IF (RELHUM /= MISSIN) RELHUM = RELHUM*10
   DCVALS(I + NOBS*4 + L+ M/3) = RELHUM
END DO
IF (LASTFEW > 0) POINT = POINT+18

IF (NNTOT < NOBS) THEN
   IF (NN > 0) I = I + NN
   GOTO 10
END IF

RETURN
END SUBROUTINE SATOB7
