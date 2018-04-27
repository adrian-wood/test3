SUBROUTINE UASORT(ARRAY,QCBIT_ARRAY,VALUES,STANDRD)

!-----------------------------------------------------------------------
!
! PROGRAM       : UASORT (upper air storage)
!
! PURPOSE       : To combine values & q/c bits in one array for
!                 encoding, sorting levels into pressure or height order
!
! CALLED BY     : UAEDIT
!
! CALLS         : SORTR
!
! ARGUMENTS     : (1) - values                                (i)
!                 (2) - q/c bits                              (i)
!                 (3) - merged values & q/c bits              (o)
!                 (4) - standard/sig flag, true if standard   (i)
!
! REVISION INFO :
!
!
! $Workfile: uasort.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 24/01/2011 13:05:29$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE METDB_COM_mod, ONLY: RMISS
USE SORTR_mod

IMPLICIT NONE

! Interface Arguments

REAL, INTENT(IN)     :: ARRAY(:)           ! (a1) array of decoded values
REAL, INTENT(IN)     :: QCBIT_ARRAY(:)     ! (a2) array of qc bits
REAL, INTENT(INOUT)  :: VALUES(:)          ! (a3)
LOGICAL, INTENT(IN)  :: STANDRD            ! (a4) true if standard levels

! Local Parameters

INTEGER  :: NLEVEL         ! number of levels in profile
INTEGER  :: NCLOUD         ! number of cloud levels
INTEGER  :: NLSLOT         ! subscript of NLEVEL in ARRAY
INTEGER  :: NCSLOT         ! subscript of NCLOUD in ARRAY
INTEGER  :: NINPUT         ! number of input values
INTEGER  :: LEVEL1         ! start of first profile level in VALUES
INTEGER  :: I              ! loop variables
INTEGER  :: J              ! loop variables
INTEGER  :: PMASK(14)      ! mask to sort on pressure
INTEGER  :: HMASK(14)      ! mask to sort on height

DATA HMASK/0,2,3*0,1,8*0/  ! sort on height & then kind of level
DATA PMASK/0,2,0,-1,10*0/  ! sort on pressure & then kind of level
                           !  (descending pressure, hence -1)
SAVE

! --------------------------------------------------------------------

! To merge q/c bits with values we need to know where the replication
! counts are, for cloud layers (if it's a significant part) or the
! profile.  There are no q/c bits corresponding to these counts (or
! to the 031021 at the start).
!  The numbers below correspond to the current descriptor sequences
! for encoding standard & significant levels, and will have to be
! changed if the sequences change.

IF (STANDRD) THEN
  NCSLOT=0                    ! no cloud if standard
  NLSLOT=14
ELSE
  NCSLOT=18
  NCLOUD=ARRAY(NCSLOT)
  NLSLOT=NCSLOT+4*NCLOUD+1    ! 4 values per cloud layer
END IF

NLEVEL=ARRAY(NLSLOT)
NINPUT=NLSLOT+7*NLEVEL        ! 7 values per profile level
IF (STANDRD) NINPUT=NINPUT+2  ! wind shears on end if standard

! Merge the two input arrays, putting q/c bits in front of all values
! except 031021 at start & replication counts.

J=1                           ! output subscript
DO I=1,NINPUT
  IF (I == 1 .OR. I == NCSLOT .OR. I == NLSLOT) THEN
    VALUES(J)=ARRAY(I)
    J=J+1
    IF (I == NLSLOT) LEVEL1=J
  ELSE
    VALUES(J)=QCBIT_ARRAY(I)
    VALUES(J+1)=ARRAY(I)
    J=J+2
  END IF
END DO

! Sort with pressure mask if first level has pressure, height otherwise.
! This puts surface, tropopause & max wind levels in order and merges
! significant temperatures & winds.
! Levels with the same pressure or height will be ordered by the "kind
! of level" flag (first value for each level).

IF (VALUES(LEVEL1+3) > RMISS) THEN
  CALL SORTR(VALUES(LEVEL1:),14,NLEVEL,PMASK)
ELSE
  CALL SORTR(VALUES(LEVEL1:),14,NLEVEL,HMASK)
END IF

RETURN
END SUBROUTINE UASORT
