SUBROUTINE DATIMGP(GROUP,BADTAF)

!-----------------------------------------------------------------------
!
! ROUTINE       : DATIMGP
!
! PURPOSE       : To check that a group is a possible time group,
!                 i.e. that it has either 4 or 6 figures followed
!                 by either space or Z.
!
! CALLED BY     : TAFBUL
!
! CALLS         : IVALUE
!
! ARGUMENTS     : (1) GROUP    (start of) group to be checked
!                 (2) BADTAF   flag set if not 4 or 6 figures
!                              or not space or Z after them
!
! REVISION INFO :
!
! $Workfile: datimgp.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/12/2010 16:09:06$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 16:09:06    Alison Weir     Add
!       USE ivalue_mod
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE ivalue_mod

IMPLICIT NONE

! Arguments:
CHARACTER(LEN=*),INTENT(IN)  :: GROUP
LOGICAL,         INTENT(OUT) :: BADTAF

! Local declarations:
INTEGER      ::  N              ! =4 or =6 if that many figures, else 0

!-----------------------------------------------------------------------
! See if there are 4 figures (IVALUE returns missing data if not).
! If so, see if there are 6.  In either case see if space or Z follows.
!-----------------------------------------------------------------------

BADTAF=.FALSE.
N=0
IF (IVALUE(GROUP(1:4)) >= 0) N=4
IF (N == 0) THEN
  BADTAF=.TRUE.
ELSE
  IF (IVALUE(GROUP(5:6)) >= 0) N=6
  IF (GROUP(N+1:N+1) /= ' ' .AND. GROUP(N+1:N+1) /= 'Z') THEN
    BADTAF=.TRUE.
  END IF
END IF

RETURN
END SUBROUTINE DATIMGP
