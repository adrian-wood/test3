SUBROUTINE DATIM(SYSTIM)
!-------------------------------------------------------------------
! Routine       : datim.f90 subroutine.
!
! Description   : fortran 90 version of datim - program to get the
!               : system date and time.
!
! Arguments     : Systim (o/p) : integer*4 Systim(8)
!               :
!               :              : Systim(1) - not set.
!               :              : Systim(2) - not set.
!               :              : Systim(3) - second (0-59)
!               :              : Systim(4) - minute (0-59)
!               :              : Systim(5) - hour   (0-23)
!               :              : Systim(6) - day    (0-31)
!               :              : Systim(7) - month  (1-12)
!               :              : Systim(8) - year   (yyyy)
!
! Calls         : DATE_AND_TIME : Fortran 90 intrinsic function.
!                                 (Standard across systems)
!
! Change record :
!
! $Workfile: datim.f90$ $Folder: OpSource$
!
! $Revision: 3$ $Date: 15/11/2010 14:34:16$
!-----------------------------------------------------------------------
! $Log:
!  3    MetDB_Refresh 1.2         15/11/2010 14:34:16    Richard Weedon  Rework
!        from peer review
!  2    MetDB_Refresh 1.1         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  1    MetDB_Refresh 1.0         18/10/2010 09:34:28    Sheila Needham  New
!       for BUFR ed 4
! $
!
!-----------------------------------------------------------------------
!
!
IMPLICIT NONE

integer                            :: values(10)
integer, intent(out)               :: Systim(8)

character (len=10)                 :: cdate
character (len=10)                 :: ctime
character (len=10)                 :: czone

call DATE_AND_TIME(cdate,ctime,czone,values)

Systim(1) = 0
Systim(2) = 0
Systim(3) = values(7)
Systim(4) = values(6)
Systim(5) = values(5)
Systim(6) = values(3)
Systim(7) = values(2)
Systim(8) = values(1)

END SUBROUTINE DATIM
