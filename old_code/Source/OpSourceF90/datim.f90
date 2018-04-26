!-----------------------------------------------------------------------
!
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
! $Revision: 1$
! $Date: 30/01/2006 20:21:56$
! $Source: /home/us0400/mdb/op/lib/source/RCS/datim.f90,v $
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:56    Sheila Needham  
! $
! Revision 1.1  2000/02/29 16:04:24  usmdb
! Initial revision
!
!-----------------------------------------------------------------------

      subroutine datim(Systim)

      implicit none

      integer, dimension(10)             :: values
      integer, intent(out), dimension(8) :: Systim

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

      end subroutine datim
