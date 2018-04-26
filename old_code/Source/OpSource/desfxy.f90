SUBROUTINE DESFXY(DESCR, F,X,Y)

IMPLICIT NONE

!-----------------------------------------------------------------------
!
! ROUTINE       : DESFXY
!
! PUTPOSE       : to split up a 16-bit BUFR descriptor into
!                 F (2 bits), X (6 bits) & Y (8 bits).
!
! CALLED BY     : any
!
! ARGUMENTS     : (1) descriptor (passed in a fullword)
!                 (2) F - kind of descriptor (element or operator)
!                 (3) X - class, number of elements
!                 (4) Y - element in class, operation
!
! REVISION INFO :
!
! $Workfile: desfxy.f90$ $Folder: OpSource$
! $Revsision: $ $Date: 27/10/2010 12:03:16$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         27/10/2010 12:03:16    Richard Weedon
!       updated to f95 standard.
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:41    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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

INTEGER,intent(in)   ::  DESCR  ! BUFR descriptor passed IN
INTEGER,intent(out)  ::  F      ! Type of descriptor OUT
INTEGER,intent(out)  ::  X      ! Class if F=0 or 3,
                                ! number of desctors to replicate
                                ! if F=1 or number of elements OUT
INTEGER,intent(out) ::   Y      ! element in class F=0 or 3,
                                ! delayed replication factor if F=1
                                ! or operation if F=2
INTEGER             ::   IFX    ! used to calculate X


IFX=DESCR/256                   ! Chop off the least significant
                                ! octet from DESCR
                          ! as FX are in the 2nd least
                          ! significant octet
F=DESCR/16384             ! Chop off the bits which hold the XY
                          ! and Y values
IF (F > 3) F=F-(F/4)*4    ! F is a table D sequence
X=IFX-(IFX/64)*64         ! Extract value of X which is the
                          ! 6 least significant bits of IFX
Y=DESCR-(DESCR/256)*256   ! calculate Y which is the least
                          ! significant octet of DESCR.

RETURN
END SUBROUTINE DESFXY
