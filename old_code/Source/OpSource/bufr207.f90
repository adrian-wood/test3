SUBROUTINE BUFR207 (Y, SCALE, WIDTH, REFVAL)

!-----------------------------------------------------------------------
!
! ROUTINE     : BUFR207
!
! CALLED BY   : BUFDATA
!
! PURPOSE     : Change scale, width & reference value together for a
!               BUFR descriptor.
!
! DESCRIPTION : Y gives the required scale change as in 202YYY and must
!               be a positive integer. The reference value is given
!               that scale change, and a new width is calculated from
!               the range implied by the old width (maybe wasting the
!               odd bit).
!
! CALLS       : nothing
!
! ARGUMENTS   :   Name   I/O   Type     Description
!                 ----   ---   ----     -----------
!                 Y        I   I*4  Required scale change (from 207YYY)
!                 SCALE   I/O  I*4  Scale factor (changed by Y)
!                 WIDTH   I/O  I*4  Bit width (changed to give at
!                                    least the same range)
!                 REFVAL  I/O  I*4  Reference value (changed by 10**Y)
!
! REVISION INFO :
!
! $Workfile: bufr207.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 25/10/2010 15:49:00$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         25/10/2010 15:49:00    Richard Weedon
!       updated to f90 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
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

IMPLICIT NONE

INTEGER,INTENT(IN)     :: Y      ! Requested scale change
INTEGER,INTENT(INOUT)  :: SCALE  ! Scale factor (increased by Y)
INTEGER,INTENT(INOUT)  :: WIDTH  ! Bit width
                                 ! (increased by 10*Y/3 rounded up)
INTEGER,INTENT(INOUT)  :: REFVAL ! Reference value (scaled by 10**Y)

SCALE  = SCALE + Y
REFVAL = REFVAL*10**Y
WIDTH  = WIDTH + (10*Y+2)/3

RETURN
END SUBROUTINE BUFR207
