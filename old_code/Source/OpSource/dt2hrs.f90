INTEGER FUNCTION DT2HRS(IY,IM,ID,IH)

!-----------------------------------------------------------------------
!                                                                     *
! SUBROUTINE    : DT2HRS                                              *
!                                                                     *
! PURPOSE       : CONVERTS A DATE IN YYYYMMDDHH FORM TO CENTURY HOURS *
!                                                                     *
! DESCRIPTION   :                                                     *
!                                                                     *
! CALLED BY     :                                                     *
!                                                                     *
! CALLS         : DATE31   IN ZPDATE                                  *
!                                                                     *
! ARGUMENTS     : INPUT                                               *
!                 -----                                               *
!                 IY      INTEGER  YEAR                               *
!                 IM      INTEGER  MONTH                              *
!                 ID      INTEGER  DAY                                *
!                 IH      INTEGER  HOUR                               *
!                                                                     *
!Y2K  16.06.1997  DT2HRS is Year 2000 compliant.
!Y2K                     Routine contains date management/
!                                                                     *
! CHANGE RECORD :                                                     *
!   DATE :-        PURPOSE:-                                          *
! 10/10/90       NEW FUNCTION                                         *
!                                                                     *
!-----------------------------------------------------------------------
! $Log:
!  7    MetDB_Refresh 1.6         17/11/2010 12:21:29    Sheila Needham  Use
!       zpdate
!  6    MetDB_Refresh 1.5         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  5    MetDB_Refresh 1.4         20/10/2010 09:04:07    Stan Kellett    ready
!       for review, comments need taking out from USE statements once mod
!       files all ready
!  4    MetDB_Refresh 1.3         15/10/2010 16:17:35    Stan Kellett    Basic
!       port almost complete just require the modules added for the interface
!       files they will need.
!  3    MetDB_Refresh 1.2         14/10/2010 17:59:43    Stan Kellett    Basic
!       port done upto changing of argument declarations. Ordering of
!       declarations and interface files still to do.
!  2    MetDB_Refresh 1.1         12/10/2010 10:10:13    Stan Kellett
!       Continuation markers changed to f90 standard.
!       Logical operators such as .LE. .GE. .GT. .LT. etc changed to <=, >=,
!       >, and < as part of porting to f90/95
!  1    MetDB_Refresh 1.0         11/10/2010 13:39:02    Stan Kellett
!       Initial f77 versions before porting to f90/95
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE zpdate_mod
IMPLICIT NONE

! Argument declarations
INTEGER, INTENT(IN)  :: IY    ! Year
INTEGER, INTENT(IN)  :: IM    ! Month
INTEGER, INTENT(IN)  :: ID    ! Day
INTEGER, INTENT(IN)  :: IH    ! Hour

! Local variable declarations
INTEGER              :: ICNTD ! Century Hours

!*--- CONVERT TO CENTURY DAY
CALL DATE31(ID,IM,IY,ICNTD)

!*--- AND CENTURY HOUR

DT2HRS=(ICNTD-1)*24+IH

RETURN
END FUNCTION DT2HRS
