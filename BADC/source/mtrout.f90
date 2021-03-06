      SUBROUTINE MTROUT(MAXOBS,NOBS,CREP,IERR)
!-----------------------------------------------------------------------
!
! SUBROUTINE    : MTROUT
!
! PURPOSE       : Output METAR data for BADC
!
! DESCRIPTION   : Reports only
!
! ARGUMENTS     : MAXOBS (I) Max size of CREP array
!               : NOBS   (I) Number of observations
!                 CREP   (I) Report text
!                 IERR   (O) Error Code
!
! CALLED BY     :  MDBRET
!
! CALLS TO      : none
!
!
! REVISION INFO :
!
! MB-575  Convert to f90 on Linux                         Andy Moorhouse
!  2    Met_DB_Project 1.1         02/06/2009 13:16:08    Sheila Needham
!       Minor changes to comments following review
!  1    Met_DB_Project 1.0         01/06/2009 09:45:41    Sheila Needham
!       Initial versions
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2009-18 - MET OFFICE. ALL RIGHTS RESERVED.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The MetDB Team.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE

! Subroutine arguments:
      INTEGER, INTENT(IN)      :: MAXOBS
      INTEGER, INTENT(IN)      :: NOBS
      CHARACTER(*), INTENT(IN) :: CREP(MAXOBS)
      INTEGER, INTENT(OUT)     :: IERR

! Local declarations:
      INTEGER       OBNUM      ! Loop counter
      INTEGER       IOS        ! File status


      IERR = 0

! Loop through reports passed from MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP

!
!* OUTPUT RECORDS
!
        WRITE(10,'(A)',IOSTAT=IOS,ERR=9999)  &
        CREP(OBNUM)

      END DO                                  ! REPORTS LOOP

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' occurred while writing out METAR data')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END
