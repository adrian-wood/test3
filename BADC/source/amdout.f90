      SUBROUTINE AMDOUT(RARRAY,MAXOBS,MAXELS, &
                    NELEM,NOBS,CSTR,IERR)
!-----------------------------------------------------------------------
!
! PROGRAM       : AMDOUT
!
! PURPOSE       : Output AMDAR data for BADC
!
! DESCRIPTION   : Formatted output, one line per observation
!
! ARGUMENTS     : RARRAY (I) Data from MetDB
!                 MAXOBS (I) Dimension of RARRAY
!                 MAXELS (I) Dimension of RARRY
!                 NELEM  (I) Number of data elements
!                 NOBS   (I) Number of observations
!                 CSTR   (I) Character elements
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
!  2    Met_DB_Project 1.1         02/06/2009 13:16:07    Sheila Needham
!       Minor changes to comments following review
!  1    Met_DB_Project 1.0         01/06/2009 09:45:40    Sheila Needham
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

      INTEGER       MAXELS     ! MAX NUMBER ELEMENTS IN INPUT ARRAY
      INTEGER       MAXOBS     ! MAX NUMBER OF OBS IN INPUT ARRAY
      INTEGER       NELEM      ! NUMBER ELEMENTS IN INPUT ARRAY
      INTEGER       NOBS       ! NUMBER OF OBSERVATIONS IN INPUT ARRAY
      INTEGER       IERR       ! Return Code
      INTEGER       I,J,K      ! Loop counters
      INTEGER       OBNUM      ! Main loop counter

      INTEGER       N_HEADER_ELEMS     ! NUMBER HEADER ELEMS
      INTEGER       N_LEVELS           ! NUMBER LEVELS
      INTEGER       NELEMS_PER_LINE    ! NUMBER ELEMS PER LINE
      CHARACTER*8   CALLSIGN
      CHARACTER*8   REGID              ! Registration number
      INTEGER       IOS                ! File status
      INTEGER       DATE(6)            ! DATE ARRAY
      INTEGER       TOR1(5)            ! TIME OF RECEIPT ARRAY

      REAL          RARRAY(MAXOBS,MAXELS) ! INPUT OBSERVATIONS ARRAY
                                          !  ARRAY
      CHARACTER*(*) CSTR(MAXOBS)          ! FOR CHARACTER ELEMENTS

      NELEMS_PER_LINE=0  ! Not needed for 1-D data
      N_HEADER_ELEMS=0   !
      N_LEVELS=0         !
      IERR = 0

! LOOP THROUGH REPORTS PASSED FROM MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP
        REGID='AIRCRAFT'        !Registration number
        IF(RARRAY(OBNUM,12).GT.-9999999.0)THEN
          IF(CSTR(OBNUM)(1:8).NE.'        ') &
                 REGID=CSTR(OBNUM)(1:8)
        ENDIF

        CALLSIGN='AIRCRAFT'        !Callsign
        IF(RARRAY(OBNUM,13).GT.-9999999.0)THEN
          IF(CSTR(OBNUM)(9:16).NE.'        ') &
                 CALLSIGN=CSTR(OBNUM)(9:16)
        ENDIF

        DO J=1,5
          DATE(J) = NINT(RARRAY(OBNUM,J))        !Obs Date/time
          TOR1(J) = NINT(RARRAY(OBNUM,J+6)) !Receipt Date/time
        ENDDO
        DATE(6) = NINT(RARRAY(OBNUM,6))

!
!* OUTPUT RECORDS
!
        WRITE(10, &
             '(A8,3('','',I4),'','',I4.4, &
             4('','',I2.2),'','',I8,'','',I4.4,4('','',I2.2),'','', &
             A8,'','',A8, &
             22('','',F13.3))',IOSTAT=IOS,ERR=9999) &
        'AMDARS',N_HEADER_ELEMS,N_LEVELS,NELEMS_PER_LINE, &
        (DATE(J),J=1,6),(TOR1(J),J=1,5),REGID,CALLSIGN, &
        (RARRAY(OBNUM,J),J=14,35)

      END DO                                  ! REPORTS LOOP

!      close(10)

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' occurred while writing out AMDAR data')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END SUBROUTINE AMDOUT
