      SUBROUTINE OZSOUT(RARRAY,MAXOBS,MAXELS,  &
                    NELEM,NOBS,CSTR,IERR)
!-----------------------------------------------------------------------
!
! PROGRAM       : OZSOUT
!
! PURPOSE       : Output OZONESAT data for BADC
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

      INTEGER       MAXELS     ! MAX NUMBER ELEMENTS IN INPUT ARRAY
      INTEGER       MAXOBS     ! MAX NUMBER OF OBS IN INPUT ARRAY
      INTEGER       NELEM      ! NUMBER ELEMENTS IN INPUT ARRAY
      INTEGER       NOBS       ! NUMBER OF OBSERVATIONS IN INPUT ARRAY
      INTEGER       IERR       ! Return Code
      INTEGER       I,J,K,L    ! Loop counters
      INTEGER       OBNUM      ! Main loop counter

      INTEGER       N_HEADER_ELEMS     ! NUMBER HEADER ELEMS
      INTEGER       N_LEVELS           ! NUMBER LEVELS
      INTEGER       NELEMS_PER_LINE    ! NUMBER ELEMS PER LINE
      INTEGER       IOS                ! File status
      INTEGER       DATE(6)            ! DATE ARRAY
      INTEGER       TOR1(5)            ! TIME OF RECEIPT ARRAY
      INTEGER       REP_START          !Start of replicated section
      INTEGER       MAX_LEVELS         ! Max replications in req str

      REAL          RARRAY(MAXOBS,MAXELS) ! INPUT OBSERVATIONS ARRAY
                                          !  ARRAY
      CHARACTER*(*) CSTR(MAXOBS)          ! FOR CHARACTER ELEMENTS

      DATA REP_START/43/
      DATA MAX_LEVELS/40/


      NELEMS_PER_LINE=4  !  hdr  d/t       fov dnsy
      N_HEADER_ELEMS= 44 !   4 + 6+5 + 6 + 8 +10  +5
      N_LEVELS=0         !
      IERR = 0

! LOOP THROUGH REPORTS PASSED FROM MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP

        DO J=1,5
          DATE(J) = NINT(RARRAY(OBNUM,J))        !Obs Date/time
          TOR1(J) = NINT(RARRAY(OBNUM,J+6)) !Receipt Date/time
        ENDDO
        DATE(6) = NINT(RARRAY(OBNUM,6))        !Obs Date/time

        N_LEVELS=MIN(MAX_LEVELS,NINT(RARRAY(OBNUM,42)))
!
!* OUTPUT RECORDS
!
        WRITE(10,  &
             '(A8,3('','',I4),  &
             6('','',I8),5('','',I8),  &
             30('','',F13.3))',IOSTAT=IOS,ERR=9999)  &
        'OZONESAT',N_HEADER_ELEMS,N_LEVELS,NELEMS_PER_LINE,  &
        (DATE(J),J=1,6),(TOR1(J),J=1,5),  &
        (RARRAY(OBNUM,J),J=12,41)

!---------------------------------------------------------------------
! Write out the replicated section. Format statement depends on whether
! the ozone density element is missing or not.
!---------------------------------------------------------------------

        DO I = 1,N_LEVELS
          K=(I-1)*NELEMS_PER_LINE+REP_START
          L=K+NELEMS_PER_LINE-1

          IF(RARRAY(OBNUM,K+2).gt.-9999999.)THEN
            WRITE(10,'(1X,2(F13.3,'',''),F13.10,'','',F13.3)',  &
                     IOSTAT=IOS,ERR=9999)  &
             (RARRAY(OBNUM,J),J=K,L)
          ELSE
            WRITE(10,'(1X,F13.3,3('','',F13.3))',  &
                     IOSTAT=IOS,ERR=9999)  &
             (RARRAY(OBNUM,J),J=K,L)
          ENDIF

        ENDDO

      END DO                                  ! REPORTS LOOP

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' occurred while writing out OZONESAT data')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END
