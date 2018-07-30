      SUBROUTINE TMPOUT(RARRAY,MAXOBS,MAXELS,  &
                    NELEM,NOBS,CSTR,IERR)
!-----------------------------------------------------------------------
!
! PROGRAM       : TMPOUT
!
! PURPOSE       : Output TEMP data for BADC
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
      INTEGER       I,J,K,L,M  ! Loop counters
      INTEGER       OBNUM      ! Main loop counter
      CHARACTER*9   CALLSIGN   !
      CHARACTER*4   CCCC       ! Collecting centre
      CHARACTER*4   BULLID     ! Bulletin ID.

      INTEGER       N_HEADER_ELEMS     ! NUMBER HEADER ELEMS
      INTEGER       N_LEVELS           ! NUMBER LEVELS
      INTEGER       NELEMS_PER_LINE    ! NUMBER ELEMS PER LINE
      INTEGER       IOS                ! File status
      INTEGER       DATE(6)            ! DATE ARRAY
      INTEGER       TOR1(25)           ! TIME OF RECEIPT ARRAY
      INTEGER       REP_START          ! Ptr to start of replicated
                                       ! section
      INTEGER       MAX_LEVELS         ! defined in request string
      CHARACTER*9   BLANK
!     DATA BLANK/X'FFFFFFFFFFFFFFFFFF'/

      REAL          RARRAY(MAXOBS,MAXELS) ! INPUT OBSERVATIONS ARRAY
                                          !  ARRAY
      CHARACTER*(*) CSTR(MAXOBS)          ! FOR CHARACTER ELEMENTS

      DATA REP_START/58/
      DATA MAX_LEVELS/300/

      do M=1,9
        blank(M:M) = char(255)
      end do

      NELEMS_PER_LINE=7  !
      N_HEADER_ELEMS= 60 !   4 + 30 + 16 + (3*3) + 1
      N_LEVELS=0         !
      IERR = 0

! LOOP THROUGH REPORTS PASSED FROM MDBRET

      DO OBNUM =1,NOBS                        ! REPORTS LOOP

        DO J=1,5
          DATE(J) = NINT(RARRAY(OBNUM,J))        !Obs Date/time
        ENDDO
        DO J=1,25
          TOR1(J) = NINT(RARRAY(OBNUM,J+5)) !Receipt Date/time
        ENDDO


        CALLSIGN='NULL'
        IF(RARRAY(OBNUM,31).GT.-9999999.0)THEN
          IF(CSTR(OBNUM)(1:9).NE.'         '.AND.  &
                CSTR(OBNUM)(1:9).NE.BLANK)THEN
             CALLSIGN=CSTR(OBNUM)(1:9)
          ENDIF
        ENDIF

        CCCC=CSTR(OBNUM)(10:13)  !CCCC
        BULLID=CSTR(OBNUM)(14:17)        ! Bul id

!-----------------------------------------------------------------------
! check the level condition code. If it is non-zero, not all the levels
! have been returned - so just loop round max_levels.
!-----------------------------------------------------------------------

        IF(NINT(RARRAY(OBNUM,56)).EQ.0)THEN
          N_LEVELS=NINT(RARRAY(OBNUM,57))
        ELSE
          N_LEVELS=MAX_LEVELS
        ENDIF
!
!* OUTPUT RECORDS
!
        WRITE(10,  &
             '(A8,3('','',I4),  &
             5('','',I8),25('','',I8),'','',A9,  &
             5('','',F13.3),'','',A4,'','',A4,  &
             18('','',F13.3))',IOSTAT=IOS,ERR=9999)  &
        'TEMP  ',N_HEADER_ELEMS,N_LEVELS,NELEMS_PER_LINE,  &
        (DATE(J),J=1,5),(TOR1(J),J=1,25),  &
         CALLSIGN,  &
        (RARRAY(OBNUM,J),J=32,36),  &
         CCCC,BULLID,  &
        (RARRAY(OBNUM,J),J=39,56)

        DO I = 1,N_LEVELS
          K=(I-1)*NELEMS_PER_LINE+REP_START
          L=K+NELEMS_PER_LINE-1
          WRITE(10,'(1X,F13.3,6('','',F13.3))',IOSTAT=IOS,ERR=9999)  &
           (RARRAY(OBNUM,J),J=K,L)
        ENDDO

      END DO                                  ! REPORTS LOOP

      RETURN

!----------------------------------------------------------------------
!   ERROR HANDLING FOR OUTPUT TO FT10 DATA OUTPUT DATASET
!----------------------------------------------------------------------

 9999 WRITE(6,9990)IOS
 9990 FORMAT(' ERROR ',I4,' occurred while writing out TEMP data')

      IERR = 3001  ! INDICATES WRITE OUT TO FT10 PROBLEM
      RETURN
      END
