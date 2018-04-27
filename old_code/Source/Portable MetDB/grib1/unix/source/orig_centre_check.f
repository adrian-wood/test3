!+    Checks the validity of a GRIB originating centre
! $Header: orig_centre_check.f, 1, 27/02/2007 16:11:07, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Orig_Centre_Check(centre, msglvl, err_unit,
     &  error_status)

      IMPLICIT NONE

!     Description:
!       Checks the validity of a GRIB originating centre. If
!       centre is not valid, and msglvl indicates that error messages
!       should be printed, an error message is issued on err_unit.
!
!     Current Code Owner: Andrew Edmunds, OS(US)
!
!     History:
!     Version  Date      Comment
!
!       1.0    5/97      Original code. (A. Edmunds)
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!     Scalar arguments with INTENT(IN):

      INTEGER    centre              ! Originating centre, as defined
                                     ! in GRIB code table C-1
      INTEGER    msglvl              ! Level of diagnostics required
      INTEGER    err_unit            ! Unit number for error messages

!     Error status:

      INTEGER    error_status        ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    Reserved               ! Possible status values
      PARAMETER (Reserved = 0)
      INTEGER    Defined
      PARAMETER (Defined = 1)

      INTEGER    No_Error               ! Levels of error
      PARAMETER (No_Error = 0)
      INTEGER    Error
      PARAMETER (Error = 3)
      
      INTEGER    Min_Centre             ! Possible range of centre
      PARAMETER (Min_Centre = 0)        ! numbers
      INTEGER    Max_Centre
      PARAMETER (Max_Centre = 2**8 - 1)

!     Local Scalars:

      INTEGER     table_entry        ! Current entry in table_c1
      LOGICAL     in_range           ! Is centre in the range of
                                     ! possible values?

!     Local Arrays:

!     Status of each time unit

      INTEGER     table_c1(Min_Centre : Max_Centre)

!     Functions and subroutines used:

      EXTERNAL   Range_Check

!-    End of header

!     Set status of each possible originating centre

      DO table_entry = Min_Centre, Max_Centre
        table_c1(table_entry) = Reserved
      END DO

      table_c1(  1) = Defined
      table_c1(  2) = Defined
      table_c1(  3) = Defined
      table_c1(  4) = Defined
      table_c1(  5) = Defined
      table_c1(  6) = Defined
      table_c1(  7) = Defined
      table_c1(  8) = Defined
      table_c1(  9) = Defined
      table_c1( 10) = Defined
      table_c1( 11) = Defined
      table_c1( 12) = Defined
      table_c1( 13) = Defined
      table_c1( 14) = Defined
      table_c1( 15) = Defined
      table_c1( 16) = Defined
      table_c1( 17) = Defined
      table_c1( 18) = Defined
      table_c1( 19) = Defined
      table_c1( 20) = Defined
      table_c1( 21) = Defined
      table_c1( 22) = Defined
      table_c1( 23) = Defined
      table_c1( 26) = Defined
      table_c1( 27) = Defined
      table_c1( 28) = Defined
      table_c1( 29) = Defined
      table_c1( 30) = Defined
      table_c1( 31) = Defined
      table_c1( 32) = Defined
      table_c1( 33) = Defined
      table_c1( 34) = Defined
      table_c1( 35) = Defined
      table_c1( 36) = Defined
      table_c1( 37) = Defined
      table_c1( 38) = Defined
      table_c1( 39) = Defined
      table_c1( 40) = Defined
      table_c1( 41) = Defined
      table_c1( 42) = Defined
      table_c1( 43) = Defined
      table_c1( 44) = Defined
      table_c1( 45) = Defined
      table_c1( 46) = Defined
      table_c1( 51) = Defined
      table_c1( 52) = Defined
      table_c1( 53) = Defined
      table_c1( 54) = Defined
      table_c1( 55) = Defined
      table_c1( 57) = Defined
      table_c1( 58) = Defined
      table_c1( 59) = Defined
      table_c1( 60) = Defined
      table_c1( 64) = Defined
      table_c1( 65) = Defined
      table_c1( 66) = Defined
      table_c1( 67) = Defined
      table_c1( 69) = Defined
      table_c1( 70) = Defined
      table_c1( 74) = Defined
      table_c1( 75) = Defined
      table_c1( 76) = Defined
      table_c1( 78) = Defined
      table_c1( 79) = Defined
      table_c1( 80) = Defined
      table_c1( 81) = Defined
      table_c1( 82) = Defined
      table_c1( 83) = Defined
      table_c1( 85) = Defined
      table_c1( 86) = Defined
      table_c1( 87) = Defined
      table_c1( 88) = Defined
      table_c1( 89) = Defined
      table_c1( 90) = Defined
      table_c1( 91) = Defined
      table_c1( 92) = Defined
      table_c1( 93) = Defined
      table_c1( 94) = Defined
      table_c1( 95) = Defined
      table_c1( 96) = Defined
      table_c1( 97) = Defined
      table_c1( 98) = Defined
      table_c1( 99) = Defined

!     Assume centre invalid initially

      error_status = Error
      
!     Check that centre is in the range of possible values

      CALL Range_Check(centre, Min_Centre, Max_Centre,
     &  'originating centre', msglvl, err_unit, in_range)
      
      IF ( in_range ) THEN

!       Check that it has a meaning defined in GRIB code table C-1

        IF ( table_c1(Centre) .EQ. Defined ) THEN
          error_status = No_Error
        ELSE
          IF ( msglvl .LT. Error ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' ERROR - specified originating centre, ', centre,
     &        ', is undefined'
          END IF
        END IF
      
      END IF

      END ! SUBROUTINE Orig_Centre_Check
