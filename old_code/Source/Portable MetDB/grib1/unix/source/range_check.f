!+    Check whether a value is in range
! $Header: range_check.f, 1, 27/02/2007 16:11:09, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Range_Check(value, minimum, maximum, param_name,
     &  msglvl, err_unit, in_range)

      IMPLICIT NONE

!     Description:
!       Checks whether a value is within a specified range. If the
!       value is out of range, and msglvl indicates that error messages
!       should be printed, an error message is issued on err_unit.
!
!     Current Code Owner: Andrew Edmunds OS(US)
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

      INTEGER    value             ! Value to be checked
      INTEGER    minimum           ! Minimum acceptable value
      INTEGER    maximum           ! Maximum acceptable value
      CHARACTER*(*) param_name     ! Name of parameter being checked
                                   ! (for error message)
      INTEGER    msglvl            ! Level of diagnostic messages
                                   ! required
      INTEGER    err_unit          ! Unit number for error messages

!     Scalar arguments with INTENT(OUT):

      LOGICAL    in_range          ! True iff value is in range

!     Local parameters

      INTEGER    Error             ! Level of error
      PARAMETER (Error = 3)
      
!-    End of header

      in_range = (value .GE. minimum) .AND. (value .LE. maximum)
        
      IF ( .NOT. in_range ) THEN
        IF ( msglvl .LT. Error ) THEN
          WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &      ' ERROR - specified ' // param_name // ', ', value,
     &      ', is out of range'
        END IF
      END IF
      
      END ! SUBROUTINE Range Check
