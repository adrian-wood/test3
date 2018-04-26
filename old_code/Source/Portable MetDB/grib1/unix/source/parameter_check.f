!+    Checks the validity of a GRIB physical parameter
! $Header: parameter_check.f, 1, 27/02/2007 16:11:08, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Parameter_Check(param, msglvl, err_unit, error_status)

      IMPLICIT NONE

!     Description:
!       Checks the validity of a GRIB physical parameter. If param
!       is not valid, and msglvl indicates that error messages should
!       be printed, an error message is issued on err_unit.
!
!     Current Code Owner: Andrew Edmunds, OS(US)
!
!     History:
!     Version  Date      Comment
!
!       1.0    6/97      Original code. (A. Edmunds)
!
!     Code Description:
!       FORTRAN 77 with extensions recommended in the Met. Office
!       F77 Standard.
!
!     Scalar arguments with INTENT(IN):

      INTEGER    param               ! Parameter, as defined in
                                     ! GRIB code table 2
      INTEGER    msglvl              ! Level of diagnostics required
      INTEGER    err_unit            ! Unit number for error messages

!     Error status:

      INTEGER    error_status        ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    No_Error               ! Levels of error
      PARAMETER (No_Error = 0)
      INTEGER    Warning
      PARAMETER (Warning = 2)
      INTEGER    Error
      PARAMETER (Error = 3)
      
      INTEGER    Min_Param              ! Possible range of parameters
      PARAMETER (Min_Param = 0)
      INTEGER    Max_Param
      PARAMETER (Max_Param = 2**8 - 1)

!     Local Scalars:

      LOGICAL     in_range           ! Is param in the range of
                                     ! possible values?

!     Functions and subroutines used:

      EXTERNAL   Range_Check

!-    End of header

!     Assume parameter invalid initially

      error_status = Error
      
!     Check that parameter is in the range of possible values

      CALL Range_Check(param, Min_Param, Max_Param,
     &  'indicator of parameter', msglvl, err_unit, in_range)
      
      IF ( in_range ) THEN

!       Check that it has a meaning defined in GRIB code table 2

        IF ( ((param .GE. 1) .AND. (param .LE. 127)) .OR.
     &        (param .EQ. 255)                            ) THEN
          error_status = No_Error
        
        ELSE IF ( (param .GE. 128) .AND. (param .LE. 254) ) THEN
          error_status = Warning
          IF ( msglvl .LT. Warning ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' WARNING - specified indicator of parameter, ', param,
     &        ' is non-standard'
          END IF
        
        ELSE
          IF ( msglvl .LT. Error ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' ERROR - specified indicator of parameter, ', param,
     &        ', is undefined'
          END IF
        END IF
      
      END IF

      END ! SUBROUTINE Parameter_Check
