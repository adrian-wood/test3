!+    Checks the validity of a GRIB time unit
! $Header: time_unit_check.f, 1, 27/02/2007 16:11:10, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Time_Unit_Check(time_unit, msglvl, err_unit,
     &  error_status)

      IMPLICIT NONE

!     Description:
!       Checks the validity of a GRIB time unit. If time_unit
!       is not valid, and msglvl indicates that error messages should
!       be printed, an error message is issued on err_unit.
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

      INTEGER    time_unit           ! Time unit, as defined in
                                     ! GRIB code table 4
      INTEGER    msglvl              ! Level of diagnostics required
      INTEGER    err_unit            ! Unit number for error messages

!     Error status:

      INTEGER    error_status        ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    Reserved               ! Possible status values
      PARAMETER (Reserved = 0)
      INTEGER    Defined
      PARAMETER (Defined = 1)

      INTEGER    No_Error
      PARAMETER (No_Error = 0)
      INTEGER    Error                  ! Level of error
      PARAMETER (Error = 3)
      
      INTEGER    Min_Time_Unit          ! Possible range of time units
      PARAMETER (Min_Time_Unit = 0)
      INTEGER    Max_Time_Unit
      PARAMETER (Max_Time_Unit = 2**8 - 1)

!     Local Scalars:

      INTEGER     table_entry        ! Current entry in table_4
      LOGICAL     in_range           ! Is time_unit in the range of
                                     ! possible values?

!     Local Arrays:

!     Status of each time unit

      INTEGER     table_4(Min_Time_Unit : Max_Time_Unit)

!     Functions and subroutines used:

      EXTERNAL   Range_Check

!-    End of header

!     Set status of each possible time unit

      DO table_entry = Min_Time_Unit, Max_Time_Unit
        table_4(table_entry) = Reserved
      END DO

      table_4(  0) = Defined
      table_4(  1) = Defined
      table_4(  2) = Defined
      table_4(  3) = Defined
      table_4(  4) = Defined
      table_4(  5) = Defined
      table_4(  6) = Defined
      table_4(  7) = Defined
      table_4( 10) = Defined
      table_4( 11) = Defined
      table_4( 12) = Defined
      table_4(254) = Defined

!     Assume time unit invalid initially

      error_status = Error
      
!     Check that time unit is in the range of possible values

      CALL Range_Check(time_unit, Min_Time_Unit, Max_Time_Unit,
     &  'time unit indicator', msglvl, err_unit, in_range)
      
      IF ( in_range ) THEN

!       Check that it has a meaning defined in GRIB code table 4

        IF ( table_4(time_unit) .EQ. Defined ) THEN
          error_status = No_Error
        ELSE
          IF ( msglvl .LT. Error ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' ERROR - specified time unit indicator, ', time_unit,
     &        ', is undefined'
          END IF
        END IF
      
      END IF

      END ! SUBROUTINE Time_Unit_Check
