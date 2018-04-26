!+    Encodes a description of a time range in GRIB
! $Header: time_range_enc.f, 1, 27/02/2007 16:11:10, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Time_Range_Enc(p1, p2, time_range_ind, num_in_avg,
     &  msglvl, err_unit, time_range_bytes, error_status)

      IMPLICIT NONE

!     Description:
!       Encodes a description of a time range in GRIB
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

!     First time interval (refered to as P1 in the WMO Manual on Codes)
!     if required for this type of time range

      INTEGER    p1

!     Second time interval (refered to as P2 in the WMO Manual on
!     Codes) if required for this type of time range

      INTEGER    p2
      
!     Time range indicator, as defined in GRIB code table 5

      INTEGER    time_range_ind

!     Number included in average or accumulation, if required for
!     this type of time range

      INTEGER    num_in_avg
      
      INTEGER    msglvl              ! Level of diagnostics required
      INTEGER    err_unit            ! Unit number for error messages

!     Array arguments with INTENT(OUT):

      INTEGER    time_range_bytes(5) ! Encoded time range description
      
!     Error status:

      INTEGER    error_status        ! +ve => warning or fatal error

!     Local Parameters:

      INTEGER    Time_Range_Ind_T      ! Pieces of information held
      PARAMETER (Time_Range_Ind_T = 1) ! for each type of time range
      INTEGER    P1_T
      PARAMETER (P1_T = 2)
      INTEGER    P2_T
      PARAMETER (P2_T = 3)
      INTEGER    Num_In_Avg_T
      PARAMETER (Num_In_Avg_T = 4)

      INTEGER    Reserved               ! Possible states for each
      PARAMETER (Reserved = 0)          ! parameter
      INTEGER    Defined
      PARAMETER (Defined = 1)
      INTEGER    Not_Required
      PARAMETER (Not_Required = 2)
      INTEGER    Req_8_Bit
      PARAMETER (Req_8_Bit = 3)
      INTEGER    Req_16_Bit
      PARAMETER (Req_16_Bit = 4)
      
      INTEGER    No_Error               ! Levels of error
      PARAMETER (No_Error = 0)
      INTEGER    Error
      PARAMETER (Error = 3)
      
      INTEGER    Min_Time_Range_Ind     ! Valid range of time range
      PARAMETER (Min_Time_Range_Ind = 0)! indicators
      INTEGER    Max_Time_Range_Ind
      PARAMETER (Max_Time_Range_Ind = 2**8 - 1)
      INTEGER    Min_16_Bit             ! Valid range for a 16-bit
      PARAMETER (Min_16_Bit = 0)        ! parameter
      INTEGER    Max_16_Bit
      PARAMETER (Max_16_Bit = 2**16 - 1)
      INTEGER    Min_8_Bit              ! Valid range for an 8-bit
      PARAMETER (Min_8_Bit = 0)         ! parameter
      INTEGER    Max_8_Bit
      PARAMETER (Max_8_Bit = 2**8 - 1)


!     Local Scalars:

      LOGICAL     ok                 ! Have all values have been
                                     ! valid so far?

!     Local Arrays:

!     Type of encoding for each time range indicator

      INTEGER     table_5(Min_Time_Range_Ind : Max_Time_Range_Ind, 4)

!     Functions and subroutines used:

      EXTERNAL   Time_Range_Table
      EXTERNAL   Range_Check

!-    End of header

!     Set type of encoding for each time range indicator

      CALL Time_Range_Table(table_5)

      error_status = No_Error
      
!     Check that time range indicator is in range

      CALL Range_Check(time_range_ind, Min_Time_Range_Ind,
     &  Max_Time_Range_Ind, 'time range indicator', msglvl, err_unit,
     &  ok)
      
      IF ( ok ) THEN

!       Check that time range indicator has a defined meaning in
!       GRIB code table 5

        IF ( table_5(time_range_ind, Time_Range_Ind_T) .EQ. Defined )
     &  THEN
          time_range_bytes(3) = time_range_ind
        ELSE
          ok = .FALSE.
          IF ( msglvl .LT. Error ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' ERROR - specified time range indicator, ',
     &        time_range_ind, ', is undefined'
          END IF
        END IF
      END IF
      
      IF ( ok ) THEN

!       Set default values which will be encoded if P1, P2 or
!       number averaged are not required to describe the time range.
!       In some of these cases the standard specifies that the values
!       must be zero, in other cases it does not specify what value
!       should be used. Using zero in all cases should be OK.

        time_range_bytes(1) = 0    ! P1
        time_range_bytes(2) = 0    ! P2
        time_range_bytes(4) = 0    ! Number in average (2 bytes)
        time_range_bytes(5) = 0
        
        IF ( table_5(time_range_ind, P1_T) .EQ. Req_8_Bit ) THEN

!         P1 is required, and should be encoded as an 8 bit value

          CALL Range_Check(p1, Min_8_Bit, Max_8_Bit,
     &      'P1 time interval', msglvl, err_unit, ok)
          IF ( ok ) THEN
            time_range_bytes(1) = p1
          END IF
        END IF
        
        IF ( table_5(time_range_ind, P1_T) .EQ. Req_16_Bit ) THEN

!         P1 is required and should be encoded as a 16 bit value

          CALL Range_Check(p1, Min_16_Bit, Max_16_Bit,
     &      'P1 time interval', msglvl, err_unit, ok)
          IF ( ok ) THEN
            time_range_bytes(1) = p1 / 256
            time_range_bytes(2) = Mod(p1, 256)
          END IF
        END IF
      END IF
      
      IF ( ok ) THEN
        IF ( table_5(time_range_ind, P2_T) .EQ. Req_8_Bit ) THEN

!         P2 is required and should be encoded as an 8 bit value

          CALL Range_Check(p2, Min_8_Bit, Max_8_Bit,
     &      'P2 time interval', msglvl, err_unit, ok)
          IF ( ok ) THEN
            time_range_bytes(2) = p2
          END IF
        END IF
      END IF

      IF ( ok ) THEN
        IF ( table_5(time_range_ind, Num_In_Avg_T) .EQ. Req_16_Bit )
     &  THEN

!         Number in average is required and should be encoded as a
!         16 bit value

          CALL Range_Check(num_in_avg, 1, Max_16_Bit,
     &      'number in average', msglvl, err_unit, ok)
          IF ( ok ) THEN
            time_range_bytes(4) = num_in_avg / 256
            time_range_bytes(5) = Mod(num_in_avg, 256)
          END IF
        END IF
      END IF

!     Signal an error if any of the parameters were out of range
      
      IF ( .NOT. ok ) THEN
        error_status = Error
      END IF
      
      END ! SUBROUTINE Time_Range_Enc
