!+    Decodes a description of a time range from GRIB
! $Header: time_range_dec.f, 1, 27/02/2007 16:11:10, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Time_Range_Dec(time_range_bytes, p1, p2,
     &  time_range_ind, num_in_avg)

      IMPLICIT NONE

!     Description:
!       Decodes a description of a time range from GRIB format.
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
!     Array arguments with INTENT(IN):

      INTEGER    time_range_bytes(5) ! Encoded time range description
      
!     Scalar arguments with INTENT(OUT):

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
      
      INTEGER    Min_Time_Range_Ind     ! Valid range of time range
      PARAMETER (Min_Time_Range_Ind = 0)! indicators
      INTEGER    Max_Time_Range_Ind
      PARAMETER (Max_Time_Range_Ind = 2**8 - 1)

!     Local Arrays:

!     Type of encoding for each time range indicator

      INTEGER     table_5(Min_Time_Range_Ind : Max_Time_Range_Ind, 4)

!     Functions and subroutines used:

      EXTERNAL   Time_Range_Table

!-    End of header

!     Set type of encoding for each time range indicator

      CALL Time_Range_Table(table_5)

!     Third byte is the time range indicator

      time_range_ind = time_range_bytes(3)
      
      IF ( table_5(time_range_ind, P1_T) .EQ. Req_16_Bit ) THEN

!       For this type of time range, P1 is a 16-bit value and P2
!       is not used

        p1 = 256 * time_range_bytes(1) + time_range_bytes(2)
        p2 = 0
      ELSE

!       P1 and P2 are 8-bit values, or are unused

        p1 = time_range_bytes(1)
        p2 = time_range_bytes(2)
      END IF

!     Number in average is always either a 16-bit value, or unused
      
      num_in_avg = 256 * time_range_bytes(4) + time_range_bytes(5)
      
      END ! SUBROUTINE Time_Range_Dec
