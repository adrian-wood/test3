!+    Initialise an array representing GRIB code table 5
! $Header: time_range_table.f, 1, 27/02/2007 16:11:10, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Time_Range_Table(table_5)

      IMPLICIT NONE

!     Description:
!       Initialises an array representing GRIB code table 5
!
!     Current Code Owner: Andrew Edmunds OS(US)
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

!     Array arguments with INTENT(OUT):

      INTEGER    table_5(Min_Time_Range_Ind : Max_Time_Range_Ind, 4)

!     Local Scalars:

      INTEGER     table_entry        ! Current entry in table_5

!-    End of header

      DO table_entry = Min_Time_Range_Ind, Max_Time_Range_Ind
        table_5(table_entry, Time_Range_Ind_T) = Reserved
      END DO

      table_5(  0, Time_Range_Ind_T) = Defined
      table_5(  0, P1_T            ) = Req_8_Bit
      table_5(  0, P2_T            ) = Not_Required
      table_5(  0, Num_In_Avg_T    ) = Not_Required
      
      table_5(  1, Time_Range_Ind_T) = Defined
      table_5(  1, P1_T            ) = Not_Required
      table_5(  1, P2_T            ) = Not_Required
      table_5(  1, Num_In_Avg_T    ) = Not_Required
      
      table_5(  2, Time_Range_Ind_T) = Defined
      table_5(  2, P1_T            ) = Req_8_Bit
      table_5(  2, P2_T            ) = Req_8_Bit
      table_5(  2, Num_In_Avg_T    ) = Not_Required
      
      table_5(  3, Time_Range_Ind_T) = Defined
      table_5(  3, P1_T            ) = Req_8_Bit
      table_5(  3, P2_T            ) = Req_8_Bit
      table_5(  3, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(  4, Time_Range_Ind_T) = Defined
      table_5(  4, P1_T            ) = Req_8_Bit
      table_5(  4, P2_T            ) = Req_8_Bit
      table_5(  4, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(  5, Time_Range_Ind_T) = Defined
      table_5(  5, P1_T            ) = Req_8_Bit
      table_5(  5, P2_T            ) = Req_8_Bit
      table_5(  5, Num_In_Avg_T    ) = Not_Required
      
      table_5( 10, Time_Range_Ind_T) = Defined
      table_5( 10, P1_T            ) = Req_16_Bit
      table_5( 10, P2_T            ) = Not_Required
      table_5( 10, Num_In_Avg_T    ) = Not_Required
      
      table_5(113, Time_Range_Ind_T) = Defined
      table_5(113, P1_T            ) = Req_8_Bit
      table_5(113, P2_T            ) = Req_8_Bit
      table_5(113, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(114, Time_Range_Ind_T) = Defined
      table_5(114, P1_T            ) = Req_8_Bit
      table_5(114, P2_T            ) = Req_8_Bit
      table_5(114, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(115, Time_Range_Ind_T) = Defined
      table_5(115, P1_T            ) = Req_8_Bit
      table_5(115, P2_T            ) = Req_8_Bit
      table_5(115, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(116, Time_Range_Ind_T) = Defined
      table_5(116, P1_T            ) = Req_8_Bit
      table_5(116, P2_T            ) = Req_8_Bit
      table_5(116, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(117, Time_Range_Ind_T) = Defined
      table_5(117, P1_T            ) = Req_8_Bit
      table_5(117, P2_T            ) = Req_8_Bit
      table_5(117, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(118, Time_Range_Ind_T) = Defined
      table_5(118, P1_T            ) = Not_Required
      table_5(118, P2_T            ) = Req_8_Bit
      table_5(118, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(119, Time_Range_Ind_T) = Defined
      table_5(119, P1_T            ) = Req_8_Bit
      table_5(119, P2_T            ) = Req_8_Bit
      table_5(119, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(123, Time_Range_Ind_T) = Defined
      table_5(123, P1_T            ) = Not_Required
      table_5(123, P2_T            ) = Req_8_Bit
      table_5(123, Num_In_Avg_T    ) = Req_16_Bit
      
      table_5(124, Time_Range_Ind_T) = Defined
      table_5(124, P1_T            ) = Not_Required
      table_5(124, P2_T            ) = Req_8_Bit
      table_5(124, Num_In_Avg_T    ) = Req_16_Bit
      
      END ! SUBROUTINE Time_Range_Table
