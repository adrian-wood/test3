!+    Initialise an array representing GRIB code table 3
! $Header: level_desc_table.f, 1, 27/02/2007 16:11:07, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Level_Desc_Table(table_3)

      IMPLICIT NONE

!     Description:
!       Initialises an array representing GRIB code table 3
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

      INTEGER    Level_Type_T          ! Pieces of information held
      PARAMETER (Level_Type_T = 1)     ! for each level type
      INTEGER    Level_Desc_1_T
      PARAMETER (Level_Desc_1_T = 2)
      INTEGER    Level_Desc_2_T
      PARAMETER (Level_Desc_2_T = 3)

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

      INTEGER    Min_Level_Type         ! Valid range of level types
      PARAMETER (Min_Level_Type = 0)
      INTEGER    Max_Level_Type
      PARAMETER (Max_Level_Type = 2**8 - 1)

!     Array arguments with INTENT(OUT):

      INTEGER    table_3(Min_Level_Type : Max_Level_Type, 3)

!     Local Scalars:

      INTEGER     table_entry        ! Current entry in table_3

!-    End of header

      DO table_entry = Min_Level_Type, Max_Level_Type
        table_3(table_entry, Level_Type_T) = Reserved
      END DO

      table_3(  1, Level_Type_T  ) = Defined
      table_3(  1, Level_Desc_1_T) = Not_Required
      table_3(  1, Level_Desc_2_T) = Not_Required

      table_3(  2, Level_Type_T  ) = Defined
      table_3(  2, Level_Desc_1_T) = Not_Required
      table_3(  2, Level_Desc_2_T) = Not_Required

      table_3(  3, Level_Type_T  ) = Defined
      table_3(  3, Level_Desc_1_T) = Not_Required
      table_3(  3, Level_Desc_2_T) = Not_Required

      table_3(  4, Level_Type_T  ) = Defined
      table_3(  4, Level_Desc_1_T) = Not_Required
      table_3(  4, Level_Desc_2_T) = Not_Required

      table_3(  5, Level_Type_T  ) = Defined
      table_3(  5, Level_Desc_1_T) = Not_Required
      table_3(  5, Level_Desc_2_T) = Not_Required

      table_3(  6, Level_Type_T  ) = Defined
      table_3(  6, Level_Desc_1_T) = Not_Required
      table_3(  6, Level_Desc_2_T) = Not_Required

      table_3(  7, Level_Type_T  ) = Defined
      table_3(  7, Level_Desc_1_T) = Not_Required
      table_3(  7, Level_Desc_2_T) = Not_Required

      table_3(  8, Level_Type_T  ) = Defined
      table_3(  8, Level_Desc_1_T) = Not_Required
      table_3(  8, Level_Desc_2_T) = Not_Required

      table_3(  9, Level_Type_T  ) = Defined
      table_3(  9, Level_Desc_1_T) = Not_Required
      table_3(  9, Level_Desc_2_T) = Not_Required
      
      table_3( 20, Level_Type_T  ) = Defined
      table_3( 20, Level_Desc_1_T) = Req_16_Bit
      table_3( 20, Level_Desc_2_T) = Not_Required

      table_3(100, Level_Type_T  ) = Defined
      table_3(100, Level_Desc_1_T) = Req_16_Bit
      table_3(100, Level_Desc_2_T) = Not_Required

      table_3(101, Level_Type_T  ) = Defined
      table_3(101, Level_Desc_1_T) = Req_8_Bit
      table_3(101, Level_Desc_2_T) = Req_8_Bit

      table_3(102, Level_Type_T  ) = Defined
      table_3(102, Level_Desc_1_T) = Not_Required
      table_3(102, Level_Desc_2_T) = Not_Required

      table_3(103, Level_Type_T  ) = Defined
      table_3(103, Level_Desc_1_T) = Req_16_Bit
      table_3(103, Level_Desc_2_T) = Not_Required

      table_3(104, Level_Type_T  ) = Defined
      table_3(104, Level_Desc_1_T) = Req_8_Bit
      table_3(104, Level_Desc_2_T) = Req_8_Bit

      table_3(105, Level_Type_T  ) = Defined
      table_3(105, Level_Desc_1_T) = Req_16_Bit
      table_3(105, Level_Desc_2_T) = Not_Required

      table_3(106, Level_Type_T  ) = Defined
      table_3(106, Level_Desc_1_T) = Req_8_Bit
      table_3(106, Level_Desc_2_T) = Req_8_Bit

      table_3(107, Level_Type_T  ) = Defined
      table_3(107, Level_Desc_1_T) = Req_16_Bit
      table_3(107, Level_Desc_2_T) = Not_Required

      table_3(108, Level_Type_T  ) = Defined
      table_3(108, Level_Desc_1_T) = Req_8_Bit
      table_3(108, Level_Desc_2_T) = Req_8_Bit

      table_3(109, Level_Type_T  ) = Defined
      table_3(109, Level_Desc_1_T) = Req_16_Bit
      table_3(109, Level_Desc_2_T) = Not_Required

      table_3(110, Level_Type_T  ) = Defined
      table_3(110, Level_Desc_1_T) = Req_8_Bit
      table_3(110, Level_Desc_2_T) = Req_8_Bit

      table_3(111, Level_Type_T  ) = Defined
      table_3(111, Level_Desc_1_T) = Req_16_Bit
      table_3(111, Level_Desc_2_T) = Not_Required

      table_3(112, Level_Type_T  ) = Defined
      table_3(112, Level_Desc_1_T) = Req_8_Bit
      table_3(112, Level_Desc_2_T) = Req_8_Bit

      table_3(113, Level_Type_T  ) = Defined
      table_3(113, Level_Desc_1_T) = Req_16_Bit
      table_3(113, Level_Desc_2_T) = Not_Required

      table_3(114, Level_Type_T  ) = Defined
      table_3(114, Level_Desc_1_T) = Req_8_Bit
      table_3(114, Level_Desc_2_T) = Req_8_Bit

      table_3(115, Level_Type_T  ) = Defined
      table_3(115, Level_Desc_1_T) = Req_16_Bit
      table_3(115, Level_Desc_2_T) = Not_Required

      table_3(116, Level_Type_T  ) = Defined
      table_3(116, Level_Desc_1_T) = Req_8_Bit
      table_3(116, Level_Desc_2_T) = Req_8_Bit

      table_3(117, Level_Type_T  ) = Defined
      table_3(117, Level_Desc_1_T) = Req_16_Bit
      table_3(117, Level_Desc_2_T) = Not_Required
      
      table_3(119, Level_Type_T  ) = Defined
      table_3(119, Level_Desc_1_T) = Req_16_Bit
      table_3(119, Level_Desc_2_T) = Not_Required

      table_3(120, Level_Type_T  ) = Defined
      table_3(120, Level_Desc_1_T) = Req_8_Bit
      table_3(120, Level_Desc_2_T) = Req_8_Bit

      table_3(121, Level_Type_T  ) = Defined
      table_3(121, Level_Desc_1_T) = Req_8_Bit
      table_3(121, Level_Desc_2_T) = Req_8_Bit

      table_3(125, Level_Type_T  ) = Defined
      table_3(125, Level_Desc_1_T) = Req_16_Bit
      table_3(125, Level_Desc_2_T) = Not_Required

      table_3(128, Level_Type_T  ) = Defined
      table_3(128, Level_Desc_1_T) = Req_8_Bit
      table_3(128, Level_Desc_2_T) = Req_8_Bit

      table_3(141, Level_Type_T  ) = Defined
      table_3(141, Level_Desc_1_T) = Req_8_Bit
      table_3(141, Level_Desc_2_T) = Req_8_Bit

      table_3(160, Level_Type_T  ) = Defined
      table_3(160, Level_Desc_1_T) = Req_16_Bit
      table_3(160, Level_Desc_2_T) = Not_Required

      table_3(200, Level_Type_T  ) = Defined
      table_3(200, Level_Desc_1_T) = Not_Required
      table_3(200, Level_Desc_2_T) = Not_Required

      table_3(201, Level_Type_T  ) = Defined
      table_3(201, Level_Desc_1_T) = Not_Required
      table_3(201, Level_Desc_2_T) = Not_Required
      
      table_3(205, Level_Type_T  ) = Defined             !6.1
      table_3(205, Level_Desc_1_T) = Req_16_Bit          !6.1
      table_3(205, Level_Desc_2_T) = Not_Required        !6.1
      
      table_3(210, Level_Type_T  ) = Defined             !6.2
      table_3(210, Level_Desc_1_T) = Req_16_Bit          !6.2
      table_3(210, Level_Desc_2_T) = Not_Required        !6.2
      
      END ! SUBROUTINE Level_Desc_Table
