!+    Decodes a description of a level in the atmosphere from GRIB
! $Header: level_desc_dec.f, 1, 27/02/2007 16:11:06, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Level_Desc_Dec(level_bytes, level_type,
     &  level_desc_1, level_desc_2)

      IMPLICIT NONE

!     Description:
!       Decodes a description of a level in the atmosphere from
!       GRIB format.
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
!     Array arguments with INTENT(IN):

      INTEGER    level_bytes(3)      ! Encoded level description

!     Scalar arguments with INTENT(OUT):

      INTEGER    level_type        ! Level type number, as defined in
                                   ! GRIB code table 3

!     Height, pressure, etc. of level, if level type requires a single
!     descriptive parameter. First height, pressure, etc. if level
!     requires two descriptive parameters.

      INTEGER    level_desc_1

!     Second height, pressure, etc. if level requires two descriptive
!     parameters.

      INTEGER    level_desc_2

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

!     Local Arrays:

!     Type of level encoding for each level type

      INTEGER     table_3(Min_Level_Type : Max_Level_Type, 3)

!     Functions and subroutines used:

      EXTERNAL   Level_Desc_Table

!-    End of header

!     Set type of level encoding for each level type

      CALL Level_Desc_Table(table_3)

!     First byte is level type

      level_type = level_bytes(1)
      
      IF (table_3(level_type, Level_Desc_1_T) .EQ. Req_16_Bit ) THEN

!       This type of level is described by a single 16-bit parameter
!       (height, pressure, etc.) Treat bytes 2 and 3 as a single
!       number

        level_desc_1 = 256 * level_bytes(2) + level_bytes(3)
        level_desc_2 = 0
      ELSE

!       In all other cases, treat the two bytes as separate values

        level_desc_1 = level_bytes(2)
        level_desc_2 = level_bytes(3)
      END IF
      
      END ! SUBROUTINE Level_Desc_Dec
