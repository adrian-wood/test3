!+    Encodes a description of a level in the atmosphere in GRIB
! $Header: level_desc_enc.f, 1, 27/02/2007 16:11:07, Stan Kellett$
!
!     *********************** COPYRIGHT *************************
!     Crown Copyright 1997, The Met. Office. All rights reserved.
!     *********************** COPYRIGHT *************************
!
!     Subroutine Interface:

      SUBROUTINE Level_Desc_Enc(level_type, level_desc_1,
     &  level_desc_2, msglvl, err_unit, level_bytes, error_status)

      IMPLICIT NONE

!     Description:
!       Encodes a description of a level in the atmosphere in GRIB
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

      INTEGER    level_type        ! Level type number, as defined in
                                   ! GRIB code table 3

!     Height, pressure, etc. of level, if level type requires a single
!     descriptive parameter. First height, pressure, etc. if level
!     requires two descriptive parameters.

      INTEGER    level_desc_1

!     Second height, pressure, etc. if level requires two descriptive
!     parameters.

      INTEGER    level_desc_2

      INTEGER    msglvl              ! Level of diagnostics required
      INTEGER    err_unit            ! Unit number for error messages

!     Array arguments with INTENT(OUT):

      INTEGER    level_bytes(3)      ! Encoded level description

!     Error status:

      INTEGER    error_status        ! +ve => warning or fatal error

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
      INTEGER    Min_16_Bit             ! Valid range for a 16-bit
      PARAMETER (Min_16_Bit = 0)        ! parameter
      INTEGER    Max_16_Bit
      PARAMETER (Max_16_Bit = 2**16 - 1)
      INTEGER    Min_8_Bit              ! Valid range for an 8-bit
      PARAMETER (Min_8_Bit = 0)         ! parameter
      INTEGER    Max_8_Bit
      PARAMETER (Max_8_Bit = 2**8 - 1)

      INTEGER    No_Error               ! Levels of error
      PARAMETER (No_Error = 0)
      INTEGER    Error
      PARAMETER (Error = 3)

!     Local Scalars:

      LOGICAL     ok                 ! Have all values have been in
                                     ! valid so far?

!     Local Arrays:

!     Type of level encoding for each level type

      INTEGER     table_3(Min_Level_Type : Max_Level_Type, 3)

!     Functions and subroutines used:

      EXTERNAL   Level_Desc_Table
      EXTERNAL   Range_Check

!-    End of header

!     Set type of level encoding for each level type

      CALL Level_Desc_Table(table_3)

      error_status = No_Error

!     Check that level type is in range

      CALL Range_Check(level_type, Min_Level_Type, Max_Level_Type,
     &  'level type', msglvl, err_unit, ok)

      IF ( ok ) THEN

!       Check that level type has a defined meaning in GRIB code
!       table 3

        IF ( table_3(level_type, Level_Type_T) .EQ. Defined ) THEN
          level_bytes(1) = level_type
        ELSE
          ok = .FALSE.
          IF ( msglvl .LT. Error ) THEN
            WRITE ( UNIT = err_unit, FMT = '(A, I8, A)' )
     &        ' ERROR - specified level type, ', level_type,
     &        ', is undefined'
          END IF
        END IF
      END IF

      IF ( ok ) THEN

!       Set default values which will be encoded if no height,
!       pressure, etc. is required to describe the level. Standard
!       specifies that the values must be zero in these cases.

        level_bytes(2) = 0
        level_bytes(3) = 0

        IF ( table_3(level_type, Level_Desc_1_T) .EQ. Req_8_Bit ) THEN

!         First level description parameter is required and is an
!         8 bit quantity

          CALL Range_Check(level_desc_1, Min_8_Bit, Max_8_Bit,
     &      'first level description parameter', msglvl, err_unit,
     &      ok)

          IF ( ok ) THEN
            level_bytes(2) = level_desc_1
          END IF
        END IF

        IF ( table_3(level_type, Level_Desc_1_T) .EQ. Req_16_Bit ) THEN

!         First level description parameter is required and is a
!         16 bit quantity

          CALL Range_Check(level_desc_1, Min_16_Bit, Max_16_Bit,
     &      'level description', msglvl, err_unit, ok)

          IF ( ok ) THEN
            level_bytes(2) = level_desc_1 / 256
            level_bytes(3) = Mod(level_desc_1, 256)
          END IF
        END IF
        
      END IF

      IF ( ok ) THEN
        IF ( table_3(level_type, Level_Desc_2_T) .EQ. Req_8_Bit ) THEN

!         Second level description parameter is required and is an
!         8 bit quantity

          CALL Range_Check(level_desc_2, Min_8_Bit, Max_8_Bit,
     &      'second level description parameter', msglvl, err_unit,
     &      ok)

          IF ( ok ) THEN
            level_bytes(3) = level_desc_2
          END IF
        END IF
      END IF

!     Signal an error if any of the parameters were out of range

      IF ( .NOT. ok ) THEN
        error_status = Error
      END IF

      END ! SUBROUTINE Level_Desc_Enc
