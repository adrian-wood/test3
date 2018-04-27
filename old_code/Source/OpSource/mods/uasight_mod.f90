MODULE UASIGHT_mod
  INTERFACE
    SUBROUTINE UASIGHT(STRING,PTR,ARRAY,NUM_LEV,PART,STNHT,KNOTS,  &
                       SIG_BASE,QCBIT_ARRAY,LENG)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: STRING    ! report string being expanded
    INTEGER, INTENT(INOUT)         :: PTR       ! pointer within report
    REAL, INTENT(OUT)              :: ARRAY(:)  ! expanded elements array
    INTEGER, INTENT(INOUT)         :: NUM_LEV
    CHARACTER, INTENT(IN)          :: PART      ! report type
    INTEGER, INTENT(IN)            :: STNHT     ! height of station
    LOGICAL,INTENT(IN)             :: KNOTS     ! indicates wind speed in knots
    INTEGER, INTENT(IN)            :: SIG_BASE
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! qc 1 bit flag array
    INTEGER, INTENT(IN)            :: LENG      ! Length of report

    END SUBROUTINE UASIGHT
  END INTERFACE
END MODULE UASIGHT_mod
