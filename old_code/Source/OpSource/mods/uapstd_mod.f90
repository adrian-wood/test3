MODULE UAPSTD_mod
  INTERFACE
    SUBROUTINE UAPSTD(OB,PTR,ARRAY,NUM_LEV,PART,BLOCK,STN,KNOTS,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: OB       ! report being decoded
    INTEGER, INTENT(INOUT)         :: PTR      ! pointer within report
    REAL, INTENT(OUT)              :: ARRAY(:) ! array of decoded elements
    INTEGER, INTENT(INOUT)         :: NUM_LEV
    CHARACTER (LEN=1), INTENT(IN)  :: PART     ! report part
    INTEGER, INTENT(IN)            :: BLOCK    ! WMO block no.
    INTEGER, INTENT(IN)            :: STN      ! WMO station number
    LOGICAL, INTENT(IN)            :: KNOTS    ! indicates wind speed in knots if true
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! array of 1 bit qc flags

    END SUBROUTINE UAPSTD
  END INTERFACE
END MODULE UAPSTD_mod
