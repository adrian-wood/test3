MODULE UAMAXW_mod
  INTERFACE
    SUBROUTINE UAMAXW(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY,LENG)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: OB             ! (a1) report being decoded
    INTEGER, INTENT(INOUT)         :: PTR            ! (a2) pointer within report
    REAL, INTENT(OUT)              :: ARRAY(:)       ! (a3) array of decoded values
    INTEGER, INTENT(INOUT)         :: NUM_LEV        ! (a4) number of levels
    CHARACTER (LEN=1), INTENT(IN)  :: PART           ! (a5) report type
    LOGICAL, INTENT(IN)            :: KNOTS          ! (a6) set if speed in knots
    REAL, INTENT(INOUT)            :: QCBIT_ARRAY(:) ! (a7) array of 1-bit qc f lags
    INTEGER, INTENT(IN)            :: LENG           ! (a8) Length of report

    END SUBROUTINE UAMAXW
  END INTERFACE
END MODULE UAMAXW_mod
