MODULE UATROP_mod
  INTERFACE
    SUBROUTINE UATROP(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: OB       ! (a1) report being expanded
    INTEGER, INTENT(INOUT)         :: PTR      ! (a2) pointer within report
    REAL, INTENT(OUT)              :: ARRAY(:) ! (a3) array of decoded v    alues
    INTEGER, INTENT(INOUT)         :: NUM_LEV  ! (a4) number of levels
    CHARACTER (LEN=1), INTENT(IN)  :: PART     ! (a5) message part (A or C)
    LOGICAL, INTENT(IN)            :: KNOTS    ! (a6) indicates wind speed knots
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)   ! (a7) array of 1 bit QC flags

    END SUBROUTINE UATROP
  END INTERFACE
END MODULE UATROP_mod
