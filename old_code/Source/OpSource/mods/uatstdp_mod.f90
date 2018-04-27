MODULE UATSTDP_mod
  INTERFACE
    SUBROUTINE UATSTDP(STRING,PTR,ARRAY,NUM_LEV,PART,ID,KNOTS, &
                       BASE,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: STRING   ! (a1) part of report being expanded
    INTEGER, INTENT(INOUT)         :: PTR      ! (a2) pointer within report
    REAL, INTENT(INOUT)            :: ARRAY(:) ! (a3) array of decoded values
    INTEGER, INTENT(INOUT)         :: NUM_LEV  ! (a4) number of standard levels
    CHARACTER (LEN=1), INTENT(IN)  :: PART     ! (a5) message part (A,C or B)
    INTEGER, INTENT(INOUT)         :: ID       ! (a6) wind top level indicator
    LOGICAL, INTENT(IN)            :: KNOTS    ! (a7) indicates speed in knots
    INTEGER, INTENT(INOUT)         :: BASE     ! (a8) base displacement in array
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! (a9) array of 1bit QC flags

    END SUBROUTINE UATSTDP
  END INTERFACE
END MODULE UATSTDP_mod
