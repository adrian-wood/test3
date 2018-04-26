MODULE UASIGPR_mod
  INTERFACE
    SUBROUTINE UASIGPR(STRING,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS, &
                       BADSEQ,SIG_BASE,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: STRING    ! (a1) report
    INTEGER, INTENT(INOUT)         :: PTR       ! (a2) pointer within report
    REAL, INTENT(OUT)              :: ARRAY(:)  ! (a3) expanded elements array
    INTEGER, INTENT(INOUT)         :: NUM_LEV   ! (a4) number of levels
    CHARACTER (LEN=1), INTENT(IN)  :: PART      ! (a5) report part
    LOGICAL, INTENT(IN)            :: WINDS     ! (a6) indicates wind or temp decode
    LOGICAL, INTENT(IN)            :: KNOTS     ! (a7) indicates speed in knots
    LOGICAL, INTENT(OUT)           :: BADSEQ    ! (a8) invalid pressure trend
    INTEGER, INTENT(IN)            :: SIG_BASE  ! (a9) array displacement base sig report
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! (a10) qcbit array

    END SUBROUTINE UASIGPR
  END INTERFACE
END MODULE UASIGPR_mod
