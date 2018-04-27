MODULE UASORT_mod
  INTERFACE
    SUBROUTINE UASORT(ARRAY,QCBIT_ARRAY,VALUES,STANDRD)

    IMPLICIT NONE

    REAL, INTENT(IN)     :: ARRAY(:)           ! (a1) array of decoded values
    REAL, INTENT(IN)     :: QCBIT_ARRAY(:)     ! (a2) array of qc bits
    REAL, INTENT(INOUT)  :: VALUES(:)          ! (a3)
    LOGICAL, INTENT(IN)  :: STANDRD            ! (a4) true if standard levels

    END SUBROUTINE UASORT
  END INTERFACE
END MODULE UASORT_mod
