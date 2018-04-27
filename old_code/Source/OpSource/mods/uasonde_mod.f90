MODULE UASONDE_mod
  INTERFACE
    SUBROUTINE UASONDE(OB,ARRAY,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: OB                 ! report being expanded
    REAL, INTENT(INOUT)            :: ARRAY(:)
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)   ! array of 1bit qc flags

    END SUBROUTINE UASONDE
  END INTERFACE
END MODULE UASONDE_mod
