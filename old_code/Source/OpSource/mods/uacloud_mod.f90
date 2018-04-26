MODULE UACLOUD_mod
  INTERFACE
    SUBROUTINE UACLOUD(OB,ARRAY,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: OB                ! report being expanded
    REAL, INTENT(INOUT)            :: ARRAY(:)          ! array of decoded values
    REAL, INTENT(OUT)              :: QCBIT_ARRAY(999)  ! array of qc bits

    END SUBROUTINE UACLOUD
  END INTERFACE
END MODULE UACLOUD_mod
