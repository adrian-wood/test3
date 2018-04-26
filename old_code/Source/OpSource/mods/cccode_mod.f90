MODULE cccode_mod
  INTERFACE
    SUBROUTINE CCCODE(DESCR,ICCCC,CCCC)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)   ::   DESCR   ! argument (1)
    INTEGER,          INTENT(OUT)  ::   ICCCC   ! argument (2)
    CHARACTER(LEN=4), INTENT(IN)   ::   CCCC    ! argument (3)

    END SUBROUTINE CCCODE
  END INTERFACE
END MODULE cccode_mod
