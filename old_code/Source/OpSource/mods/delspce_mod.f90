MODULE DELSPCE_MOD
  INTERFACE
    SUBROUTINE DELSPCE (CREQ, LENGTH, REQUEST)

    IMPLICIT NONE

    CHARACTER(LEN=*), INTENT(INOUT)  :: CREQ     ! MODIFIED COPY OF USER
    INTEGER, INTENT(INOUT)           :: LENGTH   ! LENGTH OF MODIFIED RE
    CHARACTER(LEN=*), INTENT(INOUT)  :: REQUEST  ! USER'S ORIGINAL REQUE

    END SUBROUTINE DELSPCE
  END INTERFACE
END MODULE DELSPCE_MOD