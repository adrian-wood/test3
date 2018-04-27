MODULE ICHAR3_MOD
  INTERFACE
    INTEGER FUNCTION ICHAR3 (C3)

    IMPLICIT NONE
!                               Function argument

    CHARACTER(3), INTENT(IN) :: C3 ! 3 characters to convert to integer

    END FUNCTION ICHAR3
  END INTERFACE
END MODULE ICHAR3_MOD
