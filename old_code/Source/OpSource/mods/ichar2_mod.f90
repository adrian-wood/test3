MODULE ICHAR2_MOD
  INTERFACE
    INTEGER FUNCTION ICHAR2 (C2)

    IMPLICIT NONE
!                               Function argument

    CHARACTER(2), INTENT(IN) :: C2 ! 2 characters to convert to integer

    END FUNCTION ICHAR2
  END INTERFACE
END MODULE ICHAR2_MOD
