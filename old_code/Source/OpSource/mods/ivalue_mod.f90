MODULE IVALUE_MOD
  INTERFACE
    INTEGER FUNCTION IVALUE (STRING)

    IMPLICIT NONE
!                               Function argument

    CHARACTER(*), INTENT(IN) :: STRING  ! String to be converted

    END FUNCTION IVALUE
  END INTERFACE
END MODULE IVALUE_MOD
