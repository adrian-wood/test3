MODULE oukstn_mod
  INTERFACE
    FUNCTION OUKSTN(STNNUM)
    IMPLICIT NONE
    INTEGER,INTENT(IN) :: STNNUM  ! Station Number input to the function
    LOGICAL            :: OUKSTN
    END FUNCTION OUKSTN
  END INTERFACE
END MODULE oukstn_mod
