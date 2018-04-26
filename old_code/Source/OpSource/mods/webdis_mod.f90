MODULE webdis_mod
  INTERFACE
    SUBROUTINE WEBDIS(CTYPE,CREQ,IMaxObs,TotalObs)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CTYPE    !a01 input subtype
    CHARACTER(LEN=*), INTENT(IN)    :: CREQ     !a02 input request string
    INTEGER,          INTENT(IN)    :: IMaxObs  !a03 max no. of obs to retrieve
    INTEGER,          INTENT(OUT)   :: TotalObs !a04 total no. of obs retrieved

    END SUBROUTINE WEBDIS
  END INTERFACE
END MODULE webdis_mod
