MODULE buoy_mod
  INTERFACE
    SUBROUTINE BUOY(POINT,BULEND,TTAAII,CCCC,OCOR,MIMJ,NFT,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) ::        POINT   !a01
    INTEGER,          INTENT(INOUT) ::        BULEND  !a02
    CHARACTER(LEN=*), INTENT(IN)    ::        TTAAII  !a03
    CHARACTER(LEN=*), INTENT(IN)    ::        CCCC    !a04
    LOGICAL,          INTENT(IN)    ::        OCOR    !a05
    CHARACTER(LEN=*), INTENT(IN)    ::        MIMJ    !a06
    INTEGER,          INTENT(IN)    ::        NFT     !a07
    CHARACTER(LEN=*), INTENT(INOUT) ::        BULL    !a08

    END SUBROUTINE BUOY
  END INTERFACE
END MODULE buoy_mod
