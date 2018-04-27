MODULE airice_mod
  INTERFACE
    SUBROUTINE AIRICE(REPORT,POINT,OPT_ICE)
    IMPLICIT NONE

    ! Arguments

    CHARACTER(LEN=*),INTENT(IN) :: REPORT    !airep report
    INTEGER,INTENT(INOUT)       :: POINT     !Position within airep report
    REAL,INTENT(OUT)            :: OPT_ICE   !Decoded value for icing
    END SUBROUTINE AIRICE
  END INTERFACE
END MODULE airice_mod
