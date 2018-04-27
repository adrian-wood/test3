MODULE webtadv_mod
  INTERFACE
    SUBROUTINE WEBTADV(CREP,RPRTEND,WRITEHEADER,TOR,CREP2)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
    INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
    LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
    CHARACTER(LEN=*), INTENT(IN)    :: TOR         !a04- input time of receipt
    CHARACTER(LEN=*), INTENT(INOUT) :: CREP2       !a05- string to hold line of text

    END SUBROUTINE WEBTADV
  END INTERFACE
END MODULE webtadv_mod
