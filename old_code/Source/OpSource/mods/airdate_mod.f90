MODULE airdate_mod
  INTERFACE
    SUBROUTINE AIRDATE(BHOUR,BDAY,BMONTH,BYEAR)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN)  :: BHOUR     !a01
    INTEGER,          INTENT(IN)  :: BDAY      !a02
    INTEGER,          INTENT(OUT) :: BMONTH    !a03
    INTEGER,          INTENT(OUT) :: BYEAR     !a04

    END SUBROUTINE AIRDATE
  END INTERFACE
END MODULE airdate_mod
