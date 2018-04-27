MODULE valddy_mod
  INTERFACE
    LOGICAL FUNCTION VALDDY (DAY,MONTH,YEAR)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(IN) :: DAY    ! A01
    INTEGER,          INTENT(IN) :: MONTH  ! A02
    INTEGER,          INTENT(IN) :: YEAR   ! A03

    END FUNCTION VALDDY
  END INTERFACE
END MODULE valddy_mod
