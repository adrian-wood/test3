MODULE INTCON_MOD
  INTERFACE
    LOGICAL FUNCTION INTCON (REQUEST, IPOS, IPOS2)

    IMPLICIT NONE
!                              Function arguments

    CHARACTER(*), INTENT(IN) :: REQUEST  ! User's request string
    INTEGER,      INTENT(IN) :: IPOS     ! Start position in string
    INTEGER,      INTENT(IN) :: IPOS2    ! End position in string

    END FUNCTION INTCON
  END INTERFACE
END MODULE INTCON_MOD
