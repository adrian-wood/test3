MODULE INXKEY_MOD
  INTERFACE
    INTEGER FUNCTION INXKEY (REQ, IPOS, ILEN)

    IMPLICIT NONE
!                               Function arguments

    CHARACTER(*), INTENT(IN) :: REQ   ! Request string
    INTEGER,      INTENT(IN) :: IPOS  ! Current position in string
    INTEGER,      INTENT(IN) :: ILEN  ! Position of end of request

    END FUNCTION INXKEY
  END INTERFACE
END MODULE INXKEY_MOD
