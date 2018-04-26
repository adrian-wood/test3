MODULE getchr_mod
  INTERFACE
    FUNCTION GETCHR(RDISP,CD)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,             INTENT(IN)  :: RDISP
    CHARACTER(LEN=*), INTENT(IN)  :: CD

    CHARACTER(LEN=50) ::  GETCHR

    END FUNCTION GETCHR
  END INTERFACE
END MODULE getchr_mod
