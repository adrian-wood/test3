MODULE ersind_mod
  INTERFACE
    SUBROUTINE ERSIND (A, ND, NOBS, DESCR, BULL, IFT)
      IMPLICIT NONE
      INTEGER, INTENT(IN)                     :: NOBS       ! Number of Observations in message,  a3
      INTEGER, INTENT(IN)                     :: ND         ! Number of Descriptors,              a2
      REAL, INTENT(IN)                        :: A(NOBS,*)  ! ARRAY OF DECODED COORDINATE VALUES, a1
      INTEGER, INTENT(INOUT)                  :: DESCR(*)   ! SEQUENCE OF DESCRIPTORS,            a4
      CHARACTER(LEN=*), INTENT(INOUT)         :: BULL       ! BUFR MESSAGE WITH DESCRIPTORS IN,   a5
      INTEGER, INTENT(IN)                     :: IFT        ! FT NUMBER (FOR ERSREP),             a6
    END SUBROUTINE ERSIND
  END INTERFACE
END MODULE ersind_mod
