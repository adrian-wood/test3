MODULE ARGDCODE_mod
  INTERFACE
    SUBROUTINE ARGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS, &
                         IDESCR, MAXDES, NDES, NAMES, NCHOP)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: MSGBULL   ! (a1) MSG bulletin to be decoded
    INTEGER, INTENT(IN)            :: MAXOBS    ! (a2) Size of user's NCHANS array
    INTEGER, INTENT(IN)            :: MAXVALS   ! (a3) Size of user's VALUES array
    REAL, INTENT(OUT)              :: VALUES(:) ! (a4) Array of decoded values from MSGBULL
    INTEGER, INTENT(OUT)           :: NOBS      ! (a5) Number of observations in BUFR message
    INTEGER, INTENT(IN)            :: MAXDES    ! (a7) Maximum number of BUFR descriptors
    INTEGER, INTENT(OUT)           :: IDESCR(MAXDES) ! (a6) BUFR descriptor sequence for message
    INTEGER, INTENT(OUT)           :: NDES      ! (a8) Number of descriptors in BUFR sequence
    CHARACTER (LEN=*), INTENT(OUT) :: NAMES     ! (a9) Array for decoded characters
    INTEGER, INTENT(OUT)           :: NCHOP     ! (a10) Number of bulletins to create for storage

    END SUBROUTINE ARGDCODE
  END INTERFACE
END MODULE ARGDCODE_mod
