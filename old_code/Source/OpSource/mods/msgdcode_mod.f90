MODULE MSGDCODE_mod
  INTERFACE
    SUBROUTINE MSGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS, &
                         NELEMS, NHIST, NCHANS, NCHOP)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)  :: MSGBULL   ! (a1) MSG bulletin to be decoded
    INTEGER, INTENT(IN)            :: MAXOBS    ! (a2) Size of user's NCHANS array
    INTEGER, INTENT(IN)            :: MAXVALS   ! (a3) Size of user's VALUES array
    REAL, INTENT(OUT)              :: VALUES(:) ! (a4) Array of decoded values from MSGBULL
    INTEGER, INTENT(OUT)           :: NOBS      ! (a5) Number of observations in BUFR message
    INTEGER, INTENT(OUT)           :: NELEMS    ! (a6) Number of decoded elements per observation
    INTEGER, INTENT(OUT)           :: NHIST(12) ! (a7) Numbers of obs present for 12 channels
    INTEGER, INTENT(OUT)           :: NCHANS(:) ! (a8) Channel numbers for obs in bulletin
    INTEGER, INTENT(OUT)           :: NCHOP     ! (a9) Number of bulletins to create for storage

    END SUBROUTINE MSGDCODE
  END INTERFACE
END MODULE MSGDCODE_mod
