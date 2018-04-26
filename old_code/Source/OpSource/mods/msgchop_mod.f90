MODULE MSGCHOP_mod
  INTERFACE
    SUBROUTINE MSGCHOP (NCHAN, NCHANS, NOBS1, NELEMS, NOBS2, NSEQ, &
                        VALIN, VALOUT, MSGBULL, LENBUL)

    IMPLICIT NONE

    ! Interface Arguments
    INTEGER, INTENT(IN)             :: NCHAN     ! (a1)  Channel for obs wan ted in output bulletin
    INTEGER, INTENT(IN)             :: NCHANS(:) ! (a2)  Channel numbers of  obs in input bulletin
    INTEGER, INTENT(IN)             :: NOBS1     ! (a3)  Number of obs in in put values array VALIN
    INTEGER, INTENT(IN)             :: NELEMS    ! (a4)  Number of values pe r ob in VALIN array
    INTEGER, INTENT(IN)             :: NOBS2     ! (a5)  Number of obs in ou tput BUFR message
    INTEGER, INTENT(IN)             :: NSEQ      ! (a6)  Number of obs in ou tput BUFR message
    REAL, INTENT(IN)                :: VALIN(:)  ! (a7)  Decoded values from  MSGWINDS bulletin
    REAL, INTENT(OUT)               :: VALOUT(:) ! (a8)  Values for encoding  new MSGWINDS bulletin
    CHARACTER (LEN=*), INTENT(OUT)  :: MSGBULL   ! (a9)  Re-encoded MSG bull etin
    INTEGER, INTENT(OUT)            :: LENBUL    ! (a10) Length of re-encoded message

    END SUBROUTINE MSGCHOP
  END INTERFACE
END MODULE MSGCHOP_mod
