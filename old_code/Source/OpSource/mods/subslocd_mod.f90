MODULE subslocd_mod
  INTERFACE
    SUBROUTINE SUBSLOCD(SUBTYPE,BULL,SEQDES)                        !A

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  SUBTYPE !- MetDB subtype
    CHARACTER(*), INTENT(IN)    ::  BULL !- input BUFR message
    INTEGER,      INTENT(OUT)   ::  SEQDES !- input sequence descriptor

    END SUBROUTINE SUBSLOCD
  END INTERFACE
END MODULE subslocd_mod
