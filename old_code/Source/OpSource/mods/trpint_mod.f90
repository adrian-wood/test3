MODULE trpint_mod
  INTERFACE
    SUBROUTINE TRPINT(VALUES,CNAM,YEAR,MONTH,DAY,HOUR,MINT, &
                      BULCOD,II,STNID)

    IMPLICIT NONE

! Subroutine arguments:

    REAL,         INTENT(INOUT) ::  VALUES(:) ! Expanded data values
    CHARACTER(*), INTENT(OUT)   ::  CNAM ! String to hold text element d    ata
    INTEGER,      INTENT(IN)    ::  YEAR ! Year of observation
    INTEGER,      INTENT(IN)    ::  MONTH ! Month of observation
    INTEGER,      INTENT(IN)    ::  DAY ! Day of observation
    INTEGER,      INTENT(IN)    ::  HOUR ! Hour of observation
    INTEGER,      INTENT(IN)    ::  MINT
    CHARACTER(*), INTENT(IN)    ::  BULCOD ! Bulletin identifier
    CHARACTER(*), INTENT(IN)    ::  II ! II part of TTAAII
    CHARACTER(*), INTENT(IN)    ::  STNID ! Station identifier

    END SUBROUTINE TRPINT
  END INTERFACE
END MODULE trpint_mod
