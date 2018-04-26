MODULE ncmind_mod
  INTERFACE
    SUBROUTINE NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII, &
                      HOUR,MIN,LAT,LON)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=23), INTENT(OUT) ::   ENTRY  !A01
    LOGICAL,           INTENT(IN)  ::   OCOR   !A02
    CHARACTER(LEN=2),  INTENT(IN)  ::   CORNUM !A03
    CHARACTER(LEN=4),  INTENT(IN)  ::   CCCC   !A04
    CHARACTER(LEN=6),  INTENT(IN)  ::   TTAAII !A05
    INTEGER,           INTENT(IN)  ::   HOUR   !A06
    INTEGER,           INTENT(IN)  ::   MIN    !A07
    REAL,              INTENT(IN)  ::   LAT    !A08
    REAL,              INTENT(IN)  ::   LON    !A09

    END SUBROUTINE NCMIND
  END INTERFACE
END MODULE ncmind_mod
