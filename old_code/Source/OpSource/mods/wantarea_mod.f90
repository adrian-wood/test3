MODULE wantarea_mod
  INTERFACE
    SUBROUTINE WANTAREA (ENTRY, NVER, AREA, RPOLE, KEEP)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)        ::  ENTRY ! Index entry
    INTEGER, INTENT(IN)             ::  NVER ! Index entry version number (1 or 2)
    REAL, INTENT(IN)                ::  AREA(5) ! User's area (Flag, N, W, S, E)
    REAL, INTENT(IN)                ::  RPOLE(2) ! Latitude and longitude of rotated pole
    INTEGER, INTENT(OUT)            ::  KEEP ! 'Keep' flag from area checking

    END SUBROUTINE WANTAREA
  END INTERFACE
END MODULE wantarea_mod
