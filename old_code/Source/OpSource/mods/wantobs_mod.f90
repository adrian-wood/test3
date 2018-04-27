MODULE wantobs_mod
  INTERFACE
    SUBROUTINE WANTOBS (VALUES, CSTR, NOBS, LDISP, KEEP, NTIME, AREA, &
                        RPOLE, CHID, NPLATS, WANTOB, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER, INTENT(IN)             ::  NOBS ! Number of observations in message
    REAL, INTENT(IN)                ::  VALUES(NOBS,*) ! Data values from BUFR decode
    CHARACTER(LEN=*), INTENT(IN)    ::  CSTR ! Character strings fron BUFR decode
    INTEGER, INTENT(IN)             ::  LDISP(*) ! Displacements for selected data items
    INTEGER, INTENT(IN)             ::  KEEP(3) ! 'Keep' flags for time, area and identifier
    INTEGER, INTENT(IN)             ::  NTIME(8) ! User's start and end times (y,m,d,hhmm)*2
    REAL, INTENT(IN)                ::  AREA(5) ! User's area (Flag, N, W, S, E)
    REAL, INTENT(IN)                ::  RPOLE(2) ! Latitude and longitude of rotated pole
    INTEGER, INTENT(IN)             ::  NPLATS ! Number of platforms specified by user
    CHARACTER(*), INTENT(IN)        ::  CHID(NPLATS)  ! User's requested platform identifiers
    LOGICAL(kind=1), INTENT(OUT)    ::  WANTOB(*) ! .TRUE. if obs are wanted
    INTEGER, INTENT(OUT)            ::  ICODE ! Integer return code (see above for values)

    END SUBROUTINE WANTOBS
  END INTERFACE
END MODULE wantobs_mod
