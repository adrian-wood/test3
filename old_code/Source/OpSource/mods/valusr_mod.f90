MODULE valusr_mod
  INTERFACE
    SUBROUTINE VALUSR(MESSAGE,NELEM,DISPL,WIDTH,SCALE,REFVAL, &
                      ARRAY,NOB,NOBS,CSTR,CREP,CREPRT,LFLAG,ISECT1, &
                      CHRELM,LENCHR)                                !F

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(OUT)   ::  MESSAGE
    INTEGER,      INTENT(INOUT) ::  NELEM                         !2.0
    INTEGER,      INTENT(INOUT) ::  DISPL(:)
    INTEGER,      INTENT(INOUT) ::  WIDTH(:)
    INTEGER,      INTENT(INOUT) ::  SCALE(:)
    INTEGER,      INTENT(INOUT) ::  REFVAL(:)
    INTEGER,      INTENT(INOUT) ::  NOBS                          !2.0
    REAL,         INTENT(OUT)   ::  ARRAY(NOBS,NELEM)
    INTEGER,      INTENT(IN)    ::  NOB                           !2.0
    CHARACTER(*), INTENT(OUT)   ::  CSTR(NOBS)
    CHARACTER(*), INTENT(IN)    ::  CREP
    CHARACTER(*), INTENT(OUT)   ::  CREPRT(NOBS)
    LOGICAL,      INTENT(IN)    ::  LFLAG
    INTEGER,      INTENT(INOUT) ::  ISECT1(:) ! integer elements array !B
    CHARACTER(*), INTENT(INOUT) ::  CHRELM(:) !- names to put in users string. !F
    INTEGER,      INTENT(INOUT) ::  LENCHR(:) ! lengths of CHRELM elements !F

    END SUBROUTINE VALUSR
  END INTERFACE
END MODULE valusr_mod
