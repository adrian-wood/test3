MODULE sfrexp_mod
  INTERFACE
    SUBROUTINE SFREXP(HEX,ARRAY,NELEM)
    IMPLICIT NONE

! Arguments

    CHARACTER(LEN=*),INTENT(IN)    :: HEX         !(1)
    REAL,            INTENT(INOUT) :: ARRAY(:)    !(2)
    INTEGER,         INTENT(OUT)   :: NELEM       !(3)
    END SUBROUTINE SFREXP
  END INTERFACE
END MODULE sfrexp_mod
