MODULE amdbuc_mod
  INTERFACE
    SUBROUTINE AMDBUC(BULL,NREP,IPOINT,ILEN,BDAY,BHOUR,RC)

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(INOUT) :: BULL         !a1
    INTEGER,          INTENT(OUT)   :: NREP         !a2
    INTEGER,          INTENT(OUT)   :: IPOINT(:)    !a3
    INTEGER,          INTENT(OUT)   :: ILEN(:)      !a4
    INTEGER,          INTENT(OUT)   :: BDAY         !a5
    INTEGER,          INTENT(OUT)   :: BHOUR        !a6
    INTEGER,          INTENT(OUT)   :: RC           !a7

    END SUBROUTINE AMDBUC
  END INTERFACE
END MODULE amdbuc_mod
