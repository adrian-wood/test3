MODULE tafsect_mod
  INTERFACE
    SUBROUTINE TAFSECT(OB,IN,REXP,SECTION,RC,FACT)

    IMPLICIT NONE

! Subroutine arguments:

     CHARACTER(*), INTENT(INOUT) ::  OB ! report
     INTEGER,      INTENT(INOUT) ::  IN ! pointer to group to be checked
     REAL,         INTENT(INOUT) ::  REXP(:) ! expansion
     INTEGER,      INTENT(INOUT) ::  SECTION ! zero, then number of change section
     INTEGER,      INTENT(INOUT) ::  RC ! return code
     INTEGER,      INTENT(INOUT) ::  FACT ! Expansion array factor

    END SUBROUTINE TAFSECT
  END INTERFACE
END MODULE tafsect_mod
