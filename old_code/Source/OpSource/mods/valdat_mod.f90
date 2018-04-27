MODULE valdat_mod
  INTERFACE
    SUBROUTINE VALDAT(ITIME,ITOD,FOUND,ICT,IFAIL,CERR)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(INOUT) ::  ITIME(9) !- User request times array
    INTEGER,      INTENT(IN)    ::  ITOD !- User request in TODAY format
    LOGICAL,      INTENT(IN)    ::  FOUND(:) !- Array of user-selected MDB keywords
    INTEGER,      INTENT(IN)    ::  ICT(8) !- Current time array
    INTEGER,      INTENT(OUT)   ::  IFAIL !- Return code
    CHARACTER(*), INTENT(OUT)   ::  CERR !- Error message to return to calling prog.

    END SUBROUTINE VALDAT
  END INTERFACE
END MODULE valdat_mod
