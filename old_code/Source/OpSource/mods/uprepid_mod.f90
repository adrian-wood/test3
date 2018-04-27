MODULE uprepid_mod
  INTERFACE
    SUBROUTINE UPREPID(Byte17,RprtId)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(1), INTENT(IN)    ::  Byte17 !- current index entry
    INTEGER,      INTENT(OUT)   ::  RprtId  !- report identity

    END SUBROUTINE UPREPID
  END INTERFACE
END MODULE uprepid_mod
