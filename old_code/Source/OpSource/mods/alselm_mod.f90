MODULE alselm_mod
  INTERFACE
    SUBROUTINE ALSELM(CTYPE,USRELM,NUM,LTEST)
    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)    ::  CTYPE !- input subtype
    CHARACTER(*), INTENT(INOUT) ::  USRELM(*) !- input element names
    INTEGER,      INTENT(IN)    ::  NUM   !- no. of elems in list
    LOGICAL,      INTENT(IN)    ::  LTEST !- TRUE for diagnostics

    END SUBROUTINE ALSELM
  END INTERFACE
END MODULE alselm_mod
