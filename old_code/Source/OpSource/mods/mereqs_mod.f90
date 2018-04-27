MODULE mereqs_mod
  INTERFACE
      SUBROUTINE MEREQS(DATIME,STRING,LSTRING,IRC)
      IMPLICIT NONE
      INTEGER   DATIME(*)
      CHARACTER STRING*(*) ! request to be stored or returned
      INTEGER   LSTRING
      INTEGER   IRC       ! return code input/output to MEREQS
      END SUBROUTINE MEREQS
  END INTERFACE
END MODULE mereqs_mod
