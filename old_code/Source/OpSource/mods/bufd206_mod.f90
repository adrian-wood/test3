MODULE bufd206_mod
  INTERFACE
    SUBROUTINE BUFD206(STRING,IBEFOR,DESCR,NOBS,CMPRES,Y,N,IRC)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)      :: STRING
      INTEGER,          INTENT(INOUT)   :: IBEFOR
      INTEGER,          INTENT(INOUT)   :: DESCR(*)
      INTEGER,          INTENT(IN)      :: NOBS
      LOGICAL,          INTENT(IN)      :: CMPRES
      INTEGER,          INTENT(IN)      :: Y
      INTEGER,          INTENT(INOUT)   :: N
      INTEGER,          INTENT(OUT)     :: IRC
    END SUBROUTINE BUFD206
  END INTERFACE
END MODULE bufd206_mod
