MODULE BUFRQOP_mod
  INTERFACE
    FUNCTION BUFRQOP(DESCR,ND) RESULT(BUFRQOP_0)
      IMPLICIT NONE
      INTEGER,INTENT(INOUT) :: DESCR(*)
      INTEGER,INTENT(INOUT) :: ND
      LOGICAL :: BUFRQOP_0
    END FUNCTION BUFRQOP
  END INTERFACE
END MODULE BUFRQOP_mod
