MODULE tabled_mod
  INTERFACE
    SUBROUTINE TABLED(XREQ,YREQ,SEQ,ND)
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: XREQ
      INTEGER, INTENT(IN) :: YREQ
      INTEGER, INTENT(OUT) :: SEQ(*)
      INTEGER, INTENT(OUT) :: ND
    END SUBROUTINE TABLED
  END INTERFACE
END MODULE tabled_mod