MODULE getkey_mod
  INTERFACE
SUBROUTINE GETKEY(REQ,IPOS,ILEN,KEY,IFAIL)

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(INOUT)     ::  REQ !- users request string
INTEGER, INTENT(INOUT)          ::  IPOS !- position in string REQ
INTEGER, INTENT(INOUT)          ::  ILEN !- length of REQ
CHARACTER(*), INTENT(INOUT)     ::  KEY !- individual keyword selecte   d
INTEGER, INTENT(INOUT)          ::  IFAIL !- error status

END SUBROUTINE GETKEY
END INTERFACE
END MODULE getkey_mod
