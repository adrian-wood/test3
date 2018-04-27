MODULE storbufr_mod
  INTERFACE
    SUBROUTINE STORBUFR (DATYPE, BUL18, BULL, ITEMS, STORFLAG)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=8),  INTENT(IN)    :: DATYPE     !a01 MetDB data type
    CHARACTER(LEN=18), INTENT(IN)    :: BUL18      !a02 Bulletin header ('TTAAii CCCC YYGGgg')
    CHARACTER(LEN=*),  INTENT(IN)    :: BULL       !a03 BUFR bulletin ('BUFR......7777')
    INTEGER,           INTENT(INOUT) :: ITEMS(0:*) !a04 Data processing items (from headers d/s)
    LOGICAL,           INTENT(INOUT) :: STORFLAG   !a05 .TRUE. if BUFR message is to be stored

    END SUBROUTINE STORBUFR
  END INTERFACE
END MODULE storbufr_mod
