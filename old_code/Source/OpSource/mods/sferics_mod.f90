MODULE sferics_mod
  INTERFACE
    SUBROUTINE SFERICS(BULL,POINT,BEND,TTAAII,CCCC,    &
                         CORNUM,NFTSFR)
    IMPLICIT NONE

! Arguments

    CHARACTER(LEN=*),INTENT(IN)    :: BULL    !(1)
    INTEGER,         INTENT(INOUT) :: POINT   !(2)
    INTEGER,         INTENT(IN)    :: BEND    !(3)
    CHARACTER(LEN=6),INTENT(IN)    :: TTAAII  !(4)
    CHARACTER(LEN=4),INTENT(IN)    :: CCCC    !(5)
    CHARACTER(LEN=2),INTENT(IN)    :: CORNUM  !(6)
    INTEGER,         INTENT(IN)    :: NFTSFR  !(7)
    END SUBROUTINE SFERICS
  END INTERFACE
END MODULE sferics_mod
