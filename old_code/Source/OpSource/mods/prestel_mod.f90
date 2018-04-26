MODULE prestel_mod
  INTERFACE
    SUBROUTINE PRESTEL(BULL,POINT,BEND,TTAAII,CCCC,YYGGGG,   &
                   NFTTEL)
    IMPLICIT NONE

! Arguments

    CHARACTER(LEN=*),INTENT(IN) :: BULL      !(1)
    INTEGER,         INTENT(IN) :: POINT     !(2)
    INTEGER,         INTENT(IN) :: BEND      !(3)
    CHARACTER(LEN=6),INTENT(IN) :: TTAAII    !(4)
    CHARACTER(LEN=4),INTENT(IN) :: CCCC      !(5)
    CHARACTER(LEN=6),INTENT(IN) :: YYGGGG    !(6)
    INTEGER,         INTENT(IN) :: NFTTEL    !(7)
    END SUBROUTINE PRESTEL
  END INTERFACE
END MODULE prestel_mod
