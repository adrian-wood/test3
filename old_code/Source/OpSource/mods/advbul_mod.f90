MODULE advbul_mod
  INTERFACE
    SUBROUTINE ADVBUL(BULEND,TTAAII,CCCC,YYGGGG,OCOR,FT,BULL) 
      IMPLICIT NONE
      INTEGER, INTENT(IN)           :: BULEND      ! End of bulletin.
      CHARACTER(LEN=*), INTENT(IN)     :: TTAAII      ! Bulletin identifier.
      CHARACTER(LEN=*), INTENT(IN)     :: CCCC        ! Originating centre.
      CHARACTER(LEN=*), INTENT(IN)     :: YYGGGG      ! Bulletin time.
      LOGICAL, INTENT(IN)              :: OCOR        ! Flag set if bulletin is a
      INTEGER, INTENT(IN)              :: FT          ! File allocation number.
      CHARACTER(LEN=*), INTENT(IN)  :: BULL        ! Report data (bulletin).  
    END SUBROUTINE ADVBUL
  END INTERFACE
END MODULE advbul_mod