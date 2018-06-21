MODULE bufrchek_mod
  INTERFACE
    SUBROUTINE BUFRCHEK (BULL, NOW, NBUFR, LENBUL, NBTYP, NBSUB, KODE)
      IMPLICIT NONE

! Arguments

      CHARACTER(LEN=*),INTENT(INOUT) ::  BULL   ! (1) BULLETIN TO BE CHECKED
      INTEGER,INTENT(IN)          ::  NOW(8) ! (2) DATE/TIME OF RECEIPT (FROM 'DATIM')
      INTEGER,INTENT(OUT)         ::  NBUFR  ! (3) LOCATION OF "BUFR" IN BULLETIN
      INTEGER,INTENT(OUT)         ::  LENBUL ! (4) LENGTH OF MESSAGE (FROM SECTION 0)
      INTEGER,INTENT(OUT)         ::  NBTYP  ! (5) BUFR TYPE & SUBTYPE (FROM SECTION 1)
      INTEGER,INTENT(OUT)         ::  NBSUB  ! (6) BUFR TYPE & SUBTYPE (FROM SECTION 1)
      INTEGER,INTENT(OUT)         ::  KODE   ! (7) RETURN CODE (SEE ABOVE FOR DETAILS)
    END SUBROUTINE BUFRCHEK
  END INTERFACE
END MODULE bufrchek_mod
