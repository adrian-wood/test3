MODULE metbul_mod
  INTERFACE
    SUBROUTINE METBUL(POINT,BEND,TTAAII,CCCC,YYGGGG,OAMD,AMDNUM,OCOR, &
                      CORNUM,NFT,OERR,BULL)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,          INTENT(INOUT) :: POINT !A1 Current point within repor t.
    INTEGER,          INTENT(INOUT) :: BEND !A2 Bulletin end.
    CHARACTER(LEN=*), INTENT(IN)    :: TTAAII !A3 Bulletin identifier.
    CHARACTER(LEN=*), INTENT(IN)    :: CCCC !A4 Bulletin originating cen    tre.
    CHARACTER(LEN=*), INTENT(INOUT) :: YYGGGG !A5 Day and time of bullet    in
    LOGICAL,          INTENT(IN)    :: OAMD !A6 Flag set if bulletin is an  amendment.
    CHARACTER(LEN=*), INTENT(IN)    :: AMDNUM !A7 Amendment number.
    LOGICAL,          INTENT(IN)    :: OCOR !A8 Flag set if bulletin is a c orrection.
    CHARACTER(LEN=*), INTENT(IN)    :: CORNUM !A9 Correction number.
    INTEGER,          INTENT(INOUT) :: NFT !A10 File allocation number.
    LOGICAL,          INTENT(OUT)   :: OERR !A11 Set if report invalid.
    CHARACTER(LEN=*), INTENT(INOUT) :: BULL !A12 Bulletin of reports to     be stored.

    END SUBROUTINE METBUL
  END INTERFACE
END MODULE metbul_mod
