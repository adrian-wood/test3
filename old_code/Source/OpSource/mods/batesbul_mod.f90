MODULE batesbul_mod
  INTERFACE
    SUBROUTINE BATESBUL(BULL,TTAAII,CCCC,OCOR,MIMJ,NFT)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT)                  :: BULL     ! Bulletin,                            a1
      CHARACTER(LEN=*), INTENT(IN)                     :: TTAAII   ! (not used till AIRSTO adds trailers),a2
      CHARACTER(LEN=*), INTENT(IN)                     :: CCCC     ! (not used till AIRSTO adds trailers),a3
      LOGICAL, INTENT(IN)                              :: OCOR     ! used to set COR flag in trailer,     a4
      CHARACTER(LEN=*), INTENT(INOUT)                  :: MIMJ     ! first MiMiMjMj, then current...,     a5
      INTEGER, INTENT(IN)                              :: NFT      ! storage FT number                    a6
    END SUBROUTINE BATESBUL
  END INTERFACE
END MODULE batesbul_mod
