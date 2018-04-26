MODULE SYNOB_mod
  INTERFACE
    SUBROUTINE SYNOB(REPORT,REPLEN,OCOR,CORNUM,YYGGIW,IDATIM,TTAAII, &
                     CCCC,ICCCC,MIMJ,NFT,BLKSIZ)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)    :: REPORT
    INTEGER, INTENT(IN)              :: REPLEN
    LOGICAL, INTENT(IN)              :: OCOR
    CHARACTER (LEN=2), INTENT(IN)    :: CORNUM
    CHARACTER (LEN=5), INTENT(INOUT) :: YYGGIW
    INTEGER, INTENT(IN)              :: IDATIM(5)
    CHARACTER (LEN=6), INTENT(IN)    :: TTAAII
    CHARACTER (LEN=4), INTENT(IN)    :: CCCC
    INTEGER, INTENT(IN)              :: ICCCC
    CHARACTER (LEN=4), INTENT(IN)    :: MIMJ
    INTEGER, INTENT(IN)              :: NFT
    INTEGER, INTENT(IN)              :: BLKSIZ

    END SUBROUTINE SYNOB
  END INTERFACE
END MODULE SYNOB_mod
