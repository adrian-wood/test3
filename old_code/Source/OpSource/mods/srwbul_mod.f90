MODULE srwbul_mod
  INTERFACE
    SUBROUTINE SRWBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR, &
                      CORNUM,MIMJ,NFTSRW,BULL)       
      IMPLICIT NONE
      INTEGER, INTENT(INOUT)                :: POINT   ! CURRENT POSITION IN BULLETIN
                                                       ! (BEFORE STN ID IN FIRST REPORT
                                                       ! UNLESS AN OLD REPORT)
      INTEGER, INTENT(INOUT)                :: BULEND  ! END OF BULLETIN (BEFORE NNNN)
      CHARACTER(LEN=*), INTENT(IN)          :: TTAAII  ! WILL BE SRUK95
      CHARACTER(LEN=*), INTENT(IN)          :: CCCC    ! COLLECTING CENTRE
      CHARACTER(LEN=*), INTENT(IN)          :: YYGGGG  ! DAY AND HOUR OF REPORT
      LOGICAL, INTENT(IN)                   :: OCOR    ! CORRECTED REPORT FLAG
      CHARACTER(LEN=*), INTENT(IN)          :: CORNUM  ! CORRECTION NUMBER
      CHARACTER(LEN=7), INTENT(IN)          :: MIMJ    ! 'OLDR' IF AN OLD REPORT
      INTEGER, INTENT(IN)                   :: NFTSRW  ! FT NUMBER FOR SREW DATASET (15)
      CHARACTER(LEN=*), INTENT(INOUT)       :: BULL    ! BULLETIN
    END SUBROUTINE SRWBUL
  END INTERFACE
END MODULE srwbul_mod
