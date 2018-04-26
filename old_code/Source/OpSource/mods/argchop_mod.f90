MODULE ARGCHOP_mod
  INTERFACE
    SUBROUTINE ARGCHOP (NOBS1, NSEQ, NTH, IDESCR, NDESCR,       &
                        NAMES, VALIN, VALOUT, MSGBULL, LENBUL,  &
                        ICAT,ISUB)

    IMPLICIT NONE

    INTEGER, INTENT(IN)             :: NOBS1     ! (a1)  Number of obs in input values array VALIN
    INTEGER, INTENT(IN)             :: NSEQ      ! (a2)  Number of obs in output BUFR message
    INTEGER, INTENT(IN)             :: NTH       ! (a3)  Observation number required from VALIN
    INTEGER, INTENT(IN)             :: IDESCR(:) ! (a4)  Descriptor array
    INTEGER, INTENT(IN)             :: NDESCR    ! (a5)  Number of descriptors for 1st ob in IDESCR
    CHARACTER (LEN=*),INTENT(INOUT) :: NAMES     ! (a6)  Input character values
    REAL, INTENT(IN)                :: VALIN(:)  ! (a7)  Decoded values from ARGO bulletin
    REAL, INTENT(OUT)               :: VALOUT(:) ! (a8)  Values for encoding new ARGO bulletin
    CHARACTER (LEN=*), INTENT(OUT)  :: MSGBULL   ! (a9)  Re-encoded ARGO bulletin
    INTEGER, INTENT(OUT)            :: LENBUL    ! (a10) Length of re-encoded message
    INTEGER, INTENT(IN)             :: ICAT      ! (a11) BUFR category
    INTEGER, INTENT(IN)             :: ISUB      ! (a12) Data sub-category

    END SUBROUTINE ARGCHOP
  END INTERFACE
END MODULE ARGCHOP_mod
