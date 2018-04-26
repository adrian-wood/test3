MODULE tropbul_mod
  INTERFACE
    SUBROUTINE TROPBUL(MESSAGE,TTAAII,CCCC,YYGGGG,IUNIT) 
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(INOUT)     :: MESSAGE  ! BUFR message (in bulletin envelope),      a1
      CHARACTER(LEN=6), INTENT(IN)        :: TTAAII   ! bulletin heading (ICXL20 to start with),  a2
      CHARACTER(LEN=4), INTENT(IN)        :: CCCC     ! collecting centre (FMEE to start with),   a3
      CHARACTER(LEN=6), INTENT(IN)        :: YYGGGG   ! bulletin day & time (characters),         a4
      INTEGER, INTENT(IN)                 :: IUNIT    ! FT number for storage,                    a5
    END SUBROUTINE TROPBUL
  END INTERFACE
END MODULE tropbul_mod
