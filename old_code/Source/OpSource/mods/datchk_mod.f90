MODULE DATCHK_MOD
  INTERFACE
    SUBROUTINE DATCHK(IM,ID,IH,IFAIL,CERR)
    
      IMPLICIT NONE
      
      ! Arguments
      INTEGER, INTENT(IN)            :: IM      ! Month  ,IH,IFAIL
      INTEGER, INTENT(IN)            :: ID      ! Day
      INTEGER, INTENT(IN)            :: IH      ! Hour*100 + MINUTE
      INTEGER, INTENT(OUT)           :: IFAIL   ! 8=error, 4=warning, 0=ok
      CHARACTER(LEN=*), INTENT(OUT)  :: CERR    ! Error message
    
    END SUBROUTINE DATCHK
  END INTERFACE
END MODULE DATCHK_MOD
