MODULE CHKMODID_MOD
  INTERFACE
    SUBROUTINE CHKMODID(InputName,LengthName,CodeName)
    
      IMPLICIT NONE
    
!-----------------------------------------------------------------------
! Declare argument variables (in interface order)
!-----------------------------------------------------------------------
      CHARACTER(LEN=*), INTENT(IN)   :: InputName  !- BUFR code name
      INTEGER, INTENT(IN)            :: LengthName !- Length of BUFR code name
      INTEGER, INTENT(INOUT)         :: CodeName   !- BUFR code value

    END SUBROUTINE CHKMODID
  END INTERFACE
END MODULE CHKMODID_MOD
