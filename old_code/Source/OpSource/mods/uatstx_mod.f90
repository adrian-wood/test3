MODULE UATSTX_mod
  INTERFACE
    SUBROUTINE UATSTX(STRING,PTR,LEVEL,L,MAXL,ID,ERROR)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(INOUT)  :: STRING    ! (a1)
    INTEGER, INTENT(INOUT)            :: PTR       ! (a2)
    CHARACTER (LEN=2), INTENT(IN)     :: LEVEL(:)  ! (a3)
    INTEGER, INTENT(INOUT)            :: L         ! (a4)
    INTEGER, INTENT(IN)               :: MAXL      ! (a5)
    INTEGER, INTENT(INOUT)            :: ID        ! (a6)
    INTEGER, INTENT(INOUT)            :: ERROR     ! (a7)

    END SUBROUTINE UATSTX
  END INTERFACE
END MODULE UATSTX_mod
