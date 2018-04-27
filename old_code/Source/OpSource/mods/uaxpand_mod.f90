MODULE UAXPAND_mod
  INTERFACE
    SUBROUTINE UAXPAND(OB,TT,ARRAY,NUM_LEV,IDENT,DESCR,   &
                       B17BIT,PART_TYPE,TYPE,STANDRD,ERR, &
                       TB17BIT5,QCBIT_ARRAY)

    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)   :: OB           ! (a1)  report being expanded
    CHARACTER (LEN=2), INTENT(IN)   :: TT           ! (a2)  report type ie. fixed land
    REAL, INTENT(INOUT)             :: ARRAY(999)   ! (a3)  expanded elements array
    INTEGER, INTENT(INOUT)          :: NUM_LEV      ! (a4)
    CHARACTER (LEN=10), INTENT(OUT) :: IDENT        ! (a5)  index station ID
    INTEGER, INTENT(OUT)            :: DESCR(:)     ! (a6)  expanded descriptor sequence
    CHARACTER (LEN=1), INTENT(OUT)  :: B17BIT       ! (a7)  byte17 index bits 5 - 8
    INTEGER, INTENT(OUT)            :: PART_TYPE    ! (a8)
    CHARACTER (LEN=2), INTENT(OUT)  :: TYPE         ! (a9)
    LOGICAL, INTENT(OUT)            :: STANDRD      ! (a10)  indicates to uasort standrd
    LOGICAL, INTENT(OUT)            :: ERR          ! (a11)  Indicates error in decode
    INTEGER, INTENT(OUT)            :: TB17BIT5     ! (a12)  Trailer Byte17 Bit 5
    REAL, INTENT(INOUT)             :: QCBIT_ARRAY(999)  ! (a13) array of 1 bit QC flags

    END SUBROUTINE UAXPAND
  END INTERFACE
END MODULE UAXPAND_mod
