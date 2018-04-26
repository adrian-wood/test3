MODULE UAHEAD_mod
  INTERFACE
    SUBROUTINE UAHEAD(OB,PTR,REPLEN,TT,PART,ARRAY,BLOCK,STN,IDENT,  &
                      TYPE,ID,KNOTS,ERROR,B17BITS,QCBIT_ARRAY)
    IMPLICIT NONE

    CHARACTER (LEN=*), INTENT(IN)     :: OB               ! (a1) raw report
    INTEGER, INTENT(INOUT)            :: PTR              ! (a2) pointer within report
    INTEGER,INTENT(IN)                :: REPLEN ! (a3)
    CHARACTER (LEN=2), INTENT(IN)     :: TT               ! (a4) part identifier
    CHARACTER (LEN=1), INTENT(INOUT)  :: PART             ! (a5) part a,b,c,d
    REAL, INTENT(INOUT)               :: ARRAY(999)       ! (a6) array of expanded elements
    INTEGER, INTENT(OUT)              :: BLOCK            ! (a7) WMO block number
    INTEGER, INTENT(OUT)              :: STN              ! (a8) WMO station number
    CHARACTER (LEN=*), INTENT(OUT)    :: IDENT            ! (a9) id of station
    CHARACTER (LEN=2), INTENT(OUT)    :: TYPE             ! (a10) report type
    INTEGER, INTENT(OUT)              :: ID               ! (a11) Top wind level indicator
    LOGICAL, INTENT(OUT)              :: KNOTS            ! (a12) indicates wind speed in knots
    LOGICAL, INTENT(OUT)              :: ERROR            ! (a13) indicates error in decode
    CHARACTER (LEN=1), INTENT(OUT)    :: B17BITS          ! (a14) byte 17 bits in index
    REAL, INTENT(INOUT)               :: QCBIT_ARRAY(999) ! (a15) qc bit flags

    END SUBROUTINE UAHEAD
  END INTERFACE
END MODULE UAHEAD_mod
