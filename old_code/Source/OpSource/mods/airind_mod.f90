MODULE airind_mod
  INTERFACE
    SUBROUTINE AIRIND(INDX_ARAY1,INDX_ARAY2,LOOP,SIGN,TTAAII,   &
                      CCCC,DATIME,ENTRY,IDENT,SPAN)
    IMPLICIT NONE

    ! Arguments

    REAL,INTENT(IN)                :: INDX_ARAY1(:)
    REAL,INTENT(IN)                :: INDX_ARAY2(:)
    INTEGER,INTENT(IN)             :: LOOP
    CHARACTER(LEN=8),INTENT(IN)    :: SIGN
    CHARACTER(LEN=*),INTENT(IN)    :: TTAAII
    CHARACTER(LEN=*),INTENT(IN)    :: CCCC
    INTEGER,INTENT(OUT)            :: DATIME(5)
    CHARACTER(LEN=23),INTENT(INOUT):: ENTRY
    CHARACTER(LEN=9),INTENT(OUT)   :: IDENT
    INTEGER,INTENT(IN)             :: SPAN  !time span of reps in bulletin
    END SUBROUTINE AIRIND
  END INTERFACE
END MODULE airind_mod
