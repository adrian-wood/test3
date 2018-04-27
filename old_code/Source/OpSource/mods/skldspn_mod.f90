MODULE SKLDSPN_MOD
  INTERFACE
    SUBROUTINE SKLDSPN (IDESC,NELEM,LMINDEX,NINDEX,QCREQ,SEGMENT, &
                       IBEFOR,WIDTH,SCALE,REFVAL,NELREQ,LFLAG)
    IMPLICIT NONE
!                                Subroutine arguments

    INTEGER,      INTENT(IN)  :: IDESC(*)  ! element nos. (NOT descriptors!)
    INTEGER,      INTENT(IN)  :: NELEM     ! input no. of elements requested
    CHARACTER(*), INTENT(IN)  :: LMINDEX(*)
    INTEGER,      INTENT(IN)  :: NINDEX    ! number of index entries
    LOGICAL,      INTENT(IN)  :: QCREQ     ! set if QC bits requested
    INTEGER,      INTENT(OUT) :: SEGMENT(*)
    INTEGER,      INTENT(OUT) :: IBEFOR(*)
    INTEGER,      INTENT(OUT) :: WIDTH(*)
    INTEGER,      INTENT(OUT) :: SCALE(*)
    INTEGER,      INTENT(OUT) :: REFVAL(*)
    INTEGER,      INTENT(OUT) :: NELREQ    ! no. of elements in output table
    LOGICAL,      INTENT(IN)  :: LFLAG     ! flag fof diagnostics printout

    END SUBROUTINE SKLDSPN
  END INTERFACE
END MODULE SKLDSPN_MOD
