MODULE MSGDSP_MOD
INTERFACE
SUBROUTINE MSGDSP(MESSAGE,SEGMENT,IBEFOR,DISPL,NELEM,NSEG,  &
                        NWIDTH,NREPLEN,LFLAG)

IMPLICIT NONE

!----------------------------------------------------------------------
! interface arguments
!----------------------------------------------------------------------

CHARACTER (LEN=*), INTENT(IN)    :: MESSAGE
INTEGER,           INTENT(IN)    :: NELEM
INTEGER,           INTENT(IN)    :: SEGMENT(NELEM)
INTEGER,           INTENT(IN)    :: IBEFOR(NELEM)
INTEGER,           INTENT(OUT)   :: DISPL(NELEM)
INTEGER,           INTENT(IN)    :: NSEG
INTEGER,           INTENT(IN)    :: NWIDTH(NSEG)
INTEGER,           INTENT(IN)    :: NREPLEN(NSEG)
LOGICAL,           INTENT(IN)    :: LFLAG

END SUBROUTINE MSGDSP
END INTERFACE
END MODULE MSGDSP_MOD
