MODULE MSGDSPN_MOD
INTERFACE
SUBROUTINE MSGDSPN(MESSAGE,SEGMENT,IBEFOR,IREPL,QCREQ,           &
                         DISPL,NELEM,INPUT_NSEG,INPUT_SEGNUM,    &
                         INPUT_NWIDTH,INPUT_NREPLEN,LFLAG)

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)   :: MESSAGE
INTEGER,           INTENT(IN)   :: NELEM              ! number of elements
INTEGER,           INTENT(IN)   :: SEGMENT(NELEM)     ! segment number
INTEGER,           INTENT(IN)   :: IBEFOR(NELEM)      ! bits before value (in segment)
INTEGER,           INTENT(IN)   :: IREPL(*)           ! instance (or 1 if no replication)
LOGICAL,           INTENT(IN)   :: QCREQ
INTEGER,           INTENT(OUT)  :: DISPL(NELEM)       ! bits before value (in message)
INTEGER,           INTENT(IN)   :: INPUT_NSEG         ! number of segments
INTEGER,           INTENT(IN)   :: INPUT_SEGNUM(*)    ! segment number
INTEGER,           INTENT(IN)   :: INPUT_NWIDTH(*)    ! width of replication count (or 0)
INTEGER,           INTENT(IN)   :: INPUT_NREPLEN(*)   ! bits in segment (to replicate)
LOGICAL,           INTENT(IN)   :: LFLAG

END SUBROUTINE MSGDSPN
END INTERFACE
END MODULE MSGDSPN_MOD
