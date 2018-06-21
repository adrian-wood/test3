MODULE findjsr_mod
  INTERFACE
    SUBROUTINE FINDJSR (JOBNAME, STOPFLAG, LJREC, JOBREC, NJREC)

    IMPLICIT NONE

    CHARACTER (LEN=8), INTENT(IN)        :: JOBNAME   ! NAME OF CURRENT STORAGE JOB
    LOGICAL, INTENT(INOUT)               :: STOPFLAG  ! .TRUE. IF JOB TERMINATION IS REQUIRED
    INTEGER, INTENT(IN)                  :: LJREC     ! LENGTH FOR JOB STATUS RECORD
    CHARACTER (LEN=LJREC), INTENT(OUT)   :: JOBREC    ! JOB STATUS RECORD FROM H.K. DATA SET
    INTEGER, INTENT(OUT)                 :: NJREC     ! STATUS RECORD ASSIGNED TO THIS JOB

    END SUBROUTINE FINDJSR
  END INTERFACE
END MODULE findjsr_mod