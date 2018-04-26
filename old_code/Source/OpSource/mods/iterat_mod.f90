MODULE ITERAT_MOD
  INTERFACE
    SUBROUTINE ITERAT (ELEM, IREP, TMPELM, NUMBER, IERR)

    IMPLICIT NONE
!                                   Subroutine arguments

    CHARACTER(*),  INTENT(IN)    :: ELEM      ! String of element names
    INTEGER,       INTENT(IN)    :: IREP      ! Replication count
    CHARACTER(36), INTENT(INOUT) :: TMPELM(:) ! Array of element names
    INTEGER,       INTENT(INOUT) :: NUMBER    ! (IN) Dimension of TMPELM
                                              ! (OUT) Number of names
    INTEGER,       INTENT(OUT)   :: IERR      ! return code

    END SUBROUTINE ITERAT
  END INTERFACE
END MODULE ITERAT_MOD
