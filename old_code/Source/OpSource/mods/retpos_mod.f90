MODULE retpos_mod
  INTERFACE
    SUBROUTINE RETPOS (NTYPE, NSEG, NVAL, LREQ, LNUM, NSNEST, NUMREP, &
               NCOUNT, MSBFOR, MRVALS, MCOUNT, IDISP, NDISP, ICODE)   !3

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(1), INTENT(IN)    ::  NTYPE(:)    ! Array of codes for element types
    INTEGER,      INTENT(IN)    ::  NSEG(:)     ! Segment numbers for elements
    INTEGER,      INTENT(IN)    ::  NVAL(:)     ! Data locations for elements in segment
    INTEGER,      INTENT(IN)    ::  LREQ(:)     ! Array describing request string
    INTEGER,      INTENT(IN)    ::  LNUM        ! Number of integers used in LREQ
    INTEGER,      INTENT(IN)    ::  NSNEST(:)   ! Nesting levels for each segment
    INTEGER,      INTENT(IN)    ::  NUMREP(:,:) ! Replication numbers for segments
    INTEGER,      INTENT(IN)    ::  NCOUNT(:)   ! Replication counts from element index   !3
    INTEGER,      INTENT(IN)    ::  MSBFOR(:)   ! No. of data values before each segment
    INTEGER,      INTENT(IN)    ::  MRVALS(:)   ! No. of data values in each replication
    INTEGER,      INTENT(IN)    ::  MCOUNT(:)   ! Replication counts for BUFR sequence
    INTEGER,      INTENT(OUT)   ::  IDISP(:)    ! Displacements for data values array
    INTEGER,      INTENT(INOUT) ::  NDISP       ! Counter for elements in IDISP array
    INTEGER,      INTENT(OUT)   ::  ICODE       ! Integer return code

    END SUBROUTINE RETPOS
  END INTERFACE
END MODULE retpos_mod
