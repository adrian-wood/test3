MODULE chekreps_mod
  INTERFACE
    SUBROUTINE CHEKREPS (NDSC, NUMDSC, NSEG, NRSEG1, NRSEG2, NSNEST,    &
                         NUMREP, MAXLEV, NCOUNT, MCOUNT, NREPS, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER, INTENT(IN)       ::  NUMDSC         ! Number of descriptors in NDSC array
    INTEGER, INTENT(IN)       ::  NDSC(NUMDSC)   ! Array of descriptors from BUFR decoder
    INTEGER, INTENT(IN)       ::  NSEG           ! Number of segments
    INTEGER, INTENT(IN)       ::  NRSEG1(:)      ! First segment of each replication
    INTEGER, INTENT(IN)       ::  NRSEG2(:)      ! Last segment of each replication
    INTEGER, INTENT(IN)       ::  NSNEST(:)      ! Nesting levels for segments
    INTEGER, INTENT(IN)       ::  MAXLEV         ! First dimension of NUMREP array
    INTEGER, INTENT(IN)       ::  NUMREP(:,:)    ! Replication numbers for levs. & segs.
    INTEGER, INTENT(IN)       ::  NREPS          ! Number of replications in sequence
    INTEGER, INTENT(IN)       ::  NCOUNT(NREPS)  ! Replication counts from BUFR sequence
    INTEGER, INTENT(INOUT)    ::  MCOUNT(NREPS)  ! Replication counts for bulletin
    INTEGER, INTENT(OUT)      ::  ICODE          ! Return code

    END SUBROUTINE CHEKREPS
  END INTERFACE
END MODULE chekreps_mod
