MODULE readmap_mod
  INTERFACE
    SUBROUTINE READMAP (NUNIT, MAXSEQ, MAXSEG, MAXREP, MAXLEV,    &
               MAXNAME, NDX, NDES, LISTSEQ, LSTART, NSEQS, NDXS,  &
               NSEGS, NREPS, NSVALS, NSNEST, NUMREP, NCOUNT,      &
               NAME, NTYPE, NREQ, NSEG, NVAL, NAMES, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,       INTENT(IN)     ::  NUNIT         ! Unit number for input data set
    INTEGER,       INTENT(IN)     ::  MAXSEQ        ! Maximum permitted no. of BUFR sequences
    INTEGER,       INTENT(IN)     ::  MAXSEG        ! Maximum permitted no. of segments
    INTEGER,       INTENT(IN)     ::  MAXREP        ! Maximum permitted no. of replications
    INTEGER,       INTENT(IN)     ::  MAXLEV        ! Maximum permitted nesting level
    INTEGER,       INTENT(IN)     ::  MAXNAME       ! Maximum permitted no. of element names
    INTEGER,       INTENT(OUT)    ::  NDX(:)        ! Index number associated with each seq.
    INTEGER,       INTENT(OUT)    ::  NDES(:)       ! No. of descriptors in BUFR sequences
    CHARACTER(*),  INTENT(OUT)    ::  LISTSEQ       ! String containing BUFR sequences
    INTEGER,       INTENT(OUT)    ::  LSTART(:)     ! Sequence start locations in LISTSEQ
    INTEGER,       INTENT(OUT)    ::  NSEQS         ! Number of sequences in input data set
    INTEGER,       INTENT(OUT)    ::  NDXS          ! Number of indexes in input data set
    INTEGER,       INTENT(OUT)    ::  NSEGS(:)      ! Number of segments for each index
    INTEGER,       INTENT(OUT)    ::  NREPS(:)      ! Number of replications for each index
    INTEGER,       INTENT(OUT)    ::  NSVALS(:,:)   ! No. of values in each seg. & index
    INTEGER,       INTENT(OUT)    ::  NSNEST(:,:)   ! Nesting levels for each seg. & index
    INTEGER,       INTENT(OUT)    ::  NUMREP(:,:,:) ! Replication numbers
    INTEGER,       INTENT(OUT)    ::  NCOUNT(:,:)   ! Replication counts for rep. & index
    CHARACTER(36), INTENT(OUT)    ::  NAME(:)       ! Retrieval element names
    CHARACTER,     INTENT(OUT)    ::  NTYPE(:)      ! Code for type of element
    INTEGER,       INTENT(OUT)    ::  NREQ(16)      ! NAME subscripts for selected data items
    INTEGER,       INTENT(OUT)    ::  NSEG(:,:)     ! Segment numbers for each name & index
    INTEGER,       INTENT(OUT)    ::  NVAL(:,:)     ! Data location in seg. for name & index
    INTEGER,       INTENT(OUT)    ::  NAMES         ! Number of element names found
    INTEGER,       INTENT(OUT)    ::  ICODE         ! Return code

    END SUBROUTINE READMAP
  END INTERFACE
END MODULE readmap_mod
