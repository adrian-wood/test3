MODULE bufret_mod
  INTERFACE
    SUBROUTINE BUFRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND,  &
         ARRAY, NOBS2, NELEM, IOBNUM, IDESC, NDES, ILVL,          &
         ISTAT, IERR, IDSK, LMSG, LFLAG, QCREQ, CSTR, CREP,       &
         SUBTYPE, NEWCALL, LATSUB, LONSUB, RPOLE, FOUND,          &
         ELIST)

IMPLICIT NONE

! Interface arguments
INTEGER,INTENT(IN)              :: ITIME(8)    ! User's data time window (y,m,d,hhmm)*2
INTEGER,INTENT(IN)              :: IRTIM(10)   ! User's TOR window limits (y,m,d,h,m)*2
REAL,INTENT(IN)                 :: AREA(5)     ! User defined area specification
CHARACTER(LEN=9),INTENT(IN)     :: CHID(50)    ! User's list of platform IDs
INTEGER,INTENT(IN)              :: ISELECT(50) ! User's list of data selection values
INTEGER,INTENT(IN)              :: ILAND       ! User land/sea indicator
INTEGER,INTENT(IN)              :: NOBS2       ! Number of decoded obs. ARRAY can hold
INTEGER,INTENT(IN)              :: NELEM       ! User's no. of elements
REAL,INTENT(INOUT)              :: ARRAY(NOBS2,NELEM) ! User's array to hold decoded data
INTEGER,INTENT(INOUT)           :: IOBNUM      ! (In/Out) Next/last ob. slot in ARRAY
INTEGER,INTENT(IN)              :: NDES        ! Number of user in-line elements
INTEGER,INTENT(IN)              :: IDESC(NDES) ! Array of in-line element pointers
INTEGER,INTENT(IN)              :: ILVL(NDES)  ! User in-line element replications
INTEGER,INTENT(INOUT)           :: ISTAT       ! MetDB status indicator
INTEGER,INTENT(OUT)             :: IERR        ! MetDB error code (0 = good, 16 = bad)
INTEGER,INTENT(IN)              :: IDSK(5)     ! Dataset details
LOGICAL,INTENT(IN)              :: LMSG        ! TRUE if retrieving messages one by one
LOGICAL,INTENT(IN)              :: LFLAG       ! TRUE to produce diagnostic printout
LOGICAL,INTENT(INOUT)           :: QCREQ       ! TRUE if QC bits are required
CHARACTER(LEN=*),INTENT(INOUT)  :: CSTR(NOBS2) ! Char strings from mess
CHARACTER(LEN=*),INTENT(INOUT)  :: CREP(NOBS2) ! USER'S REPORT TEXT
CHARACTER(LEN=8),INTENT(IN)     :: SUBTYPE     ! MetDB data subtype
LOGICAL, INTENT(INOUT)          :: NEWCALL     ! TRUE for new MetDB retrieval
INTEGER,INTENT(IN)              :: LATSUB      ! Latitude subscript in u ser's request
INTEGER,INTENT(IN)              :: LONSUB      ! Longitude subscript in  user's request
REAL,INTENT(IN)                 :: RPOLE(2)    ! Rotated pole lat. & lon  g. co-ordinates
LOGICAL,INTENT(IN)              :: FOUND(:)    ! Flags for MetDB keyword  s selected
CHARACTER(LEN=8),INTENT(IN)     :: ELIST       ! Element index member name

    END SUBROUTINE BUFRET
  END INTERFACE
END MODULE bufret_mod
