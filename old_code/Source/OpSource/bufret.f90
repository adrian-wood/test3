SUBROUTINE BUFRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND,  &
     ARRAY, NOBS2, NELEM, IOBNUM, IDESC, NDES, ILVL,          &
     ISTAT, IERR, IDSK, LMSG, LFLAG, QCREQ, CSTR, CREP,       &
     SUBTYPE, NEWCALL, LATSUB, LONSUB, RPOLE, FOUND,          &
     ELIST)

!-----------------------------------------------------------------------
!
! SUBROUTINE   : BUFRET
!
! PURPOSE      : Retrieval of satellite (and possibly other) data types
!                stored in new-format storage data sets.
!
! DESCRIPTION  : BUFRET searches through a MetDB storage data set
!                retrieving data elements as specified by the user's
!                request string and transferring the data to the user's
!                buffer ("ARRAY"). Steps include:
!
!                 - Check ISTAT to see if it's a new retrieval request.
!                 - If so, set up pointers for loops and read the
!                     element index using READIDX.
!                 - Loop over index periods covering user's time window.
!                 - Loop over index blocks in each period.
!                 - Loop over index entries in each block.
!                 - Check details in index entry against:
!                    - user's data time window,
!                    - user's list of satellite identifiers,
!                    - user's list of data selection values,
!                    - area (un-rotated lat/long only),
!                    - land/sea flag,
!                    - time of receipt window.
!                 - If message is still wanted:
!                    - Call BUFINDX to get displacements in message.
!                    - Call VALARR to get required data values and
!                        transfer to user's buffer.
!                    - Return if (1) user's array has been filled or
!                        (2) messages requested one by one.
!                 - Return if retrieval request is complete.
!                 - Save pointers to pick up where left off next call.
!
! USAGE        : CALL BUFRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND,
!                ARRAY, NOBS2, NELEM, IOBNUM, IDESC, NDES, ILVL,
!                ISTAT, IERR, IDSK, LMSG, LFLAG, QCREQ, CSTR, CREP,
!                SUBTYPE, NEWCALL, LATSUB, LONSUB, RPOLE, FOUND,
!                ELIST)
!
! ARGUMENTS    : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  ITIME   I   I4     8   User data time window (y,m,d,hhmm)*2.
!      2  IRTIM   I   I4    10   User receipt time window (y,m,d,h,m)*2.
!      3  AREA    I   R4     5   User-defined area specification.
!      4  CHID    I   C9    50   List of wanted platform IDs.
!      5  ISELECT I   I4    50   List of wanted data selection vals.
!      6  ILAND   I   I4         User's land/sea indicator (1 for land,
!                                     2 for sea, 0 for both).
!      7  ARRAY   O   R4 (NOBS2,   User's array of decoded elements.
!                         NELEM)   (NELEM elements for NOBS2 obs.)
!      8  NOBS2   I   I4         First dimension of ARRAY (no. of obs).
!      9  NELEM   I   I4         2nd dimension of ARRAY (no. of elmnts).
!     10  IOBNUM I/O  I4         (I) Next unused ob. slot in ARRAY,
!                                (O) Last used ob. slot in ARRAY.
!     11  IDESC   I   I4  NDES   Array of in-line element pointers.
!     12  NDES    I   I4         Number of user's in-line elements.
!     13  ILVL    I   I4  NDES   Number of replications of each element.
!     14  ISTAT  I/O  I4         MetDB status indicator:
!                                  (0 = new request, 4 = more data to
!                                  come, 8 = complete, 16 = problem).
!     15  IERR    O   I4         Error code (0 = OK, 16 = error).
!     16  IDSK    I   I4     5   Storage data set details (from DDICT)
!                                  (only elements 2 and 3 used here).
!     17  LMSG    I   L4         Flag for one BUFR message at a time.
!     18  LFLAG   I   L4         Flag for generating diagnostic output.
!     19  QCREQ  I/O  L4         Flag for getting associated QC bits.
!     20  CSTR   I/O  C*  NOBS2  Character strings from message.
!     21  CREP   I/O  C*  NOBS2  Raw report text.
!     22  SUBTYPE I   C8         MetDB data subtype.
!     23  NEWCALL I/O L4         Flag for new retrieval request.
!     24  LATSUB  I   I4         Latitude element in DISPL array.
!     25  LONSUB  I   I4         Longitude element in DISPL array.
!     26  RPOLE   I   R4     2   True lat. and long. of rotated pole.
!     27  FOUND   I   L4     *   MetDB keywords (from GETREQ).
!     28  ELIST   I   C8         Element index member name
!
! CALLED BY    : MDB
!
! CALLS        : BUFINDX, DATIM, DESFXY, DT2HRS, HRS2DT,
!                ICHAR2, ICHAR3, LOCALD, READIDX, VALARR
!
! HISTORY      : Original version by Brian Barwell, February 2001.
!
! REVISION INFO:
!
! $Workfile: bufret.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 05/04/2011 14:10:15$
!
! CHANGE RECORD:
!
! $Log:
!  9    MetDB_Refresh 1.8         05/04/2011 14:10:15    Alison Weir     Read
!       storage datasets with C routine.
!  8    MetDB_Refresh 1.7         16/02/2011 12:13:01    John Norton     Rework
!        done
!  7    MetDB_Refresh 1.6         09/02/2011 16:30:27    Sheila Needham  Change
!        INTENTs on users arrays
!  6    MetDB_Refresh 1.5         25/11/2010 15:08:28    Brian Barwell   Check
!       for missing time tag before calling DT2HRS.
!  5    MetDB_Refresh 1.4         25/11/2010 09:44:09    Sheila Needham  Fix
!       ITIME print dimensions (missed off the basic merge)
!  4    MetDB_Refresh 1.3         24/11/2010 13:00:16    Rosemary Lavery
!       amended dim of FOUND and tidying 
!  3    MetDB_Refresh 1.2         24/11/2010 12:08:22    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         22/11/2010 16:33:45    Stan Kellett    QCRET
!       changed to intent(inout) as modified by called routine bufindx
!  1    MetDB_Refresh 1.0         04/11/2010 17:12:39    Richard Weedon
!       initioal version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The MeteoroLOGICAL  Database
! Team at the above address.
!-----------------------------------------------------------------------
!
USE bufindx_mod
USE datim_mod
USE desfxy_mod
USE dt2hrs_mod
USE hrs2dt_mod
USE ichar2_mod
USE ichar3_mod
USE locald_mod
USE readidx_mod
USE valarr_mod

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

! Local Parameters

INTEGER,PARAMETER :: IXHED=10     ! Length of index block header
INTEGER,PARAMETER :: MAXBLK=27998 ! Maximum block size of dataset
INTEGER,PARAMETER :: MCHAR=27998  ! Maximum length of CNAME string
INTEGER,PARAMETER :: MDATA=180000 ! Maximum size of VALUES array
INTEGER,PARAMETER :: MDES=4500    ! Maximum number of descriptors
INTEGER,PARAMETER :: MEXT=300     ! size of IEXTRA array
!                  Variable used as dimension of arrays
!
! Integers
! (See also note at end for different meanings of
! some variables when retrieving BUFR messages.)
!
INTEGER  ::  BufrEdition   ! BUFR edition number
INTEGER  ::  DISPL(MDES)   ! Displacements from BUFINDX
INTEGER  ::  F             ! F from FXXYYY BUFR sequence descriptor
INTEGER  ::  I             ! Integer variable for local use
INTEGER  ::  IBLOCK        ! Data block number to read
INTEGER  ::  ICOPY         ! No. of bytes to copy (BUFR retrievals)
INTEGER  ::  IDLEN         ! Length of identifier in CHID array
INTEGER  ::  IDSAT         ! Index entry satid
INTEGER  ::  IDSEL         ! Data selection byte in index
INTEGER  ::  IDTIM1        ! Earliest ob. time in century minutes
INTEGER  ::  IDTIM2        ! Latest ob. time in century minutes
INTEGER  ::  IDUMMY(1)     ! Dummy argument for LOCALD and VALARR
INTEGER  ::  IENTRY        ! First index entry to process this call
INTEGER  ::  IEXTRA(MEXT)  ! Array of extra values (from BUFINDX)
INTEGER  ::  IFAIL         ! Status flag from BUFINDX
INTEGER  ::  IFT           ! Unit number of storage data set
INTEGER  ::  IHOUR         ! Hour of observation time
INTEGER  ::  ILOCD         ! Sequence descriptor (integer)
INTEGER  ::  IMIN          ! Minute of observation time
INTEGER  ::  INDLEN        ! Length of index entry
INTEGER  ::  INT4          ! Dummy variable for TRANSFER function
INTEGER  ::  IOB1, IOB2    ! Temporary storage of ob locs for VALARR
INTEGER  ::  IORC          ! Return code from C read routine
INTEGER  ::  IPDS(2)       ! Pointers to 'data selection' in index
INTEGER  ::  IPDT(2)       ! Pointers to data time in index
INTEGER  ::  IPLL(2)       ! Pointers to lat/long info in index
INTEGER  ::  IPRT(2)       ! Pointers to receipt time in index
INTEGER  ::  IPT0          ! Pointer to before start of index entry
INTEGER  ::  IPT           ! Pointer to a location in index entry
INTEGER  ::  IPTIM1        ! First index time to process this call
INTEGER  ::  IPTIM2        ! Last index time containing user's data
INTEGER  ::  IRCVD(7)      ! Data from BUFR section 1 (for VALARR)
INTEGER  ::  IRLEN         ! Record length of storage data set
INTEGER  ::  IRTIM1        ! User's start TOR in century minutes
INTEGER  ::  IRTIM2        ! User's end TOR in century minutes
INTEGER  ::  ISAT(10)      ! User's list of satellite identifiers
INTEGER  ::  ITIM1         ! User's start time in century minutes
INTEGER  ::  ITIM2         ! User's end time in century minutes
INTEGER  ::  ITOR          ! Index entry TOR in minutes after 00Z
INTEGER  ::  IXTIM(10)     ! Current index period (y,m,d,h,m)*2
INTEGER  ::  IXTIM1        ! Start of index period (century minutes)
INTEGER  ::  IVER          ! Format version numbers for storage d.s.
INTEGER  ::  I1, I3        ! Offsets of sections 1 & 3 in message
INTEGER  ::  JXE           ! Loop variable for index entries
INTEGER  ::  JXP           ! Loop variable for index periods
INTEGER  ::  LASTREP       ! No. of last byte used in CREP element
INTEGER  ::  LASTOB1       ! No. of last ob. copied from VALUES
INTEGER  ::  LASTOB2       ! No. of last ob. position used in ARRAY
INTEGER  ::  LATN          ! Maximum latitude of box
INTEGER  ::  LATS          ! Minimum latitude of box
INTEGER  ::  LENCREP       ! Length of elements of CREP array
INTEGER  ::  LONE          ! Maximum longitude of box
INTEGER  ::  LONW          ! Minimum longitude of box
INTEGER  ::  MEND          ! Message end byte number in data block
INTEGER  ::  MLEN          ! Length of message in bytes
INTEGER  ::  MSTART        ! Message start byte number in data block
INTEGER  ::  NCENT         ! Start year of current century (e.g. 2000)
INTEGER  ::  NELREQ        ! Elements required from BUFINDX
INTEGER  ::  NEXTBLK = 0   ! Next index block to look at
INTEGER  ::  NEXTENT = 0   ! Next index entry in block to look at
INTEGER  ::  NEXTIME = 0   ! Next index time to look at
INTEGER  ::  NMBLK         ! No. of map blocks in storage data set
INTEGER  ::  NOBS1         ! Number of decoded obs. in VALUES array
INTEGER  ::  NOW(8)        ! Current time (from DATIM)
INTEGER  ::  NPLATS        ! Number of user's platform identifiers
INTEGER  ::  NSEL          ! Number of user's data selection vals.
INTEGER  ::  NTRIES        ! No. of index entries in index block
INTEGER  ::  NUMDAT        ! Block number of data block in store
INTEGER  ::  NUMNDX        ! Block number of index block in store
INTEGER  ::  NUMREC        ! Block number
INTEGER  ::  NXBLK         ! No. of index blocks in storage data set
INTEGER  ::  NXMIN         ! No. of minutes in index period
INTEGER  ::  NX00Z         ! Index period offset from 00Z (minutes)
INTEGER  ::  SOURCE(MDES)  ! Source array from BUFINDX
INTEGER  ::  XX            ! XX from FXXYYY BUFR sequence descriptor
INTEGER  ::  YYY           ! YYY from FXXYYY BUFR sequence descriptor

! Note: For retrievals in BUFR, LASTOB1 refers to a byte number
!       in a data record rather than an ob. number in a message.
!       Also LASTOB2 and OBNUM refer to elements of CREP rather
!       than the number of obs. whose data has been put in VALUES.

! Real

REAL  ::   OBLAT         ! Latitude for 1 observation
REAL  ::   OBLON         ! Longitude for 1 observation
REAL  ::   VALUES(MDATA) ! Array of values decoded from message

! Logical                                                               ::  s

LOGICAL  ::   ELIDXFOUND      ! TRUE if element index found
LOGICAL  ::   FIRST = .TRUE.  ! TRUE if first call to BUFRET
LOGICAL  ::   LCONT           ! TRUE if continuing retrieval request
LOGICAL  ::   LMID            ! TRUE if part of message has been passed
LOGICAL  ::   WANTED          ! TRUE if current message is wanted

! Character

CHARACTER(LEN=MAXBLK)   :: BLOCK      ! Block read from stord.s.
CHARACTER(LEN=1)        :: CDUMMY(1)  ! Dummy char arr(for VALARR)
CHARACTER(LEN=10000)    :: CINDX(12)  ! Holds element index
CHARACTER(LEN=MCHAR)    :: CNAME      ! Characters from BUFR decode
CHARACTER(LEN=MAXBLK)   :: DATBLK     ! Data block read from stord.s.
CHARACTER(LEN=6)        :: LOCD       ! Local D descriptor (from message)
CHARACTER(LEN=MAXBLK)   :: NDXBLK     ! Index block read from storage d.s.
CHARACTER(LEN=1)        :: XTYPE      ! Type of element index

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

COMMON /BUFCOM1/ VALUES, CNAME
COMMON /BUFCOM2/ NDXBLK, DATBLK, CINDX
COMMON /BUFCOM3/ SOURCE, IEXTRA, DISPL

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

SAVE ! Everything

!-----------------------------------------------------------------------
! Data statements.
!-----------------------------------------------------------------------

DATA IPDT /3,10/, IPLL /9,16/, IPDS /15,22/, IPRT /18,25/

!-----------------------------------------------------------------------
! First call only:  revision information and current date & time.
!-----------------------------------------------------------------------

IF (FIRST) THEN
  CALL DATIM (NOW)
  NCENT = NOW(8) - MOD(NOW(8),100)
  FIRST = .FALSE.
END IF
!                                                   Diagnostic printout
IF (LFLAG) THEN
  WRITE (*,'(/A/A/2A)') ' In MetDB subroutine BUFRET',      &
                             ' ==========================', &
                             ' Data subtype = ', SUBTYPE
END IF
!                                                    Initialise LASTOB2

LASTOB2 = IOBNUM - 1  ! Last used ob slot in ARRAY

!=======================================================================
!             CHECK "ISTAT" AND SET UP LOOP COUNTERS.
!=======================================================================

!-----------------------------------------------------------------------
!     ISTAT=16:  Reset to 0 for new retrieval request.
!-----------------------------------------------------------------------

IF (ISTAT == 16) THEN
  IF (LFLAG) WRITE (*,*) ' BUFRET: New data set'
  ISTAT = 0
END IF

!-----------------------------------------------------------------------
!     ISTAT=4:  Continuation of existing request.
!-----------------------------------------------------------------------

IF_MORE: &
IF (ISTAT == 4) THEN
  LCONT  = .TRUE.    ! This is a continuation
  IPTIM1 = NEXTIME   ! Next index period to consider
  IENTRY = NEXTENT   ! Next index entry to look at
!                                                              Printout
  IF (LFLAG) THEN
    WRITE (*,*) ' BUFRET: Continuation of retrieval -',  &
                      ' IPTIM1, IENTRY =', IPTIM1, IENTRY
  END IF

!-----------------------------------------------------------------------
!     ISTAT=0:  New retrieval request.
!-----------------------------------------------------------------------

ELSE IF (ISTAT == 0) THEN
!                                                   Diagnostic printout
  IF (LFLAG) THEN
    WRITE (*,*) ' BUFRET: Descriptor  Replication'
    WRITE (*,'(1X,I8,8X,I3/)') (IDESC(I), ILVL(I), I=1,NDES)
    WRITE (*,*) ' BUFRET: "AREA" array - ', AREA
  END IF

!-----------------------------------------------------------------------
! Check if NDES > MDES. If so, output an error message and exit the
! subroutine. MDES is the maximum number of elements that can be
! handled by this routine. Exceeding this limit will lead to
! overwriting in routines downstream. MDES is enough to service any
! user request providing that the replication count sizes described
! in subtype documentation have been observed.
!-----------------------------------------------------------------------

  IF (NDES > MDES) THEN
    WRITE (*,'(A)')&
       ' BUFRET: MDB ERROR - NDES > MDES!  NUMBER OF ELEMENTS IN', &
       ' REQUEST STRING (NDES) EXCEEDS MAXIMUM PERMITTED (MDES).', &
       ' CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.',            &
       ' CONTACT METDB TEAM FOR ADVICE IF NECESSARY.'
    WRITE (*,'(/A,A9,2(A,I6))') ' INFO: SUBTYPE = ', SUBTYPE,      &
           ',  NDES =', NDES, ',  MDES =', MDES
    IERR = 16
    RETURN
  END IF

!-----------------------------------------------------------------------
! Check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not,
! output an error and exit subroutine.
!-----------------------------------------------------------------------

  IF (NELEM < NDES) THEN
    WRITE (*,'(A)')' BUFRET: MDB ERROR - ARRAY NOT LARGE ENOUGH !'
    WRITE (*,'(T10,A,I4,A,I4,A)') 'TRY ARRAY (',NOBS2,',',NDES,')'
    IERR = 16
    RETURN
  END IF

!-----------------------------------------------------------------------
! Read header of storage data set. Check formats and version numbers.
!-----------------------------------------------------------------------

  IRLEN = IDSK(2)   ! Record length
  IFT   = IDSK(3)   ! Unit number
!                                                      Read header data

  NUMREC = 1
  CALL METDB_CREAD_DIR (IFT, BLOCK(1:IRLEN), IRLEN, NUMREC, IORC)
  IVER   = TRANSFER(BLOCK( 1: 4),INT4)
  I      = TRANSFER(BLOCK( 5: 8),INT4)
  NMBLK  = TRANSFER(BLOCK( 9:12),INT4)
  NXBLK  = TRANSFER(BLOCK(13:16),INT4)
  NXMIN  = TRANSFER(BLOCK(17:20),INT4)
  NX00Z  = TRANSFER(BLOCK(21:24),INT4)
  INDLEN = TRANSFER(BLOCK(25:28),INT4)

!                                                       Printout
  IF (LFLAG) WRITE (*,*) ' BUFRET: Data set header - ',    &
            IVER, I, NMBLK, NXBLK, NXMIN, NX00Z, INDLEN
!                                                       Check format
IF_DSFMT: &
  IF (IVER >= 65536 .OR. IVER < 0) THEN
    WRITE (*,'(T2,A)')                                     &
                 'BUFRET: CANNOT SUPPORT OLD STORAGE DATA SET FORMAT'
    IERR = 16
    RETURN
!                                                 Check version numbers
  ELSE
    I = IVER/256          ! Data set format version
    IVER = MOD(IVER,256)  ! Index entry version
    IF (I /= 1 .OR. (IVER /= 1.AND.IVER /= 2)) THEN
      WRITE (*,'(T2,2A,2I4)') 'BUFRET: CANNOT SUPPORT ',  &
                   'DATA SET & INDEX FORMATS', I, IVER
      IERR = 16
      RETURN
    END IF
  END IF IF_DSFMT

!-----------------------------------------------------------------------
! Check for local table D entry in second record.
! (NDXBLK used for temporary storage.)
!-----------------------------------------------------------------------

  NUMREC = 2
  CALL METDB_CREAD_DIR (IFT, NDXBLK(1:IRLEN), IRLEN, NUMREC, IORC)

  IF (NDXBLK(1:1) == '3') THEN   ! Local sequence present
    IF (LFLAG) WRITE(*,*)&
     ' BUFRET: Sequence descriptor ',NDXBLK(1:6)
    CALL LOCALD (0, 0, IDUMMY, IDUMMY(1), NDXBLK, 'NEW')
  END IF

!-----------------------------------------------------------------------
! A new data set has now been read so cancel the block numbers of index
! and data blocks held is store.
!-----------------------------------------------------------------------

  NUMNDX = 0  ! Index block number
  NUMDAT = 0  ! Data block number

!-----------------------------------------------------------------------
! Read the element index from the the element index dataset. The subtype
! is now passed in order to select the correct element index.
!-----------------------------------------------------------------------

  CALL READIDX (ELIST, CINDX, XTYPE, ELIDXFOUND)
!                                                        Check if found
  IF (.NOT.ELIDXFOUND) THEN
    WRITE (*,*) 'In BUFRET: MDB ERROR: Cannot find element ',&
     'index for subtype, elist = ',SUBTYPE,ELIST
    IERR = 16
    RETURN
  END IF

  IF (LFLAG) WRITE (*,*) ' BUFRET: After READIDX, XTYPE = ', XTYPE

!-----------------------------------------------------------------------
! For index format 1, convert platform identifiers to satellite IDs.
!-----------------------------------------------------------------------
!                                   Count number of platforms specified
  NPLATS = 0
  DO WHILE (NPLATS < 50 .AND.  &
             CHID(NPLATS+1) /= '00000    ')
    NPLATS = NPLATS + 1
  END DO
!                   Convert to integer satellite IDs if index version 1
!
  IF (IVER == 1) THEN
    NPLATS = MIN0(NPLATS,10)  ! Maximum 10 satellites
    DO I=1,NPLATS
      READ (CHID(I),*) ISAT(I)
    END DO
  END IF

!-----------------------------------------------------------------------
! Count the number of data selection values specified.
!-----------------------------------------------------------------------

  NSEL = 0
  DO WHILE (NSEL < 50 .AND. ISELECT(NSEL+1) >= 0)
    NSEL = NSEL + 1
  END DO

!-----------------------------------------------------------------------
! Convert user's start and end times to century minutes.
!-----------------------------------------------------------------------
!                                                            Start time

  ITIM1 = 60*DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4)/100) +&
     MOD(ITIME(4),100)
!                                               End time (if specified)
  IF (ITIME(6) == 0) THEN
    ITIM2 = ITIM1     ! = Start time
  ELSE
    ITIM2 = 60*DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8)/100) +&
     MOD(ITIME(8),100)
  END IF

!-----------------------------------------------------------------------
! Convert time of receipt limits to century minutes.
!-----------------------------------------------------------------------
!                                                 'Received after' time
  IF (IRTIM(1) == 0) THEN
    IRTIM1 = 0
  ELSE
    IRTIM1 = 60*DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4)) +&
     IRTIM(5)
  END IF
!                                                'Received before' time
  IF (IRTIM(6) == 0) THEN
    IRTIM2 = 0
  ELSE
    IRTIM2 = 60*DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9)) +&
     IRTIM(10)
  END IF

!-----------------------------------------------------------------------
! Century minutes of index periods to look through.
!-----------------------------------------------------------------------

  IPTIM1 = ITIM1 - MOD((ITIM1-NX00Z),NXMIN) ! First index period
  IPTIM2 = ITIM2 - MOD((ITIM2-NX00Z),NXMIN) ! Last index period

  IF (LFLAG) THEN
    WRITE (*,*) ' BUFRET: User start time ', (ITIME(I),I=1,4)
    WRITE (*,*) '         User end time   ', (ITIME(I),I=5,8)
    WRITE (*,*) '         Earliest T.O.R. ', (IRTIM(I),I=1,5)
    WRITE (*,*) '         Latest T.O.R.   ', (IRTIM(I),I=6,10)
    WRITE (*,*) '         Century minutes (User/index/TOR limits)'
    WRITE (*,*) '        ',ITIM1,ITIM2,IPTIM1,IPTIM2,IRTIM1,IRTIM2
  END IF

!-----------------------------------------------------------------------
! Initialise loop counters etc.
!-----------------------------------------------------------------------

  IENTRY  = 1       ! Start from first index entry
  LASTOB1 = 0       ! No obs. yet read from message
  NEXTBLK = 0       ! No continuation of a previous index block
  LCONT   = .FALSE. ! Not continuation of retrieval request
  LMID    = .FALSE. ! Not in mid-message
END IF IF_MORE

ISTAT = 0

!=======================================================================
!    INITIALISE OUTPUT ARRAYS AND POINTERS FOR RETRIEVALS IN BUFR
!=======================================================================

!                                 Clear out CREP and ARRAY if necessary
IF_CLEAR: &
IF (FOUND(34) .AND. (LMSG.OR.LASTOB2 <= 0)) THEN
  LENCREP = LEN(CREP(1))  ! Length of elements of CREP
  LASTOB2 = 0             ! Return to start of user's ARRAY
  LASTREP = LENCREP       ! Put new data in new CREP element
!
!                           Reset CREP and ARRAY
  DO I=1,LENCREP          ! First element
    CREP(1)(I:I) = CHAR(0)
  END DO ! I
  ARRAY(1,1) = 0.0
!
  DO I=2,NOBS2            ! Other elements
    CREP(I) = CREP(1)
    ARRAY(I,1) = 0.0
  END DO ! I
END IF IF_CLEAR

!=======================================================================
!       LOOP OVER INDEX PERIODS COVERING USER'S DATA TIME WINDOW
!=======================================================================

IF (LFLAG) WRITE (*,*) ' BUFRET: Loop over index periods ',&
     IPTIM1, IPTIM2, NXMIN

DO_IDXPER: &
DO JXP=IPTIM1,IPTIM2,NXMIN
!                                           Start from base index block
  IF (NEXTBLK <= 0)&
     NEXTBLK = MOD((JXP-NX00Z)/NXMIN,NXBLK) + NMBLK + 3

!=======================================================================
!            LOOP OVER INDEX BLOCKS IN CURRENT INDEX PERIOD
!=======================================================================

DO_IDXBLK: &
  DO WHILE (NEXTBLK > 0)
!                                      Read index block if not in store
IF_FETCH: &
    IF (NEXTBLK /= NUMNDX) THEN
      CALL METDB_CREAD_DIR (IFT, NDXBLK(1:IRLEN), IRLEN, NEXTBLK, IORC)
      NUMNDX = NEXTBLK
!                                                     Decode index time
      DO I=1,5
        IXTIM(I) = ICHAR(NDXBLK(I:I))
      END DO ! I
!                                               Convert to 4-digit year
      IXTIM(1) = IXTIM(1) + NCENT
      IF (IXTIM(1) > NOW(8)) IXTIM(1) = IXTIM(1) - 100
!
!                                                     Number of entries
      NTRIES = ICHAR2(NDXBLK(6:7))

      IF (LFLAG) WRITE (*,*) ' BUFRET: Index block ', NUMNDX,&
         ', Entries ', NTRIES, ', Time ', (IXTIM(I),I=1,5)

!-----------------------------------------------------------------------
! Use time-tag to check that correct date/time has been read.
!-----------------------------------------------------------------------

      IF (IXTIM(2).EQ.0) THEN  ! Month=0 means no time-tag
        IXTIM1 = -1
      ELSE
        IXTIM1 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IXTIM(4)) + &
                    IXTIM(5)
      END IF
!                                                     Compare time tags
IF_OLDDATA: &
      IF (IXTIM1 /= JXP) THEN ! Old data
!                                                 Start of index period
        I = JXP/60
        CALL HRS2DT (IXTIM(1), IXTIM(2), IXTIM(3), IXTIM(4), I)
        IXTIM(5) = MOD(JXP,60)
!                                                   End of index period
        I = JXP + NXMIN - 1
        CALL HRS2DT (IXTIM(6), IXTIM(7), IXTIM(8), IXTIM(9), I/60)
        IXTIM(10) = MOD(I,60)
!                                                   Information message

        WRITE (6,'(T2,3A,2(I4,2(''/'',I2.2),I3.2,'':'',I2.2,A))') &
                    'NO ', SUBTYPE, ' DATA IN MDB FOR PERIOD ',   &
                   (IXTIM(I),I=1,5), 'Z - ', (IXTIM(I),I=6,10), 'Z.'
        NEXTBLK = 0
        NTRIES = 0
      END IF IF_OLDDATA
    END IF IF_FETCH

!=======================================================================
!           LOOP OVER INDEX ENTRIES IN CURRENT INDEX BLOCK
!=======================================================================

    IPT0 = IXHED + (IENTRY-1)*INDLEN ! End of last entry read

DO_IDXENT: &
    DO JXE=IENTRY,NTRIES

      WANTED = .TRUE.  ! Until proved otherwise

      IF (LFLAG) WRITE (*,*) ' BUFRET: Index entry ', JXE

!-----------------------------------------------------------------------
! If message hasn't already been decoded, check whether it is wanted.
!-----------------------------------------------------------------------

IF_UNDECODED: &
      IF (.NOT.LMID) THEN
        IPT = IPT0 + IPDT(IVER)
!                                                      Earliest ob time
        IHOUR = ICHAR(NDXBLK(IPT  :IPT  ))
        IMIN  = ICHAR(NDXBLK(IPT+1:IPT+1))
        IDTIM1 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IHOUR) +&
             IMIN
!                                                        Latest ob time
        IHOUR = ICHAR(NDXBLK(IPT+3:IPT+3))
        IMIN  = ICHAR(NDXBLK(IPT+4:IPT+4))
        IDTIM2 = 60*DT2HRS(IXTIM(1),IXTIM(2),IXTIM(3),IHOUR) +&
           IMIN
!                                              Dates may need adjusting
!                                                  if period covers 00Z
        IF (IDTIM1 < IXTIM1) THEN
          IDTIM1 = IDTIM1 + 1440  ! add a day
          IDTIM2 = IDTIM2 + 1440  ! add a day
        END IF
        IF (IDTIM2 < IDTIM1) IDTIM2 = IDTIM2 + 1440

!-----------------------------------------------------------------------
! Reject bulletin if data falls entirely outside user's time window.
!-----------------------------------------------------------------------

!                  Data too early  or   Data too late
        IF (IDTIM2 < ITIM1 .OR. IDTIM1 > ITIM2) WANTED = .FALSE.
!
        IF (LFLAG) WRITE (*,*) ' BUFRET:   Bulletin wanted ? ',&
                WANTED, ',  Data time window is', IDTIM1, IDTIM2

!-----------------------------------------------------------------------
! Check platform identifier if required.
!-----------------------------------------------------------------------

IF_PLAT: &
        IF (WANTED .AND. NPLATS > 0) THEN
          WANTED = .FALSE.

!-----------------------------------------------------------------------
!          (1)  Index format 1:  Check integer satellite identifier.
!-----------------------------------------------------------------------

IF_VSN: &
          IF (IVER == 1) THEN
!                                          Get satellite id. from index
!
            IDSAT = MOD(ICHAR2(NDXBLK(IPT0+1:IPT0+2)),1024)
!
!                                   Check against list of wanted satids
            I = 1
            DO WHILE (.NOT.WANTED .AND. I <= NPLATS)
              IF (IDSAT == ISAT(I)) WANTED = .TRUE.
              I = I + 1
            END DO
!
            IF (LFLAG) WRITE (*,*) ' BUFRET: Satellite Id ',&
                    'check - IDSAT, WANTED = ', IDSAT, WANTED

!-----------------------------------------------------------------------
!          (2)  Index format 2:  Check character identifier.
!-----------------------------------------------------------------------

          ELSE IF (IVER == 2) THEN
!                                        Check against list of stations
            I = 1
            DO WHILE (.NOT.WANTED .AND. I <= NPLATS)
              IDLEN = INDEX(CHID(I),' ') - 1
              IF (IDLEN <= 0) IDLEN = 8
              IF (NDXBLK(IPT0+2:IPT0+IDLEN+1)  ==&
                       CHID(I)(1:IDLEN)) WANTED = .TRUE.
              I = I + 1
            END DO
!
            IF (LFLAG) WRITE (*,*) ' BUFRET: Station Id ',&
                    'check - Station, WANTED = ',         &
                      NDXBLK(IPT0+2:IPT0+9), WANTED
          END IF IF_VSN
        END IF IF_PLAT

!-----------------------------------------------------------------------
! Check data selection parameter if required.
!-----------------------------------------------------------------------

IF_SEL: &
        IF (WANTED .AND. NSEL > 0) THEN
          WANTED = .FALSE.
!                                          Get data selection parameter
          IPT = IPT0 + IPDS(IVER)
          IDSEL = ICHAR(NDXBLK(IPT:IPT))
!-----------------------------------------------------------------------
! For JMAWINDS data, the data selection parameter in the index entry
! is a composite number 60*n+ii where 'ii' is an integer derived from
! the buleltin header and included to overcome problems with duplicate
! data checking (see comments in STORBUFR source for details).
! Divide by 60 here to recover 'n' before checking.
!-----------------------------------------------------------------------
          IF (SUBTYPE == 'JMAWINDS') IDSEL = IDSEL/60
!
!                                   Check against list of wanted values
          I = 1
          DO WHILE (.NOT.WANTED .AND. I <= NSEL)
            IF (IDSEL == ISELECT(I)) WANTED = .TRUE.
            I = I + 1
          END DO
!
          IF (LFLAG) WRITE (*,*) ' BUFRET: Data selection ', &
                   'check - IDSEL, WANTED = ', IDSEL, WANTED
        END IF IF_SEL

!-----------------------------------------------------------------------
! Check areas if required. (Only done for non-rotated lat/long areas.)
!-----------------------------------------------------------------------

IF_AREA: &
        IF (WANTED .AND. AREA(1) == 0.0) THEN
          IPT = IPT0 + IPLL(IVER)

!-----------------------------------------------------------------------
!          (1)  Single observation. (Index version 2 with 1 ob.)
!-----------------------------------------------------------------------

IF_SINGLE: &
          IF (IVER == 2 .AND.                                 &
                  ICHAR2(NDXBLK(IPT0+20:IPT0+21)) == 1) THEN
            LATS = ICHAR2(NDXBLK(IPT  :IPT+1))    ! Latitude
            LONW = ICHAR2(NDXBLK(IPT+2:IPT+3))    ! Longitude
            IF (LATS >= 32768) LATS = LATS-65536  ! S. hem.
            IF (LONW >= 32768) LONW = LONW-65536  ! W. hem.
            OBLAT = 0.01*FLOAT(LATS)              ! REAL lat.
            OBLON = 0.01*FLOAT(LONW)              ! REAL lon.
!
!                                  Check for observation in user's AREA
!
!                      Ob. too far N    or   Ob. too far S
            IF (OBLAT > AREA(2) .OR. OBLAT < AREA(4)) THEN
              WANTED = .FALSE.
!                                           AREA doesn't cross dateline
!                           West < or = East
            ELSE IF (AREA(3) <= AREA(5)) THEN
              WANTED = OBLON >= AREA(3) .AND. &  ! Not too far W
                            OBLON <= AREA(5)       ! Not too far E
!
!                                                 AREA crosses dateline
            ELSE
              WANTED = OBLON >= AREA(3) .OR. & ! Not too far W
                           OBLON <= AREA(5)       ! Not too far E
            END IF

!-----------------------------------------------------------------------
!           (2) Observation lat/long box
!-----------------------------------------------------------------------

          ELSE
            LATS =   ICHAR(NDXBLK(IPT  :IPT  )) - 90  ! South
            LATN =   ICHAR(NDXBLK(IPT+1:IPT+1)) - 90  ! North
            LONW = 2*ICHAR(NDXBLK(IPT+2:IPT+2)) - 180 ! West
            LONE = 2*ICHAR(NDXBLK(IPT+3:IPT+3)) - 180 ! East

!                    Check for overlap between obs. box and user's area
!
            IF (FLOAT(LATS) > AREA(2) .OR. &  ! Obs too far N
                    FLOAT(LATN) < AREA(4)) THEN   ! Obs too far S
              WANTED = .FALSE.
!                                  Neither box crosses the dateline
!
            ELSE IF (LONW <= LONE .AND. AREA(3) <= AREA(5)) THEN
              WANTED = FLOAT(LONE) >= AREA(3) & ! Obs not too far W
                     .AND. FLOAT(LONW) <= AREA(5) ! Obs not too far E
!
!                                     Both boxes cross the dateline
!
            ELSE IF (LONW > LONE .AND. AREA(3) > AREA(5)) THEN
              WANTED = .TRUE.  ! Dateline is in both boxes
!
!                                      One box crosses the dateline
            ELSE
              WANTED = FLOAT(LONE) >= AREA(3)&! Obs not too far W
              .OR. FLOAT(LONW) <= AREA(5) ! Obs not too far E
            END IF
          END IF IF_SINGLE
!                                                              Printout
          IF (LFLAG) THEN
            WRITE (*,*)' BUFRET: Area requested ', (AREA(I),I=2,5)
            IF (IVER == 2 .AND.&
               ICHAR2(NDXBLK(IPT0+20:IPT0+21)) == 1) THEN
              WRITE (*,*)&
               ' BUFRET: Observation at:', OBLAT, OBLON
            ELSE
              WRITE (*,*) ' BUFRET: Data boundary  ',&
              LATS, LONE, LATN, LONW
            END IF
          END IF
        END IF IF_AREA

!-----------------------------------------------------------------------
! Check land/sea flag if required.
!-----------------------------------------------------------------------

        IF (WANTED .AND. ILAND > 0) THEN
          I = ICHAR(NDXBLK(IPT0+1:IPT0+1))/128 + 1
          IF (I /= ILAND) WANTED = .FALSE.
!                                                              Printout
          IF(LFLAG) WRITE (*,*)&
          ' BUFRET: Land/sea flags ', I, ILAND
        END IF

!-----------------------------------------------------------------------
! Time of receipt (T.O.R.) check if required.
!-----------------------------------------------------------------------

IF_MSGRQD1: &
        IF (WANTED .AND. (IRTIM1 > 0 .OR. IRTIM2 > 0)) THEN
          IPT = IPT0 + IPRT(IVER)
!  Get index T.O.R.
          ITOR = ICHAR2(NDXBLK(IPT:IPT+1))
          IF (LFLAG) WRITE (*,*) ' BUFRET: "ITOR" = ', ITOR
!
!  Check for data received too late
          IF (IRTIM2 /= 0) THEN
            IF (JXP+ITOR >= IRTIM2) WANTED = .FALSE.
          END IF
!  Check for data received too early
!
!            (ITOR=65535 may mean that the true value is larger so that
!           the check can't be done, so set ITOR to -1 to indicate that
!          a check against receipt time in message is to be done later)
!
          IF (IRTIM1 /= 0) THEN
            IF (ITOR >= 65535) THEN
              ITOR = -1
            ELSE
              IF (JXP+ITOR < IRTIM1) WANTED = .FALSE.
            END IF
          END IF
!    No T.O.R. test required
        ELSE
          ITOR = 0  ! To avoid TOR test later
        END IF IF_MSGRQD1

!=======================================================================
!     BULLETIN CHECKS COMPLETE: EXTRACT DATA IF MESSAGE IS WANTED
!=======================================================================

IF_MSGRQD2: &
        IF (WANTED) THEN
          I = IPT0 + INDLEN                ! End of index entry
          IBLOCK = ICHAR3(NDXBLK(I-6:I-4)) ! Data block number
          MSTART = ICHAR2(NDXBLK(I-3:I-2)) ! Message start byte
          MLEN   = ICHAR2(NDXBLK(I-1:I))   ! Message length
          MEND   = MSTART + MLEN - 1       ! Message end byte

          IF (LFLAG) WRITE (*,*) ' BUFRET: Data block ', IBLOCK,&
           ', Start, length, end = ', MSTART, MLEN, MEND

!                                       Read data block if not in store
!
          IF (IBLOCK /= NUMDAT) THEN
            CALL METDB_CREAD_DIR (IFT, DATBLK(1:IRLEN), IRLEN, &
                                  IBLOCK, IORC)
            NUMDAT = IBLOCK

            IF (LFLAG) WRITE (*,*) ' BUFRET: Read block ', NUMDAT
          END IF

!-----------------------------------------------------------------------
! Get BUFR edition (byte 8 of message) and offsets for sections 1 and 3.
!-----------------------------------------------------------------------

          BufrEdition = ICHAR(DATBLK(MSTART+7:MSTART+7))
!
!                                                  Offset for Section 1
          IF (BufrEdition <= 1) THEN
            I1 = MSTART + 3          ! BUFR edition 0 or 1
          ELSE
            I1 = MSTART + 7          ! BUFR edition 2 or more
          END IF
!                                                  Offset for Section 3
!
          I3 = I1 + ICHAR3(DATBLK(I1+1:I1+3))   ! Skip section 1
          I = ICHAR(DATBLK(I1+8:I1+8))          ! Section 1 flags
          IF (I >= 128) THEN                    ! Section 2 exists
            I3 = I3 + ICHAR3(DATBLK(I3+1:I3+3)) ! so skip that too
          END IF
!                                                              Printout
          IF (LFLAG) WRITE (*,*) ' BUFRET: BUFR edition ',&
              BufrEdition, ', I1, I3 = ', I1, I3

!-----------------------------------------------------------------------
! Extract quantities from BUFR section 1 or index entry (for VALARR).
!-----------------------------------------------------------------------
!                                                       Year of receipt
          IF (BufrEdition < 4) THEN
            I = I1 + 13
            IRCVD(1) = ICHAR(DATBLK(I:I)) ! Year of century

!                                              Convert year to 4-digits
            IRCVD(1) = IRCVD(1) + NCENT
            IF (IRCVD(1) > NOW(8)) IRCVD(1) = IRCVD(1) - 100
          ELSE
            IRCVD(1) = ICHAR2(DATBLK(I1+16:I1+17)) ! Year
            I = I1 + 17
          END IF
!                                      Receipt month, day, hour, minute

          IRCVD(2) = ICHAR(DATBLK(I+1:I+1)) ! T.O.R. month
          IRCVD(3) = ICHAR(DATBLK(I+2:I+2)) ! T.O.R. day
          IRCVD(4) = ICHAR(DATBLK(I+3:I+3)) ! T.O.R. hour
          IRCVD(5) = ICHAR(DATBLK(I+4:I+4)) ! T.O.R. minute

          IPT = IPT0 + IPDS(IVER)
          IRCVD(6) = ICHAR(NDXBLK(IPT:IPT)) ! Data selection

          IF (BufrEdition < 4) THEN
            IRCVD(7) = ICHAR(DATBLK(I1+5:I1+5))  ! Subcentre
          ELSE
            IRCVD(7) = ICHAR2(DATBLK(I1+7:I1+8)) ! Subcentre
          END IF

!-----------------------------------------------------------------------
! Additional date/time check if it couldn't be done earlier.
!-----------------------------------------------------------------------
!                                                    Get message T.O.R.
          IF (ITOR == -1) THEN
            I = 60*DT2HRS(IRCVD(1),IRCVD(2),IRCVD(3),IRCVD(4)) +&
                IRCVD(5)
!                                     Reject if < 'received after' time
            IF (I < IRTIM1) THEN
              WANTED = .FALSE.
              IF (LFLAG) WRITE (*,*) ' BUFRET: TOR too early ', I
            END IF
          END IF
        END IF IF_MSGRQD2

!-----------------------------------------------------------------------
! Get the local D sequence number to identify the message type.
! (Not done if data not wanted or if retrieving BUFR messages.)
! BUFR message is rejected if there is no local D sequence.
!-----------------------------------------------------------------------

IF_MSGRQD3: &
        IF (WANTED .AND. .NOT.FOUND(34)) THEN
          ILOCD = ICHAR2(DATBLK(I3+8:I3+9))
          CALL DESFXY (ILOCD, F, XX, YYY)
          WRITE (LOCD,'(I1,I2.2,I3.3)') F, XX, YYY

!                    Check sequence exists
IF_SEQ: &
          IF (F /= 3) THEN  ! No sequence
            WRITE (*,'(T2,2A)') 'BUFRET: No sequence ' //&
            'descriptor - first descriptor is ', LOCD
            WANTED = .FALSE.  ! Reject message

!-----------------------------------------------------------------------
! Call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

          ELSE
            CALL BUFINDX (CINDX, IDESC, NDES, ILVL, QCREQ, IFAIL, &
               LFLAG, LOCD, DATBLK(MSTART:MEND), NELREQ,          &
               DISPL, SOURCE, VALUES, MDATA, CNAME,               &
               IEXTRA, NOBS1, MDES, NEWCALL, MEXT)

            IF (IFAIL == 16) THEN   ! Element index not found
              IERR = 16
              RETURN
            END IF

            IF (NOBS1 <= 0) WANTED = .FALSE. ! No obs decoded
          END IF IF_SEQ
        END IF IF_MSGRQD3
      END IF IF_UNDECODED

IF_MSGRQD4: &
      IF (WANTED) THEN

!-----------------------------------------------------------------------
! Retrieve BUFR messages (FOUND(34) is true):  Copy data to CREP array.
!-----------------------------------------------------------------------

IF_RETBUFR: &
        IF (FOUND(34)) THEN
!                                 Check that CREPT can hold the message
!
IF_MSGLEN: &
          IF (MLEN > NOBS2*LENCREP) THEN
            WRITE (6,'(T2,2A,I7)') 'BUFRET: Message skipped ',&
            'as too big for CREP. Size =', MLEN
            LASTOB1 = MLEN  ! To skip message
!
!                    Check that free space in CREP can hold the message
!
          ELSE IF (MLEN <=&
                        (NOBS2+1-LASTOB2)*LENCREP-LASTREP) THEN
!
!               If getting a new message, set pointer just before start
!
            IF (LASTOB1 == 0) LASTOB1 = MSTART - 1
!
!                             Transfer batches of data until either all
!                                 has been copied or CREP array is full
DO_NXTBAT: &
            DO WHILE (LASTOB1 < MEND .AND.&
                      (LASTREP < LENCREP .OR. LASTOB2 < NOBS2))
!
!                            If CREP element full, move to next element
!
              IF (LASTREP == LENCREP) THEN ! Full
                LASTOB2 = LASTOB2 + 1      ! Next element
                LASTREP = 0                ! It's empty
              END IF
!                            Find number of bytes to copy in next batch
!
!                           Min of (Bytes left,  Space in CREP)
              ICOPY = MIN0 (MEND-LASTOB1,LENCREP-LASTREP)
!
!                                Transfer batch of data to CREP element
!
              CREP(LASTOB2)(LASTREP+1:LASTREP+ICOPY) =&
                 DATBLK(LASTOB1+1:LASTOB1+ICOPY)
!
!                                             Update pointers and ARRAY
              LASTOB1 = LASTOB1 + ICOPY
              LASTREP = LASTREP + ICOPY
              ARRAY(LASTOB2,1) = FLOAT(LASTREP)
            END DO DO_NXTBAT
          END IF IF_MSGLEN
          NOBS1 = MEND ! so that LMID gets set OK below
        ELSE

!-----------------------------------------------------------------------
! Retrieve decoded elements: Call VALARR to put data into VALUES array.
!-----------------------------------------------------------------------

          IOB1 = LASTOB1 + 1
          IOB2 = LASTOB2
          CALL VALARR (DISPL, NELREQ, SOURCE, IRCVD,     &
          CDUMMY, IDUMMY, IEXTRA, VALUES, CNAME,         &
          '    1 ', IOB1, LASTOB1, NOBS1,                &
          ARRAY, NOBS2, NELEM, IOB2, LASTOB2, CSTR, CREP,&
          LFLAG, LATSUB, LONSUB, AREA, RPOLE)
        END IF IF_RETBUFR

        IF (LFLAG) WRITE (*,'(4(A,I5))') ' BUFRET: Obs',&
            IOB1, ' to', LASTOB1, ' copied. ',          &
            LASTOB2, ' obs in output array'

!=======================================================================
!         DATA EXTRACTION DONE:  CHECK IF READY TO RETURN NOW
!=======================================================================

!-----------------------------------------------------------------------
! Check whether all data has been transferred (LMID = .FALSE.) or not.
!-----------------------------------------------------------------------

        LMID = LASTOB1 < NOBS1

!-----------------------------------------------------------------------
! If user's ARRAY is full, update pointers and return.
!-----------------------------------------------------------------------

IF_FULLARR: &
        IF (LMID) THEN       ! User's array full
          NEXTIME = JXP      ! Next (= current) index period
          NEXTENT = JXE      ! Next (= current) index entry

          IOBNUM = LASTOB2   ! Last used slot in ARRAY
          ISTAT = 4          ! To indicate more data to come

          IF (LFLAG) WRITE (*,*)' BUFRET: User array filled - ',&
                       'NEXTIME, NEXTENT, IOBNUM = ',           &
                        NEXTIME, NEXTENT, IOBNUM
          RETURN

        ELSE                 ! All data from message transferred
          LASTOB1 = 0        ! Start from 1st ob next time

!-----------------------------------------------------------------------
! If retrieving 1 message at a time, update pointers and return.
! (It doesn't matter if NEXTENT is incremented to NTRIES+1.)
!-----------------------------------------------------------------------

          IF (LMSG .AND. LASTOB2 > 0) THEN ! return with 1 msg

            NEXTIME = JXP      ! Next (= current) index period
            NEXTENT = JXE + 1  ! Next index entry

            IOBNUM = LASTOB2   ! Last used slot in ARRAY
            ISTAT = 4          ! May be more data to come

            IF (LFLAG) WRITE (*,*) ' BUFRET: All obs copied - ',&
                'NEXTIME, NEXTENT, IOBNUM = ',                  &
                 NEXTIME, NEXTENT, IOBNUM
            RETURN
          END IF
        END IF IF_FULLARR
      END IF IF_MSGRQD4

!-----------------------------------------------------------------------
! End DO loops.
!-----------------------------------------------------------------------

      IPT0 = IPT0 + INDLEN  ! Update index block pointer
    END DO DO_IDXENT        ! JXE   End loop over index entries
!                                                      Next index block
!
    IF (NTRIES > 0) NEXTBLK = ICHAR3(NDXBLK(8:10))
    IENTRY = 1
  END DO DO_IDXBLK    ! End loop over index blocks
END DO DO_IDXPER      ! JXP  End loop over index periods

!=======================================================================
!                   END OF RETRIEVAL REQUEST.
!=======================================================================
! Set ISTAT = 0 (all data returned) or 8 (no data found).
!-----------------------------------------------------------------------

IOBNUM = LASTOB2   ! Last used slot in ARRAY
IF (.NOT.LCONT .AND. LASTOB2 == 0) THEN
  ISTAT = 8
ELSE
  ISTAT = 0
END IF
!                                                                Return
IERR = 0
IF (LFLAG) WRITE (*,*)&
     ' BUFRET: Request completed - IOBNUM = ', IOBNUM
RETURN
END SUBROUTINE BUFRET
