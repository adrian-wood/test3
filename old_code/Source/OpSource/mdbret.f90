SUBROUTINE MDBRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND, &
           ARRAY, NOBS2, NELEM, IOBNUM, CREQ, ISTAT, IDSK, &
           LMSG, LFLAG, CSTR, CREP, SUBTYPE, RPOLE, FOUND, &
           ELIST, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  MDBRET
!
!    Retrieval of MetDB data types stored in new-format storage data
!    sets.
!
! DESCRIPTION:
!
!    MDBRET searches through a MetDB storage data set retrieving data
!    as specified by the user's request string and transferring it to
!    the user's buffer ("ARRAY"). It is based on subroutine BUFRET but
!    uses new software written in 2010 which can handle storage data
!    sets with data record overflow and different BUFR sequences for
!    different bulletins.
!
!    Steps include:
!
!                 - Check ISTAT to see if it's a new retrieval request.
!                 - If so, set up pointers for loops and open the
!                     element index using OPENINDX.
!                 - Loop over index periods covering user's time window.
!                 - Loop over index blocks in each period.
!                 - Loop over index entries in each block.
!                 - Check details in index entry against:
!                    - user's data time window,
!                    - user's list of platform identifiers,
!                    - user's list of data selection values,
!                    - area
!                    - land/sea flag,
!                    - time of receipt window.
!                 - If message is still wanted:
!                    - read the data record(s) containing the message
!                    - decode the BUFR message
!                    - call DATAINDX to get displacements in message.
!                    - final check for observations wanted
!                    - Call COPYVALS to get required data values and
!                        transfer to user's buffer.
!                    - Return if (1) user's array has been filled or
!                        (2) messages requested one by one.
!                 - Return if retrieval request is complete.
!                 - Save pointers to pick up where left off next call.
!
! USAGE:  CALL MDBRET (ITIME, IRTIM, AREA, CHID, ISELECT, ILAND, ARRAY,
!              NOBS2, NELEM, IOBNUM, CREQ, ISTAT, IDSK, LMSG, LFLAG,
!              CSTR, CREP, SUBTYPE, RPOLE, FOUND, ELIST, ICODE)
!
! ARGUMENTS:
!
!   No.  Arg.  I/O Type  Size   Contents
!   --  -----  --- ----  ----   --------
!    1  ITIME   I   I4     8   User data time window (y,m,d,hhmm)*2.
!    2  IRTIM   I   I4    10   User receipt time window (y,m,d,h,m)*2.
!    3  AREA    I   R4     5   User-defined area specification.
!    4  CHID    I   C9    50   List of wanted platform IDs.
!    5  ISELECT I   I4    50   List of wanted data selection vals.
!    6  ILAND   I   I4         User's land/sea indicator (1 for land,
!                                2 for sea, 0 for both).
!    7  ARRAY   O   R4 (NOBS2, User's array of decoded elements.
!                       NELEM)   (NELEM elements for NOBS2 obs.)
!    8  NOBS2   I   I4         First dimension of ARRAY (no. of obs).
!    9  NELEM   I   I4         2nd dimension of ARRAY (no. of elmnts).
!   10  IOBNUM I/O  I4         (Input) Next unused ob. slot in ARRAY,
!                              (Output) Last used ob. slot in ARRAY.
!   11  CREQ    C  C500        User's retrieval request string
!   12  ISTAT  I/O  I4         MetDB status indicator:
!                                (0 = new request, 4 = more data to
!                                come, 8 = complete, 16 = error).
!   13  IDSK    I   I4     5   Storage data set details (from DDICT)
!                                (only elements 2 and 3 used here).
!   14  LMSG    I   L4         Flag for one BUFR message at a time.
!   15  LFLAG   I   L4         Flag for generating diagnostic output.
!   16  CSTR    O   C*  NOBS2  Character strings from messages.
!   17  CREP    O   C*  NOBS2  Undecoded BUFR messages.
!   18  SUBTYPE I   C8         MetDB data subtype.
!   19  RPOLE   I   R4     2   True lat. and long. of rotated pole.
!   20  FOUND   I   L4     *   MetDB keywords (from GETREQ).
!   21  ELIST   I   C8         Element index member name
!   22  ICODE   O   I4         Return code (0 = OK, 16 = error).
!
! CALLED BY:  MDB
!
! CALLS:  COPYVALS, DATAINDX, DATIM, DEBUFR, DT2HRS, HRS2DT, ICHAR2,
!         ICHAR3, LOCALD, OPENINDX, WANTAREA, WANTOBS, WANTTIME
!
! REVISION INFO:
!
!    $Workfile: mdbret.f90$ $Folder: OpSource$
!    $Revision: 16$ $Date: 20/02/2012 11:33:52$
!
! CHANGE RECORD:
!
! $Log:
!  16   MetDB_Refresh 1.15        20/02/2012 11:33:52    Sheila Needham  Remove
!        allocatable CNAM which uses an f2003 extension.
!  15   MetDB_Refresh 1.14        30/01/2012 18:36:53    Sheila Needham
!       Increase array sizes for KMAWINDS and other large bulletins.
!  14   MetDB_Refresh 1.13        30/01/2012 11:24:34    Sheila Needham
!       Increase array sizes + dynamic allocation
!  13   MetDB_Refresh 1.12        22/08/2011 15:50:25    Brian Barwell
!       Identifier checking modified and WANTID introduced.
!  12   MetDB_Refresh 1.11        26/04/2011 15:33:38    Brian Barwell
!       Correct first argument of call to WANTAREA.
!  11   MetDB_Refresh 1.10        05/04/2011 14:10:37    Alison Weir     Read
!       storage datasets with C routine.
!  10   MetDB_Refresh 1.9         20/12/2010 13:05:32    Sheila Needham
!       Initialise ICODE
!  9    MetDB_Refresh 1.8         13/12/2010 14:34:12    Brian Barwell   Change
!        DATBLK from a CHARACTER*27998 array to a string of length
!       500000+MAXBLK.
!  8    MetDB_Refresh 1.7         07/12/2010 10:11:09    Brian Barwell   Print
!       correct number of elements for ITIME in test printout.
!  7    MetDB_Refresh 1.6         30/11/2010 10:02:36    Brian Barwell   Delete
!        2 characters which slipped in accidentally last time.
!  6    MetDB_Refresh 1.5         30/11/2010 09:54:25    Brian Barwell   Change
!        to first argument in call to WANTTIME.
!  5    MetDB_Refresh 1.4         29/11/2010 14:47:21    Brian Barwell   Delete
!        function declarations.
!  4    MetDB_Refresh 1.3         29/11/2010 14:17:33    Brian Barwell
!       Uncomment USE statements. Check for missing time tag before calling
!       DT2HRS.
!  3    MetDB_Refresh 1.2         22/11/2010 17:23:18    Stan Kellett    CDUMMY
!        changed from array of dimension 1 to scalar
!       calls to routine dataindx changes to arguments passed.
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE copyvals_mod
USE dataindx_mod
USE datim_mod
USE debufr_mod
USE dt2hrs_mod
USE hrs2dt_mod
USE ichar2_mod
USE ichar3_mod
USE locald_mod
USE openindx_mod
USE wantarea_mod
USE wantid_mod
USE wantobs_mod
USE wanttime_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(IN)     ::  ITIME(8)   ! User's data time window (y,m,d,hhmm)*2
INTEGER,      INTENT(IN)     ::  IRTIM(10)  ! User's TOR window limits (y,m,d,h,m)*2
REAL,         INTENT(IN)     ::  AREA(5)    ! User defined area specification
CHARACTER(9), INTENT(IN)     ::  CHID(50)   ! User's list of platform IDs
INTEGER,      INTENT(IN)     ::  ISELECT(50) ! User's list of data selection values
INTEGER,      INTENT(IN)     ::  ILAND      ! User land/sea indicator
INTEGER,      INTENT(IN)     ::  NOBS2      ! Number of decoded obs. ARRAY can hold
INTEGER,      INTENT(IN)     ::  NELEM      ! User's no. of elements
REAL,         INTENT(OUT)    ::  ARRAY(NOBS2,NELEM) ! User's array to hold decoded data
INTEGER,      INTENT(INOUT)  ::  IOBNUM     ! (In/Out) Next/last ob. slot in ARRAY
CHARACTER(*), INTENT(INOUT)  ::  CREQ       ! User's request string
INTEGER,      INTENT(INOUT)  ::  ISTAT      ! MetDB status indicator
INTEGER,      INTENT(IN)     ::  IDSK(5)    ! Dataset details
LOGICAL,      INTENT(IN)     ::  LMSG       ! TRUE if retrieving messages one by one
LOGICAL,      INTENT(IN)     ::  LFLAG      ! TRUE to produce diagnostic printout
CHARACTER(*), INTENT(OUT)    ::  CSTR(NOBS2)! Character strings from message
CHARACTER(*), INTENT(OUT)    ::  CREP(NOBS2)! Undecoded BUFR message
CHARACTER(8), INTENT(IN)     ::  SUBTYPE    ! MetDB data subtype
REAL,         INTENT(IN)     ::  RPOLE(2)   ! Rotated pole lat. & long. co-ordinates
LOGICAL,      INTENT(IN)     ::  FOUND(*)   ! Flags for MetDB keywords selected
CHARACTER(8), INTENT(IN)     ::  ELIST      ! Element index member name
INTEGER,      INTENT(OUT)    ::  ICODE      ! Return code

! Local declarations:
!                                                            Parameters
INTEGER, PARAMETER  ::  IDHED  = 8        ! Length of data block header
INTEGER, PARAMETER  ::  IXHED  = 10       ! Length of index block header
INTEGER, PARAMETER  ::  MAXBLK = 27998    ! Maximum block size of dataset
INTEGER, PARAMETER  ::  MAXDES = 60000    ! Size of IRDES array
INTEGER, PARAMETER  ::  MAXOBS = 20000    ! Maximum obs per bulletin (size of WANTOB)
INTEGER, PARAMETER  ::  MCHAR  = 27998    ! Maximum length of CNAME string
INTEGER, PARAMETER  ::  MDATA  = 3000000  ! Maximum size of VALUES array
INTEGER, PARAMETER  ::  MDES   = 50000    ! Maximum number of descriptors
INTEGER, PARAMETER  ::  MAXDAT = 500000+MAXBLK ! Size of DATBLK array

!                                                              Integers
!                  (See also note at end for different meanings of
!                   some variables when retrieving BUFR messages.)
!
INTEGER      ::  BufrEdition ! BUFR edition number
INTEGER,ALLOCATABLE ::  DISPL(:) ! Displacements from BUFINDX
INTEGER      ::  I, J ! Integer variables for local use
INTEGER      ::  IBLOCK ! Data block number to read
INTEGER      ::  ICOPY ! No. of bytes to copy (BUFR retrievals)
INTEGER      ::  IDSEL ! Data selection byte in index
INTEGER      ::  IDUMMY(1) ! Dummy argument for LOCALD
INTEGER      ::  IENTRY ! First index entry to process this call
INTEGER      ::  IFT  ! Unit number of storage data set
INTEGER      ::  INDLEN ! Length of index entry
INTEGER      ::  INT4   ! Dummy variable for TRANSFER function
INTEGER      ::  IOVFL ! Number of overflow data records
INTEGER      ::  IORC    ! Return code from C read routine
INTEGER      ::  IPDS(2) ! Pointers to 'data selection' in index
INTEGER      ::  IPRT(2) ! Pointers to receipt time in index
INTEGER      ::  IPT0 ! Pointer to before start of index entry
INTEGER      ::  IPT  ! Pointer to a location in index entry
INTEGER      ::  IPTIM1 ! First index time to process this call
INTEGER      ::  IPTIM2 ! Last index time containing user's data
INTEGER      ::  IRCVD(7) ! Data from BUFR section 1 (for VALARR)
INTEGER,ALLOCATABLE::  IRDES(:) ! Descriptor array from BUFR decode
INTEGER      ::  IRLEN ! Record length of storage data set
INTEGER      ::  IRTIM1 ! User's start TOR in century minutes
INTEGER      ::  IRTIM2 ! User's end TOR in century minutes
INTEGER      ::  ISAT(10) ! User's list of satellite identifiers
INTEGER      ::  ITIM1 ! User's start time in century minutes
INTEGER      ::  ITIM2 ! User's end time in century minutes
INTEGER      ::  ITOR ! Index entry TOR in minutes after 00Z
INTEGER      ::  IXTIM(10) ! Current index period (y,m,d,h,m)*2
INTEGER      ::  IXTIM1 ! Start of index period (century minutes)
INTEGER      ::  IVER ! Format version numbers for storage d.s.
INTEGER      ::  I1, I3 ! Offsets of sections 1 & 3 in message
INTEGER      ::  JXE  ! Loop variable for index entries
INTEGER      ::  JXP  ! Loop variable for index periods
INTEGER      ::  KEEP(3) ! Bulletin flags for time, area, Id
INTEGER      ::  LASTREP ! No. of last byte used in CREP element
INTEGER      ::  LASTOB1 ! No. of last ob. copied from VALUES
INTEGER      ::  LASTOB2 ! No. of last ob. position used in ARRAY
INTEGER      ::  LDISP(16) ! Displacements for selected data
INTEGER      ::  LENCREP ! Length of elements of CREP array
INTEGER      ::  MEND ! Message end byte number in data block
INTEGER      ::  MLEN ! Length of message in bytes
INTEGER      ::  MSTART ! Message start byte number in data block
INTEGER      ::  NCENT ! Start year of current century (e.g. 2000)
INTEGER      ::  NDISP ! Number of elements of IDISP used
INTEGER      ::  NEXTBLK ! Next index block to look at
INTEGER      ::  NEXTENT ! Next index entry in block to look at
INTEGER      ::  NEXTIME ! Next index time to look at
INTEGER      ::  NMBLK ! No. of map blocks in storage data set
INTEGER      ::  NOBS1 ! Number of decoded obs. in VALUES array
INTEGER      ::  NOW(8) ! Current time (from DATIM)
INTEGER      ::  NPLATS ! Number of user's platform identifiers
INTEGER      ::  NRDES ! Number of descriptors returned from decode
INTEGER      ::  NSEL ! Number of user's data selection vals.
INTEGER      ::  NTRIES ! No. of index entries in index block
INTEGER      ::  NUMDAT ! Block number of data block in store
INTEGER      ::  NUMDES ! Number of descriptors in BUFR message
INTEGER      ::  NUMNDX ! Block number of index block in store
INTEGER      ::  NUMREC ! Block number
INTEGER      ::  NXBLK ! No. of index blocks in storage data set
INTEGER      ::  NXMIN ! No. of minutes in index period
INTEGER      ::  NX00Z ! Index period offset from 00Z (minutes)

! Note: For retrievals in BUFR, LASTOB1 refers to a byte number
!       in a data record rather than an ob. number in a message.
!       Also LASTOB2 and OBNUM refer to elements of CREP rather
!       than the number of obs. whose data has been put in VALUES.

!                                                                 Reals
REAL,ALLOCATABLE :: VALUES(:) ! Array of values decoded from message

!                                                              Logicals
LOGICAL         ::  FIRST  ! TRUE if first call to MDBRET
LOGICAL         ::  LCONT  ! TRUE if continuing retrieval request
LOGICAL         ::  LMID   ! TRUE if part of message has been passed
LOGICAL         ::  WANTED ! TRUE if current message is wanted
LOGICAL(kind=1) :: WANTOB(MAXOBS)! TRUE if current observation is wanted

!                                                            Characters
CHARACTER(MAXBLK) ::  BLOCK   ! Block read from storage d.s.
CHARACTER(1)      ::  CDUMMY  ! Dummy character String
CHARACTER(MCHAR)  ::  CNAME   ! Characters from BUFR decode
CHARACTER(MAXDAT) ::  DATBLK  ! Data read from storage d.s.
CHARACTER(IDHED)  ::  DATHED  ! Data record header
CHARACTER(MAXBLK) ::  NDXBLK  ! Index block read from storage d.s.

!-----------------------------------------------------------------------
! Common blocks (for dynamic allocation - compile with FPARMS='DC(*)').
!-----------------------------------------------------------------------

COMMON /MDBCOM2/ NDXBLK, DATBLK
COMMON /MDBCOM3/ WANTOB

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

SAVE ! Everything

!-----------------------------------------------------------------------
! Data statements.
!-----------------------------------------------------------------------

DATA FIRST /.TRUE./     ! First call to MDBRET
DATA NEXTBLK, NEXTIME, NEXTENT /3*0/
DATA IPDS /15,22/, IPRT /18,25/

!-----------------------------------------------------------------------
! First call only:  revision information and current date & time.
!-----------------------------------------------------------------------

IF (.NOT.ALLOCATED(VALUES))ALLOCATE(VALUES(MDATA))
IF (.NOT.ALLOCATED(DISPL))ALLOCATE(DISPL(MDES))
IF (.NOT.ALLOCATED(IRDES))ALLOCATE(IRDES(MAXDES))

IF (FIRST) THEN
!                                             Get current date and time
  CALL DATIM (NOW)
  NCENT = NOW(8) - MOD(NOW(8),100)
  FIRST = .FALSE.

END IF
!                                                   Diagnostic printout
IF (LFLAG) THEN
  WRITE (*,'(/A/A/2A)') ' In MetDB subroutine MDBRET', &
                        ' ==========================', &
                        ' Data subtype = ', SUBTYPE
END IF
!                                                    Initialise LASTOB2
ICODE = 0
LASTOB2 = IOBNUM - 1  ! Last used ob slot in ARRAY

!=======================================================================
!             CHECK "ISTAT" AND SET UP LOOP COUNTERS.
!=======================================================================

!-----------------------------------------------------------------------
!     ISTAT=16:  Reset to 0 for new retrieval request.
!-----------------------------------------------------------------------

IF (ISTAT == 16) THEN
  IF (LFLAG) WRITE (*,*) ' MDBRET: New data set'
  ISTAT = 0
END IF

!-----------------------------------------------------------------------
!     ISTAT=4:  Continuation of existing request.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (ISTAT == 4) THEN
  LCONT  = .TRUE.    ! This is a continuation
  IPTIM1 = NEXTIME   ! Next index period to consider
  IENTRY = NEXTENT   ! Next index entry to look at
!                                                              Printout
  IF (LFLAG) THEN
    WRITE (*,*) ' MDBRET: Continuation of retrieval -', &
                ' IPTIM1, IENTRY =', IPTIM1, IENTRY
  END IF

!-----------------------------------------------------------------------
!     ISTAT=0:  New retrieval request.
!-----------------------------------------------------------------------

ELSE IF (ISTAT == 0) THEN
!                                                   Diagnostic printout

  IF (LFLAG) WRITE (*,*) ' MDBRET: "AREA" array - ', AREA

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
!                                                              Printout
  IF (LFLAG) WRITE (*,*) ' MDBRET: Data set header - ', &
        IVER, I, NMBLK, NXBLK, NXMIN, NX00Z, INDLEN
!                                                          Check format
IFLABEL2: &
  IF (IVER >= 65536 .OR. IVER < 0) THEN
    WRITE (*,'(T2,A)') &
             'MDBRET: CANNOT SUPPORT OLD STORAGE DATA SET FORMAT'
    ICODE = 16
    RETURN
!                                                 Check version numbers
  ELSE
    I = IVER/256          ! Data set format version
    IVER = MOD(IVER,256)  ! Index entry version
    IF (I /= 1 .OR. (IVER /= 1.AND.IVER /= 2)) THEN
      WRITE (*,'(T2,2A,2I4)') 'MDBRET: CANNOT SUPPORT ', &
               'DATA SET & INDEX FORMATS', I, IVER
      ICODE = 16
      RETURN
    END IF
  END IF IFLABEL2

!-----------------------------------------------------------------------
! Check for local table D entry in second record.
! (NDXBLK used for temporary storage.)
!-----------------------------------------------------------------------

  NUMREC = 2
  CALL METDB_CREAD_DIR (IFT, NDXBLK(1:IRLEN), IRLEN, NUMREC, IORC)

  IF (NDXBLK(1:1) == '3') THEN   ! Local sequence present
    IF (LFLAG) WRITE(*,*) ' MDBRET: Sequence descriptor ',NDXBLK(1:6)
    CALL LOCALD (0, 0, IDUMMY, IDUMMY(1), NDXBLK, 'NEW')
  END IF

!-----------------------------------------------------------------------
! A new data set has now been read so cancel the block numbers of index
! and data blocks held is store.
!-----------------------------------------------------------------------

  NUMNDX = 0  ! Index block number
  NUMDAT = 0  ! Data block number

!-----------------------------------------------------------------------
! Open the element index
!-----------------------------------------------------------------------
!                                                     Open the data set
  CALL OPENINDX (ELIST, ICODE)
!                                                     Check return code
  IF (ICODE > 0) THEN
    WRITE (*,'(T4,A,I4)') 'RETURN CODE FROM "OPENINDX" IS', ICODE
    ICODE = 16
    RETURN
  ELSE IF (ICODE == -1) THEN
    J = 0    ! No need to read element index
  ELSE
    J = 81   ! Unit number for element index
  END IF

!-----------------------------------------------------------------------
! Read element index and decode element names in user's request string
!-----------------------------------------------------------------------

  I = INDEX(CREQ,'ELEMENTS') + 9  ! 1st byte after 'ELEMENTS'
  CALL DATAINDX (J, CDUMMY,0, IRDES,0, CREQ,I, DISPL,IDUMMY(1), ICODE)

  IF (ICODE > 99) THEN
    WRITE (*,'(T4,A,I4)') 'RETURN CODE FROM "DATAINDX" IS', ICODE
    ICODE = 16
    RETURN
  END IF

!-----------------------------------------------------------------------
! For index format 1, convert platform identifiers to satellite IDs.
!-----------------------------------------------------------------------
!                                   Count number of platforms specified
  NPLATS = 0
  DO WHILE (NPLATS < 50 .AND. CHID(NPLATS+1) /= '00000    ')
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

  ITIM1 = 60*DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4)/100) + &
          MOD(ITIME(4),100)
!                                            End time (if notspecified)
  IF (ITIME(6) == 0) THEN
    ITIM2 = ITIM1     ! = Start time
  ELSE
    ITIM2 = 60*DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8)/100) + &
            MOD(ITIME(8),100)
  END IF

!-----------------------------------------------------------------------
! Convert time of receipt limits to century minutes.
!-----------------------------------------------------------------------
!                                                 'Received after' time
  IF (IRTIM(1) == 0) THEN
    IRTIM1 = 0
  ELSE
    IRTIM1 = 60*DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4)) + IRTIM(5)
  END IF
!                                                'Received before' time
  IF (IRTIM(6) == 0) THEN
    IRTIM2 = 0
  ELSE
    IRTIM2 = 60*DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9)) + IRTIM(10)
  END IF

!-----------------------------------------------------------------------
! Century minutes of index periods to look through.
!-----------------------------------------------------------------------

  IPTIM1 = ITIM1 - MOD((ITIM1-NX00Z),NXMIN) ! First index period
  IPTIM2 = ITIM2 - MOD((ITIM2-NX00Z),NXMIN) ! Last index period

  IF (LFLAG) THEN
    WRITE (*,*) ' MDBRET: User start time ', (ITIME(I),I=1,4)
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
END IF IFLABEL1

ISTAT = 0

!=======================================================================
!    INITIALISE OUTPUT ARRAYS AND POINTERS FOR RETRIEVALS IN BUFR
!=======================================================================

!                                 Clear out CREP and ARRAY if necessary
!
IFLABEL3: &
IF (FOUND(34) .AND. (LMSG.OR.LASTOB2 <= 0)) THEN
  LENCREP = LEN(CREP(1))  ! Length of elements of CREP
  LASTOB2 = 0             ! Return to start of user's ARRAY
  LASTREP = LENCREP       ! Put new data in new CREP element
!
!                                                  Reset CREP and ARRAY
  DO I=1,LENCREP          ! First element
    CREP(1)(I:I) = CHAR(0)
  END DO ! I
  ARRAY(1,1) = 0.0
!
  DO I=2,NOBS2            ! Other elements
    CREP(I) = CREP(1)
    ARRAY(I,1) = 0.0
  END DO ! I
END IF IFLABEL3

!=======================================================================
!       LOOP OVER INDEX PERIODS COVERING USER'S DATA TIME WINDOW
!=======================================================================

IF (LFLAG) WRITE (*,*) ' MDBRET: Loop over index periods ', &
                         IPTIM1, IPTIM2, NXMIN

DOLABEL1: &
DO JXP=IPTIM1,IPTIM2,NXMIN
!                                           Start from base index block
  IF (NEXTBLK <= 0) &
      NEXTBLK = MOD((JXP-NX00Z)/NXMIN,NXBLK) + NMBLK + 3

!=======================================================================
!            LOOP OVER INDEX BLOCKS IN CURRENT INDEX PERIOD
!=======================================================================

DOLABEL2: &
  DO WHILE (NEXTBLK > 0)
!                                      Read index block if not in store
IFLABEL4: &
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

!                                                     Number of entries
      NTRIES = ICHAR2(NDXBLK(6:7))

      IF (LFLAG) WRITE (*,*) ' MDBRET: Index block ', NUMNDX, &
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
IFLABEL5: &
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

        WRITE (*,'(T2,3A,2(I4,2(''/'',I2.2),I3.2,'':'',I2.2,A))') &
              'NO ', SUBTYPE, ' DATA IN MDB FOR PERIOD ', &
              (IXTIM(I),I=1,5), 'Z - ', (IXTIM(I),I=6,10), 'Z.'
        NEXTBLK = 0
        NTRIES = 0
      END IF IFLABEL5
    END IF IFLABEL4

!=======================================================================
!           LOOP OVER INDEX ENTRIES IN CURRENT INDEX BLOCK
!=======================================================================

    IPT0 = IXHED + (IENTRY-1)*INDLEN ! End of last entry read
DOLABEL3: &
    DO JXE=IENTRY,NTRIES

      WANTED = .TRUE.  ! Until proved otherwise

      IF (LFLAG) WRITE (*,*) ' MDBRET: Index entry ', JXE

!-----------------------------------------------------------------------
! If message hasn't already been decoded, check whether it is wanted.
!-----------------------------------------------------------------------

IFLABEL6: &
      IF (.NOT.LMID) THEN

!-----------------------------------------------------------------------
! Check observation times against requested time window
!-----------------------------------------------------------------------

        CALL WANTTIME (NDXBLK(IPT0+1:IPT0+INDLEN), IVER, IXTIM1, &
                       ITIM1, ITIM2, KEEP(1))
        IF (KEEP(1) < 0) WANTED = .FALSE.

!-----------------------------------------------------------------------
! Check platform identifier if required.
!-----------------------------------------------------------------------

IFLABEL7: &
        IF (WANTED .AND. NPLATS > 0) THEN
          CALL WANTID (NDXBLK(IPT0+1:IPT0+INDLEN),         &
                       IVER, ISAT, CHID, NPLATS, KEEP(3))
          IF (KEEP(3) < 0) WANTED = .FALSE.
!                                                   Diagnostic printout
          IF (LFLAG) THEN
            IF (IVER == 1) THEN
              I = MOD(ICHAR2(NDXBLK(IPT0+1:IPT0+2)),1024)
              WRITE (*,*) ' MDBRET: Satellite Id check - ',  &
                          'Sat.ID, WANTED = ', I, WANTED
            ELSE IF (IVER == 2) THEN
              WRITE (*,*) ' MDBRET: Station Id check - ',    &
                          'Station, WANTED = ',              &
                          NDXBLK(IPT0+2:IPT0+9), WANTED
            END IF
          END IF

        ELSE
          KEEP(3) = 1
        END IF IFLABEL7

!-----------------------------------------------------------------------
! Check data selection parameter if required.
!-----------------------------------------------------------------------

IFLABEL9: &
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
          IF (LFLAG) WRITE (*,*) ' MDBRET: Data selection ', &
               'check - IDSEL, WANTED = ', IDSEL, WANTED
        END IF IFLABEL9

!-----------------------------------------------------------------------
! Check areas if required.
!-----------------------------------------------------------------------

        IF (WANTED .AND. FOUND(7)) THEN
          CALL WANTAREA &
               (NDXBLK(IPT0+1:IPT0+INDLEN), IVER, AREA, RPOLE, KEEP(2))
          IF (KEEP(2) < 0) WANTED = .FALSE.
        ELSE
          KEEP(2) = 1
        END IF

!-----------------------------------------------------------------------
! Check land/sea flag if required.
!-----------------------------------------------------------------------

        IF (WANTED .AND. ILAND > 0) THEN
          I = ICHAR(NDXBLK(IPT0+1:IPT0+1))/128 + 1
          IF (I /= ILAND) WANTED = .FALSE.
!                                                   Diagnostic printout

          IF(LFLAG) WRITE (*,*) ' MDBRET: Land/sea flags ', I, ILAND
        END IF

!-----------------------------------------------------------------------
! Time of receipt (T.O.R.) check if required.
!-----------------------------------------------------------------------

IFLABEL10: &
        IF (WANTED .AND. (IRTIM1 > 0 .OR. IRTIM2 > 0)) THEN
          IPT = IPT0 + IPRT(IVER)
!                                                      Get index T.O.R.
          ITOR = ICHAR2(NDXBLK(IPT:IPT+1))
          IF (LFLAG) WRITE (*,*) ' MDBRET: "ITOR" = ', ITOR
!
!                                      Check for data received too late
          IF (IRTIM2 /= 0) THEN
            IF (JXP+ITOR >= IRTIM2) WANTED = .FALSE.
          END IF
!                                     Check for data received too early
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
!                                               No T.O.R. test required
        ELSE
          ITOR = 0  ! To avoid TOR test later
        END IF IFLABEL10

!=======================================================================
!     BULLETIN CHECKS COMPLETE: EXTRACT DATA IF MESSAGE IS WANTED
!=======================================================================

IFLABEL11: &
        IF (WANTED) THEN
          I = IPT0 + INDLEN                ! End of index entry
          IBLOCK = ICHAR3(NDXBLK(I-6:I-4)) ! Data block number
          MSTART = ICHAR2(NDXBLK(I-3:I-2)) ! Message start byte
          IOVFL  = ICHAR(NDXBLK(I-9:I-9))  ! Overflow data blocks
          MLEN   = ICHAR2(NDXBLK(I-1:I))   ! Message length
          MEND   = MSTART + MLEN - 1       ! Message end byte

!           The following statement allows for a bug affecting data
!           stored on 14-15 June 2011 when the data block overflow byte
!           was accidentally set to a space (hex '40' = decimal '64').

          IF (IOVFL.EQ.64) IOVFL = 0

          IF (LFLAG) WRITE (*,*) ' MDBRET: Data block ', IBLOCK, &
                  ', Start, length, end = ', MSTART, MLEN, MEND

!                                       Read data block if not in store
IFLABEL12: &
          IF (IOVFL == 0) THEN
            IF (IBLOCK /= NUMDAT) THEN
              CALL METDB_CREAD_DIR (IFT, DATBLK(1:IRLEN), IRLEN, &
                                    IBLOCK, IORC)
              NUMDAT = IBLOCK

              IF (LFLAG) WRITE(*,*) ' MDBRET: Read block ',NUMDAT
            END IF

          ELSE
            I = MEND + IOVFL*(IRLEN-IDHED)
            IF (I > MAXDAT) THEN
              WRITE (*,'(T2,2A,I3)') 'MDBRET: Message skipped ', &
                   'as too big for DATBLK. Overflows =', IOVFL
              GO TO 99  ! to skip processing for this message
            END IF

            CALL METDB_CREAD_DIR (IFT, DATBLK(1:IRLEN), IRLEN, &
                                  IBLOCK, IORC)
            IBLOCK = ICHAR3(DATBLK(MSTART:MSTART+3))
            MSTART = MSTART + 3
            MLEN = MLEN - 3
            NUMDAT = 0

            I = IRLEN - IDHED
            DO J=1,IOVFL
              CALL METDB_CREAD_DIR (IFT, BLOCK(1:IRLEN), IRLEN, &
                                    IBLOCK, IORC)
              DATHED = BLOCK(1:8)
              DATBLK(MEND+1:MEND+I) = BLOCK(9:I+8)
              MLEN = MLEN + I
              MEND = MEND + I
              IBLOCK = ICHAR3(DATHED(6:8))
            END DO
          END IF IFLABEL12

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
          I3 = I1 + ICHAR3(DATBLK(I1+1:I1+3)) ! Skip section 1
          I = ICHAR(DATBLK(I1+8:I1+8))       ! Section 1 flags
          IF (I >= 128) &                       ! Section 2 exists
            I3 = I3 + ICHAR3(DATBLK(I3+1:I3+3)) ! so skip it

!                                                              Printout
          IF (LFLAG) WRITE (*,*) ' MDBRET: BUFR edition ', &
                     BufrEdition, ', I1, I3 = ', I1, I3

!-----------------------------------------------------------------------
! Extract quantities from BUFR section 1 or index entry
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
            IRCVD(7) = ICHAR(DATBLK(I1+5:I1+5)) ! Subcentre
          ELSE
            IRCVD(7) = ICHAR2(DATBLK(I1+7:I1+8)) ! Subcentre
          END IF

!-----------------------------------------------------------------------
! Additional date/time check if it couldn't be done earlier.
!-----------------------------------------------------------------------
!                                                    Get message T.O.R.
          IF (ITOR == -1) THEN
            I = 60*DT2HRS(IRCVD(1),IRCVD(2),IRCVD(3),IRCVD(4)) + &
                IRCVD(5)
!                                     Reject if < 'received after' time
            IF (I < IRTIM1) THEN
              WANTED = .FALSE.
              IF (LFLAG) WRITE (*,*) ' MDBRET: TOR too early ', I
            END IF
          END IF
        END IF IFLABEL11

!-----------------------------------------------------------------------
! Decode the BUFR message and compute displacements for required data.
! (Not done if data not wanted or if retrieving undecoded messages.)
!-----------------------------------------------------------------------

IFLABEL13: &
        IF (WANTED .AND. .NOT.FOUND(34)) THEN
!                                                    Decode the message
          NRDES = MAXDES
          NOBS1 = MDATA
          CALL DEBUFR (IRDES, VALUES, CNAME, NRDES, NOBS1, &
                       DATBLK(MSTART:), .FALSE.)

!                            Get the number of descriptors in section 3

          NUMDES = (ICHAR3(DATBLK(I3+1:I3+3))-7)/2
          NDISP = MDES
!                              Compute displacements of required values

          CALL DATAINDX (0, DATBLK(I3+8:), NUMDES, IRDES, &
                         NRDES, CREQ, 0, DISPL, NDISP, ICODE)
IFLABEL14: &
          IF (ICODE == 0) THEN

!                       Check that output array can hold NDISP elements

            IF (NELEM < NDISP) THEN
              WRITE (*,'(2A)') 'MDBRET: MDB ERROR - ', &
                       'ARRAY NOT LARGE ENOUGH !'
              WRITE (*,'(T10,A,I4,A,I4,A)') &
                       'TRY ARRAY (', NOBS2, ',', NDISP, ')'
              ICODE = 16
              RETURN
            END IF
!                                Compute displacements of selected data

            IDUMMY(1) = -1 ! set to -1 as not changed but -1 is special value
            CALL DATAINDX (0, CDUMMY, 0, IRDES, 0, &
                          CREQ, 0, LDISP, IDUMMY(1), ICODE)
          END IF IFLABEL14
!                                                     Fatal error check
IFLABEL15: &
          IF (ICODE > 99) THEN
            WRITE (*,'(T4,A,I4)') &
                     'RETURN CODE FROM "DATAINDX" IS', ICODE
            ICODE = 16
            RETURN
!                                                 Non-fatal error check
          ELSE IF (ICODE > 0) THEN
            WANTED = .FALSE.

!                            Check for wanted observations if necessary
          ELSE
            CALL WANTOBS (VALUES, CNAME, NOBS1, LDISP, KEEP,     &
                 ITIME, AREA, RPOLE, CHID, NPLATS, WANTOB, ICODE)

!                                                           Error check
            IF (ICODE > 99) THEN
              WRITE (*,'(T4,A,I4)') &
                       'RETURN CODE FROM "WANTOBS" IS', ICODE
              ICODE = 16
              RETURN
            END IF
          END IF IFLABEL15
        END IF IFLABEL13
      END IF IFLABEL6

IFLABEL16: &
      IF (WANTED) THEN

!-----------------------------------------------------------------------
! Retrieve BUFR messages (FOUND(34) is true):  Copy data to CREP array.
!-----------------------------------------------------------------------

IFLABEL17: &
        IF (FOUND(34)) THEN
!                                 Check that CREPT can hold the message
IFLABEL18: &
          IF (MLEN > NOBS2*LENCREP) THEN
            WRITE (*,'(T2,2A,I7)') 'MDBRET: Message skipped ', &
                     'as too big for CREP. Size =', MLEN
            LASTOB1 = MEND  ! To skip message

!                    Check that free space in CREP can hold the message

          ELSE IF (MLEN <= (NOBS2+1-LASTOB2)*LENCREP-LASTREP) THEN

!               If getting a new message, set pointer just before start

            IF (LASTOB1 == 0) LASTOB1 = MSTART - 1

!                             Transfer batches of data until either all
!                                 has been copied or CREP array is full
DOLABEL4: &
            DO WHILE (LASTOB1 < MEND .AND. &
                 (LASTREP < LENCREP .OR. LASTOB2 < NOBS2))

!                            If CREP element full, move to next element

              IF (LASTREP == LENCREP) THEN ! Full
                LASTOB2 = LASTOB2 + 1      ! Next element
                LASTREP = 0                ! It's empty
              END IF
!                            Find number of bytes to copy in next batch

!                           Min of (Bytes left,  Space in CREP)
              ICOPY = MIN0 (MEND-LASTOB1,LENCREP-LASTREP)

!                                Transfer batch of data to CREP element

              CREP(LASTOB2)(LASTREP+1:LASTREP+ICOPY) = &
                     DATBLK(LASTOB1+1:LASTOB1+ICOPY)

!                                             Update pointers and ARRAY
              LASTOB1 = LASTOB1 + ICOPY
              LASTREP = LASTREP + ICOPY
              ARRAY(LASTOB2,1) = FLOAT(LASTREP)
            END DO DOLABEL4
          END IF IFLABEL18
          NOBS1 = MEND ! so that LMID gets set OK below
        ELSE

!-----------------------------------------------------------------------
! Call COPYVALS to put required data into VALUES array
!-----------------------------------------------------------------------

          CALL COPYVALS (DISPL, WANTOB, NDISP, IRCVD, &
                         VALUES, CNAME, NOBS1, LASTOB1, &
                         ARRAY, CSTR, NOBS2, LASTOB2, ICODE)
!                                                           Error check
            IF (ICODE > 99) THEN
              WRITE (*,'(T4,A,I4)') &
                       'RETURN CODE FROM "COPYVALS" IS', ICODE
              ICODE = 16
              RETURN
            END IF
        END IF IFLABEL17

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

IFLABEL19: &
        IF (LMID) THEN       ! User's array full
          NEXTIME = JXP      ! Next (= current) index period
          NEXTENT = JXE      ! Next (= current) index entry

          IOBNUM = LASTOB2   ! Last used slot in ARRAY
          ISTAT = 4          ! To indicate more data to come

          IF (LFLAG) WRITE (*,*)' MDBRET: User array filled - ', &
                    'NEXTIME, NEXTENT, IOBNUM = ', &
                     NEXTIME, NEXTENT, IOBNUM
          RETURN

        ELSE                 ! All data from message transferred
          LASTOB1 = 0        ! Start from 1st ob next time

!-----------------------------------------------------------------------
! If retrieving 1 message at a time, update pointers and return.
! (It doesn't matter if NEXTENT is incremented to NTRIES+1.)
!-----------------------------------------------------------------------

IFLABEL20: &
          IF (LMSG .AND. LASTOB2 > 0) THEN ! return with 1 msg

            NEXTIME = JXP      ! Next (= current) index period
            NEXTENT = JXE + 1  ! Next index entry

            IOBNUM = LASTOB2   ! Last used slot in ARRAY
            ISTAT = 4          ! May be more data to come

            IF (LFLAG) WRITE (*,*) ' MDBRET: All obs copied - ', &
                      'NEXTIME, NEXTENT, IOBNUM = ', &
                       NEXTIME, NEXTENT, IOBNUM
            RETURN
          END IF IFLABEL20
        END IF IFLABEL19
      END IF IFLABEL16

!-----------------------------------------------------------------------
! End DO loops.
!-----------------------------------------------------------------------

   99       CONTINUE
      IPT0 = IPT0 + INDLEN  ! Update index block pointer
    END DO DOLABEL3 ! JXE   End loop over index entries
!                                                      Next index block
    IF (NTRIES > 0) NEXTBLK = ICHAR3(NDXBLK(8:10))
    IENTRY = 1
  END DO DOLABEL2 !         End loop over index blocks
END DO DOLABEL1 ! JXP       End loop over index periods

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
ICODE = 0
IF (ISTAT /= 4) THEN
  DEALLOCATE(VALUES)
  DEALLOCATE(DISPL)
  DEALLOCATE(IRDES)
END IF
IF (LFLAG) WRITE (*,*) ' MDBRET: Request completed - IOBNUM = ', IOBNUM
RETURN
END SUBROUTINE MDBRET
