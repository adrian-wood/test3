SUBROUTINE SSMRET(ITIME,IRTIM,AREA,ISAT,ILAND,ARRAY,NOBS,NELEM, &
                  IOBS,IDESC,NDES,ILVL,ISTAT,IERR,IDSK,LMSG,    &
                  LFLAG,QCREQ,CSTR,CREPT,SUBTYPE,NEWMDBCALL,    &
                  LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,FOUND,      &
                  ELIST)

!-----------------------------------------------------------------------
!
! subroutine    : SSMRET
!
! purpose       : Eventual replacement for SATRET. SSMRET is for the
!               : retrieval of all satellite data through in-line
!               : elements. First subtype = SSM-I
!
! description   : Loops over requested hours finding data blocks in
!               : index. loops over entries for an hour checking lat/
!               : lon, and satellite id. decodes relevant messages
!               : and finds displacements for required elements.
!               : transfers data to users array.
!
! data type(s)  : satellite data from BUFR datasets.
!
! called by     : MDB
!
! calls         : BUFINDX   - get BUFR displacements from message
!               : CENTURY   - assigns 2-fig year to a century
!               : DESFXY    - convert descriptor to F, XX, YYY
!               : DT2HRS    - convert date/time to century hours
!               : GETLOCD   - to read local seqs for SAT120 data
!               : HRS2DT    - convert century hours to date/time
!               : MAPRD     - to read records from dataset
!               : READIDX   - to read element index from dataset
!               : SUBSLOCD  - to find SAT120 seq desc.
!               : VALARR    - to put values into users array
!
! arguments     :
!
! ITIME(9)      : integer   (ip) : (1)  yyyy     start date
!                                : (2)  mm
!                                : (3)  dd
!                                : (4)  hh
!                                : (5)  yyyy     end date
!                                : (6)  mm
!                                : (7)  dd
!                                : (8)  hh
!                                : (9)  increment
! IRTIM(10)     : integer   (ip) : (1)  yyyy     earliest t.o.r
!                                : (2)  mm
!                                : (3)  dd
!                                : (4)  hh
!                                : (5)  min
!                                : (6)  yyyy     latest t.o.r
!                                : (7)  mm
!                                : (8)  dd
!                                : (9)  hh
!                                : (10) min
!
! AREA(5)       : real      (ip) : user defined area
! ISAT(10)      : integer   (ip) : list of satellite ids wanted
! ILAND         : integer   (ip) : 1 for land 2 for sea 0 for both
!
! ARRAY(NOBS,NELEM) : real  (op) : users data array
!
! NOBS          : integer  (iop) : number of soundings
! NELEM         : integer   (ip) : number of elements
! IOBS          : integer   (op) : no. of next ob to put in users array
! IDESC(NDES)   : integer   (ip) : array of in-line elem pointers
! NDES          : integer   (ip) : number of user in-line elements
! ILVL(NDES)    : integer   (ip) : number of replications of each elem
! ISTAT         : integer  (iop) : MetDB status indicator
! IERR          : integer   (op) : error indicator
! IDSK(5)       : integer   (ip) : storage dataset details
! LMSG          : logical   (ip) : true for one bufr msg at a time
! LFLAG         : logical   (ip) : true for diagnostic output
! QCREQ         : logical   (ip) : true if associated QC bits required
! CSTR(NOBS)    : char*(*)  (ip) : character strings from message
! CREPT(NOBS)   : char*(*)  (ip) : raw report text
! SUBTYPE       : char*8    (ip) : data subtype
! NEWMDBCALL    : logical  (iop) : true for new MetDB call
! LAT_SUBSCRIPT : integer   (ip) : users lat subscript
! LON_SUBSCRIPT : integer   (ip) : users lon subscript
! RPOLE(2)      : real      (ip) : rotated pole lat/lon coords
! FOUND(*)      : logical   (ip) : MetDB keywords
! ELIST         : char*8    (ip) : Element index member name
!
! REVISION INFO :
!
!
! $Workfile: ssmret.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 09/02/2011 17:15:52$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 17:15:52    Sheila Needham  Change
!        INTENTS on users arrays
!  3    MetDB_Refresh 1.2         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  2    MetDB_Refresh 1.1         12/11/2010 14:23:39    Rosemary Lavery
!       amended ahead 
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE BUFINDX_MOD
USE CENTURY_MOD
USE DESFXY_MOD
USE DT2HRS_MOD
USE GETLOCD_MOD
USE HRS2DT_MOD
USE MAPRD_MOD
USE READIDX_MOD
USE SUBSLOCD_MOD
USE VALARR_MOD

IMPLICIT NONE

!-----------------------------------------------------------------------
! interface arguments
!-----------------------------------------------------------------------

INTEGER,           INTENT(IN)    :: ITIME(9)          !- users request times
INTEGER,           INTENT(IN)    :: IRTIM(10)         !- users TOR request
REAL,              INTENT(IN)    :: AREA(5)           !- user defined area
INTEGER,           INTENT(IN)    :: ISAT(10)          !- list of users satids
INTEGER,           INTENT(IN)    :: ILAND             !- user land/sea indicator
INTEGER,           INTENT(INOUT) :: NOBS              !- users no. of obs (ARRAY)
INTEGER,           INTENT(IN)    :: NELEM             !- users no. of elements (ARRAY)
REAL,              INTENT(INOUT) :: ARRAY(NOBS,NELEM) !- users array
INTEGER,           INTENT(INOUT) :: IOBS              !- next ob in users array
INTEGER,           INTENT(IN)    :: NDES              !- no. of user in-line elements
INTEGER,           INTENT(IN)    :: IDESC(NDES)       !- array of in-line elems pointers
INTEGER,           INTENT(IN)    :: ILVL(NDES)        !- user in-line elems replications
INTEGER,           INTENT(INOUT) :: ISTAT             !- MetDB status indicator
INTEGER,           INTENT(OUT)   :: IERR              !- MetDB error indicator
INTEGER,           INTENT(IN)    :: IDSK(5)           !- dataset details
LOGICAL,           INTENT(INOUT) :: LFLAG             ! TRUE for diagnostics on
CHARACTER (LEN=*), INTENT(INOUT) :: CSTR(NOBS)        ! users characters
CHARACTER (LEN=*), INTENT(INOUT) :: CREPT(NOBS)       ! users report text
CHARACTER (LEN=8), INTENT(IN)    :: SUBTYPE           ! data subtype
LOGICAL,           INTENT(INOUT) :: NEWMDBCALL        ! TRUE for new MetDB call
INTEGER,           INTENT(IN)    :: LAT_SUBSCRIPT     !- lat subscript in users request
INTEGER,           INTENT(IN)    :: LON_SUBSCRIPT     !- lon subscript in users request
REAL,              INTENT(IN)    :: RPOLE(2)          !- Rotated pole Lat Long coords
LOGICAL,           INTENT(IN)    :: FOUND(:)          ! MetDB keywords selected
CHARACTER (LEN=8), INTENT(IN)    :: ELIST             ! Element index member

!-----------------------------------------------------------------------
! local parameters
!-----------------------------------------------------------------------

INTEGER, PARAMETER     :: BLKSIZ = 27998           ! max block size of d ataset
INTEGER, PARAMETER     :: INDLEN = 12              ! index entry length
INTEGER, PARAMETER     :: MDATA  = 180000          ! max size of values  array
INTEGER, PARAMETER     :: MDES   = 2000            ! max number of descr iptors
INTEGER, PARAMETER     :: MEXT   = 99              ! size of IEXTRA arra y
INTEGER, PARAMETER     :: MXINDX = BLKSIZ/INDLEN   ! max number of index  entries per block

!-----------------------------------------------------------------------
! Local Variables
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! declare character variables in alphabetical order
!-----------------------------------------------------------------------

CHARACTER (LEN=4)      :: BUFR               ! 'BUFR' in EBCDIC
CHARACTER (LEN=10000)  :: CINDX(12)          ! holds element index
CHARACTER (LEN=BLKSIZ) :: CMSG               ! BUFR message
CHARACTER (LEN=1)      :: CNAM               ! characters from BUFR
CHARACTER (LEN=12)     :: CNTRY(MXINDX*4)    ! index entries
CHARACTER (LEN=1)      :: DUMMY1(1)          ! dummy character array
CHARACTER (LEN=6)      :: LOCD               ! local D descriptor
CHARACTER (LEN=4)      :: SEVENS             ! '7777' in EBCDIC
CHARACTER (LEN=1)      :: XTYPE              ! type of index 'A','B'

!-----------------------------------------------------------------------
! Local scalars
! (See also note at end for different meanings of some variables
!  when retrieving BUFR messages.)
!-----------------------------------------------------------------------

INTEGER                :: BSQ                !- BUFR displacement number
INTEGER                :: BufrEdition        !- BUFR edition number
INTEGER                :: BufrEnd            !- End of BUFR message
INTEGER                :: BufrStart          !- Start of BUFR message
INTEGER                :: DISPL(MDES)        !- Displacements from BUFINDX
INTEGER                :: DUMMY2(1)          !- Dummy integer array
INTEGER                :: F                  !- F from FXXYYY BUFR descriptor
INTEGER                :: IBLOCK             !- physical block number to read
INTEGER                :: ICOPY              !- bytes to copy (BUFR retrievals)
INTEGER                :: ID                 !- day
INTEGER                :: ID2                !- day (end of index block)
INTEGER                :: IDATHR             !- time tag of index read
INTEGER                :: IDAY               !- day from index block
INTEGER                :: IDHR               !- hour from index entry
INTEGER                :: IDREC              !- logical record number (maprd)
INTEGER                :: IENT1              !- used in index entry looping
INTEGER                :: IEXTRA(MEXT)       !- array of extra values
INTEGER                :: IFAIL              !- status flag from BUFINDX
INTEGER                :: IFLAG              !- index entry byte 1
INTEGER                :: IH                 !- hour
INTEGER                :: IH2                !- hour (end of index block)
INTEGER                :: IHOUR              !- hour from index block
INTEGER                :: II                 !- observation position
INTEGER                :: ILAT1              !- max lat of box
INTEGER                :: ILAT2              !- min lat of box
INTEGER                :: ILOCD              !- sequence descriptor (INTEGER)
INTEGER                :: ILON1              !- min lon of box
INTEGER                :: ILON2              !- max lon of box
INTEGER                :: ILSEA              !- index entry land/sea indicator
INTEGER                :: IM                 !- month
INTEGER                :: IM2                !- month (end of index block)
INTEGER                :: INCR               !- users increment (itime(9))
INTEGER                :: INDHR1             !- index block start time
INTEGER                :: INDHR2             !- index block end time
INTEGER                :: INXBLK             !- no. of index blocks in dataset
INTEGER                :: INXHRS             !- no. of hours per index block
INTEGER                :: IOB                !- last ob in users array
INTEGER                :: IRCVD(6)           !- Data from BUFR section 1
INTEGER                :: IRCV1              !- TOR in century hours
INTEGER                :: IREG               !- limarea from index entry
INTEGER                :: IRTIM1             !- users start TOR in century mins
INTEGER                :: IRTIM2             !- users end TOR in century mins
INTEGER                :: IS                 !- bytes 3 & 4 from index entry
INTEGER                :: ISATID             !- index entry satid
INTEGER                :: ISHR1              !- users time as start of index block
INTEGER                :: ISHR2              !- users time as end of index block
INTEGER                :: ISND1              !- 1st sounding number
INTEGER                :: ITIM1              !- user start time in century hours
INTEGER                :: ITIM1S             !- saved 1st ITIM1
INTEGER                :: ITIM2              !- users end time in century hours
INTEGER                :: ITIM2S             !- save 1st ITIM2
INTEGER                :: ITOR               !- index entry TOR
INTEGER                :: ITORM              !- index entry TOR in minutes
INTEGER                :: IY                 !- year
INTEGER                :: IY2                !- year (index block end)
INTEGER                :: J1                 !- index block loop counter
INTEGER                :: J2                 !- index entry loop counter
INTEGER                :: J3                 !- loop counter for users satids
INTEGER                :: K                  !- general loop counter
INTEGER                :: LASTREP            !- last byte used in CREPT element
INTEGER                :: LENCREP            !- Length of elements of CREPT array
INTEGER                :: NAMHR              !- start hour of 1st index block
INTEGER                :: NBLOCK             !- physical data block number
INTEGER                :: NELMIX             !- block number for element index
INTEGER                :: NELREQ             !- elements required from BUFINDX
INTEGER                :: NREP               !- no. of BUFR obs (BUFINDX op)
INTEGER                :: NSEND              !- no. of obs put in users array
INTEGER                :: NSOUND             !- no. of obs in BUFR message
INTEGER                :: NSQ                !- 1 or 0 if local descriptor or not
INTEGER                :: NTRIES             !- no. of index entries
INTEGER                :: NXTENT = 0         !- next index entry
INTEGER                :: NXTIME = 0         !- next start time
INTEGER                :: NXTSND = 0         !- next ob
INTEGER                :: RECLEN             !- dataset record length
INTEGER                :: SOURCE(MDES)       !- source array from BUFINDX
INTEGER                :: X                  !- XX from FXXYYY BUFR descriptor
INTEGER                :: Y                  !- YYY from FXXYYY BUFR descriptor

! Note: For retrievals in BUFR, NSEND refers to a byte number in
!       a data record rather than an ob. number in a message.
!       Also II and IOBS refer to elements of CREPT rather than
!       the number of obs. whose data has been put in VALUES.

!-----------------------------------------------------------------------
! declare logical variables in alphabetical order
!-----------------------------------------------------------------------

LOGICAL                :: ELIDXFOUND         ! TRUE if element index found
LOGICAL                :: FIRST = .TRUE.     ! TRUE if first call to SSMRET
LOGICAL                :: LCONT              ! TRUE if a continuation
LOGICAL                :: LMID               ! TRUE if part of message passed
LOGICAL                :: LMSG               ! TRUE if MESSAGE in CREQ
LOGICAL                :: LOCDFG             ! TRUE if dataset has local seq
LOGICAL                :: LTOR               ! TRUE for accurate TOR check
LOGICAL                :: QCREQ              ! TRUE if QC bits required

!-----------------------------------------------------------------------
! declare real variables in alphabetical order
!-----------------------------------------------------------------------

REAL                   :: VALUES(MDATA)      !- DEBUFR values array

!-----------------------------------------------------------------------
! declare commons. Compile with FPARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /SSMCOM1/ VALUES
COMMON /SSMCOM2/ CMSG,CNTRY,CINDX
COMMON /SSMCOM3/ SOURCE,IEXTRA,DISPL

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! First time only:  Initialise BUFR start and end markers
!-----------------------------------------------------------------------

IF (FIRST) THEN
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'
  FIRST=.FALSE.
END IF

! ----------------------------------------------------------------------
! diagnostic output to say SSMRET has been called.
! ----------------------------------------------------------------------

IF (LFLAG) THEN
  WRITE(*,'(/1X,''In MetDB subroutine SSMRET'' )')
  WRITE(*,'( 1X,''==========================''/)')
  WRITE(*,'( 1X,''Data subtype = '',A8/)')SUBTYPE
END IF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IERR = 0
IF (.NOT.FOUND(34)) II=0  !- no reset of II in BUFR retrieval
IOB  = IOBS-1

IF (LFLAG) WRITE(*,*)'In SSMRET: Entry: II,IOB,IOBS,NOBS = ',  &
                      II,IOB,IOBS,NOBS

!-----------------------------------------------------------------------
! Check for continuation and set up loop counters
!-----------------------------------------------------------------------

IF_STATUS: &
IF (ISTAT == 16) THEN
  IF (LFLAG) WRITE(*,*)'In SSMRET: New dataset'
  ISTAT = 0
ELSE IF (ISTAT == 4) THEN
  IF (LFLAG) WRITE(*,*)'In SSMRET: Continuation'
  ISHR1 = NXTIME     ! start time
  IENT1 = NXTENT     ! first entry number
  ISND1 = NXTSND     ! first sounding number
  NSEND = NXTSND-1
  LCONT = .TRUE.     ! this is a continuation
  IF (LFLAG) WRITE(*,*)'In SSMRET: ISHR1,IENT1,ISND1,NSEND = ', &
  ISHR1,IENT1,ISND1,NSEND
ELSE
  CONTINUE
END IF IF_STATUS

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here.
!=======================================================================

IF_START: &
IF (ISTAT == 0) THEN

  IF (LFLAG) THEN
    WRITE(*,*)'In SSMRET:'
    WRITE(*,*)' Descriptor  Replication'
    WRITE(*,'(1X,I8,8X,I3/)')(IDESC(K),ILVL(K),K=1,NDES)
    WRITE(*,*)'In SSMRET: AREA: ',AREA
  END IF

!-----------------------------------------------------------------------
! check if NDES > MDES. If so, output an error message & exit the
! subroutine. MDES is the max number of elements that can be handled
! by this routine. Exceeding this limit will lead to overwriting in
! routines downstream to this. MDES is enough to service any user
! request as providing they have observed replication count
! sizes described in subtype documentation.
!-----------------------------------------------------------------------

  IF (NDES > MDES) THEN
    IERR=16
    WRITE(*,'(1X,''SSMRET: MDB ERROR: NDES > MDES! '',     &
  & ''NUMBER OF ELEMENTS IN REQUEST STRING (NES)'')')
    WRITE(*,'(1X,''EXCEEDS MAX PERMITTED (MDES). '',       &
  & ''CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.'')')
    WRITE(*,'(1X,''CONTACT METDB TEAM FOR ADVICE IF '',    &
  & ''NECESSARY.'')')
    WRITE(*,'(1X,''INFO: SUBTYPE = '',A8,''  NDES = '',I6, &
  & ''  MDES = '',I6)')SUBTYPE,NDES,MDES
    GOTO 999
  END IF

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not.
! output an error and exit subroutine.
!-----------------------------------------------------------------------

  IF (NELEM < NDES) THEN
    IERR = 16
    WRITE(*,'(1X,''MDB ERROR: ARRAY NOT LARGE ENOUGH !'')')
    WRITE(*,'(1X,''TRY ARRAY ('',I3,'','',I3,'')'')')NOBS,NDES
    GOTO 999
  END IF

!-----------------------------------------------------------------------
! read map block to find dataset details. If LOCDFG is TRUE, then there
! is a local table D descriptor. Set NSQ=1.
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG, &
             NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,      &
             RECLEN,CMSG,'MAPRD')

  IF (LFLAG) WRITE(*,*) 'In SSMRET: LOCDFG = ',LOCDFG

  IF (LOCDFG) THEN
    NSQ=1
  ELSE
    NSQ=0

!-----------------------------------------------------------------------
! Patch for SAT120 data. If there is no local D in record 2, call
! GETLOCD to read the local sequence from the local sequence datasets.
! (Non-portable!!!). This will not be needed for the TOMS project, as
! the SAT120 dataset and the messages themselves will contain a local
! sequence descriptor.
!-----------------------------------------------------------------------

    IF (SUBTYPE(1:6) == 'SAT120') CALL GETLOCD (SUBTYPE)

  END IF !- locdfg

!-----------------------------------------------------------------------
! read the element index from the the element index dataset.
!-----------------------------------------------------------------------

  CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)

  IF (.NOT.ELIDXFOUND) THEN
    WRITE(*,*)'In SSMRET: MDB ERROR: Cannot find element ', &
    'index for subtype,elist = ',SUBTYPE,ELIST
    IERR=16
    GOTO 999
  END IF

  IF (LFLAG) WRITE(*,*)'In SSMRET: after call readidx, XTYPE=',XTYPE

!-----------------------------------------------------------------------
! Convert times to century hours
!-----------------------------------------------------------------------

  ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4))

  IF (ITIME(5) == 0) THEN
    ITIM2=ITIM1
  ELSE
    ITIM2=DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8))
  END IF

  INCR=ITIME(9)

  IF (INCR /= 1) THEN
    WRITE(*,*)'In SSMRET: MDB ERROR: Invalid increment -',INCR
    INCR=1
  END IF

  IF (LFLAG) &
    WRITE(*,'(1X,''In SSMRET: Start '',I10,'' End '', I10)') ITIM1,ITIM2

!-----------------------------------------------------------------------
! adjust times so that they fall on block boundaries
! offset of requested time in block
!-----------------------------------------------------------------------

  INDHR1=MOD(ITIME(4)+(24-NAMHR),INXHRS)
  INDHR2=MOD(ITIME(8)+(24-NAMHR),INXHRS)

!-----------------------------------------------------------------------
! base hours of blocks for start and end of request
!-----------------------------------------------------------------------

  ISHR1=ITIM1-INDHR1
  ISHR2=ITIM2-INDHR2

!-----------------------------------------------------------------------
! save original base offsets for check for part blocks later
!-----------------------------------------------------------------------

  ITIM1S=ISHR1
  ITIM2S=ISHR2

  IF (LFLAG) &
    WRITE(*,'('' No of index blocks      (INXBLK)   '',I8/,   &
   &          '' hours per block         (INXHRS)   '',I8/,   &
   &          '' start of 1st slot       (NAMHR)    '',I8/,   &
   &          '' hour offset             (INDHR1/2) '',2I4/,  &
   &          '' slot hours for request  (ISHR1/2)  '',2I8)') &
   &          INXBLK,INXHRS,NAMHR,INDHR1,INDHR2,ISHR1,ISHR2

!-----------------------------------------------------------------------
! time of receipt in century minutes
!-----------------------------------------------------------------------

  IF (IRTIM(1) == 0) THEN
    IRTIM1=0
  ELSE
    IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1=(IRTIM1-1)*60 + IRTIM(5)
  END IF

  IF (IRTIM(6) == 0) THEN
    IRTIM2=0
  ELSE
    IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2=(IRTIM2-1)*60 + IRTIM(10)
  END IF

  IF (LFLAG) WRITE(*,*)'In SSMRET: IRTIM1/2:',IRTIM1,IRTIM2

!-----------------------------------------------------------------------
! initialise loop counters
!-----------------------------------------------------------------------

  IENT1  = 1
  ISND1  = 1
  NXTSND = 1
  NSEND  = 0
  LMID   = .FALSE.   ! TRUE for array full mid message
  LCONT  = .FALSE.

END IF IF_START

ISTAT=0

!=======================================================================
!    INITIALISE OUTPUT ARRAYS AND POINTERS FOR RETRIEVALS IN BUFR
!=======================================================================

!                                Clear out CREPT and ARRAY if necessary
!
IF_INIT: &
IF (FOUND(34) .AND. (LMSG.OR.IOB <= 0)) THEN
  LENCREP = LEN(CREPT(1)) ! Length of elements of CREPT
  II = 0                  ! Return to start of user's ARRAY
  LASTREP = LENCREP       ! Put new data in new CREPT element
!
!                                                 Reset CREPT and ARRAY
  DO K=1,LENCREP          ! First element
    CREPT(1)(K:K) = CHAR(0)
  END DO ! K
  ARRAY(1,1) = 0.0
!
  DO K=2,NOBS             ! Other elements
    CREPT(K) = CREPT(1)
    ARRAY(K,1) = 0.0
  END DO ! K
END IF IF_INIT

!=======================================================================
! main loop over index blocks
!=======================================================================

IF (LFLAG) WRITE(*,*) 'In SSMRET: Looping over times ', ISHR1,ISHR2,INXHRS

DO_HOURS: &
DO J1=ISHR1,ISHR2,INXHRS

!-----------------------------------------------------------------------
! read index block for this period
!-----------------------------------------------------------------------

  IBLOCK = MOD(J1/INXHRS,INXBLK)+2+NSQ

  CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG, &
             NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,      &
             RECLEN,CMSG,'IDXRD')

!-----------------------------------------------------------------------
! use time-tag to check that correct date/time has been read
!-----------------------------------------------------------------------

  IDAY  = IDATHR/256
  IHOUR = IDATHR-IDAY*256

  CALL HRS2DT(IY,IM,ID,IH,J1)

  IF (ID /= IDAY.OR.IH /= IHOUR) THEN
    CALL HRS2DT(IY2,IM2,ID2,IH2,J1+INXHRS-1)
    WRITE(6,9003)SUBTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2
    GOTO 799
  END IF

!=======================================================================
! main loop over index entries for this period
!=======================================================================

  IF (LFLAG) THEN
    WRITE(*,*)'In SSMRET: Index block ',IBLOCK,' Entries ',NTRIES
    WRITE(*,*)'In SSMRET: Looping over entries ',IENT1,NTRIES
  END IF

DO_INDEX: &
  DO J2=IENT1,NTRIES

    IS=ICHAR(CNTRY(J2)(3:3))*256+ICHAR(CNTRY(J2)(4:4))

    NSOUND=MOD(IS,1024)
    IF (NSOUND < 0) NSOUND = NSOUND + 1024
    IF (LFLAG) WRITE(*,*) 'In SSMRET: Entry ',J2,' Soundings ',NSOUND

!-----------------------------------------------------------------------
! if message hasn't already been decoded...
!-----------------------------------------------------------------------

IF_DECODE: &
    IF (.NOT.LMID) THEN

!-----------------------------------------------------------------------
! if only part of block needed, check hour
!-----------------------------------------------------------------------

      IDHR=ICHAR(CNTRY(J2)(2:2))
      IDHR=MOD(IDHR,32)

      IF ((J1 == ISHR1).AND.(ISHR1 == ITIM1S)) THEN

        IF (LFLAG) &
          WRITE(*,'(1X,''In SSMRET: Entry hour offset (1) '',I5)') IDHR
        IF (IDHR < INDHR1) GOTO 699
      END IF

      IF (J1 == ISHR2) THEN
        IF (LFLAG) &
          WRITE(*,'(1X,''In SSMRET: Entry hour offset (2) '',I5)') IDHR
        IF (IDHR > INDHR2) GOTO 699
      END IF

!-----------------------------------------------------------------------
! check satellite identifier
!-----------------------------------------------------------------------

      ISATID=MOD(ICHAR(CNTRY(J2)(1:1)),64)*8+  &
      ICHAR(CNTRY(J2)(2:2))/32

      IF (LFLAG) WRITE(*,*)'In SSMRET: sat id',ISAT,ISATID

      DO J3=1,10
        IF (ISAT(J3) == 0) THEN
          IF (J3 == 1) THEN
            GOTO 125    ! no sat ids to match
          ELSE
            GOTO 699    ! end of list and no match
          END IF
        END IF
        IF (ISATID == ISAT(J3)) GOTO 125   ! matched
      END DO

      GOTO 699    ! 10 ids with no match!

125   CONTINUE

      IF (LFLAG) WRITE(*,*)'In SSMRET: No sats to match'

!-----------------------------------------------------------------------
! now check areas
!-----------------------------------------------------------------------

      ILAT2=ICHAR(CNTRY(J2)(5:5))-90
      ILAT1=ICHAR(CNTRY(J2)(6:6))-90
      ILON1=(ICHAR(CNTRY(J2)(7:7))-90)*2
      ILON2=(ICHAR(CNTRY(J2)(8:8))-90)*2

IF_AREAS: &
      IF (AREA(1) == 0.0) THEN

IF_OUTSIDE: &
        IF (ILAT2 > AREA(2) .OR.     &     ! Obs too far N
            ILAT1 < AREA(4)) THEN          ! Obs too far S
          GO TO 699
!                                  Neither box crosses the dateline
!
        ELSE IF (ILON1 <= ILON2 .AND. AREA(3) <= AREA(5)) THEN
          IF (ILON2 < AREA(3) .OR.   &     ! Obs too far W
              ILON1 > AREA(5)) GO TO 699   ! Obs too far E
!
!                                      One box crosses the dateline
!
        ELSE IF (ILON1 <= ILON2 .OR. AREA(3) <= AREA(5)) THEN
          IF (ILON2 < AREA(3) .AND. &      ! Obs too far W
              ILON1 > AREA(5)) GO TO 699   ! Obs too far E
        END IF IF_OUTSIDE
      END IF IF_AREAS

!-----------------------------------------------------------------------
! land/sea and fine mesh flags
!-----------------------------------------------------------------------

      IFLAG = ICHAR(CNTRY(J2)(1:1))
      ILSEA = IFLAG/128              ! ILSEA=1 for sea
      IREG  = MOD(IFLAG/64,2)        ! IREG=1 for outside the area

      IF (LFLAG) WRITE(*,*)'In SSMRET: land/sea and reg:', ILSEA,IREG

      IF (AREA(1) == -1.0 .AND. IREG == 1) GOTO 699

      IF (ILAND == 1 .AND. ILSEA /= 0 .OR.  &
          ILAND == 2 .AND. ILSEA /= 1) GOTO 699

!-----------------------------------------------------------------------
! Rough time of receipt check. LTOR is set true for a more accurate
! check later on. TOR can be missing data if ob is received more
! than 12 hours after base time of slot.
!
! Change users RECEIVED BEFORE check slightly. Change ITORM > IRTIM2
! to ITORM >= IRTIM2
!-----------------------------------------------------------------------

      LTOR=.FALSE.

IF_USERTIME: &
      IF (IRTIM1+IRTIM2 /= 0) THEN
        ITOR=ICHAR(CNTRY(J2)(9:9))

IF_TOR: &
        IF (ITOR >= 255) THEN
          LTOR=.TRUE.
        ELSE
          ITOR=ICHAR(CNTRY(J2)(9:9))*3
          ITORM=(J1-1)*60+ITOR
          IF (LFLAG) WRITE(*,*)'In SSMRET: ITORM:',ITORM

          IF ((IRTIM1 /= 0 .AND. (ITORM+2) < IRTIM1) .OR.   &
              (IRTIM2 /= 0 .AND. ITORM >= IRTIM2)) GOTO 699

          IF (ABS(IRTIM1-ITORM) < 3 .OR.                    &
              ABS(IRTIM2-ITORM) < 3) LTOR=.TRUE.

        END IF IF_TOR
      END IF IF_USERTIME

!-----------------------------------------------------------------------
! physical block (not counting index & map blocks)
!-----------------------------------------------------------------------

      NBLOCK=ICHAR(CNTRY(J2)(11:11))*256+ICHAR(CNTRY(J2)(12:12))

!-----------------------------------------------------------------------
! and record in block
!-----------------------------------------------------------------------

      IDREC=ICHAR(CNTRY(J2)(10:10))

!-----------------------------------------------------------------------
! get message
!-----------------------------------------------------------------------

      IBLOCK=1+NSQ+INXBLK+NBLOCK

      CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG, &
                 NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,      &
                 RECLEN,CMSG,'MSGRD')

      IF (LFLAG) &
        WRITE(*,*)'In SSMRET: Block ',IBLOCK,' Record ',IDREC

!-----------------------------------------------------------------------
! Get BUFR edition number from message, byte 8 in BUFR message.
!-----------------------------------------------------------------------

      BufrEdition=ICHAR(CMSG(8:8))

      IF (BufrEdition <= 1) THEN        ! BUFR edition 0 or 1
        BSQ=0
      ELSE
        BSQ=4                           ! BUFR edition 2 or 3
      END IF

      IF (LFLAG) WRITE(*,*)'In SSMRET: BufrEdition = ',BufrEdition

!-----------------------------------------------------------------------
! check TOR in section 1. There is a crude year 2000 check here!
!-----------------------------------------------------------------------

      IRCVD(1)=ICHAR(CMSG(17+BSQ:17+BSQ))
      IRCVD(1)=IRCVD(1)+CENTURY(IRCVD(1))

      IRCVD(2)=ICHAR(CMSG(18+BSQ:18+BSQ))
      IRCVD(3)=ICHAR(CMSG(19+BSQ:19+BSQ))
      IRCVD(4)=ICHAR(CMSG(20+BSQ:20+BSQ))
      IRCVD(5)=ICHAR(CMSG(21+BSQ:21+BSQ))

!-----------------------------------------------------------------------
! extract the BUFR subtype from BUFR section 1
!-----------------------------------------------------------------------

      IRCVD(6)=ICHAR(CMSG(14+BSQ:14+BSQ))

!-----------------------------------------------------------------------
! Additional date/time check. If the TOR is more than a day older than
! the index block data/time, reject the sounding. S.Cox 3/9/96
!-----------------------------------------------------------------------

      IRCV1=DT2HRS(IRCVD(1),IRCVD(2),IRCVD(3),IRCVD(4))

      IF ((J1-IRCV1) >= 24) THEN
        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: TOR at least 24 hrs older than'
          WRITE(*,*)'data time. Rejecting sounding! TOR = ',IRCV1, &
                    'data time = ',J1
        END IF
        GOTO 699
      END IF

IF_TOR2: &
      IF (LTOR) THEN
        IRCV1=(IRCV1-1)*60+IRCVD(5)
        LTOR=.FALSE.

        IF (LFLAG) &
          WRITE(*,*)'In SSMRET: TOR = ',IRCV1,IRTIM1,IRTIM2

!-----------------------------------------------------------------------
! Change users RECEIVED BEFORE check slightly. Change IRCV1 > IRTIM2
! to IRCV1 >= IRTIM2
!-----------------------------------------------------------------------

        IF ((IRTIM1 /= 0.AND.IRCV1 < IRTIM1) .OR.   &
            (IRTIM2 /= 0.AND.IRCV1 >= IRTIM2)) GOTO 699
      END IF IF_TOR2

!-----------------------------------------------------------------------
! Get the local D sequence number to identify the message type.
! (Not required if retrieving undecoded BUFR messages.)
!-----------------------------------------------------------------------

IF_MSGTYPE: &
      IF (.NOT.FOUND(34)) THEN
        ILOCD=ICHAR(CMSG(30+BSQ:30+BSQ))*256+       &
              ICHAR(CMSG(31+BSQ:31+BSQ))
        CALL DESFXY(ILOCD,F,X,Y)

!-----------------------------------------------------------------------
! Patch for SAT120 data. If there is not a sequence descriptor in the
! BUFR message, and the subtype is SAT120, then we need to expand the
! BUFR descriptors to determine the correct sequence descriptor to pass
! to BUFINDX. This patch will not be needed for the TOMS project, as
! the SAT120 on-line dataset will have BUFR messages containing local
! sequence descriptors.
!-----------------------------------------------------------------------

IF_BUFRDESC: &
        IF (F /= 3) THEN
          IF (SUBTYPE(1:6) == 'SAT120') THEN
            CALL SUBSLOCD(SUBTYPE,CMSG,ILOCD)
            WRITE(LOCD,'(I6)')ILOCD
            IF (LFLAG) &
              WRITE(*,*)'In SSMRET, LOCD from SUBSLOCD = ',ILOCD
            IF (ILOCD == 0) THEN
             WRITE(*,*)'In SSMRET: MDB ERROR: ILOCD from SUBSLOCD'
              WRITE(*,*)'is equal to zero! Retrieval halted'
              IERR = 16
              GOTO 999
            END IF
          ELSE
            WRITE(*,*)'In SSMRET: MDB ERROR: No sequence '
            WRITE(*,*)'descriptor in BUFR message and subtype '
            WRITE(*,*)'is not SAT120'
            IERR = 16
            GOTO 999
          END IF
        ELSE
          WRITE(LOCD,'(I1,I2.2,I3.3)')F,X,Y
        END IF IF_BUFRDESC

        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: LocalD from message=',ILOCD,LOCD
          WRITE(*,*)'In SSMRET: Cmsg=',CMSG(1:31+BSQ)
        END IF

!-----------------------------------------------------------------------
! call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

        BufrStart = INDEX(CMSG,BUFR)
        BufrEnd   = INDEX(CMSG,SEVENS)+3

        CALL BUFINDX(CINDX,IDESC,NDES,ILVL,QCREQ,IFAIL,        &
                     LFLAG,LOCD,CMSG(BufrStart:),NELREQ,DISPL, &
                     SOURCE,VALUES,MDATA,CNAM,IEXTRA,NREP,     &
                     MDES,NEWMDBCALL,MEXT)

        IF (NREP <= 0) GOTO 699

        IF (IFAIL == 16) THEN   ! element index not found
          IERR=16
          GOTO 999
        END IF

      END IF IF_MSGTYPE
    END IF IF_DECODE

!-----------------------------------------------------------------------
! Retrieve BUFR messages (FOUND(34) is true):  Copy data to CREPT array.
!-----------------------------------------------------------------------

IF_BUFR: &
    IF (FOUND(34)) THEN
!                                 Check that CREPT can hold the message
!
IF_SIZE: &
      IF (RECLEN > NOBS*LENCREP) THEN
         WRITE (6,'(T2,2A,I7)') 'SSMRET: Message skipped ',  &
                  'as too big for CREP. Size =', RECLEN
         NSEND = RECLEN  ! To skip message
!
!                   Check that free space in CREPT can hold the message
!
      ELSE IF (RECLEN <= (NOBS+1-II)*LENCREP-LASTREP) THEN
!
!                             Transfer batches of data until either all
!                                has been copied or CREPT array is full
!
DO_BATCH: &
        DO WHILE (NSEND < RECLEN .AND. &
                  (LASTREP < LENCREP .OR. II < NOBS))
!
!                           If CREPT element full, move to next element
!
          IF (LASTREP == LENCREP) THEN ! Full
            II = II + 1                ! Next element
            LASTREP = 0                ! It's empty
          END IF
!                            Find number of bytes to copy in next batch
!
!                       Min of (Bytes left, Space in CREPT)
          ICOPY = MIN0 (RECLEN-NSEND,LENCREP-LASTREP)
!
!                               Transfer batch of data to CREPT element
!
          CREPT(II)(LASTREP+1:LASTREP+ICOPY) =  &
               CMSG(  NSEND+1:  NSEND+ICOPY)
!
!                                             Update pointers and ARRAY
          NSEND = NSEND + ICOPY
          LASTREP = LASTREP + ICOPY
          ARRAY(II,1) = FLOAT(LASTREP)
        END DO DO_BATCH
      END IF IF_SIZE
      NREP = RECLEN ! so that test after VALARR is OK
    ELSE

!-----------------------------------------------------------------------
! Retrieve decoded elements: Call VALARR to put data into VALUES array.
!-----------------------------------------------------------------------

      CALL VALARR(DISPL,NELREQ,SOURCE,IRCVD,DUMMY1,      &
                  DUMMY2,IEXTRA,VALUES,CNAM,'    1 ',    &
                  ISND1,NSEND,NREP,ARRAY,NOBS,NELEM,     &
                  IOB,II,CSTR,CREPT,LFLAG,LAT_SUBSCRIPT, &
                  LON_SUBSCRIPT,AREA,RPOLE)
    END IF IF_BUFR

!-----------------------------------------------------------------------
! save next loop counts to be processed by next entry
!-----------------------------------------------------------------------

IF_SAVECNT: &
    IF (NSEND < NREP) THEN
      LMID=.TRUE.
! **        IF (LMSG) THEN
! **          WRITE(*,*)'In SSMRET: MDB ERROR: ARRAY NOT BIG ENOUGH ',
! ** &                  'FOR 1 MESSAGE'
! **          IERR=16
! **          GOTO 999
! **        END IF

      IF (LFLAG) WRITE(*,*)'In SSMRET: No more room in array', IOB

      NXTSND=NSEND+1
      NXTIME=J1
      NXTENT=J2

      ISTAT=4
      IF (LFLAG) WRITE(*,*)'In SSMRET: NXTSND,NXTENT,NXTIME=',NXTSND,NXTENT,NXTIME
      GOTO 999
    ELSE
      LMID=.FALSE.
      IOB=II         !- changed from IOB=IOB+(NSEND-ISND1)+1
      NSEND=0
      NXTSND=1
      ISND1=1

IF_FULLARR: &
      IF ((.NOT.FOUND(34).AND.IOB == NOBS) &  ! ARRAY filled
         .OR. (LMSG.AND.IOB > 0)) THEN        ! 1 msg. copied
        IF (J2+1 > NTRIES) THEN
          NXTENT=1
          IF (J1+INXHRS > ISHR2) GOTO 999
          NXTIME=J1+INXHRS
        ELSE
          NXTIME=J1
          NXTENT=J2+1
        END IF
        ISTAT=4
        IF (LFLAG) &
          WRITE(*,*)'In SSMRET: Array full: NXTSND',  &
          'NXTENT,NXTIME=',NXTSND,NXTENT,NXTIME
        GOTO 999
      END IF IF_FULLARR
    END IF IF_SAVECNT

699 CONTINUE   ! End of loop over entries this hour
  END DO DO_INDEX

  IENT1=1

799 CONTINUE   ! End of loop over hours
END DO DO_HOURS

IF (IOB == 0) THEN
  IF (LCONT) THEN
    ISTAT=0
  ELSE
    ISTAT=8
  END IF
END IF

!-----------------------------------------------------------------------
! return number of soundings
!-----------------------------------------------------------------------

999   CONTINUE

IF (II /= 0) THEN
  IOBS=II
ELSE
  IOBS=IOBS-1
END IF

IF (LFLAG) &
  WRITE(*,*)'In SSMRET: EXIT. II,IOB,IOBS,NOBS',II,IOB,IOBS,NOBS

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/',  &
       I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',  &
       I2.2,1X,I2.2,'Z')

RETURN
END SUBROUTINE SSMRET
