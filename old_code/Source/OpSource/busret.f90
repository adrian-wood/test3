SUBROUTINE BUSRET(SUBTYPE,ITIME,IRTIM,AREA,CHID,ARRAY, &
     NOBS,NELEM,IOBS,IDESC,NDES,ILVL,ISTAT,IERR,       &
     IDSK,CSTR,CREPT,LCHREP,LFLAG,QCREQ,               &
     NEWMDBCALL,LAT_SUBSCRIPT,LON_SUBSCRIPT,           &
     RPOLE,LMSG,FOUND,ELIST)

!-----------------------------------------------------------------------
!
! subroutine    : BUSRET in load module MDB
!
! purpose       : Eventual replacement for SUBRET. BUSRET if for the
!               : retrieval of all 23-byte unchained index entries
!               : datatypes through in-line elements.
!
! description   : Loops over blocks in requested period, loops
!               : over index entries for the period skipping entries
!               : that do not match the request. decodes relevant
!               : messages and finds displacements for required
!               : elements. Transfers data to users array.
!
! data types    : AMDAR, BUOY, WINPRO, BATHY, TESAC, SATOB,
!                 SFERICS, SFLOC, PAOBS, GOESAMW, BUOYPROF        !1.18
!
! called by     : MDB
!
! calls         : BUFINDX  : get BUFR displacements from message
!               : CODE     : to get description of BUFR code or flag
!               : DT2HRS   : convert date/time to century hours
!               : GETLOCD  : to get AMDARS local D
!               : HRS2DT   : convert century hours to date/time
!               : MAPRD    : to read records from dataset
!               : READIDX  : to read element index from dataset
!               : SETHED   : set raw report header
!               : VALARR   : to put values into users array
!
! arguments     :
!
! SUBTYPE       : char*(*)  (ip)  : MetDB subtype
! ITIME(9)      : integer   (ip)  : (1) yyyy     start date
!               :                 : (2) mm
!               :                 : (3) dd
!               :                 : (4) hh
!               :                 : (5) yyyy     end date
!               :                 : (6) mm
!               :                 : (7) dd
!               :                 : (8) hh
!               :                 : (9) increment
! IRTIM(10)     : integer    (ip) : (1) yyyy     earliest t.o.r
!               :                 : (2) mm
!               :                 : (3) dd
!               :                 : (4) hh
!               :                 : (5) min
!               :                 : (6) yyyy     latest t.o.r
!               :                 : (7) mm
!               :                 : (8) dd
!               :                 : (9) hh
!               :                 : (10) min
!
! AREA(5)       : real       (ip) : user-defined area                 !D
!
! CHID(50)      : char*9     (ip) : list of identifiers with '0' at end
!                                 : of list.  chid(1)='0' for all data.
!
! ARRAY(NOBS,NELEM) : real  (iop) : users data array
!
! NOBS          : integer   (iop) : number of observations
! NELEM         : integer    (ip) : number of elements
! IOBS          : integer   (iop) : no. of next ob to put in users array
! IDESC(NDES)   : integer    (ip) : array of in-line element pointers
! NDES          : integer    (ip) : number of user in-line elements
! ILVL(NDES)    : integer    (ip) : number of replications of each elem
! ISTAT         : integer   (iop) : MetDB status indicator
! IERR          : integer    (op) : error indicator
! IDSK(5)       : integer    (ip) : storage dataset details
! CSTR(NOBS)    : char*(*)  (iop) : character strings from message
! CREPT(NOBS)   : char*(*)  (iop) : array for character report
! LCHREP        : logical    (ip) : true if only report text wanted
! LFLAG         : logical    (ip) : true for diagnostic output
! QCREQ         : logical    (ip) : true for qc bits required
! NEWMDBCALL    : logical   (iop) : true for new MetDB call (istat=0)
! LAT_SUBSCRIPT : integer    (ip) : subscript of users Lat element
! LON_SUBSCRIPT : integer    (ip) : subscript of users Long element
! RPOLE         : real       (ip) : Lat/Long of rotated pole coords
! LMSG          : logical    (ip) : true for 1 BUFR message at a time
! FOUND         : logical    (ip) : MetDB keywords                    !H
! ELIST         : char*8     (ip) : Element index member name       !2.7
!
! REVISION INFO:
!
! $Workfile: busret.f90$ $Folder: OpSource$
! $Author: Alison Weir$
! $Revision: 12$ $Date: 05/04/2011 14:11:04$
!
! CHANGE RECORD :
!
! $Log:
!  12   MetDB_Refresh 1.11        05/04/2011 14:11:04    Alison Weir     Read
!       storage datasets with C routine.
!  11   MetDB_Refresh 1.10        16/02/2011 12:13:01    John Norton     Rework
!        done
!  10   MetDB_Refresh 1.9         09/02/2011 16:40:47    Sheila Needham  Change
!        INTENTS on users arrays
!  9    MetDB_Refresh 1.8         25/11/2010 10:14:38    Sheila Needham  Change
!        INTENT on NEWMDBCALL to INOUT
!  8    MetDB_Refresh 1.7         25/11/2010 09:56:09    Sheila Needham  Remove
!        function declarations
!  7    MetDB_Refresh 1.6         25/11/2010 09:36:54    Brian Barwell   Add
!       USE statements.
!  6    MetDB_Refresh 1.5         24/11/2010 17:23:24    Brian Barwell   DUMMY1
!        changed to DUMMY1(:) in calls to MAPRD.
!  5    MetDB_Refresh 1.4         16/11/2010 16:33:57    Stan Kellett
!       corrected revision info. Corrected copyright. put FIRST back in and
!       then initialis BUFR variable
!  4    MetDB_Refresh 1.3         12/11/2010 16:24:37    Richard Weedon  peer
!       review workaround updated
!  3    MetDB_Refresh 1.2         02/11/2010 12:00:05    Richard Weedon
!       initial version
!  2    MetDB_Refresh 1.1         02/11/2010 11:55:56    Richard Weedon
!       Previous change history removed
!  1    MetDB_Refresh 1.0         01/11/2010 16:39:19    Richard Weedon
!       Updated to f95 standard. Object file produced. NOTE warning - unused
!       var FIRST.
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
USE IDES_MOD
USE MAPRD_MOD
USE READIDX_MOD
USE SETHED_MOD
USE VALARR_MOD

IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter statements
! (For MXINDX allow for 6-byte header & 2-byte overflow record number)
!-----------------------------------------------------------------------

INTEGER,PARAMETER ::  BLKSIZ=27998   ! MAX BLOCKSIZE OF DATASET
INTEGER,PARAMETER ::  IHDLEN=44      ! LENGTH OF REPORT TEXT HEADER
INTEGER,PARAMETER ::  INDLEN=23      ! LENGTH OF INDEX ENTRY IN BYTES
INTEGER,PARAMETER ::  MDATA=80000    ! MAX SIZE OF VALUES ARRAY
INTEGER,PARAMETER ::  MDES=7750      ! max number of descriptors
INTEGER,PARAMETER ::  MEXT=99        ! size of IEXTRA array
INTEGER,PARAMETER ::  MXOFLW=8       ! max number of overflow blocks
INTEGER,PARAMETER ::  MXINDX=(BLKSIZ-8)/INDLEN
!max no of index entries per block

!-----------------------------------------------------------------------
! declare interface variables - variables used as dimensions for arrays
! declared before the arrays
!-----------------------------------------------------------------------

CHARACTER(LEN=8),INTENT(IN)  :: SUBTYPE      !- MetDB subtype
INTEGER,INTENT(IN)           :: ITIME(9) !- users request times
INTEGER,INTENT(IN)           :: IRTIM(10)!- users TOR request
REAL,INTENT(IN)              :: AREA(5)  !- user defined area
CHARACTER(LEN=9),INTENT(IN)  :: CHID(50) !- list of users ids wanted
INTEGER,INTENT(INOUT)        :: NOBS     !- users no. of obs (ARRAY)
INTEGER,INTENT(IN)           :: NELEM    !- users no.of elements (ARRAY)
REAL,INTENT(INOUT)           :: ARRAY(NOBS,NELEM) !- users array
INTEGER,INTENT(INOUT)        :: IOBS    !- next ob in users array
INTEGER,INTENT(IN)           :: NDES !- no. of user in-line elements
INTEGER,INTENT(IN)           :: IDESC(NDES)
                             !- array of in-line element pointers
INTEGER,INTENT(IN)           ::  ILVL(NDES)
                             !- user in-line elems replications
INTEGER,INTENT(INOUT)        :: ISTAT      !- MetDB status indicator
INTEGER,INTENT(OUT)          :: IERR       !- MetDB error indicator
INTEGER,INTENT(IN)           :: IDSK(5)    !- dataset details
CHARACTER(LEN=*),INTENT(INOUT) :: CSTR(NOBS) !- users character strings
CHARACTER(LEN=*),INTENT(INOUT) :: CREPT(NOBS)!- users report text
LOGICAL,INTENT(IN)           :: LCHREP !-TRUE if report text only wanted
LOGICAL,INTENT(IN)           :: LFLAG  !- TRUE for diagnostics on
LOGICAL,INTENT(IN)           :: QCREQ  !- TRUE for QC flags required
LOGICAL,INTENT(INOUT)        :: NEWMDBCALL    !- TRUE for new MetDB call
INTEGER,INTENT(IN)           :: LAT_SUBSCRIPT !- users lat subscript
INTEGER,INTENT(IN)           :: LON_SUBSCRIPT !- users lon subscript
REAL,INTENT(IN)              :: RPOLE(2) !- Lat Long of rotated pole
LOGICAL,INTENT(IN)           :: LMSG     !- TRUE if MESSAGE in CREQ
LOGICAL,INTENT(IN)           :: FOUND(:) !- MetDB keywords array
CHARACTER(LEN=8),INTENT(IN)  :: ELIST    !- Element index member name

!-----------------------------------------------------------------------
! declare integer variables in alphabetical order
!-----------------------------------------------------------------------

INTEGER  ::   BufrEdition       !- BUFR edition number
INTEGER  ::   BufrStart         !- start of BUFR in data block.
INTEGER  ::   Bufr1             !- start of section 1 of message
INTEGER  ::   Bufr3             !- start of section 3 of message
INTEGER  ::   DISPL(MDES)       !- Displacements from BUFINDX
INTEGER  ::   DUMMY2(1)         !- Dummy integer array
INTEGER  ::   F                 !- F part of BUFR descriptor FXXYYY
INTEGER  ::   FLAGS             !- index entry byte 17
INTEGER  ::   I                 !- general loop counter
INTEGER  ::   IBLOCK            !- physical block number to read
INTEGER  ::   ICHAR2            !- Conversion function C*2 to I*4
INTEGER  ::   ICHAR3            !- Conversion function C*3 to I*4
INTEGER  ::   ID                !- day
INTEGER  ::   ID2               !- day (end of index block)
INTEGER  ::   IDATHR            !- time tag of index read
INTEGER  ::   IDAY              !- day from index block
INTEGER  ::   IDHR              !- hour from index entry
INTEGER  ::   IDMIN             !- minute from index entry
INTEGER  ::   IDREC             !- logical record number (maprd)
INTEGER  ::   IEXTRA(MEXT)      !- array of extra values
INTEGER  ::   IENT1             !- used in index entry looping
INTEGER  ::   IFAIL             !- status flag from BUFINDX
INTEGER  ::   IFLAG             !- index entry byte 1
INTEGER  ::   IFVAL             !- BATHY/TESAC index entry indicator
INTEGER  ::   IH                !- hour
INTEGER  ::   IH2               !- hour (end of index block)
INTEGER  ::   IHOUR             !- hour from index block
INTEGER  ::   II                !- observation position
INTEGER  ::   ILAT1             !- max lat of box
INTEGER  ::   ILAT2             !- min lat of box
INTEGER  ::   ILOCD             !- sequence descriptor (integer)
INTEGER  ::   ILON1             !- min lon of box
INTEGER  ::   ILON2             !- max lon of box
INTEGER  ::   IM                !- month
INTEGER  ::   IM2               !- month (end of index block)
INTEGER  ::   IMDI              !- missing data value
INTEGER  ::   IMON              !- month of ob to put in header
INTEGER  ::   INCR              !- users increment (itime(9))
INTEGER  ::   INDHR1            !- index block start time
INTEGER  ::   INDHR2            !- index block end time
INTEGER  ::   INXBLK            !- no. of index blocks in dataset
INTEGER  ::   INXHRS            !- no. of hours per index block
INTEGER  ::   IOB               !- last ob in users array
INTEGER  ::   IORC              !- return code from C read routine
INTEGER  ::   IPOS              !- position indicator for INDEX function
INTEGER  ::   IPX               !- pointer to byte in index record
INTEGER  ::   IREG              !- limarea from index entry
INTEGER  ::   IREP1             !- 1st ob number
INTEGER  ::   IRTIM1            !- users start TOR in century mins
INTEGER  ::   IRTIM2            !- users end TOR in century mins
INTEGER  ::   ISECT1(6)         !- BUFR section 1 info for VALARR
INTEGER  ::   ISHR1             !- users time as start of index block
INTEGER  ::   ISHR2             !- users time as end of index block
INTEGER  ::   ISPACE            !- index find for ' '
INTEGER  ::   ITIM1             !- user start time in century hours
INTEGER  ::   ITIM1S            !- saved 1st ITIM1
INTEGER  ::   ITIM2             !- users end time in century hours
INTEGER  ::   ITIM2S            !- saved 1st ITIM2
INTEGER  ::   ITORHR            !- TOR hour for header
INTEGER  ::   ITORM             !- index entry TOR in minutes
INTEGER  ::   ITORMN            !- TOR minute for header
INTEGER  ::   IY                !- year
INTEGER  ::   IY2               !- year (end of index block)
INTEGER  ::   J1                !- index block loop counter
INTEGER  ::   J2                !- index entry loop counter
INTEGER  ::   J3                !- loop counter for users ids
INTEGER  ::   K                 !- general loop counter
INTEGER  ::   LASTNDX           !- last base index record number
INTEGER  ::   NAMD              !- amend number for header
INTEGER  ::   NAMHR             !- start hour of first index block
INTEGER  ::   NBLOCK            !- physical data block number
INTEGER  ::   NCOR              !- cor number for header
INTEGER  ::   NELMIX            !- block number for element index
INTEGER  ::   NELREQ            !- elements required from BUFINDX
INTEGER  ::   NLAT              !- latitude for index entry ob
INTEGER  ::   NLON              !- longitude for index entry ob
INTEGER  ::   NREC              !- data block record number
INTEGER  ::   NREP              !- no. of BUFR obs (BUFINDX op)
INTEGER  ::   NSEND             !- no. of obs put in users away
INTEGER  ::   NSQ               !- 1 or 0 if local descriptor or not
INTEGER  ::   NTOR              !- used for index entry TOR calculation
INTEGER  ::   NTRIES            !- no. of index entries
INTEGER  ::   NUMNDX            !- index entry number in record
INTEGER  ::   NXTENT            !- next index entry
INTEGER  ::   NXTIME            !- next start time
INTEGER  ::   NXTREP            !- next report number
INTEGER  ::   RECLEN            !- dataset record length
INTEGER  ::   SOURCE(MDES)      !- source array from BUFINDX
INTEGER  ::   X                 !- XX part of BUFR descriptor FXXYYY
INTEGER  ::   Y                 !- YYY part of BUFR descriptor FXXYYY

!-----------------------------------------------------------------------
! declare real variables in alphabetical order
!-----------------------------------------------------------------------

REAL   ::    RLAT                 !- ob latitude (real)
REAL   ::    RLON                 !- ob longitude (real)
REAL   ::    VALUES(MDATA)        !- DEBUFR values array

!-----------------------------------------------------------------------
! declare logical variables in alphabetical order
!-----------------------------------------------------------------------

LOGICAL  ::  ALLREQ            !- TRUE if model data wanted !1.20
!                                             or no met element   !1.20
LOGICAL  ::  CLOUDREP          !- TRUE if cloud data present in ob
LOGICAL  ::  CLOUDREQ          !- TRUE if cloud data wanted
LOGICAL  ::  ELIDXFOUND        !- TRUE if element index found
LOGICAL  ::  FIRST             !- TRUE if first call to subroutine
LOGICAL  ::  GETEM             !- TRUE if ob wanted
LOGICAL  ::  LCONT             !- TRUE if a continuation
LOGICAL  ::  LMID              !- TRUE if part of message passed
LOGICAL  ::  LOCDFG            !- TRUE if dataset has local seq
LOGICAL  ::  RHREP             !- TRUE if RH data present in ob
LOGICAL  ::  RHREQ             !- TRUE if RH data wanted
LOGICAL  ::  TEMPREP           !- TRUE if temp data present in ob
LOGICAL  ::  TEMPREQ           !- TRUE if temp data wanted
LOGICAL  ::  WINDREP           !- TRUE if wind data present in ob
LOGICAL  ::  WINDREQ           !- TRUE if wind data wanted

!-----------------------------------------------------------------------
! declare character variables in alphabetical order
!-----------------------------------------------------------------------

CHARACTER(LEN=4)       ::       BUFR       !- 'BUFR' in EBCDIC
CHARACTER(LEN=4)       ::       CCCC       !- collecting centre
CHARACTER(LEN=10000)   ::       CINDX(12)  !- holds element index
CHARACTER(LEN=BLKSIZ)  ::       CMSG       !- BUFR message
CHARACTER(LEN=20000)   ::       CNAM       !- character string in BUFR
CHARACTER(LEN=9)       ::       DBID       !- index entry id
CHARACTER(LEN=1)       ::       DUMMY1(1)  !- dummy character array
! CHARACTER(LEN=132)     ::       HEAD       !- revision information
CHARACTER(LEN=44)      ::       HEADER     !- report text header
CHARACTER(LEN=5)       ::       HELN       !- length of report text
CHARACTER(LEN=BLKSIZ)  ::       INDXREC    !- index record
CHARACTER(LEN=6)       ::       LOCD       !- local D descriptor
CHARACTER(LEN=100)     ::       WORDS    !- CCCC string from code tables
CHARACTER(LEN=1)       ::       XTYPE      !- type of element index


!-----------------------------------------------------------------------
! declare dynamic common. Compile with FPARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /BUSCOM1/VALUES,SOURCE,IEXTRA,DISPL                      !A
COMMON /BUSCOM2/CINDX,INDXREC,CMSG,CNAM,HEADER,WORDS         !2.11

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA FIRST    /.TRUE./           !- first call to subroutine
DATA IMDI     /-9999999/         !- missing data indicator

!-----------------------------------------------------------------------
! diagnostic output to say BUSRET has been called.
!-----------------------------------------------------------------------

IF (LFLAG) THEN
  WRITE(*,'(/1X,''In MetDB subroutine BUSRET'' )')
  WRITE(*,'( 1X,''==========================''/)')
  WRITE(*,'( 1X,''Data subtype = '',A8/)')SUBTYPE
END IF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IERR = 0
II   = 0
IOB  = IOBS-1

HEADER = '0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '         !F

IF (LFLAG) THEN
  WRITE(*,*)'In BUSRET: Entry: II,IOB,IOBS,NOBS',II,IOB,IOBS,NOBS
END IF

!-----------------------------------------------------------------------
! First time only:  Initialise revision information and BUFR
!-----------------------------------------------------------------------
!
IF (FIRST) BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)

!-----------------------------------------------------------------------
! check for continuation and set up loop counters
!-----------------------------------------------------------------------

IF (ISTAT == 16) THEN
  IF (LFLAG) WRITE(*,*)'In BUSRET: New dataset'
  ISTAT = 0
ELSE IF (ISTAT == 4) THEN
  ISHR1 = NXTIME              ! start time
  IENT1 = NXTENT              ! first entry number
  IREP1 = NXTREP              ! first report number
  NSEND = NXTREP-1
  LCONT = .TRUE.              ! this is a continuation
  IF (LFLAG) THEN
    WRITE(*,*)'In BUSRET: Continuation'
    WRITE(*,*)'In BUSRET: ISHR1,IENT1,IREP1,NSEND = ', &
     ISHR1,IENT1,IREP1,NSEND
  END IF
END IF

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here
!=======================================================================

IF (ISTAT == 0) THEN

  IF (LFLAG) THEN
    WRITE(*,*)'In BUSRET:'
    WRITE(*,*)' Descriptor  Replication'
    WRITE(*,'(1X,I8,8X,I3/)')(IDESC(K),ILVL(K),K=1,NDES)
    WRITE(*,*)'In BUSRET: AREA: ',AREA                          !D
    WRITE(*,*)'In BUSRET: IDS:  ',CHID
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
    WRITE(*,'(1X,''BUSRET: MDB ERROR: NDES > MDES! '', &
     &    ''NUMBER OF ELEMENTS IN REQUEST STRING (NES)'')')
    WRITE(*,'(1X,''EXCEEDS MAX PERMITTED (MDES). '',   &
     &    ''CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.'')')
    WRITE(*,'(1X,''CONTACT METDB TEAM FOR ADVICE IF '',&
     &    ''NECESSARY.'')')
    WRITE(*,'(1X,''INFO: SUBTYPE = '',A8,''  NDES = '',I6,&
     &    ''  MDES = '',I6)')SUBTYPE,NDES,MDES
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
! is a local table D descriptor. Set NSQ=1
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,  &
     &       NELMIX,IBLOCK,NTRIES,DUMMY1(:),IDATHR,IDREC,   &
     &       RECLEN,CMSG,'MAPRD')

! The above MAPRD call reads the sequence record.  For SFERICS
! before Jan 2003 the sequence is invalid, so replace it by the
! current sequence, which will decode old SFERICS messages too.
! (N.B. This fix will only work while 316192 stays in the local
! sequence list, so DON'T drop it after 2 changes of sequence!)

  IF (SUBTYPE(1:7) == 'SFERICS') THEN
    IF (ITIME(1) < 2003 .OR. &
     &       (ITIME(1) == 2003 .AND. ITIME(2) == 1)) THEN
      CALL GETLOCD(SUBTYPE)
    END IF
  END IF

!-----------------------------------------------------------------------
! If there is no local D in record 2, return with an error.         !2.7
!-----------------------------------------------------------------------

  IF (LOCDFG) THEN
    NSQ=1
  ELSE
    NSQ=0
    WRITE(*,*)'BUSRET: MDB ERROR: No Local D in record 2'
    IERR = 16
    GOTO 999
  END IF !- locdfg

!-----------------------------------------------------------------------
! read the element index from the element index dataset.
!-----------------------------------------------------------------------

  CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)

  IF (.NOT.ELIDXFOUND) THEN
    WRITE(*,*)'In BUSRET: MDB ERROR: Cannot find element ', &
     'index for subtype,elist = ',SUBTYPE,ELIST
    IERR=16
    GOTO 999
  END IF

  IF (LFLAG) THEN
    WRITE(*,*)'In BUSRET: after call readidx, XTYPE=',XTYPE
  END IF

!-----------------------------------------------------------------------
! convert request times to century hours
!
! ITIM1, ITIM2 = users start, end times in century hours.
!-----------------------------------------------------------------------

  ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4)/100)

  IF (ITIME(5) == 0) THEN
    ITIM2=ITIM1
  ELSE
    ITIM2=DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8)/100)
  END IF

!-----------------------------------------------------------------------
! check for a valid time increment.
!-----------------------------------------------------------------------

  INCR=ITIME(9)

  IF (INCR /= 1) THEN
    WRITE(*,*)'In BUSRET: MDB ERROR: Invalid increment -',INCR
    INCR=1
  END IF

  IF (LFLAG) WRITE(*,'(1X,''In BUSRET: Start '',I10,''End'', &
           &I10)')ITIM1,ITIM2

!-----------------------------------------------------------------------
! round times down to starts of index blocks
!
! ISHR1, ISHR2 = users start, end times as start of index block times.
! They will be the same if users period contained within 1 index block
!-----------------------------------------------------------------------

  INDHR1=MOD(ITIME(4)/100+NAMHR,INXHRS)
  INDHR2=MOD(ITIME(8)/100+NAMHR,INXHRS)

  ISHR1=ITIM1-INDHR1
  ISHR2=ITIM2-INDHR2

!-----------------------------------------------------------------------
! save original base offsets for check for part blocks later
!-----------------------------------------------------------------------

  ITIM1S=ISHR1
  ITIM2S=ISHR2

  IF (LFLAG) THEN
    WRITE(*,'('' No of index blocks      (INXBLK)   '',I8/,&
            &'' hours per block         (INXHRS)   '',I8/,&
            &'' start of 1st slot       (NAMHR)    '',I8/,&
            &'' hour offset             (INDHR1/2) '',2I4/,&
            &'' slot hours for request  (ISHR1/2)  '',2I8)')&
            &INXBLK,INXHRS,NAMHR,INDHR1,INDHR2,ISHR1,ISHR2
  END IF

!-----------------------------------------------------------------------
! keywords RECEIVED BETWEEN and RECEIVED BEFORE/AFTER are used to
! constrain the data returned to be received (time of receipt in the
! MetDB) within the given period. Convert requested times of receipt
! to century minutes.
!-----------------------------------------------------------------------

  IF (IRTIM(1) == 0) THEN
    IRTIM1=0
  ELSE
    IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1=(IRTIM1-1)*60+IRTIM(5)
  END IF
  IF (IRTIM(6) == 0) THEN
    IRTIM2=0
  ELSE
    IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2=(IRTIM2-1)*60+IRTIM(10)
  END IF

  IF (LFLAG) WRITE(*,*)'In BUSRET: IRTIM1/2:',IRTIM1,IRTIM2

!-----------------------------------------------------------------------
! initialise some variables
!-----------------------------------------------------------------------

  IENT1  = 1
  IREP1  = 1
  LASTNDX= 0        ! No index record read yet
  NXTREP = 1
  NSEND  = 0
  LMID   = .FALSE.       !- TRUE for array full mid message
  LCONT  = .FALSE.

!------------------------ ----------------------------------------------
! If SATOBS, see if messages can be skipped, by checking sequence !1.20
! numbers of meteorological elements (from data dictionary).      !1.20
! But if no met elements in request or model data wanted, don't   !1.20
! skip any message.                                               !1.20
!-----------------------------------------------------------------------

  IF (SUBTYPE(1:5) == 'SATOB') THEN

    TEMPREQ  = .FALSE.
    WINDREQ  = .FALSE.
    CLOUDREQ = .FALSE.
    RHREQ    = .FALSE.
    ALLREQ   = .FALSE.

    DO I = 1,NDES
      IF (IDESC(I) == 24) THEN
        CLOUDREQ=.TRUE.
      ELSE IF (IDESC(I) == 21 .OR. IDESC(I) == 25) THEN
        TEMPREQ=.TRUE.
      ELSE IF (IDESC(I) == 22 .OR. IDESC(I) == 23) THEN
        WINDREQ=.TRUE.
      ELSE IF (IDESC(I) >= 14 .AND. IDESC(I) <= 16) THEN
        RHREQ=.TRUE.
      ELSE IF (IDESC(I) >= 26) THEN    ! model elements
        ALLREQ=.TRUE.
      END IF
    END DO

    IF (.NOT.TEMPREQ .AND. .NOT.CLOUDREQ .AND.   &
             .NOT.WINDREQ .AND. .NOT.RHREQ) THEN
      ALLREQ=.TRUE.
    END IF

  END IF !- subtype=SATOB

END IF !- istat=0

ISTAT=0

!=======================================================================
! main loop over blocks
!=======================================================================

IF (LFLAG) WRITE(*,*)'In BUSRET: Looping over times ', &
              ISHR1,ISHR2,INXHRS

DO 799 J1 = ISHR1,ISHR2,INXHRS

!-----------------------------------------------------------------------
! If starting a new index period, read the first index record (the
! 'base' record) for this period and check that the tag time is OK.
!-----------------------------------------------------------------------

  IBLOCK=MOD(J1/INXHRS,INXBLK)+2+NSQ  ! 'Base' record number

  IF (IBLOCK /= LASTNDX) THEN
    CALL METDB_CREAD_DIR (IDSK(3), INDXREC(1:IDSK(2)), IDSK(2), &
                          IBLOCK, IORC)
    NUMNDX = 0      ! No index entries processed
    LASTNDX = IBLOCK

    IDATHR = ICHAR2(INDXREC(1:2))  ! Time tag
    NTRIES = ICHAR2(INDXREC(3:4))  ! No. of entries

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
  END IF

!=======================================================================
! main loop over entries for this period
!=======================================================================

  IF (LFLAG) THEN
    WRITE(*,*)'In BUSRET: Index block ',IBLOCK,' Entries ',NTRIES
    WRITE(*,*)'In BUSRET: Looping over entries ',IENT1,NTRIES
  END IF

!-----------------------------------------------------------------------
!
! 23 Byte Index Entry Structure
!
! +-------------------------------------------------------------------
! !COR ! FINE ! HOUR ! MINUTE !PLATFORM ! NO.!LATITUDE(S)!LONGITUDE(S)
! !    ! MESH !  (6  !   (1   ! IDENT.  ! OF !           !
! !FLAG! FLAG ! BITS)!  BYTE) !(9 BYTES)!OBS.! (2 BYTES) ! (2 BYTES)
! +------------------+--------+---------+----+-----------+------------
!          1              2      3 - 11   12     13-14       15-16
!
! +--------------------------------
! ! FLAGS !TIME ! NO. IN !  DATA  !
! !       ! OF  !  DATA  ! RECORD !
! !       !RCPT.! RECORD ! NUMBER !
! +-------+-----+--------+--------+
!    17    18-19   20-21    22-23
!
!-----------------------------------------------------------------------

DO 699 J2=IENT1,NTRIES
  IF (.NOT.LMID) THEN

!-----------------------------------------------------------------------
! If at the end of an index record, read the next one in.
! Update index counter and record pointer for next index entry.
!-----------------------------------------------------------------------

    IF (NUMNDX >= MXINDX) THEN                               !2.11
      IPX = IPX + INDLEN
      IBLOCK = ICHAR2(INDXREC(IPX+1:IPX+2)) + NSQ
      CALL METDB_CREAD_DIR (IDSK(3), INDXREC(1:IDSK(2)), IDSK(2), &
                            IBLOCK, IORC)
      NUMNDX = 1           ! First index entry in record
    ELSE
      NUMNDX = NUMNDX + 1  ! Next entry in record
    END IF
!                               Set IPX to point just before next entry
    IF (NUMNDX == 1) THEN
      IPX = 6              ! 6 bytes before first entry
    ELSE
      IPX =IPX + INDLEN    ! Move to next index entry
    END IF

!-----------------------------------------------------------------------
! If only part of block needed, check hour.
! (Hour is contained in six bits of first byte of index entry.)
!-----------------------------------------------------------------------

    IDHR = ICHAR(INDXREC(IPX+1:IPX+1))
    IDHR=MOD(IDHR,64)
    IDMIN = ICHAR(INDXREC(IPX+2:IPX+2))
    IF (LFLAG) PRINT *,'BUSRET: ', &
                    INDXREC(IPX+1:IPX+INDLEN), IDHR, IDMIN

    IF (J1 == ISHR1 .AND. ISHR1 == ITIM1S .AND.&
          60*(IDHR-INDHR1)+IDMIN-MOD(ITIME(4),100) < 0) GOTO 699

    IF (J1 == ISHR2 .AND. &
          60*(IDHR-INDHR2)+IDMIN-MOD(ITIME(8),100) > 0) GOTO 699

!-----------------------------------------------------------------------
! Check Identifier:
! checking chid(j3)(1:1) = '0' is not strong enough; a user may
! request climat for 03 (say), so the condition must be strengthened
! to take account of the fact that the first character may be zero.
! a full check for 5 zeros is used to avoid any ambiguity.
!-----------------------------------------------------------------------

    DO J3 = 1,50
      IF (CHID(J3)(1:5) == '00000') THEN
        IF (J3 == 1) THEN
          IF (LFLAG) WRITE(*,*)'In BUSRET: No ids to match'
          GOTO 125    !- no ids to match
        ELSE
          GOTO 699    !- end of list and no match
        END IF
      END IF
      IPOS = INDEX(CHID(J3),' ')
      IF (IPOS == 0) IPOS = 10
      DBID = INDXREC(IPX+3:IPX+11)
      IF (LFLAG) WRITE(*,*)'In BUSRET: Users ID = ',&
                      CHID(J3),' DB ID = ',DBID
      IF (DBID(1:IPOS-1) == CHID(J3)(1:IPOS-1)) GOTO 125  !- match
    END DO  ! j3 loop

    GOTO 699  ! 50 ids with no match!

125       CONTINUE

!-----------------------------------------------------------------------
! If satob retrieval, check that the users chosen element(s) is/are
! flagged as maybe present in the message
! (unless model data or no met elements requested)
!-----------------------------------------------------------------------

    IF (SUBTYPE(1:5) == 'SATOB' .AND. .NOT.ALLREQ) THEN

      TEMPREP  = .FALSE.
      WINDREP  = .FALSE.
      CLOUDREP = .FALSE.
      RHREP    = .FALSE.
      GETEM    = .FALSE.
      FLAGS    = ICHAR(INDXREC(IPX+17:IPX+17))

      IF (MOD(FLAGS/8,2) == 1) TEMPREP    = .TRUE.
      IF (MOD(FLAGS/4,2) == 1) WINDREP    = .TRUE.
      IF (MOD(FLAGS/2,2) == 1) CLOUDREP   = .TRUE.
      IF (MOD(FLAGS,2) == 1)   RHREP      = .TRUE.

      IF (TEMPREQ  .AND. TEMPREP ) GETEM  = .TRUE.
      IF (WINDREQ  .AND. WINDREP ) GETEM  = .TRUE.
      IF (CLOUDREQ .AND. CLOUDREP) GETEM  = .TRUE.
      IF (RHREQ    .AND. RHREP   ) GETEM  = .TRUE.

      IF (.NOT.GETEM) GOTO 699     !- no required elements flagged

    END IF  !- if subtype == satob

!-----------------------------------------------------------------------
! number of observations
!-----------------------------------------------------------------------

    IF (SUBTYPE(1:5) == 'SATOB' .OR. &
            SUBTYPE(1:7) == 'SFERICS') THEN
      NREP = ICHAR2(INDXREC(IPX+11:IPX+12))
    ELSE
      NREP = ICHAR(INDXREC(IPX+12:IPX+12))
    END IF
    IF (NREP == 0) NREP=1        !- inconsistency in old BUOY data

!-----------------------------------------------------------------------
! NREP=1 gives a single lat/lon in hundredths
! NREP>1 gives a lat/lon box
!-----------------------------------------------------------------------

    IF (LFLAG) THEN
      WRITE(*,*)'In BUSRET: Entry: ',J2,' Reports: ',NREP
      WRITE(*,*)'In BUSRET: Index: ',INDXREC(IPX+1:IPX+INDLEN)
    END IF

!-----------------------------------------------------------------------
! Check areas if required. (Only done for non-rotated lat/long areas.)
!-----------------------------------------------------------------------

    IF (AREA(1) == 0.0) THEN

!-----------------------------------------------------------------------
!          (1)  Single observation in index.
!-----------------------------------------------------------------------

      IF (NREP == 1) THEN
!     Get lat & long (negative if > 32768)

        NLAT = ICHAR2(INDXREC(IPX+13:IPX+14))
        NLON = ICHAR2(INDXREC(IPX+15:IPX+16))

        IF (NLAT > 32768) NLAT=NLAT-65536
        IF (NLON > 32768) NLON=NLON-65536

        RLAT=0.01*FLOAT(NLAT)   ! Real latitude
        RLON=0.01*FLOAT(NLON)   ! Real longitude

        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET: Area DB: ',RLAT,RLON
          WRITE(*,*)'In BUSRET: Area RQ: ',(AREA(K),K=2,5)
        END IF
!                                  Check for observation in user's AREA

!                 Ob. too far N    or   Ob. too far S
        IF (AREA(2) < RLAT .OR. AREA(4) > RLAT) THEN
          GO TO 699
!                                       AREA doesn't cross dateline
!                       West < or = East
        ELSE IF (AREA(3) <= AREA(5)) THEN
          IF (AREA(3) > RLON .OR. &         ! Ob. too far W
                   AREA(5) < RLON) GO TO 699     ! Ob. too far E

!                                             AREA crosses dateline
        ELSE
          IF (AREA(3) > RLON .AND. &        ! Ob. too far W
                   AREA(5) < RLON) GO TO 699     ! Ob. too far E
        END IF

!-----------------------------------------------------------------------
!           (2) Observation lat/long box
!-----------------------------------------------------------------------

      ELSE    !  NREP>1

        ILAT2 =  ICHAR(INDXREC(IPX+13:IPX+13))-90   ! South
        ILAT1 =  ICHAR(INDXREC(IPX+14:IPX+14))-90   ! North
        ILON1 = (ICHAR(INDXREC(IPX+15:IPX+15))-90)*2 ! West
        ILON2 = (ICHAR(INDXREC(IPX+16:IPX+16))-90)*2 ! East

        IF (LFLAG) THEN
          WRITE(*,*)'In BUSRET: Area DB: ',ILAT1,ILAT2,ILON1,ILON2
          WRITE(*,*)'In BUSRET: Area RQ: ',(AREA(K),K=2,5)
        END IF
!        Check for overlap between obs. box and user's area

!                  AREA too far S   or   AREA too far N
        IF (AREA(2) < ILAT2 .OR. AREA(4) > ILAT1) THEN
          GO TO 699
!       Neither box crosses the dateline

        ELSE IF (ILON1 <= ILON2 .AND. AREA(3) <= AREA(5)) THEN
          IF (ILON2 < AREA(3) .OR. &     ! Obs too far W
            ILON1 > AREA(5)) GO TO 699   ! Obs too far E

!                                      One box crosses the dateline

        ELSE IF (ILON1 <= ILON2 .OR. AREA(3) <= AREA(5)) THEN
          IF (ILON2 < AREA(3) .AND.&     ! Obs too far W
            ILON1 > AREA(5)) GO TO 699   ! Obs too far E
        END IF
      END IF
    END IF

!-----------------------------------------------------------------------
! byte 1 of index entry
!-----------------------------------------------------------------------

    IFLAG = ICHAR(INDXREC(IPX+1:IPX+1))

!-----------------------------------------------------------------------
! IREG=1 for outside the area
!-----------------------------------------------------------------------

    IREG = MOD(IFLAG/64,2)
    IF (LFLAG) WRITE(*,*)'In BUSRET: Flags: ',IFLAG/128,IREG
    IF (AREA(1) == -1 .AND. IREG == 1) GOTO 699

!-----------------------------------------------------------------------
! check if TOR in range of users request. TOR in minutes after slot
! base hour
!-----------------------------------------------------------------------

    IF (IRTIM1+IRTIM2 /= 0) THEN
      NTOR = ICHAR2(INDXREC(IPX+18:IPX+19))
      ITORM = (J1-1)*60+NTOR
      IF (LFLAG) WRITE(*,*)'In BUSRET: ITORM: ',ITORM
      IF ((IRTIM1 /= 0.AND.ITORM < IRTIM1) .OR. &
           (IRTIM2 /= 0.AND.ITORM >= IRTIM2)) GOTO 699
    END IF

!-----------------------------------------------------------------------
! BATHY/TESAC retrieval - both types stored in same dataset - check
! bit 8 in byte 17 of index. 1 for TESAC, 0 for BATHY
!-----------------------------------------------------------------------

    IFVAL = MOD(ICHAR(INDXREC(IPX+17:IPX+17)),2)

    IF ((SUBTYPE(1:5) == 'BATHY') .AND. (IFVAL /= 0)) GOTO 699
    IF ((SUBTYPE(1:5) == 'TESAC') .AND. (IFVAL /= 1)) GOTO 699

!-----------------------------------------------------------------------
! For a BUOY with a profile, the low order bit of byte 17 will be set
! in storage. If the subtype is BUOYPROF and byte 17 is even,
! reject the report because it does not contain a profile.
!-----------------------------------------------------------------------

    IF ((SUBTYPE(1:8) == 'BUOYPROF') .AND. &
            MOD(IFVAL,2) == 0) GOTO 699

!-----------------------------------------------------------------------
! physical block (not counting index & map blocks) and record in block
!-----------------------------------------------------------------------

    NBLOCK = ICHAR2(INDXREC(IPX+22:IPX+23))
    NREC   = ICHAR2(INDXREC(IPX+20:IPX+21))
    IDREC  = NREC

!-----------------------------------------------------------------------
! safety check. Don't read data block if either block or record pointer
! zero or less.
!-----------------------------------------------------------------------

    IF (NBLOCK <= 0 .OR. NREC <= 0) GOTO 699

!-----------------------------------------------------------------------
! get message
!-----------------------------------------------------------------------

    IBLOCK=INXBLK+NBLOCK+1+NSQ

    CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,  &
               NELMIX,IBLOCK,NTRIES,DUMMY1(:),IDATHR,IDREC,   &
               RECLEN,CMSG,'MSGRD')

    IF (LFLAG) THEN
      WRITE(*,*)'In BUSRET: CMSG  = ',CMSG(1:80)
      WRITE(*,*)'In BUSRET: Block = ',IBLOCK,' Record = ',IDREC
    END IF

!-----------------------------------------------------------------------
! Get BUFR edition number from message, byte 8 in BUFR message.
! Also find start of BUFR sections 1 and 3.
!-----------------------------------------------------------------------

    BufrStart = INDEX(CMSG,BUFR)
    BufrEdition=ICHAR(CMSG(BufrStart+7:BufrStart+7))

    IF (BufrEdition <= 1) THEN    ! BUFR edition 0 or 1
      Bufr1 = BufrStart + 4
    ELSE                          ! BUFR edition 2 or more
      Bufr1 = BufrStart + 8
    END IF
!                Skip section 1
    Bufr3 = Bufr1 + ICHAR3(CMSG(Bufr1:Bufr1+2))
!                Skip section 2
    IF (ICHAR(CMSG(Bufr1+7:Bufr1+7)) >= 128)&
           Bufr3 = Bufr3 + ICHAR3(CMSG(Bufr3:Bufr3+2))

    IF (LFLAG) WRITE(*,*)'In BUSRET: BufrEdition = ',BufrEdition

!-----------------------------------------------------------------------
! get TOR and collecting centre from section 1. There is a crude year
! 2000 check here!
!-----------------------------------------------------------------------

    ISECT1(1)=ICHAR(CMSG(Bufr1+12:Bufr1+12))
    ISECT1(1)=ISECT1(1)+CENTURY(ISECT1(1))

    ISECT1(2)=ICHAR(CMSG(Bufr1+13:Bufr1+13))
    ISECT1(3)=ICHAR(CMSG(Bufr1+14:Bufr1+14))
    ISECT1(4)=ICHAR(CMSG(Bufr1+15:Bufr1+15))
    ISECT1(5)=ICHAR(CMSG(Bufr1+16:Bufr1+16))
    ISECT1(6)=ICHAR2(CMSG(Bufr1+4:Bufr1+5))

    IF (ISECT1(6) >= 65535) ISECT1(6)=IMDI

!-----------------------------------------------------------------------
! if report text wanted, set up header to go in front
!-----------------------------------------------------------------------

    IH     = MOD(ICHAR(INDXREC(IPX+1:IPX+1)),32) !- hour
    IM     = ICHAR(INDXREC(IPX+2:IPX+2))       !- minute
    ID     = IMDI                              !- day
    IMON   = IMDI                              !- month
    IY     = IMDI                              !- year
    ITORHR = ISECT1(4)                         !- TOR hour
    ITORMN = ISECT1(5)                         !- TOR minute
    NAMD   = 0                                 !- Amend no.
    NCOR   = 0                                 !- COR no.
    CCCC   = '    '

!-----------------------------------------------------------------------
! Collecting centre is from BUFR section 1. Call CODE to translate
! code table value into collecting centre name.
!-----------------------------------------------------------------------

    CALL CODE(IDES(001031),ISECT1(6),WORDS)

!-----------------------------------------------------------------------
! find first non-space in words string
!-----------------------------------------------------------------------

    I=1
197       CONTINUE
    IF (WORDS(I:I) == ' ') THEN
      I=I+1
      IF (I > 100) GOTO 198
      GOTO 197
    ELSE
      ISPACE = INDEX(WORDS(I:),' ')
      CCCC   = WORDS(I:I+ISPACE-1)
    END IF

!-----------------------------------------------------------------------
! make raw report header.
!-----------------------------------------------------------------------

198       CONTINUE

    WRITE(HELN,'(I5)') BufrStart-1+IHDLEN
    CALL SETHED(HEADER,RLAT,RLON,IH,IM,ID,IMON,IY,&
                     ITORHR,ITORMN,NAMD,NCOR,CCCC)

!-----------------------------------------------------------------------
! if more than just the raw report text wanted, get the local D
! sequence descriptor to identify the message type.
!-----------------------------------------------------------------------

    IF (.NOT.LCHREP) THEN
      ILOCD=ICHAR2(CMSG(Bufr3+7:Bufr3+8))
      CALL DESFXY(ILOCD,F,X,Y)

!-----------------------------------------------------------------------
! Check that there is a sequence descriptor in the BUFR message.
!-----------------------------------------------------------------------

      IF (F /= 3) THEN
        WRITE(*,*)'BUSRET: MDB ERROR: No sequence descriptor '
        WRITE(*,*)'in BUFR message'
        IERR = 16
        GOTO 999
      ELSE
        WRITE(LOCD,'(I1,I2.2,I3.3)')F,X,Y
      END IF

      IF (LFLAG) THEN
        WRITE(*,*)'In BUSRET: LocalD from message=',ILOCD,LOCD
      END IF

!-----------------------------------------------------------------------
! call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

      CALL BUFINDX(CINDX,IDESC,NDES,ILVL,QCREQ,IFAIL,LFLAG, &
                LOCD,CMSG(BufrStart:),NELREQ,DISPL,SOURCE,  &
                VALUES,MDATA,CNAM,IEXTRA,NREP,MDES,         &
                NEWMDBCALL,MEXT)

      IF (NREP <= 0) GOTO 699

      IF (IFAIL == 16) THEN   ! element index not found
        IERR=16
        GOTO 999
      END IF

!-----------------------------------------------------------------------
! just the raw report text wanted.
!-----------------------------------------------------------------------

    ELSE
      NREP=1
      NELREQ=1
      SOURCE(1)=-99
    END IF

  END IF !- .not.lmid

!-----------------------------------------------------------------------
! Call VALARR to put data into users array.  Pass the raw report
! text if there is one (BufrStart > 1) or just the header if not.
! (Facility to retrieve BUFR messages now removed.)
!-----------------------------------------------------------------------

  IF (BufrStart > 1) THEN
    CALL VALARR(DISPL,NELREQ,SOURCE,ISECT1,DUMMY1,           &
                     DUMMY2,IEXTRA,VALUES,CNAM,              &
                     HELN//HEADER//CMSG(1:BufrStart-1),      &
                     IREP1,NSEND,NREP,ARRAY,NOBS,            &
                     NELEM,IOB,II,CSTR,CREPT,LFLAG,          &
                     LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)
  ELSE
    CALL VALARR(DISPL,NELREQ,SOURCE,ISECT1,DUMMY1,            &
                     DUMMY2,IEXTRA,VALUES,CNAM,HELN//HEADER,  &
                     IREP1,NSEND,NREP,ARRAY,NOBS,             &
                     NELEM,IOB,II,CSTR,CREPT,LFLAG,           &
                     LAT_SUBSCRIPT,LON_SUBSCRIPT,AREA,RPOLE)
  END IF

  LMID = (NSEND < NREP)

!-----------------------------------------------------------------------
! Save next loop counts to be processed by next entry
!-----------------------------------------------------------------------

  IF (LMID) THEN

    IF (LMSG) THEN
      WRITE(*,*)'MDB ERROR: In BUSRET: ARRAY NOT BIG ENOUGH ',  &
                'FOR 1 MESSAGE'
      IERR=16
      GOTO 999
    END IF

    IF (LFLAG) WRITE(*,*)'In BUSRET: No more room in ARRAY',IOB
    IF (NSEND+1 > NREP) THEN
      NXTREP=1
      NSEND=0
      IF (J2+1 > NTRIES) THEN
        NXTENT=1
        IF (J1+INXHRS > ISHR2) GOTO 999
        NXTIME=J1+INXHRS
      ELSE
        NXTIME=J1
        NXTENT=J2+1
      END IF
    ELSE
      NXTREP=NSEND+1
      NXTIME=J1
      NXTENT=J2
    END IF
    ISTAT=4
    IF (LFLAG) WRITE(*,*)'In BUSRET: NXTREP,NXTENT,NXTIME',&
        NXTREP,NXTENT,NXTIME
    GOTO 999
  ELSE
    IOB=II       !- changed from IOB=IOB+(NSEND-IREP1)+1
    NSEND=0
    NXTREP=1
    IREP1=1
    IF (IOB == NOBS .OR. (LMSG.AND.IOB > 0)) THEN
      IF (J2+1 > NTRIES) THEN
        NXTENT=1
        IF (J1+INXHRS > ISHR2) GOTO 999
        NXTIME=J1+INXHRS
      ELSE
        NXTIME=J1
        NXTENT=J2+1
      END IF
      ISTAT=4
      IF (LFLAG) THEN
        WRITE(*,*)'In BUSRET: Array full: NXTREP,NXTENT,NXTIME ', &
           NXTREP,NXTENT,NXTIME
      END IF
      GOTO 999
    END IF
  END IF

699     CONTINUE   ! end of loop over entries this hour

  IENT1=1

799   CONTINUE   ! end of loop over hours

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

IF (LFLAG) WRITE(*,*)'In BUSRET: EXIT: II,IOB,IOBS,NOBS', &
                II,IOB,IOBS,NOBS

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/', &
            I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',  &
            I2.2,1X,I2.2,'Z')

RETURN
END SUBROUTINE BUSRET
