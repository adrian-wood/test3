SUBROUTINE SYNRET(CTYPE,ITIME,IRTIM,AREA,CIDENT,ARRAY,NOBS, &
                  NELEM,IOBS,IDESC,NDES,IREPL,ISTAT,IERR,IDSK, &
                  CSTR,CREPRT,LFLAG,CHAREP,ORDER,LATEST,QCREQ, &
                  FOUND,VER,NEWMDBCALL,RPOLE,WANTSTNNAME,      &  !L!I!H
                  TRANGE,ELIST)                               !2.3

!-----------------------------------------------------------------------
!
! subroutine    : SYNRET
!
! purpose       : retrieval routine for SYNOPS, SHIPS and AIREPS
!
! description   : loop over index blocks covering requested period
!               : - loop over index entries in one block
!               : --- loop over reports in chain
!               : --- call expansion or get values otherwise,
!               : ----- loop over reports in one message
!               : ----- transfer values to users array
!
! data type(s)  : LNDSYN, SHPSYN, AIREPS, BOGUS
!
! called by     : MDB
!
! sub calls     : BITINDX   - bit indexes                             !B
!               : CENTURY   - century from 2-fig year                 !J
!               : DESFXY    - to convert BUFR descriptor to FXXYYY
!               : DT2HRS    - to convert date/time to century hours
!               : EOCHN     - end of chain
!               : HRS2DT    - to convert century hours to date/time
!               : IDMTCH    - to match index entries by identifier
!               : MAPRD     - to read map, index, data blocks
!               : READABRV  - to read abrreviated STNMAS list         !L
!               : READIDX   - to read element index from dataset      !B
!               : ROTAREA   - to rotate an ob round RPOLE             !I
!               : SETHED    - to set report string header info
!               : SORTCHR   - to sort index entries                !2.4
!               : SUBPER    - to select INCREMENT data.
!               : VALAREA   - to check ob in users area               !I
!               : VALUSR    - to fill users ARRAY                     !B
!
! arguments     :
!
! the first parameters have been validated in mdb so invalid
! combinations will not occur.
!
!   CTYPE       : char*8    (ip) : data subtype
!   ITIME(9)    : integer   (ip) : (1)  yyyy  start date
!                                : (2)  mm
!                                : (3)  dd
!                                : (4)  hhmm
!                                : (5)  yyyy  end date
!                                : (6)  mm
!                                : (7)  dd
!                                : (8)  hhmm
!                                : (9)  increment (not used here)
!   IRTIM(10)   : integer   (ip) : (1)  yyyy  earliest t.o.r (zero
!                                : (2)  mm    if not set)
!                                : (3)  dd
!                                : (4)  hh
!                                : (5)  min
!                                : (6)  yyyy  latest t.o.r
!                                : (7)  mm
!                                : (8)  dd
!                                : (9)  hh
!                                : (10) min
!   AREA(5)     : real      (ip) : (1)  0 for lat/lon area -2 for glob
!                                : (2)  top lh lat
!                                : (3)  top lh lon
!                                : (4)  bottom rh lat
!                                : (5)  bottom rh lon
!   CIDENT(50)  : char*9    (ip) : list of identifiers with '00000' at
!                                : end cident(1)='00000' for all data.
!   ARRAY       : real      (op) : array(nobs,nelem) users array.
!   NOBS        : integer (i/op) : number of observations
!   NELEM       : integer   (ip) : number of elements
!   IOBS        : integer        : number of next observation to be put
!                                : in array on entry. last ob on exit.
!   IDESC       : integer   (ip) : idesc(ndes) list of element index
!                                : numbers
!   NDES        : integer   (ip) : number of elements to find
!   IREPL       : integer   (ip) : array of counts against each element
!                                : name, for multiple occurrences     
!   ISTAT       : integer (i/op) : returns 4 for more data to come
!                                : if reset by the user-assumesa new
!                                : request
!   IERR        : integer   (op) : error indicator
!   IDSK(5)     : integer   (ip) : (2) record length
!                                : (3) ft number
!                                : (5) 1 for disk, 2 for tape
!   CSTR        : char*(*)  (op) : cstr(nobs) array for character elems
!   CREPRT      : char*(*)  (op) : creprt(nobs) array for raw reports +
!                                : header.
!   LFLAG       : logical   (ip) : true for diagnostic output
!   CHAREP      : logical   (ip) : true for character report only
!   ORDER       : char*1    (ip) : 'f'orwards or 'b'ackwards in time
!   LATEST      : logical   (ip) : true for latest reports only
!                                : (nothing to do with version!)
!   QCREQ       : logical   (ip) : true if qcflag required with every
!                                : element
!   FOUND       : logical   (ip) : found(*) array of keywords found
!                                : or not.
!   VER         : integer   (ip) : 1 for preferred, 2 for all
!   NEWMDBCALL  : logical   (iop) : TRUE if new MDB call (istat=0)
!   RPOLE(2)    : real      (ip) : rotated pole lat/lon coords        
!   WANTSTNNAME : logical   (ip) : TRUE if user wants station name   
!   TRANGE      : integer   (ip) : START TIME sub-period            
!   ELIST       : char*8    (ip) : Element index member name       
!
! REVISION INFO :
!
! $Workfile: synret.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 09/02/2011 17:22:26$
!
! Change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 17:22:26    Sheila Needham  Use
!       int2ch function
!  3    MetDB_Refresh 1.2         23/11/2010 09:41:33    Stan Kellett    old
!       revision info removed.
!       Function declarations removed as declared in Mod files.
!       argument NEWMDBCAL changed to INTENT(INOUT).
!  2    MetDB_Refresh 1.1         18/11/2010 14:08:04    Sheila Needham  Add
!       USE stmts
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE bitindx_mod
USE century_mod
USE desfxy_mod
USE dt2hrs_mod
USE eochn_mod
USE hrs2dt_mod
USE idmtch_mod
USE maprd_mod
USE readabrv_mod
USE readidx_mod
USE rotarea_mod
USE sethed_mod
USE sortchr_mod
USE subper_mod
USE valarea_mod
USE valusr_mod
USE int2ch_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)    ::  CTYPE
INTEGER,      INTENT(IN)    ::  ITIME(9)
INTEGER,      INTENT(IN)    ::  IRTIM(10)
REAL,         INTENT(IN)    ::  AREA(5) !- user-defined area    !I
CHARACTER(9), INTENT(IN)    ::  CIDENT(50)
INTEGER,      INTENT(INOUT) ::  NOBS !- NOBS in users array.
INTEGER,      INTENT(IN)    ::  NELEM !- NELEM in users array.
REAL,         INTENT(OUT)   ::  ARRAY(NOBS,NELEM) !- users array.
INTEGER,      INTENT(INOUT) ::  IOBS
INTEGER,      INTENT(IN)    ::  NDES
INTEGER,      INTENT(IN)    ::  IDESC(1+NDES) !- 1+NDES in case NDES=0
INTEGER,      INTENT(IN)    ::  IREPL(:)                        !B
INTEGER,      INTENT(INOUT) ::  ISTAT
INTEGER,      INTENT(OUT)   ::  IERR
INTEGER,      INTENT(IN)    ::  IDSK(5)
CHARACTER(*), INTENT(OUT)   ::  CSTR(NOBS)
CHARACTER(*), INTENT(OUT)   ::  CREPRT(NOBS)
LOGICAL,      INTENT(IN)    ::  LFLAG ! TRUE if TEST diagnostics wanted
LOGICAL,      INTENT(IN)    ::  CHAREP ! TRUE if raw report wanted
CHARACTER(1), INTENT(IN)    ::  ORDER
LOGICAL,      INTENT(IN)    ::  LATEST ! TRUE if latest ob wanted
LOGICAL,      INTENT(IN)    ::  QCREQ ! TRUE if QC flags wanted for each elem
LOGICAL,      INTENT(IN)    ::  FOUND(:) ! TRUE for each keyword if requested
INTEGER,      INTENT(IN)    ::  VER
LOGICAL,      INTENT(INOUT)    ::  NEWMDBCALL ! TRUE if new MetDB call (istat=0) !H
REAL,         INTENT(IN)    ::  RPOLE(2) !- rotated pole lat/lon coords !I
LOGICAL,      INTENT(IN)    ::  WANTSTNNAME ! TRUE if user wants station name !L
INTEGER,      INTENT(IN)    ::  TRANGE !- START TIME sub-period !P
CHARACTER(8), INTENT(IN)    ::  ELIST ! element index member name !2.3

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER,     PARAMETER ::  IHDLEN  = 44           !- len of header for rept text
INTEGER,     PARAMETER ::  INDLEN  = 23           !- index entry length
INTEGER,     PARAMETER ::  MXBLK   = 10000        !- max blksize             !1.15
INTEGER,     PARAMETER ::  MXCHLEN = 50           !- max no. of chained reports
INTEGER,     PARAMETER ::  MXINDX  = 27992/INDLEN !- max no. of index entries
INTEGER,     PARAMETER ::  MXOFL   = 7            !- max overflow blocks
INTEGER,     PARAMETER ::  MXRPLEN = 4096         !- max length of BUFR message

!-----------------------------------------------------------------------
! declare integers (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER      ::  BFEND  !- End of BUFR message                  !T
INTEGER      ::  BFSTART !- Start of BUFR message               !T
INTEGER      ::  DISPL(999)                                     !A
INTEGER      ::  F
INTEGER      ::  HDAY   !- header text day                      !F
INTEGER      ::  HHOUR  !- header text hour                     !F
INTEGER      ::  HMONTH !- header text month                    !F
INTEGER      ::  HYEAR  !- header text year                     !F
INTEGER      ::  I
INTEGER      ::  IBLOCK
INTEGER      ::  ICHL1
INTEGER      ::  ICHL2
INTEGER      ::  ICHL3
INTEGER      ::  ICHR   !- century hour for header text         !F
INTEGER      ::  ICMIN
INTEGER      ::  ID
INTEGER      ::  ID2                                            !F
INTEGER      ::  IDAT
INTEGER      ::  IDATHR
INTEGER      ::  IDAY
INTEGER      ::  IDHR(MXCHLEN)
INTEGER      ::  IENT1
INTEGER      ::  IFAIL
INTEGER      ::  IFLAG
INTEGER      ::  IH
INTEGER      ::  IH2                                            !F
INTEGER      ::  IHL
INTEGER      ::  IHOUR
INTEGER      ::  II
INTEGER      ::  ILOCD
INTEGER      ::  IM
INTEGER      ::  IM2                                            !F
INTEGER      ::  IMD
INTEGER      ::  IMIN(MXCHLEN)
INTEGER      ::  INDHR1
INTEGER      ::  INDHR2
INTEGER      ::  INOBS
INTEGER      ::  INUM
INTEGER      ::  INXBLK
INTEGER      ::  INXHRS
INTEGER      ::  IOB
INTEGER      ::  IOFSET
INTEGER      ::  IRECLN
INTEGER      ::  IREP1
INTEGER      ::  IRTIM1
INTEGER      ::  IRTIM2
INTEGER      ::  ISECT1(16)
INTEGER      ::  ISHR1
INTEGER      ::  ISHR2
INTEGER      ::  ISLOTHR
INTEGER      ::  ISTEP
INTEGER      ::  ITHR(3)
INTEGER      ::  ITIM1
INTEGER      ::  ITIM2
INTEGER      ::  ITIMND
INTEGER      ::  ITIMST
INTEGER      ::  ITORM
INTEGER      ::  ITRIES
INTEGER      ::  IY
INTEGER      ::  IY2                                            !F
INTEGER      ::  J1
INTEGER      ::  J2
INTEGER      ::  J3
INTEGER      ::  J5
INTEGER      ::  K
INTEGER      ::  KEPCHN
INTEGER      ::  KEPTIM   !- century minute of preferred ob.
INTEGER      ::  KOUNT
INTEGER      ::  LAST_RETRY=0 ! to avoid rereading the index  !1.18
!                        more than once if a report has length=0  !1.18
INTEGER      ::  LAT      !- index entry latitude (integer)
INTEGER      ::  LENCHR(1) !- lengths of strings in CHRELM      !L
INTEGER      ::  LON      !- index entry longitude (integer)
INTEGER      ::  MLEN
INTEGER      ::  MSGLEN(MXCHLEN)
INTEGER      ::  NAC
INTEGER      ::  NAMD
INTEGER      ::  NB2RD
INTEGER      ::  NBLOCK
INTEGER      ::  NCHN
INTEGER      ::  NCOR
INTEGER      ::  NELMIX   !- block number of element bit index
INTEGER      ::  NELREQ                                         !A
INTEGER      ::  NREC
INTEGER      ::  NREP
INTEGER      ::  NSEND
INTEGER      ::  NSQ
INTEGER      ::  NTOR
INTEGER      ::  NXTCHN
INTEGER      ::  NXTENT
INTEGER      ::  NXTIME
INTEGER      ::  NXTREP
INTEGER      ::  RC       !- General return code              !2.0
INTEGER      ::  REFVAL(999)                                    !A
INTEGER      ::  SCALE(999)                                     !A
INTEGER      ::  WIDTH(999)                                     !A
INTEGER      ::  X
INTEGER      ::  Y

!-----------------------------------------------------------------------
! declare reals (in alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  RLAT       !- index entry latitude (real)
REAL         ::  RLON       !- index entry longitude (real)
REAL         ::  RMDI=-9999999.0 !- missing data indicator
REAL         ::  ROT_LAT    !- rotated latitude                 !I
REAL         ::  ROT_LON    !- rotated longitude                !I

!-----------------------------------------------------------------------
! declare logicals (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      ::  AREA_FLAG ! TRUE if ob inside users area       !I
LOGICAL      ::  CHANRED ! TRUE if CHAIN is in array in core
LOGICAL      ::  ELIDXFOUND ! TRUE if READIDX finds an element index
LOGICAL      ::  FIRST=.TRUE. ! TRUE if first call to synret
LOGICAL      ::  INDXRED ! TRUE if index has been read in
LOGICAL      ::  ISPREF ! TRUE if preferred flag set
LOGICAL      ::  LCONT ! TRUE for a continuation call
LOGICAL      ::  LMIDMSG ! TRUE if returned in the middle of message
LOGICAL      ::  LOCDFG ! TRUE if BUFR sequence in block 2
LOGICAL      ::  NTRYCHK ! TRUE if IDENT & AREA checked
LOGICAL      ::  PASSED ! TRUE if time & TOR checks passed
LOGICAL      ::  POINTER_CHANGED ! true if data block split for this !1.18
!                                station since index first read    !1.18
LOGICAL      ::  WASPREF ! TRUE if ob was once preffered

!-----------------------------------------------------------------------
! declare characters (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(4)       :: BUFR
CHARACTER(4)       :: CCCC
CHARACTER(42)      :: CHRELM(1)                                  !L
CHARACTER(10000)   :: CINDX(12)                         !1.15!B
CHARACTER(MXRPLEN) :: CMSG(MXCHLEN)
CHARACTER(INDLEN)  :: CNTRY(3*MXOFL*MXINDX)
CHARACTER(INDLEN)  :: CTRAIL(MXCHLEN)
CHARACTER(INDLEN)  :: CTRAN(12*MXINDX)                      !O
CHARACTER(IHDLEN)  :: HEADER
CHARACTER(5)       :: HELN      ! report length before header    !T
CHARACTER(9)       :: LASTID                                     !C
CHARACTER(6)       :: LOCALD
CHARACTER(23)      :: MASKB                                   !2.4
CHARACTER(23)      :: MASKF                                   !2.4
CHARACTER(23)      :: MASKX                                      !2
CHARACTER(4)       :: SEVENS                                     !T
CHARACTER(9)       :: THISID                                     !C
CHARACTER(1)       :: XTYPE                                      !B

!-----------------------------------------------------------------------
! dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

COMMON /SURF1/CNTRY,CTRAIL,CTRAN,CMSG,CINDX                     !B
COMMON /SURF2/DISPL,REFVAL,SCALE,WIDTH                          !G

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

DATA HEADER /'0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '/

DATA MASKB  /'yzXXXXXXXXX XXXX       '/   ! order=backward   !2.4
DATA MASKF  /'YZXXXXXXXXX XXXX       '/   ! order=forward    !2.4
DATA MASKX  /'                     XX'/   ! order=none          !2
                                          ! (& time flag after
                                          ! (it if LATEST used)
                                          ! + sort on lat/lon   !M

!-----------------------------------------------------------------------
! ensure that all variables/arrays are still set on re-entry.
!-----------------------------------------------------------------------

SAVE

IF (LFLAG) THEN
  WRITE(*,'(/1X,''In MetDB subroutine SYNRET'' )')
  WRITE(*,'( 1X,''==========================''/)')
END IF

!-----------------------------------------------------------------------
! initialise variables.
!-----------------------------------------------------------------------

IERR  = 0
IOB   = IOBS-1   ! IOB --> last ob in users array (IOBS --> NEXT)
II    = 0                                                     !Q!P

!-----------------------------------------------------------------------
! First time only:  Initialise revision information, BUFR and SEVENS
!-----------------------------------------------------------------------

IF (FIRST) THEN
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'    !2.3
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'    !2.3
  FIRST=.FALSE.
END IF

!-----------------------------------------------------------------------
! check for continuation and if so, reset loop counters
!-----------------------------------------------------------------------

IF (ISTAT == 16) THEN
  ISTAT = 0
ELSE IF (ISTAT == 4) THEN
  ISHR1 = NXTIME          ! start time
  IENT1 = NXTENT          ! re-start entry number
  IREP1 = NXTREP          ! re-start report number
  ICHL1 = NXTCHN          ! re-start point in chained reports
  NSEND = NXTREP-1
  LCONT = .TRUE.          ! this is a continuation
ELSE
  CONTINUE
END IF

!=======================================================================
! if ISTAT=0, (first time in subroutine) start here.
!=======================================================================

IFLABEL1: &
IF (ISTAT == 0) THEN

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not
! output an error and exit subroutine.
!-----------------------------------------------------------------------

  IF (NELEM < NDES) THEN
    IERR = 16
    WRITE(*,*)'In SYNRET: MDB ERROR: You have not specified'    !G
    WRITE(*,*)'enough array elements. Try NELEM = ',NDES        !G
    GOTO 999
  END IF

!-----------------------------------------------------------------------
! Read map block to find dataset details. If LOCDFG is TRUE, then
! there is one or more local table D descriptors. Set NSQ=1.
! Last argument of call to MAPRD changed from 'MAPRD' to 'MAPR1'     !4
! to force MAPRD to look for local D entries.                        !4
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
             NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
             NREC,IRECLN,CMSG(KEPCHN+1),'MAPR1')               !4
  NSQ=0
  IF (LOCDFG) NSQ=1

!-----------------------------------------------------------------------
! read the element index from the the element index dataset. The subtype
! is now passed in order to select the correct element index.       !G!B
!-----------------------------------------------------------------------

  CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)                  !2.3

  IF (.NOT.ELIDXFOUND) THEN                                     !G
    WRITE(*,*)'In SYNRET: MDB ERROR: Cannot find element ', &   !G
    'index for subtype,elist = ',CTYPE,ELIST                  !2.3
    IERR=16                                                   !2.2
    GOTO 999                                                    !G
  END IF                                                        !G

  IF (LFLAG) THEN                                               !A
    WRITE(*,*)'In SYNRET: after call readidx, XTYPE=',XTYPE     !B
  END IF                                                        !A

!-----------------------------------------------------------------------
! convert request times to century-hours & century-minutes
!
! ITIM1,ITIM2 = users start,end times in century-hours.
! ITIMST,ITIMND = users start,end times in century-minutes.
!-----------------------------------------------------------------------

  ITIM1 = DT2HRS(ITIME(1),ITIME(2),ITIME(3),(ITIME(4)/100))
  ITIM2 = DT2HRS(ITIME(5),ITIME(6),ITIME(7),(ITIME(8)/100))
  ITIMST = ITIM1 * 60 + MOD(ITIME(4),100)
  ITIMND = ITIM2 * 60 + MOD(ITIME(8),100)

!-----------------------------------------------------------------------
! if the LATEST keyword is specified without a START TIME and an END
! TIME, we will search through the most recent 3 index blocks for the
! data. ITIMST is used later in the program so we need to adjust it to
! span a further two index block periods in the past.
!-----------------------------------------------------------------------

  IF (LATEST .AND. .NOT.FOUND(1) .AND. .NOT.FOUND(2)) THEN
    ITIMST=ITIMST-((2*INXHRS)*60)
  END IF

!-----------------------------------------------------------------------
! round times down to starts of index blocks
!
! ISHR1,ISHR2 = users start,end times as start of index block times.
! they will be the same if users time period contained within 1 index
! block.
!-----------------------------------------------------------------------

  INDHR1 = MOD(ITIME(4)/100+(24-ISLOTHR),INXHRS)                !E
  INDHR2 = MOD(ITIME(8)/100+(24-ISLOTHR),INXHRS)                !E
  ISHR1  = ITIM1 - INDHR1
  ISHR2  = ITIM2 - INDHR2

!-----------------------------------------------------------------------
! for SYNOP retrieval, the keyword LATEST can be used, either on
! its own or in combination with start and end times to access the
! latest reports. Either way, only look back a maximum of 3 index blocks
! to find the latest reports.
!
! Check to see if a time range is specified. If so, calculate the number
! of index blocks (NB2RD) the time range covers. If this is more than
! 3, output a warning message, and set the number if index to read to 3.
! If no time range is specified, just set the number of index blocks to
! read to 3.
!
! If the request has LATEST and a time range, set ISHR1 = ISHR2. If
! the request has LATEST and no time range, set ISHR2 = ISHR1. This
! corrects change !1.19 which did not work for cases when both
! LATEST and a time range were coded in the MDB request.           !2.0a
!-----------------------------------------------------------------------

IFLABEL2: &
  IF (LATEST) THEN
    IF (FOUND(1)) THEN      !- time range specified.
      NB2RD = (ISHR2-ISHR1)/INXHRS + 1
      IF (NB2RD > 3) THEN
        WRITE(6,9004)INXHRS*3,(ITIME(K),K=5,8)
        NB2RD = 3
      END IF
      ISHR1 = ISHR2                                          !2.0a
    ELSE                    !- no time range specified.
      NB2RD = 3
      ISHR2 = ISHR1                                          !2.0a
    END IF
  END IF IFLABEL2

!-----------------------------------------------------------------------
! keyword ORDER BACKWARDS can be used to retrieve data with the most
! recent reports being returned first. If that is the case here, swap
! ISHR1 and ISHR2 and set ISTEP.
!-----------------------------------------------------------------------

  IF (ORDER == 'B') THEN
    I     = ISHR1
    ISHR1 = ISHR2
    ISHR2 = I
    ISTEP = -INXHRS
  ELSE
    ISTEP = INXHRS
  END IF

!-----------------------------------------------------------------------
! keywords RECEIVED BETWEEN and RECEIVED BEFORE/AFTER are used to
! constrain the data returned to be received (time of receipt in the
! MetDB) within the given period. Convert requested times of receipt
! to century minutes.
!-----------------------------------------------------------------------

  IF (IRTIM(1) == 0) THEN    !- start time
    IRTIM1 = 0
  ELSE
    IRTIM1 = DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1 = (IRTIM1)*60+IRTIM(5)
  END IF

  IF (IRTIM(6) == 0) THEN    !- end time
    IRTIM2 = 0
  ELSE
    IRTIM2 = DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2 = (IRTIM2)*60+IRTIM(10)
  END IF

!-----------------------------------------------------------------------
! initialise loop counters.
!-----------------------------------------------------------------------

  IENT1   = 1          ! entry number
  IREP1   = 1          ! report within message
  NXTREP  = 1
  NSEND   = 0
  LMIDMSG = .FALSE.    ! NOT part-way through a message
  LCONT   = .FALSE.
  INDXRED = .FALSE.    ! index has NOT been read
  NTRYCHK = .FALSE.    ! entry has NOT been checked
  CHANRED = .FALSE.    ! chain has NOT been read
  LASTID  = ' '
  PASSED  = .FALSE.

END IF IFLABEL1 ! end of ISTAT=0 if block.

ISTAT = 0

!=======================================================================
!
! after the first time, start here.
!
! the rest of synret consists of 3 nested loops, over index blocks,
! entries in index block & reports in chain attached to entry.
!
! loop over index blocks (forwards or backwards, or all at once if
! latest). at index level check for valid index block time
!
!=======================================================================

DOLABEL1: &
DO J1 = ISHR1, ISHR2, ISTEP

!-----------------------------------------------------------------------
! Check whether index block has already been read (INDXRED=.TRUE.)
!
! Check if LATEST has been requested. If so, ISHR1=ISHR2, so that we
! only go round this loop once (work done further down)
!-----------------------------------------------------------------------

IFLABEL3: &
  IF (.NOT.INDXRED) THEN

IFLABEL4: &
    IF (LATEST) THEN

!-----------------------------------------------------------------------
! LATEST requested. Read up to 3 index blocks, starting with the latest.
! Set IHL equal to J1 which is equal to ISHR1 & ISHR2 which are set to
! the users end request time i.e. latest index block.
!
! Loop round NB2RD (max=3). IBLOCK=index block we wish to read. 2 is
! added to get us past the map block, and NSQ is 1 if there is a local
! D to get past, 0 if not. Call MAPRD to read the index block (IBLOCK).
!-----------------------------------------------------------------------

      IHL   = J1     !- same as ISHR1 & ISHR2
      KOUNT = 0      !- initialise counter.

DOLABEL2: &
      DO J2 = 1, NB2RD

        IBLOCK = MOD(IHL/INXHRS,INXBLK)+2+NSQ

        CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
                   LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
                   NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')

!-----------------------------------------------------------------------
! Check that correct data/time has been read. Convert date/time
! requested from century-hours to day/hour and compare with day/hour
! returned by MAPRD. If they dont match there is no data for the
! requested period, output a message to the user. If they do match,
! keep the century-hour of the index block in ITHR.
!-----------------------------------------------------------------------

        IDAY  = IDATHR/256
        IHOUR = IDATHR - IDAY * 256
        CALL HRS2DT(IY,IM,ID,IH,IHL)

IFLABEL5: &
        IF (ID /= IDAY.OR.IH /= IHOUR) THEN
          CALL HRS2DT(IY2,IM2,ID2,IH2,IHL+INXHRS-1)             !F
          WRITE(6,9003)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2    !1.16b
        ELSE
          ITHR(J2) = IHL  ! match! keep century hour of index blk.

!-----------------------------------------------------------------------
! Index block matched. Now loop through index entries (INUM) in this
! index block.
!
! Removed index entry rejection if lat or lon is missing              !K
!
! Check for each index entry an identifier match (IDMTCH) to see if
! they match those in the users request string
!
! If IDMTCH matches an identifier, it returns with a return code
! RC=0, otherwise it returns with RC=1.                             !2.0
!-----------------------------------------------------------------------

DOLABEL3: &
          DO J3=1,INUM

!-----------------------------------------------------------------------
! safety check. Reject index entry if data block or record number being
! pointed to are  <=  0
!-----------------------------------------------------------------------

            NBLOCK=ICHAR(CTRAN(J3)(22:22))*256+ &               !C
                   ICHAR(CTRAN(J3)(23:23))                      !C

            IBLOCK=1+NSQ+INXBLK+NBLOCK                          !C

            NREC=ICHAR(CTRAN(J3)(20:20))*256+ &                 !C
                 ICHAR(CTRAN(J3)(21:21))                        !C

            IF (NBLOCK <= 0 .OR. NREC <= 0) THEN                !R
              IF (LFLAG) THEN                                   !C
                WRITE(*,*)'In SYNRET: Bad Index Entry!!'        !C
              END IF                                            !C
              GOTO 80                                           !C
            END IF                                              !C

!-----------------------------------------------------------------------
! check identifier.
!-----------------------------------------------------------------------

            CALL IDMTCH(CTRAN(J3),THISID,CIDENT,.FALSE.,RC)   !2.0
            IF (RC /= 0) GOTO 80                              !2.0

!-----------------------------------------------------------------------
! If subtype is not SHPSYN then do an area selection/rejection.       !O
!
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If =0 then normal grid
!                                            =1 then rotated grid
!                                            =anything else then no chk
!-----------------------------------------------------------------------

            AREA_FLAG = .TRUE.                                  !O

IFLABEL6: &
            IF (CTYPE(1:6) /= 'SHPSYN') THEN                    !O

              LAT=ICHAR(CTRAN(J3)(13:13))*256+ &                !C
                  ICHAR(CTRAN(J3)(14:14))                       !C
              LON=ICHAR(CTRAN(J3)(15:15))*256+ &                !C
                  ICHAR(CTRAN(J3)(16:16))                       !C

              IF (LAT >= 32768) LAT=LAT-65536                   !C
              IF (LON >= 32768) LON=LON-65536                   !C

              RLAT=LAT*0.01                                     !I
              RLON=LON*0.01                                     !I

              IF (AREA(1) == 1.0) THEN                          !I
                CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)   !I
                CALL VALAREA(ROT_LAT,ROT_LON,AREA, &            !I
                             AREA_FLAG,LFLAG)                   !I
              ELSE IF (AREA(1) == 0.0) THEN                     !I
                CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)    !I
              ELSE                                              !I
                AREA_FLAG=.TRUE.                                !I
              END IF                                            !I

            END IF IFLABEL6 !- ctype

IFLABEL7: &
            IF (AREA_FLAG) THEN                                 !I

!-----------------------------------------------------------------------
! There was a match, so we want to keep this index entry. Increment the
! counter and put the index entry into CNTRY. Set BYTE at end of
! identifier field in the index entry, so that entries can be sorted on
! ident and time. the left-hand 2 bits are set from J2 (can be 1,2 or
! 3). The next 3 bits are from the hour in the 1st BYTE. It is assumed
! that this is <6 (i.e. 6-hr index blocks)
!-----------------------------------------------------------------------

              KOUNT=KOUNT+1
              CNTRY(KOUNT)=CTRAN(J3)
              IFLAG=MOD(ICHAR(CNTRY(KOUNT)(1:1)),8)
              CNTRY(KOUNT)(11:11)=int2ch(J2*64+IFLAG*8)

!-----------------------------------------------------------------------
! if only one ident requested & whole strings match, stop looking     !B
! It is contained in this index entry, so no need to search any more.
! CIDENT(1) will have the identifier, CIDENT(2)='00000' as
! this denotes the end of the selection list. If there was more than 1
! station requested CIDENT(2) would not equal '00000'
!-----------------------------------------------------------------------

              IF (CIDENT(2) == '00000' .AND. &
              CIDENT(1)(1:8) == CTRAN(J3)(3:10)) GO TO 131      !B

            END IF IFLABEL7

80              CONTINUE  !- end of loop over index entries (J3 do loop)
          END DO DOLABEL3

        END IF IFLABEL5 !- end of index block match if block.

!-----------------------------------------------------------------------
! Decrement index block hours. Will next look at previous index block.
!-----------------------------------------------------------------------

        IHL=IHL-INXHRS

130         CONTINUE       !- end of loop over index blocks (J2 do loop)
      END DO DOLABEL2
131         CONTINUE
      ITRIES=KOUNT

!-----------------------------------------------------------------------
! Not LATEST report requested. Read the index block (IBLOCK) using
! MAPRD. 2 and NSQ are added to get us past the map block and the local
! D block if there is one.
!-----------------------------------------------------------------------

    ELSE    ! .not. latest

      IBLOCK = MOD(J1/INXHRS,INXBLK)+2+NSQ
      CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
                 LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
                 NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')

!-----------------------------------------------------------------------
! Check that correct data/time has been read. If not, output a message
! to the user and GOTO 799 to read the next index block.
!-----------------------------------------------------------------------

      IDAY  = IDATHR/256
      IHOUR = IDATHR-IDAY * 256
      CALL HRS2DT(IY,IM,ID,IH,J1)
      IF (ID /= IDAY.OR.IH /= IHOUR) THEN
        CALL HRS2DT(IY2,IM2,ID2,IH2,J1+ISTEP-1)                 !F
        WRITE(6,9003)CTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2      !1.16b
        GOTO 799  ! NEXT INDEX BLOCK
      END IF

!-----------------------------------------------------------------------
! Index block matched! Loop round index entries in this index block.
!
! Removed index entry rejection if lat or lon is missing              !K
!
! Try and match identifier. See note in LATEST section about IDMTCH.
! Put valid index entries in CNTRY array.
!-----------------------------------------------------------------------

      ITRIES=0
DOLABEL4: &
      DO J3=1,INUM

!-----------------------------------------------------------------------
! safety check. Reject index entry if data block or record number being
! pointed to are  <=  0
!-----------------------------------------------------------------------

        NBLOCK=ICHAR(CTRAN(J3)(22:22))*256+ &                   !C
        ICHAR(CTRAN(J3)(23:23))                                 !C
        IBLOCK=1+NSQ+INXBLK+NBLOCK                              !C
        NREC=ICHAR(CTRAN(J3)(20:20))*256+ &                     !C
        ICHAR(CTRAN(J3)(21:21))                                 !C

        IF (IBLOCK <= 0 .OR. NREC <= 0) THEN                    !C
          IF (LFLAG) WRITE(*,*)'In SYNRET: Bad Index Entry!!'   !C
          GOTO 81                                               !C
        END IF                                                  !C

!-----------------------------------------------------------------------
! check identifier.
!-----------------------------------------------------------------------

        CALL IDMTCH(CTRAN(J3),THISID,CIDENT,.FALSE.,RC)       !2.0
        IF (RC /= 0) GOTO 81                                  !2.0

!-----------------------------------------------------------------------
! If subtype is not SHPSYN then do an area selection/rejection.       !O
!
! Check if we want to rotate the Lat and Long values from the index
! entry for a rotated grid and then check it falls within the grid area
! or if we just want to check the observation lies within a normal lat
! long area or whether we want all obs (no checking)
! This can be done by looking at AREA(1). If =0 then normal grid
!                                            =1 then rotated grid
!                                            =anything else then no chk
!-----------------------------------------------------------------------

        AREA_FLAG = .TRUE.                                      !O

IFLABEL8: &
        IF (CTYPE(1:6) /= 'SHPSYN') THEN                        !O

          LAT=ICHAR(CTRAN(J3)(13:13))*256+ &                    !C
              ICHAR(CTRAN(J3)(14:14))                           !C
          LON=ICHAR(CTRAN(J3)(15:15))*256+ &                    !C
              ICHAR(CTRAN(J3)(16:16))                           !C

          IF (LAT >= 32768) LAT=LAT-65536                       !C
          IF (LON >= 32768) LON=LON-65536                       !C

          RLAT=LAT*0.01                                         !I
          RLON=LON*0.01                                         !I

          IF (AREA(1) == 1.0) THEN                              !I
            CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)       !I
            CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)  !I
          ELSE IF (AREA(1) == 0.0) THEN                         !I
            CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)        !I
          ELSE                                                  !I
            AREA_FLAG=.TRUE.                                    !I
          END IF                                                !I

        END IF IFLABEL8 !- ctype

        IF (AREA_FLAG) THEN                                     !I

          ITRIES=ITRIES+1
          CNTRY(ITRIES)=CTRAN(J3)
        END IF                                                  !I

   81       CONTINUE  !- end of loop over index entries (J3 do loop)
      END DO DOLABEL4

    END IF IFLABEL4     !- end of LATEST/.NOT.LATEST if block.

!-----------------------------------------------------------------------
! Sort entries into identifier & time order and set INDXRED to TRUE to
! say we've read an index block.  Sort either backward or forward on
! time as requested.                                               !2.4
!-----------------------------------------------------------------------

    IF (ORDER == 'B') THEN                                   !2.4
      CALL SORTCHR(CNTRY,INDLEN,ITRIES,MASKB)                !2.4
    ELSE IF (ORDER == 'F') THEN                              !2.4
      CALL SORTCHR(CNTRY,INDLEN,ITRIES,MASKF)                !2.4
    ELSE IF (ORDER == ' ') THEN                                 !2
      CALL SORTCHR(CNTRY,INDLEN,ITRIES,MASKX)                   !2
    END IF                                                   !2.4
    INDXRED = .TRUE.

  END IF IFLABEL3      ! end of .NOT.INDXRED block.

!=======================================================================
!
! WEVE NOW READ IN AN INDEX BLOCK AND SELECTED ENTRIES FROM IT.
! LOOP OVER THE ENTRIES SELECTED, DOING FURTHER TIME CHECKS WHILE
! GOING DOWN EACH CHAIN. STRUCTURE OF 23-BYTE INDEX ENTRY SHOWN BELOW.
!
!
! -----------------------------------------------------------------
! : HOUR : MINUTE :TTAAII: COR : CCCC : NOBS :LATITUDE:LONGITUDE
! :      :        :      : NUM :      :      :        :
! -----------------------------------------------------------------
!     1      2      3-6     7    8-11    12    13-14     15-16
!
! -----------------------------
! : FLAGS : TOR : REC : BLOCK :        *************
! :       :     : NUM : NUMBER:        ** TRAILER **
! -----------------------------        *************
!   17     18-19 20-21  22-23
!
!=======================================================================

DOLABEL5: &
  DO J2 = IENT1, ITRIES

!-----------------------------------------------------------------------
! get lat and lon from index entry.
!-----------------------------------------------------------------------

    LAT=ICHAR(CNTRY(J2)(13:13))*256+ICHAR(CNTRY(J2)(14:14))    !B
    LON=ICHAR(CNTRY(J2)(15:15))*256+ICHAR(CNTRY(J2)(16:16))    !B
    IF (LAT >= 32768) LAT=LAT-65536                            !B
    IF (LON >= 32768) LON=LON-65536                            !B

    IF (LAT /= -32768) THEN                                    !B
      RLAT=0.01*REAL(LAT)                                      !B
      RLON=0.01*REAL(LON)                                      !B
    ELSE                                                       !B
      RLAT=-9999999.0                                          !B
      RLON=-9999999.0                                          !B
    END IF                                                     !B

!-----------------------------------------------------------------------
! for latest requests we only need one report for a given id so
! check if a report with this id has already been returned
! (if more than one index block is involved the list of index entries
! will probably include a given identifier more than once!)
!-----------------------------------------------------------------------

IFLABEL9: &
    IF (.NOT.NTRYCHK) THEN
      THISID = CNTRY(J2)(3:11)
      IF (LATEST) THEN
        IF (THISID(1:6) == LASTID(1:6)) THEN                    !E
          IF (PASSED) GOTO 699
        ELSE
          LASTID = THISID
          PASSED = .FALSE.
        END IF
      END IF

!-----------------------------------------------------------------------
! get number of reports in the chain. Set it to max if chain too long.
!-----------------------------------------------------------------------

      NCHN = ICHAR(CNTRY(J2)(12:12))

      IF (NCHN > MXCHLEN) WRITE(*,*)'SYNRET: MDB ', &       !1.16a
         'WARNING - MAX CHAIN LENGTH EXCEEDED'              !1.16a

!-----------------------------------------------------------------------
! get data block number (NBLOCK) from index entry. Calculate actual
! block number (IBLOCK) from start of dataset. Then get record number
! of the data in this data block from the index entry.
!-----------------------------------------------------------------------

      NBLOCK=ICHAR(CNTRY(J2)(22:22))*256+ICHAR(CNTRY(J2)(23:23))
      IBLOCK=1+NSQ+INXBLK+NBLOCK
      NREC=ICHAR(CNTRY(J2)(20:20))*256+ICHAR(CNTRY(J2)(21:21))

!-----------------------------------------------------------------------
! if the user wants a station name (only available for subtype LNDSYN)
! call READABRV, passing the station number. READABRV will read the
! abbreviated list into memory on first call, match station number
! with station name and return station name in CHRELM(1).
!-----------------------------------------------------------------------

      IF (WANTSTNNAME) THEN                                     !L
        CALL READABRV(CNTRY(J2)(3:7),CHRELM(1))                 !L
        LENCHR(1)=42                                            !L
      END IF                                                    !L

!-----------------------------------------------------------------------
! if latest report wanted, get century hour from flag to get correct
! index block.
!-----------------------------------------------------------------------

      IF (LATEST) THEN
        I = ICHAR( CNTRY(J2)(11:11) )/64
        IDAT = ITHR(I)
        CALL HRS2DT(IY,IM,IDAY,IHOUR,IDAT)
      ELSE
        IDAT=J1             !- century-hour
      END IF
      NTRYCHK = .TRUE.
    END IF IFLABEL9         !- end of .NOT.NTRYCHK block

!=======================================================================
!
! READ IN REPORTS IN THIS CHAIN (SAME IDENTIFIER, BUT MAYBE DIFFERENT
! TIMES, MAYBE DIFFERENT VERSIONS).  GET THEM ALL IN CORE AT ONCE SO
! THAT WE CAN FOLLOW THE CHAIN EITHER FORWARDS OR BACKWARDS,
! DOING TIME CHECKS AS THE CHAIN IS READ IN.
! CHECK (IF REQUIRED) FOR PREFERRED REPORT IN THIS LOOP.
!
!=======================================================================

!-----------------------------------------------------------------------
! if the chain has not already been read in, read it in.
!-----------------------------------------------------------------------

IFLABEL10: &
    IF (.NOT.CHANRED) THEN

      KEPCHN=0            ! zero count of obs wanted in chain
      KEPTIM=0            ! zero minutes for check to return only
                          ! one preferred report for any one time

!-----------------------------------------------------------------------
! loop over number of reports in chain, call MAPRD to read the
! correct data block (IBLOCK), report position in the data block pointed
! to by NREC.
!-----------------------------------------------------------------------

  650       CONTINUE

IFLABEL11: &
      IF (NBLOCK > 0 .AND. NREC > 0 .AND. &                 !1.16a
          KEPCHN < MXCHLEN) THEN                            !1.16a

  651         CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, & !1.18
                   LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
                   NREC,IRECLN,CMSG(KEPCHN+1),'MSGRD')

!-----------------------------------------------------------------------
! Returns from MAPRD: IRECLN : length of report, CMSG : the report.
!  If a zero length is returned, this means the data block has    !1.18
! split since the index was read.  Read the index again (this     !1.18
! means reading a different index first; otherwise MAPRD just     !1.18
! returns what it has in core!), look for the current entry &     !1.18
! replace the pointer that led to the zero length, resetting      !1.18
! block & record number to retry the MAPRD call above.            !1.18
! (Only one retry allowed - one should be enough.)                !1.18
!-----------------------------------------------------------------------
IFLABEL12: &
        IF (IRECLN == 0 .AND. J2 /= LAST_RETRY) THEN        !1.18

! If the index has already been reread, see if this entry was     !1.18
! reread with a different pointer.  If so, no need to read again. !1.18

          POINTER_CHANGED=.FALSE.                           !1.18
          IF (LAST_RETRY > 0) THEN                          !1.18
            J3=1
            DO WHILE (CTRAN(J3)(3:7) /= CNTRY(J2)(3:7) &    !1.18
                      .AND. J3 <= INUM)                     !1.18
              J3=J3+1                                       !1.18
            END DO                                          !1.18

            IF (CTRAN(J3)(20:23) /= CNTRY(J2)(20:23)) THEN  !1.18
              POINTER_CHANGED=.TRUE.                        !1.18
            END IF                                          !1.18
          END IF                                            !1.18

! If the index hasn't been reread yet or this entry has the same  !1.18
! pointer, read it in again.                                      !1.18
! First read the next index block (not wanted - see above)        !1.18

IFLABEL13: &
          IF (LAST_RETRY == 0.OR..NOT.POINTER_CHANGED) THEN !1.18
            IBLOCK=MOD(J1/INXHRS+1,INXBLK)+2+NSQ            !1.18
            CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN, & !1.18
               LFLAG,LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,     & !1.18
               IDATHR,NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')   !1.18

! Now reread the index concerned                                  !1.18

            IBLOCK=MOD(J1/INXHRS,INXBLK)+2+NSQ              !1.18
            CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN, & !1.18
               LFLAG,LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,     & !1.18
               IDATHR,NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')   !1.18

! Look for the current index entry (with different pointer)       !1.18

            J3=1
            DO WHILE (CTRAN(J3)(3:7) /= CNTRY(J2)(3:7) &    !1.18
                      .AND. J3 <= INUM)                     !1.18
              J3=J3+1                                       !1.18
            END DO                                          !1.18
            IF (J3 <= INUM) POINTER_CHANGED=.TRUE.
          END IF IFLABEL13                                 !1.18

! If the entry's found (it should be!) reset the pointer & retry  !1.18

          IF (POINTER_CHANGED) THEN                         !1.18
            CNTRY(J2)=CTRAN(J3)                             !1.18
            NCHN=ICHAR(CNTRY(J2)(12:12))                    !1.18
            NBLOCK=ICHAR(CNTRY(J2)(22:22))*256 &            !1.18
                  +ICHAR(CNTRY(J2)(23:23))                  !1.18
            IBLOCK=1+NSQ+INXBLK+NBLOCK                      !1.18
            NREC=ICHAR(CNTRY(J2)(20:20))*256 &              !1.18
                +ICHAR(CNTRY(J2)(21:21))                    !1.18
            LAST_RETRY=J2                                   !1.18
            GO TO 651                                       !1.18
          END IF                                            !1.18
        END IF IFLABEL12                                   !1.18

!-----------------------------------------------------------------------
! As the reports are chained, there is a 23-byte index entry at the end
! of each report which points to the next report in the chain. The
! actual length of the report, therfore, is MSGLEN, as INDLEN=23.
! Put the 23-byte chained index entry in CTRAIL.
!-----------------------------------------------------------------------

        MSGLEN(KEPCHN+1)=IRECLN-INDLEN
        CTRAIL(KEPCHN+1)=CMSG(KEPCHN+1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! get the data block and record number of the next report in the chain
! from the 23-byte chained index entry.
!-----------------------------------------------------------------------

        NBLOCK=ICHAR(CTRAIL(KEPCHN+1)(22:22))*256 &
              +ICHAR(CTRAIL(KEPCHN+1)(23:23))

        IF (CTYPE(1:5) == 'BOGUS') NBLOCK = 0                   !S

        IBLOCK=1+NSQ+INXBLK+NBLOCK
        NREC=ICHAR(CTRAIL(KEPCHN+1)(20:20))*256 &
            +ICHAR(CTRAIL(KEPCHN+1)(21:21))

        IF (CTYPE(1:5) == 'BOGUS') NREC = 0                     !S

!-----------------------------------------------------------------------
! get the data time of this report from the chained index entry and
! compare with the start and end times of the users request. If it is
! out of range, let the next chained report overwrite this one.
! ICMIN = time of report in century minutes. ITIMST and ITIMND = users
! start and end request times in century minutes.
!-----------------------------------------------------------------------

        IDHR(KEPCHN+1)=MOD(ICHAR(CTRAIL(KEPCHN+1)(1:1)),64)
        IMIN(KEPCHN+1)=ICHAR(CTRAIL(KEPCHN+1)(2:2))

        ICHR=DT2HRS(IY,IM,IDAY,IHOUR)+IDHR(KEPCHN+1)            !F
        ICMIN=ICHR*60+IMIN(KEPCHN+1)                            !F

!-----------------------------------------------------------------------
! check to see that users INCREMENT keword request is satisfied. If
! FOUND(4) is TRUE, user has specified the keyword INCREMENT. Call
! SUBPER, passing users start time, sub-period, increment period in
! hours (ITIME(9)), users end time and return point. If the increment
! is not matched, GOTO 650.
!-----------------------------------------------------------------------

        IF (FOUND(4)) THEN
          CALL SUBPER(ITIMST,TRANGE,ITIME(9),ITIMND,ICMIN,RC) !2.0
          IF (RC /= 0) GOTO 650                               !2.0
        END IF

!-----------------------------------------------------------------------
! If subtype is SHPSYN then do an area selection/rejection. See
! area selection on index entries for info.                           !O
!-----------------------------------------------------------------------

IFLABEL14: &
        IF (CTYPE(1:6) == 'SHPSYN') THEN

          LAT=ICHAR(CTRAIL(KEPCHN+1)(13:13))*256+ &
              ICHAR(CTRAIL(KEPCHN+1)(14:14))
          LON=ICHAR(CTRAIL(KEPCHN+1)(15:15))*256+ &
              ICHAR(CTRAIL(KEPCHN+1)(16:16))

          IF (LAT >= 32768) LAT=LAT-65536
          IF (LON >= 32768) LON=LON-65536

          RLAT=LAT*0.01
          RLON=LON*0.01

          IF (AREA(1) == 1.0) THEN
            CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)
            CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)
          ELSE IF (AREA(1) == 0.0) THEN
            CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)
          ELSE
            AREA_FLAG=.TRUE.
          END IF

          IF (.NOT.AREA_FLAG) GOTO 650

        END IF IFLABEL14 !- ctype

!-----------------------------------------------------------------------
! data time check passed OK. Now see if time of receipt is in range.
! chained index entry holds time of receipt in minutes after slot base
! hour.
!
! Check to see if NTOR is meant to be negative. NTOR is read from
! 2 bytes which means that the maximum positive value NTOR can have
! is 32768. If NTOR is greater than this value, it must be converted
! into a negative number. This is done simply by subtracting the max
! possible value that can be held  in a 2 byte integer when the sign
! bit is ignored (i.e. 2 to the power 16) from NTOR.                  !H
!-----------------------------------------------------------------------

IFLABEL15: &
        IF (ICMIN >= ITIMST .AND. ICMIN <= ITIMND) THEN

          NTOR=ICHAR(CTRAIL(KEPCHN+1)(18:18))*256 &
              +ICHAR(CTRAIL(KEPCHN+1)(19:19))

          IF (NTOR > 32768) THEN                                !H
            NTOR=NTOR-65536                                     !H
          END IF                                                !H

          ITORM=IDAT*60+NTOR

!-----------------------------------------------------------------------
! ISPREF = report is the prefered report.
! WASPREF = report was once the preferred report. Keep both these as
! logical variables.
!-----------------------------------------------------------------------

          ISPREF=ICHAR(CTRAIL(KEPCHN+1)(17:17))/128 == 1
          WASPREF=MOD(ICHAR(CTRAIL(KEPCHN+1)(17:17))/32,2) == 1

!-----------------------------------------------------------------------
! check time of receipt & preferred flags, only keeping report if it
! was received in time slot and (if ver=1, preferred report requested),
! it is preferred or was preferred at the cutoff time.  if ver=1, only
! one report is wanted for any time, but two or more obs for that time
! may have been preferred before a cutoff, so keep report time when a
! preferred report is found to avoid returning more than one. (reports
! for the same time are consecutive in the chain). Change check (!H)
! slightly from  >  to  >=
!-----------------------------------------------------------------------

IFLABEL16: &
          IF (ITORM < IRTIM1) THEN
            CONTINUE         ! received too early
          ELSE IF (IRTIM2 > 0 .AND. ITORM >= IRTIM2) THEN       !H
            CONTINUE         ! received too late
          ELSE IF (VER == 1.AND.IRTIM2 == 0.AND..NOT.ISPREF) THEN
            CONTINUE         ! not finally preferred
          ELSE IF (VER == 1.AND.IRTIM2 > 0.AND..NOT.WASPREF) THEN
            CONTINUE         ! not preferred at cutoff time
          ELSE IF (VER == 1.AND.ICMIN == KEPTIM) THEN
            CONTINUE         ! only one pref wanted for this time
          ELSE
            KEPCHN=KEPCHN+1  ! keep this ob if time checks passed
            KEPTIM=ICMIN     ! keep time to check if not 1st pref
          END IF IFLABEL16

        END IF IFLABEL15 !- time of report in range if block.

        GO TO 650                                               !C

      END IF IFLABEL11   !- end of loop over reports in chain.

!-----------------------------------------------------------------------
! decide whether to loop forwards or backwards over chain.
! order='f' is forwards in time i.e start at the end of the chain
! as it is in the data base  (loop skipped if kepchn=0)
!-----------------------------------------------------------------------

      IF (LATEST.OR.ORDER == 'B') THEN
        ICHL1 = 1                      ! from start
        ICHL2 = KEPCHN                 ! to end of chain
        ICHL3 = 1                      ! with increment 1
      ELSE
        ICHL1 = KEPCHN                 ! from end of chain
        ICHL2 = 1                      ! to start
        ICHL3 = -1                     ! with increment -1
      END IF

      CHANRED = .TRUE.                 ! index entry chain read

    END IF IFLABEL10                   ! end of .NOT.CHANRED block

!=======================================================================
!
! LOOP OVER MESSAGES IN CHAIN (THOSE NOT REJECTED BY PREF CHECK ABOVE)
!
!=======================================================================

DOLABEL6: &
    DO J5 = ICHL1, ICHL2, ICHL3

IFLABEL17: &
    IF (.NOT.LMIDMSG) THEN
      PASSED = .TRUE.

!-----------------------------------------------------------------------
! get collecting centre, amend and /cor numbers from chained index
! entry for the header.
!-----------------------------------------------------------------------

      CCCC  = CTRAIL(J5)(8:11)
      NAC   = ICHAR(CTRAIL(J5)(7:7))
      NAMD  = NAC/16
      NCOR  = MOD(NAC,16)

!-----------------------------------------------------------------------
! find 'BUFR' in chained report. There will be the raw report text
! before 'BUFR' and the length of this is LEN. IOFSET is the position
! of the start of the BUFR message. Get the number of observations
! (subsets) in the BUFR message from section 3, octets 5-6. There will
! be one BUFR message per SYNOP for on-line SYNOP data.
! Also get the local D sequence number to identify the message type.
!-----------------------------------------------------------------------

      MLEN = INDEX(CMSG(J5),BUFR)-1     ! length of report text.
      IOFSET = MLEN + 1
      INOBS=ICHAR(CMSG(J5)(IOFSET+26:IOFSET+26))*256 &          !A
           +ICHAR(CMSG(J5)(IOFSET+27:IOFSET+27))                !A

      ILOCD=ICHAR(CMSG(J5)(IOFSET+29:IOFSET+29))*256 &          !B
           +ICHAR(CMSG(J5)(IOFSET+30:IOFSET+30))                !B
      CALL DESFXY(ILOCD,F,X,Y)
      WRITE(LOCALD,'(I1,I2.2,I3.3)')F,X,Y
      IF(LFLAG)THEN
        PRINT*,'LOCALD D FROM MESSAGE',ILOCD,LOCALD
        PRINT*,CMSG(J5)(IOFSET:IOFSET+30)
      END IF

      BFSTART = IOFSET                    ! start of BUFR       !T
      BFEND   = INDEX(CMSG(J5),SEVENS)+3  ! end of BUFR         !T

!-----------------------------------------------------------------------
! Initialise array of elements from section 1 of the BUFR message.
!-----------------------------------------------------------------------

      ISECT1(1) = ICHAR(CMSG(J5)(IOFSET+16:IOFSET+16))          !J
      ISECT1(1) = ISECT1(1)+CENTURY(ISECT1(1))                  !J
      ISECT1(2) = ICHAR(CMSG(J5)(IOFSET+17:IOFSET+17))
      ISECT1(3) = ICHAR(CMSG(J5)(IOFSET+18:IOFSET+18))
      ISECT1(4) = ICHAR(CMSG(J5)(IOFSET+19:IOFSET+19))
      ISECT1(5) = ICHAR(CMSG(J5)(IOFSET+20:IOFSET+20))
      ISECT1(7) = NAMD
      ISECT1(8) = NCOR
      ISECT1(9) = ICHAR(CMSG(J5)(IOFSET+8:IOFSET+8))*256 + &
                  ICHAR(CMSG(J5)(IOFSET+9:IOFSET+9))
      IF (ISECT1(9) >= 65535) ISECT1(9)=INT(RMDI)

      ISECT1(10) = MOD((ICHAR(CTRAIL(J5)(17:17))/64),2)         !M

!-----------------------------------------------------------------------
! put the total length of the report (report length (LEN) + length of
! browsable header (IHDLEN)) into char*5 variable HELN, and initialise
! the header (SETHED).  Report text may be any one of many elements
! so always do this part. Calculate the century hour and add IDHR. This
! is because IHOUR+IDHR can take us on to a new day.
!-----------------------------------------------------------------------

      ICHR=DT2HRS(IY,IM,IDAY,IHOUR)+IDHR(J5)                    !F
      CALL HRS2DT(HYEAR,HMONTH,HDAY,HHOUR,ICHR)                 !F

!-----------------------------------------------------------------------
! If SHPSYN, get the latitude an longitude from the trailor. It can
! then be put into the report text header.                            !O
!-----------------------------------------------------------------------

IFLABEL18: &
      IF (CTYPE(1:6) == 'SHPSYN') THEN
        LAT=ICHAR(CTRAIL(J5)(13:13))*256+ICHAR(CTRAIL(J5)(14:14))
        LON=ICHAR(CTRAIL(J5)(15:15))*256+ICHAR(CTRAIL(J5)(16:16))
        IF (LAT >= 32768) LAT=LAT-65536
        IF (LON >= 32768) LON=LON-65536
        IF (LAT /= -32768) THEN
          RLAT=0.01*REAL(LAT)
          RLON=0.01*REAL(LON)
        ELSE
          RLAT=-9999999.0
          RLON=-9999999.0
        END IF
      END IF IFLABEL18

      WRITE(HELN,'(I5)')MLEN+IHDLEN                             !T
      CALL SETHED(HEADER,RLAT,RLON,HHOUR,IMIN(J5), &            !F
      HDAY,HMONTH,HYEAR,ISECT1(4),ISECT1(5),NAMD,NCOR,CCCC)     !F

!=======================================================================
!
! Use the element index type to decide which displacement routine to
! call.'B'itindex is Fastrack retrieval for single reports per message.
!
!=======================================================================

IFLABEL19: &
      IF(.NOT.CHAREP) THEN                                      !B
                                                                !B
IFLABEL20: &
        IF (XTYPE == 'B') THEN
          NREP=1
          CALL BITINDX(CINDX,IDESC,NDES,IREPL,QCREQ,      & !2.1!B
              IFAIL,LFLAG,LOCALD,CMSG(J5)(IOFSET:),NELREQ, &    !B
              DISPL,WIDTH,SCALE,REFVAL,999,NEWMDBCALL)          !H
                                                                !B
          IF(IFAIL == 16)THEN    ! Element index not found      !B
            ISTAT=16                                            !B
            GOTO 999                                            !B
          END IF                                                !B
                                                                !B
        ELSE                                                    !B
          WRITE(6,9005)XTYPE                                    !B
          ISTAT=16                                              !B
          GOTO 999                                              !B
        END IF IFLABEL20
                                                                !B
      ELSE   ! just the report text needed                      !B
        IMD      = 1                                            !B
        NREP     = 1                                            !B
        NREP     = 1                                            !B
        NELREQ   = 1                                            !F
        DISPL(1) = -99                                          !F
      END IF IFLABEL19 ! end of block for CHAREP or not         !B

    END IF IFLABEL17    ! end of .NOT.LMIDMSG block

!-----------------------------------------------------------------------
! if there's not room for all these reports, set lmidmsg=.true.
!-----------------------------------------------------------------------

    IF (NOBS-IOB < NREP-NSEND) THEN
      NSEND = IREP1 + (NOBS - IOB) - 1
      LMIDMSG = .TRUE.
    ELSE
      NSEND = NREP
      LMIDMSG = .FALSE.
    END IF

!-----------------------------------------------------------------------
! fast retrieval. Copy values from BUFR message to users array.
!-----------------------------------------------------------------------

    IF (LFLAG) PRINT*,'XTYPE before VALUSR ',XTYPE
IFLABEL21: &
    IF (XTYPE == 'B') THEN                                      !B
      IF (LFLAG) PRINT*,'CALLING VALUSR'

!-----------------------------------------------------------------------
! If FOUND(34) the user wants to retrieve the BUFR message in the
! raw report string.                                                  !T
!-----------------------------------------------------------------------

IFLABEL22: &
      IF (FOUND(34)) THEN                                       !T
        WRITE(HELN,'(I5)')BFEND-BFSTART+1                       !T
        CALL VALUSR(CMSG(J5)(IOFSET:),NELREQ,             &     !T
                    DISPL,WIDTH,SCALE,REFVAL,             &     !T
                    ARRAY,IOB+1,NOBS,CSTR,                &     !T
                    HELN//CMSG(J5)(BFSTART:BFEND),CREPRT, &     !T
                    LFLAG,ISECT1,CHRELM,LENCHR)                 !T

!-----------------------------------------------------------------------
! Else the user may want the observation raw report.                  !T
!-----------------------------------------------------------------------

      ELSE                                                      !T
        IF (MLEN > 0) THEN                                      !N
          CALL VALUSR(CMSG(J5)(IOFSET:),NELREQ,              &  !A
                      DISPL,WIDTH,SCALE,REFVAL,              &  !A
                      ARRAY,IOB+1,NOBS,CSTR,                 &  !A
                      HELN//HEADER//CMSG(J5)(1:MLEN),CREPRT, &  !A
                      LFLAG,ISECT1,CHRELM,LENCHR)           !L!D!B
        ELSE                                                  !1.7
          CALL VALUSR(CMSG(J5)(IOFSET:),NELREQ,    &          !1.7
                      DISPL,WIDTH,SCALE,REFVAL,    &          !1.7
                      ARRAY,IOB+1,NOBS,CSTR,       &          !1.7
                      HELN//HEADER,CREPRT,         &         !1.17
                      LFLAG,ISECT1,CHRELM,LENCHR)             !1.7
        END IF                                                !1.7.7
      END IF IFLABEL22 !- found(34)

      II=IOB+1

    ELSE
      CONTINUE
    END IF IFLABEL21

!=======================================================================
! INCREMENT AND SAVE LOOP COUNTERS FOR NEXT ENTRANCE
!=======================================================================
! LMIDMSG IS TRUE WHEN A MESSAGE HAS BEEN DECODED BUT NOT ALL OF
! ITS REPORTS HAVE BEEN TRANSFERRED TO THE USER'S ARRAY.
!-----------------------------------------------------------------------

IFLABEL23: &
    IF (LMIDMSG) THEN
IFLABEL24: &
      IF(NSEND+1 > NREP)THEN   ! start at first report of next msg
        NXTREP = 1
        NSEND = 0
IFLABEL25: &
        IF (EOCHN(J5,ICHL2,ICHL3)) THEN ! test for end of chain
          NXTCHN = 1
          CHANRED = .FALSE.
IFLABEL26: &
          IF (J2+1 > ITRIES) THEN ! end of entries in this block
            NXTENT = 1

! Don't know why this check is needed, but it must allow for        !2.4
! looping backward as well as forward over index blocks!            !2.4

            IF (ISTEP > 0) THEN                               !2.4
              IF (J1+ISTEP > ISHR2) GOTO 999 ! no more to do !2.4
            ELSE                                              !2.4
              IF (J1+ISTEP < ISHR2) GOTO 999 ! no more to do !2.4
            END IF                                            !2.4

            NXTIME=J1+ISTEP
            INDXRED=.FALSE.
          ELSE
            NXTENT = J2 + 1
            NXTIME = J1
          END IF IFLABEL26
          NTRYCHK = .FALSE.
        ELSE                   ! IF NOT END OF CHAIN
          NXTCHN = J5 + ICHL3  ! NEXT IN CHAIN
          NXTENT = J2          ! SAME INDEX ENTRY
          NXTIME = J1          ! SAME INDEX BLOCK
        END IF IFLABEL25
      ELSE                     ! IF MORE OBS IN SAME MESSAGE
        NXTREP = NSEND + 1     ! NEXT REPORT IN MESSAGE
        NXTCHN = J5
        NXTIME = J1
        NXTENT = J2
      END IF IFLABEL24
      ISTAT = 4                ! SET MORE DATA TO COME AND RETURN
      GOTO 999
    ELSE                       ! .not.lmidmsg
      IOB =IOB+(NSEND-IREP1)+1 ! INCREMENT NO OF REPORTS TRANSFERDED
      NSEND = 0
      NXTREP = 1
      IREP1 = 1
IFLABEL27: &
      IF (IOB == NOBS) THEN    ! IF ARRAY IS FULL

!-----------------------------------------------------------------------
! increment loop variables for entry next time
!-----------------------------------------------------------------------

IFLABEL28: &
        IF (EOCHN(J5,ICHL2,ICHL3)) THEN ! IF END OF CHAIN
          NXTCHN = 1           ! START OF CHAIN (FOR NEXT ENTRY)
          CHANRED = .FALSE.
IFLABEL29: &
          IF (J2+1 > ITRIES) THEN   ! IF NO MORE ENTRIES
            NXTENT = 1         ! FIRST ENTRY

! Don't know why this check is needed, but it must allow for        !2.4
! looping backward as well as forward over index blocks!            !2.4

            IF (ISTEP > 0) THEN                               !2.4
              IF (J1+ISTEP > ISHR2) GOTO 999 ! no more to do !2.4
            ELSE                                              !2.4
              IF (J1+ISTEP < ISHR2) GOTO 999 ! no more to do !2.4
            END IF                                            !2.4

            NXTIME=J1+ISTEP    ! IN NEXT INDEX (IF THERE IS ONE)
            INDXRED=.FALSE.
          ELSE
            NXTENT=J2+1        ! NEXT ENTRY
            NXTIME=J1          ! IN SAME INDEX
          END IF IFLABEL29
          NTRYCHK = .FALSE.
        ELSE                   ! IF NOT END OF CHAIN
          NXTCHN = J5 + ICHL3  ! NEXT IN CHAIN (ADD +1 OR -1)
          NXTENT = J2
          NXTIME = J1

!-----------------------------------------------------------------------
! if latest, only one ob for each identifier is wanted, but if there
! is more than one index block there may be several entries for the
! same identifier - code at the start of the entry loop should cope.
!-----------------------------------------------------------------------

          IF (LATEST) THEN
            NTRYCHK = .FALSE.
            CHANRED = .FALSE.
          END IF
        END IF IFLABEL28
        ISTAT = 4              ! MORE DATA TO COME (MAYBE)
        GOTO 999
      END IF IFLABEL27
    END IF IFLABEL23

    IF (LATEST.AND.PASSED) THEN
      NTRYCHK = .FALSE.
      CHANRED = .FALSE.
      GOTO 699                 ! JUMP OUT OF LOOP ROUND CHAIN
    END IF
675       CONTINUE                   ! END OF LOOP OVER OBS IN CHAIN
    END DO DOLABEL6
    NTRYCHK = .FALSE.
    CHANRED = .FALSE.
699     CONTINUE                     ! END OF INDEX ENTRY LOOP
  END DO DOLABEL5
  IENT1   = 1
  INDXRED = .FALSE.
799   CONTINUE                       ! END OF INDEX BLOCK LOOP
END DO DOLABEL1

IF (IOB == 0) THEN
  IF (LCONT) THEN
    ISTAT = 0
  ELSE
    ISTAT = 8
  END IF
END IF

!-----------------------------------------------------------------------
! return number of reports
!-----------------------------------------------------------------------

999   CONTINUE

IF (II /= 0) THEN                                            !1.14
  IOBS=II                                                    !1.14
ELSE                                                         !1.14
  IOBS=IOBS-1                                                !1.14
END IF                                                       !1.14

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/', &
       I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/', &
       I2.2,1X,I2.2,'Z')                                    !1.16b
9004  FORMAT(' MDB WARNING: CAN ONLY FIND LATEST REPORTS IN',  &
       ' THE ',I3,' HOURS UP TO ',I4,'/',I2.2,'/',I2.2,1X, &
       I4.4,'Z')
9005  FORMAT(' MDB ERROR: INVALID ELEMENT INDEX TYPE ',A1)

RETURN
END SUBROUTINE SYNRET
