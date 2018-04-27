SUBROUTINE UPRRET(CTYPE,LFLAG,ISTAT,IDSK,ITIME,IRTIM,FOUND, &
                  UAPART,CIDENT,AREA,IERR,IOVER,VER,IDESC,  &
                  NDES,IREPL,QCREQ,ARRAY,NOBS,NELEM,CSTR, &
                  CREPRT,IOBS,WANTTEMPS,WANTWINDS,NUMTEMPS, &
                  NUMWINDS,TRANGE,NEWMDBCALL,CRTYP,         &
                  LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,ELIST)

!-----------------------------------------------------------------------
!
! Subroutine    : UPRRET
!
! Purpose       : MetDB retrieval routine for UPPER AIR (TEMP, PILOT,
!               : DROPSOND)
!
! Description   : This routine retrieves upper air data from the MetDB.
!               : It loops over the index entries in an index block
!               : covering the request period. For each index entry
!               : (covers 1 upper air station report) there are chained
!               : index entries covering the parts (A,B,C,D) of the
!               : upper air report. The parts can be in any order and
!               : can be corrections. Data is returned according to the
!               : user's request string.
!
! Data type(s)  : TEMP, PILOT, DROPSOND
!
! Called by     : MDB
!
! Sub calls     : ARRINDX   - to get array displacements
!               : BITINDX   - to get BUFR displacements
!               : CHECKPOSITIONS - see if room left in users array
!               : DT2HRS    - to convert date/time to century hours
!               : HRS2DT    - to convert century hours to date/time
!               : MAPRD     - to read map, index, data blocks
!               : READIDX   - to read element idex from ds
!               : IDMTCH    - to match index entry identifiers
!               : ROTAREA   - to rotate a lat lon
!               : SORTCH    - to sort index entries into order.
!               : SUBPER    - to select INCREMENT data.
!               : UPRPARTS  - to combine parts into 1 profile
!               : UPRWINDS  - to combine & sort TEMP & PILOT winds
!               : UPREPID   - to get the reports identity
!               : VALAREA   - to check ob in users lat lon area
!               : VALARR    - to put values in users array
!               : VALUE     - to extract a value from a BUFR message
!               : VALUSR    - to put values in users array
!
! Arguments     :
!
! CTYPE         : char*(*)  (ip) : retrieval subtype
! LFLAG         : logical   (ip) : diagnostics o/p ON or OFF (T/F)
! ISTAT         : integer  (iop) : MetDB state indicator
! IDSK(5)       : integer   (ip) : storage dataset details
! ITIME(9)      : integer   (ip) : (1)  yyyy start date
!                                : (2)  mm
!                                : (3)  dd
!                                : (4)  hhmm
!                                : (5)  yyyy  end date
!                                : (6)  mm
!                                : (7)  dd
!                                : (8)  hhmm
!                                : (9)  increment (not used here)
! IRTIM(10)     : integer   (ip) : (1)  yyyy  earliest t.o.r (zero
!                                : (2)  mm    if not set)
!                                : (3)  dd
!                                : (4)  hh
!                                : (5)  min
!                                : (6)  yyyy  latest t.o.r
!                                : (7)  mm
!                                : (8)  dd
!                                : (9)  hh
!                                : (10) min
! FOUND(29)     : logical   (ip) : array of user requested keywords
! UAPART        : integer   (ip) : part of report wanted by user
! CIDENT(50)    : character (ip) : station identifiers array
! AREA(5)       : real      (ip) : user-defined area.
! IERR          : integer   (op) : error indicator.
! IOVER         : integer   (ip) : user-defined land/sea selection
! VER           : integer   (ip) : preferred or all versions wanted
! IDESC(NDES)   : integer   (ip) : user element descriptors
! NDES          : integer   (ip) : no. of element descriptors
! IREPL(NDES)   : integer   (ip) : user element replications
! QCREQ         : logical   (ip) : TRUE if the users wants QC flags
!
! ARRAY(NOBS,NELEM) : real  (op) : users array
!
! NOBS          : integer  (iop) : no. of user observations
! NELEM         : integer   (ip) : no. of user elements
! CSTR(NOBS)    : character (op) : users strings
! CREPRT(NOBS)  : character (op) : raw report text
! IOBS          : integer   (op) : no. of next ob to put in users array
! WANTTEMPS     : logical   (ip) : TRUE if user wants temp levels
! WANTWINDS     : logical   (ip) : TRUE if user wants wind levels
! NUMTEMPS      : integer   (ip) : number of user temp levels wanted
! NUMWINDS      : integer   (ip) : number of user wind levels wanted
! TRANGE        : integer   (ip) : users START TIME sub period.
! NEWMDBCALL    : logical  (iop) : TRUE if new MetDB call
! CRTYP         : character (ip) : type of data to retrieve C or M
! LAT_SUBSCRIPT : integer   (ip) : users lat subscript
! LON_SUBSCRIPT : integer   (ip) : users lon subscript
! RPOLE         : real      (ip) : rotated pole
! ELIST         : char*8    (ip) : Element index member name
!
! REVISION INFO:
!
! $Workfile: uprret.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 16/02/2011 12:13:01$
!
! Change record :
!
! $Log:
!  8    MetDB_Refresh 1.7         16/02/2011 12:13:01    John Norton     Rework
!        done
!  7    MetDB_Refresh 1.6         09/02/2011 17:30:43    Sheila Needham  Reduce
!        diagnostic output; correct INTENTS in contained subroutine
!       CHECKPOSITIONS
!  6    MetDB_Refresh 1.5         21/12/2010 16:57:54    Sheila Needham  Change
!        INTENT to INOUT on IOBS
!  5    MetDB_Refresh 1.4         26/11/2010 14:06:00    Brian Barwell
!       Correct intents in CHEKPOSITIONS.
!  4    MetDB_Refresh 1.3         19/11/2010 14:18:12    John Norton
!       Restored WRITE statement with format 9003 contained within it.
!  3    MetDB_Refresh 1.2         18/11/2010 13:18:09    Sheila Needham  Change
!        date/time check
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
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

! Use statements:
! <Interfaces>

USE arrindx_mod
USE bitindx_mod
USE desfxy_mod
USE dt2hrs_mod        !function
USE hrs2dt_mod
USE idmtch_mod
USE maprd_mod
USE readidx_mod
USE rotarea_mod
USE sortch_mod
USE subper_mod
USE uprepid_mod
USE uprparts_mod
USE uprwinds_mod
USE valarea_mod
USE valarr_mod
USE value_mod         !function
USE valusr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)    ::  CTYPE !- retrieval subtype
LOGICAL,      INTENT(IN)    ::  LFLAG !- diagnostics output
INTEGER,      INTENT(INOUT) ::  ISTAT !- MetDB state indicator
INTEGER,      INTENT(IN)    ::  IDSK(5) !- dataset storage details
INTEGER,      INTENT(IN)    ::  ITIME(9) !- users end time in century hours
INTEGER,      INTENT(IN)    ::  IRTIM(10) !- users time of receipt
LOGICAL,      INTENT(IN)    ::  FOUND(:) !- array of keywords selected
INTEGER,      INTENT(IN)    ::  UAPART !- 0=A&C or B&D, 1=AorB, 2=CorD
CHARACTER(9), INTENT(IN)    ::  CIDENT(50) !- station identifiers
REAL,         INTENT(IN)    ::  AREA(5) !- user-defined area
INTEGER,      INTENT(OUT)   ::  IERR !- error indicator
INTEGER,      INTENT(IN)    ::  IOVER !- land/sea user-selection
INTEGER,      INTENT(IN)    ::  VER !- 1=preferred (default), 2=all
INTEGER,      INTENT(IN)    ::  NDES !- number of user elements
INTEGER,      INTENT(IN)    ::  IDESC(1+NDES) !- array of user required element pointers
INTEGER,      INTENT(IN)    ::  IREPL(:) !- array of user required replications
LOGICAL,      INTENT(IN)    ::  QCREQ !- TRUE if users wants QC elements
INTEGER,      INTENT(INOUT) ::  NOBS !- users no. of observations.
INTEGER,      INTENT(IN)    ::  NELEM !- users no. of elements
REAL,         INTENT(INOUT) ::  ARRAY(NOBS,NELEM) !- users array
CHARACTER(*), INTENT(INOUT) ::  CSTR(NOBS) !- users strings
CHARACTER(*), INTENT(INOUT) ::  CREPRT(NOBS) !- users report string
INTEGER,      INTENT(INOUT) ::  IOBS !- next ob in users array
LOGICAL,      INTENT(IN)    ::  WANTTEMPS !- TRUE if user wants temp levels
LOGICAL,      INTENT(IN)    ::  WANTWINDS !- TRUE if user wants wind levels
INTEGER,      INTENT(IN)    ::  NUMTEMPS !- number of user temp levels wanted
INTEGER,      INTENT(IN)    ::  NUMWINDS !- number of user wind levels wanted
INTEGER,      INTENT(IN)    ::  TRANGE !- users START TIME sub period.
LOGICAL,      INTENT(INOUT) ::  NEWMDBCALL !- TRUE if new MetDB call (istat=0)
CHARACTER(*), INTENT(IN)    ::  CRTYP !- type of data stored
INTEGER,      INTENT(IN)    ::  LAT_SUBSCRIPT !- users lat subscript
INTEGER,      INTENT(IN)    ::  LON_SUBSCRIPT !- users lon subscript
REAL,         INTENT(IN)    ::  RPOLE(2) !- rotated pole lat lon coords
CHARACTER(*), INTENT(IN)    ::  ELIST  !- elidx member

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

INTEGER,     PARAMETER ::  INDLEN = 23 !- index entry length
INTEGER,     PARAMETER ::  MXOFL = 2 !- max no. of overflow blocks used
INTEGER,     PARAMETER ::  MXINDX = 27992/INDLEN !- max no. of index entries
INTEGER,     PARAMETER ::  MXRPLEN = 5000 !- max length of report
INTEGER,     PARAMETER ::  MXRPLEN_MER = 27998 !- max length of report (merged data) !1.19a
INTEGER,     PARAMETER ::  MXCHLEN = 15 !- max no of chained reports
INTEGER,     PARAMETER ::  MXCHLEN_MER = 2 !- max no of chained reports (merged) !1.19a
INTEGER,     PARAMETER ::  MXBLK = 10000 !- max block size
INTEGER,     PARAMETER ::  MXFINELMS = 24000 !- max no. of elements in FINALPROFILE array
INTEGER,     PARAMETER ::  MXARRELMS = 75 !- max no. of elements in ARRINDX
INTEGER,     PARAMETER ::  MXARRSEGS = 10 !- max no. of segments in ARRINDX
INTEGER,     PARAMETER ::  MXREPTXTLEN = 27998 !- report text length for user

!-----------------------------------------------------------------------
! declare variables (INTEGERS)
!-----------------------------------------------------------------------

INTEGER      ::  ABLOCK !- actual data block number (from start)

INTEGER      ::  ARRELMNUM(MXARRELMS) !- ARRINDX elements array
INTEGER      ::  ARRNELREQ      !- no. of returned elems
INTEGER      ::  ARRNROWS       !- ARRINDX no. of elements
INTEGER      ::  ARRNSEGS       !- ARRINDX no. of segments
INTEGER      ::  ARRSEGLEN(MXARRSEGS) !- ARRINDX segment length array
INTEGER      ::  ARRSEGNUM(MXARRELMS) !- ARRINDX segment no. array
INTEGER      ::  ARRSEGST(MXARRSEGS) !- ARRINDX segment start array
INTEGER      ::  ARRSOURCE(MXFINELMS) !- ARRINDX segment start array
INTEGER      ::  ARRSTYP(MXARRSEGS) !- ARRINDX segment repl count arr
INTEGER      ::  ARRSUBNUM(MXARRELMS) !- ARRINDX segment subscript array

INTEGER      ::  BUFREND !- position of end of BUFR message
INTEGER      ::  BUFRSTART !- position of start of BUFR message
INTEGER      ::  BYTE17 !- index entry byte 17
INTEGER      ::  CHLOOP !- loop over reports in chain
INTEGER      ::  COMB_CENT_MIN ! century-minute of combined ascent
INTEGER      ::  DBLOCK !- data block number

INTEGER      ::  DISPL(MXFINELMS) !- array of BUFR/array displacements

INTEGER      ::  F  !- F of FXXYYY BUFR descriptor
INTEGER      ::  HDAY !- day to put in header
INTEGER      ::  HHOUR !- hour to put in header
INTEGER      ::  HMONTH !- month to put in header
INTEGER      ::  HYEAR !- year to put in header
INTEGER      ::  I  !- general use loop counter
INTEGER      ::  IBEFOR !- used in call to VALUE
INTEGER      ::  IBLD !- index block day
INTEGER      ::  IBLD2 !- index block day (end)
INTEGER      ::  IBLH !- index block hour
INTEGER      ::  IBLH2 !- index block hour (end)
INTEGER      ::  IBLM !- index block month
INTEGER      ::  IBLM2 !- index block month (end)
INTEGER      ::  IBLY !- index block year
INTEGER      ::  IBLY2 !- index block year (end)
INTEGER      ::  IBLOCK !- physical block no. to read (maprd)
INTEGER      ::  IBLOOP !- loop counter over index blocks
INTEGER      ::  ICHL1 !- start position in chain
INTEGER      ::  ICHL2 !- end position in chain
INTEGER      ::  ICHL3 !- loop increment in chain
INTEGER      ::  ICHR !- century hour from chained index entry
INTEGER      ::  ICMIN !- century minute from chained index entry
INTEGER      ::  IDATHR !- time tag of index read          (maprd)
INTEGER      ::  IDAY !- day from index block

INTEGER      ::  IDHR !- hour from chained index entry
INTEGER      ::  IELOOP !- loop counter over index entries
INTEGER      ::  IENTRIES !- count of index entries we want
INTEGER      ::  IENT1 !- used in chain loop
INTEGER      ::  IEXTRA(1) !- array of extra values for VALARR
INTEGER      ::  IFAIL !- status flag for merge retrieval
INTEGER      ::  II
INTEGER      ::  IHOUR !- hour from index block
INTEGER      ::  ILOCALD !- integer local D descriptor
INTEGER      ::  IMIN !- minute from chained index entry
INTEGER      ::  INDHR1 !- index block start time
INTEGER      ::  INDHR2 !- index block end time
INTEGER      ::  INOMIN !- century-minute of nominal time
INTEGER      ::  INUM !- no. of entries read               (maprd)
INTEGER      ::  INXBLK !- no. of index blocks in ds       (maprd)
INTEGER      ::  INXHRS !- no. of hours per index block    (maprd)
INTEGER      ::  IOB !- last ob in users array
INTEGER      ::  IRECLN !- total length of message         (maprd)
INTEGER      ::  IREP1
INTEGER      ::  IRTIM1 !- users start TOR in century minutes
INTEGER      ::  IRTIM2 !- users end TOR in century minutes
INTEGER      ::  ISECT1(32) !- BUFR section 1 array
INTEGER      ::  ISHR1 !- users start time as start of index blk
INTEGER      ::  ISHR2 !- users start time as end of index blk
INTEGER      ::  ISLOTHR !- start hour of 1st index block  (maprd)
INTEGER      ::  ITIM1 !- users start time in century hours
INTEGER      ::  ITIM2 !- users end time in century hours
INTEGER      ::  ITIMND !- users end time in century minutes
INTEGER      ::  ITIMST !- users start time in century minutes
INTEGER      ::  ITORM !- actual report TOR in century minutes
INTEGER      ::  KEPCHN = 0 !- position in chain
INTEGER      ::  LAT !- latitude from index entry

INTEGER      ::  LENCHR(3) !- array of lengths of CHRELM
INTEGER      ::  LON !- longitude from index entry

INTEGER      ::  LRCPOINT !- LEVL_RPLTN_CONT displ array pointer

INTEGER      ::  MSGLEN(MXCHLEN) !- actual length of chained report

INTEGER      ::  NCHN !- no. of entries in chain
INTEGER      ::  NELMIX !- block no. for element index     (maprd)
INTEGER      ::  NREC !- logical record number             (maprd)
INTEGER      ::  NREP
INTEGER      ::  NSEND
INTEGER      ::  NSQ !- 1 or 0 if local descriptor or not
INTEGER      ::  NTOR !- report TOR after slot base hour
INTEGER      ::  NXTCHN = 0
INTEGER      ::  NXTENT = 0
INTEGER      ::  NXTIME = 0
INTEGER      ::  NXTREP = 0

INTEGER      ::  POSN_PART(4) !- position of part in chain

INTEGER      ::  RC         !- general return code
INTEGER      ::  RECNUM     !- record no. of data in data block
INTEGER      ::  REFVAL(MXFINELMS) !- bitindex refval array
INTEGER      ::  SCALE(MXFINELMS) !- bitindex scale array
INTEGER      ::  TRB17      !- report trailor byte 17
INTEGER      ::  TRL_OB_TYPE !- TEMP or PILOT part in trailor
INTEGER      ::  USERLEVS   !- number of user levels
INTEGER      ::  WIDTH(MXFINELMS) !- bitindex width array
INTEGER      ::  X          !- XX of FXXYYY BUFR descriptor
INTEGER      ::  Y          !- YYY of FXXYYY BUFR descriptor

!-----------------------------------------------------------------------
! declare variables (REALS - in alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  FINALPROFILE(MXFINELMS) !- final combined ascent
REAL         ::  RLAT            !- real lat from index entry
REAL         ::  RLON            !- real lon from index entry
REAL         ::  ROT_LAT         !- rotated latitude
REAL         ::  ROT_LON         !- rotated longitude

!-----------------------------------------------------------------------
! declare variables (LOGICALS - in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      ::  AREA_FLAG !- TRUE if ob inside users area
LOGICAL      ::  CHANRED !- TRUE if chain has been read in
LOGICAL      ::  ELIDXFOUND !- TRUE if element index found
LOGICAL      ::  FIRST = .TRUE. !- TRUE for first call to UPRAIRET
LOGICAL      ::  FLAG999
LOGICAL      ::  FOUNDLCC !- TRUE if LEVL_CDTN_CODE found (merge)
LOGICAL      ::  FOUNDLRC !- TRUE if LEVL_RPLTN_CONT found (merge)
LOGICAL      ::  LCONT
LOGICAL      ::  LOCDFG !- TRUE if local descriptor in dataset
LOGICAL      ::  INDXRED !- TRUE if index block has been read
LOGICAL      ::  ITS_A_TEMP !- TRUE if report is a TEMP
LOGICAL      ::  ITS_A_PILOT !- TRUE if report is a PILOT
LOGICAL      ::  ITS_A_DROP !- TRUE if report is a DROPSOND
LOGICAL      ::  ITS_FIXED !- TRUE if report is a FIXED ob
LOGICAL      ::  ITS_MARINE !- TRUE if report is a MARINE ob
LOGICAL      ::  ITS_MOBILE !- TRUE if report is a MOBILE ob
LOGICAL      ::  ITS_ONLAND !- TRUE if report is a LAND ob
LOGICAL      ::  ITS_PART(4) !- TRUE if report is part 1=A,2=B,3=C,4=D
LOGICAL      ::  KEEPIE !- TRUE to keep index entry
LOGICAL      ::  KEPT_PART(4) !- TRUE if weve kept part 1=A,2=B,3=C,4=D
LOGICAL      ::  NEWMRGCALL !- TRUE if new MetDB call (istat=0)
LOGICAL      ::  NEWSKLARR !- TRUE if new MetDB call (istat=0)
LOGICAL      ::  NEWBITIND !- TRUE if new MetDB call (istat=0)
LOGICAL      ::  NTRYCHK !- TRUE if index entry has been read
LOGICAL      ::  READ_CHNREPS !- TRUE if reports left in chain to read
LOGICAL      ::  TPMIX !- TRUE if TEMPs & PILOTs in chain
LOGICAL      ::  USERDROP !- TRUE if user wants DROPSONDEs
LOGICAL      ::  USERPILOT !- TRUE if user wants PILOTs
LOGICAL      ::  USERTEMP !- TRUE if user wants TEMPs
LOGICAL      ::  WANT_ABCD !- TRUE if user wants all parts of report
LOGICAL      ::  WANT_OB_TYPE !- TRUE if we want the ob type in trailor
LOGICAL      ::  WANT_PART(4) !- TRUE if user wants part 1=A,2=B,3=C,4=D
LOGICAL      ::  WASPREF !- TRUE if report was once preferred

!-----------------------------------------------------------------------
! declare variables (CHARACTERS - in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(4) ::  BUFR                !- BUFR Character string
CHARACTER(9) ::  CALLSIGN(1)         !- Call Sign
CHARACTER(4) ::  CCCC                !- Collecting centre
CHARACTER(9) ::   CHRELM(3)           !- character elems
CHARACTER(MXBLK) ::  CINDX(12) !- holds bit/array index
                                         !- (readidx)
CHARACTER(MXRPLEN) :: CMSG(MXCHLEN) !- actual message (maprd)
CHARACTER(MXRPLEN_MER) :: CMSG_MER(MXCHLEN_MER)

CHARACTER(1) ::   CNAM
CHARACTER(INDLEN) :: CNTRY(MXOFL*MXINDX) !- character array of
                                         !- kept index entires
CHARACTER(INDLEN) :: CTRAIL(MXCHLEN) !- chained index entry
CHARACTER(INDLEN) :: CTRAN(MXOFL*MXINDX) !- character array of
                                         !- entries read   (maprd)
CHARACTER(5) ::   HELN                !- rprt header length
CHARACTER(6) ::   LOCALD              !- Character local D
CHARACTER(23)::   MASKIT = '  XXXXXXXXX' ! used for (sortch) to sort on ident.
CHARACTER(1) ::   PMAP(4)             !- map 1 to A, 2 to B etc

CHARACTER(MXREPTXTLEN) ::  REPORTTEXT !- raw report text

CHARACTER(4) ::   SEVENS              !- 7777 char string
CHARACTER(9) ::   THISID              !- this station id.
CHARACTER(9) ::   TTAA                !- TTAA

CHARACTER(1) ::   XTYPE               !- type of index 'A','B'

!-----------------------------------------------------------------------
! dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

COMMON /UPAIR1/ARRELMNUM,ARRSEGNUM,ARRSOURCE,ARRSUBNUM
COMMON /UPAIR2/DISPL,FINALPROFILE,CINDX
COMMON /UPAIR3/CMSG,CNTRY,CTRAIL,CTRAN,REPORTTEXT
COMMON /UPAIR4/WIDTH,SCALE,REFVAL,CMSG_MER

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! initialise variables (data statements)
!-----------------------------------------------------------------------

DATA PMAP   /'A','B','C','D'/  !- to map 1-4 to A-D

!-----------------------------------------------------------------------
! diagnostic output to say UPRRET has been called.
!-----------------------------------------------------------------------

IF (LFLAG) THEN
  WRITE(*,'(/1X,''In MetDB subroutine UPRRET'' )')
  WRITE(*,'( 1X,''==========================''/)')
END IF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IERR = 0
II   = 0
IOB  = IOBS-1    ! IOB --> last ob in users array (IOBS --> NEXT)

!-----------------------------------------------------------------------
! First time only:  Initialise revision information, BUFR and SEVENS
!-----------------------------------------------------------------------

IF (FIRST) THEN
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'
  FIRST=.FALSE.
END IF

!-----------------------------------------------------------------------
! If a new mdb call with ISTAT=0, NEWMDBCALL=.TRUE. Set NEWSKLARR and
! NEWBITIND to .TRUE.
!-----------------------------------------------------------------------

IF (NEWMDBCALL) THEN
  NEWSKLARR  = .TRUE.
  NEWBITIND  = .TRUE.
  NEWMRGCALL = .TRUE.
  NEWMDBCALL = .FALSE.
END IF

!-----------------------------------------------------------------------
! check for continuation and if so, reset loop counters.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (ISTAT == 16) THEN
  ISTAT = 0
ELSE IF (ISTAT == 4) THEN
  ISHR1 = NXTIME
  IENT1 = NXTENT
  IREP1 = NXTREP
  ICHL1 = NXTCHN
  NSEND = NXTREP-1
  LCONT = .TRUE.
ELSE
  CONTINUE
END IF IFLABEL1

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here.
!=======================================================================

IFLABEL2: &
IF (ISTAT == 0) THEN

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not
! output an error and exit subroutine.
!-----------------------------------------------------------------------

  IF (NELEM < NDES) THEN
    IERR = 16
    WRITE(*,*)'In UPRRET: MDB ERROR: You have not specified'
    WRITE(*,*)'enough array elements. Try NELEM = ',NDES
    GOTO 999
  END IF

!-----------------------------------------------------------------------
! calculate number of levels, user has coded
!-----------------------------------------------------------------------

  USERLEVS = MAX(NUMTEMPS,NUMWINDS)

!-----------------------------------------------------------------------
! decide which subtype user wants
! (both TEMP & PILOT, for the levels to be combined, if UAWINDS)
!-----------------------------------------------------------------------

  USERTEMP=(CTYPE(1:4) == 'TEMP' .OR.   &
            CTYPE(1:7) == 'UAWINDS')
  USERPILOT=(CTYPE(1:5) == 'PILOT' .OR.  &
            CTYPE(1:7) == 'UAWINDS')
  USERDROP  = (CTYPE(1:8) == 'DROPSOND')

!-----------------------------------------------------------------------
! decide which parts of the upper air report the user has requested.
!-----------------------------------------------------------------------

  WANT_ABCD = (.NOT.FOUND(25) .AND. .NOT.FOUND(26) .AND. &
               .NOT.FOUND(30))

  WANT_PART(1) = (WANT_ABCD .OR. (FOUND(25) .AND. UAPART /= 2) &
                            .OR. (FOUND(30) .AND. UAPART /= 2))
  WANT_PART(2) = (WANT_ABCD .OR. (FOUND(26) .AND. UAPART /= 2) &
                            .OR. (FOUND(30) .AND. UAPART /= 2))
  WANT_PART(3) = (WANT_ABCD .OR. (FOUND(25) .AND. UAPART /= 1) &
                            .OR. (FOUND(30) .AND. UAPART /= 1))
  WANT_PART(4) = (WANT_ABCD .OR. (FOUND(26) .AND. UAPART /= 1) &
                            .OR. (FOUND(30) .AND. UAPART /= 1))

  IF (LFLAG) THEN
    WRITE(*,*)'In UPRRET: WANT_ABCD  = ',WANT_ABCD
    DO I=1,4
      WRITE(*,*)'In UPRRET: WANT_PART',PMAP(I),' = ',WANT_PART(I)
    END DO
  END IF

!-----------------------------------------------------------------------
! read map block to find dataset details. If LOCDFG is TRUE, then there
! is a local table D descriptor. Set NSQ=1.
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG, &
             NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
             NREC,IRECLN,CMSG(KEPCHN+1),'MAPRD')

  NSQ=0
  IF (LOCDFG) NSQ=1

!-----------------------------------------------------------------------
! Read the element index from the the element index dataset.
!-----------------------------------------------------------------------

  CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)

  IF (.NOT.ELIDXFOUND) THEN
    WRITE(*,*)'In UPRRET: MDB ERROR: Cannot find element ', &
    'index for subtype,elist = ',CTYPE,ELIST
    IERR=16
    GOTO 999
  END IF

  IF (LFLAG) THEN
    WRITE(*,*)'In UPRRET: after call readidx, XTYPE=',XTYPE
  END IF

!-----------------------------------------------------------------------
! convert request times to century-hours & century-minutes
!
! ITIM1,ITIM2 = users start,end times in century-hours.
! ITIMST,ITIMND = users start,end times in century-minutes.
!-----------------------------------------------------------------------

  ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),(ITIME(4)/100))
  ITIM2=DT2HRS(ITIME(5),ITIME(6),ITIME(7),(ITIME(8)/100))

  ITIMST=ITIM1 * 60 + MOD(ITIME(4),100)
  ITIMND=ITIM2 * 60 + MOD(ITIME(8),100)

!-----------------------------------------------------------------------
! round times down to starts of index blocks
!
! ISHR1,ISHR2 = users start,end times as start of index block times.
! they will be the same if users time period contained within 1 index
! block.
!-----------------------------------------------------------------------

  INDHR1=MOD((ITIME(4)/100+(24-ISLOTHR)),INXHRS)
  INDHR2=MOD((ITIME(8)/100+(24-ISLOTHR)),INXHRS)

  ISHR1=ITIM1 - INDHR1
  ISHR2=ITIM2 - INDHR2

!-----------------------------------------------------------------------
! keywords RECEIVED BETWEEN and RECEIVED BEFORE/AFTER are used to
! constrain the data returned to be received (time of receipt in the
! MetDB) within the given period. Convert requested times of receipt
! to century minutes.
!-----------------------------------------------------------------------

  IF (IRTIM(1) == 0) THEN    !- start time
    IRTIM1=0
  ELSE
    IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1=(IRTIM1)*60+IRTIM(5)
  END IF

  IF (IRTIM(6) == 0) THEN    !- end time
    IRTIM2=0
  ELSE
    IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2=(IRTIM2)*60+IRTIM(10)
  END IF

!-----------------------------------------------------------------------
! initialise some variables
!-----------------------------------------------------------------------

  KEPCHN  = 0
  IENT1   = 1          ! index entry number
  IREP1   = 1          ! report in entry number
  NXTREP  = 1
  NSEND   = 0
  LCONT   = .FALSE.
  INDXRED = .FALSE.    !- index block has not been read
  NTRYCHK = .FALSE.    !- index entry has not been checked
  CHANRED = .FALSE.    !- chain has not been read

END IF IFLABEL2 !- end of ISTAT=0 if block

ISTAT=0    !- set ISTAT to zero.

!=======================================================================
!
! if istat is not zero, i.e. not first time in subroutine, we will
! start here.
!
! the rest of uprairet comprises of 3 nested loops over index blocks,
! index entries and reports in chain attached to entry.
!
! loop over index blocks. Check whether the index block has already been
! read (INDXRED=.TRUE.)
!
!=======================================================================

DOLABEL1: &
DO IBLOOP=ISHR1,ISHR2,INXHRS

IFLABEL3: &
  IF (.NOT.INDXRED) THEN

!-----------------------------------------------------------------------
! Read the index block (IBLOCK) using MAPRD. 2 and NSQ are added to get
! us past the map block and the local D block if there is one.
!-----------------------------------------------------------------------

    IBLOCK=MOD(IBLOOP/INXHRS,INXBLK)+2+NSQ

    CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
               LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR, &
               NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')

!-----------------------------------------------------------------------
! Check that correct date/time has been read. Compare the day and hour
! for the index block (iday, ihour) with the day and hour in the IBLOOP
! loop (IBLD, IBLH calculated from IBLOOP). If no match, output a
! message to the user and GOTO 799 to read the next index block.
!-----------------------------------------------------------------------

    IDAY=IDATHR/256
    IHOUR=IDATHR-IDAY * 256

    CALL HRS2DT(IBLY,IBLM,IBLD,IBLH,IBLOOP)

    IF (IBLD /= IDAY.OR.IBLH /= IHOUR) THEN
      CALL HRS2DT(IBLY2,IBLM2,IBLD2,IBLH2,IBLOOP+INXHRS-1)
      WRITE(*,'(A4,A8,A24,I4,A1,I2.2,A1,&
         &I2.2,1X,I2.2,A4,I4,A1,I2.2,A1,I2.2,1X,I2.2,A1)')              &
        ' NO ','UPRAIR  ',' DATA IN MDB FOR PERIOD ',IBLY,'/',IBLM,'/', &
          IBLD,IBLH,'Z - ',IBLY2,'/',IBLM2,'/',IBLD2,IBLH2,'Z'
      GOTO 799
    ENDIF

!=======================================================================
! Index block matched! Loop round number of index entries (INUM) in this
! index block. IENTRIES is a count of the index entries we want to keep.
!=======================================================================

    IENTRIES=0

DOLABEL2: &
    DO IELOOP=1,INUM

!-----------------------------------------------------------------------
! safety check : Check that the data block number and record number in
!              : the data block are greater than zero.
!-----------------------------------------------------------------------

      DBLOCK=ICHAR(CTRAN(IELOOP)(22:22))*256+ &
             ICHAR(CTRAN(IELOOP)(23:23))
      ABLOCK=1+NSQ+INXBLK+DBLOCK
      RECNUM=ICHAR(CTRAN(IELOOP)(20:20))*256+ &
             ICHAR(CTRAN(IELOOP)(21:21))

      IF (DBLOCK <= 0 .OR. RECNUM <= 0) THEN
        IF (LFLAG) WRITE(*,*)'In UPRRET: Bad index entry!!'
        GOTO 81
      END IF

!---------------------------------------------------------------------
! 1st check : Initially set KEEPIE (KEEP Index Entry) to false.
!           : See if the index entry subtype (byte 17, bits 5-6) matches
!           : that requested by the user. If so, KEEPIE=.TRUE.
!           :
!           : bits 5&6 determine what subtype the ob is :
!           :
!           : 00 = PILOT
!           : 01 = TEMP
!           : 10 = DROPSOND
!           : 11 = TEMP & PILOT mix
!-----------------------------------------------------------------------

      KEEPIE = .FALSE.

      BYTE17 = ICHAR(CTRAN(IELOOP)(17:17))

      ITS_A_TEMP  = .FALSE.
      ITS_A_PILOT = .FALSE.
      ITS_A_DROP  = .FALSE.

IFLABEL4: &
      IF (MOD(BYTE17/8,2) == 0 .AND.  &
      MOD(BYTE17/4,2) == 0) THEN
        ITS_A_PILOT = .TRUE.
      ELSE IF (MOD(BYTE17/8,2) == 0 .AND. &
      MOD(BYTE17/4,2) == 1) THEN
        ITS_A_TEMP = .TRUE.
      ELSE IF (MOD(BYTE17/8,2) == 1 .AND. &
      MOD(BYTE17/4,2) == 0) THEN
        ITS_A_DROP = .TRUE.
      ELSE
        ITS_A_PILOT = .TRUE.
        ITS_A_TEMP  = .TRUE.
      END IF IFLABEL4

      IF (USERTEMP  .AND. ITS_A_TEMP)  KEEPIE=.TRUE.
      IF (USERPILOT .AND. ITS_A_PILOT) KEEPIE=.TRUE.
      IF (USERDROP  .AND. ITS_A_DROP)  KEEPIE=.TRUE.


!-----------------------------------------------------------------------
! The next 2 checks only apply to TEMP or PILOT, not DROPSOND. Only do
! this check, however, if check 1 was passed (KEEPIE=.TRUE.)
!-----------------------------------------------------------------------

IFLABEL5: &
      IF (KEEPIE) THEN

IFLABEL6: &
        IF (ITS_A_TEMP .OR. ITS_A_PILOT) THEN

!-----------------------------------------------------------------------
! 2nd check : reset KEEPIE to false for the next index entry check.
!           : check bit 8 of byte 17 in the index entry. If it is 1 the
!           : ob is marine else it is land. Check the user's request.
!           : iover = 0 = land & sea, iover = 1 = land, iover = 2 = sea.
!-----------------------------------------------------------------------

          KEEPIE=.FALSE.

          ITS_ONLAND=(MOD(BYTE17/1,2) == 0)
          ITS_MARINE=(MOD(BYTE17/1,2) == 1)

          IF (IOVER == 1 .AND. ITS_ONLAND)          KEEPIE=.TRUE.
          IF (IOVER == 2 .AND. ITS_MARINE)          KEEPIE=.TRUE.
          IF (IOVER == 0)                           KEEPIE=.TRUE.


!-----------------------------------------------------------------------
! 3rd check : If the last check was passed (KEEPIE=.TRUE.) and it is
!           : a land observation
!-----------------------------------------------------------------------

IFLABEL7: &
          IF (KEEPIE .AND. ITS_ONLAND) THEN

            KEEPIE=.FALSE.

            ITS_FIXED=(MOD(BYTE17/2,2) == 0)
            ITS_MOBILE=(MOD(BYTE17/2,2) == 1)

            IF (FOUND(27) .AND. ITS_FIXED)           KEEPIE=.TRUE.
            IF (FOUND(28) .AND. ITS_MOBILE)          KEEPIE=.TRUE.
            IF (.NOT.FOUND(27) .AND. .NOT.FOUND(28)) KEEPIE=.TRUE.


          END IF IFLABEL7 !- check 3

        END IF IFLABEL6 !- check 2

      END IF IFLABEL5 !- check 1

!-----------------------------------------------------------------------
! 4th check : Check that the data time in the index entry is within the
!           : start and end times requested by the user. ICMIN = time
!           : of report in century minutes. If not, don't keep report.
! Treat missing minute (no launch time) as on the nominal hour.
!-----------------------------------------------------------------------

IFLABEL8: &
      IF (KEEPIE) THEN

        IDHR=MOD(ICHAR(CTRAN(IELOOP)(1:1)),64)
        IMIN=ICHAR(CTRAN(IELOOP)(2:2))
        IF (IMIN == 255) IMIN=0

        ICHR=DT2HRS(IBLY,IBLM,IDAY,IHOUR)+IDHR
        ICMIN=ICHR*60+MOD(IMIN,64)

! Work out century-minute for nominal hour too - either launch
! hour with zero minutes or next hour, depending on minute flag.

        IF (IMIN > 60) THEN
          IF (IMIN < 128) THEN
            INOMIN=ICHR*60
          ELSE
            INOMIN=(ICHR+1)*60
          END IF
        END IF

! If index time is not in window, reject this ob - UNLESS it was
! once indexed under a different time (nominal time rather than
! launch time) and that time IS in the window.
! So the condition below can be read as follows:
!   if the current index time is not in the window,
!     and either this ob was never indexed under any other time
!         or that original index time is outside the window too,
!   then reject this ob.

        IF (.NOT. (ICMIN >= ITIMST .AND. ICMIN <= ITIMND)   &
            .AND. (.NOT. IMIN > 60 .OR.                     &
          .NOT. (INOMIN >= ITIMST .AND. INOMIN <= ITIMND))) &
                                 KEEPIE=.FALSE.
        IF (LFLAG .AND. .NOT.KEEPIE) THEN
          PRINT *,'UPRRET time check rejected'
        END IF


      END IF IFLABEL8 !- check 4

!-----------------------------------------------------------------------
! 5th check : If we still want this index entry, get the lat and lon.
!           : The check for a missing lat or lon has been removed
!-----------------------------------------------------------------------

IFLABEL9: &
      IF (KEEPIE) THEN

        LAT=ICHAR(CTRAN(IELOOP)(13:13))*256+ &
            ICHAR(CTRAN(IELOOP)(14:14))
        LON=ICHAR(CTRAN(IELOOP)(15:15))*256+ &
            ICHAR(CTRAN(IELOOP)(16:16))

        IF (LAT >= 32768) LAT=LAT-65536
        IF (LON >= 32768) LON=LON-65536

        RLAT=0.01*REAL(LAT)
        RLON=0.01*REAL(LON)

!-----------------------------------------------------------------------
! 6th check : Check that the report ID matches requested IDs. If not,
!           : goto 81.
!-----------------------------------------------------------------------

        CALL IDMTCH(CTRAN(IELOOP),THISID,CIDENT,.FALSE.,RC)
        IF (RC /= 0) GOTO 81

        IF (LFLAG) THEN
          WRITE(*,*)'In UPRRET: Identifier matched'
        END IF

!-----------------------------------------------------------------------
! 7th check : Check that the report is within the requested area. If
!           : not, goto 81.
!-----------------------------------------------------------------------

        IF (AREA(1) == 1.0) THEN
          CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)
          CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)
        ELSE IF (AREA(1) == 0.0) THEN
          CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)
        ELSE
          AREA_FLAG=.TRUE.
        END IF

IFLABEL10: &
        IF (AREA_FLAG) THEN

          IF (LFLAG) THEN
            WRITE(*,*)'In UPRRET: Area matched'
          END IF

!-----------------------------------------------------------------------
! 8th check : Check on user's INCREMENT.
!-----------------------------------------------------------------------

          IF (FOUND(4)) THEN
            CALL SUBPER(ITIMST,TRANGE,ITIME(9),ITIMND, &
                        ICMIN,RC)
            IF (RC /= 0) GOTO 81
          END IF

          IF (LFLAG) THEN
            WRITE(*,*)'In UPRRET: passed INCREMENT check'
          END IF

!-----------------------------------------------------------------------
! If we get to here, the report has passed all the checks. KEEP IT!!
!-----------------------------------------------------------------------

          IENTRIES=IENTRIES+1
          CNTRY(IENTRIES)=CTRAN(IELOOP)

          IF (LFLAG) THEN
            WRITE(*,*)'In UPRRET: KEEP THE INDEX ENTRY!!!!!!!'
          END IF

        END IF IFLABEL10 !- area_flag check

      END IF IFLABEL9 !- keepie checks

81        CONTINUE !- end of loop over index entries
    END DO DOLABEL2

!-----------------------------------------------------------------------
! sort entries into identifier & time order and set INDXRED to TRUE to
! say weve read an index block.
!-----------------------------------------------------------------------

    CALL SORTCH(CNTRY,INDLEN,IENTRIES,MASKIT)
    INDXRED=.TRUE.

  END IF IFLABEL3 !- end of .NOT.INDXRED if block

!=======================================================================
! Now loop over the selected index entries. Check that we have not
! already checked this index entry (NTRYCHK=.TRUE.)
! For UAWINDS retrieval go down the chain twice, looking first
! for TEMPs & then for PILOTs.
!=======================================================================

DOLABEL3: &
  DO IELOOP = IENT1,IENTRIES

 789 CONTINUE
IFLABEL11: &
  IF (.NOT.NTRYCHK) THEN ! back here for PILOT if UAWINDS

!-----------------------------------------------------------------------
! get number of reports in the chain. Set it to max if chain too long.
!-----------------------------------------------------------------------

      NCHN = ICHAR(CNTRY(IELOOP)(12:12))
      IF (NCHN > MXCHLEN) NCHN=MXCHLEN

!-----------------------------------------------------------------------
! get data block number (DBLOCK) from index entry. Calculate actual
! block number (ABLOCK) from start of dataset. Then get record number
! (RECNUM) of the data in this data block from the index entry.
!-----------------------------------------------------------------------

      DBLOCK=ICHAR(CNTRY(IELOOP)(22:22))*256+ &
             ICHAR(CNTRY(IELOOP)(23:23))

      ABLOCK=1+NSQ+INXBLK+DBLOCK

      RECNUM=ICHAR(CNTRY(IELOOP)(20:20))*256+ &
             ICHAR(CNTRY(IELOOP)(21:21))

!-----------------------------------------------------------------------
! get the hour and minute from the kept index entry. Convert the date
! to a century hour (ICHR) as IHOUR + IDHR will often mean a new day.
! Treat missing minute (no launch time) as on the nominal hour.
!-----------------------------------------------------------------------

      IDHR=MOD(ICHAR(CNTRY(IELOOP)(1:1)),64)
      IMIN=ICHAR(CNTRY(IELOOP)(2:2))
      IF (IMIN == 255) IMIN=0
      ICHR=DT2HRS(IBLY,IBLM,IDAY,IHOUR)+IDHR

!-----------------------------------------------------------------------
! get the latitude and longitude from the index entry.
!-----------------------------------------------------------------------

      LAT=ICHAR(CNTRY(IELOOP)(13:13))*256+   &
          ICHAR(CNTRY(IELOOP)(14:14))
      LON=ICHAR(CNTRY(IELOOP)(15:15))*256+   &
          ICHAR(CNTRY(IELOOP)(16:16))

      IF (LAT >= 32768) LAT=LAT-65536
      IF (LON >= 32768) LON=LON-65536

      RLAT=0.01*REAL(LAT)
      RLON=0.01*REAL(LON)

!-----------------------------------------------------------------------
! Check byte 17 of the kept index entry. If both bits 5 and 6 are set
! to 1, the ascent is a mixture of TEMPs and PILOTs, set TPMIX = TRUE
!-----------------------------------------------------------------------

      IF (MOD(ICHAR(CNTRY(IELOOP)(17:17))/8,2) == 1 .AND. &
      MOD(ICHAR(CNTRY(IELOOP)(17:17))/4,2) == 1) THEN
        TPMIX = .TRUE.
      ELSE
        TPMIX = .FALSE.
      END IF

      NTRYCHK=.TRUE.

  END IF IFLABEL11 !- ntrychk if block.

!=======================================================================
! There are 3 options now:
!
! 1) Current data retrieval: The user wants more than 1 part
! 2) Current data retrieval: The user wants an individual part
! 3) Merged  data retrieval: The user will get a combined profile
!
!=======================================================================

IFLABEL12: &
    IF (CRTYP == 'C') THEN

! **********************************************************************
! **********************************************************************
! **                                                                  **
! **  1) Current data retrieval : The user wants more than 1 part     **
! **                                                                  **
! **********************************************************************
! **********************************************************************

IFLABEL13: &
      IF ((WANT_PART(1) .AND. WANT_PART(3)) .OR. &
          (WANT_PART(2) .AND. WANT_PART(4)) .OR. &
          (WANT_PART(1) .AND. WANT_PART(2)) .OR. &
          (WANT_PART(3) .AND. WANT_PART(4)) .OR. &
          WANT_ABCD) THEN

IFLABEL14: &
        IF (.NOT.CHANRED) THEN

          KEPCHN=0

!-----------------------------------------------------------------------
! Before looping over the reports in the chain, initialise some
! variables
!-----------------------------------------------------------------------

          DO I=1,4
            KEPT_PART(I)=.FALSE.
            POSN_PART(I)=0
          END DO

!-----------------------------------------------------------------------
! Loop over reports in chain, call MAPRD to read the correct data block
! (ABLOCK), report position in the data block pointed to by RECNUM
! If TEMP & PILOT both wanted (UAWINDS), look for TEMP first.
!-----------------------------------------------------------------------
          IF (USERTEMP.AND.USERPILOT) USERPILOT=.FALSE.
          READ_CHNREPS = .TRUE.

DOLABEL4: &
          DO WHILE (READ_CHNREPS)

            CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
                       LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR, &
                       RECNUM,IRECLN,CMSG(KEPCHN+1),'MSGRD')

!-----------------------------------------------------------------------
! Returns from MAPRD: IRECLN : length of report, CMSG : the report.
!
! As the reports are chained, there is a 23-byte index entry at the end
! of each report which points to the next report in the chain. The
! actual length of the report, therfore, is MSGLEN, as INDLEN=23.
! Put the 23-byte chained index entry in CTRAIL.
!-----------------------------------------------------------------------

            MSGLEN(KEPCHN+1)=IRECLN-INDLEN
            CTRAIL(KEPCHN+1)= &
            CMSG(KEPCHN+1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! get the data block and record number of the next report in the chain
! from the 23-byte chained index entry. If the block or record number
! are zero, we are at the end of the chain. Set READ_CHNREPS=.FALSE.
!-----------------------------------------------------------------------

            DBLOCK=ICHAR(CTRAIL(KEPCHN+1)(22:22))*256 &
                  +ICHAR(CTRAIL(KEPCHN+1)(23:23))

            ABLOCK=1+NSQ+INXBLK+DBLOCK

            RECNUM=ICHAR(CTRAIL(KEPCHN+1)(20:20))*256 &
                  +ICHAR(CTRAIL(KEPCHN+1)(21:21))

            IF (DBLOCK <= 0 .OR. RECNUM <= 0) THEN
              READ_CHNREPS = .FALSE.
            END IF

!-----------------------------------------------------------------------
! check byte 17 of the trailor to see which part the report is.
!-----------------------------------------------------------------------

            TRB17=ICHAR(CTRAIL(KEPCHN+1)(17:17))

            ITS_PART(1)=((MOD(TRB17/2,2) == 0) .AND. &
                        (MOD(TRB17/1,2) == 0))
            ITS_PART(2)=((MOD(TRB17/2,2) == 0) .AND. &
                        (MOD(TRB17/1,2) == 1))
            ITS_PART(3)=((MOD(TRB17/2,2) == 1) .AND. &
                        (MOD(TRB17/1,2) == 0))
            ITS_PART(4)=((MOD(TRB17/2,2) == 1) .AND. &
                        (MOD(TRB17/1,2) == 1))

            IF (LFLAG) THEN
              DO I=1,4
                IF (ITS_PART(I)) WRITE(*,*) 'In UPRRET: part is ', &
                'PART ',PMAP(I)
              END DO
            END IF

!-----------------------------------------------------------------------
! if the index entry contains a mix of TEMPS and PILOTS (TPMIX=.TRUE.)
! then decide from bit 6 of the trailor byte 17 whether the part is
! a TEMP or a PILOT (0=TEMP, 1=PILOT). If the users wants TEMPS and
! the part is a TEMP or the user wants PILOTS and the part is a PILOT
! then we want the part (subject to further checking). If the index
! entry does not contain a mixture of TEMPS and PILOTS, we want the
! part subject to further checking.
!-----------------------------------------------------------------------

IFLABEL15: &
            IF (TPMIX) THEN
              TRL_OB_TYPE=MOD(TRB17/4,2)

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: TPMIX = TRUE, ', &
                'TRL_OB_TYPE = ',TRL_OB_TYPE
              END IF

              IF (USERTEMP .AND. TRL_OB_TYPE == 0) THEN
                WANT_OB_TYPE = .TRUE.
              ELSE IF (USERPILOT .AND. TRL_OB_TYPE == 1) THEN
                WANT_OB_TYPE = .TRUE.
              ELSE
                WANT_OB_TYPE = .FALSE.
              END IF
            ELSE
              WANT_OB_TYPE = .TRUE.
            END IF IFLABEL15

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: WANT_OB_TYPE = ',WANT_OB_TYPE
            END IF

!-----------------------------------------------------------------------
! check to see that the user wants the part, and check that we haven't
! already kept this part - we only want the most preferred of each
! part! Jump out of CHLOOP if we don't want it.
!-----------------------------------------------------------------------

IFLABEL16: &
            IF (ITS_PART(1) .AND. WANT_PART(1) .AND. &
            (.NOT.KEPT_PART(1)) .AND. WANT_OB_TYPE) THEN
              CONTINUE
            ELSE IF (ITS_PART(2) .AND. WANT_PART(2) .AND. &
            (.NOT.KEPT_PART(2)) .AND. WANT_OB_TYPE) THEN
              CONTINUE
            ELSE IF (ITS_PART(3) .AND. WANT_PART(3) .AND. &
            (.NOT.KEPT_PART(3)) .AND. WANT_OB_TYPE) THEN
              CONTINUE
            ELSE IF (ITS_PART(4) .AND. WANT_PART(4) .AND. &
            (.NOT.KEPT_PART(4)) .AND. WANT_OB_TYPE) THEN
              CONTINUE
            ELSE
              IF (LFLAG) WRITE(*,*)'In UPRRET: Part is not wanted'
              GOTO 650
            END IF IFLABEL16

!-----------------------------------------------------------------------
! check the preferred flag for this part. We only want to keep the
! most preferred of each part.
!-----------------------------------------------------------------------

            WASPREF=MOD(ICHAR(CTRAIL(KEPCHN+1)(17:17))/32,2) == 1

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: WASPREF = ',WASPREF
            END IF

IFLABEL17: &
            IF (WASPREF) THEN

!-----------------------------------------------------------------------
! See if time of receipt is in range. Time of receipt (NTOR) is in
! minutes after slot base hour. Actual time of receipt (ITORM)
! in minutes after slot base hour is calculated.
!
! Check to see if NTOR is meant to be negative. NTOR is read from
! 2 bytes which means that the maximum positive value NTOR can have
! is 32768. If NTOR is greater than this value, it must be converted
! into a negative number. This is done simply by subtracting the max
! possible value that can be held  in a 2 byte integer when the sign
! bit is ignored (i.e. 2 to the power 16) from NTOR.
!-----------------------------------------------------------------------

              NTOR=ICHAR(CTRAIL(KEPCHN+1)(18:18))*256 &
                  +ICHAR(CTRAIL(KEPCHN+1)(19:19))

              IF (NTOR > 32768) THEN
                NTOR=NTOR-65536
              END IF

              ITORM=IBLOOP*60+NTOR

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: ITORM         =',ITORM
                WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 =', &
                IRTIM1,IRTIM2
              END IF

!-----------------------------------------------------------------------
! Change check slightly from ITORM > IRTIM2 to ITORM >= IRTIM2
!-----------------------------------------------------------------------

              IF ((IRTIM1 /= 0 .AND. ITORM < IRTIM1) .OR. &
                  (IRTIM2 /= 0 .AND. ITORM >= IRTIM2)) THEN

                IF (LFLAG) THEN
                  WRITE(*,*)'IN UPRRET: Part rejected on TOR'
                END IF

                GOTO 650
              END IF

!-----------------------------------------------------------------------
! If we get to here, we keep the part. Increment KEPCHN. If we don't
! do this, i.e. if we don't want to keep the part, the next part will
! overwrite the previous part.
!-----------------------------------------------------------------------

              KEPCHN=KEPCHN+1

              DO I=1,4
                IF (ITS_PART(I)) THEN
                  KEPT_PART(I)=.TRUE.
                  POSN_PART(I)=KEPCHN
                END IF
              END DO

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: Keep the part!!'
              END IF

            ELSE
              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: part not preferred'
              END IF
            END IF IFLABEL17    !- waspref

650               CONTINUE
          END DO DOLABEL4 !- end of loop over reports in chain

!-----------------------------------------------------------------------
! We now have an array of the parts that have passed the above checks.
! Call subroutine UPRPARTS to put the parts into FinalProfile
!-----------------------------------------------------------------------

IFLABEL18: &
          IF (KEPCHN > 0) THEN

            CALL HRS2DT(HYEAR,HMONTH,HDAY,HHOUR,ICHR)

! If UAWINDS, then if this ascent has both TEMP & PILOT parts,
! and if we're now on PILOTs, put these winds in the second
! half of FINALPROFILE and then combine TEMP & PILOT winds.

IFLABEL19: &
            IF (CTYPE == 'UAWINDS'                               &
                .AND.TPMIX.AND.USERPILOT) THEN
              CALL UPRPARTS(KEPT_PART,POSN_PART,KEPCHN,          &
                          CTRAIL,CMSG,LFLAG,CINDX,ISTAT,         &
                          WANTTEMPS,WANTWINDS,                   &
                          NUMTEMPS,NUMWINDS,RLAT,RLON,           &
                          HHOUR,IMIN,HDAY,HMONTH,HYEAR, &
                          REPORTTEXT,                            &
                          FINALPROFILE(1:MXFINELMS/2+1),         &
                          MXFINELMS,                             &
                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS,&
                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS, &
                          ISECT1,                                &
                          CALLSIGN,NEWBITIND,CCCC,TTAA,          &
                          DISPL,WIDTH,SCALE,REFVAL,FOUND)
              CALL UPRWINDS(FINALPROFILE,MXFINELMS,              &
                            NUMWINDS)
            ELSE
            CALL UPRPARTS(KEPT_PART,POSN_PART,KEPCHN,CTRAIL,CMSG, &
                          LFLAG,CINDX,ISTAT,WANTTEMPS,WANTWINDS, &
                          NUMTEMPS,NUMWINDS,RLAT,RLON,            &
                          HHOUR,IMIN,HDAY,HMONTH,HYEAR, &
                          REPORTTEXT,FINALPROFILE,MXFINELMS, &
                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS, &
                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS, &
                          ISECT1,                                 &
                          CALLSIGN,NEWBITIND,CCCC,TTAA,           &
                          DISPL,WIDTH,SCALE,REFVAL,FOUND)
!
! For UAWINDS retrieval call UPRWINDS even if all the parts are
! PILOTs: this will sort the winds so that levels with pressure
! come first, consistently with TEMP/PILOT mixtures.
! (TRB17 gives the TEMP/PILOT bit in the chain's last trailer:
!  if it says PILOT & there's no mixture, they're all PILOTs.)
!
              IF (CTYPE == 'UAWINDS' .AND. .NOT.TPMIX  &
                  .AND. MOD(TRB17/4,2) == 1) THEN
                CALL UPRWINDS(FINALPROFILE,MXFINELMS,  &
                              NUMWINDS)
              END IF
            END IF IFLABEL19

! If UAWINDS, and if this ascent has both TEMP & PILOT parts,
! then if we've just done the TEMPs, look down the chain again
! for PILOTs; otherwise reset flags for the next index entry.

            IF (CTYPE == 'UAWINDS'.AND.TPMIX) THEN
              IF (USERTEMP) THEN
                USERTEMP=.FALSE.
                USERPILOT=.TRUE.
                NTRYCHK=.FALSE.
                GO TO 789
              ELSE
                USERTEMP=.TRUE.
                USERPILOT=.FALSE.
              END IF
            END IF

!-----------------------------------------------------------------------
! Now put the data into the user's array. Only do this if there were
! no problems in UPRPARTS and the time in the final profile is in
! the retrieval window.  (N.B. minutes may be missing.)
!-----------------------------------------------------------------------

IFLABEL20: &
            IF (IMIN > 60) THEN
              COMB_CENT_MIN=DT2HRS(IFIX(FINALPROFILE(16)),  &
                                 IFIX(FINALPROFILE(18)),    &
                                 IFIX(FINALPROFILE(20)),    &
                                 IFIX(FINALPROFILE(22)))*60
              IF (FINALPROFILE(24) >= 0) THEN
                COMB_CENT_MIN=COMB_CENT_MIN                 &
                        +IFIX(FINALPROFILE(24))
              END IF
              IF (COMB_CENT_MIN < ITIMST .OR.               &
                  COMB_CENT_MIN > ITIMND) ISTAT=16
            END IF IFLABEL20

IFLABEL21: &
            IF (ISTAT == 16) THEN
              ISTAT=0
              KEPCHN=0
            ELSE

              CALL ARRINDX(IDESC,NDES,IREPL,QCREQ, &
                           LFLAG,FINALPROFILE,ARRELMNUM,ARRSEGNUM, &
                           ARRSUBNUM,ARRNROWS,ARRSTYP,ARRSEGST, &
                           ARRSEGLEN,ARRNSEGS,2,ARRNELREQ,DISPL, &
                           ARRSOURCE,NEWSKLARR)

              CALL UPREPID(CNTRY(IELOOP)(17:17),ISECT1(10))

              CHRELM(1)(1:9)=CALLSIGN(1)(1:9)
              LENCHR(1)=9
              CHRELM(2)(1:4)=CCCC
              LENCHR(2)=4
              CHRELM(3)(1:4)=TTAA
              LENCHR(3)=4

              NREP=1
              NSEND=1

              CALL VALARR(DISPL,ARRNELREQ,ARRSOURCE,ISECT1, &
                          CHRELM,LENCHR,IEXTRA,FINALPROFILE, &
                          CNAM,REPORTTEXT,1,NSEND,NREP,      &
                          ARRAY,NOBS,NELEM,IOB,II,CSTR, &
                          CREPRT,LFLAG,LAT_SUBSCRIPT,        &
                          LON_SUBSCRIPT,AREA,RPOLE)

              CHANRED=.TRUE.

              CHLOOP=KEPCHN
              ICHL2=KEPCHN
              ICHL3=KEPCHN

!-----------------------------------------------------------------------
! call CheckPosition to see what room we have left in the users array
! etc and reset pointers, logicals to indicate this.
!-----------------------------------------------------------------------

              CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3, &
                                  IELOOP,INXHRS,IOB,IREP1,ISHR2, &
                                  ISTAT,IENTRIES,NOBS,NSEND, &
                                  NXTCHN,NXTENT,NXTIME,NXTREP, &
                                  CHANRED,INDXRED,NTRYCHK,FLAG999)

              IF (FLAG999) GOTO 999

            END IF IFLABEL21 !- istat

          END IF IFLABEL18 !- kepchn > 0 for more than 1 part

        END IF IFLABEL14 !- end of chanred if block for more than 1 part.

! **********************************************************************
! **********************************************************************
! **                                                                  **
! **  2) Current data retrieval: The user wants an individual part    **
! **                                                                  **
! **********************************************************************
! **********************************************************************

      ELSE

!-----------------------------------------------------------------------
! loop over number of reports (NCNN) in chain, call MAPRD to read the
! correct data block (ABLOCK), report position in the data block pointed
! to by RECNUM
!-----------------------------------------------------------------------

IFLABEL22: &
        IF (.NOT.CHANRED) THEN

          KEPCHN=0

          READ_CHNREPS = .TRUE.

DOLABEL5: &
          DO WHILE (READ_CHNREPS)

            CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
                       LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR, &
                       RECNUM,IRECLN,CMSG(KEPCHN+1),'MSGRD')

!-----------------------------------------------------------------------
! Returns from MAPRD: IRECLN : length of report, CMSG : the report.
!
! As the reports are chained, there is a 23-byte index entry at the end
! of each report which points to the next report in the chain. The
! actual length of the report, therfore, is MSGLEN, as INDLEN=23.
! Put the 23-byte chained index entry in CTRAIL.
!-----------------------------------------------------------------------

            MSGLEN(KEPCHN+1)=IRECLN-INDLEN
            CTRAIL(KEPCHN+1)= &
            CMSG(KEPCHN+1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! get the data block and record number of the next report in the chain
! from the 23-byte chained index entry. If the block or record number
! are zero, we are at the end of the chain. Set READ_CHNREPS=.FALSE.
!-----------------------------------------------------------------------

            DBLOCK=ICHAR(CTRAIL(KEPCHN+1)(22:22))*256 &
                  +ICHAR(CTRAIL(KEPCHN+1)(23:23))

            ABLOCK=1+NSQ+INXBLK+DBLOCK

            RECNUM=ICHAR(CTRAIL(KEPCHN+1)(20:20))*256 &
                  +ICHAR(CTRAIL(KEPCHN+1)(21:21))

            IF (DBLOCK <= 0 .OR. RECNUM <= 0) THEN
              READ_CHNREPS = .FALSE.
            END IF

!-----------------------------------------------------------------------
! check byte 17 of the trailor to see which part the report is.
!-----------------------------------------------------------------------

            TRB17=ICHAR(CTRAIL(KEPCHN+1)(17:17))

            ITS_PART(1)=((MOD(TRB17/2,2) == 0) .AND. &
                         (MOD(TRB17/1,2) == 0))
            ITS_PART(2)=((MOD(TRB17/2,2) == 0) .AND. &
                         (MOD(TRB17/1,2) == 1))
            ITS_PART(3)=((MOD(TRB17/2,2) == 1) .AND. &
                         (MOD(TRB17/1,2) == 0))
            ITS_PART(4)=((MOD(TRB17/2,2) == 1) .AND. &
                         (MOD(TRB17/1,2) == 1))

            IF (LFLAG) THEN
              DO I=1,4
                IF (ITS_PART(I)) WRITE(*,*) 'In UPRRET: part is ', &
                'PART ',PMAP(I)
              END DO
            END IF

!-----------------------------------------------------------------------
! if the index entry contains a mix of TEMPS and PILOTS (TPMIX=.TRUE.)
! then decide from bit 6 of the trailor byte 17 whether the part is
! a TEMP or a PILOT (0=TEMP, 1=PILOT). If the users wants TEMPS and
! the part is a TEMP or the user wants PILOTS and the part is a PILOT
! then we want the part (subject to further checking). If the index
! entry does not contain a mixture of TEMPS and PILOTS, we want the
! part subject to further checking.
!-----------------------------------------------------------------------

IFLABEL23: &
            IF (TPMIX) THEN
              TRL_OB_TYPE=MOD(TRB17/4,2)

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: TPMIX = TRUE, ', &
                'TRL_OB_TYPE = ',TRL_OB_TYPE
              END IF

              IF (USERTEMP .AND. TRL_OB_TYPE == 0) THEN
                WANT_OB_TYPE = .TRUE.
              ELSE IF (USERPILOT .AND. TRL_OB_TYPE == 1) THEN
                WANT_OB_TYPE = .TRUE.
              ELSE
                WANT_OB_TYPE = .FALSE.
              END IF
            ELSE
              WANT_OB_TYPE = .TRUE.
            END IF IFLABEL23

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: WANT_OB_TYPE = ',WANT_OB_TYPE
            END IF

!-----------------------------------------------------------------------
! check to see that the user wants the part, and check that we haven't
! already kept this part - we only want the most preferred of each
! part! Jump out of CHLOOP if we don't want it.
!-----------------------------------------------------------------------

IFLABEL24: &
            IF (ITS_PART(1) .AND. WANT_PART(1) .AND. &
            WANT_OB_TYPE) THEN
             CONTINUE
            ELSE IF (ITS_PART(2) .AND. WANT_PART(2) .AND. &
            WANT_OB_TYPE) THEN
              CONTINUE
            ELSE IF (ITS_PART(3) .AND. WANT_PART(3) .AND. &
            WANT_OB_TYPE) THEN
              CONTINUE
            ELSE IF (ITS_PART(4) .AND. WANT_PART(4) .AND. &
            WANT_OB_TYPE) THEN
              CONTINUE
            ELSE
              IF (LFLAG) WRITE(*,*)'In UPRRET: Part is not wanted'
              GOTO 660
            END IF IFLABEL24

!-----------------------------------------------------------------------
! See if time of receipt is in range.
!
! Check to see if NTOR is meant to be negative. NTOR is read from
! 2 bytes which means that the maximum positive value NTOR can have
! is 32768. If NTOR is greater than this value, it must be converted
! into a negative number. This is done simply by subtracting the max
! possible value that can be held  in a 2 byte integer when the sign
! bit is ignored (i.e. 2 to the power 16) from NTOR.
!-----------------------------------------------------------------------

            NTOR=ICHAR(CTRAIL(KEPCHN+1)(18:18))*256 &
                +ICHAR(CTRAIL(KEPCHN+1)(19:19))

            IF (NTOR > 32768) THEN
              NTOR=NTOR-65536
            END IF

            ITORM=IBLOOP*60+NTOR

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: ITORM         =',ITORM
              WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 =',IRTIM1,IRTIM2
            END IF

!-----------------------------------------------------------------------
! Change check slightly from ITORM > IRTIM2 to ITORM >= IRTIM2
!-----------------------------------------------------------------------

            IF ((IRTIM1 /= 0 .AND. ITORM < IRTIM1) .OR. &
                (IRTIM2 /= 0 .AND. ITORM >= IRTIM2)) THEN

              IF (LFLAG) THEN
                WRITE(*,*)'IN UPRRET: Part rejected on TOR'
              END IF
              GOTO 660
            END IF

!-----------------------------------------------------------------------
! check the preferred flag for this part. We only want to keep the
! most preferred of each part.
!-----------------------------------------------------------------------

            WASPREF=MOD(ICHAR(CTRAIL(KEPCHN+1)(17:17))/32,2) == 1

            IF (LFLAG) WRITE(*,*) 'In UPRRET: WASPREF = ',WASPREF

!-----------------------------------------------------------------------
! If the user doesn't want all the reports (VER /= 2), check the "was
! preferred" flag WASPREF of the report. It it is set, we have found
! the preferred report. This entry will be kept. Jump out of the loop.
! If the user wants all reports in the chain, increment the pointer
! KEPCHN.
!-----------------------------------------------------------------------

IFLABEL25: &
            IF (VER /= 2) THEN
              IF (WASPREF) THEN
                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: Keep the part!!'
                END IF
                KEPCHN=KEPCHN+1
                GOTO 661
              END IF
            ELSE
              KEPCHN=KEPCHN+1
              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: Keep the part!!'
              END IF
            END IF IFLABEL25

660               CONTINUE
          END DO DOLABEL5 !- end of loop over reports in chain

661             CONTINUE

          ICHL1=1
          ICHL2=KEPCHN
          ICHL3=1

          CHANRED=.TRUE.

        END IF IFLABEL22 !- end of chanred if block.

!-----------------------------------------------------------------------
! for user wanting 1 part only, loop over message in chain not rejected
! by the checks above.
!-----------------------------------------------------------------------

IFLABEL26: &
        IF (KEPCHN > 0) THEN

DOLABEL6: &
          DO CHLOOP=ICHL1,ICHL2,ICHL3

            DO I=1,4
              IF (WANT_PART(I)) THEN
                KEPT_PART(I)=.TRUE.
                POSN_PART(I)=1
              ELSE
                KEPT_PART(I)=.FALSE.
                POSN_PART(I)=0
              END IF
            END DO

            NREP=1
            NSEND=NREP

            CALL HRS2DT(HYEAR,HMONTH,HDAY,HHOUR,ICHR)
            CALL UPRPARTS(KEPT_PART,POSN_PART,1,CTRAIL(CHLOOP), &
                          CMSG(CHLOOP), &
                          LFLAG,CINDX,ISTAT,WANTTEMPS,WANTWINDS, &
                          NUMTEMPS,NUMWINDS,RLAT,RLON,HHOUR,     &
                          IMIN,HDAY,HMONTH,HYEAR, &
                          REPORTTEXT,FINALPROFILE,MXFINELMS, &
                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS, &
                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS, &
                          ISECT1,                                &
                          CALLSIGN,NEWBITIND,CCCC,TTAA,          &
                          DISPL,WIDTH,SCALE,REFVAL,FOUND)

!-----------------------------------------------------------------------
! Now put the data into the user's array. Only do this if there are
! no problems in UPRPARTS.
!-----------------------------------------------------------------------

IFLABEL27: &
            IF (ISTAT == 16) THEN
              ISTAT=0
            ELSE

              CALL ARRINDX(IDESC,NDES,IREPL,QCREQ,LFLAG, &
                           FINALPROFILE,ARRELMNUM,ARRSEGNUM, &
                           ARRSUBNUM,ARRNROWS,ARRSTYP,ARRSEGST, &
                           ARRSEGLEN,ARRNSEGS,2,ARRNELREQ,DISPL, &
                           ARRSOURCE,NEWSKLARR)

              CALL UPREPID(CNTRY(IELOOP)(17:17),ISECT1(10))

              CHRELM(1)(1:9)=CALLSIGN(1)(1:9)
              LENCHR(1)=9
              CHRELM(2)(1:4)=CCCC
              LENCHR(2)=4
              CHRELM(3)(1:4)=TTAA
              LENCHR(3)=4

              CALL VALARR(DISPL,ARRNELREQ,ARRSOURCE,ISECT1, &
                          CHRELM,LENCHR,IEXTRA,FINALPROFILE, &
                          CNAM,REPORTTEXT,1,NSEND,NREP,        &
                          ARRAY,NOBS,NELEM,IOB,II,CSTR,CREPRT, &
                          LFLAG,LAT_SUBSCRIPT,LON_SUBSCRIPT,   &
                          AREA,RPOLE)

!-----------------------------------------------------------------------
! call CheckPosition to see what room we have left in the users array
! etc and reset pointers, logicals to indicate this.
!-----------------------------------------------------------------------

              CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3, &
                                  IELOOP,INXHRS,IOB,IREP1,ISHR2, &
                                  ISTAT,IENTRIES,NOBS,NSEND, &
                                  NXTCHN,NXTENT,NXTIME,NXTREP, &
                                  CHANRED,INDXRED,NTRYCHK,FLAG999)

              IF (FLAG999) GOTO 999

            END IF IFLABEL27   !- istat == 16 check

          END DO DOLABEL6  !- chloop for kept single parts

        END IF IFLABEL26    !- kepchn > 0 for kept single parts

      END IF IFLABEL13    !-wantmore than 1 part or just 1 part if block

! **********************************************************************
! **********************************************************************
! **                                                                  **
! **  3) Merged data retrieval: The user will get a combined profile
! **                                                                  **
! **********************************************************************
! **********************************************************************

    ELSE IF (CRTYP == 'M') THEN

      CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG, &
                 LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR, &
                 RECNUM,IRECLN,CMSG_MER(1),'MSGRD')

      MSGLEN(1)=IRECLN-INDLEN
      CTRAIL(1)=CMSG_MER(1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! Check that the TOR is in range.
!-----------------------------------------------------------------------

      NTOR=ICHAR(CTRAIL(1)(18:18))*256 + ICHAR(CTRAIL(1)(19:19))

      IF (NTOR > 32768) THEN
        NTOR=NTOR-65536
      END IF

      ITORM=IBLOOP*60+NTOR

      IF (LFLAG) THEN
        WRITE(*,*)'In UPRRET: ITORM         = ',ITORM
        WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 = ',IRTIM1,IRTIM2
      END IF

IFLABEL28: &
      IF ((IRTIM1 /= 0 .AND. ITORM < IRTIM1) .OR. &
          (IRTIM2 /= 0 .AND. ITORM >= IRTIM2)) THEN

        IF (LFLAG) WRITE(*,*)'In UPRRET: Ob rejected on TOR'

!-----------------------------------------------------------------------
! Keep the observation. Find the start of the BUFR message and extract
! the sequence descriptor.
!-----------------------------------------------------------------------

      ELSE

        BUFRSTART=INDEX(CMSG_MER(1),BUFR)
        BUFREND=INDEX(CMSG_MER(1),SEVENS)+3

        ILOCALD=ICHAR(CMSG_MER(1)                    &
                (BUFRSTART+29:BUFRSTART+29))*256 +   &
                ICHAR(CMSG_MER(1)                    &
                (BUFRSTART+30:BUFRSTART+30))

        CALL DESFXY(ILOCALD,F,X,Y)
        WRITE(LOCALD,'(I1,I2.2,I3.3)')F,X,Y

!-----------------------------------------------------------------------
! If this is a new call to the MDB, i.e. ISTAT=0, loop over the IDESC
! array to see if the user has requested LEVL_CDTN_CODE and
! LEVL_RPLTN_CONT. Keep the IDESC value (pointer to DISPL array) for
! LEVL_RPLTN_CONT.
!
! If the user has requested LEVL_CDTN_CODE but not LEVL_RPLTN_CONT,
! issue a warning as we cannot set LEVL_CDTN_CODE without knowing the
! LEVL_RPLTN_CONT.
!-----------------------------------------------------------------------

IFLABEL29: &
        IF (NEWMRGCALL) THEN

          NEWMRGCALL=.FALSE.
          FOUNDLCC=.FALSE.
          FOUNDLRC=.FALSE.

          DO I=1,NDES
            IF (IDESC(I) == 19) FOUNDLCC=.TRUE.
            IF (IDESC(I) == 20) THEN
              FOUNDLRC=.TRUE.
              LRCPOINT=I
            END IF
          END DO

          IF (FOUNDLCC .AND. .NOT.FOUNDLRC) THEN
            FOUNDLCC=.FALSE.
            WRITE(*,*)'In UPRRET: MDB WARNING: LEVL_CDTN_CODE ', &
            'WILL BE MISSING. REQUEST LEVL_RPLTN_CONT AS WELL'
          END IF

        END IF IFLABEL29

!-----------------------------------------------------------------------
! Call BITINDX to calculate the bit displacements of the elements
! requested by the user.
!-----------------------------------------------------------------------

        CALL BITINDX(CINDX,IDESC,NDES,IREPL,QCREQ,       &
                     IFAIL,LFLAG,LOCALD,CMSG_MER(1),     &
                     ARRNELREQ,DISPL,WIDTH,SCALE,REFVAL, &
                     MXFINELMS,NEWBITIND)

IFLABEL30: &
        IF (IFAIL == 16) THEN
          ISTAT=16
          IF (LFLAG) THEN
            WRITE(*,*)'In UPRRET: Element Index not found'
          END IF
        END IF IFLABEL30

!-----------------------------------------------------------------------
! If we are to set LEVL_CDTN_CODE (FOUNDLCC is true), call VALUE to
! get the LEVL_RPLTN_CONT value from the BUFR message using the DISPL
! and WIDTH values for LEVL_RPLTN_CONT. IT IS IMPORTANT NOT TO PASS
! DISPL TO VALUE AS IT WE BE ALTERED!!!!!. If LEVL_RPLTN count is  <=
! to the number of levels requested by the user, set LEVL_CDTN_CODE to
! 0, otherwise to 8. If the user doesn't want the LEVL_CDTN_CODE or has
! requested it but not LEVL_RPLTN_CONT, set LEVL_CDTN_CODE to missing.
!-----------------------------------------------------------------------

IFLABEL31: &
        IF (FOUNDLCC) THEN
          IBEFOR=DISPL(LRCPOINT)
          I=VALUE(CMSG_MER(1)(BUFRSTART:),IBEFOR, &
            WIDTH(LRCPOINT))
          IF (I <= USERLEVS) THEN
            ISECT1(11)=0
          ELSE
            ISECT1(11)=8
          END IF
        ELSE
          ISECT1(11)=-9999999
        END IF IFLABEL31

!-----------------------------------------------------------------------
! Call UPRREPID to get the RPRT_IDNY from the index entry and put it in
! ISECT1(10). Get the PESR_SNSR_FLAG from trailor byte 17, bit 5 and
! put it in ISECT1(12)
!-----------------------------------------------------------------------

        CALL UPREPID(CNTRY(IELOOP)(17:17),ISECT1(10))

        ISECT1(12) = MOD(ICHAR(CTRAIL(1)(17:17))/8,2)

!-----------------------------------------------------------------------
! Call VALUSR to put the retrieval elements in the user's array. If the
! user wants to retrieve BUFR messages, FOUND(34) is TRUE.
!-----------------------------------------------------------------------

IFLABEL32: &
        IF (FOUND(34)) THEN
          WRITE(HELN,'(I5)')BUFREND-BUFRSTART+1
          CALL VALUSR(CMSG_MER(1)(BUFRSTART:),             &
                      ARRNELREQ,DISPL,WIDTH,SCALE,         &
                      REFVAL,ARRAY,IOB+1,NOBS,CSTR,        &
                      HELN//CMSG_MER(1)(BUFRSTART:BUFREND),&
                      CREPRT,LFLAG,ISECT1,CHRELM,          &
                      LENCHR)
        ELSE
          HELN='    1'
          CALL VALUSR(CMSG_MER(1)(BUFRSTART:),ARRNELREQ, &
                      DISPL,WIDTH,SCALE,REFVAL,ARRAY,    &
                      IOB+1,NOBS,CSTR,HELN//' ',CREPRT,  &
                      LFLAG,ISECT1,CHRELM,LENCHR)
        END IF IFLABEL32

        II=IOB+1

        CHANRED=.TRUE.
        CHLOOP=1
        ICHL2=1
        ICHL3=1
        NREP=1
        NSEND=1

!-----------------------------------------------------------------------
! call CheckPosition to see what room we have left in the users array
! etc and reset pointers, logicals to indicate this.
!-----------------------------------------------------------------------

        CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,IELOOP, &
                            INXHRS,IOB,IREP1,ISHR2,ISTAT, &
                            IENTRIES,NOBS,NSEND,NXTCHN, &
                            NXTENT,NXTIME,NXTREP,CHANRED, &
                            INDXRED,NTRYCHK,FLAG999)

        IF (FLAG999) GOTO 999

      END IF IFLABEL28 !- ITORM check for merged retrieval.

!=======================================================================
! Upper Air Retrieval from Merged data. End Of New Section
!=======================================================================

    ELSE
      WRITE(*,*)'In UPRRET: Invalid CRTYP. CRTYP = ',CRTYP
    END IF IFLABEL12

    NTRYCHK = .FALSE.
    CHANRED = .FALSE.

  END DO DOLABEL3 !- end of loop over kept index entries (ieloop)

  IENT1   = 1
  INDXRED = .FALSE.

799   CONTINUE   !- end of loop over index blocks (ibloop)
END DO DOLABEL1 !- end of loop over index blocks (ibloop)

IF (IOB == 0) THEN
  IF (LCONT) THEN
    ISTAT=0
  ELSE
    ISTAT=8
  END IF
END IF

999   CONTINUE

IF (II /= 0) THEN
  IOBS=II
ELSE
  IOBS=IOBS-1
END IF


RETURN
CONTAINS

!***********************************************************************
!**   Subroutine CheckPositions
!***********************************************************************

  SUBROUTINE CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,IELOOP, &
                            INXHRS,IOB,IREP1,ISHR2,ISTAT,ITRIES, &
                            NOBS,NSEND,NXTCHN,NXTENT,NXTIME, &
                            NXTREP,CHANRED,INDXRED,NTRYCHK,FLAG999)

!-----------------------------------------------------------------------
!
! subroutine    : CheckPositions
!
!               : ANSI standard except for IMPLICIT NONE and '!' used
!               : for comments
!
! purpose       : MetDB Upper-Air retrieval. Used to check the positions
!               : in users arrays, index blocks, index entries and
!               : chained report loops for multi-user calls to the MetDB
!               : of the data.
!
! data type(s)  : TEMP, PILOT, DROPSOND
!
! called by     : UPRRET
!
! sub calls     : None
!
! arguments     :
!
! change record :
!
! 20-09-96      : Written S.Cox
!
!-----------------------------------------------------------------------

! Use statements:
! None

  IMPLICIT NONE

! Subroutine arguments:

  INTEGER,   INTENT(IN)    ::  CHLOOP !- chained report loop number
  INTEGER,   INTENT(IN)    ::  IBLOOP !- index block loop number
  INTEGER,   INTENT(IN)    ::  ICHL2 !- end of chained reports loop
  INTEGER,   INTENT(IN)    ::  ICHL3 !- chained reports step
  INTEGER,   INTENT(IN)    ::  IELOOP !- index entries loop number
  INTEGER,   INTENT(IN)    ::  INXHRS !- no. of hours per index block
  INTEGER,   INTENT(INOUT) ::  IOB !- last ob number passed to users array
  INTEGER,   INTENT(INOUT) ::  IREP1
  INTEGER,   INTENT(IN)    ::  ISHR2
  INTEGER,   INTENT(INOUT) ::  ISTAT
  INTEGER,   INTENT(IN)    ::  ITRIES
  INTEGER,   INTENT(INOUT) ::  NOBS
  INTEGER,   INTENT(INOUT) ::  NSEND
  INTEGER,   INTENT(OUT)   ::  NXTCHN
  INTEGER,   INTENT(OUT)   ::  NXTENT
  INTEGER,   INTENT(OUT)   ::  NXTIME
  INTEGER,   INTENT(OUT)   ::  NXTREP
  LOGICAL,   INTENT(OUT)   ::  CHANRED
  LOGICAL,   INTENT(OUT)   ::  INDXRED
  LOGICAL,   INTENT(OUT)   ::  NTRYCHK
  LOGICAL,   INTENT(OUT)   ::  FLAG999

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

  EXTERNAL EOCHN

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

  LOGICAL EOCHN        !- declare function

  SAVE

  IOB =IOB+(NSEND-IREP1)+1 ! increment no of reports transferred
  NSEND = 0
  NXTREP = 1
  IREP1 = 1
  FLAG999=.FALSE.

IFLABEL33: &
  IF (IOB == NOBS) THEN            ! if array is full

!-----------------------------------------------------------------------
! increment loop variables for entry next time
!-----------------------------------------------------------------------

IFLABEL34: &
    IF (EOCHN(CHLOOP,ICHL2,ICHL3)) THEN ! if end of chain
      NXTCHN = 1                   ! start of chain for next entry)
      CHANRED = .FALSE.
      IF (IELOOP+1 > ITRIES) THEN ! if no more entries
        NXTENT = 1                 ! first entry
        IF (IBLOOP+INXHRS > ISHR2) FLAG999=.TRUE.
        NXTIME=IBLOOP+INXHRS       ! in next entry (if there is one)
        INDXRED=.FALSE.
      ELSE
        NXTENT=IELOOP+1        ! next entry
        NXTIME=IBLOOP          ! in same index
      END IF
      NTRYCHK = .FALSE.
    ELSE                       ! if not end of chain
      NXTCHN = CHLOOP + ICHL3  ! next in chain (add +1 or -1)
      NXTENT = IELOOP
      NXTIME = IBLOOP
    END IF IFLABEL34
    ISTAT = 4                  ! more data to come (maybe)
    FLAG999=.TRUE.
  END IF IFLABEL33

  RETURN
  END SUBROUTINE CheckPositions
END SUBROUTINE UPRRET
