PROGRAM WEBRET

!-----------------------------------------------------------------------
!
! routine       : WEBRET
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters,
!               : END DO statements.
!
! purpose       : To call the MetDB to extract raw reports and output
!               : them to a dataset (FT06) for web display.
!
! description   : The Web MetDB extraction page submits a QUERY to a
!               : perl CGI script on Open Edition. That runs the
!               : load module built from this source.
!               : This code calls GENENV to get variables from the perl
!               : script. It 'fetches' MetDB retrieval request details
!               : entered by the user on web page and 'sets' the
!               : number of observations satisfying the web request.
!
! data types    : Those where raw reports are stored in the MetDB
!
! calls         : DELSPCE  : to strip extra spaces from request
!               : MDBC     : to get description of BUFR code or flag
!               : REXXVAR  : get BUFR displacements from message
!
! REVISION INFO:
!
! $Workfile: webret.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 11/04/2013 11:36:28$
!
!-----------------------------------------------------------------------
! change record :
!
! $Log:
!  7    Met_DB_Project 1.6         11/04/2013 11:36:28    John Norton     Added
!        changes for ATDNET
!  6    MetDB_Refresh 1.5         20/07/2012 10:16:49    John Norton
!       Updates required to make operational
!  5    MetDB_Refresh 1.4         16/07/2012 15:56:27    John Norton
!       Updated after review with Stan.
!  4    MetDB_Refresh 1.3         11/05/2012 16:09:16    John Norton     Check
!       in prior to going on leave. Still needs more testing!
!  3    MetDB_Refresh 1.2         25/04/2011 10:13:48    John Norton
!       Updated after retrieval testing.
!  2    MetDB_Refresh 1.1         01/03/2011 09:41:32    John Norton     After
!       porting.
!  1    MetDB_Refresh 1.0         23/02/2011 14:41:28    John Norton     Prior
!       to porting to f95
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE delspce_mod
USE f90_unix_env  ! Required for GETENV
USE webair_mod
USE webbuoyp_mod
USE webdis_mod
USE webesaw_mod
USE webform_mod
USE weblnd_mod
USE webncm_mod
USE websamos_mod
USE webshp_mod
USE websrew_mod
USE webssea_mod
USE webtadv_mod
USE webtbus_mod
USE webtfmet_mod
USE webupr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

! None - Main program

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  IOBS= 25  !- number of obs for MDB call
INTEGER,     PARAMETER ::  IELS= 354 !- number of elements for MDB call

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

!    INTEGER          ::  EHOURS    !- user end hours
!    INTEGER          ::  END_TIME  !- end time (century mins)
INTEGER          ::  I         !- general loop counter
INTEGER          ::  IMAXOBS   !- user no. of max obs to retrieve
INTEGER          ::  ISTAT     !- return code from MDB
INTEGER          ::  J         !- general loop counter
INTEGER          ::  LEN_CREQ2 !- length of DELSPCE req string
INTEGER          ::  NELEM     !- number of elements for MDB call
INTEGER          ::  NOBS      !- number of obs for MDB call
INTEGER          ::  RPRTEND   !- end of report text from MDB
!    INTEGER          ::  RPRTSTART !- start of report text from MDB
!    INTEGER          ::  SHOURS    !- user start hours
!    INTEGER          ::  START_TIME !- start time (century mins)
INTEGER          ::  TotalObs  !- total obs retrieved from MDB

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL             ::  ARRAY(IOBS,IELS) !- array of values from MDB

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL          ::  DATIME    !- TRUE if user wants a time series
LOGICAL          ::  ReadMoreData !- TRUE if more MDB data to read
LOGICAL          ::  WRITEHEADER !- TRUE if user wants MDB header

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(LEN=35)   ::  AREA   !- AREA request for MDB
CHARACTER(LEN=6)    ::  AREA_N  !- user area request (top lat)
CHARACTER(LEN=6)    ::  AREA_S  !- user area request (low lat)
CHARACTER(LEN=6)    ::  AREA_W  !- user area request (left lon)
CHARACTER(LEN=6)    ::  AREA_E  !- user area request (right lon)
CHARACTER(LEN=4)    ::  CHAROBTOT !- total obs retrieved from MDB
CHARACTER(LEN=8500) ::  CREP(IOBS) !- MDB report text array
CHARACTER(LEN=8500) ::  CREP2 !- MDB report text (expanded)
CHARACTER(LEN=1200) ::  CREQ !- users MDB request
CHARACTER(LEN=1200) ::  CREQ2 !- MDB request (no blanks)
CHARACTER(LEN=9)    ::  CSTR(IOBS) !- MDB element strings
CHARACTER(LEN=8)    ::  CSUBT   !- MDB subtype
CHARACTER(LEN=950)  ::  ELEMENTS !- ELEMENTS request for MDB
CHARACTER(LEN=8)    ::  ENDDATE !- user end date request
CHARACTER(LEN=2)    ::  ENDHOUR !- user end hour request
CHARACTER(LEN=2)    ::  ENDMINUTE !- user end minute request
CHARACTER(LEN=2)    ::  INCHOUR !- users increment request
CHARACTER(LEN=12)   ::  INCREMENT !- INCREMENT request for MDB
CHARACTER(LEN=20)   ::  IN_TEXT !- text to preceed output report
CHARACTER(LEN=18)   ::  KEY_PARTS !- UPRAIR parts request for MDB
CHARACTER(LEN=4)    ::  MAXOBS  !- users max obs request
CHARACTER(LEN=15)   ::  REPORT_STRING !- RPRT_TEXT request for MDB
CHARACTER(LEN=8)    ::  STARTDATE !- user start date request
CHARACTER(LEN=2)    ::  STARTHOUR !- user start hour request
CHARACTER(LEN=2)    ::  STARTMINUTE !- user start minute request
CHARACTER(LEN=110)  ::  PLATFORM !- PLATFORM request for MDB
CHARACTER(LEN=100)  ::  STNS  !- users platform request
CHARACTER(LEN=4)    ::  SONDEPARTS !- users sonde parts request
CHARACTER(LEN=30)   ::  SUBMIT !- users submit type request
CHARACTER(LEN=11)   ::  TOR    !- Time Of Receipt
CHARACTER(LEN=11)   ::  TOR_UPR(4) !- Time Of Receipt - sonde
CHARACTER(LEN=3)    ::  HEADER  !- users header request
CHARACTER(LEN=3)    ::  VERSIONS !- users version request
CHARACTER(LEN=17)   ::  VERSIONSTR !- VERSION request for MDB

CHARACTER*80   TEMPFILE        !- file for passing obs count

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Dynamic common
!-----------------------------------------------------------------------

COMMON /WEBRTDC1/ ARRAY,CREP,CREP2,CREQ,CREQ2,CSTR,ELEMENTS

!-----------------------------------------------------------------------
! Get subtype, platforms, start date, start hour, start minute,
! end date, end hour, end minute, type of request, maxobs,
! header wanted, versions, increment, sondeparts, area from calling
! REXX script
!-----------------------------------------------------------------------

      CALL GETENV('SUBTYPE',CSUBT)
      CALL GETENV('STNS',STNS)
      CALL GETENV('SDATE',STARTDATE)
      CALL GETENV('SHOUR',STARTHOUR)
      CALL GETENV('SMINUTE',STARTMINUTE)
      CALL GETENV('EDATE',ENDDATE)
      CALL GETENV('EHOUR',ENDHOUR)
      CALL GETENV('EMINUTE',ENDMINUTE)
      CALL GETENV('SUBMIT',SUBMIT)
      CALL GETENV('MAXOBS',MAXOBS)
      CALL GETENV('HEADER',HEADER)
      CALL GETENV('VERSIONS',VERSIONS)
      CALL GETENV('INCREMENT',INCHOUR)
      CALL GETENV('SONDEPARTS',SONDEPARTS)
      CALL GETENV('AREA_N',AREA_N)
      CALL GETENV('AREA_S',AREA_S)
      CALL GETENV('AREA_W',AREA_W)
      CALL GETENV('AREA_E',AREA_E)

!-----------------------------------------------------------------------
! To test a web retrieval on the IBM platform only (no CGI involved),
! comment out all the GETVAR calls in WEBRET and uncomment the lines
! below. Change the lines to suit your test. The load module that this
! routine belongs to is SYS1.SDBLOAD(WEBRET).
!-----------------------------------------------------------------------

!      CSUBT       = 'AMDARS  '
!      STNS        = '  '
!      STARTDATE   = '20010116'
!      STARTHOUR   = '00'
!      STARTMINUTE = '00'
!      ENDDATE     = '20010116'
!      ENDHOUR     = '00'
!      ENDMINUTE   = '59'
!      SUBMIT      = 'SUBMIT METDB RETRIEVAL'
!      MAXOBS      = '0500'
!      HEADER      = 'NO'
!      VERSIONS    = ' '
!      INCHOUR     = '00'
!      SONDEPARTS  = ' '
!      AREA_N      = ' '
!      AREA_S      = ' '
!      AREA_W      = ' '
!      AREA_E      = ' '

!TST  CSUBT       = 'LNDSYN  '
!TST  STNS        = '  '
!TST  STARTDATE   = '20000831'
!TST  STARTHOUR   = '00'
!TST  STARTMINUTE = '00'
!TST  ENDDATE     = '20000831'
!TST  ENDHOUR     = '00'
!TST  ENDMINUTE   = '59'
!TST  SUBMIT      = 'SUBMIT METDB RETRIEVAL'
!TST  MAXOBS      = '0500'
!TST  HEADER      = 'NO'
!TST  VERSIONS    = ' '
!TST  INCHOUR     = '00'
!TST  SONDEPARTS  = ' '
!TST  AREA_N      = ' '
!TST  AREA_S      = ' '
!TST  AREA_W      = ' '
!TST  AREA_E      = ' '

!-----------------------------------------------------------------------
! Set default MAXOBS to avoid problems with internal read in testing
!-----------------------------------------------------------------------

! MAXOBS      = '0500'  !testing error prevention option

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

CHAROBTOT       = '0000'
ISTAT           = 0
NELEM           = IELS
NOBS            = IOBS
CREQ(:)         = ' '
CREQ2(:)        = ' '
ELEMENTS(:)     = ' '
KEY_PARTS(:)    = ' '

DO I=1,IOBS
  CREP(I)(:) = ' '
END DO

!-----------------------------------------------------------------------
! Put the 'max obs to retrieve' (set by user) into variable IMAXOBS.
!-----------------------------------------------------------------------

READ(MAXOBS(1:4),'(I4)')IMAXOBS

!-----------------------------------------------------------------------
! If the user wants a MetDB header to be output, WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

WRITEHEADER = (INDEX(HEADER,'YES') > 0)

!-----------------------------------------------------------------------
! Set VERSIONSTR for inclusion in request string CREQ depending on
! user choice.
!-----------------------------------------------------------------------

IF (INDEX(VERSIONS,'YES') > 0) THEN
  VERSIONSTR(1:17) = 'VERSION ALL      '
ELSE
  VERSIONSTR(1:17) = 'VERSION PREFERRED'
END IF

!-----------------------------------------------------------------------
! Check for INCREMENT 00 (Passed if no INCREMENT wanted by user for a
! subtype that allows the INCREMENT keyword). If '00' set INCREMENT
! string to null, otherwise, construct INCREMENT string.
!-----------------------------------------------------------------------

  IF (INCHOUR(1:2) < '01' .OR. INCHOUR(1:2) > '24') THEN
    INCREMENT(:) = ' '
  ELSE
    INCREMENT = 'INCREMENT '//INCHOUR
  END IF

!-----------------------------------------------------------------------
! Produce an AREA request if an AREA is selected.
!-----------------------------------------------------------------------

  IF (AREA_N /= ' ' .AND. AREA_S /= ' ' .AND. &
      AREA_W /= ' ' .AND. AREA_E /= ' ') THEN
    AREA = 'AREA ' // AREA_N // ' ' // AREA_S // ' ' // &
                      AREA_W // ' ' // AREA_E
  ELSE
    AREA(:) = ' '
  END IF

!-----------------------------------------------------------------------
! If the user wants TEMP, PILOT or DROPSOND data, set the combination
! of parts for the retrieval.
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (CSUBT(1:4) == 'TEMP' .OR. CSUBT(1:5) == 'PILOT' .OR. &
      CSUBT(1:8) == 'DROPSOND') THEN
IFLABEL2: &
    IF (SONDEPARTS == 'A') THEN
      KEY_PARTS = 'STANDARD PARTA'
    ELSE IF (SONDEPARTS == 'B') THEN
      KEY_PARTS = 'SIGNIFICANT PARTB'
    ELSE IF (SONDEPARTS == 'C') THEN
      KEY_PARTS = 'STANDARD PARTC'
    ELSE IF (SONDEPARTS == 'D') THEN
      KEY_PARTS = 'SIGNIFICANT PARTD'
    ELSE IF (SONDEPARTS == 'AC') THEN
      KEY_PARTS = 'STANDARD'
    ELSE IF (SONDEPARTS == 'BD') THEN
      KEY_PARTS = 'SIGNIFICANT'
    ELSE IF (SONDEPARTS == 'AB') THEN
      KEY_PARTS = 'COMBINED PARTAB'
    ELSE IF (SONDEPARTS == 'CD') THEN
      KEY_PARTS = 'COMBINED PARTCD'
    ELSE
      KEY_PARTS = ' '
    END IF IFLABEL2
  END IF IFLABEL1

!-----------------------------------------------------------------------
! For in-line element retrieval, the element for 'report text' has
! different names for different subtypes. Put correct name into string
! REPORT_STRING.
!-----------------------------------------------------------------------

IF (CSUBT(1:6) == 'METARS') THEN
  REPORT_STRING = 'MTR_RPT_TXT'
ELSE IF (CSUBT(1:4) == 'TAFS' .OR. &
        CSUBT(1:5) == 'ATAFS') THEN
  REPORT_STRING = 'TAF_RPT_TXT'
ELSE
  REPORT_STRING = 'RPRT_TEXT'
END IF

!-----------------------------------------------------------------------
! Search the string of user-selected stations for the string ' ALL '.
! If found, user wants all platforms so set string PLATFORM = ' ', else
! put user-selected platforms into string PLATFORM.
!-----------------------------------------------------------------------

IF (INDEX(STNS,' ALL ') == 0) THEN
  PLATFORM = 'PLATFORM ' // STNS
ELSE
  PLATFORM(:) = ' '
END IF

!-----------------------------------------------------------------------
! Construct the elements string depending on subtype. Retrieve the
! the report text, time of receipt, identifier and data time if
! possible. Note: for TEMP, PILOT, DROPSOND, time of receipt is not
! retrieved - as there are different times of receipt for each part.
! these are retrieved from the report text header later in the UPRAIR
! format routine.
!-----------------------------------------------------------------------

IFLABEL3: &
IF (CSUBT(1:4) == 'TEMP' .OR. CSUBT(1:5) == 'PILOT' .OR. &
    CSUBT(1:8) == 'DROPSOND') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' WMO_BLCK_NMBR ' // &
             'WMO_STTN_NMBR CALL_SIGN DAY HOUR MINT ' //   &
             'LTTD LNGD RCPT_DAY_PARTA RCPT_HOUR_PARTA ' // &
             'RCPT_MINT_PARTA RCPT_DAY_PARTC ' //          &
             'RCPT_HOUR_PARTC RCPT_MINT_PARTC ' //         &
             'RCPT_DAY_PARTB RCPT_HOUR_PARTB ' //          &
             'RCPT_MINT_PARTB RCPT_DAY_PARTD ' //          &
             'RCPT_HOUR_PARTD RCPT_MINT_PARTD '

ELSE IF (CSUBT(1:6) == 'LNDSYN' .OR.CSUBT(1:5) == 'ESAWS') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT WMO_BLCK_NMBR ' // &
             'WMO_STTN_NMBR DAY HOUR MINT '

ELSE IF (CSUBT(1:6) == 'SHPSYN' .OR. CSUBT(1:6) == 'AIREPS') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT CALL_SIGN DAY HOUR MINT '

ELSE IF (CSUBT(1:3) == 'NCM' .OR. CSUBT(1:4) == 'SREW') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT WMO_STTN_INDX_NMBR ' // &
             'DAY HOUR '

ELSE IF (CSUBT(1:6) == 'SAMOSX' .OR. CSUBT(1:4) == 'TBUS' .OR.&
        CSUBT(1:8) == 'HEALTHRR') THEN
  ELEMENTS = ' '

ELSE IF (CSUBT(1:5) == 'BATHY' .OR. CSUBT(1:5) == 'TESAC') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT CALL_SIGN BUOY_IDNY DAY ' // &
             'HOUR MINT LTTD LNGD '

ELSE IF (CSUBT(1:6) == 'METARS' .OR. &
        CSUBT(1:4) == 'TAFS' .OR.   &
        CSUBT(1:5) == 'ATAFS') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCT_DAY ' // &
             'RCT_HR RCT_MNT ICAO_ID DAY HR MNT '

ELSE IF (CSUBT(1:7) == 'TROPADV') THEN
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT '

ELSE IF (CSUBT(1:6) == 'AMDARS') THEN
  ELEMENTS = ' ELEMENTS COLTN_CNTR_CODE RGSRN_NMBR CALL_SIGN ' // &
             'NVGTN_SYTM DATA_RELY_SYTM YEAR MNTH DAY HOUR ' // &
             'MINT RCPT_YEAR RCPT_MNTH RCPT_DAY RCPT_HOUR ' // &
             'RCPT_MINT CRDT_ORGL_SPFCN LTTD LNGD '// &
             'FLGT_PHAS ALTD WIND_INST_TYPE LEVL_WIND_DRCTN ' // &
             'LEVL_WIND_SPED VRTL_GUST_VLCY VRTL_GUST_ACLTN ' // &
             'TRBC_DEGR TMPR_OBSVN_PRCSN LEVL_AIR_TMPR ' // &
             'LEVL_DEW_PONT_TMPR LEVL_RLTV_HUMDY LEVL_PESR ' // &
             'ARCT_ROLL_ANGL ICE_DEGR '

ELSE IF (CSUBT(1:6) == 'DRIFTR') THEN
    ELEMENTS = ' ELEMENTS BUOY_IDNY RCPT_YEAR RCPT_MNTH ' // &
    'RCPT_DAY RCPT_HOUR RCPT_MINT YEAR MNTH DAY HOUR MINT ' // &
    'PSTN_ACRY LTTD LNGD HOUR_SNCE_LAST_PSTN BUOY_SPED ' // &
    'BUOY_DRCTN ALTV_LTTD ALTV_LNGD SRFC_WIND_DRCTN ' // &
    'SRFC_WIND_SPED SRFC_AIR_TMPR STTN_PESR ' // &
    'Q3HOUR_PESR_TNDY Q3HOUR_STTN_LEVL_PESR_DFFRC ' // &
    'SEA_SRFC_TMPR WIND_WAVE_PERD_1 WIND_WAVE_HGHT_1 ' // &
    'WIND_WAVE_PERD_2 WIND_WAVE_HGHT_2 DROG_IDNY ' // &
    'DROG_CABL_LNGH SALNY_DPTH_INST ' // &
    '(DPTH_OF_TMPR_SALNY_MESRT LEVL_SEA_TMPR ' // &
    'LEVL_SEA_SALNY)*15 RMVL_SHIP_MOTN_MTHD ' // &
    'DRTN_TIME_CRNT_MESRT (DPTH_OF_CRNT_MESRT ' // &
    'LEVL_CRNT_DRCTN LEVL_CRNT_SPED)*15 ' // &
    'QLTY_CNTL_GROP QLTY_OF_PSTN ENGRG_INFMN_1 ' // &
    'ENGRG_INFMN_2 ENGRG_INFMN_3 COLTN_CNTR '

ELSE IF (CSUBT(1:7) == 'TRIAXYS') THEN
    ELEMENTS = ' ELEMENTS PLTM_SERL_NMBR ' // &
    'YEAR MNTH DAY HOUR MINT LTTD LNGD ' // &
    'MSL_PESR SRFC_AIR_TMPR SRFC_DEW_PONT_TMPR ' // &
    'SRFC_RLTV_HUMDY SRFC_WIND_DRCTN SRFC_WIND_SPED ' // &
    'MXMM_SRFC_GUST SEA_SRFC_TMPR ' // &
    'SGNT_WAVE_HGHT MXMM_WAVE_HGHT MEAN_WAVE_PERD ' // &
    'PEAK_WAVE_PERD DRCTN_PEAK_WAVE SPRD_PEAK_WAVE ' // &
    'WMO_REGN_NMBR WMO_REGN_SUB_AREA BUOY_IDNY '

!       ELSE IF (CSUBT(1:6) == 'METARS') THEN
!         ELEMENTS = ' ELEMENTS ICAO_ID LAT LON YR MON DAY HR ' //
!    &    'MNT RCT_YR RCT_MON RCT_DAY RCT_HR RCT_MNT CLTN_CNTR ' //
!    &    'BLTN_ID AMND_NUM COR_NUM STN_RPT_TYPE WND_DIR ' //
!    &    'WND_SPD MAX_GUST VRBL_WND_MAX_DIR VRBL_WND_MIN_DIR ' //
!    &    'MIN_VSBY_DIR MIN_VSBY MAX_VSBY_DIR MAX_VSBY ' //
!    &    'GNRL_WX_ID (VSBY_RNWY_NUM VSBY_PLL_RNWY_ID ' //
!    &    'RNWY_VSBY_TDCY_ID MIN_RNWY_VSBY_QLFY_ID ' //
!    &    'MIN_RNWY_VSBY MAX_RNWY_VSBY_QLFY_ID MAX_RNWY_VSBY)*2 ' //
!    &    '(SIG_WX_INSY_ID SIG_WX_DSC_ID SIG_WX_PHNM_ID)*3 ' //
!    &    '(RCNT_WX_DSC_ID RCNT_WX_PHNM_ID)*3 PRST_WX_ID ' //
!    &    '(CLD_AMT_ID CLD_TYPE_ID CLD_BASE_HT)*3 ' //
!    &    '(DEEP_CONV_CLD_AMT_ID DEEP_CONV_CLD_TYPE_ID ' //
!    &    'DEEP_CONV_CLD_BASE_HT)*3 VERT_VSBY AIR_TEMP DEWPT ' //
!    &    'ALTM_PRES (WND_SHR_RNWY_NUM WND_SHR_PLL_RNWY_ID ' //
!    &    'RNWY_USG_ID RNWY_WND_SHR_ID)*2 RNWY_ST_TXT MTR_RPT_TXT '

!       ELSE IF (CSUBT(1:4) == 'TAFS') THEN
!         ELEMENTS = ' ELEMENTS ICAO_ID LAT LON YR MON DAY HR ' //
!    &    'MNT RCT_YR RCT_MON RCT_DAY RCT_HR RCT_MNT CLTN_CNTR ' //
!    &    'BLTN_ID AMND_NUM COR_NUM TAF_LEN_TYPE FCST_BGN_DAY ' //
!    &    'FCST_BGN_HR FCST_END_HR WND_DIR WND_SPD MAX_GUST ' //
!    &    'GNRL_WX_ID (SIG_WX_INSY_ID SIG_WX_DSC_ID ' //
!    &    'SIG_WX_PHNM_ID)*3 (CLD_AMT_ID CLD_TYPE_ID ' //
!    &    'CLD_BASE_HT)*3 DEEP_CONV_CLD_AMT_ID ' //
!    &    'DEEP_CONV_CLD_TYPE_ID DEEP_CONV_CLD_BASE_HT ' //
!    &    'MIN_VSBY VERT_VSBY FCST_AIR_TEMP_HR AIR_TEMP ICG_LVL ' //
!    &    'ICG_LAY_THKN ARFM_ICG_ID TBLC_FREQ_ID TBLC_BASE_HT ' //
!    &    'TBLC_TOP_HT TBLC_AMT_ID PRST_WX_ID ' //
!    &    '(FCST_CHG_ID FCST_CHG_BGN_HR FCST_CHG_END_HR ' //
!    &    'FCST_PRBL CHG_WND_DIR CHG_WND_SPD CHG_MAX_GUST ' //
!    &    'CHG_GNRL_WX_ID CHG_MIN_VSBY (CHG_SIG_WX_INSY_ID ' //
!    &    'CHG_SIG_WX_DSC_ID CHG_SIG_WX_PHNM_ID)*3 ' //
!    &    '(CHG_CLD_AMT_ID CHG_CLD_TYPE_ID CHG_CLD_BASE_HT)*3 ' //
!    &    'CHG_DEEP_CONV_CLD_AMT_ID CHG_DEEP_CONV_CLD_TYPE_ID ' //
!    &    'CHG_DEEP_CONV_CLD_BASE_HT)*5 '

ELSE IF (CSUBT(1:8) == 'BUOYPROF') THEN
    ELEMENTS = ' ELEMENTS BUOY_IDNY YEAR MNTH DAY HOUR ' // &
    'MINT COLTN_CNTR PSTN_ACRY LTTD LNGD HOUR_SNCE_LAST_PSTN ' // &
    'RCPT_YEAR RCPT_MNTH RCPT_DAY RCPT_HOUR RCPT_MINT ' // &
    'BUOY_SPED BUOY_DRCTN SRFC_WIND_SPED_RCRDG_IDNY ' // &
    'SRFC_WIND_DRCTN SRFC_WIND_SPED SRFC_AIR_TMPR ' // &
    'SRFC_DEW_PONT_TMPR SRFC_RLTV_HUMDY STTN_PESR ' // &
    'MSL_PESR Q3HOUR_PESR_TNDY Q3HOUR_STTN_LEVL_PESR_DFFRC ' // &
    'SEA_SRFC_TMPR WIND_WAVE_PERD1 WIND_WAVE_HGHT1 ' // &
    'WIND_WAVE_PERD2 WIND_WAVE_HGHT2 DROG_IDNY DROG_CABL_LNGH ' // &
    'SALNY_DPTH_INST TMPR_SALNY_LEVL_RPLTN_CONT ' // &
    '(DPTH_OF_TMPR_SALNY_MESRT LEVL_SEA_TMPR ' // &
    'LEVL_SEA_SALNY)*50 RMVL_SHIP_MOTN_MTHD ' // &
    'DRTN_TIME_CRNT_MESRT CRNT_LEVL_RPLTN_CONT ' // &
    '(DPTH_OF_CRNT_MESRT LEVL_CRNT_DRCTN LEVL_CRNT_SPED)*50 ' // &
    'QLTY_CNTL_GROP QLTY_OF_PSTN QLTY_OF_PSTN_CLSS_SCTN1 ' // &
    'QLTY_OF_PSTN_CLSS_SCTN4 ENGRG_INFMN1 ENGRG_INFMN2 ' // &
    'ENGRG_INFMN3 ALTV_LTTD ALTV_LNGD YEAR_OF_LAST_PSTN ' // &
    'MNTH_OF_LAST_PSTN DAY_OF_LAST_PSTN HOUR_OF_LAST_PSTN ' // &
    'MINT_OF_LAST_PSTN '

ELSE IF (CSUBT(1:6) == 'ATDNET') THEN
    ELEMENTS = ' ELEMENTS DAY MNTH YEAR HOUR MINT SCND ' // &
    'LTTD LNGD ERRR_ELPS_X ERRR_ELPS_Y ' // &
    'ANGL_AXIS_X MDFD_RSDL NMBR_SNSRS '

ELSE
  ELEMENTS = ' ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' // &
             'RCPT_HOUR RCPT_MINT '
END IF IFLABEL3

!-----------------------------------------------------------------------
! retrieve the latest ob, SUBMIT = 'RETRIEVE LATEST REPORT' or a data
! period, SUBMIT = 'SUBMIT METDB RETRIEVAL'
!-----------------------------------------------------------------------

DATIME = (SUBMIT(1:22) == 'SUBMIT METDB RETRIEVAL')

IFLABEL4: &
IF (DATIME) THEN

!-----------------------------------------------------------------------
! Construct MDB request string if time period wanted
!-----------------------------------------------------------------------

  CREQ  = VERSIONSTR // ' ' // PLATFORM // ' ' // &
          'START TIME ' // STARTDATE // '/' // STARTHOUR // &
                           STARTMINUTE // 'Z ' // &
          'END TIME '   // ENDDATE // '/' // ENDHOUR // &
                           ENDMINUTE // 'Z ' // &
          INCREMENT  // ' ' // KEY_PARTS // ' ' // AREA // ' ' // &
          ELEMENTS

!-----------------------------------------------------------------------
! Construct MDB request string if LATEST report wanted
!-----------------------------------------------------------------------

ELSE
  CREQ = VERSIONSTR // ' ' // 'LATEST ' // PLATFORM // ' ' // &
         KEY_PARTS // ' ' // AREA // ' ' // ELEMENTS
END IF IFLABEL4

!-----------------------------------------------------------------------
! MetDB reuest string is now complete. Remove the additional spaces from
! it using DELSPCE and write it to the output dataset using routine
! WEBFORM.
!-----------------------------------------------------------------------

WRITE(6,*)'<pre>'
IN_TEXT(:) = ' '
CALL DELSPCE(CREQ2,LEN_CREQ2,CREQ)
CALL WEBFORM(CREQ2,1,LEN_CREQ2,1,79,IN_TEXT)
WRITE(6,*)'<hr>'

!-----------------------------------------------------------------------
! While there is MetDB data to read (ReadMoreData = .TRUE.), call the
! MetDB.
!-----------------------------------------------------------------------

ReadMoreData = .TRUE.
TotalObs     = 0

IF (CSUBT(1:6) == 'AMDARS' .OR. CSUBT(1:6) == 'DRIFTR' .OR. &
    CSUBT(1:7) == 'TRIAXYS' .OR. CSUBT(1:6) == 'ATDNET') THEN
  CALL WEBDIS(CSUBT,CREQ2,IMaxObs,TotalObs)
  ReadMoreData = .FALSE.
END IF

DOLABEL1: &
DO WHILE (ReadMoreData)

  CALL MDB(CSUBT,CREQ2,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

!-----------------------------------------------------------------------
! If data is returned from the MetDB (ISTAT=0 or ISTAT=4), loop over
! the obs returned and process them.
!-----------------------------------------------------------------------

IFLABEL5: &
  IF (ISTAT <= 4) THEN

DOLABEL2: &
    DO I=1,NOBS

      TotalObs = TotalObs + 1

!-----------------------------------------------------------------------
! Report text length and therefore report text end is ARRAY(I,1)
!-----------------------------------------------------------------------

      RPRTEND   = NINT(ARRAY(I,1))

!-----------------------------------------------------------------------
! Put the time of receipt in the form dd/hhmm into string TOR.
!-----------------------------------------------------------------------

      WRITE(TOR,'(A4,I2.2,A1,2I2.2)')'TOR=',NINT(ARRAY(I,2)),'/', &
      NINT(ARRAY(I,3)),NINT(ARRAY(I,4))

!-----------------------------------------------------------------------
! For each subtype, put identifier and date/time information into
! the string in_text and call the appropriate text formatting routine.
!-----------------------------------------------------------------------

IFLABEL6: &
      IF (CSUBT(1:4) == 'TEMP'      .OR. &
          CSUBT(1:5) == 'PILOT'     .OR. &
          CSUBT(1:8) == 'DROPSOND') THEN
        WRITE(IN_TEXT(1:8),'(3I2.2,''Z '')')NINT(ARRAY(I,5)), &
        NINT(ARRAY(I,6)),NINT(ARRAY(I,7))
        IF (ARRAY(I,2) < -9000000.0) THEN
          IN_TEXT(9:17)=CSTR(I)(1:9)
          IN_TEXT(18:18)=' '
        ELSE
          WRITE(IN_TEXT(9:13),'(I2.2,I3.3)')NINT(ARRAY(I,2)), &
          NINT(ARRAY(I,3))
          IN_TEXT(14:18)=' '
        END IF

        DO J=1,4
          TOR_UPR(J)='TOR=**/****'
          IF (ARRAY(I,10+(J-1)*3) >= 0 .AND.                &
              ARRAY(I,11+(J-1)*3) >= 0 .AND.                &
              ARRAY(I,12+(J-1)*3) >= 0) THEN
            WRITE(TOR_UPR(J),'(A4,I2.2,A1,2I2.2)')'TOR=',   &
            NINT(ARRAY(I,10+(J-1)*3)),'/',                  &
            NINT(ARRAY(I,11+(J-1)*3)),                      &
            NINT(ARRAY(I,12+(J-1)*3))
          END IF
        END DO

        CALL WEBUPR(CREP(I),RPRTEND,WRITEHEADER,IN_TEXT, &
        TOR_UPR)

      ELSE IF (CSUBT(1:6) == 'LNDSYN') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',I2.2,I3.3,'' '')') &
        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),NINT(ARRAY(I,9)), &
        NINT(ARRAY(I,5)),NINT(ARRAY(I,6))
        CALL WEBLND(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2, &
        IN_TEXT)

      ELSE IF (CSUBT(1:6) == 'SHPSYN') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',A9,'' '')') &
        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,8)), &
        CSTR(I)(1:9)
        CALL WEBSHP(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2, &
        IN_TEXT)

      ELSE IF (CSUBT(1:6) == 'AIREPS') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',A8,'' '')') &
        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,8)), &
        CSTR(I)(1:8)
        CALL WEBAIR(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

      ELSE IF (CSUBT(1:3) == 'NCM') THEN
        WRITE(IN_TEXT,'(2I2.2,''00Z '',I5.5,'' '')') &
        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,5))
        CALL WEBNCM(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2, &
        IN_TEXT)

      ELSE IF (CSUBT(1:5) == 'ESAWS') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',I2.2,I3.3,'' '')') &
        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),NINT(ARRAY(I,9)), &
        NINT(ARRAY(I,5)),NINT(ARRAY(I,6))
        CALL WEBESAW(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

      ELSE IF (CSUBT(1:4) == 'SREW') THEN
        WRITE(IN_TEXT,'(2I2.2,''00Z '',I5.5,'' '')') &
        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,5))
        CALL WEBSREW(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

      ELSE IF (CSUBT(1:6) == 'SAMOSX') THEN
        CALL WEBSAMOS(CREP(I),RPRTEND,WRITEHEADER)

      ELSE IF (CSUBT(1:5) == 'BATHY' .OR. &
              CSUBT(1:5) == 'TESAC') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',A9,'' '')')NINT(ARRAY(I,7)), &
        NINT(ARRAY(I,8)),NINT(ARRAY(I,9)),CSTR(I)(1:9)
        IF (CSTR(I)(1:9) == ' ' .AND. &
           ARRAY(I,6) > -9000000.0) THEN
          WRITE(IN_TEXT(9:13),'(I5.5)')NINT(ARRAY(I,6))
        END IF
        CALL WEBSSEA(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

      ELSE IF (CSUBT(1:6) == 'METARS' .OR. &
              CSUBT(1:4) == 'TAFS' .OR. &
              CSUBT(1:5) == 'ATAFS') THEN
        WRITE(IN_TEXT,'(3I2.2,''Z '',A4,'' '')')NINT(ARRAY(I,6)), &
        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),CSTR(I)(1:4)
        CALL WEBTFMET(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

      ELSE IF (CSUBT(1:7) == 'TROPADV') THEN
        CALL WEBTADV(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2)

      ELSE IF (CSUBT(1:4) == 'TBUS' .OR. &
              CSUBT(1:8) == 'HEALTHRR') THEN
        CALL WEBTBUS(CREP(I),RPRTEND,WRITEHEADER,CREP2)

      ELSE IF (CSUBT(1:8) == 'BUOYPROF') THEN
        WRITE(6,*)'</pre>'
        CALL WEBBUOYP(ARRAY,IOBS,IELS,I)
        WRITE(6,*)'<pre>'

      ELSE
        WRITE(6,*)'WEBRET: No format routine for subtype ',CSUBT
        TOTALOBS=0
        EXIT DOLABEL1              ! Leave DO loops
      END IF IFLABEL6

    END DO DOLABEL2 !- i=1,nobs

  END IF IFLABEL5 !- istat <= 4

!-----------------------------------------------------------------------
! Set ReadMoreData to TRUE if there is more to read from MDB
!-----------------------------------------------------------------------

  ReadMoreData = (ISTAT == 4 .AND. TotalObs < IMaxObs)

END DO DOLABEL1 !- do while ReadMoreData

!-----------------------------------------------------------------------
! Finally put the total number of obs retrieved into the dataset
! TEMPFILE to be picked up in the perl script.
!-----------------------------------------------------------------------

WRITE(6,*)'</pre>'
WRITE(CHAROBTOT(1:4),'(I4.4)')TOTALOBS

CALL GETENV('TEMPFILE',TEMPFILE)
OPEN(15,FILE=TEMPFILE,FORM="FORMATTED")
WRITE(15,'(A)')CHAROBTOT
CLOSE(15)

STOP
END PROGRAM WEBRET
