      PROGRAM WEBRET

!-----------------------------------------------------------------------
!
! routine       : WEBRET
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters,
!               : enddo statements.
!
! purpose       : To call the MetDB to extract raw reports and output
!               : them to a dataset for web display.
!
! description   : The Web MetDB extraction page submits a QUERY to a
!               : REXX CGI script on Open Edition  1 . That calls  1
!               : REXX which runs the load module built from this
!               : source. This code calls REXVARR to 'F'etch or 'S'et
!               : variables from the REXX. It 'fetches' MetDB retrieval
!               : request details entered by the user from the REXX and
!               : 'sets' the number of observations satisfying the web
!               :  request.
!
! data types    : Those where raw reports are stored in the MetDB
!
! calls         : DELSPCE  : to strip extra spaces from request     !1.9
!               : MDB      : to get description of BUFR code or flag
!               : REXXVAR  : get BUFR displacements from message
!
!-----------------------------------------------------------------------
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         21/09/2006 10:52:18    Stan Kellett    
! $
! Revision 1.13  2001/03/22 10:47:40  usmdb
! INCREMENT check changed to allow an INCREMENT of 24 - S.Cox
!
! Revision 1.12  2001/01/18 09:49:58  usmdb
! Added pre-processor statements to allow code to run on either
! IBM or HP. On HP version, environment variables are read using
! MetDB routine metdb_getenv and the total number of obs is
! written to a file. Also made increment section more robust as
! this didn't work on HP. - S.Cox
!
! Revision 1.11  2000/10/04  15:46:21  15:46:21  usmdb (Generic MDB account)
! Retrieve Sonde TOR for each part from MDB and pass
! to WEBUPR for display - S.Cox
! 
! Revision 1.10  2000/09/06  14:04:05  14:04:05  usmdb (Generic MDB account)
! 18 Sept 2000   S.Cox
! Added BUOYPROF subtype retrieval. New subroutine
! WEBBUOYP called.
!
! Revision 1.9  2000/08/09  15:25:34  15:25:34  usmdb (Generic MDB account)
! 21 August 2000
! Remove user request time QC check. Leave this
! for improved MDB routine VALDAT to do instead - S.Cox
!
! Revision 1.8  2000/04/07  08:52:02  08:52:02  usmdb (Generic MDB account)
! Added retrieval of minutes. New variables fetched by
! REXXVAR. Added corresponding date-time validation - S.Cox
!
! Revision 1.7  2000/03/10  09:24:15  09:24:15  usmdb (Generic MDB account)
! Operational 20-03-2000
! Pass CREP2 to WEBTBUS and WEBTADV. It will be used
! as temporary string storage.
!
! Revision 1.6  99/09/09  10:24:59  10:24:59  usmdb (Generic MDB account)
! Change date: 20-09-1999 - S.Cox
! Addition of HEALTHRR subtype. Increase max size of report
! text to 8500 for long TROPADV reports. Correct setting
! of END TIME if > current time. Now set to nearest minute.
!
! Revision 1.5  99/06/11  13:10:51  13:10:51  usmdb (Generic MDB account)
! Addition of ATAFS - S.Cox
! v(G)=51, ev(G)=18
!
! Revision 1.4  99/03/11  15:07:08  15:07:08  usmdb (Generic MDB account)
! 15th March 1999 - S.Cox
! Addition of code for running a test case on IBM
!
! Revision 1.3  99/01/14  14:34:04  14:34:04  usmdb (Generic MDB account)
! Correct retrieval of sonde parts
!
! Revision 1.2  98/08/10  08:56:38  08:56:38  usmdb (Generic MDB account)
! Addition of dynamic common & AMDAR + DRIFTR retrieval.
!
! Revision 1.1  1998/07/29 08:06:32  usmdb
! Initial revision
!
! 10-08-1998    : Addition of dynamic common, AMDAR, DRIFTR retrieval.
!
! 06-08-1998    : Addition of dynamic common to keep load module size
!               : down and addition of code to cater for AMDAR web
!               : retrieval - S.Cox
!
! 29-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Parameter statements
!-----------------------------------------------------------------------

      INTEGER IOBS                   !- number of obs for MDB call
      INTEGER IELS                   !- number of elements for MDB call

      PARAMETER    (IOBS   = 25)     !- set IOBS                   !1.10
      PARAMETER    (IELS   = 354)    !- set IELS                   !1.10

!-----------------------------------------------------------------------
! Declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER        EHOURS          !- user end hours
      INTEGER        END_TIME        !- end time (century mins)     !1.8
      INTEGER        I               !- general loop counter
      INTEGER        IMAXOBS         !- user no. of max obs to retrieve
      INTEGER        IRC             !- return code from REXXVAR
      INTEGER        ISTAT           !- return code from MDB
      INTEGER        J               !- general loop counter       !1.11
      INTEGER        LEN_CREQ2       !- length of DELSPCE req string
      INTEGER        NELEM           !- number of elements for MDB call
      INTEGER        NOBS            !- number of obs for MDB call
      INTEGER        RPRTEND         !- end of report text from MDB
      INTEGER        RPRTSTART       !- start of report text from MDB
      INTEGER        SHOURS          !- user start hours
      INTEGER        START_TIME      !- start time (century mins)   !1.8
      INTEGER        TotalObs        !- total obs retrieved from MDB

!-----------------------------------------------------------------------
! Declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

      REAL           ARRAY(IOBS,IELS)   !- array of values from MDB

!-----------------------------------------------------------------------
! Declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL        DATIME          !- TRUE if user wants a time series
      LOGICAL        ReadMoreData    !- TRUE if more MDB data to read
      LOGICAL        WRITEHEADER     !- TRUE if user wants MDB header

!-----------------------------------------------------------------------
! Declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*35   AREA            !- AREA request for MDB
      CHARACTER*6    AREA_N          !- user area request (top lat)
      CHARACTER*6    AREA_S          !- user area request (low lat)
      CHARACTER*6    AREA_W          !- user area request (left lon)
      CHARACTER*6    AREA_E          !- user area request (right lon)
      CHARACTER*4    CHAROBTOT       !- total obs retrieved from MDB
      CHARACTER*8500 CREP(IOBS)      !- MDB report text array       !1.6
      CHARACTER*8500 CREP2           !- MDB report text (expanded)  !1.6
      CHARACTER*1200 CREQ            !- users MDB request          !1.10
      CHARACTER*1200 CREQ2           !- MDB request (no blanks)    !1.10
      CHARACTER*9    CSTR(IOBS)      !- MDB element strings
      CHARACTER*8    CSUBT           !- MDB subtype
      CHARACTER*950  ELEMENTS        !- ELEMENTS request for MDB   !1.10
      CHARACTER*8    ENDDATE         !- user end date request
      CHARACTER*2    ENDHOUR         !- user end hour request       !1.8
      CHARACTER*2    ENDMINUTE       !- user end minute request     !1.8
      CHARACTER*132  HEAD            !- RCS info
      CHARACTER*20   IN_TEXT         !- text to preceed output report
      CHARACTER*18   KEY_PARTS       !- UPRAIR parts request for MDB
      CHARACTER*4    MAXOBS          !- users max obs request
      CHARACTER*15   REPORT_STRING   !- RPRT_TEXT request for MDB
      CHARACTER*8    STARTDATE       !- user start date request
      CHARACTER*2    STARTHOUR       !- user start hour request     !1.8
      CHARACTER*2    STARTMINUTE     !- user start minute request   !1.8
      CHARACTER*110  PLATFORM        !- PLATFORM request for MDB
      CHARACTER*100  STNS            !- users platform request
      CHARACTER*4    SONDEPARTS      !- users sonde parts request
      CHARACTER*30   SUBMIT          !- users submit type request
      CHARACTER*11   TOR             !- Time Of Receipt
      CHARACTER*11   TOR_UPR(4)      !- Time Of Receipt - sonde    !1.11
      CHARACTER*3    HEADER          !- users header request
      CHARACTER*3    VERSIONS        !- users version request
      CHARACTER*17   VERSIONSTR      !- VERSION request for MDB
      CHARACTER*2    INCHOUR         !- users increment request
      CHARACTER*12   INCREMENT       !- INCREMENT request for MDB
#if !defined (MVS)
      CHARACTER*80   TEMPFILE        !- temporary file for HP      !1.12
#endif

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

#if defined (MVS)
      CALL REXXVAR('F','SUBTYPE',7,CSUBT,8,'N',IRC)
      CALL REXXVAR('F','STNS',4,STNS,100,'N',IRC)
      CALL REXXVAR('F','SDATE',5,STARTDATE,8,'N',IRC)
      CALL REXXVAR('F','SHOUR',5,STARTHOUR,2,'N',IRC)               !1.8
      CALL REXXVAR('F','SMINUTE',7,STARTMINUTE,2,'N',IRC)           !1.8
      CALL REXXVAR('F','EDATE',5,ENDDATE,8,'N',IRC)
      CALL REXXVAR('F','EHOUR',5,ENDHOUR,2,'N',IRC)                 !1.8
      CALL REXXVAR('F','EMINUTE',7,ENDMINUTE,2,'N',IRC)             !1.8
      CALL REXXVAR('F','SUBMIT',6,SUBMIT,30,'N',IRC)
      CALL REXXVAR('F','MAXOBS',6,MAXOBS,4,'N',IRC)
      CALL REXXVAR('F','HEADER',6,HEADER,3,'N',IRC)
      CALL REXXVAR('F','VERSIONS',8,VERSIONS,3,'N',IRC)
      CALL REXXVAR('F','INCREMENT',9,INCHOUR,2,'N',IRC)
      CALL REXXVAR('F','SONDEPARTS',10,SONDEPARTS,4,'N',IRC)
      CALL REXXVAR('F','AREA_N',6,AREA_N,6,'N',IRC)
      CALL REXXVAR('F','AREA_S',6,AREA_S,6,'N',IRC)
      CALL REXXVAR('F','AREA_W',6,AREA_W,6,'N',IRC)
      CALL REXXVAR('F','AREA_E',6,AREA_E,6,'N',IRC)
#else
      CALL METDB_GETENV("SUBTYPE",CSUBT,IRC)                       !1.12
      CALL METDB_GETENV("STNS",STNS,IRC)                           !1.12
      CALL METDB_GETENV("SDATE",STARTDATE,IRC)                     !1.12
      CALL METDB_GETENV("SHOUR",STARTHOUR,IRC)                     !1.12
      CALL METDB_GETENV("SMINUTE",STARTMINUTE,IRC)                 !1.12
      CALL METDB_GETENV("EDATE",ENDDATE,IRC)                       !1.12
      CALL METDB_GETENV("EHOUR",ENDHOUR,IRC)                       !1.12
      CALL METDB_GETENV("EMINUTE",ENDMINUTE,IRC)                   !1.12
      CALL METDB_GETENV("SUBMIT",SUBMIT,IRC)                       !1.12
      CALL METDB_GETENV("MAXOBS",MAXOBS,IRC)                       !1.12
      CALL METDB_GETENV("HEADER",HEADER,IRC)                       !1.12
      CALL METDB_GETENV("VERSIONS",VERSIONS,IRC)                   !1.12
      CALL METDB_GETENV("INCREMENT",INCHOUR,IRC)                   !1.12
      CALL METDB_GETENV("SONDEPARTS",SONDEPARTS,IRC)               !1.12
      CALL METDB_GETENV("AREA_N",AREA_N,IRC)                       !1.12
      CALL METDB_GETENV("AREA_S",AREA_S,IRC)                       !1.12
      CALL METDB_GETENV("AREA_W",AREA_W,IRC)                       !1.12
      CALL METDB_GETENV("AREA_E",AREA_E,IRC)                       !1.12
#endif

!-----------------------------------------------------------------------
! To test a web retrieval on the IBM platform only (no CGI involved),
! comment out all the REXXVAR calls in WEBRET and uncomment the lines
! below. Change the lines to suit your test. The load module that this
! routine belongs to is SYS1.SDBLOAD(WEBRET).
!-----------------------------------------------------------------------

!      CSUBT       = 'AMDARS  '                                      !1.4
!      STNS        = '  '                                            !1.4
!      STARTDATE   = '20010116'                                      !1.4
!      STARTHOUR   = '00'                                            !1.8
!      STARTMINUTE = '00'                                            !1.8
!      ENDDATE     = '20010116'                                      !1.4
!      ENDHOUR     = '00'                                            !1.8
!      ENDMINUTE   = '59'                                            !1.8
!      SUBMIT      = 'SUBMIT METDB RETRIEVAL'                        !1.4
!      MAXOBS      = '0500'                                          !1.4
!      HEADER      = 'NO'                                            !1.4
!      VERSIONS    = ' '                                             !1.4
!      INCHOUR     = '00'                                            !1.4
!      SONDEPARTS  = ' '                                             !1.4
!      AREA_N      = ' '                                             !1.4
!      AREA_S      = ' '                                             !1.4
!      AREA_W      = ' '                                             !1.4
!      AREA_E      = ' '                                             !1.4

!TST  CSUBT       = 'LNDSYN  '                                      !1.4
!TST  STNS        = '  '                                            !1.4
!TST  STARTDATE   = '20000831'                                      !1.4
!TST  STARTHOUR   = '00'                                            !1.8
!TST  STARTMINUTE = '00'                                            !1.8
!TST  ENDDATE     = '20000831'                                      !1.4
!TST  ENDHOUR     = '00'                                            !1.8
!TST  ENDMINUTE   = '59'                                            !1.8
!TST  SUBMIT      = 'SUBMIT METDB RETRIEVAL'                        !1.4
!TST  MAXOBS      = '0500'                                          !1.4
!TST  HEADER      = 'NO'                                            !1.4
!TST  VERSIONS    = ' '                                             !1.4
!TST  INCHOUR     = '00'                                            !1.4
!TST  SONDEPARTS  = ' '                                             !1.4
!TST  AREA_N      = ' '                                             !1.4
!TST  AREA_S      = ' '                                             !1.4
!TST  AREA_W      = ' '                                             !1.4
!TST  AREA_E      = ' '                                             !1.4

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
      ENDDO

!-----------------------------------------------------------------------
! RCS revision information
!-----------------------------------------------------------------------

      HEAD = '
     &$Source: /net/home/h01/mdb_new/op/lib/web_source/RCS/webret.F,v $
     &'//'$Date: 21/09/2006 10:52:18$ $Revision: 1$'

!-----------------------------------------------------------------------
! Put the 'max obs to retrieve' (set by user) into variable IMAXOBS.
!-----------------------------------------------------------------------

      READ(MAXOBS(1:4),'(I4)')IMAXOBS

!-----------------------------------------------------------------------
! If the user wants a MetDB header to be output, WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

      WRITEHEADER = (INDEX(HEADER,'YES').GT.0)

!-----------------------------------------------------------------------
! Set VERSIONSTR for inclusion in request string CREQ depending on
! user choice.
!-----------------------------------------------------------------------

      IF (INDEX(VERSIONS,'YES').GT.0) THEN
        VERSIONSTR(1:17) = 'VERSION ALL      '
      ELSE
        VERSIONSTR(1:17) = 'VERSION PREFERRED'
      ENDIF

!-----------------------------------------------------------------------
! Check for INCREMENT 00 (Passed if no INCREMENT wanted by user for a
! subtype that allows the INCREMENT keyword). If '00' set INCREMENT
! string to null, otherwise, construct INCREMENT string.
!-----------------------------------------------------------------------

        IF (INCHOUR(1:2).LT.'01' .OR. INCHOUR(1:2).GT.'24') THEN   !1.13
          INCREMENT(:) = ' '
        ELSE
          INCREMENT = 'INCREMENT '//INCHOUR
        ENDIF

!-----------------------------------------------------------------------
! Produce an AREA request if an AREA is selected.
!-----------------------------------------------------------------------

        IF (AREA_N.NE.' ' .AND. AREA_S.NE.' ' .AND.
     &      AREA_W.NE.' ' .AND. AREA_E.NE.' ') THEN
          AREA = 'AREA ' // AREA_N // ' ' // AREA_S // ' ' //
     &                      AREA_W // ' ' // AREA_E
        ELSE
          AREA(:) = ' '
        ENDIF

!-----------------------------------------------------------------------
! If the user wants TEMP, PILOT or DROPSOND data, set the combination
! of parts for the retrieval.
!-----------------------------------------------------------------------

        IF (CSUBT(1:4).EQ.'TEMP' .OR. CSUBT(1:5).EQ.'PILOT' .OR.
     &      CSUBT(1:8).EQ.'DROPSOND') THEN
          IF (SONDEPARTS.EQ.'A') THEN
            KEY_PARTS = 'STANDARD PARTA'
          ELSEIF (SONDEPARTS.EQ.'B') THEN
            KEY_PARTS = 'SIGNIFICANT PARTB'
          ELSEIF (SONDEPARTS.EQ.'C') THEN
            KEY_PARTS = 'STANDARD PARTC'                            !1.3
          ELSEIF (SONDEPARTS.EQ.'D') THEN
            KEY_PARTS = 'SIGNIFICANT PARTD'                         !1.3
          ELSEIF (SONDEPARTS.EQ.'AC') THEN
            KEY_PARTS = 'STANDARD'
          ELSEIF (SONDEPARTS.EQ.'BD') THEN
            KEY_PARTS = 'SIGNIFICANT'
          ELSEIF (SONDEPARTS.EQ.'AB') THEN
            KEY_PARTS = 'COMBINED PARTAB'
          ELSEIF (SONDEPARTS.EQ.'CD') THEN
            KEY_PARTS = 'COMBINED PARTCD'
          ELSE
            KEY_PARTS = ' '
          ENDIF
        ENDIF

!-----------------------------------------------------------------------
! For in-line element retrieval, the element for 'report text' has
! different names for different subtypes. Put correct name into string
! REPORT_STRING.
!-----------------------------------------------------------------------

      IF (CSUBT(1:6).EQ.'METARS') THEN
        REPORT_STRING = 'MTR_RPT_TXT'
      ELSEIF (CSUBT(1:4).EQ.'TAFS' .OR.                             !1.5
     &        CSUBT(1:5).EQ.'ATAFS') THEN                           !1.5
        REPORT_STRING = 'TAF_RPT_TXT'
      ELSE
        REPORT_STRING = 'RPRT_TEXT'
      ENDIF

!-----------------------------------------------------------------------
! Search the string of user-selected stations for the string ' ALL '.
! If found, user wants all platforms so set string PLATFORM = ' ', else
! put user-selected platforms into string PLATFORM.
!-----------------------------------------------------------------------

      IF (INDEX(STNS,' ALL ').EQ.0) THEN
        PLATFORM = 'PLATFORM ' // STNS
      ELSE
        PLATFORM(:) = ' '
      ENDIF

!-----------------------------------------------------------------------
! Construct the elements string depending on subtype. Retrieve the
! the report text, time of receipt, identifier and data time if
! possible. Note: for TEMP, PILOT, DROPSOND, time of receipt is not
! retrieved - as there are different times of receipt for each part.
! these are retrieved from the report text header later in the UPRAIR
! format routine.
!-----------------------------------------------------------------------

      IF (CSUBT(1:4).EQ.'TEMP' .OR. CSUBT(1:5).EQ.'PILOT' .OR.
     &    CSUBT(1:8).EQ.'DROPSOND') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' WMO_BLCK_NMBR ' //
     &             'WMO_STTN_NMBR CALL_SIGN DAY HOUR MINT ' //     !1.11
     &             'LTTD LNGD RCPT_DAY_PARTA RCPT_HOUR_PARTA ' //  !1.11
     &             'RCPT_MINT_PARTA RCPT_DAY_PARTC ' //            !1.11
     &             'RCPT_HOUR_PARTC RCPT_MINT_PARTC ' //           !1.11
     &             'RCPT_DAY_PARTB RCPT_HOUR_PARTB ' //            !1.11
     &             'RCPT_MINT_PARTB RCPT_DAY_PARTD ' //            !1.11
     &             'RCPT_HOUR_PARTD RCPT_MINT_PARTD '              !1.11

      ELSEIF (CSUBT(1:6).EQ.'LNDSYN' .OR.CSUBT(1:5).EQ.'ESAWS') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT WMO_BLCK_NMBR ' //
     &             'WMO_STTN_NMBR DAY HOUR MINT '

      ELSEIF (CSUBT(1:6).EQ.'SHPSYN' .OR. CSUBT(1:6).EQ.'AIREPS') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT CALL_SIGN DAY HOUR MINT '

      ELSEIF (CSUBT(1:3).EQ.'NCM' .OR. CSUBT(1:4).EQ.'SREW') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT WMO_STTN_INDX_NMBR ' //
     &             'DAY HOUR '

      ELSEIF (CSUBT(1:6).EQ.'SAMOSX' .OR. CSUBT(1:4).EQ.'TBUS' .OR. !1.6
     &        CSUBT(1:8).EQ.'HEALTHRR') THEN                        !1.6
        ELEMENTS = ' '

      ELSEIF (CSUBT(1:5).EQ.'BATHY' .OR. CSUBT(1:5).EQ.'TESAC') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT CALL_SIGN BUOY_IDNY DAY ' //
     &             'HOUR MINT LTTD LNGD '

      ELSEIF (CSUBT(1:6).EQ.'METARS' .OR.                           !1.5
     &        CSUBT(1:4).EQ.'TAFS' .OR.                             !1.5
     &        CSUBT(1:5).EQ.'ATAFS') THEN                           !1.5
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCT_DAY ' //
     &             'RCT_HR RCT_MNT ICAO_ID DAY HR MNT '

      ELSEIF (CSUBT(1:7).EQ.'TROPADV') THEN
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT '

      ELSEIF (CSUBT(1:6).EQ.'AMDARS') THEN
        ELEMENTS = 'ELEMENTS COLTN_CNTR_CODE RGSRN_NMBR CALL_SIGN ' //
     &             'NVGTN_SYTM DATA_RELY_SYTM YEAR MNTH DAY HOUR ' //
     &             'MINT RCPT_YEAR RCPT_MNTH RCPT_DAY RCPT_HOUR ' //
     &             'RCPT_MINT CRDT_ORGL_SPFCN LTTD LNGD '//
     &             'FLGT_PHAS ALTD WIND_INST_TYPE LEVL_WIND_DRCTN ' //
     &             'LEVL_WIND_SPED VRTL_GUST_VLCY VRTL_GUST_ACLTN ' //
     &             'TRBC_DEGR TMPR_OBSVN_PRCSN LEVL_AIR_TMPR ' //
     &             'LEVL_DEW_PONT_TMPR LEVL_RLTV_HUMDY LEVL_PESR ' //
     &             'ARCT_ROLL_ANGL ICE_DEGR '

      ELSEIF (CSUBT(1:6).EQ.'DRIFTR') THEN
          ELEMENTS = 'ELEMENTS BUOY_IDNY RCPT_YEAR RCPT_MNTH ' //
     &    'RCPT_DAY RCPT_HOUR RCPT_MINT YEAR MNTH DAY HOUR MINT ' //
     &    'PSTN_ACRY LTTD LNGD HOUR_SNCE_LAST_PSTN BUOY_SPED ' //
     &    'BUOY_DRCTN ALTV_LTTD ALTV_LNGD SRFC_WIND_DRCTN ' //
     &    'SRFC_WIND_SPED SRFC_AIR_TMPR STTN_PESR ' //
     &    'Q3HOUR_PESR_TNDY Q3HOUR_STTN_LEVL_PESR_DFFRC ' //
     &    'SEA_SRFC_TMPR WIND_WAVE_PERD_1 WIND_WAVE_HGHT_1 ' //
     &    'WIND_WAVE_PERD_2 WIND_WAVE_HGHT_2 DROG_IDNY ' //
     &    'DROG_CABL_LNGH SALNY_DPTH_INST ' //
     &    '(DPTH_OF_TMPR_SALNY_MESRT LEVL_SEA_TMPR ' //
     &    'LEVL_SEA_SALNY)*15 RMVL_SHIP_MOTN_MTHD ' //
     &    'DRTN_TIME_CRNT_MESRT (DPTH_OF_CRNT_MESRT ' //
     &    'LEVL_CRNT_DRCTN LEVL_CRNT_SPED)*15 ' //
     &    'QLTY_CNTL_GROP QLTY_OF_PSTN ENGRG_INFMN_1 ' //
     &    'ENGRG_INFMN_2 ENGRG_INFMN_3 COLTN_CNTR '

!       ELSEIF (CSUBT(1:6).EQ.'METARS') THEN
!         ELEMENTS = 'ELEMENTS ICAO_ID LAT LON YR MON DAY HR ' //
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

!       ELSEIF (CSUBT(1:4).EQ.'TAFS') THEN
!         ELEMENTS = 'ELEMENTS ICAO_ID LAT LON YR MON DAY HR ' //
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

      ELSEIF (CSUBT(1:8).EQ.'BUOYPROF') THEN                       !1.10
          ELEMENTS = 'ELEMENTS BUOY_IDNY YEAR MNTH DAY HOUR ' //
     &    'MINT COLTN_CNTR PSTN_ACRY LTTD LNGD HOUR_SNCE_LAST_PSTN ' //
     &    'RCPT_YEAR RCPT_MNTH RCPT_DAY RCPT_HOUR RCPT_MINT ' //
     &    'BUOY_SPED BUOY_DRCTN SRFC_WIND_SPED_RCRDG_IDNY ' //
     &    'SRFC_WIND_DRCTN SRFC_WIND_SPED SRFC_AIR_TMPR ' //
     &    'SRFC_DEW_PONT_TMPR SRFC_RLTV_HUMDY STTN_PESR ' //
     &    'MSL_PESR Q3HOUR_PESR_TNDY Q3HOUR_STTN_LEVL_PESR_DFFRC ' //
     &    'SEA_SRFC_TMPR WIND_WAVE_PERD1 WIND_WAVE_HGHT1 ' //
     &    'WIND_WAVE_PERD2 WIND_WAVE_HGHT2 DROG_IDNY DROG_CABL_LNGH ' //
     &    'SALNY_DPTH_INST TMPR_SALNY_LEVL_RPLTN_CONT ' //
     &    '(DPTH_OF_TMPR_SALNY_MESRT LEVL_SEA_TMPR ' //
     &    'LEVL_SEA_SALNY)*50 RMVL_SHIP_MOTN_MTHD ' //
     &    'DRTN_TIME_CRNT_MESRT CRNT_LEVL_RPLTN_CONT ' //
     &    '(DPTH_OF_CRNT_MESRT LEVL_CRNT_DRCTN LEVL_CRNT_SPED)*50 ' //
     &    'QLTY_CNTL_GROP QLTY_OF_PSTN QLTY_OF_PSTN_CLSS_SCTN1 ' //
     &    'QLTY_OF_PSTN_CLSS_SCTN4 ENGRG_INFMN1 ENGRG_INFMN2 ' //
     &    'ENGRG_INFMN3 ALTV_LTTD ALTV_LNGD YEAR_OF_LAST_PSTN ' //
     &    'MNTH_OF_LAST_PSTN DAY_OF_LAST_PSTN HOUR_OF_LAST_PSTN ' //
     &    'MINT_OF_LAST_PSTN '

      ELSE
        ELEMENTS = 'ELEMENTS ' // REPORT_STRING // ' RCPT_DAY ' //
     &             'RCPT_HOUR RCPT_MINT '
      ENDIF

!-----------------------------------------------------------------------
! retrieve the latest ob, SUBMIT = 'RETRIEVE LATEST REPORT' or a data
! period, SUBMIT = 'SUBMIT METDB RETRIEVAL'
!-----------------------------------------------------------------------

      DATIME = (SUBMIT(1:22).EQ.'SUBMIT METDB RETRIEVAL')

      IF (DATIME) THEN

!-----------------------------------------------------------------------
! Construct MDB request string if time period wanted
!-----------------------------------------------------------------------

        CREQ  = VERSIONSTR // ' ' // PLATFORM // ' ' //
     &          'START TIME ' // STARTDATE // '/' // STARTHOUR //
     &                           STARTMINUTE // 'Z ' //
     &          'END TIME '   // ENDDATE // '/' // ENDHOUR //
     &                           ENDMINUTE // 'Z ' //
     &          INCREMENT  // ' ' // KEY_PARTS // ' ' // AREA // ' ' //
     &          ELEMENTS

!-----------------------------------------------------------------------
! Construct MDB request string if LATEST report wanted
!-----------------------------------------------------------------------

      ELSE
        CREQ = VERSIONSTR // ' ' // 'LATEST ' // PLATFORM // ' ' //
     &         KEY_PARTS // ' ' // AREA // ' ' // ELEMENTS
      ENDIF

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

      IF (CSUBT(1:6).EQ.'AMDARS' .OR. CSUBT(1:6).EQ.'DRIFTR') THEN
        CALL WEBDIS(CSUBT,CREQ2,IMaxObs,TotalObs)
        ReadMoreData = .FALSE.
      ENDIF

      DO WHILE (ReadMoreData)

        CALL MDB(CSUBT,CREQ2,ARRAY,NOBS,NELEM,ISTAT,CSTR,CREP)

!-----------------------------------------------------------------------
! If data is returned from the MetDB (ISTAT=0 or ISTAT=4), loop over
! the obs returned and process them.
!-----------------------------------------------------------------------

        IF (ISTAT.LE.4) THEN

          DO I=1,NOBS

            TotalObs = TotalObs + 1

!-----------------------------------------------------------------------
! Report text length and therefore report text end is ARRAY(I,1)
!-----------------------------------------------------------------------

            RPRTEND   = NINT(ARRAY(I,1))

!-----------------------------------------------------------------------
! Put the time of receipt in the form dd/hhmm into string TOR.
!-----------------------------------------------------------------------

            WRITE(TOR,'(A4,I2.2,A1,2I2.2)')'TOR=',NINT(ARRAY(I,2)),'/',
     &      NINT(ARRAY(I,3)),NINT(ARRAY(I,4))

!-----------------------------------------------------------------------
! For each subtype, put identifier and date/time information into
! the string in_text and call the appropriate text formatting routine.
!-----------------------------------------------------------------------

            IF (CSUBT(1:4).EQ.'TEMP'      .OR.
     &          CSUBT(1:5).EQ.'PILOT'     .OR.
     &          CSUBT(1:8).EQ.'DROPSOND') THEN
              WRITE(IN_TEXT(1:8),'(3I2.2,''Z '')')NINT(ARRAY(I,5)),
     &        NINT(ARRAY(I,6)),NINT(ARRAY(I,7))
              IF (ARRAY(I,2).LT.-9000000.0) THEN
                IN_TEXT(9:17)=CSTR(I)(1:9)
                IN_TEXT(18:18)=' '
              ELSE
                WRITE(IN_TEXT(9:13),'(I2.2,I3.3)')NINT(ARRAY(I,2)),
     &          NINT(ARRAY(I,3))
                IN_TEXT(14:18)=' '
              ENDIF

              DO J=1,4                                             !1.11
                TOR_UPR(J)='TOR=**/****'                           !1.11
                IF (ARRAY(I,10+(J-1)*3).GE.0 .AND.                 !1.11
     &              ARRAY(I,11+(J-1)*3).GE.0 .AND.                 !1.11
     &              ARRAY(I,12+(J-1)*3).GE.0) THEN                 !1.11
                  WRITE(TOR_UPR(J),'(A4,I2.2,A1,2I2.2)')'TOR=',    !1.11
     &            NINT(ARRAY(I,10+(J-1)*3)),'/',                   !1.11
     &            NINT(ARRAY(I,11+(J-1)*3)),                       !1.11
     &            NINT(ARRAY(I,12+(J-1)*3))                        !1.11
                ENDIF                                              !1.11
              ENDDO                                                !1.11

              CALL WEBUPR(CREP(I),RPRTEND,WRITEHEADER,IN_TEXT,     !1.11
     &        TOR_UPR)                                             !1.11

            ELSEIF (CSUBT(1:6).EQ.'LNDSYN') THEN
              WRITE(IN_TEXT,'(3I2.2,''Z '',I2.2,I3.3,'' '')')
     &        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),NINT(ARRAY(I,9)),
     &        NINT(ARRAY(I,5)),NINT(ARRAY(I,6))
              CALL WEBLND(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2,
     &        IN_TEXT)

            ELSEIF (CSUBT(1:6).EQ.'SHPSYN') THEN
              WRITE(IN_TEXT,'(3I2.2,''Z '',A9,'' '')')
     &        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),
     &        CSTR(I)(1:9)
              CALL WEBSHP(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2,
     &        IN_TEXT)

            ELSEIF (CSUBT(1:6).EQ.'AIREPS') THEN
              WRITE(IN_TEXT,'(3I2.2,''Z '',A8,'' '')')
     &        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),
     &        CSTR(I)(1:8)
              CALL WEBAIR(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

            ELSEIF (CSUBT(1:3).EQ.'NCM') THEN
              WRITE(IN_TEXT,'(2I2.2,''00Z '',I5.5,'' '')')
     &        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,5))
              CALL WEBNCM(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2,
     &        IN_TEXT)

            ELSEIF (CSUBT(1:5).EQ.'ESAWS') THEN
              WRITE(IN_TEXT,'(3I2.2,''Z '',I2.2,I3.3,'' '')')
     &        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),NINT(ARRAY(I,9)),
     &        NINT(ARRAY(I,5)),NINT(ARRAY(I,6))
              CALL WEBESAW(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

            ELSEIF (CSUBT(1:4).EQ.'SREW') THEN
              WRITE(IN_TEXT,'(2I2.2,''00Z '',I5.5,'' '')')
     &        NINT(ARRAY(I,6)),NINT(ARRAY(I,7)),NINT(ARRAY(I,5))
              CALL WEBSREW(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

            ELSEIF (CSUBT(1:6).EQ.'SAMOSX') THEN
              CALL WEBSAMOS(CREP(I),RPRTEND,WRITEHEADER)

            ELSEIF (CSUBT(1:5).EQ.'BATHY' .OR.
     &              CSUBT(1:5).EQ.'TESAC') THEN
              WRITE(IN_TEXT,'(3I2.2,''Z '',A9,'' '')')NINT(ARRAY(I,7)),
     &        NINT(ARRAY(I,8)),NINT(ARRAY(I,9)),CSTR(I)(1:9)
              IF (CSTR(I)(1:9).EQ.' ' .AND.
     &           ARRAY(I,6).GT.-9000000.0) THEN
                WRITE(IN_TEXT(9:13),'(I5.5)')NINT(ARRAY(I,6))
              ENDIF
              CALL WEBSSEA(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

            ELSEIF (CSUBT(1:6).EQ.'METARS' .OR.
     &              CSUBT(1:4).EQ.'TAFS' .OR.                       !1.5
     &              CSUBT(1:5).EQ.'ATAFS') THEN                     !1.5
              WRITE(IN_TEXT,'(3I2.2,''Z '',A4,'' '')')NINT(ARRAY(I,6)),
     &        NINT(ARRAY(I,7)),NINT(ARRAY(I,8)),CSTR(I)(1:4)
              CALL WEBTFMET(CREP(I),RPRTEND,WRITEHEADER,TOR,IN_TEXT)

            ELSEIF (CSUBT(1:7).EQ.'TROPADV') THEN
              CALL WEBTADV(CREP(I),RPRTEND,WRITEHEADER,TOR,CREP2)   !1.7

            ELSEIF (CSUBT(1:4).EQ.'TBUS' .OR.                       !1.6
     &              CSUBT(1:8).EQ.'HEALTHRR') THEN                  !1.6
              CALL WEBTBUS(CREP(I),RPRTEND,WRITEHEADER,CREP2)       !1.7

            ELSEIF (CSUBT(1:8).EQ.'BUOYPROF') THEN                 !1.10
              WRITE(6,*)'</pre>'                                   !1.10
              CALL WEBBUOYP(ARRAY,IOBS,IELS,I)                     !1.10
              WRITE(6,*)'<pre>'                                    !1.10

            ELSE
              WRITE(6,*)'WEBRET: No format routine for subtype ',CSUBT
              TOTALOBS=0
              GOTO 999
            ENDIF

          ENDDO  !- i=1,nobs

        ENDIF  !- istat.le.4

!-----------------------------------------------------------------------
! Set ReadMoreData to TRUE if there is more to read from MDB
!-----------------------------------------------------------------------

        ReadMoreData = (ISTAT.EQ.4 .AND. TotalObs.LT.IMaxObs)

      ENDDO  !- do while ReadMoreData

!-----------------------------------------------------------------------
! Finally put the total number of obs retrieved into the variable
! OBTOT to be picked up in the  1  REXX.
!-----------------------------------------------------------------------

 999  WRITE(6,*)'</pre>'
      WRITE(CHAROBTOT(1:4),'(I4.4)')TOTALOBS
#if defined (MVS)
      CALL REXXVAR('S','OBTOT',5,CHAROBTOT,4,'N',IRC)
#else
      CALL METDB_GETENV("TEMPFILE",TEMPFILE,IRC)
      OPEN(15,FILE=TEMPFILE,FORM="FORMATTED")
      WRITE(15,'(A)')CHAROBTOT
      CLOSE(15)
#endif

      STOP
      END
