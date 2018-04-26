SUBROUTINE MDB(CTYPE,REQUEST,RVALS,NOBS,NELEM,STATUS,CSTR,CREPT)

!-----------------------------------------------------------------------
!
! ROUTINE       : MDB
!
! DESCRIPTION   : The SQL style interface to validate the users input.
!               : It then allocates the correct datasets using the
!               : retrieval table and calls the appropriate retrieval
!               : routine.
!
! CALLED BY     : User's program (via shell MDB)
!
! CALLS         : ALSELM    - converts alias names to standard
!               : BUFRET    - for retrieval from new data format    !2.4
!               : BUSRET    - for retrieval of various subtypes       !P
!               : CHK4TW    - used for upper-air retrieval            !M
!               : CECK_AUTH - to check access permissions
!               : DATE31    - converts date to yearday
!               : DATIM     - gets clock time
!               : DELSPCE   - removes extra spaces
!               : EXPELM    - expands in-line element names
!               : GETREQ    - decodes request string
!               : GRIBRET   - for GRIB data retrieval              !1.26
!               : HRS2DT    - converts century hours to date/time
!               : ICERET    - for SEAICE and TROPADV retrieval        !J
!               : MAPELM    - gets mapping values for names
!               : MDBALC    - allocates datasets
!               : MDBRET    - for long bulletins & multiple sequences
!               : READLIST  - reads list of retrieval element names !2.4
!               : RTABLE    - gets info from retrieval table        !2.4
!               : SETDEF    - sets default request values
!               : SSMRET    - for SSM-I & other satellite retrieval   !O
!               : STMRET    - for STNMAS retrieval                    !E
!               : SYNRET    - for SYNOP (land) retrieval              !I
!               : TFMRET    - for TAF/METAR retrieval                 !E
!               : UPRRET    - for TEMP,PILOT,DROPSOND retrieval       !J
!               : VALREQ    - validates request parameters
!
! ARGUMENTS     :
!
! (1) CTYPE     : CHAR*(*)         : data subtype                    (i)
! (2) REQUEST   : CHAR*(*)         : keywords and values defining
!                                  : required data
!                                  : set to io as delspace changes   (io)
! (3) RVALS     : REAL(NOBS,NELEM) : data array                      (o)
! (4) NOBS      : INTEGER          : max dimension of rvals array    (i)
!                                  : number of observations          (o)
! (5) NELEM     : INTEGER          : number of elements each obs     (io)
! (6) STATUS    : INTEGER          : 0 new request                   (i)
!                                  : 0 request complete              (o)
!                                  : 4 continuation                  (i)
!                                  : 4 more data to come             (o)
!                                  : 8 no data                       (o)
!                                  : 16 fatal error                  (o)
! (7) CSTR      : CHAR*(*)(NOBS)   : array for string data           (o)
! (8) CREPT     : CHAR*(*)(NOBS)   : array for report text           (o)
!
! REVISION INFO :
!
! $Revision: 18$
! $Date: 16/07/2012 14:10:07$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdb.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  18   MetDB_Refresh 1.17        16/07/2012 14:10:07    Sheila Needham  Extra
!       print stmts for bug tracing
!  17   MetDB_Refresh 1.16        19/10/2011 10:40:02    Sheila Needham
!       Changes to batch logging and restricted access
!  16   MetDB_Refresh 1.15        15/07/2011 10:43:04    Sheila Needham  syntax
!        error
!  15   MetDB_Refresh 1.14        13/07/2011 12:27:55    Sheila Needham  Same
!       as previous but for logging too
!  14   MetDB_Refresh 1.13        13/07/2011 11:08:36    Richard Weedon
!       updated to exclude RPC generic logins from checks on UID
!  13   MetDB_Refresh 1.12        05/07/2011 10:50:52    Richard Weedon  latest
!        update
!  12   MetDB_Refresh 1.11        01/07/2011 16:39:09    Richard Weedon
!       Updated
!  11   MetDB_Refresh 1.10        29/06/2011 16:37:35    Richard Weedon
!       Updated for monitoring calls
!  10   MetDB_Refresh 1.9         05/04/2011 14:12:24    Alison Weir     Close
!       storage datasets with C routine.
!  9    MetDB_Refresh 1.8         29/11/2010 10:28:44    Sheila Needham
!       Correct inquire on DDICT dsn
!  8    MetDB_Refresh 1.7         24/11/2010 10:26:25    Brian Barwell
!       Replace statement "IPOS=I+10" accidentally deleted.
!  7    MetDB_Refresh 1.6         22/11/2010 14:18:45    Stan Kellett
!       argument REQUEST changed to intent inout
!       removed HEAD variable as not now used
!       add use inquire_mod
!       in call to gribret dimension passed in chaged to 1 dimensional as
!       RVALS(1:NOBS,1:1)
!  6    MetDB_Refresh 1.5         22/11/2010 10:23:40    Stan Kellett
!       removed HEAD and added USE inquire_mod
!  5    MetDB_Refresh 1.4         22/11/2010 10:15:30    Stan Kellett
!       argument NELEM changed to INOUT
!  4    MetDB_Refresh 1.3         18/11/2010 14:34:16    Sheila Needham
!       Corrected INQUIRE; USE zpdate_mod instead of date13_mod
!  3    MetDB_Refresh 1.2         18/11/2010 10:09:18    Stan Kellett    use
!       statements uncommented, removed old revision info
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
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

USE alselm_mod
USE bufret_mod
USE busret_mod
USE check_auth_mod
USE check_uid_mod
USE chk4tw_mod
USE zpdate_mod
USE delspce_mod
USE expelm_mod
USE getreq_mod
USE gribret_mod
USE hrs2dt_mod
USE iceret_mod
USE inquire_mod
USE mapelm_mod
USE mdbalc_mod
USE mdbret_mod
USE readlist_mod
USE rtable_mod
USE setdef_mod
USE ssmret_mod
USE stmret_mod
USE synret_mod
USE tfmret_mod
USE uprret_mod
USE valreq_mod

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)        ::  CTYPE
CHARACTER(*), INTENT(INOUT)        ::  REQUEST
INTEGER, INTENT(INOUT)          ::  NOBS
INTEGER, INTENT(INOUT)             ::  NELEM
REAL, INTENT(OUT)               ::  RVALS(NOBS,NELEM)
INTEGER, INTENT(INOUT)          ::  STATUS
CHARACTER(*), INTENT(OUT)       ::  CSTR(NOBS)
CHARACTER(*), INTENT(OUT)       ::  CREPT(NOBS)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

INTEGER      ::  AGFLAG
INTEGER      ::  ANERR
INTEGER      ::  ARRAY1(12000)
INTEGER      ::  ARRAY2(12000)
INTEGER      ::  ARRAY3(500)
INTEGER      ::  ARRAY4(500)
INTEGER      ::  EXEC        ! INDICATOR FOR EXC UID'S
INTEGER      ::  FTNO
INTEGER      ::  I      !- general loop counter
INTEGER      ::  ICENT
INTEGER      ::  ICT(8) !- for system time
INTEGER      ::  ICTMN2 !- end time century minutes
INTEGER      ::  ICTHR1
INTEGER      ::  ICTHR2
INTEGER      ::  IDATA(5)
INTEGER      ::  IDCNNL(50)
INTEGER      ::  IDESNO
INTEGER      ::  IDTYPE
INTEGER      ::  IERR
INTEGER      ::  IETIME(9) !- Start & end time for 1 data set !2.4
INTEGER      ::  IFAIL
INTEGER      ::  IFORM
INTEGER      ::  ILEN   !- length of request string
INTEGER      ::  IMAP(500)
INTEGER      ::  IMODEL
INTEGER      ::  IOBS
INTEGER      ::  IOVER
INTEGER      ::  IPOS
INTEGER      ::  IRAINL(50)
INTEGER      ::  IRECAL !- 1 if more datasets to come
INTEGER      ::  IRECV(10) !- receipt time
INTEGER      ::  IRM    ! Merged data flag (1=raw, 2=merged)
INTEGER      ::  ISATID(10) !- identifier list
INTEGER      ::  ISPOS
INTEGER      ::  ISTAT
INTEGER      ::  ISTYP
INTEGER      ::  ISTAN(50)
INTEGER      ::  TAFLEN !for short/long (A)TAFs; not report length
INTEGER      ::  METSPECI !- METAR or SPECI specified
INTEGER      ::  ITIME(9) !- User's data time window
INTEGER      ::  ITIMEK(2) !- Temp. store for start/end 'hhmm'
INTEGER      ::  ITOD
INTEGER      ::  ITYPE
INTEGER      ::  IVER
INTEGER      ::  IWMOL(50)
INTEGER      ::  J1     !- general loop counter
INTEGER      ::  K      !- general loop counter
INTEGER      ::  KNELEM
INTEGER      ::  KNOBS
INTEGER      ::  LAT_SUBSCRIPT
INTEGER      ::  LON_SUBSCRIPT
INTEGER      ::  NDES
INTEGER      ::  NHOUR  ! Hour of day (00 - 23)
INTEGER      ::  NUM
INTEGER      ::  NUMTEMPS
INTEGER      ::  NUMWINDS
INTEGER      ::  P        ! loop counter
INTEGER      ::  RC
INTEGER      ::  RETSTA
INTEGER      ::  SELECT(50) !- SELECT keyword values
INTEGER      ::  TOTAL_OBS !- total obs retrieved per subtype
INTEGER      ::  TRANGE
INTEGER      ::  UAPART

!-----------------------------------------------------------------------
! declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

REAL         ::  AREA(5)    !- lat/lon area
REAL         ::  RPOLE(2)   !- Rotated POLE coords

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

LOGICAL      ::  DDEXIST !- TRUE if user's DDICT exists
LOGICAL      ::  FIRSTIM !- TRUE first time through
LOGICAL      ::  FOUND(35) !- TRUE for keywords used
LOGICAL      ::  LATEST !- TRUE for latest reports
LOGICAL      ::  LAT_FOUND
LOGICAL      ::  LCHREP !- TRUE if no elements wanted, only text
LOGICAL      ::  LMSG   !- TRUE to return data one msg at a time
LOGICAL      ::  LON_FOUND
LOGICAL      ::  LQCFLG !- TRUE for qc elements in CLIMUK
LOGICAL      ::  LTEST  !- TRUE for lots of output
LOGICAL      ::  NEWMDBCALL
LOGICAL      ::  WANTSTNNAME !- TRUE if user wants LNDSYN stn name
LOGICAL      ::  WANTTEMPS !- TRUE for UPRAIR temperatures
LOGICAL      ::  WANTWINDS !- TRUE for UPRAIR winds

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

CHARACTER(36)   ::  ANAME(500)
CHARACTER(120)  ::  CDSN
CHARACTER(48)   ::  CERR
CHARACTER(4)    ::  CICAOL(50)
CHARACTER(2)    ::  CM         ! 'CM' Current/Merge indicators
CHARACTER(9)    ::  CIDENT(50) !- list of identifiers
CHARACTER(1)    ::  CPOS       !- 'L'atest or 'A'rchive
CHARACTER(5000) ::  CREQ
CHARACTER(1)    ::  CRTYP
CHARACTER(8)    ::  CTYPEK
CHARACTER(80)   ::  DDICTNAME !- users ddict REQUEST ds name
CHARACTER(8)    ::  ELIST      !- element index member name
CHARACTER(8)    ::  LAST       ! Last 'LIST' processed
CHARACTER(8)    ::  LIST       ! Name of list of element names
CHARACTER(3)    ::  MSTREAM    !- MASS stream (minus MDB pfx)
CHARACTER(1)    ::  ORDER      !- 'B'ackwards or 'F'orwards
CHARACTER(5)    ::  RPCEXC(6)  ! List of names exc from secure logg
CHARACTER(8)    ::  SUBTYPE
CHARACTER(5)    ::  UID        ! user id
CHARACTER(36)   ::  USRELM(12000)

!-----------------------------------------------------------------------
! declare functions
!-----------------------------------------------------------------------

INTEGER DT2HRS !- convert date/time to century hours

!-----------------------------------------------------------------------
! dynamic common
!-----------------------------------------------------------------------

COMMON/MDBDC1/USRELM,ANAME,CIDENT,CICAOL
COMMON/MDBDC2/ARRAY1,ARRAY2,ARRAY3,ARRAY4,IWMOL,IDCNNL, &
              IRAINL,IMAP,ISTAN

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Data initialisation
!-----------------------------------------------------------------------
!
DATA CM/'CM'/
DATA LAST/' '/
DATA RPCEXC/'T12DB','MDBOE','MDBOC','MDBOG','MDBOQ','MDBOT'/
!
!
!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

SAVE

EXEC=1
!
!-----------------------------------------------------------------------
! !G S.Cox 04/03/96. initialise output strings for RPC
! work. if this isn't done, C will not interpret hexadeciamal 0
! correctly as the system initialises the "blanks".
!-----------------------------------------------------------------------

DO I = 1,NOBS
   CSTR(I)(:)=' '
   CREPT(I)(:)=' '
END DO

!-----------------------------------------------------------------------
! If called with ISTAT=99, return immediately. ISTAT=99 is used for
! the MetDB RPC killserver call, but may be present at the end of users
! code to call the MDB directly. Allow this so users can have the
! same code for RPC and non-RPC MetDB calls.
!-----------------------------------------------------------------------

IF (STATUS == 99) RETURN

!-----------------------------------------------------------------------
! skip to recall if this is not the 1st call!
!-----------------------------------------------------------------------

IOBS=0
IF (STATUS == 4 .OR. STATUS == 12) THEN
  SUBTYPE = CTYPEK
  NOBS    = KNOBS
  NELEM   = KNELEM
  GOTO 500
END IF

!=======================================================================
! initialise first time through  (GOTO 500 above means all code
!                                 till 500 is first time only)
!
!======================================================================
IDESNO     = 0
STATUS     = 0
FIRSTIM    = .TRUE.
NEWMDBCALL = .TRUE.
TOTAL_OBS  = 0
WANTSTNNAME = .FALSE.

!-----------------------------------------------------------------------
! get current date/time
!-----------------------------------------------------------------------

#if defined (MVS)
CALL DATIM(ICT)
#else
CALL METDB_DATIM(ICT)
#ENDIF
!
!-----------------------------------------------------------------------
! change STAFS and LTAFS to the general subtype TAFS (for use by
! RTABLE & MDBALC, i.e. both kinds of TAF are in the same dataset)
!-----------------------------------------------------------------------

TAFLEN=0    !both short and long (A)TAFs
METSPECI=0  !METARs (not SPECIs)

IFLABEL1: &
IF (CTYPE(1:5) == 'STAFS') THEN
  SUBTYPE='TAFS'
  TAFLEN=2
ELSE IF (CTYPE(1:5) == 'LTAFS') THEN
  SUBTYPE='TAFS'
  TAFLEN=1
!-----------------------------------------------------------------------
! change SATAFS and LATAFS to the general subtype ATAFS (for use by
! RTABLE & MDBALC, i.e. both kinds of TAF are in the same dataset)
!-----------------------------------------------------------------------

ELSE IF (CTYPE(1:6) == 'SATAFS') THEN
  SUBTYPE='ATAFS'
  TAFLEN=2
ELSE IF (CTYPE(1:6) == 'LATAFS') THEN
  SUBTYPE='ATAFS'
  TAFLEN=1

!-----------------------------------------------------------------------
! change SPECI to the general subtype METARS (for use by
! RTABLE & MDBALC, i.e. both METAR and SPECI are in the same dataset)
!-----------------------------------------------------------------------

ELSE IF (CTYPE(1:5) == 'SPECI') THEN
  SUBTYPE='METARS'
  METSPECI=3
ELSE
  SUBTYPE=CTYPE
END IF IFLABEL1

CTYPEK=SUBTYPE
!
!-----------------------------------------------------------------------
! KNOBS and KNELEM only used for reset above
!-----------------------------------------------------------------------

KNOBS  = NOBS
KNELEM = NELEM
IFORM  = 0
IERR   = 0

!-----------------------------------------------------------------------
! take extra spaces out of user string
!-----------------------------------------------------------------------

CALL DELSPCE(CREQ,ILEN,REQUEST)
!=======================================================================
! check user id for authorisation and record details of the request
! in a date delimited log file (non RPC only)
!=======================================================================
!
CALL CHECK_UID(UID)

DO P=1,6
 IF (RPCEXC(P) == UID) THEN
   EXEC=-1
 ENDIF
END DO

IF (EXEC > 0) THEN
  CALL CHECK_AUTH(UID,CTYPE,ICT,NOBS,NELEM,CREQ,RC)
  IF (RC == -1) THEN
    IERR=1
    CERR='UNAUTHORISED ACCESS ATTEMPTED'
    GOTO 998
  END IF
END IF

!-----------------------------------------------------------------------
! initialise request variables
!-----------------------------------------------------------------------

CALL SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID,AREA, &
            CIDENT,IOVER,IVER,ORDER,LTEST,LMSG,ISTYP, &
            UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)

!-----------------------------------------------------------------------
! decode request up to keyword ELEMENTS
!-----------------------------------------------------------------------

CALL DATE31(ICT(6),ICT(7),ICT(8),ICENT)

CALL GETREQ(SUBTYPE,CREQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST, &
            ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER, &
            ICENT,LTEST,LMSG,FOUND,ITOD,ISTYP,IFAIL,CERR, &
            UAPART,DDICTNAME,IMODEL,RPOLE,SELECT)

IF (IFAIL == 8) THEN
  IERR=1
  GOTO 998
END IF

!-----------------------------------------------------------------------
! adjust day and hour if CLIMAT data requested because time tag
! for this data type handled differently ie by the month
!-----------------------------------------------------------------------

IF (SUBTYPE(1:6) == 'CLIMAT') THEN
  ITIME(3) = 14 + MOD(ITIME(2),4)
  ITIME(4) = 0
END IF

!-----------------------------------------------------------------------
! If diagnostics switched on (LTEST = TRUE) output those returned from
! the GETREQ call
!-----------------------------------------------------------------------

IF (LTEST) THEN
  WRITE(*,'(/1X,''In MetDB subroutine MDB after GETREQ'' )')
  WRITE(*,'( 1X,''====================================''/)')

  WRITE(*,'(1X,''In MDB: ITIME  = '',10(1X,I5))')ITIME
  WRITE(*,'(1X,''In MDB: IRECV  = '',10(1X,I5))')IRECV
  WRITE(*,'(1X,''In MDB: AREA  = '',10(1X,F8.3))')AREA
  WRITE(*,*)'in MDB: TRANGE     = ',TRANGE
  WRITE(*,*)'In MDB: IFORM      = ',IFORM
  WRITE(*,*)'In MDB: IOVER      = ',IOVER
  WRITE(*,*)'In MDB: IVER       = ',IVER
  WRITE(*,*)'In MDB: IDTYPE     = ',IDTYPE
  WRITE(*,*)'In MDB: IMODEL     = ',IMODEL
  WRITE(*,*)'In MDB: ISTYP      = ',ISTYP
  WRITE(*,*)'In MDB: SELECT:'
  WRITE(*,'(20(1X,I3))')SELECT
  WRITE(*,*)'In MDB: ISATID:'
  WRITE(*,'(10(1X,I5))')ISATID
  WRITE(*,*)'In MDB: ISTAN:'
  WRITE(*,'(10(1X,I5))')(ISTAN(K),K=1,ISTAN(1)+1)
  WRITE(*,*)'In MDB: CIDENT:'
  WRITE(*,'(10(1X,A10))')CIDENT
  WRITE(*,*)'In MDB: FOUND:'
  WRITE(*,'(35(1X,L1))')FOUND

  WRITE(*,*)'In MDB: LATEST    = ',LATEST
  WRITE(*,*)'In MDB: ORDER     = ',ORDER
  WRITE(*,*)'In MDB: LMSG      = ',LMSG
END IF

!-----------------------------------------------------------------------
! Before we call RTABLE to get ITYPE, check to see if the user has
! coded a retrieval table name in the request string. If so, check to
! see if the data set exists. If not return with an error.
!-----------------------------------------------------------------------

IFLABEL2: &
IF (FOUND(31)) THEN
  DDEXIST=INQUIRE(DDICTNAME(2:),'DSN')

  IF (.NOT.DDEXIST) THEN
    WRITE(6,'(/2X,75(''*''))')
    WRITE(6,'(2x,A,A)')'Returning to MDB calling program. ', &
    'Data Dict name specified in CREQ'
    WRITE(6,'(2X,''does not exist. Check name '',A)')DDICTNAME(2:)
    WRITE(6,'(2X,75(''*'')/)')
    IERR=1
    CERR='See info above'
    GOTO 998
  END IF
END IF IFLABEL2

!-----------------------------------------------------------------------
! Copy and validate request times (doesn't validate that much !!)
!-----------------------------------------------------------------------

ITYPE = 0
IF (SUBTYPE == 'STNMAS  ') ITYPE = 1
CALL VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL,CERR)

IF (IFAIL == 8) THEN
  IERR=1
  GOTO 998
ELSE IF (IFAIL == 4) THEN
  WRITE(*,*)'MDB WARNING:'//CERR
END IF
!                                             Copy ITIME to IETIME
DO I=1,9
  IETIME(I) = ITIME(I)
END DO

!-----------------------------------------------------------------------
! Loop over datasets needed to satisfy request.
! RTABLE allocates one at a time.
! Condition code may say more to come, so come back to here.
!-----------------------------------------------------------------------

 997  CONTINUE
IRM = IFORM
CALL RTABLE (SUBTYPE, IRM, IMODEL, DDICTNAME, LTEST, &
             IETIME, CDSN, IDATA, LIST, IERR, CERR, &
             MSTREAM, ELIST)
ITYPE = IDATA(4)
CRTYP = CM(IRM:IRM)

!-----------------------------------------------------------------------
! Check for errors from RTABLE; 16 means no data set, 4 means more data
! sets to come, so set a flag to come back when this data set is done,
! and adjust the end times for this data set.
!-----------------------------------------------------------------------

IF (IERR == 4) THEN
  IRECAL=1
ELSE IF (IERR > 4) THEN
  NOBS=0                                                     !1.22
  STATUS=8
  GOTO 998
END IF

!-----------------------------------------------------------------------
! Skip elements check if retrieving with MDBRET (ITYPE=7) or skip
! both elements check and call to MDBALC if retrieving GRIB data.
!-----------------------------------------------------------------------

IF (ITYPE == 7)  GO TO 499
IF (ITYPE == 10) GO TO 500

!-----------------------------------------------------------------------
! Call READLIST to read the required list of element names and map
! numbers if it was not the last one read.
!-----------------------------------------------------------------------

IF (LIST /= LAST) THEN
  CALL READLIST (LIST, LTEST, ANAME, IMAP, NDES, IERR, CERR)
  LAST = LIST
END IF

IF (LTEST) THEN
  WRITE(*,*)'In MDB: CRTYP = ',CRTYP
  WRITE(*,*)
  WRITE(*,*)'In MDB: Elements list, NDES = ',NDES
  WRITE(*,'(1X,A36,I8)')(ANAME(K),IMAP(K),K=1,NDES)
END IF

!-----------------------------------------------------------------------
! Find string ' ELEMENTS ' in the request string.
!-----------------------------------------------------------------------

IFLABEL3: &
IF (FIRSTIM) THEN
  IF (LTEST) WRITE(*,*)'MDB: Looking for elements'
  I=INDEX(CREQ,' ELEMENTS ')

!-----------------------------------------------------------------------
! If string found move to end of ' ELEMENTS ' (I+10)
!-----------------------------------------------------------------------

IFLABEL4: &
  IF (I /= 0) THEN
    IF (LTEST) WRITE(*,*)'MDB: Elements keyword found'
    IPOS=I+10

!-----------------------------------------------------------------------
! For retrieval from inline list, put unique element names in array
! USRELEM and correct any misspellings (from DTMN3 or aliases)
!-----------------------------------------------------------------------

    IF (LTEST) WRITE(*,*)'MDB: Calling EXPELM'

    CALL EXPELM(CREQ,IPOS,ILEN,USRELM,NUM,LQCFLG,IFAIL,CERR)

    IF (LTEST) WRITE(*,*)'MDB: After EXPELM: NUM,LQCFLG = ', &
               NUM,LQCFLG

    IF (IFAIL == 8) THEN
      IERR=1
      GOTO 998
    END IF

    CALL ALSELM(SUBTYPE,USRELM,NUM,LTEST)

    CALL MAPELM(USRELM,NUM,ANAME,IMAP,NDES,ARRAY1, &
                IDESNO,ARRAY2,LTEST)

    IF (LTEST) THEN
      WRITE(*,*)'MDB: After MAPELM: IDESNO = ',IDESNO
      WRITE(*,'(10I6)')(ARRAY1(I),ARRAY2(I),I=1,IDESNO)
    END IF

!-----------------------------------------------------------------------
! Test for AREA and Lat Long elements requested if ITYPE=2, 3, 4 or 9.
! If Lat and Long requested obtain subscripts from ARRAY1 from MAPELM
!-----------------------------------------------------------------------

IFLABEL5: &
    IF (ITYPE == 2 .OR. ITYPE == 3 .OR. &
        ITYPE == 4 .OR. ITYPE == 9) THEN
IFLABEL6: &
      IF (FOUND(7)) THEN
        DO I=1,NUM
          IF (USRELM(I)  ==  'LTTD') THEN
            LAT_SUBSCRIPT=I
            LAT_FOUND=.TRUE.
          ELSE IF (USRELM(I) ==  'LNGD') THEN
            LON_SUBSCRIPT=I
            LON_FOUND=.TRUE.
          END IF
        END DO

!-----------------------------------------------------------------------
! If Lat/Long not found then print a warning message to the user
! and abandon retrieval of data
!-----------------------------------------------------------------------

        IF ((.NOT. LAT_FOUND) .OR. (.NOT. LON_FOUND)) THEN
          WRITE(*,*)'MDB ERROR: PLEASE RESPECIFY YOUR'
          WRITE(*,*)'REQUEST AREA LAT/LONG BOX SPECIFIED'
          WRITE(*,*)'BUT EITHER LAT OR LONG OR BOTH'
          WRITE(*,*)'LAT AND LONG ARE MISSING FROM YOUR'
          WRITE(*,*)'REQUESTED ELEMENTS'
          IFAIL=8
        END IF !- lat_found, lon_found
      END IF IFLABEL6 !- found(7)
    END IF IFLABEL5 !- itype

    IF (IFAIL  ==  8) THEN
      IERR=1
      GOTO 998
    END IF

!-----------------------------------------------------------------------
! Test for WMO_STTN_NAME in USRELM array if the subtype is LNDSYN.
!-----------------------------------------------------------------------

    IF (SUBTYPE(1:6) == 'LNDSYN') THEN
      DO I=1,NUM
        IF (USRELM(I) == 'WMO_STTN_NAME') WANTSTNNAME=.TRUE.
      END DO
    END IF

!-----------------------------------------------------------------------
! set LCHREP TRUE if only report text needed (if no keyword 'ELEMENTS'
! or no element names found after it, or it's the only element
! requested).
!-----------------------------------------------------------------------

    LCHREP = (IDESNO == 1 .AND. ARRAY1(1) == 65250)

  ELSE  ! elements keyword not given - default report text only

    IPOS=1
    LCHREP=.TRUE.
    ARRAY1(1)=65250
    IDESNO=1
  END IF IFLABEL4

  FIRSTIM=.FALSE.
ELSE       !- if not first time, remap elements for next data set
  IF (.NOT.LCHREP) THEN
    CALL MAPELM(USRELM,NUM,ANAME,IMAP,NDES,ARRAY1, &
                IDESNO,ARRAY2,LTEST)
  END IF
END IF IFLABEL3

IF (LTEST) THEN
  WRITE(6,1010) &
      (ARRAY1(K),ARRAY3(K),ARRAY2(K),ARRAY4(K),K=1,IDESNO)
1010    FORMAT(' DESCRIPTORS  SEQUENCE  REPLICATION  SIGNIFICANCE'/ &
          (I8,6X,I5,8X,I3,10X,I5))
END IF

!-----------------------------------------------------------------------
! allocate required datasets
!-----------------------------------------------------------------------

499   CONTINUE
FTNO=IDATA(3)   ! KEEP TO CLOSE LATER

CALL MDBALC(SUBTYPE,IDATA,CDSN,IFAIL,CERR,LTEST,DDICTNAME, &
            MSTREAM)

IF (IFAIL == 8) THEN
  IERR=1
  GOTO 998
END IF

!=======================================================================
!=======================================================================
!
! RETRIEVE DATA (BRANCH HERE FROM START IF NOT FIRST CALL)
!
!=======================================================================
!=======================================================================

500   CONTINUE

!=======================================================================
!
! STNMAS
!
!=======================================================================

!-----------------------------------------------------------------------
! Correct setting of IWMOL, IDCNNL & IRAINL arrays. These arrays are
! never initialised, so if a new STNMAS request is made wih a shorter
! list of ids, the previous list is preserved and used in subroutine
! STNINDX. Now loop over all 49 identifiers in ISTAN array
!-----------------------------------------------------------------------

IFLABEL7: &
IF (ITYPE == 1) THEN

IFLABEL8: &
   IF (IDTYPE == 1) THEN      ! wmo identifiers
     DO J1=1,49
       IWMOL(J1)=ISTAN(J1+1)
       CICAOL(J1)='    '
       IDCNNL(J1)=-32768
       IRAINL(J1)=-32768
     END DO
   ELSE IF (IDTYPE == 2) THEN ! icao identifiers
     DO J1=1,50               ! no count from getreq, so use max
       IWMOL(J1)=-32768
       CICAOL(J1)=CIDENT(J1)(1:4)
       IDCNNL(J1)=-32768
       IRAINL(J1)=-32768
     END DO
   ELSE IF (IDTYPE == 3) THEN ! dcnn identifiers
     DO J1=1,49
       IWMOL(J1)=-32768
       CICAOL(J1)='    '
       IDCNNL(J1)=ISTAN(J1+1)
       IRAINL(J1)=-32768
     END DO
   ELSE IF (IDTYPE == 4) THEN ! rainfall identifiers
     DO J1=1,49
       IWMOL(J1)=-32768
       CICAOL(J1)='    '
       IDCNNL(J1)=-32768
       IRAINL(J1)=ISTAN(J1+1)
     END DO
   ELSE
     IERR=1
     CERR='NO PLATFORM SPECIFIED FOR STNMAS RETRIEVAL'
     GOTO 998
   END IF IFLABEL8

   IF (IVER == 1) THEN
     CPOS='L'      ! LATEST
   ELSE
     CPOS='A'      ! archive
   END IF

   ISTAT=STATUS

   CALL STMRET(IWMOL,CICAOL,IDCNNL,IRAINL,IDTYPE,CPOS,ISTYP, &
               ISTAT,RVALS,NOBS,NELEM,ARRAY1,CSTR,CERR)

   IF (ISTAT < 16) THEN
     STATUS=ISTAT
   ELSE
     STATUS=16
   END IF

!=======================================================================
!
! GRIBRET for TCRTEMP and other GRIB products.
! (Check that 'NELEM' has been specified as 1 for ITYPE 10.)
!
!=======================================================================

ELSE IF (ITYPE == 10) THEN
!                                                     Check NELEM
  IF (NELEM /= 1) THEN
    WRITE (*,*) 'MDB ERROR: ', &
      '"NELEM" MUST BE SPECIFIED AS 1 FOR GRIB RETRIEVALS.'
    IERR = 1
    GO TO 998
  END IF
!                                                  GRIB retrieval
  CALL GRIBRET (SUBTYPE, CDSN, NOBS, IDATA, &
                FOUND, LTEST, SELECT, IETIME, STATUS, &
                CREPT, RVALS(1:NOBS,1:1))
  RETURN

!=======================================================================
!
! check for retrieval across data sets: either tell user more data to
! come or carry on filling array from new data set.
!
!=======================================================================

ELSE
  IF (AGFLAG == 1 .AND. NOBS == IOBS) THEN
    STATUS=4
    GOTO 999
  ELSE IF (AGFLAG == 1 .AND. NOBS /= IOBS) THEN
    AGFLAG=0
    STATUS=16
  END IF
  IOBS=IOBS+1
  RETSTA=STATUS

!-----------------------------------------------------------------------
! SSMRET (type 4) requires hours only, not hours & minutes
!-----------------------------------------------------------------------

  IF (ITYPE == 4) THEN
    ITIMEK(1) = IETIME(4)        ! Store hours & minutes
    ITIMEK(2) = IETIME(8)        ! for replacement later
    IETIME(4) = IETIME(4)/100    ! 'hhmm' to 'hh'
    IETIME(8) = IETIME(8)/100    ! 'hhmm' to 'hh'
  END IF

!=======================================================================
!
! NEW-FORMAT SATELLITE STORAGE
!
!=======================================================================

IFLABEL9: &
  IF (ITYPE == 2) THEN

    CALL BUFRET(IETIME,IRECV,AREA,CIDENT,SELECT,IOVER, &
                RVALS,NOBS,NELEM,IOBS,ARRAY1,IDESNO,ARRAY2, &
                RETSTA,ANERR,IDATA,LMSG,LTEST,LQCFLG,CSTR, &
                CREPT,SUBTYPE,NEWMDBCALL,LAT_SUBSCRIPT, &
                LON_SUBSCRIPT,RPOLE,FOUND,ELIST)

!#######################################################################
!=======================================================================
!
! RETRIEVAL ROUTINE FOR LONG BULLETINS & MULTIPLE SEQUENCES
!
!=======================================================================

  ELSE IF (ITYPE == 7) THEN

    CALL MDBRET (IETIME, IRECV, AREA, CIDENT, SELECT, IOVER, &
                 RVALS, NOBS, NELEM, IOBS, CREQ, RETSTA, &
                 IDATA, LMSG, LTEST, CSTR, CREPT, SUBTYPE, &
                 RPOLE, FOUND, ELIST, ANERR)
!#######################################################################

!=======================================================================
!
! AMDARS, DRIFTR, WINPRO, BATHY, BUOY, TESAC, SATOB
!
!=======================================================================

  ELSE IF (ITYPE == 3) THEN

    CALL BUSRET(SUBTYPE,IETIME,IRECV,AREA,CIDENT,RVALS,NOBS, &
                NELEM,IOBS,ARRAY1,IDESNO,ARRAY2,RETSTA,ANERR, &
                IDATA,CSTR,CREPT,LCHREP,LTEST,LQCFLG,NEWMDBCALL, &
                LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,LMSG, &
                FOUND,ELIST)

!=======================================================================
!
! SAT120, GLOSS, UKMMWI, ESAUWA, ESAURA, RALUAT, SSM-I
!
!=======================================================================

  ELSE IF (ITYPE == 4) THEN

!-----------------------------------------------------------------------
! if satellite idents have been set under 'PLATFORM' keyword, convert
! the character strings to numbers in array input to SATRET.
!-----------------------------------------------------------------------

    IF (ISATID(1) == 0 .AND. CIDENT(1)(1:5) /= '00000') THEN
      DO J1=1,10
        IF (CIDENT(J1)(1:5) /= '00000') THEN
          ISPOS=INDEX(CIDENT(J1),' ')
          READ (CIDENT(J1)(1:ISPOS-1),*) ISATID(J1)
        END IF
      END DO
    END IF

    CALL SSMRET(IETIME,IRECV,AREA,ISATID,IOVER,RVALS,NOBS, &
                NELEM,IOBS,ARRAY1,IDESNO,ARRAY2,RETSTA,ANERR, &
                IDATA,LMSG,LTEST,LQCFLG,CSTR,CREPT,SUBTYPE, &
                NEWMDBCALL,LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE, &
                FOUND,ELIST)

!=======================================================================
!
! TAFS, ATAFS, METARS, SAMOSX, CLIMAT
!
!=======================================================================

  ELSE IF (ITYPE == 5) THEN

    CALL TFMRET(SUBTYPE,IETIME,TRANGE,IRECV,AREA,CIDENT, &
                RVALS,NOBS,NELEM,IOBS,ARRAY1,IDESNO, &
                RETSTA,ANERR,IDATA,CSTR,CREPT, &
                LTEST,LCHREP,ORDER,LATEST,TAFLEN,METSPECI, &
                FOUND,RPOLE,IVER)

!=======================================================================
!
! SYNRET for SHIPS, SYNOPS and AIREPS
!
!=======================================================================

  ELSE IF (ITYPE == 6) THEN

    CALL SYNRET(SUBTYPE,IETIME,IRECV,AREA,CIDENT, &
                RVALS,NOBS,NELEM, IOBS,ARRAY1,IDESNO,ARRAY2, &
                RETSTA,ANERR,IDATA,CSTR,CREPT,LTEST,LCHREP,ORDER, &
                LATEST,LQCFLG,FOUND,IVER,NEWMDBCALL,RPOLE, &
                WANTSTNNAME,TRANGE,ELIST)

!=======================================================================
!
! SEAICE and TROPADV                                                  !X
!
!=======================================================================

  ELSE IF (ITYPE == 8) THEN

    CALL ICERET(IETIME,RVALS,NOBS,NELEM,IOBS,ARRAY1, &
                IDESNO,CIDENT,RETSTA,ANERR,IDATA,CSTR,CREPT, &
                LCHREP,ORDER,FOUND,IVER)

!=======================================================================
!
! UPRRET (TEMP, PILOT, DROPSOND)
!
!=======================================================================

  ELSE IF (ITYPE == 9) THEN

    CALL CHK4TW(ARRAY1,ARRAY2,IDESNO,WANTTEMPS,WANTWINDS, &
                NUMTEMPS,NUMWINDS,LTEST)

IFLABEL10: &
    IF ((WANTTEMPS.AND.WANTWINDS) .AND. &
        (NUMTEMPS /= NUMWINDS)) THEN
      IERR=1
      CERR='SEE MESSAGE ABOVE'
      WRITE(6,'(/1X,''MDB: Cannot do UPRAIR retrieval.'')')
      WRITE(6,'( 1X,''You are requesting temperature and '')')
      WRITE(6,'( 1X,''wind level data but the replication'')')
      WRITE(6,'( 1X,''counts given differ - NOT PERMITTED!'')')
    ELSE
      CALL UPRRET(SUBTYPE,LTEST,RETSTA,IDATA,IETIME,IRECV, &
                  FOUND,UAPART,CIDENT,AREA,ANERR,IOVER,IVER, &
                  ARRAY1,IDESNO,ARRAY2,LQCFLG,RVALS,NOBS, &
                  NELEM,CSTR,CREPT,IOBS,WANTTEMPS,WANTWINDS, &
                  NUMTEMPS,NUMWINDS,TRANGE,NEWMDBCALL, &
                  CRTYP,LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE, &
                  ELIST)
    END IF IFLABEL10

!=======================================================================
!
! RETURN IF RETRIEVAL IMPOSSIBLE OR ERROR IN RETRIEVAL
!
!=======================================================================

  ELSE
    CERR='UNABLE TO DO RETRIEVAL FOR THIS SUBTYPE '//CTYPE
    IERR=1
    WRITE(*,'(/1X,''UNABLE TO DO RETRIEVAL FOR THIS SUBTYPE ''/)')
    WRITE(*,'( 1X,''=======================================''/)')
    WRITE(*,'(1X,''D/T:'',8I10)')ICT
    WRITE(*,'(1X,''In MDB: CTYPE  = '',(1X,A8))')CTYPE
    WRITE(*,*)'In MDB: DDICTNAME  = ',DDICTNAME
    WRITE(*,'(1X,''In MDB: NOBS   = '',(1X,I10))')NOBS
    WRITE(*,'(1X,''In MDB: NELEM  = '',(1X,I10))')NELEM
    WRITE(*,'(1X,''In MDB: STATUS = '',(1X,I10))')STATUS
    WRITE(*,'(1X,''In MDB: ITIME  = '',10(1X,I5))')ITIME
    WRITE(*,'(1X,''In MDB: IRECV  = '',10(1X,I5))')IRECV
    WRITE(*,'(1X,''In MDB: AREA  = '',10(1X,F8.3))')AREA
    WRITE(*,*)'In MDB: ITYPE      = ',ITYPE
    WRITE(*,*)'In MDB: IDATA      = ',IDATA
    WRITE(*,*)'in MDB: TRANGE     = ',TRANGE
    WRITE(*,*)'In MDB: IFORM      = ',IFORM
    WRITE(*,*)'In MDB: IOVER      = ',IOVER
    WRITE(*,*)'In MDB: IVER       = ',IVER
    WRITE(*,*)'In MDB: IDTYPE     = ',IDTYPE
    WRITE(*,*)'In MDB: IMODEL     = ',IMODEL
    WRITE(*,*)'In MDB: ISTYP      = ',ISTYP
    WRITE(*,*)'In MDB: SELECT:'
    WRITE(*,'(20(1X,I3))')SELECT
    WRITE(*,*)'In MDB: ISATID:'
    WRITE(*,'(10(1X,I5))')ISATID
    WRITE(*,*)'In MDB: ISTAN:'
    WRITE(*,'(10(1X,I5))')(ISTAN(K),K=1,ISTAN(1)+1)
    WRITE(*,*)'In MDB: CIDENT:'
    WRITE(*,'(10(1X,A10))')CIDENT
    WRITE(*,*)'In MDB: FOUND:'
    WRITE(*,'(35(1X,L1))')FOUND

    WRITE(*,*)'In MDB: LATEST    = ',LATEST
    WRITE(*,*)'In MDB: ORDER     = ',ORDER
    WRITE(*,*)'In MDB: LMSG      = ',LMSG
    GOTO 998
  END IF IFLABEL9

!-----------------------------------------------------------------------
! check for error from retrieval first
!-----------------------------------------------------------------------

  IF (ANERR == 16) THEN
    CERR='ABNORMAL TERMINATION'
    IERR=1
    GOTO 998
  END IF

!-----------------------------------------------------------------------
! set status if no errors
!-----------------------------------------------------------------------

  IF (RETSTA <= 8) STATUS=RETSTA

!-----------------------------------------------------------------------
! If using SSMRET (type 4) restore hours & minutes
!-----------------------------------------------------------------------

  IF (ITYPE == 4) THEN
    IETIME(4) = ITIMEK(1)
    IETIME(8) = 100*IETIME(8) + 59
  END IF

!-----------------------------------------------------------------------
! If this call is complete, check for more datasets
!-----------------------------------------------------------------------

IFLABEL11: &
  IF (STATUS == 0 .OR. STATUS == 8) THEN
IFLABEL12: &
    IF (IRECAL == 1) THEN
      IF (ITYPE == 1) THEN
        CLOSE(FTNO)
      ELSE
        CALL METDB_CCLOSE(FTNO)
      END IF
!                                                     Get century hours
      NHOUR = IETIME(4)/100
      ICTHR1=DT2HRS(IETIME(1),IETIME(2),IETIME(3),NHOUR)
      NHOUR = IETIME(8)/100
      ICTHR2=DT2HRS(IETIME(5),IETIME(6),IETIME(7),NHOUR)

!                                       Increment to get new start time
      IF (FOUND(4)) THEN
!                             Add the smallest multiple of increment to
!                               start time which gets you past end time
        ICTHR1 = ICTHR1 + &
                 IETIME(9)*((ICTHR2-ICTHR1)/IETIME(9) + 1)
        ICTMN2=ICTHR1*60+MOD(IETIME(4),100)
      ELSE
        ICTMN2=ICTHR2*60+MOD(IETIME(8),100)
        ICTMN2=ICTMN2+1
        ICTHR1=ICTMN2/60
      END IF
!                                     Put new start time in IETIME(1-4)

      CALL HRS2DT(IETIME(1),IETIME(2),IETIME(3),NHOUR,ICTHR1)
      IETIME(4) = 100*NHOUR + MOD(ICTMN2,60)
      IF (LTEST) WRITE(*,*)'In MDB: MDB new start time = ',IETIME

!                                Restore user's end time in IETIME(5-8)
      IETIME(5)=ITIME(5)
      IETIME(6)=ITIME(6)
      IETIME(7)=ITIME(7)
      IETIME(8)=ITIME(8)

!-----------------------------------------------------------------------
! Go back to RTABLE call for another data set
!-----------------------------------------------------------------------

      IRECAL=0
      AGFLAG=1
      IF (LTEST) WRITE(*,*)'In MDB: Back for next dataset'
      GOTO 997
    ELSE
      IF (ITYPE == 1) THEN
        CLOSE(FTNO)
      ELSE
        CALL METDB_CCLOSE(FTNO)
      END IF
    END IF IFLABEL12
    NOBS=IOBS
  ELSE IF (LMSG) THEN
    NOBS=IOBS
  END IF IFLABEL11
END IF IFLABEL7
GOTO 999

!-----------------------------------------------------------------------
! print error messages
!-----------------------------------------------------------------------

 998  IF (IERR == 1) THEN
  WRITE(6,*)' MDB ERROR: ',CERR
  STATUS=16
ELSE IF (IERR > 1) THEN
  WRITE(6,*)' MDB ERROR: ',IERR,CERR
END IF

 999  CONTINUE

TOTAL_OBS = TOTAL_OBS + NOBS                                 !1.20
IF (STATUS == 8 .AND. TOTAL_OBS > 0) STATUS = 0              !1.20

RETURN
END SUBROUTINE MDB
