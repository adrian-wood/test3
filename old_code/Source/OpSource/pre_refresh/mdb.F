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
!                                  : required data                   (i)
! (3) RVALS     : REAL(NOBS,NELEM) : data array                      (o)
! (4) NOBS      : INTEGER          : max dimension of rvals array    (i)
!                                  : number of observations          (o)
! (5) NELEM     : INTEGER          : number of elements each obs     (i)
! (6) STATUS    : INTEGER          : 0 new request                   (i)
!                                  : 0 request complete              (o)
!                                  : 4 continuation                  (i)
!                                  : 4 more data to come             (o)
!                                  : 8 no data                       (o)
!                                  : 16 fatal error                  (o)
! (7) CSTR      : CHAR*(*)(NOBS)   : array for string data           (o)
! (8) CREP      : CHAR*(*)(NOBS)   : array for report text           (o)
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 07/04/2009 16:04:57$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mdb.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  4    Met_DB_Project 1.3         07/04/2009 16:04:57    Richard Weedon
!       References to SELECT on the MDBDC1 common block removed
!  3    Met_DB_Project 1.2         03/04/2009 10:24:53    Richard Weedon
!       Revisioned under CR9502
!  2    Met_DB_Project 1.1         02/04/2009 11:19:40    Richard Weedon
!       Select parameter increased to 50 CHG 7636
!  1    Met_DB_Project 1.0         30/01/2006 20:23:10    Sheila Needham  
! $
! Revision 2.11  2003/09/04 11:31:38  usmdb
! 2.11.  15 September 2003.  Brian Barwell.  Change 90/03.
! Correct end time for second data set when INCREMENT is specified.
!
! Revision 2.10  2003/08/05  10:43:53  10:43:53  usmdb (MetDB account c/o usjh)
! 18 Aug 2003    C Long
! 2.10  Let BUSRET use minutes from request as well as hours
!
! Revision 2.9  2003/05/06 07:37:27  usmdb
! New variable ELIST contains the member name of the element index
! library - S.Cox.
!
! Revision 2.8  2003/03/06  09:10:36  09:10:36  usmdb (MetDB account c/o usjh)
! Added SELECT array for SELECT keyword. Array passed to various
! routines - S.Cox
!
! Revision 2.7  2003/02/03  15:51:24  15:51:24  usmdb (MetDB account c/o usjh)
! 2.7  Removed stationmaster log (revision 2.5). Added immediate
!      return to calling program if ISTAT=99. Put HEADSET logical
!      around the revision info - S.Cox
! 2.7b MSTREAM (mass stream) variable added. Passed to RTABLE and
!      MDBALC - S.Cox
!
! Revision 2.6  2002/07/01  14:05:46  14:05:46  usmdb (MetDB account c/o usjh)
! 2.6.  15 July 2002.  Brian Barwell.  Change 48/02.
! Pass list of platforms to BUFRET in character form.
!
! Revision 2.5  2002/06/18  15:26:52  15:26:52  usmdb (Generic MetDB account)
! For STNMAS calls only, create a log file and write to it
! the JOBNAME and MetDB request string for monitoring
! purposes - S.Cox
!
! Revision 2.4  2002/05/07  09:06:46  09:06:46  usmdb (Generic MetDB account)
! 2.4.  20 May 2002.  Brian Barwell.  Change 36/02.
! Calls to DDICT replaced by calls to new routines RTABLE and
! READLIST. ITIME not changed after validaton: IETIME used instead.
!
! Revision 2.3  2001/09/05  09:10:36  09:10:36  usmdb (Generic MetDB account)
! Changed declaration of CDSN to CHARACTER*120. Removed redundant
! code concerned with 1994 BUOY check - S.Cox
!
! Revision 2.2  2001/08/09  09:26:28  09:26:28  usmdb (Generic MetDB account)
! 2.2.  20 August 2001.  Brian Barwell.  Change 105/01.
! Add FOUND array to arguments in call to GRIBRET.
!
! Revision 2.1  2001/04/23  13:15:25  13:15:25  usmdb (Generic MetDB account)
! 23 April 2001.  Brian Barwell.
! 2.1:  Add call to BUFRET for retrievals from new-format
!       satellite storage data sets.
!
! Revision 2.0  2001/01/08  11:58:51  11:58:51  usmdb (Generic MetDB account)
! Removed argument SUBTYPE from ICERET call. Removed
! arguments CERR & IFAIL from MAPELM calls. Removed
! argument ICT from SETDEF call. Removed argument IFORM
! from VALREQ call. Changed pre-processor statement to
! provide a default. Removed EXTERNAL statements, added
! copyright & modified header - S.Cox
!
! Revision 1.26  2000/12/08  14:14:48  14:14:48  usmdb (Generic MDB account)
! 1.26  Additions for GRIB data retrieval: add call to GRIBRET
!       and skip code which handles element lists - B.Barwell
! 1.26a Remove duplicate argument in call to SSMRET - S.Cox
! 1.26b Enable retrieval of SPECIs (from the METAR datastore)
!       and change variable name ITAFT to TAFLEN - R Hirst
!
! Revision 1.25  99/03/11  14:56:31  14:56:31  usmdb (Generic MDB account)
! dummy revision due to problem with ckeckin
!
! Revision 1.24  99/03/11  14:02:47  14:02:47  usmdb (Generic MDB account)
! 15-03-1999 - S.Cox
! Removal of element dataset retrieval
! v(G)=83, ev(G)=28
!
! Revision 1.23  99/02/11  12:14:00  12:14:00  usmdb (Generic MDB account)
! 15-02-1999 S.Cox
! Update warning for demise of element dataset retrieval. This will now
! be 15th March 1999
! v(G)=96, ev(G)=33
!
! Revision 1.22  99/01/14  14:18:52  14:18:52  usmdb (Generic MDB account)
! 18-01-99  S.Cox  Ref. MetDB Problem 314
! Allow retrieval of ALL VERSIONS of SREW reports
! v(G)=96, ev(G)=33
!
! Revision 1.21  98/10/15  11:37:57  11:37:57  usmdb (Generic MDB account)
! 19-10-98 : Addition of subtype ATAFS (Automatic TAFS). Users
! can specify ATAFS, SATAFS or LATAFS as valid subtypes - S.Cox
! v(G)=96, ev(G)=33
!
! Revision 1.20  98/09/16  16:11:19  16:11:19  usmdb (Generic MDB account)
! 21-09-98 S.Cox - Ref problem 144. Addition of code to correct
! setting of STATUS when a retrieval spans more than 1 dataset.
!
! Revision 1.19  98/08/12  08:52:44  08:52:44  usmdb (Generic MDB account)
! Correction to retrieval that spans 2 or more
! datasets
!
! Revision 1.18  98/07/23  08:36:23  08:36:23  usmdb (Generic MDB account)
! Extra keyword for retrieval of BUFR messages
!
! Revision 1.17  98/05/15  10:06:34  10:06:34  usmdb (Generic MDB account)
! Removal of CLIMUK retrieval. Ammend element dataset
! retrieval warning.
!
! Revision 1.16  98/04/20  07:19:20  07:19:20  usmdb (Generic MDB account)
! Ammend CLIMUK warning
!
! Revision 1.15  98/03/12  08:53:32  08:53:32  usmdb (Generic MDB account)
! Add IVER argument to TFMRET to allow retrieval of
! NCM reports with preferred report flag options. Add
! TRANGE argument to SYNRET call to pass the START TIME
! sub-period
!
! Revision 1.14  1998/01/29 17:14:02  usmdb
! Addition of IBM preprocess directive.
!
! Revision 1.13  97/10/24  13:34:45  13:34:45  usjl (Jon Lewthwaite)
! Changed code relating to 29-09-97 as this caused IOBS to be
! incremented incorrectly.
!
! Revision 1.12  1997/09/24 14:29:26  uspm
! Removed code relating to 27-11-91 change - now redundant
! Changes to ICERET, CLMUK retrieval
!
! Revision 1.11  1997/09/10 15:32:26  uspm
! If the subtype is LNDSYN check for WMO_STTN_NAME in request string.
! If present pass a flag to SYNRET.
!
! Revision 1.10  1997/08/06 08:41:56  uspm
! Correct OPEN statement for COSMOS version.
!
! Revision 1.9  1997/08/04 13:14:24  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.8  1997/07/25 14:06:28  uspm
! Latest version from COSMOS
!
! Revision 1.6  1997/05/12 13:26:35  uspm
! Version dated 21-4-97 copied from COSMOS
!
! Revision 1.5  1997/04/07 12:43:08  uspm
! Add 'extra' $ after $Source:  to ensure following ident keyword
! found in .o , lib
!
! Revision 1.4  1997/02/28 14:35:29  uspm
! Latest version from COSMOS
!
! Revision 1.3  1997/02/27 12:13:06  uspm
! Latst version from COSMOS
!
! Revision 1.2  1997/02/12 14:00:20  uspm
! Put #ifdef #endif around open statement and filename
! definition as different
! for operating systems
!
! Revision 1.1  1997/02/11 16:39:01  uspm
! Initial revision
!
! 17-08-98 !C98 : Correction to retrieval that spans 2 or more datasets.
!               : If the user speicfies a START TIME that is not on the
!               : hour and the retrieval request spans more than 1
!               : dataset, some data is not retrieved from the datasets
!               : as whole hours are added to the original START TIME
!               : to make a new START TIME for the new dataset. Change
!               : code to add 1 minute to the END TIME to form the new
!               : START TIME, unless INCREMENT keyword is coded - S.Cox
!
! 20-07-98 !B98 : Extra keyword needed for retrieval of BUFR messages
!               : in the report text string - S.Cox
!
! 18-05-98 !A98 : Removal of CLIMUK retrieval. Ammend element dataset
!               : retrieval warning. It will now give the date 17th
!               : August 1998. Initialise IDTYPE=0 for STNMAS retrieval
!               : and add a check to return to user if IDTYPE (PLATFORM)
!               : is not set. This prevents an infinate loop - S.Cox
!
! 20-04-98  !Z  : Ammend CLIMUK warning. It will now give the date
!               : 18th May 1998 for the decommisioning of CLIMUK
!               : retrieval - S.Cox
!
! 16-03-98  !Y  : Add IVER argument to TFMRET to allow retrieval of
!               : NCM reports with preferred report flag options. Add
!               : TRANGE argument to SYNRET call to pass the START TIME
!               : sub-period - S.Cox/J.Norton
!
! 27-10-97  !X  : Change !W for ICERET caused an empty observation to
!               : be retrieved in addition to the normal observations
!               : as variable IOBS was getting incremented in two
!               : places. Repaired code - S.Cox
!
! 29-09-97  !W  : ICERET off-line rerieval does not work when data
!               : requested spans off-line datasets. Move ICERET code
!               : so IRECAL variable can get set. Also print warning
!               : messages to CLIMUK users telling them to move their
!               : retrievals over to MIDAS database. Also removed code
!               : relating to 27-11-91 change - now redundent - S.Cox
!
! 15-09-97  !V  : Check the user's requested in-line elements if the
!               : subtype is LNDSYN for WMO_STTN_NAME. If present, the
!               : user wants the WMO station name, so pass a flag to
!               : SYNRET saying so. - S.Cox
!
! 28-07-97  !U  : Changes to allow retrieval of observations by exact
!               : area and by rotated lat/lon area. Also add a
!               : warning message for users using element dataset
!               : selection - S.Cox.
!
! 21-07-97  !T  : Changes to allow retrieval from new merged data for
!               : UPPERAIR data. Increased size of ARRAY1, ARRAY2 and
!               : USRELM from 5000 to 12000. Also pass CRTYP to
!               : UPRRET - S.Cox
!
! 30-06-97  !S  : Increase CREQ dimension from *3000 to *5000 - S.Cox
!
! 21-04-97  !R  : Code changed to allow user to specify a data dict
!               : dataset name in the request string. There is a
!               : section of code in MDB to check this file exists
!               : and the dataset name is passed to several
!               : subroutines. Also changed call to VALREQ so that only
!               : the parameters actaully checked are passed - S.Cox
!
! 01-04-97  !Q  : Correct setting of Station master identifier arrays
!               : S.Cox
!
! 19-02-97  !P  : Addition of retrieval module BUSRET. This is an
!               : eventual replacement for SUBRET. It will allow
!               : in-line element retrieval for subtypes that prev-
!               : iously passed through SUBRET. Introduce a new
!               : variable NEWMDBCALL to tell retrieval routines that
!               : the user is making a new call - S.Cox
!
! 27-01-97  !O  : Addition of retrieval module SSMRET. This is for new
!               : retrieval subtype SSM-I, but can be used for all
!               : satellite subtypes, providing they are retrievable
!               : through in-line elements - S.Cox
!
! 25-11-96  !N  : Add an additional argument to UPRRET call (TRANGE)
!               : to pass the START TIME sub period. There has also
!               : an extra keyword (COMBINED) for Upper-Air retrieval.
!               : The number of keywords has been increased - S.Cox
!
! 07-10-96  !M  : Addition of subtypes TEMP (UPRRET), PILOT (UPRRET)
!               : and DROPSOND (UPRRET). The number of keywords has
!               : increased to 29 and the max number of elems the user
!               : can request increased fromm 500 to 5000 for Upper
!               : air retrieval. Also removed ENTRY MDBRT - S.Cox
!
! 16-09-96  !L  : Add an additional argument to ICERET call.
!
! 23-07-96  !K  : Remove references to search sequences. Remove call
!               : to ELMNAM because Fastrack retrieval now works from
!               : element numbers rather than names.  Return QCFLAG
!               : from EXPELM rather than pass the request string to
!               : a lower level. Add an extra argument to MAPELM and
!               : subtype calls to pass the count for each element
!               : (rather than NAME_n ).
!
! 19-07-96  !J  : Addition of subtype SEAICE.
!
! 15-07-96  !I  : Addition of subtypes LNDSYN (SYNRET) and AIREPS
!               : (AIRRET).
!               : change includes fast-retrieval.
!               : code to pad subtype removed - it did nothing!
!               : change CTYPE to SUBTYPE where applicable.
!               : pass ITYPE as well as SUBTYPE to RDELEM - S.Cox
!
! 31-05-96  !H  : Addition of new variable CRTYP which is set in
!               : subroutine DDICT. 'F' indicates that the dataset
!               : will allow fast-retrievals from it - S.Cox
!
! 04-03-96  !G  : Addition to allow elements dataset to be specified
!               : in the request string (needed for RPC work). also
!               : initialise CSTR and CREPT output strings for RPC.
!               : there is an INQUIRE and an OPEN statement in MDBRT
!               : associated with this change. there is also a non-
!               : portable element associated with the 1 file
!               : naming convention on the OPEN and INQUIRE
!               : statements- S.Cox.
!
!           !F  : Accept satellite idents under PLATFORM keyword
!
! 20-11-95  !E  : Change name from MDBRT to MDB (like shell),
!               : names of further retrieval modules to be same
!               : as shells, recompiling everything in FORTRAN 2.6
!               : (but leave MDBRT entry so that old shell will
!               : still work if load module has alias MDBRT.)
!
! 05-07-95  !D  : Increase length of CREQ from *500 to *3000 to
!               : overcome string constraints on in-line element
!               : selection. Modify subroutine call to DELSPCE in
!               : line with changes to delspce adding 'REQUEST' as
!               : an additional parameter  A.M
!
! 09-01-95  !C  : Set time tag if CLIMAT data requested to enable
!               : retrieval by month
!
! 28-11-94  !B  : Change to redefine end  time of DRIFTER
!               : retrieval when start and end times cross
!               : 31/10/94/1100z (change of dataset record length)
!
! 05-09-94  !A  : Send report string array CREPT(NOBS) to RTSHEL.
!
! 07-03-94      : Make sure start time has the right minutes value
!               : when returning to DDICT for another dataset.
!               : (and check ierr from RDELEM)
! 24-01-94      : Allow sub-periods and increments.
! 29-11-93      : Extra words on the end of identifier lists for
!               : stnmas surface or upair stations.
! 30-06-93      : Allow for times to the minute when incrementing
!               : for an off-line series.
! 22-05-93      : Short or long TAFS.
! 19-04-93      : Changes to DDICT to allow elements list to be
!               : returned. in-line element selection.
! 16-02-93      : Add a new retrieval path for 23 byte chained index
!               : type datasets.  re-organise decoding of request and
!               : validation.
! 10-02-93      : Allocate STNMAS datasets once only. main D/S on
!               : FT88. index is opened on FT89 in STNRET.
! 27-01-93      : Tidy up. correct allocation of associated datasets
!               : when user calls with different subtypes.
! 21-12-92      : Close STNMAS datasets at finish
! 23-10-92      : New subtype GLOSS going through satellite retrieval
! 09-09-92      : Check for fatal errors from satellite retrieval
!               : routine ie error in decode.
! 02-09-92      : If there is an error from the data-dictionary,set
!               : status to 8 for missing data rather than 16 for
!               : fatal error.
! 25-08-92      : Addition of STNMAS subtype to MDB
! 24-08-92      : New list of associated datasets for STNMAS
! 28-07-92      : Correct check for retrieval complete before going
!               : on to the next dataset in a series to allow for
!               : no data (8) as well as end of data (0).
! 09-06-92      : New keyword 'MESSAGE' to retrieve one BUFR msg at
!               : a time. (for internal SDB team use only - not
!               : documented in dmtn 3).
! 11-03-92      : Put DRIFTR through general retrieval module rather
!               : than DRFTRT.
! 02-03-92      : Get VOLSER from system catalogue if the dataset is
!               : catalogued on tape; use DSNCHU to amend JFCB.
! 14-01-92      : Optional character string as last argument, tidy
!               : up and take elements dataset section out to a new
!               : module, RDELEM.
! 13-01-92      : SDB.CLIMSYN.mapping from FT37 to FT86
! 27-11-91      : Amend SAT120 descriptors if retrieving data from
!               : before 91/11/21/21Z.  error in descriptors in
!               : database - 007001 should be 010001
!               :            010004 should be 007004
! 06-11-91      : Check for previous allocation
! 14-10-91      : Addition of DRIFTER subtype
! 02-10-91      : Close CLIMUK dataset.
! 08-08-91      : Get LQCFLG from elements dataset (and amend call
!               : to CLIMUK)
! 04-07-91      : Standardise error messages.
! 02-07-91      : Retry OPEN if error detected.
! 28-05-91      : Pass unit number to retrieval.
! 22-05-91      : CLOSE main D/S at the end of a CREQ
! 02-05-91      : Introduction of CLIMUK
! 26-03-91      : To test FT num rather than DSN in the INQUIRE
! 03-03-91      : Allow retrieval from cartridge as well as disk.
! 18-02-91      : Introduction of new code for ERS-1 data retrieval
! 04-10-90      : Introduced
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! declare integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER  AGFLAG
      INTEGER  ANERR
      INTEGER  ARRAY1(12000)                                        !T!M
      INTEGER  ARRAY2(12000)                                        !T!M
      INTEGER  ARRAY3(500)                                            !M
      INTEGER  ARRAY4(500)                                            !M
      INTEGER  FTNO
      INTEGER  I              !- general loop counter
      INTEGER  ICENT
      INTEGER  ICT(8)         !- for system time
      INTEGER  ICTMN2         !- end time century minutes           !C98
      INTEGER  ICTHR1
      INTEGER  ICTHR2
      INTEGER  IDATA(5)
      INTEGER  IDCNNL(50)
      INTEGER  IDESNO
      INTEGER  IDTYPE
      INTEGER  IERR
      INTEGER  IETIME(9)      !- Start & end time for 1 data set    !2.4
      INTEGER  IFAIL
      INTEGER  IFORM                                                  !N
      INTEGER  ILEN           !- length of request string
      INTEGER  IMAP(500)
      INTEGER  IMODEL                                                 !R
      INTEGER  IOBS
      INTEGER  IOVER
      INTEGER  IPOS
      INTEGER  IRAINL(50)
      INTEGER  IRECAL         !- 1 if more datasets to come
      INTEGER  IRECV(10)      !- receipt time
      INTEGER  IRM            ! Merged data flag (1=raw, 2=merged)  !2.4
      INTEGER  ISATID(10)     !- identifier list
      INTEGER  ISPOS
      INTEGER  ISTAT
      INTEGER  ISTYP
      INTEGER  ISTAN(50)
      INTEGER  TAFLEN !for short/long (A)TAFs; not report length  !1.26b
      INTEGER  METSPECI       !- METAR or SPECI specified         !1.26b
      INTEGER  ITIME(9)       !- User's data time window
      INTEGER  ITIMEK(2)      !- Temp. store for start/end 'hhmm'   !2.4
      INTEGER  ITIM1                                                  !B
      INTEGER  ITIM2                                                  !B
      INTEGER  ITOD
      INTEGER  ITYPE
      INTEGER  IVER
      INTEGER  IWMOL(50)
      INTEGER  J1             !- general loop counter
      INTEGER  K              !- general loop counter
      INTEGER  KNELEM
      INTEGER  KNOBS
      INTEGER  LAT_SUBSCRIPT
      INTEGER  LON_SUBSCRIPT
      INTEGER  NDES
      INTEGER  NELEM
      INTEGER  NHOUR          ! Hour of day (00 - 23)               !2.4
      INTEGER  NOBS
      INTEGER  NUM
      INTEGER  NUMTEMPS                                               !M
      INTEGER  NUMWINDS                                               !M
      INTEGER  RETSTA
      INTEGER  SELECT(50)     !- SELECT keyword values              !3
      INTEGER  STATUS
      INTEGER  TOTAL_OBS      !- total obs retrieved per subtype   !1.20
      INTEGER  TRANGE                                                 !N
      INTEGER  UAPART                                                 !M

!-----------------------------------------------------------------------
! declare real variables (in alphabetical order)
!-----------------------------------------------------------------------

      REAL     AREA(5)            !- lat/lon area                     !U
      REAL     RVALS(NOBS,NELEM)
      REAL     RPOLE(2)           !- Rotated POLE coords              !U

!-----------------------------------------------------------------------
! declare logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL  DDEXIST        !- TRUE if user's DDICT exists          !R
      LOGICAL  FIRSTIM        !- TRUE first time through
      LOGICAL  FOUND(35)      !- TRUE for keywords used             !2.8
      LOGICAL  HEADSET        !- TRUE if HEAD set                   !2.7
      LOGICAL  LATEST         !- TRUE for latest reports
      LOGICAL  LAT_FOUND                                              !U
      LOGICAL  LCHREP         !- TRUE if no elements wanted, only text
      LOGICAL  LMSG           !- TRUE to return data one msg at a time
      LOGICAL  LON_FOUND                                              !U
      LOGICAL  LQCFLG         !- TRUE for qc elements in CLIMUK
      LOGICAL  LTEST          !- TRUE for lots of output
      LOGICAL  NEWMDBCALL                                             !P
      LOGICAL  WANTSTNNAME    !- TRUE if user wants LNDSYN stn name   !V
      LOGICAL  WANTTEMPS      !- TRUE for UPRAIR temperatures         !M
      LOGICAL  WANTWINDS      !- TRUE for UPRAIR winds                !M

!-----------------------------------------------------------------------
! declare character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*36    ANAME(500)
      CHARACTER*120   CDSN                                          !2.3
      CHARACTER*48    CERR
      CHARACTER*4     CICAOL(50)
      CHARACTER*2     CM            ! 'CM' Current/Merge indicators !2.4
      CHARACTER*9     CIDENT(50)    !- list of identifiers
      CHARACTER*(*)   CREPT(NOBS)
      CHARACTER*1     CPOS          !- 'L'atest or 'A'rchive
      CHARACTER*5000  CREQ                                          !S!D
      CHARACTER*1     CRTYP                                           !H
      CHARACTER*(*)   CSTR(NOBS)
      CHARACTER*(*)   CTYPE
      CHARACTER*8     CTYPEK
      CHARACTER*80    DDICTNAME     !- users ddict REQUEST ds name    !R
      CHARACTER*8     ELIST         !- element index member name    !2.9
      CHARACTER*132   HEAD          !- revision information
      CHARACTER*8     LAST          ! Last 'LIST' processed         !2.4
      CHARACTER*8     LIST          ! Name of list of element names !2.4
      CHARACTER*3     MSTREAM       !- MASS stream (minus MDB pfx) !2.7b
      CHARACTER*1     ORDER         !- 'B'ackwards or 'F'orwards
      CHARACTER*(*)   REQUEST
      CHARACTER*8     SUBTYPE
      CHARACTER*36    USRELM(12000)                                 !T!M

!-----------------------------------------------------------------------
! declare functions
!-----------------------------------------------------------------------

      INTEGER DT2HRS !- convert date/time to century hours

!-----------------------------------------------------------------------
! dynamic common
!-----------------------------------------------------------------------

      COMMON/MDBDC1/USRELM,ANAME,CIDENT,CICAOL                      !4
      COMMON/MDBDC2/ARRAY1,ARRAY2,ARRAY3,ARRAY4,IWMOL,IDCNNL,
     &              IRAINL,IMAP,ISTAN

!-----------------------------------------------------------------------
! Data initialisation
!-----------------------------------------------------------------------

      DATA CM/'CM'/                                                 !2.4
      DATA LAST/' '/                                                !2.4
      DATA HEADSET/.FALSE./                                         !2.7

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Revision information
!-----------------------------------------------------------------------

      IF (.NOT.HEADSET) THEN                                        !2.7
        HEADSET=.TRUE.                                              !2.7
        HEAD='$RCSfile: mdb.F,v $ ' //
     &       '$Revision: 4$ $Date: 07/04/2009 16:04:57$'
      ENDIF                                                         !2.7

!-----------------------------------------------------------------------
! !G S.Cox 04/03/96. initialise output strings for RPC
! work. if this isn't done, C will not interpret hexadeciamal 0
! correctly as the system initialises the "blanks".
!-----------------------------------------------------------------------

      DO I = 1,NOBS
         CSTR(I)(:)=' '
         CREPT(I)(:)=' '
      ENDDO

!-----------------------------------------------------------------------
! If called with ISTAT=99, return immediately. ISTAT=99 is used for
! the MetDB RPC killserver call, but may be present at the end of users
! code to call the MDB directly. Allow this so users can have the
! same code for RPC and non-RPC MetDB calls.                        !2.7
!-----------------------------------------------------------------------

      IF (STATUS.EQ.99) RETURN                                      !2.7

!-----------------------------------------------------------------------
! skip to recall if this is not the 1st call!
!-----------------------------------------------------------------------

      IOBS=0
      IF (STATUS.EQ.4 .OR. STATUS.EQ.12) THEN
        SUBTYPE = CTYPEK
        NOBS    = KNOBS
        NELEM   = KNELEM
        GOTO 500
      ENDIF

!=======================================================================
! initialise first time through  (GOTO 500 above means all code
!                                 till 500 is first time only)
!=======================================================================

      IDESNO     = 0
      STATUS     = 0
      ITIM1      = 0                                                  !B
      ITIM2      = 0                                                  !B
      FIRSTIM    = .TRUE.
      NEWMDBCALL = .TRUE.                                             !P
      TOTAL_OBS  = 0                                               !1.20
      WANTSTNNAME = .FALSE.                                           !V

!-----------------------------------------------------------------------
! get current date/time
!-----------------------------------------------------------------------

#if defined (MVS)
      CALL DATIM(ICT)
#else
      CALL METDB_DATIM(ICT)
#endif

!-----------------------------------------------------------------------
! change STAFS and LTAFS to the general subtype TAFS (for use by
! RTABLE & MDBALC, i.e. both kinds of TAF are in the same dataset)
!-----------------------------------------------------------------------

      TAFLEN=0    !both short and long (A)TAFs                    !1.26b
      METSPECI=0  !METARs (not SPECIs)                            !1.26b

      IF (CTYPE(1:5).EQ.'STAFS') THEN
        SUBTYPE='TAFS'
        TAFLEN=2                                                  !1.26b
      ELSEIF (CTYPE(1:5).EQ.'LTAFS') THEN
        SUBTYPE='TAFS'
        TAFLEN=1                                                  !1.26b

!-----------------------------------------------------------------------
! change SATAFS and LATAFS to the general subtype ATAFS (for use by
! RTABLE & MDBALC, i.e. both kinds of TAF are in the same dataset)
!-----------------------------------------------------------------------

      ELSEIF (CTYPE(1:6).EQ.'SATAFS') THEN                         !1.21
        SUBTYPE='ATAFS'                                            !1.21
        TAFLEN=2                                            !1.26b !1.21
      ELSEIF (CTYPE(1:6).EQ.'LATAFS') THEN                         !1.21
        SUBTYPE='ATAFS'                                            !1.21
        TAFLEN=1                                            !1.26b !1.21

!-----------------------------------------------------------------------
! change SPECI to the general subtype METARS (for use by
! RTABLE & MDBALC, i.e. both METAR and SPECI are in the same dataset)
!-----------------------------------------------------------------------

      ELSEIF (CTYPE(1:5).EQ.'SPECI') THEN                   !1.26b !1.21
        SUBTYPE='METARS'                                    !1.26b !1.21
        METSPECI=3                                          !1.26b !1.21
      ELSE
        SUBTYPE=CTYPE
      ENDIF

      CTYPEK=SUBTYPE

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

      CALL DELSPCE(CREQ,ILEN,REQUEST)                                 !D

!-----------------------------------------------------------------------
! initialise request variables
!-----------------------------------------------------------------------

      CALL SETDEF(ITIME,TRANGE,IRECV,IFORM,LATEST,ISTAN,ISATID,AREA,
     &            CIDENT,IOVER,IVER,ORDER,LTEST,LMSG,ISTYP,         !2.0
     &            UAPART,DDICTNAME,IMODEL,RPOLE,IDTYPE,SELECT)      !2.8

!-----------------------------------------------------------------------
! decode request up to keyword ELEMENTS
!-----------------------------------------------------------------------

      CALL DATE31(ICT(6),ICT(7),ICT(8),ICENT)

      CALL GETREQ(SUBTYPE,CREQ,ILEN,ITIME,TRANGE,IRECV,IFORM,LATEST,
     &            ISTAN,ISATID,AREA,CIDENT,ORDER,IOVER,IDTYPE,IVER,
     &            ICENT,LTEST,LMSG,FOUND,ITOD,ISTYP,IFAIL,CERR,
     &            UAPART,DDICTNAME,IMODEL,RPOLE,SELECT)             !2.8

      IF (IFAIL.EQ.8) THEN
        IERR=1
        GOTO 998
      ENDIF

!-----------------------------------------------------------------------
! adjust day and hour if CLIMAT data requested because time tag       !C
! for this data type handled differently ie by the month              !C
!-----------------------------------------------------------------------

      IF (SUBTYPE(1:6).EQ.'CLIMAT') THEN                            !2.3
        ITIME(3) = 14 + MOD(ITIME(2),4)                               !C
        ITIME(4) = 0                                                  !C
      ENDIF

!-----------------------------------------------------------------------
! If diagnostics switched on (LTEST = TRUE) output those returned from
! the GETREQ call
!-----------------------------------------------------------------------

      IF (LTEST) THEN
        WRITE(*,'(/1X,''In MetDB subroutine MDB after GETREQ'' )')
        WRITE(*,'( 1X,''====================================''/)')

        WRITE(*,'(1X,''In MDB: ITIME  = '',10(1X,I5))')ITIME
        WRITE(*,'(1X,''In MDB: IRECV  = '',10(1X,I5))')IRECV
        WRITE(*,'(1X,''In MDB: AREA  = '',10(1X,F8.3))')AREA       !1.24
        WRITE(*,*)'in MDB: TRANGE     = ',TRANGE
        WRITE(*,*)'In MDB: IFORM      = ',IFORM
        WRITE(*,*)'In MDB: IOVER      = ',IOVER
        WRITE(*,*)'In MDB: IVER       = ',IVER
        WRITE(*,*)'In MDB: IDTYPE     = ',IDTYPE
        WRITE(*,*)'In MDB: IMODEL     = ',IMODEL                      !U
        WRITE(*,*)'In MDB: ISTYP      = ',ISTYP
        WRITE(*,*)'In MDB: SELECT:'                                 !2.8
        WRITE(*,'(20(1X,I3))')SELECT                                !2.8
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
      ENDIF

!-----------------------------------------------------------------------
! Before we call RTABLE to get ITYPE, check to see if the user has
! coded a retrieval table name in the request string. If so, check to
! see if the data set exists. If not return with an error.            !R
!-----------------------------------------------------------------------

      IF (FOUND(31)) THEN
#if defined (MVS)
        INQUIRE (FILE=DDICTNAME(1:),EXIST=DDEXIST)  !- for IBM 1
#else
        INQUIRE (FILE=DDICTNAME(2:),EXIST=DDEXIST)
#endif
        IF (.NOT.DDEXIST) THEN
          WRITE(6,'(/2X,75(''*''))')
          WRITE(6,'(2x,A,A)')'Returning to MDB calling program. ',
     &    'Data Dict name specified in CREQ'
          WRITE(6,'(2X,''does not exist. Check name '',A)')DDICTNAME(2:)
          WRITE(6,'(2X,75(''*'')/)')
          IERR=1
          CERR='See info above'
          GOTO 998
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! Copy and validate request times (doesn't validate that much !!)   !2.4
!-----------------------------------------------------------------------

      ITYPE = 0                                                     !2.4
      IF (SUBTYPE.EQ.'STNMAS  ') ITYPE = 1                          !2.4
      CALL VALREQ(ITYPE,FOUND,ITIME,IRECV,ICT,ITOD,IFAIL,CERR)    !2.0!R

      IF (IFAIL.EQ.8) THEN
        IERR=1
        GOTO 998
      ELSEIF (IFAIL.EQ.4) THEN
        WRITE(*,*)'MDB WARNING:'//CERR
      ENDIF
!                                             Copy ITIME to IETIME  !2.4
      DO I=1,9                                                      !2.4
        IETIME(I) = ITIME(I)                                        !2.4
      END DO ! I                                                    !2.4

!-----------------------------------------------------------------------
! Loop over datasets needed to satisfy request.
! RTABLE allocates one at a time.                                   !2.4
! Condition code may say more to come, so come back to here.        !2.4
!-----------------------------------------------------------------------

 997  CONTINUE                                                      !2.4
      IRM = IFORM                                                   !2.4
      CALL RTABLE (SUBTYPE, IRM, IMODEL, DDICTNAME, LTEST,          !2.4
     &             IETIME, CDSN, IDATA, LIST, IERR, CERR,          !2.7b
     &             MSTREAM, ELIST)                                  !2.9
      ITYPE = IDATA(4)                                              !2.4
      CRTYP = CM(IRM:IRM)                                           !2.4

!-----------------------------------------------------------------------
! Check for errors from RTABLE; 16 means no data set, 4 means more data
! sets to come, so set a flag to come back when this data set is done,
! and adjust the end times for this data set.
!-----------------------------------------------------------------------

      IF (IERR.EQ.4) THEN
        IRECAL=1
      ELSE IF (IERR.GT.4) THEN
        NOBS=0                                                     !1.22
        STATUS=8
        GOTO 998
      ENDIF

!-----------------------------------------------------------------------
! Skip elements check and call to MDBALC if retrieving GRIB data.  !1.26
!-----------------------------------------------------------------------

      IF (ITYPE.EQ.10) GO TO 500                                   !1.26

!-----------------------------------------------------------------------
! Call READLIST to read the required list of element names and map
! numbers if it was not the last one read.
!-----------------------------------------------------------------------

      IF (LIST.NE.LAST) THEN                                        !2.4
        CALL READLIST (LIST, LTEST, ANAME, IMAP, NDES, IERR, CERR)  !2.4
        LAST = LIST                                                 !2.4
      END IF                                                        !2.4

      IF (LTEST) THEN
        WRITE(*,*)'In MDB: CRTYP = ',CRTYP
        WRITE(*,*)
        WRITE(*,*)'In MDB: Elements list, NDES = ',NDES
        WRITE(*,'(1X,A36,I8)')(ANAME(K),IMAP(K),K=1,NDES)
      ENDIF

!-----------------------------------------------------------------------
! Find string ' ELEMENTS ' in the request string.
!-----------------------------------------------------------------------

      IF (FIRSTIM) THEN
        IF (LTEST) WRITE(*,*)'MDB: Looking for elements'           !1.24
        I=INDEX(CREQ,' ELEMENTS ')

!-----------------------------------------------------------------------
! If string found move to end of ' ELEMENTS ' (I+10)
!-----------------------------------------------------------------------

        IF (I.NE.0) THEN
          IF (LTEST) WRITE(*,*)'MDB: Elements keyword found'       !1.24
          IPOS=I+10

!-----------------------------------------------------------------------
! For retrieval from inline list, put unique element names in array
! USRELEM and correct any misspellings (from DTMN3 or aliases)
!-----------------------------------------------------------------------

          IF (LTEST) WRITE(*,*)'MDB: Calling EXPELM'               !1.24

          CALL EXPELM(CREQ,IPOS,ILEN,USRELM,NUM,LQCFLG,IFAIL,CERR) !1.24

          IF (LTEST) WRITE(*,*)'MDB: After EXPELM: NUM,LQCFLG = ', !1.24
     &               NUM,LQCFLG                                    !1.24

          IF (IFAIL.EQ.8) THEN                                     !1.24
            IERR=1                                                 !1.24
            GOTO 998                                               !1.24
          ENDIF                                                    !1.24

          CALL ALSELM(SUBTYPE,USRELM,NUM,LTEST)                    !1.24

          CALL MAPELM(USRELM,NUM,ANAME,IMAP,NDES,ARRAY1,       !2.0!1.24
     &                IDESNO,ARRAY2,LTEST)                     !2.0!1.24

          IF (LTEST) THEN                                          !1.24
            WRITE(*,*)'MDB: After MAPELM: IDESNO = ',IDESNO        !1.24
            WRITE(*,'(10I6)')(ARRAY1(I),ARRAY2(I),I=1,IDESNO)      !1.24
          ENDIF                                                    !1.24

!-----------------------------------------------------------------------
! Test for AREA and Lat Long elements requested if ITYPE=2, 3, 4 or 9.
! If Lat and Long requested obtain subscripts from ARRAY1 from MAPELM
!-----------------------------------------------------------------------

          IF (ITYPE.EQ.2 .OR. ITYPE.EQ.3 .OR.                       !2.1
     &        ITYPE.EQ.4 .OR. ITYPE.EQ.9) THEN                      !2.1
            IF (FOUND(7)) THEN                                     !1.24
              DO I=1,NUM                                           !1.24
                IF (USRELM(I) .EQ. 'LTTD') THEN                    !1.24
                  LAT_SUBSCRIPT=I                                  !1.24
                  LAT_FOUND=.TRUE.                                 !1.24
                ELSEIF (USRELM(I) .EQ. 'LNGD') THEN                !1.24
                  LON_SUBSCRIPT=I                                  !1.24
                  LON_FOUND=.TRUE.                                 !1.24
                ENDIF                                              !1.24
              ENDDO                                                !1.24

!-----------------------------------------------------------------------
! If Lat/Long not found then print a warning message to the user
! and abandon retrieval of data                                       !U
!-----------------------------------------------------------------------

              IF ((.NOT. LAT_FOUND) .OR. (.NOT. LON_FOUND)) THEN   !1.24
                WRITE(*,*)'MDB ERROR: PLEASE RESPECIFY YOUR'       !1.24
                WRITE(*,*)'REQUEST AREA LAT/LONG BOX SPECIFIED'    !1.24
                WRITE(*,*)'BUT EITHER LAT OR LONG OR BOTH'         !1.24
                WRITE(*,*)'LAT AND LONG ARE MISSING FROM YOUR'     !1.24
                WRITE(*,*)'REQUESTED ELEMENTS'                     !1.24
                IFAIL=8                                            !1.24
              ENDIF  !- lat_found, lon_found                       !1.24
            ENDIF  !- found(7)                                     !1.24
          ENDIF  !- itype                                          !1.24

          IF (IFAIL .EQ. 8) THEN                                   !1.24
            IERR=1                                                 !1.24
            GOTO 998                                               !1.24
          ENDIF                                                    !1.24

!-----------------------------------------------------------------------
! Test for WMO_STTN_NAME in USRELM array if the subtype is LNDSYN.    !V
!-----------------------------------------------------------------------

          IF (SUBTYPE(1:6).EQ.'LNDSYN') THEN                       !1.24
            DO I=1,NUM                                             !1.24
              IF (USRELM(I).EQ.'WMO_STTN_NAME') WANTSTNNAME=.TRUE. !1.24
            ENDDO                                                  !1.24
          ENDIF                                                    !1.24

!-----------------------------------------------------------------------
! set LCHREP TRUE if only report text needed (if no keyword 'ELEMENTS'
! or no element names found after it, or it's the only element
! requested).                                                         !K
!-----------------------------------------------------------------------

          LCHREP = (IDESNO.EQ.1 .AND. ARRAY1(1).EQ.65250)          !1.24

        ELSE  ! elements keyword not given - default report text only

          IPOS=1                                   ! S.Cox 8.5.96     !I
          LCHREP=.TRUE.
          ARRAY1(1)=65250
          IDESNO=1
        ENDIF

        FIRSTIM=.FALSE.
      ELSE       !- if not first time, remap elements for next data set
        IF (.NOT.LCHREP) THEN
          CALL MAPELM(USRELM,NUM,ANAME,IMAP,NDES,ARRAY1,            !2.0
     &                IDESNO,ARRAY2,LTEST)                        !2.0!K
        ENDIF
      ENDIF

      IF (LTEST) THEN
        WRITE(6,1010)
     &      (ARRAY1(K),ARRAY3(K),ARRAY2(K),ARRAY4(K),K=1,IDESNO)
1010    FORMAT(' DESCRIPTORS  SEQUENCE  REPLICATION  SIGNIFICANCE'/
     &          (I8,6X,I5,8X,I3,10X,I5))
      ENDIF

!-----------------------------------------------------------------------
! allocate required datasets
!-----------------------------------------------------------------------

      FTNO=IDATA(3)   ! KEEP TO CLOSE LATER

      CALL MDBALC(SUBTYPE,IDATA,CDSN,IFAIL,CERR,LTEST,DDICTNAME,
     &            MSTREAM)                                         !2.7b

      IF (IFAIL.EQ.8) THEN
        IERR=1
        GOTO 998
      ENDIF

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
! STNINDX. Now loop over all 49 identifiers in ISTAN array            !Q
!-----------------------------------------------------------------------

      IF (ITYPE.EQ.1) THEN

         IF (IDTYPE.EQ.1) THEN      ! wmo identifiers
           DO J1=1,49                                                 !Q
             IWMOL(J1)=ISTAN(J1+1)
             CICAOL(J1)='    '
             IDCNNL(J1)=-32768
             IRAINL(J1)=-32768
           ENDDO
         ELSEIF (IDTYPE.EQ.2) THEN  ! icao identifiers
           DO J1=1,50               ! no count from getreq, so use max
             IWMOL(J1)=-32768
             CICAOL(J1)=CIDENT(J1)(1:4)
             IDCNNL(J1)=-32768
             IRAINL(J1)=-32768
           ENDDO
         ELSEIF (IDTYPE.EQ.3) THEN  ! dcnn identifiers
           DO J1=1,49                                                 !Q
             IWMOL(J1)=-32768
             CICAOL(J1)='    '
             IDCNNL(J1)=ISTAN(J1+1)
             IRAINL(J1)=-32768
           ENDDO
         ELSEIF (IDTYPE.EQ.4) THEN  ! rainfall identifiers
           DO J1=1,49                                                 !Q
             IWMOL(J1)=-32768
             CICAOL(J1)='    '
             IDCNNL(J1)=-32768
             IRAINL(J1)=ISTAN(J1+1)
           ENDDO
         ELSE                                                       !A98
           IERR=1                                                   !A98
           CERR='NO PLATFORM SPECIFIED FOR STNMAS RETRIEVAL'        !A98
           GOTO 998                                                 !A98
         ENDIF

         IF (IVER.EQ.1) THEN
           CPOS='L'      ! LATEST
         ELSE
           CPOS='A'      ! archive
         ENDIF

         ISTAT=STATUS

         CALL STMRET(IWMOL,CICAOL,IDCNNL,IRAINL,IDTYPE,CPOS,ISTYP,
     &               ISTAT,RVALS,NOBS,NELEM,ARRAY1,CSTR,CERR)         !E

         IF (ISTAT.LT.16) THEN
           STATUS=ISTAT
         ELSE
           STATUS=16
         ENDIF

!=======================================================================
!
! GRIBRET for TCRTEMP and other GRIB products.                     !1.26
! (Check that 'NELEM' has been specified as 1 for ITYPE 10.)       !1.26
!
!=======================================================================

      ELSE IF (ITYPE.EQ.10) THEN                                   !1.26
!                                                     Check NELEM  !1.26
        IF (NELEM.NE.1) THEN                                       !1.26
          WRITE (*,*) 'MDB ERROR: ',                               !1.26
     &      '"NELEM" MUST BE SPECIFIED AS 1 FOR GRIB RETRIEVALS.'  !1.26
          IERR = 1                                                 !1.26
          GO TO 998                                                !1.26
        END IF                                                     !1.26
!                                                  GRIB retrieval  !1.26
        CALL GRIBRET (SUBTYPE, CDSN, NOBS, IDATA,                  !1.26
     &                FOUND, LTEST, SELECT, IETIME, STATUS,         !2.8
     &                CREPT, RVALS)                                 !2.4
        RETURN                                                     !1.26

!=======================================================================
!
! check for retrieval across data sets: either tell user more data to
! come or carry on filling array from new data set.
!
!=======================================================================

      ELSE
        IF (AGFLAG.EQ.1 .AND. NOBS.EQ.IOBS) THEN
          STATUS=4
          GOTO 999
        ELSEIF (AGFLAG.EQ.1 .AND. NOBS.NE.IOBS) THEN
          AGFLAG=0
          STATUS=16
        ENDIF
        IOBS=IOBS+1
        RETSTA=STATUS

!-----------------------------------------------------------------------
! SSMRET (type 4) requires hours only, not hours & minutes         !2.10
!-----------------------------------------------------------------------

        IF (ITYPE.EQ.4) THEN                                       !2.10
          ITIMEK(1) = IETIME(4)        ! Store hours & minutes      !2.4
          ITIMEK(2) = IETIME(8)        ! for replacement later      !2.4
          IETIME(4) = IETIME(4)/100    ! 'hhmm' to 'hh'             !2.4
          IETIME(8) = IETIME(8)/100    ! 'hhmm' to 'hh'             !2.4
        END IF

!=======================================================================
!
! NEW-FORMAT SATELLITE STORAGE                                      !2.1
!
!=======================================================================

        IF (ITYPE.EQ.2) THEN                                        !2.1

          CALL BUFRET(IETIME,IRECV,AREA,CIDENT,SELECT,IOVER,
     &                RVALS,NOBS,NELEM,IOBS,ARRAY1,IDESNO,ARRAY2,
     &                RETSTA,ANERR,IDATA,LMSG,LTEST,LQCFLG,CSTR,
     &                CREPT,SUBTYPE,NEWMDBCALL,LAT_SUBSCRIPT,
     &                LON_SUBSCRIPT,RPOLE,FOUND,ELIST)              !2.9

!=======================================================================
!
! AMDARS, DRIFTR, WINPRO, BATHY, BUOY, TESAC, SATOB
!
!=======================================================================

        ELSE IF (ITYPE.EQ.3) THEN                                   !2.1

          CALL BUSRET(SUBTYPE,IETIME,IRECV,AREA,CIDENT,RVALS,NOBS,  !2.4
     &                NELEM,IOBS,ARRAY1,IDESNO,ARRAY2,RETSTA,ANERR,
     &                IDATA,CSTR,CREPT,LCHREP,LTEST,LQCFLG,NEWMDBCALL,
     &                LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,LMSG,
     &                FOUND,ELIST)                                  !2.9

!=======================================================================
!
! SAT120, GLOSS, UKMMWI, ESAUWA, ESAURA, RALUAT, SSM-I                !O
!
!=======================================================================

        ELSEIF (ITYPE.EQ.4) THEN

!-----------------------------------------------------------------------
! if satellite idents have been set under 'PLATFORM' keyword, convert
! the character strings to numbers in array input to SATRET.          !F
!-----------------------------------------------------------------------

          IF (ISATID(1).EQ.0 .AND. CIDENT(1)(1:5).NE.'00000') THEN    !F
            DO J1=1,10                                                !F
              IF (CIDENT(J1)(1:5).NE.'00000') THEN                    !F
                ISPOS=INDEX(CIDENT(J1),' ')                           !F
                READ (CIDENT(J1)(1:ISPOS-1),*) ISATID(J1)             !F
              ENDIF                                                   !F
            ENDDO                                                     !F
          ENDIF                                                       !F

          CALL SSMRET(IETIME,IRECV,AREA,ISATID,IOVER,RVALS,NOBS,    !2.4
     &                NELEM,IOBS,ARRAY1,IDESNO,ARRAY2,RETSTA,ANERR,
     &                IDATA,LMSG,LTEST,LQCFLG,CSTR,CREPT,SUBTYPE,
     &                NEWMDBCALL,LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,
     &                FOUND,ELIST)                                  !2.9

!=======================================================================
!
! TAFS, ATAFS, METARS, SAMOSX, CLIMAT
!
!=======================================================================

        ELSEIF (ITYPE.EQ.5) THEN

          CALL TFMRET(SUBTYPE,IETIME,TRANGE,IRECV,AREA,CIDENT,      !2.4
     &                RVALS,NOBS,NELEM,IOBS,ARRAY1,IDESNO,          !2.0
     &                RETSTA,ANERR,IDATA,CSTR,CREPT,                !2.0
     &                LTEST,LCHREP,ORDER,LATEST,TAFLEN,METSPECI,  !1.26b
     &                FOUND,RPOLE,IVER)                            !1.24

!=======================================================================
!
! SYNRET for SHIPS, SYNOPS and AIREPS
!
!=======================================================================

        ELSEIF (ITYPE.EQ.6) THEN                                    !P!K

          CALL SYNRET(SUBTYPE,IETIME,IRECV,AREA,CIDENT,             !2.4
     &                RVALS,NOBS,NELEM, IOBS,ARRAY1,IDESNO,ARRAY2,
     &                RETSTA,ANERR,IDATA,CSTR,CREPT,LTEST,LCHREP,ORDER,
     &                LATEST,LQCFLG,FOUND,IVER,NEWMDBCALL,RPOLE,
     &                WANTSTNNAME,TRANGE,ELIST)                     !2.9

!=======================================================================
!
! SEAICE and TROPADV                                                  !X
!
!=======================================================================

        ELSEIF (ITYPE.EQ.8) THEN

          CALL ICERET(IETIME,RVALS,NOBS,NELEM,IOBS,ARRAY1,          !2.4
     &                IDESNO,CIDENT,RETSTA,ANERR,IDATA,CSTR,CREPT,
     &                LCHREP,ORDER,FOUND,IVER)                        !L

!=======================================================================
!
! UPRRET (TEMP, PILOT, DROPSOND)                                      !M
!
!=======================================================================

        ELSEIF (ITYPE.EQ.9) THEN                                      !M
                                                                      !M
          CALL CHK4TW(ARRAY1,ARRAY2,IDESNO,WANTTEMPS,WANTWINDS,       !M
     &                NUMTEMPS,NUMWINDS,LTEST)                        !M
                                                                      !M
          IF ((WANTTEMPS.AND.WANTWINDS) .AND.                         !M
     &        (NUMTEMPS.NE.NUMWINDS)) THEN                            !M
            IERR=1                                                    !M
            CERR='SEE MESSAGE ABOVE'                                  !M
            WRITE(6,'(/1X,''MDB: Cannot do UPRAIR retrieval.'')')  !1.24
            WRITE(6,'( 1X,''You are requesting temperature and '')')  !M
            WRITE(6,'( 1X,''wind level data but the replication'')')  !M
            WRITE(6,'( 1X,''counts given differ - NOT PERMITTED!'')') !M
          ELSE                                                        !M
            CALL UPRRET(SUBTYPE,LTEST,RETSTA,IDATA,IETIME,IRECV,    !2.4
     &                  FOUND,UAPART,CIDENT,AREA,ANERR,IOVER,IVER,
     &                  ARRAY1,IDESNO,ARRAY2,LQCFLG,RVALS,NOBS,
     &                  NELEM,CSTR,CREPT,IOBS,WANTTEMPS,WANTWINDS,
     &                  NUMTEMPS,NUMWINDS,TRANGE,NEWMDBCALL,
     &                  CRTYP,LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,
     &                  ELIST)                                      !2.9
          ENDIF                                                       !M

!=======================================================================
!
! RETURN IF RETRIEVAL IMPOSSIBLE OR ERROR IN RETRIEVAL
!
!=======================================================================

        ELSE
          CERR='UNABLE TO DO RETRIEVAL FOR THIS SUBTYPE'
          IERR=1
          GOTO 998
        ENDIF

!-----------------------------------------------------------------------
! check for error from retrieval first
!-----------------------------------------------------------------------

        IF (ANERR.EQ.16) THEN
          CERR='ABNORMAL TERMINATION'
          IERR=1
          GOTO 998
        ENDIF

!-----------------------------------------------------------------------
! set status if no errors
!-----------------------------------------------------------------------

        IF (RETSTA.LE.8) STATUS=RETSTA                              !2.4

!-----------------------------------------------------------------------
! If using SSMRET (type 4) restore hours & minutes                 !2.10
!-----------------------------------------------------------------------

        IF (ITYPE.EQ.4) THEN                                       !2.10
          IETIME(4) = ITIMEK(1)                                     !2.4
          IETIME(8) = 100*IETIME(8) + 59                            !2.4
        END IF                                                      !2.4

!-----------------------------------------------------------------------
! If this call is complete, check for more datasets
!-----------------------------------------------------------------------

        IF (STATUS.EQ.0 .OR. STATUS.EQ.8) THEN
          IF (IRECAL.EQ.1) THEN
            CLOSE(FTNO)
!                                                     Get century hours
            NHOUR = IETIME(4)/100                                   !2.4
            ICTHR1=DT2HRS(IETIME(1),IETIME(2),IETIME(3),NHOUR)      !2.4
            NHOUR = IETIME(8)/100                                   !2.4
            ICTHR2=DT2HRS(IETIME(5),IETIME(6),IETIME(7),NHOUR)      !2.4

!                                       Increment to get new start time
            IF (FOUND(4)) THEN                                      !2.4
!                             Add the smallest multiple of increment to
!                               start time which gets you past end time
              ICTHR1 = ICTHR1 +                                     !2.4
     &                 IETIME(9)*((ICTHR2-ICTHR1)/IETIME(9) + 1)    !2.4
              ICTMN2=ICTHR1*60+MOD(IETIME(4),100)                  !2.11
            ELSE
              ICTMN2=ICTHR2*60+MOD(IETIME(8),100)                   !C98
              ICTMN2=ICTMN2+1                                       !C98
              ICTHR1=ICTMN2/60                                      !C98
            ENDIF
!                                     Put new start time in IETIME(1-4)

            CALL HRS2DT(IETIME(1),IETIME(2),IETIME(3),NHOUR,ICTHR1) !2.4
            IETIME(4) = 100*NHOUR + MOD(ICTMN2,60)                  !2.4
            IF (LTEST) WRITE(*,*)'In MDB: MDB new start time = ',IETIME

!                                Restore user's end time in IETIME(5-8)
            IETIME(5)=ITIME(5)                                      !2.4
            IETIME(6)=ITIME(6)                                      !2.4
            IETIME(7)=ITIME(7)                                      !2.4
            IETIME(8)=ITIME(8)                                      !2.4

!-----------------------------------------------------------------------
! Go back to RTABLE call for another data set
!-----------------------------------------------------------------------

            IRECAL=0
            AGFLAG=1
            IF (LTEST) WRITE(*,*)'In MDB: Back for next dataset'
            GOTO 997
          ELSE
            CLOSE(FTNO)
          ENDIF
          NOBS=IOBS
        ELSEIF (LMSG) THEN
          NOBS=IOBS
        ENDIF
      ENDIF
      GOTO 999

!-----------------------------------------------------------------------
! print error messages
!-----------------------------------------------------------------------

 998  IF (IERR.EQ.1) THEN
        WRITE(6,*)' MDB ERROR: ',CERR
        STATUS=16
      ELSEIF (IERR.GT.1) THEN
        WRITE(6,*)' MDB ERROR: ',IERR,CERR
      ENDIF

 999  CONTINUE

      TOTAL_OBS = TOTAL_OBS + NOBS                                 !1.20
      IF (STATUS.EQ.8 .AND. TOTAL_OBS.GT.0) STATUS = 0             !1.20

      RETURN
      END
