      SUBROUTINE UPRRET(CTYPE,LFLAG,ISTAT,IDSK,ITIME,IRTIM,FOUND,
     &                  UAPART,CIDENT,AREA,IERR,IOVER,VER,IDESC,      !J
     &                  NDES,IREPL,QCREQ,ARRAY,NOBS,NELEM,CSTR,
     &                  CREPRT,IOBS,WANTTEMPS,WANTWINDS,NUMTEMPS,
     &                  NUMWINDS,TRANGE,NEWMDBCALL,CRTYP,           !I!E
     &                  LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,ELIST)    !2.4

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
!               : BITINDX   - to get BUFR displacements               !I
!               : CHECKPOSITIONS - see if room left in users array
!               : DT2HRS    - to convert date/time to century hours
!               : HRS2DT    - to convert century hours to date/time
!               : MAPRD     - to read map, index, data blocks
!               : READIDX   - to read element idex from ds
!               : IDMTCH    - to match index entry identifiers
!               : ROTAREA   - to rotate a lat lon                     !J
!               : SORTCH    - to sort index entries into order.
!               : SUBPER    - to select INCREMENT data.
!               : UPRPARTS  - to combine parts into 1 profile
!               : UPRWINDS  - to combine & sort TEMP & PILOT winds !1.21
!               : UPREPID   - to get the reports identity
!               : VALAREA   - to check ob in users lat lon area       !J
!               : VALARR    - to put values in users array
!               : VALUE     - to extract a value from a BUFR message  !I
!               : VALUSR    - to put values in users array            !I
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
! CRTYP         : character (ip) : type of data to retrieve C or M    !I
! LAT_SUBSCRIPT : integer   (ip) : users lat subscript                !J
! LON_SUBSCRIPT : integer   (ip) : users lon subscript                !J
! RPOLE         : real      (ip) : rotated pole                       !J
! ELIST         : char*8    (ip) : Element index member name        !2.4
!
! Revision info :
!
! $Workfile: uprret.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 21/12/2006 14:45:19$
!
! Change record :
!
! $Log:
!  2    Met_DB_Project 1.1         21/12/2006 14:45:19    Brian Barwell
!       Rempve premature jump out of index entry loop.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:53    Sheila Needham  
! $
! Revision 2.4  2003/05/02 15:28:59  usmdb
! Element index member name ELIST passed in and passed to READIDX.
! Initialise BUFR & SEVENS in ASCII which avoids calling EB2ASC - S.Cox.
!
! Revision 2.3  2003/02/03  15:17:46  15:17:46  usmdb (MetDB account c/o usjh)
! Set IERR=16 instead of ISTAT=16 if no match in READIDX - S.Cox
!
! Revision 2.2  2001/05/31  13:23:59  13:23:59  usmdb (MetDB account c/o usjh)
! Removed argument 1 from call to BITINDX as not used in BITINDX - S.Cox
!
! Revision 2.1  2001/03/07  11:53:24  11:53:24  usmdb (Generic MetDB account)
! 19 March 2001    C Long
! 2.1  Make sure that retrievals are repeatable even if a PILOT has been
!      reindexed under a launch time taken from a TEMP for same ascent.
!
! Revision 2.0  2001/01/08  11:59:24  11:59:24  usmdb (Generic MetDB account)
! Moved declaration of NDES before any array declarations.
! Removed arguments MAXARRSEGS and MAXARRELMS from calls to
! UPRPARTS as they are unused. Removed EXTERNAL statements.
! Changed calls to IDMTCH and SUBPER - they now return a return code
! rather than making use of the obsolete ALTERNATE RETURN feature.
! Added copyright and modified header - S.Cox
!
! Revision 1.21  2000/11/07  12:12:38  12:12:38  usmdb (Generic MDB account)
! 20 Nov 2000   C Long
! 1.21  Allow retrieval of new type UAWINDS
!       (all winds reported in either TEMP or PILOT)
!
! Revision 1.20  2000/08/09  14:58:50  14:58:50  usmdb (Generic MDB account)
! 21 Aug 2000        C Long
! 1.20  Allow retrieval by launch time
!
! Revision 1.19  2000/03/13  10:16:52  10:16:52  usmdb (Generic MDB account)
! Implemented 20-03-2000 - S.Cox
! a) Change to BUFR message lengths to be read from MetDB data blocks.
!    Use separate strings for observed and merged data as the lengths
!    can be vastly different. Merge message length increased to 27998
!    bytes, Observed message length reduced to 5000 bytes.  This is
!    part of a fix to MetDB problem 479.
! b) Check TOR of each part rather than just 1st part in chain.
!    Increase size of internal arrays so TOR for each part can be
!    retrieved by user.
!
! Revision 1.18  99/02/11  12:09:42  12:09:42  usmdb (Generic MDB account)
! 15-02-1999 S.Cox
! Add subtype to NO DATA FOR THIS PERIOD warning message.
!
! Revision 1.17  98/11/12  11:45:21  11:45:21  usmdb (Generic MDB account)
! 16-11-98 S.Cox
! Change declaration of CINDX - now an array of element indexes.
!
! Revision 1.16  98/09/16  16:11:23  16:11:23  usmdb (Generic MDB account)
! 21-09-98  S.Cox  Ref. Problem 144
! Correct setting of IOBS if a retrieval spans more than one dataset.
!
! Revision 1.15  98/07/23  08:42:11  08:42:11  usmdb (Generic MDB account)
! Changes to allow retrieval of BUFR messages.
!
! Revision 1.14  98/05/15  10:03:42  10:03:42  usmdb (Generic MDB account)
! Change loop over reports in chain.
!
! Revision 1.13  98/04/20  07:17:39  07:17:39  usmdb (Generic MDB account)
! Allow up to 150 levels for merged data.
!
! Revision 1.12  98/03/18  09:25:16  09:25:16  usmdb (Generic MDB account)
! Correction to change !M - Revision 1.11.
!
! Revision 1.11  98/03/12  08:52:12  08:52:12  usmdb (Generic MDB account)
! Change initialisation of II. Only set it to zero if NEWMDBCALL=.TRUE.
!
! Revision 1.10  97/09/22  11:05:29  11:05:29  uspm (Pat McCormack)
! Pass 2 extra arguments to VALUSR.
!
! Revision 1.9  1997/08/20 12:30:21  uspm
! Remove check on lat, long when no AREA keyword supplied.
!
! Revision 1.8  1997/08/04 13:42:33  uspm
! First revisioned version for COSMOS - with Y2K changes.
!
! Revision 1.7  1997/07/25 14:13:37  uspm
! Latest version from COSMOS - dated 21-7-97.
!
! Revision 1.5  1997/04/18 15:27:27  uspm
! Version dated 18-4-97 copied from COSMOS.
!
! Revision 1.4  1997/04/07 13:29:37  uspm
! version dated 12-3-97 from COSMOS.
!
! Revision 1.3  1997/02/27 12:18:15  uspm
! Latest version from COSMOS.
!
! Revision 1.2  1997/02/20 13:16:50  uspm
! latest COSMOS version.
!
! Revision 1.1  1997/02/17 09:16:22  uspm
! Initial revision.
!
! 20-07-98  !Q  : Code added to allow retrieval of BUFR messages - S.Cox
!
! 18-05-98  !P  : Change loop over reports in chain. Change from loop
!               : over number of entries in chain to a DO WHILE until
!               : end of chain reached (next block/record pointers
!               : equal to zero). Also correct safety check - S.Cox
!
! 20-04-98  !O  : Increase MXRPLEN from 15000 to 18000 to cope with up
!               : to 150 levels of merged data - S.Cox
!
! 18-03-98  !N  : Correction to change !M. Initialise II on each entry
!               : to UPRRET - S.Cox
!
! 16-03-98  !M  : Change initialisation of II. Only set it to zero if
!               : NEWMDBCALL=.TRUE. - S.Cox
!
! 29-09-97  !L  : Pass 2 extra arguments to VALUSR. These are not used
!               : in VALUSR for UPRAIR retrieval, but are needed to
!               : keep code consistent - S.Cox
!
! 01-09-97  !K  : Remove the sections of code which reject index entries
!               : that have a missing lat or lon. All SYNOPS will be
!               : retrieved unless the user specifies an AREA <lat>
!               : <lat> <lon> <lon> - S.Cox
!
! 28-07-97  !J  : Changes to allow rotated lat/lon selection of obs.
!               : NSEND and NREP are now passed to VALARR which has had
!               : it's loop structure changed. IAREA is now AREA (real),
!               : LAT_SUBSCRIPT, LON_SUBSCRIPT and RPOLE are passed in
!               : as arguments and are passed to VALARR. The call to
!               : ARMTCH has been replaced by calls to ROTAREA and
!               : VALAREA - J.Lewthwaite
!
! 21-07-97  !I  : Changes to cope with retrieval of merged data - S.Cox
!
! 30-06-97  !H  : Make CCCC and TTAA available for retrieval - S.Cox
!
! 18-04-97  !G  : Pass NEWBITIND to UPRPARTS and NEWSKLARR to
!               : ARRINDX if NEWMDBCALL is TRUE - S.Cox
!
! 12-03-97  !F  : Correct calcuation of latitude and longitude to pass
!               : to UPRPARTS to put in report text header - S.Cox
!
! 21-02-97  !E  : New argument NEWMDBCALL. This is needed by BITINDX
!               : to decide if a new call has been made to the MetDB
!               : with ISTAT=0. Also check for negative TORs and
!               : change the users RECEIVED BEFORE rejection slightly
!               : S.Cox
!
! 04-02-97  !D  : Allow retrieval to continue if "ELEMENT INDEX NOT
!               : FOUND" in RDBITAB - S.Cox
!
! 02-02-97  !C  : Correction to the way we decide if an index entry
!               : has both TEMPs and PILOTs in it - S.Cox
!
! 13-12-96  !B  : Change the call to READIDX to read the element index
!               : from the element index dataset rather than the storage
!               : dataset. Also add some dynamic common. Add sections
!               : to check byte 17 of the index entry to see if it
!               : contains TEMPS and PILOTS for the same ascent (this
!               : happens when stations report temperatures in TEMPS
!               : and winds in PILOTS for the same ascent). If so,
!               : check byte 17 of the trailer when selecting
!               : parts - S.Cox
!
! 09-12-96  !A  : Correction to "MDB data not available" warning
!               : message. Also now possible to select parts A&B or
!               : C&D only using COMBINED keyword. It is also possible
!               : to use the keyword INCREMENT. Correction to the
!               : calculation of the raw report text header hour and
!               : minute - S.Cox
!
! 30-07-96      : Written - S.Cox
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! declare variables used in parameter statements
!-----------------------------------------------------------------------

      INTEGER  INDLEN      !- index entry length
      INTEGER  MXOFL       !- max no. of overflow blocks used
      INTEGER  MXINDX      !- max no. of index entries
      INTEGER  MXRPLEN     !- max length of report
      INTEGER  MXRPLEN_MER !- max length of report (merged data)  !1.19a
      INTEGER  MXCHLEN     !- max no of chained reports
      INTEGER  MXCHLEN_MER !- max no of chained reports (merged)  !1.19a
      INTEGER  MXBLK       !- max block size
      INTEGER  MXFINELMS   !- max no. of elements in FINALPROFILE array
      INTEGER  MXARRELMS   !- max no. of elements in ARRINDX
      INTEGER  MXARRSEGS   !- max no. of segments in ARRINDX
      INTEGER  MXREPTXTLEN !- report text length for user         !1.19a

!-----------------------------------------------------------------------
! parameter statements (see above for descriptions)
!-----------------------------------------------------------------------

      PARAMETER (INDLEN      = 23)
      PARAMETER (MXOFL       = 2)
      PARAMETER (MXINDX      = 27992/INDLEN)
      PARAMETER (MXRPLEN     = 5000)                              !1.19a
      PARAMETER (MXRPLEN_MER = 27998)                             !1.19a
      PARAMETER (MXCHLEN     = 15)                 ! was 20           !I
      PARAMETER (MXCHLEN_MER = 2)                                 !1.19a
      PARAMETER (MXBLK       = 10000)
      PARAMETER (MXFINELMS   = 24000)              ! was 7064         !I
      PARAMETER (MXARRELMS   = 75)                                !1.19b
      PARAMETER (MXARRSEGS   = 10)
      PARAMETER (MXREPTXTLEN = 27998)              ! was 20000    !1.19a

!-----------------------------------------------------------------------
! declare variables (INTEGERS)
!-----------------------------------------------------------------------

      INTEGER  NDES      !- number of user elements                 !2.0

      INTEGER  ABLOCK     !- actual data block number (from start)

      INTEGER  ARRELMNUM(MXARRELMS)   !- ARRINDX elements array
      INTEGER  ARRNELREQ              !- no. of returned elems
      INTEGER  ARRNROWS               !- ARRINDX no. of elements
      INTEGER  ARRNSEGS               !- ARRINDX no. of segments
      INTEGER  ARRSEGLEN(MXARRSEGS)   !- ARRINDX segment length array
      INTEGER  ARRSEGNUM(MXARRELMS)   !- ARRINDX segment no. array
      INTEGER  ARRSEGST(MXARRSEGS)    !- ARRINDX segment start array
      INTEGER  ARRSOURCE(MXFINELMS)   !- ARRINDX segment start array
      INTEGER  ARRSTYP(MXARRSEGS)     !- ARRINDX segment repl count arr
      INTEGER  ARRSUBNUM(MXARRELMS)   !- ARRINDX segment subscript array

      INTEGER  BUFREND    !- position of end of BUFR message          !Q
      INTEGER  BUFRSTART  !- position of start of BUFR message        !I
      INTEGER  BYTE17     !- index entry byte 17
      INTEGER  CHLOOP     !- loop over reports in chain
      INTEGER  COMB_CENT_MIN ! century-minute of combined ascent  !2.1
      INTEGER  DBLOCK     !- data block number

      INTEGER  DISPL(MXFINELMS)     !- array of BUFR/array displacements

      INTEGER  F          !- F of FXXYYY BUFR descriptor              !I
      INTEGER  HDAY       !- day to put in header
      INTEGER  HHOUR      !- hour to put in header
      INTEGER  HMONTH     !- month to put in header
      INTEGER  HYEAR      !- year to put in header
      INTEGER  I          !- general use loop counter
      INTEGER  IBEFOR     !- used in call to VALUE                    !I
      INTEGER  IBLD       !- index block day
      INTEGER  IBLD2      !- index block day (end)                    !A
      INTEGER  IBLH       !- index block hour
      INTEGER  IBLH2      !- index block hour (end)                   !A
      INTEGER  IBLM       !- index block month
      INTEGER  IBLM2      !- index block month (end)                  !A
      INTEGER  IBLY       !- index block year
      INTEGER  IBLY2      !- index block year (end)                   !A
      INTEGER  IBLOCK     !- physical block no. to read (maprd)
      INTEGER  IBLOOP     !- loop counter over index blocks
      INTEGER  ICHL1      !- start position in chain
      INTEGER  ICHL2      !- end position in chain
      INTEGER  ICHL3      !- loop increment in chain
      INTEGER  ICHR       !- century hour from chained index entry    !A
      INTEGER  ICMIN      !- century minute from chained index entry
      INTEGER  IDATHR     !- time tag of index read              (maprd)
      INTEGER  IDAY       !- day from index block

      INTEGER  IDESC(1+NDES) !- array of user required element pointers

      INTEGER  IDHR       !- hour from chained index entry
      INTEGER  IDSK(5)    !- dataset storage details
      INTEGER  IELOOP     !- loop counter over index entries
      INTEGER  IENTRIES   !- count of index entries we want
      INTEGER  IENT1      !- used in chain loop
      INTEGER  IERR       !- error indicator
      INTEGER  IEXTRA(1)  !- array of extra values for VALARR
      INTEGER  IFAIL      !- status flag for merge retrieval          !I
      INTEGER  II
      INTEGER  IHOUR      !- hour from index block
      INTEGER  ILOCALD    !- integer local D descriptor               !I
      INTEGER  IMIN       !- minute from chained index entry
      INTEGER  INDHR1     !- index block start time
      INTEGER  INDHR2     !- index block end time
      INTEGER  INOMIN     !- century-minute of nominal time       !2.1
      INTEGER  INUM       !- no. of entries read                 (maprd)
      INTEGER  INXBLK     !- no. of index blocks in ds           (maprd)
      INTEGER  INXHRS     !- no. of hours per index block        (maprd)
      INTEGER  IOB        !- last ob in users array
      INTEGER  IOBS       !- next ob in users array
      INTEGER  IOVER      !- land/sea user-selection
      INTEGER  IRECLN     !- total length of message             (maprd)
      INTEGER  IREPL(*)   !- array of user required replications
      INTEGER  IREP1
      INTEGER  IRTIM(10)  !- users time of receipt
      INTEGER  IRTIM1     !- users start TOR in century minutes
      INTEGER  IRTIM2     !- users end TOR in century minutes
      INTEGER  ISECT1(32) !- BUFR section 1 array                 !1.19b
      INTEGER  ISHR1      !- users start time as start of index blk
      INTEGER  ISHR2      !- users start time as end of index blk
      INTEGER  ISLOTHR    !- start hour of 1st index block       (maprd)
      INTEGER  ISTAT      !- MetDB state indicator
      INTEGER  ITIM1      !- users start time in century hours
      INTEGER  ITIM2      !- users end time in century hours
      INTEGER  ITIME(9)   !- users end time in century hours
      INTEGER  ITIMND     !- users end time in century minutes
      INTEGER  ITIMST     !- users start time in century minutes
      INTEGER  ITORM      !- actual report TOR in century minutes
      INTEGER  KEPCHN     !- position in chain
      INTEGER  LAT        !- latitude from index entry

      INTEGER  LAT_SUBSCRIPT  !- users lat subscript                  !J

      INTEGER  LENCHR(3)  !- array of lengths of CHRELM               !H
      INTEGER  LON        !- longitude from index entry

      INTEGER  LON_SUBSCRIPT  !- users lon subscript                  !J

      INTEGER  LRCPOINT   !- LEVL_RPLTN_CONT displ array pointer      !I

      INTEGER  MSGLEN(MXCHLEN)   !- actual length of chained report

      INTEGER  NCHN      !- no. of entries in chain
      INTEGER  NELEM     !- users no. of elements
      INTEGER  NELMIX    !- block no. for element index          (maprd)
      INTEGER  NOBS      !- users no. of observations.
      INTEGER  NREC      !- logical record number                (maprd)
      INTEGER  NREP
      INTEGER  NSEND
      INTEGER  NSQ       !- 1 or 0 if local descriptor or not
      INTEGER  NTOR      !- report TOR after slot base hour
      INTEGER  NUMTEMPS  !- number of user temp levels wanted
      INTEGER  NUMWINDS  !- number of user wind levels wanted
      INTEGER  NXTCHN
      INTEGER  NXTENT
      INTEGER  NXTIME
      INTEGER  NXTREP

      INTEGER  POSN_PART(4)       !- position of part in chain

      INTEGER  RC                 !- general return code            !2.0
      INTEGER  RECNUM             !- record no. of data in data block
      INTEGER  REFVAL(MXFINELMS)  !- bitindex refval array            !I
      INTEGER  SCALE(MXFINELMS)   !- bitindex scale array             !I
      INTEGER  TRANGE             !- users START TIME sub period.
      INTEGER  TRB17              !- report trailor byte 17
      INTEGER  TRL_OB_TYPE        !- TEMP or PILOT part in trailor    !B
      INTEGER  UAPART             !- 0=A&C or B&D, 1=AorB, 2=CorD
      INTEGER  USERLEVS           !- number of user levels
      INTEGER  VER                !- 1=preferred (default), 2=all
      INTEGER  WIDTH(MXFINELMS)   !- bitindex width array             !I
      INTEGER  X                  !- XX of FXXYYY BUFR descriptor     !I
      INTEGER  Y                  !- YYY of FXXYYY BUFR descriptor    !I

!-----------------------------------------------------------------------
! declare variables (REALS - in alphabetical order)
!-----------------------------------------------------------------------

      REAL     AREA(5)                 !- user-defined area           !J
      REAL     ARRAY(NOBS,NELEM)       !- users array
      REAL     FINALPROFILE(MXFINELMS) !- final combined ascent
      REAL     RLAT                    !- real lat from index entry   !F
      REAL     RLON                    !- real lon from index entry   !F
      REAL     ROT_LAT                 !- rotated latitude            !J
      REAL     ROT_LON                 !- rotated longitude           !J
      REAL     RPOLE(2)                !- rotated pole lat lon coords !J

!-----------------------------------------------------------------------
! declare variables (LOGICALS - in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL  AREA_FLAG    !- TRUE if ob inside users area           !J
      LOGICAL  CHANRED      !- TRUE if chain has been read in
      LOGICAL  ELIDXFOUND   !- TRUE if element index found            !B
      LOGICAL  FIRST        !- TRUE for first call to UPRAIRET
      LOGICAL  FLAG999
      LOGICAL  FOUND(*)     !- array of keywords selected
      LOGICAL  FOUNDLCC     !- TRUE if LEVL_CDTN_CODE found (merge)   !I
      LOGICAL  FOUNDLRC     !- TRUE if LEVL_RPLTN_CONT found (merge)  !I
      LOGICAL  LCONT
      LOGICAL  LFLAG        !- diagnostics output
      LOGICAL  LOCDFG       !- TRUE if local descriptor in dataset
      LOGICAL  INDXRED      !- TRUE if index block has been read
      LOGICAL  ITS_A_TEMP   !- TRUE if report is a TEMP
      LOGICAL  ITS_A_PILOT  !- TRUE if report is a PILOT
      LOGICAL  ITS_A_DROP   !- TRUE if report is a DROPSOND
      LOGICAL  ITS_FIXED    !- TRUE if report is a FIXED ob
      LOGICAL  ITS_MARINE   !- TRUE if report is a MARINE ob
      LOGICAL  ITS_MOBILE   !- TRUE if report is a MOBILE ob
      LOGICAL  ITS_ONLAND   !- TRUE if report is a LAND ob
      LOGICAL  ITS_PART(4)  !- TRUE if report is part 1=A,2=B,3=C,4=D
      LOGICAL  KEEPIE       !- TRUE to keep index entry
      LOGICAL  KEPT_PART(4) !- TRUE if weve kept part 1=A,2=B,3=C,4=D
      LOGICAL  NEWMDBCALL   !- TRUE if new MetDB call (istat=0)       !E
      LOGICAL  NEWMRGCALL   !- TRUE if new MetDB call (istat=0)       !I
      LOGICAL  NEWSKLARR    !- TRUE if new MetDB call (istat=0)       !G
      LOGICAL  NEWBITIND    !- TRUE if new MetDB call (istat=0)       !G
      LOGICAL  NTRYCHK      !- TRUE if index entry has been read
      LOGICAL  READ_CHNREPS !- TRUE if reports left in chain to read  !P
      LOGICAL  QCREQ        !- TRUE if users wants QC elements
      LOGICAL  TPMIX        !- TRUE if TEMPs & PILOTs in chain        !B
      LOGICAL  USERDROP     !- TRUE if user wants DROPSONDEs          !B
      LOGICAL  USERPILOT    !- TRUE if user wants PILOTs              !B
      LOGICAL  USERTEMP     !- TRUE if user wants TEMPs               !B
      LOGICAL  WANT_ABCD    !- TRUE if user wants all parts of report
      LOGICAL  WANT_OB_TYPE !- TRUE if we want the ob type in trailor !B
      LOGICAL  WANT_PART(4) !- TRUE if user wants part 1=A,2=B,3=C,4=D
      LOGICAL  WANTTEMPS    !- TRUE if user wants temp levels
      LOGICAL  WANTWINDS    !- TRUE if user wants wind levels
      LOGICAL  WASPREF      !- TRUE if report was once preferred

!-----------------------------------------------------------------------
! declare variables (CHARACTERS - in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER*4          BUFR                !- BUFR character string
      CHARACTER*9          CALLSIGN(1)         !- Call Sign
      CHARACTER*4          CCCC                !- Collecting centre   !H
      CHARACTER*9          CHRELM(3)           !- character elems     !H
      CHARACTER*9          CIDENT(50)          !- station identifiers
      CHARACTER*(MXBLK)    CINDX(12)    !- holds bit/array index   !1.17
                                               !- (readidx)
      CHARACTER*(MXRPLEN)  CMSG(MXCHLEN)       !- actual message (maprd)
      CHARACTER*(MXRPLEN_MER) CMSG_MER(MXCHLEN_MER)               !1.19a

      CHARACTER*1          CNAM
      CHARACTER*(INDLEN)   CNTRY(MXOFL*MXINDX) !- character array of
                                               !- kept index entires
      CHARACTER*(*)        CREPRT(NOBS)        !- users report string
      CHARACTER*(*)        CRTYP               !- type of data stored !I
      CHARACTER*(*)        CSTR(NOBS)          !- users strings
      CHARACTER*(INDLEN)   CTRAIL(MXCHLEN)     !- chained index entry
      CHARACTER*(INDLEN)   CTRAN(MXOFL*MXINDX) !- character array of
                                               !- entries read   (maprd)
      CHARACTER*(*)        CTYPE               !- retrieval subtype
      CHARACTER*8          ELIST               !- elidx member      !2.4
      CHARACTER*80         HEAD                !- revision information!2
      CHARACTER*5          HELN                !- rprt header length  !Q
      CHARACTER*6          LOCALD              !- character local D   !I
      CHARACTER*23         MASKIT              !- used for (sortch)
      CHARACTER*1          PMAP(4)             !- map 1 to A, 2 to B etc

      CHARACTER*(MXREPTXTLEN)  REPORTTEXT      !- raw report text

      CHARACTER*4          SEVENS              !- 7777 char string    !Q
      CHARACTER*9          THISID              !- this station id.
      CHARACTER*9          TTAA                !- TTAA                !H

      CHARACTER*1          XTYPE               !- type of index 'A','B'

!-----------------------------------------------------------------------
! declare fuctions
!-----------------------------------------------------------------------

      INTEGER  DT2HRS    !- convert date/time to century hours
      INTEGER  VALUE     !- extract value from BUFR message           !I

!-----------------------------------------------------------------------
! dynamic common, compile with FPARMS='DC(*)' on IBM mainframe
!-----------------------------------------------------------------------

      COMMON /UPAIR1/ARRELMNUM,ARRSEGNUM,ARRSOURCE,ARRSUBNUM          !B
      COMMON /UPAIR2/DISPL,FINALPROFILE,CINDX                         !F
      COMMON /UPAIR3/CMSG,CNTRY,CTRAIL,CTRAN,REPORTTEXT               !B
      COMMON /UPAIR4/WIDTH,SCALE,REFVAL,CMSG_MER                !1.19a!I

!-----------------------------------------------------------------------
! SAVE statement to ensure all variables are still set on next call
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! initialise variables (data statements)
!-----------------------------------------------------------------------

      DATA FIRST  /.TRUE./           !- TRUE for first call to UPRAIRET

      DATA MASKIT /'  XXXXXXXXX'/    !- to sort on ident.
      DATA PMAP   /'A','B','C','D'/  !- to map 1-4 to A-D
      DATA NXTCHN /0/
      DATA NXTENT /0/
      DATA NXTIME /0/
      DATA NXTREP /0/
      DATA KEPCHN /0/

!-----------------------------------------------------------------------
! diagnostic output to say UPRRET has been called.
!-----------------------------------------------------------------------

      IF (LFLAG) THEN
        WRITE(*,'(/1X,''In MetDB subroutine UPRRET'' )')
        WRITE(*,'( 1X,''==========================''/)')
      ENDIF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

      IERR = 0
      II   = 0                                                        !N
      IOB  = IOBS-1    ! IOB --> last ob in users array (IOBS --> NEXT)

!-----------------------------------------------------------------------
! First time only:  Initialise revision information, BUFR and SEVENS
!-----------------------------------------------------------------------

      IF (FIRST) THEN
        BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'    !2.4
        SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'    !2.4
        HEAD='$Workfile: uprret.f$ ' //
     &       '$Revision: 2$ $Date: 21/12/2006 14:45:19$'
        FIRST=.FALSE.
      ENDIF

!-----------------------------------------------------------------------
! If a new mdb call with ISTAT=0, NEWMDBCALL=.TRUE. Set NEWSKLARR and
! NEWBITIND to .TRUE.                                                 !G
!-----------------------------------------------------------------------

      IF (NEWMDBCALL) THEN
        NEWSKLARR  = .TRUE.
        NEWBITIND  = .TRUE.
        NEWMRGCALL = .TRUE.
        NEWMDBCALL = .FALSE.
      ENDIF

!-----------------------------------------------------------------------
! check for continuation and if so, reset loop counters.
!-----------------------------------------------------------------------

      IF (ISTAT.EQ.16) THEN
        ISTAT = 0
      ELSEIF (ISTAT.EQ.4) THEN
        ISHR1 = NXTIME
        IENT1 = NXTENT
        IREP1 = NXTREP
        ICHL1 = NXTCHN
        NSEND = NXTREP-1
        LCONT = .TRUE.
      ELSE
        CONTINUE
      ENDIF

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here.
!=======================================================================

      IF (ISTAT.EQ.0) THEN

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not
! output an error and exit subroutine.
!-----------------------------------------------------------------------

        IF (NELEM.LT.NDES) THEN
          IERR = 16
          WRITE(*,*)'In UPRRET: MDB ERROR: You have not specified'    !B
          WRITE(*,*)'enough array elements. Try NELEM = ',NDES        !B
          GOTO 999
        ENDIF

!-----------------------------------------------------------------------
! calculate number of levels, user has coded
!-----------------------------------------------------------------------

        USERLEVS = MAX(NUMTEMPS,NUMWINDS)

!-----------------------------------------------------------------------
! decide which subtype user wants                                     !B
! (both TEMP & PILOT, for the levels to be combined, if UAWINDS)  !1.21
!-----------------------------------------------------------------------

        USERTEMP=(CTYPE(1:4).EQ.'TEMP' .OR.                       !1.21
     &            CTYPE(1:7).EQ.'UAWINDS')                        !1.21
        USERPILOT=(CTYPE(1:5).EQ.'PILOT' .OR.                     !1.21
     &            CTYPE(1:7).EQ.'UAWINDS')                        !1.21
        USERDROP  = (CTYPE(1:8).EQ.'DROPSOND')                        !B

!-----------------------------------------------------------------------
! decide which parts of the upper air report the user has requested.
!-----------------------------------------------------------------------

        WANT_ABCD = (.NOT.FOUND(25) .AND. .NOT.FOUND(26) .AND.
     &               .NOT.FOUND(30))                                  !A

        WANT_PART(1) = (WANT_ABCD .OR. (FOUND(25) .AND. UAPART.NE.2)
     &                            .OR. (FOUND(30) .AND. UAPART.NE.2)) !A
        WANT_PART(2) = (WANT_ABCD .OR. (FOUND(26) .AND. UAPART.NE.2)
     &                            .OR. (FOUND(30) .AND. UAPART.NE.2)) !A
        WANT_PART(3) = (WANT_ABCD .OR. (FOUND(25) .AND. UAPART.NE.1)
     &                            .OR. (FOUND(30) .AND. UAPART.NE.1)) !A
        WANT_PART(4) = (WANT_ABCD .OR. (FOUND(26) .AND. UAPART.NE.1)
     &                            .OR. (FOUND(30) .AND. UAPART.NE.1)) !A

        IF (LFLAG) THEN
          WRITE(*,*)'In UPRRET: WANT_ABCD  = ',WANT_ABCD
          DO I=1,4
            WRITE(*,*)'In UPRRET: WANT_PART',PMAP(I),' = ',WANT_PART(I)
          ENDDO
        ENDIF

!-----------------------------------------------------------------------
! read map block to find dataset details. If LOCDFG is TRUE, then there
! is a local table D descriptor. Set NSQ=1.
!-----------------------------------------------------------------------

        CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,LOCDFG,
     &             NELMIX,IBLOCK,INUM,CTRAN,IDATHR,
     &             NREC,IRECLN,CMSG(KEPCHN+1),'MAPRD')

        NSQ=0
        IF (LOCDFG) NSQ=1

!-----------------------------------------------------------------------
! Read the element index from the the element index dataset.       !1.21
!-----------------------------------------------------------------------

        CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)                  !2.4

        IF (.NOT.ELIDXFOUND) THEN                                     !B
          WRITE(*,*)'In UPRRET: MDB ERROR: Cannot find element ',     !B
     &    'index for subtype,elist = ',CTYPE,ELIST                  !2.4
          IERR=16                                                   !2.3
          GOTO 999                                                    !B
        ENDIF                                                         !B

        IF (LFLAG) THEN
          WRITE(*,*)'In UPRRET: after call readidx, XTYPE=',XTYPE
        ENDIF

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

        IF (IRTIM(1).EQ.0) THEN    !- start time
          IRTIM1=0
        ELSE
          IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
          IRTIM1=(IRTIM1)*60+IRTIM(5)
        ENDIF

        IF (IRTIM(6).EQ.0) THEN    !- end time
          IRTIM2=0
        ELSE
          IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
          IRTIM2=(IRTIM2)*60+IRTIM(10)
        ENDIF

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

      ENDIF  !- end of ISTAT=0 if block

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

      DO 799 IBLOOP=ISHR1,ISHR2,INXHRS

        IF (.NOT.INDXRED) THEN

!-----------------------------------------------------------------------
! Read the index block (IBLOCK) using MAPRD. 2 and NSQ are added to get
! us past the map block and the local D block if there is one.
!-----------------------------------------------------------------------

          IBLOCK=MOD(IBLOOP/INXHRS,INXBLK)+2+NSQ

          CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,
     &               LOCDFG,NELMIX,IBLOCK,INUM,CTRAN,IDATHR,
     &               NREC,IRECLN,CMSG(KEPCHN+1),'IDXRD')

!-----------------------------------------------------------------------
! Check that correct date/time has been read. Compare the day and hour
! for the index block (iday, ihour) with the day and hour in the IBLOOP
! loop (IBLD, IBLH calculated from IBLOOP). If no match, output a
! message to the user and GOTO 799 to read the next index block.
!-----------------------------------------------------------------------

          IDAY=IDATHR/256
          IHOUR=IDATHR-IDAY * 256

          CALL HRS2DT(IBLY,IBLM,IBLD,IBLH,IBLOOP)

          IF (IBLD.NE.IDAY.OR.IBLH.NE.IHOUR) THEN
            CALL HRS2DT(IBLY2,IBLM2,IBLD2,IBLH2,IBLOOP+INXHRS-1)      !A
            WRITE(*,9003)'UPRAIR  ',IBLY,IBLM,IBLD,IBLH,           !1.18
     &      IBLY2,IBLM2,IBLD2,IBLH2                                !1.18
            GOTO 799
          ENDIF

!=======================================================================
! Index block matched! Loop round number of index entries (INUM) in this
! index block. IENTRIES is a count of the index entries we want to keep.
!=======================================================================

          IENTRIES=0

          DO 81 IELOOP=1,INUM

!-----------------------------------------------------------------------
! safety check : Check that the data block number and record number in
!              : the data block are greater than zero.
!-----------------------------------------------------------------------

            DBLOCK=ICHAR(CTRAN(IELOOP)(22:22))*256+
     &             ICHAR(CTRAN(IELOOP)(23:23))
            ABLOCK=1+NSQ+INXBLK+DBLOCK
            RECNUM=ICHAR(CTRAN(IELOOP)(20:20))*256+
     &             ICHAR(CTRAN(IELOOP)(21:21))

            IF (DBLOCK.LE.0 .OR. RECNUM.LE.0) THEN                    !P
              IF (LFLAG) WRITE(*,*)'In UPRRET: Bad index entry!!'
              GOTO 81
            ENDIF

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
!           : 11 = TEMP & PILOT mix                                   !B
!-----------------------------------------------------------------------

            KEEPIE = .FALSE.

            BYTE17 = ICHAR(CTRAN(IELOOP)(17:17))

            ITS_A_TEMP  = .FALSE.                                     !B
            ITS_A_PILOT = .FALSE.                                     !B
            ITS_A_DROP  = .FALSE.                                     !B

            IF (MOD(BYTE17/8,2).EQ.0 .AND.                            !B
     &      MOD(BYTE17/4,2).EQ.0) THEN                                !B
              ITS_A_PILOT = .TRUE.                                    !B
            ELSEIF (MOD(BYTE17/8,2).EQ.0 .AND.                        !B
     &      MOD(BYTE17/4,2).EQ.1) THEN                                !B
              ITS_A_TEMP = .TRUE.                                     !B
            ELSEIF (MOD(BYTE17/8,2).EQ.1 .AND.                        !B
     &      MOD(BYTE17/4,2).EQ.0) THEN                                !B
              ITS_A_DROP = .TRUE.                                     !B
            ELSE                                                      !B
              ITS_A_PILOT = .TRUE.                                    !B
              ITS_A_TEMP  = .TRUE.                                    !B
            ENDIF                                                     !B

            IF (USERTEMP  .AND. ITS_A_TEMP)  KEEPIE=.TRUE.            !B
            IF (USERPILOT .AND. ITS_A_PILOT) KEEPIE=.TRUE.            !B
            IF (USERDROP  .AND. ITS_A_DROP)  KEEPIE=.TRUE.            !B

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: ITS_A_PILOT = ',ITS_A_PILOT
              WRITE(*,*)'In UPRRET: ITS_A_TEMP  = ',ITS_A_TEMP
              WRITE(*,*)'In UPRRET: ITS_A_DROP  = ',ITS_A_DROP
              WRITE(*,*)'In UPRRET: KEEPIE      = ',KEEPIE
            ENDIF

!-----------------------------------------------------------------------
! The next 2 checks only apply to TEMP or PILOT, not DROPSOND. Only do
! this check, however, if check 1 was passed (KEEPIE=.TRUE.)
!-----------------------------------------------------------------------

            IF (KEEPIE) THEN

              IF (ITS_A_TEMP .OR. ITS_A_PILOT) THEN

!-----------------------------------------------------------------------
! 2nd check : reset KEEPIE to false for the next index entry check.
!           : check bit 8 of byte 17 in the index entry. If it is 1 the
!           : ob is marine else it is land. Check the user's request.
!           : iover = 0 = land & sea, iover = 1 = land, iover = 2 = sea.
!-----------------------------------------------------------------------

                KEEPIE=.FALSE.

                ITS_ONLAND=(MOD(BYTE17/1,2).EQ.0)
                ITS_MARINE=(MOD(BYTE17/1,2).EQ.1)

                IF (IOVER.EQ.1 .AND. ITS_ONLAND)          KEEPIE=.TRUE.
                IF (IOVER.EQ.2 .AND. ITS_MARINE)          KEEPIE=.TRUE.
                IF (IOVER.EQ.0)                           KEEPIE=.TRUE.

                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: ITS_ONLAND  = ',ITS_ONLAND
                  WRITE(*,*)'In UPRRET: ITS_MARINE  = ',ITS_MARINE
                  WRITE(*,*)'In UPRRET: KEEPIE      = ',KEEPIE
                ENDIF

!-----------------------------------------------------------------------
! 3rd check : If the last check was passed (KEEPIE=.TRUE.) and it is
!           : a land observation
!-----------------------------------------------------------------------

                IF (KEEPIE .AND. ITS_ONLAND) THEN

                  KEEPIE=.FALSE.

                  ITS_FIXED=(MOD(BYTE17/2,2).EQ.0)
                  ITS_MOBILE=(MOD(BYTE17/2,2).EQ.1)

                  IF (FOUND(27) .AND. ITS_FIXED)           KEEPIE=.TRUE.
                  IF (FOUND(28) .AND. ITS_MOBILE)          KEEPIE=.TRUE.
                  IF (.NOT.FOUND(27) .AND. .NOT.FOUND(28)) KEEPIE=.TRUE.

                  IF (LFLAG) THEN
                    WRITE(*,*)'In UPRRET: ITS_FIXED   = ',ITS_FIXED
                    WRITE(*,*)'In UPRRET: ITS_MOBILE  = ',ITS_MOBILE
                    WRITE(*,*)'In UPRRET: KEEPIE      = ',KEEPIE
                  ENDIF

                ENDIF  !- check 3

              ENDIF  !- check 2

            ENDIF  !- check 1

!-----------------------------------------------------------------------
! 4th check : Check that the data time in the index entry is within the
!           : start and end times requested by the user. ICMIN = time
!           : of report in century minutes. If not, don't keep report.
! Treat missing minute (no launch time) as on the nominal hour.   !1.20
!-----------------------------------------------------------------------

            IF (KEEPIE) THEN

              IDHR=MOD(ICHAR(CTRAN(IELOOP)(1:1)),64)
              IMIN=ICHAR(CTRAN(IELOOP)(2:2))
              IF (IMIN.EQ.255) IMIN=0                             !1.20

              ICHR=DT2HRS(IBLY,IBLM,IDAY,IHOUR)+IDHR                  !A
              ICMIN=ICHR*60+MOD(IMIN,64)                          !2.1

! Work out century-minute for nominal hour too - either launch    !2.1
! hour with zero minutes or next hour, depending on minute flag.  !2.1

              IF (IMIN.GT.60) THEN                                !2.1
                IF (IMIN.LT.128) THEN                             !2.1
                  INOMIN=ICHR*60                                  !2.1
                ELSE                                              !2.1
                  INOMIN=(ICHR+1)*60                              !2.1
                ENDIF                                             !2.1
              ENDIF                                               !2.1

! If index time is not in window, reject this ob - UNLESS it was  !2.1
! once indexed under a different time (nominal time rather than   !2.1
! launch time) and that time IS in the window.                    !2.1
! So the condition below can be read as follows:                  !2.1
!   if the current index time is not in the window,               !2.1
!     and either this ob was never indexed under any other time   !2.1
!         or that original index time is outside the window too,  !2.1
!   then reject this ob.                                          !2.1

              IF (.NOT. (ICMIN.GE.ITIMST .AND. ICMIN.LE.ITIMND)   !2.1
     &            .AND. (.NOT. IMIN.GT.60 .OR.                    !2.1
     &          .NOT. (INOMIN.GE.ITIMST .AND. INOMIN.LE.ITIMND))) !2.1
     &                                 KEEPIE=.FALSE.             !2.1
              IF (LFLAG .AND. .NOT.KEEPIE) THEN                   !2.1
                PRINT *,'UPRRET time check rejected',CTRAN(IELOOP)!2.1
              ENDIF

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: Data time check: KEEPIE = ',KEEPIE
              ENDIF

            ENDIF  !- check 4

!-----------------------------------------------------------------------
! 5th check : If we still want this index entry, get the lat and lon.
!           : The check for a missing lat or lon has been removed     !K
!-----------------------------------------------------------------------

            IF (KEEPIE) THEN

              LAT=ICHAR(CTRAN(IELOOP)(13:13))*256+
     &            ICHAR(CTRAN(IELOOP)(14:14))
              LON=ICHAR(CTRAN(IELOOP)(15:15))*256+
     &            ICHAR(CTRAN(IELOOP)(16:16))

              IF (LAT.GE.32768) LAT=LAT-65536
              IF (LON.GE.32768) LON=LON-65536

              RLAT=0.01*REAL(LAT)                                     !J
              RLON=0.01*REAL(LON)                                     !J

!-----------------------------------------------------------------------
! 6th check : Check that the report ID matches requested IDs. If not,
!           : goto 81.
!-----------------------------------------------------------------------

              CALL IDMTCH(CTRAN(IELOOP),THISID,CIDENT,.FALSE.,RC)   !2.0
              IF (RC.NE.0) GOTO 81                                  !2.0

              IF (LFLAG) THEN
                WRITE(*,*)'In UPRRET: Identifier matched'
              ENDIF

!-----------------------------------------------------------------------
! 7th check : Check that the report is within the requested area. If
!           : not, goto 81.
!-----------------------------------------------------------------------

              IF (AREA(1).EQ.1.0) THEN                                !J
                CALL ROTAREA(RLAT,RLON,ROT_LAT,ROT_LON,RPOLE)         !J
                CALL VALAREA(ROT_LAT,ROT_LON,AREA,AREA_FLAG,LFLAG)    !J
              ELSEIF (AREA(1).EQ.0.0) THEN                            !J
                CALL VALAREA(RLAT,RLON,AREA,AREA_FLAG,LFLAG)          !J
              ELSE                                                    !J
                AREA_FLAG=.TRUE.                                      !J
              ENDIF                                                   !J

              IF (AREA_FLAG) THEN                                     !J

                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: Area matched'
                ENDIF

!-----------------------------------------------------------------------
! 8th check : Check on user's INCREMENT.                              !A
!-----------------------------------------------------------------------

                IF (FOUND(4)) THEN                                    !A
                  CALL SUBPER(ITIMST,TRANGE,ITIME(9),ITIMND,          !A
     &                        ICMIN,RC)                             !2.0
                  IF (RC.NE.0) GOTO 81                              !2.0
                ENDIF                                                 !A

                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: passed INCREMENT check'
                ENDIF

!-----------------------------------------------------------------------
! If we get to here, the report has passed all the checks. KEEP IT!!
!-----------------------------------------------------------------------

                IENTRIES=IENTRIES+1
                CNTRY(IENTRIES)=CTRAN(IELOOP)

                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: KEEP THE INDEX ENTRY!!!!!!!'
                ENDIF

              ENDIF  !- area_flag check                               !J

            ENDIF  !- keepie checks

81        CONTINUE !- end of loop over index entries

!-----------------------------------------------------------------------
! sort entries into identifier & time order and set INDXRED to TRUE to
! say weve read an index block.
!-----------------------------------------------------------------------

          CALL SORTCH(CNTRY,INDLEN,IENTRIES,MASKIT)
          INDXRED=.TRUE.

        ENDIF  !- end of .NOT.INDXRED if block

!=======================================================================
! Now loop over the selected index entries. Check that we have not
! already checked this index entry (NTRYCHK=.TRUE.)
! For UAWINDS retrieval go down the chain twice, looking first    !1.21
! for TEMPs & then for PILOTs.                                    !1.21
!=======================================================================

        DO IELOOP = IENT1,IENTRIES

 789      IF (.NOT.NTRYCHK) THEN ! back here for PILOT if UAWINDS !1.21

!-----------------------------------------------------------------------
! get number of reports in the chain. Set it to max if chain too long.
!-----------------------------------------------------------------------

            NCHN = ICHAR(CNTRY(IELOOP)(12:12))
            IF (NCHN.GT.MXCHLEN) NCHN=MXCHLEN

!-----------------------------------------------------------------------
! get data block number (DBLOCK) from index entry. Calculate actual
! block number (ABLOCK) from start of dataset. Then get record number
! (RECNUM) of the data in this data block from the index entry.
!-----------------------------------------------------------------------

            DBLOCK=ICHAR(CNTRY(IELOOP)(22:22))*256+
     &             ICHAR(CNTRY(IELOOP)(23:23))

            ABLOCK=1+NSQ+INXBLK+DBLOCK

            RECNUM=ICHAR(CNTRY(IELOOP)(20:20))*256+
     &             ICHAR(CNTRY(IELOOP)(21:21))

!-----------------------------------------------------------------------
! get the hour and minute from the kept index entry. Convert the date
! to a century hour (ICHR) as IHOUR + IDHR will often mean a new day.
! Treat missing minute (no launch time) as on the nominal hour.   !1.20
!-----------------------------------------------------------------------

            IDHR=MOD(ICHAR(CNTRY(IELOOP)(1:1)),64)                    !A
            IMIN=ICHAR(CNTRY(IELOOP)(2:2))                            !A
            IF (IMIN.EQ.255) IMIN=0                               !1.20
            ICHR=DT2HRS(IBLY,IBLM,IDAY,IHOUR)+IDHR                    !A

!-----------------------------------------------------------------------
! get the latitude and longitude from the index entry.
!-----------------------------------------------------------------------

            LAT=ICHAR(CNTRY(IELOOP)(13:13))*256+                      !F
     &          ICHAR(CNTRY(IELOOP)(14:14))                           !F
            LON=ICHAR(CNTRY(IELOOP)(15:15))*256+                      !F
     &          ICHAR(CNTRY(IELOOP)(16:16))                           !F

            IF (LAT.GE.32768) LAT=LAT-65536                           !F
            IF (LON.GE.32768) LON=LON-65536                           !F

            RLAT=0.01*REAL(LAT)                                       !F
            RLON=0.01*REAL(LON)                                       !F

!-----------------------------------------------------------------------
! Check byte 17 of the kept index entry. If both bits 5 and 6 are set
! to 1, the ascent is a mixture of TEMPs and PILOTs, set TPMIX = TRUE
!-----------------------------------------------------------------------

            IF (MOD(ICHAR(CNTRY(IELOOP)(17:17))/8,2).EQ.1 .AND.       !C
     &      MOD(ICHAR(CNTRY(IELOOP)(17:17))/4,2).EQ.1) THEN           !C
              TPMIX = .TRUE.                                          !C
            ELSE                                                      !C
              TPMIX = .FALSE.                                         !C
            ENDIF                                                     !C

            NTRYCHK=.TRUE.

          ENDIF  !- ntrychk if block.

!=======================================================================
! There are 3 options now:
!
! 1) Current data retrieval: The user wants more than 1 part
! 2) Current data retrieval: The user wants an individual part
! 3) Merged  data retrieval: The user will get a combined profile
!
!=======================================================================

          IF (CRTYP.EQ.'C') THEN                                      !I

! **********************************************************************
! **********************************************************************
! **                                                                  **
! **  1) Current data retrieval : The user wants more than 1 part     **
! **                                                                  **
! **********************************************************************
! **********************************************************************

            IF ((WANT_PART(1) .AND. WANT_PART(3)) .OR.
     &          (WANT_PART(2) .AND. WANT_PART(4)) .OR.
     &          (WANT_PART(1) .AND. WANT_PART(2)) .OR.                !A
     &          (WANT_PART(3) .AND. WANT_PART(4)) .OR.                !A
     &          WANT_ABCD) THEN                                       !A

              IF (.NOT.CHANRED) THEN

                KEPCHN=0

!-----------------------------------------------------------------------
! Before looping over the reports in the chain, initialise some
! variables
!-----------------------------------------------------------------------

                DO I=1,4
                  KEPT_PART(I)=.FALSE.
                  POSN_PART(I)=0
                ENDDO

!-----------------------------------------------------------------------
! Loop over reports in chain, call MAPRD to read the correct data block
! (ABLOCK), report position in the data block pointed to by RECNUM
! If TEMP & PILOT both wanted (UAWINDS), look for TEMP first.     !1.21
!-----------------------------------------------------------------------
                IF (USERTEMP.AND.USERPILOT) USERPILOT=.FALSE.     !1.21
                READ_CHNREPS = .TRUE.                                 !P

                DO WHILE (READ_CHNREPS)                               !P

                  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,
     &                       LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR,
     &                       RECNUM,IRECLN,CMSG(KEPCHN+1),'MSGRD')

!-----------------------------------------------------------------------
! Returns from MAPRD: IRECLN : length of report, CMSG : the report.
!
! As the reports are chained, there is a 23-byte index entry at the end
! of each report which points to the next report in the chain. The
! actual length of the report, therfore, is MSGLEN, as INDLEN=23.
! Put the 23-byte chained index entry in CTRAIL.
!-----------------------------------------------------------------------

                  MSGLEN(KEPCHN+1)=IRECLN-INDLEN
                  CTRAIL(KEPCHN+1)=
     &            CMSG(KEPCHN+1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! get the data block and record number of the next report in the chain
! from the 23-byte chained index entry. If the block or record number
! are zero, we are at the end of the chain. Set READ_CHNREPS=.FALSE.
!-----------------------------------------------------------------------

                  DBLOCK=ICHAR(CTRAIL(KEPCHN+1)(22:22))*256
     &                  +ICHAR(CTRAIL(KEPCHN+1)(23:23))

                  ABLOCK=1+NSQ+INXBLK+DBLOCK

                  RECNUM=ICHAR(CTRAIL(KEPCHN+1)(20:20))*256
     &                  +ICHAR(CTRAIL(KEPCHN+1)(21:21))

                  IF (DBLOCK.LE.0 .OR. RECNUM.LE.0) THEN              !P
                    READ_CHNREPS = .FALSE.                            !P
                  ENDIF                                               !P

!-----------------------------------------------------------------------
! check byte 17 of the trailor to see which part the report is.
!-----------------------------------------------------------------------

                  TRB17=ICHAR(CTRAIL(KEPCHN+1)(17:17))

                  ITS_PART(1)=((MOD(TRB17/2,2).EQ.0) .AND.
     &                        (MOD(TRB17/1,2).EQ.0))
                  ITS_PART(2)=((MOD(TRB17/2,2).EQ.0) .AND.
     &                        (MOD(TRB17/1,2).EQ.1))
                  ITS_PART(3)=((MOD(TRB17/2,2).EQ.1) .AND.
     &                        (MOD(TRB17/1,2).EQ.0))
                  ITS_PART(4)=((MOD(TRB17/2,2).EQ.1) .AND.
     &                        (MOD(TRB17/1,2).EQ.1))

                  IF (LFLAG) THEN
                    DO I=1,4
                      IF (ITS_PART(I)) WRITE(*,*) 'In UPRRET: part is ',
     &                'PART ',PMAP(I)
                    ENDDO
                  ENDIF

!-----------------------------------------------------------------------
! if the index entry contains a mix of TEMPS and PILOTS (TPMIX=.TRUE.)
! then decide from bit 6 of the trailor byte 17 whether the part is
! a TEMP or a PILOT (0=TEMP, 1=PILOT). If the users wants TEMPS and
! the part is a TEMP or the user wants PILOTS and the part is a PILOT
! then we want the part (subject to further checking). If the index
! entry does not contain a mixture of TEMPS and PILOTS, we want the
! part subject to further checking.                                   !B
!-----------------------------------------------------------------------

                  IF (TPMIX) THEN                                     !B
                    TRL_OB_TYPE=MOD(TRB17/4,2)                        !B

                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: TPMIX = TRUE, ',
     &                'TRL_OB_TYPE = ',TRL_OB_TYPE
                    ENDIF

                    IF (USERTEMP .AND. TRL_OB_TYPE.EQ.0) THEN         !B
                      WANT_OB_TYPE = .TRUE.                           !B
                    ELSEIF (USERPILOT .AND. TRL_OB_TYPE.EQ.1) THEN    !B
                      WANT_OB_TYPE = .TRUE.                           !B
                    ELSE                                              !B
                      WANT_OB_TYPE = .FALSE.                          !B
                    ENDIF                                             !B
                  ELSE                                                !B
                    WANT_OB_TYPE = .TRUE.                             !B
                  ENDIF                                               !B

                  IF (LFLAG) THEN
                    WRITE(*,*)'In UPRRET: WANT_OB_TYPE = ',WANT_OB_TYPE
                  ENDIF

!-----------------------------------------------------------------------
! check to see that the user wants the part, and check that we haven't
! already kept this part - we only want the most preferred of each
! part! Jump out of CHLOOP if we don't want it.
!-----------------------------------------------------------------------

                  IF (ITS_PART(1) .AND. WANT_PART(1) .AND.
     &            (.NOT.KEPT_PART(1)) .AND. WANT_OB_TYPE) THEN        !B
                    CONTINUE
                  ELSEIF (ITS_PART(2) .AND. WANT_PART(2) .AND.
     &            (.NOT.KEPT_PART(2)) .AND. WANT_OB_TYPE) THEN        !B
                    CONTINUE
                  ELSEIF (ITS_PART(3) .AND. WANT_PART(3) .AND.
     &            (.NOT.KEPT_PART(3)) .AND. WANT_OB_TYPE) THEN        !B
                    CONTINUE
                  ELSEIF (ITS_PART(4) .AND. WANT_PART(4) .AND.
     &            (.NOT.KEPT_PART(4)) .AND. WANT_OB_TYPE) THEN        !B
                    CONTINUE
                  ELSE
                    IF (LFLAG) WRITE(*,*)'In UPRRET: Part is not wanted'
                    GOTO 650
                  ENDIF

!-----------------------------------------------------------------------
! check the preferred flag for this part. We only want to keep the
! most preferred of each part.
!-----------------------------------------------------------------------

                  WASPREF=MOD(ICHAR(CTRAIL(KEPCHN+1)(17:17))/32,2).EQ.1

                  IF (LFLAG) THEN
                    WRITE(*,*)'In UPRRET: WASPREF = ',WASPREF
                  ENDIF

                  IF (WASPREF) THEN

!-----------------------------------------------------------------------
! See if time of receipt is in range. Time of receipt (NTOR) is in
! minutes after slot base hour. Actual time of receipt (ITORM)
! in minutes after slot base hour is calculated.                  !1.19b
!
! Check to see if NTOR is meant to be negative. NTOR is read from
! 2 bytes which means that the maximum positive value NTOR can have
! is 32768. If NTOR is greater than this value, it must be converted
! into a negative number. This is done simply by subtracting the max
! possible value that can be held  in a 2 byte integer when the sign
! bit is ignored (i.e. 2 to the power 16) from NTOR.                  !E
!-----------------------------------------------------------------------

                    NTOR=ICHAR(CTRAIL(KEPCHN+1)(18:18))*256
     &                  +ICHAR(CTRAIL(KEPCHN+1)(19:19))

                    IF (NTOR.GT.32768) THEN                           !E
                      NTOR=NTOR-65536                                 !E
                    ENDIF                                             !E

                    ITORM=IBLOOP*60+NTOR

                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: ITORM         =',ITORM
                      WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 =',
     &                IRTIM1,IRTIM2
                    ENDIF

!-----------------------------------------------------------------------
! Change check slightly from ITORM.GT.IRTIM2 to ITORM.GE.IRTIM2       !E
!-----------------------------------------------------------------------

                    IF ((IRTIM1.NE.0 .AND. ITORM.LT.IRTIM1) .OR.
     &                  (IRTIM2.NE.0 .AND. ITORM.GE.IRTIM2)) THEN     !E

                      IF (LFLAG) THEN
                        WRITE(*,*)'IN UPRRET: Part rejected on TOR'
                      ENDIF

                      GOTO 650
                    ENDIF

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
                      ENDIF
                    ENDDO

                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: Keep the part!!'
                    ENDIF

                  ELSE
                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: part not preferred'
                    ENDIF
                  ENDIF      !- waspref

650               CONTINUE                                            !P
                ENDDO        !- end of loop over reports in chain     !P

!-----------------------------------------------------------------------
! We now have an array of the parts that have passed the above checks.
! Call subroutine UPRPARTS to put the parts into FinalProfile
!-----------------------------------------------------------------------

                IF (KEPCHN.GT.0) THEN

                  CALL HRS2DT(HYEAR,HMONTH,HDAY,HHOUR,ICHR)           !A

! If UAWINDS, then if this ascent has both TEMP & PILOT parts,    !1.21
! and if we're now on PILOTs, put these winds in the second       !1.21
! half of FINALPROFILE and then combine TEMP & PILOT winds.       !1.21

                  IF (CTYPE.EQ.'UAWINDS'                          !1.21
     &                .AND.TPMIX.AND.USERPILOT) THEN              !1.21
                    CALL UPRPARTS(KEPT_PART,POSN_PART,KEPCHN,     !1.21
     &                          CTRAIL,CMSG,LFLAG,CINDX,ISTAT,    !1.21
     &                          WANTTEMPS,WANTWINDS,              !1.21
     &                          NUMTEMPS,NUMWINDS,RLAT,RLON,          !F
     &                          HHOUR,IMIN,HDAY,HMONTH,HYEAR,
     &                          REPORTTEXT,                       !1.21
     &                          FINALPROFILE(MXFINELMS/2+1),      !1.21
     &                          MXFINELMS,                        !1.21
     &                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS,
     &                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS,
     &                          ISECT1,                             !2.0
     &                          CALLSIGN,NEWBITIND,CCCC,TTAA,     !H!G!E
     &                          DISPL,WIDTH,SCALE,REFVAL,FOUND)     !Q!I
                    CALL UPRWINDS(FINALPROFILE,MXFINELMS,         !1.21
     &                            NUMWINDS)                       !1.21
                  ELSE                                            !1.21
                  CALL UPRPARTS(KEPT_PART,POSN_PART,KEPCHN,CTRAIL,CMSG,
     &                          LFLAG,CINDX,ISTAT,WANTTEMPS,WANTWINDS,
     &                          NUMTEMPS,NUMWINDS,RLAT,RLON,          !F
     &                          HHOUR,IMIN,HDAY,HMONTH,HYEAR,
     &                          REPORTTEXT,FINALPROFILE,MXFINELMS,
     &                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS,
     &                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS,
     &                          ISECT1,                             !2.0
     &                          CALLSIGN,NEWBITIND,CCCC,TTAA,     !H!G!E
     &                          DISPL,WIDTH,SCALE,REFVAL,FOUND)     !Q!I
!
! For UAWINDS retrieval call UPRWINDS even if all the parts are   !1.21
! PILOTs: this will sort the winds so that levels with pressure   !1.21
! come first, consistently with TEMP/PILOT mixtures.              !1.21
! (TRB17 gives the TEMP/PILOT bit in the chain's last trailer:    !1.21
!  if it says PILOT & there's no mixture, they're all PILOTs.)    !1.21
!
                    IF (CTYPE.EQ.'UAWINDS' .AND. .NOT.TPMIX       !1.21
     &                  .AND. MOD(TRB17/4,2).EQ.1) THEN           !1.21
                      CALL UPRWINDS(FINALPROFILE,MXFINELMS,       !1.21
     &                              NUMWINDS)                     !1.21
                    ENDIF                                         !1.21
                  ENDIF                                           !1.21

! If UAWINDS, and if this ascent has both TEMP & PILOT parts,     !1.21
! then if we've just done the TEMPs, look down the chain again    !1.21
! for PILOTs; otherwise reset flags for the next index entry.     !1.21

                  IF (CTYPE.EQ.'UAWINDS'.AND.TPMIX) THEN          !1.21
                    IF (USERTEMP) THEN                            !1.21
                      USERTEMP=.FALSE.                            !1.21
                      USERPILOT=.TRUE.                            !1.21
                      NTRYCHK=.FALSE.                             !1.21
                      GO TO 789                                   !1.21
                    ELSE                                          !1.21
                      USERTEMP=.TRUE.                             !1.21
                      USERPILOT=.FALSE.                           !1.21
                    ENDIF                                         !1.21
                  ENDIF                                           !1.21

!-----------------------------------------------------------------------
! Now put the data into the user's array. Only do this if there were
! no problems in UPRPARTS and the time in the final profile is in !2.1
! the retrieval window.  (N.B. minutes may be missing.)           !2.1
!-----------------------------------------------------------------------

                  IF (IMIN.GT.60) THEN                            !2.1
                    COMB_CENT_MIN=DT2HRS(IFIX(FINALPROFILE(16)),  !2.1
     &                                 IFIX(FINALPROFILE(18)),    !2.1
     &                                 IFIX(FINALPROFILE(20)),    !2.1
     &                                 IFIX(FINALPROFILE(22)))*60 !2.1
                    IF (FINALPROFILE(24).GE.0) THEN               !2.1
                      COMB_CENT_MIN=COMB_CENT_MIN                 !2.1
     &                        +IFIX(FINALPROFILE(24))             !2.1
                    ENDIF                                         !2.1
                    IF (COMB_CENT_MIN.LT.ITIMST .OR.              !2.1
     &                  COMB_CENT_MIN.GT.ITIMND) ISTAT=16         !2.1
                  ENDIF                                           !2.1

                  IF (ISTAT.EQ.16) THEN                               !D
                    ISTAT=0                                           !D
                    KEPCHN=0                                          !D
                  ELSE                                                !D

                    CALL ARRINDX(IDESC,NDES,IREPL,QCREQ,
     &                           LFLAG,FINALPROFILE,ARRELMNUM,ARRSEGNUM,
     &                           ARRSUBNUM,ARRNROWS,ARRSTYP,ARRSEGST,
     &                           ARRSEGLEN,ARRNSEGS,2,ARRNELREQ,DISPL,
     &                           ARRSOURCE,NEWSKLARR)                 !G

                    CALL UPREPID(CNTRY(IELOOP)(17:17),ISECT1(10))

                    CHRELM(1)(1:9)=CALLSIGN(1)(1:9)
                    LENCHR(1)=9
                    CHRELM(2)(1:4)=CCCC                               !H
                    LENCHR(2)=4                                       !H
                    CHRELM(3)(1:4)=TTAA                               !H
                    LENCHR(3)=4                                       !H

                    NREP=1                                            !J
                    NSEND=1                                           !J

                    CALL VALARR(DISPL,ARRNELREQ,ARRSOURCE,ISECT1,
     &                          CHRELM,LENCHR,IEXTRA,FINALPROFILE,
     &                          CNAM,REPORTTEXT,1,NSEND,NREP,         !J
     &                          ARRAY,NOBS,NELEM,IOB,II,CSTR,
     &                          CREPRT,LFLAG,LAT_SUBSCRIPT,           !J
     &                          LON_SUBSCRIPT,AREA,RPOLE)             !J

                    CHANRED=.TRUE.

                    CHLOOP=KEPCHN
                    ICHL2=KEPCHN
                    ICHL3=KEPCHN

!-----------------------------------------------------------------------
! call CheckPosition to see what room we have left in the users array
! etc and reset pointers, logicals to indicate this.
!-----------------------------------------------------------------------

                    CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,
     &                                  IELOOP,INXHRS,IOB,IREP1,ISHR2,
     &                                  ISTAT,IENTRIES,NOBS,NSEND,
     &                                  NXTCHN,NXTENT,NXTIME,NXTREP,
     &                                  CHANRED,INDXRED,NTRYCHK,FLAG999)

                    IF (FLAG999) GOTO 999

                  ENDIF !- istat                                      !D

                ENDIF  !- kepchn.gt.0 for more than 1 part

              ENDIF  !- end of chanred if block for more than 1 part.

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

              IF (.NOT.CHANRED) THEN

                KEPCHN=0

                READ_CHNREPS = .TRUE.                                 !P

                DO WHILE (READ_CHNREPS)                               !P

                  CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,
     &                       LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR,
     &                       RECNUM,IRECLN,CMSG(KEPCHN+1),'MSGRD')

!-----------------------------------------------------------------------
! Returns from MAPRD: IRECLN : length of report, CMSG : the report.
!
! As the reports are chained, there is a 23-byte index entry at the end
! of each report which points to the next report in the chain. The
! actual length of the report, therfore, is MSGLEN, as INDLEN=23.
! Put the 23-byte chained index entry in CTRAIL.
!-----------------------------------------------------------------------

                  MSGLEN(KEPCHN+1)=IRECLN-INDLEN
                  CTRAIL(KEPCHN+1)=
     &            CMSG(KEPCHN+1)(IRECLN-INDLEN+1:IRECLN)

!-----------------------------------------------------------------------
! get the data block and record number of the next report in the chain
! from the 23-byte chained index entry. If the block or record number
! are zero, we are at the end of the chain. Set READ_CHNREPS=.FALSE.
!-----------------------------------------------------------------------

                  DBLOCK=ICHAR(CTRAIL(KEPCHN+1)(22:22))*256
     &                  +ICHAR(CTRAIL(KEPCHN+1)(23:23))

                  ABLOCK=1+NSQ+INXBLK+DBLOCK

                  RECNUM=ICHAR(CTRAIL(KEPCHN+1)(20:20))*256
     &                  +ICHAR(CTRAIL(KEPCHN+1)(21:21))

                  IF (DBLOCK.LE.0 .OR. RECNUM.LE.0) THEN              !P
                    READ_CHNREPS = .FALSE.                            !P
                  ENDIF                                               !P

!-----------------------------------------------------------------------
! check byte 17 of the trailor to see which part the report is.
!-----------------------------------------------------------------------

                  TRB17=ICHAR(CTRAIL(KEPCHN+1)(17:17))

                  ITS_PART(1)=((MOD(TRB17/2,2).EQ.0) .AND.
     &                         (MOD(TRB17/1,2).EQ.0))
                  ITS_PART(2)=((MOD(TRB17/2,2).EQ.0) .AND.
     &                         (MOD(TRB17/1,2).EQ.1))
                  ITS_PART(3)=((MOD(TRB17/2,2).EQ.1) .AND.
     &                         (MOD(TRB17/1,2).EQ.0))
                  ITS_PART(4)=((MOD(TRB17/2,2).EQ.1) .AND.
     &                         (MOD(TRB17/1,2).EQ.1))

                  IF (LFLAG) THEN
                    DO I=1,4
                      IF (ITS_PART(I)) WRITE(*,*) 'In UPRRET: part is ',
     &                'PART ',PMAP(I)
                    ENDDO
                  ENDIF

!-----------------------------------------------------------------------
! if the index entry contains a mix of TEMPS and PILOTS (TPMIX=.TRUE.)
! then decide from bit 6 of the trailor byte 17 whether the part is
! a TEMP or a PILOT (0=TEMP, 1=PILOT). If the users wants TEMPS and
! the part is a TEMP or the user wants PILOTS and the part is a PILOT
! then we want the part (subject to further checking). If the index
! entry does not contain a mixture of TEMPS and PILOTS, we want the
! part subject to further checking.                                   !B
!-----------------------------------------------------------------------

                  IF (TPMIX) THEN                                     !B
                    TRL_OB_TYPE=MOD(TRB17/4,2)                        !B

                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: TPMIX = TRUE, ',
     &                'TRL_OB_TYPE = ',TRL_OB_TYPE
                    ENDIF

                    IF (USERTEMP .AND. TRL_OB_TYPE.EQ.0) THEN         !B
                      WANT_OB_TYPE = .TRUE.                           !B
                    ELSEIF (USERPILOT .AND. TRL_OB_TYPE.EQ.1) THEN    !B
                      WANT_OB_TYPE = .TRUE.                           !B
                    ELSE                                              !B
                      WANT_OB_TYPE = .FALSE.                          !B
                    ENDIF                                             !B
                  ELSE                                                !B
                    WANT_OB_TYPE = .TRUE.                             !B
                  ENDIF                                               !B

                  IF (LFLAG) THEN
                    WRITE(*,*)'In UPRRET: WANT_OB_TYPE = ',WANT_OB_TYPE
                  ENDIF

!-----------------------------------------------------------------------
! check to see that the user wants the part, and check that we haven't
! already kept this part - we only want the most preferred of each
! part! Jump out of CHLOOP if we don't want it.
!-----------------------------------------------------------------------

                  IF (ITS_PART(1) .AND. WANT_PART(1) .AND.
     &            WANT_OB_TYPE) THEN                                  !B
                   CONTINUE
                  ELSEIF (ITS_PART(2) .AND. WANT_PART(2) .AND.
     &            WANT_OB_TYPE) THEN                                  !B
                    CONTINUE
                  ELSEIF (ITS_PART(3) .AND. WANT_PART(3) .AND.
     &            WANT_OB_TYPE) THEN                                  !B
                    CONTINUE
                  ELSEIF (ITS_PART(4) .AND. WANT_PART(4) .AND.
     &            WANT_OB_TYPE) THEN                                  !B
                    CONTINUE
                  ELSE
                    IF (LFLAG) WRITE(*,*)'In UPRRET: Part is not wanted'
                    GOTO 660
                  ENDIF

!-----------------------------------------------------------------------
! See if time of receipt is in range.
!
! Check to see if NTOR is meant to be negative. NTOR is read from
! 2 bytes which means that the maximum positive value NTOR can have
! is 32768. If NTOR is greater than this value, it must be converted
! into a negative number. This is done simply by subtracting the max
! possible value that can be held  in a 2 byte integer when the sign
! bit is ignored (i.e. 2 to the power 16) from NTOR.                  !E
!-----------------------------------------------------------------------

                  NTOR=ICHAR(CTRAIL(KEPCHN+1)(18:18))*256
     &                +ICHAR(CTRAIL(KEPCHN+1)(19:19))

                  IF (NTOR.GT.32768) THEN                             !E
                    NTOR=NTOR-65536                                   !E
                  ENDIF                                               !E

                  ITORM=IBLOOP*60+NTOR

                  IF (LFLAG) THEN
                    WRITE(*,*)'In UPRRET: ITORM         =',ITORM
                    WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 =',IRTIM1,IRTIM2
                  ENDIF

!-----------------------------------------------------------------------
! Change check slightly from ITORM.GT.IRTIM2 to ITORM.GE.IRTIM2       !E
!-----------------------------------------------------------------------

                  IF ((IRTIM1.NE.0 .AND. ITORM.LT.IRTIM1) .OR.
     &                (IRTIM2.NE.0 .AND. ITORM.GE.IRTIM2)) THEN       !E

                    IF (LFLAG) THEN
                      WRITE(*,*)'IN UPRRET: Part rejected on TOR'
                    ENDIF
                    GOTO 660
                  ENDIF

!-----------------------------------------------------------------------
! check the preferred flag for this part. We only want to keep the
! most preferred of each part.
!-----------------------------------------------------------------------

                  WASPREF=MOD(ICHAR(CTRAIL(KEPCHN+1)(17:17))/32,2).EQ.1

                  IF (LFLAG) WRITE(*,*) 'In UPRRET: WASPREF = ',WASPREF

!-----------------------------------------------------------------------
! If the user doesn't want all the reports (VER.ne.2), check the "was
! preferred" flag WASPREF of the report. It it is set, we have found
! the preferred report. This entry will be kept. Jump out of the loop.
! If the user wants all reports in the chain, increment the pointer
! KEPCHN.
!-----------------------------------------------------------------------

                  IF (VER.NE.2) THEN
                    IF (WASPREF) THEN
                      IF (LFLAG) THEN
                        WRITE(*,*)'In UPRRET: Keep the part!!'
                      ENDIF
                      KEPCHN=KEPCHN+1
                      GOTO 661
                    ENDIF
                  ELSE
                    KEPCHN=KEPCHN+1
                    IF (LFLAG) THEN
                      WRITE(*,*)'In UPRRET: Keep the part!!'
                    ENDIF
                  ENDIF

660               CONTINUE                                            !P
                ENDDO        !- end of loop over reports in chain     !P

661             CONTINUE

                ICHL1=1
                ICHL2=KEPCHN
                ICHL3=1

                CHANRED=.TRUE.

              ENDIF  !- end of chanred if block.

!-----------------------------------------------------------------------
! for user wanting 1 part only, loop over message in chain not rejected
! by the checks above.
!-----------------------------------------------------------------------

              IF (KEPCHN.GT.0) THEN

                DO 670 CHLOOP=ICHL1,ICHL2,ICHL3

                  DO I=1,4
                    IF (WANT_PART(I)) THEN
                      KEPT_PART(I)=.TRUE.
                      POSN_PART(I)=1
                    ELSE
                      KEPT_PART(I)=.FALSE.
                      POSN_PART(I)=0
                    ENDIF
                  ENDDO

                  NREP=1
                  NSEND=NREP

                  CALL HRS2DT(HYEAR,HMONTH,HDAY,HHOUR,ICHR)           !A
                  CALL UPRPARTS(KEPT_PART,POSN_PART,1,CTRAIL(CHLOOP),
     &                          CMSG(CHLOOP),
     &                          LFLAG,CINDX,ISTAT,WANTTEMPS,WANTWINDS,
     &                          NUMTEMPS,NUMWINDS,RLAT,RLON,HHOUR,    !F
     &                          IMIN,HDAY,HMONTH,HYEAR,
     &                          REPORTTEXT,FINALPROFILE,MXFINELMS,
     &                          ARRELMNUM,ARRSEGNUM,ARRSUBNUM,ARRNROWS,
     &                          ARRSTYP,ARRSEGST,ARRSEGLEN,ARRNSEGS,
     &                          ISECT1,                             !2.0
     &                          CALLSIGN,NEWBITIND,CCCC,TTAA,     !H!G!E
     &                          DISPL,WIDTH,SCALE,REFVAL,FOUND)     !Q!I

!-----------------------------------------------------------------------
! Now put the data into the user's array. Only do this if there are
! no problems in UPRPARTS.                                            !D
!-----------------------------------------------------------------------

                  IF (ISTAT.EQ.16) THEN                               !D
                    ISTAT=0                                           !D
                  ELSE                                                !D

                    CALL ARRINDX(IDESC,NDES,IREPL,QCREQ,LFLAG,
     &                           FINALPROFILE,ARRELMNUM,ARRSEGNUM,
     &                           ARRSUBNUM,ARRNROWS,ARRSTYP,ARRSEGST,
     &                           ARRSEGLEN,ARRNSEGS,2,ARRNELREQ,DISPL,
     &                           ARRSOURCE,NEWSKLARR)                 !G

                    CALL UPREPID(CNTRY(IELOOP)(17:17),ISECT1(10))

                    CHRELM(1)(1:9)=CALLSIGN(1)(1:9)
                    LENCHR(1)=9
                    CHRELM(2)(1:4)=CCCC                               !H
                    LENCHR(2)=4                                       !H
                    CHRELM(3)(1:4)=TTAA                               !H
                    LENCHR(3)=4                                       !H

                    CALL VALARR(DISPL,ARRNELREQ,ARRSOURCE,ISECT1,
     &                          CHRELM,LENCHR,IEXTRA,FINALPROFILE,
     &                          CNAM,REPORTTEXT,1,NSEND,NREP,         !J
     &                          ARRAY,NOBS,NELEM,IOB,II,CSTR,CREPRT,
     &                          LFLAG,LAT_SUBSCRIPT,LON_SUBSCRIPT,    !J
     &                          AREA,RPOLE)                           !J

!-----------------------------------------------------------------------
! call CheckPosition to see what room we have left in the users array
! etc and reset pointers, logicals to indicate this.
!-----------------------------------------------------------------------

                    CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,
     &                                  IELOOP,INXHRS,IOB,IREP1,ISHR2,
     &                                  ISTAT,IENTRIES,NOBS,NSEND,
     &                                  NXTCHN,NXTENT,NXTIME,NXTREP,
     &                                  CHANRED,INDXRED,NTRYCHK,FLAG999)

                    IF (FLAG999) GOTO 999

                  ENDIF     !- istat.eq.16 check                      !D

670             CONTINUE   !- chloop for kept single parts

              ENDIF      !- kepchn.gt.0 for kept single parts

            ENDIF      !- want more than 1 part or just 1 part if block

! **********************************************************************
! **********************************************************************
! **                                                                  **
! **  3) Merged data retrieval: The user will get a combined profile  !I
! **                                                                  **
! **********************************************************************
! **********************************************************************

          ELSEIF (CRTYP.EQ.'M') THEN

            CALL MAPRD(IDSK,INXBLK,INXHRS,ISLOTHR,INDLEN,LFLAG,
     &                 LOCDFG,NELMIX,ABLOCK,INUM,CTRAN,IDATHR,
     &                 RECNUM,IRECLN,CMSG_MER(1),'MSGRD')         !1.19a

            MSGLEN(1)=IRECLN-INDLEN
            CTRAIL(1)=CMSG_MER(1)(IRECLN-INDLEN+1:IRECLN)         !1.19a

!-----------------------------------------------------------------------
! Check that the TOR is in range.
!-----------------------------------------------------------------------

            NTOR=ICHAR(CTRAIL(1)(18:18))*256 + ICHAR(CTRAIL(1)(19:19))

            IF (NTOR.GT.32768) THEN
              NTOR=NTOR-65536
            ENDIF

            ITORM=IBLOOP*60+NTOR

            IF (LFLAG) THEN
              WRITE(*,*)'In UPRRET: ITORM         = ',ITORM
              WRITE(*,*)'In UPRRET: IRTIM1,IRTIM2 = ',IRTIM1,IRTIM2
            ENDIF

            IF ((IRTIM1.NE.0 .AND. ITORM.LT.IRTIM1) .OR.
     &          (IRTIM2.NE.0 .AND. ITORM.GE.IRTIM2)) THEN

              IF (LFLAG) WRITE(*,*)'In UPRRET: Ob rejected on TOR'

!-----------------------------------------------------------------------
! Keep the observation. Find the start of the BUFR message and extract
! the sequence descriptor.
!-----------------------------------------------------------------------

            ELSE

              BUFRSTART=INDEX(CMSG_MER(1),BUFR)                   !1.19a
              BUFREND=INDEX(CMSG_MER(1),SEVENS)+3                 !1.19a

              ILOCALD=ICHAR(CMSG_MER(1)                           !1.19a
     &                (BUFRSTART+29:BUFRSTART+29))*256 +          !1.19a
     &                ICHAR(CMSG_MER(1)                           !1.19a
     &                (BUFRSTART+30:BUFRSTART+30))                !1.19a

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

              IF (NEWMRGCALL) THEN

                NEWMRGCALL=.FALSE.
                FOUNDLCC=.FALSE.
                FOUNDLRC=.FALSE.

                DO I=1,NDES
                  IF (IDESC(I).EQ.19) FOUNDLCC=.TRUE.
                  IF (IDESC(I).EQ.20) THEN
                    FOUNDLRC=.TRUE.
                    LRCPOINT=I
                  ENDIF
                ENDDO

                IF (FOUNDLCC .AND. .NOT.FOUNDLRC) THEN
                  FOUNDLCC=.FALSE.
                  WRITE(*,*)'In UPRRET: MDB WARNING: LEVL_CDTN_CODE ',
     &            'WILL BE MISSING. REQUEST LEVL_RPLTN_CONT AS WELL'
                ENDIF

              ENDIF

!-----------------------------------------------------------------------
! Call BITINDX to calculate the bit displacements of the elements
! requested by the user.
!-----------------------------------------------------------------------

              CALL BITINDX(CINDX,IDESC,NDES,IREPL,QCREQ,            !2.2
     &                     IFAIL,LFLAG,LOCALD,CMSG_MER(1),        !1.19a
     &                     ARRNELREQ,DISPL,WIDTH,SCALE,REFVAL,    !1.19a
     &                     MXFINELMS,NEWBITIND)                   !1.19a

              IF (IFAIL.EQ.16) THEN
                ISTAT=16
                IF (LFLAG) THEN
                  WRITE(*,*)'In UPRRET: Element Index not found'
                ENDIF
              ENDIF

!-----------------------------------------------------------------------
! If we are to set LEVL_CDTN_CODE (FOUNDLCC is true), call VALUE to
! get the LEVL_RPLTN_CONT value from the BUFR message using the DISPL
! and WIDTH values for LEVL_RPLTN_CONT. IT IS IMPORTANT NOT TO PASS
! DISPL TO VALUE AS IT WE BE ALTERED!!!!!. If LEVL_RPLTN count is .LE.
! to the number of levels requested by the user, set LEVL_CDTN_CODE to
! 0, otherwise to 8. If the user doesn't want the LEVL_CDTN_CODE or has
! requested it but not LEVL_RPLTN_CONT, set LEVL_CDTN_CODE to missing.
!-----------------------------------------------------------------------

              IF (FOUNDLCC) THEN
                IBEFOR=DISPL(LRCPOINT)
                I=VALUE(CMSG_MER(1)(BUFRSTART:),IBEFOR,           !1.19a
     &            WIDTH(LRCPOINT))                                !1.19a
                IF (I.LE.USERLEVS) THEN
                  ISECT1(11)=0
                ELSE
                  ISECT1(11)=8
                ENDIF
              ELSE
                ISECT1(11)=-9999999
              ENDIF

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

              IF (FOUND(34)) THEN                                     !Q
                WRITE(HELN,'(I5)')BUFREND-BUFRSTART+1                 !Q
                CALL VALUSR(CMSG_MER(1)(BUFRSTART:),              !1.19a
     &                      ARRNELREQ,DISPL,WIDTH,SCALE,          !1.19a
     &                      REFVAL,ARRAY,IOB+1,NOBS,CSTR,         !1.19a
     &                      HELN//CMSG_MER(1)(BUFRSTART:BUFREND), !1.19a
     &                      CREPRT,LFLAG,ISECT1,CHRELM,           !1.19a
     &                      LENCHR)                               !1.19a
              ELSE                                                    !Q
                HELN='    1'                                          !Q
                CALL VALUSR(CMSG_MER(1)(BUFRSTART:),ARRNELREQ,    !1.19a
     &                      DISPL,WIDTH,SCALE,REFVAL,ARRAY,       !1.19a
     &                      IOB+1,NOBS,CSTR,HELN//' ',CREPRT,     !1.19a
     &                      LFLAG,ISECT1,CHRELM,LENCHR)           !1.19a
              ENDIF                                                   !Q

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

              CALL CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,IELOOP,
     &                            INXHRS,IOB,IREP1,ISHR2,ISTAT,
     &                            IENTRIES,NOBS,NSEND,NXTCHN,
     &                            NXTENT,NXTIME,NXTREP,CHANRED,
     &                            INDXRED,NTRYCHK,FLAG999)

              IF (FLAG999) GOTO 999

            ENDIF  !- ITORM check for merged retrieval.

!=======================================================================
! Upper Air Retrieval from Merged data. End Of New Section            !I
!=======================================================================

          ELSE                                                        !I
            WRITE(*,*)'In UPRRET: Invalid CRTYP. CRTYP = ',CRTYP      !I
          ENDIF                                                       !I

          NTRYCHK = .FALSE.
          CHANRED = .FALSE.

        ENDDO      !- end of loop over kept index entries (ieloop)

        IENT1   = 1
        INDXRED = .FALSE.

799   CONTINUE   !- end of loop over index blocks (ibloop)

      IF (IOB.EQ.0) THEN
        IF (LCONT) THEN
          ISTAT=0
        ELSE
          ISTAT=8
        ENDIF
      ENDIF

999   CONTINUE

      IF (II.NE.0) THEN                                            !1.16
        IOBS=II                                                    !1.16
      ELSE                                                         !1.16
        IOBS=IOBS-1                                                !1.16
      ENDIF                                                        !1.16

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/',
     &       I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',
     &       I2.2,1X,I2.2,'Z')                                     !1.18

      RETURN
      END

!***********************************************************************
!**   Subroutine CheckPositions
!***********************************************************************

      SUBROUTINE CheckPositions(CHLOOP,IBLOOP,ICHL2,ICHL3,IELOOP,
     &                          INXHRS,IOB,IREP1,ISHR2,ISTAT,ITRIES,
     &                          NOBS,NSEND,NXTCHN,NXTENT,NXTIME,
     &                          NXTREP,CHANRED,INDXRED,NTRYCHK,FLAG999)

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

      IMPLICIT NONE

      EXTERNAL EOCHN

!-----------------------------------------------------------------------
! declare variables
!-----------------------------------------------------------------------

      INTEGER CHLOOP        !- chained report loop number
      INTEGER IBLOOP        !- index block loop number
      INTEGER ICHL2         !- end of chained reports loop
      INTEGER ICHL3         !- chained reports step
      INTEGER IELOOP        !- index entries loop number
      INTEGER INXHRS        !- no. of hours per index block
      INTEGER IOB           !- last ob number passed to users array
      INTEGER IREP1
      INTEGER ISHR2
      INTEGER ISTAT
      INTEGER ITRIES
      INTEGER NOBS
      INTEGER NSEND
      INTEGER NXTCHN
      INTEGER NXTENT
      INTEGER NXTIME
      INTEGER NXTREP

      LOGICAL CHANRED
      LOGICAL EOCHN        !- declare function
      LOGICAL FLAG999
      LOGICAL INDXRED
      LOGICAL NTRYCHK

      SAVE

      IOB =IOB+(NSEND-IREP1)+1 ! increment no of reports transferred
      NSEND = 0
      NXTREP = 1
      IREP1 = 1
      FLAG999=.FALSE.

      IF (IOB.EQ.NOBS) THEN            ! if array is full

!-----------------------------------------------------------------------
! increment loop variables for entry next time
!-----------------------------------------------------------------------

        IF (EOCHN(CHLOOP,ICHL2,ICHL3)) THEN ! if end of chain
          NXTCHN = 1                   ! start of chain for next entry)
          CHANRED = .FALSE.
          IF (IELOOP+1.GT.ITRIES) THEN ! if no more entries
            NXTENT = 1                 ! first entry
            IF (IBLOOP+INXHRS.GT.ISHR2) FLAG999=.TRUE.
            NXTIME=IBLOOP+INXHRS       ! in next entry (if there is one)
            INDXRED=.FALSE.
          ELSE
            NXTENT=IELOOP+1        ! next entry
            NXTIME=IBLOOP          ! in same index
          ENDIF
          NTRYCHK = .FALSE.
        ELSE                       ! if not end of chain
          NXTCHN = CHLOOP + ICHL3  ! next in chain (add +1 or -1)
          NXTENT = IELOOP
          NXTIME = IBLOOP
        ENDIF
        ISTAT = 4                  ! more data to come (maybe)
        FLAG999=.TRUE.
      ENDIF

      RETURN
      END
