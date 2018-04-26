      PROGRAM MERGE

!-----------------------------------------------------------------------
!
! PROGRAM       : MERGE   (which can also operate in SWEEP mode!)
!
! PURPOSE       : To add model data to observed data stored in the mdb
!                 (or make messages with missing model data to "sweep
!                  up" old obs)
!
! DATA TYPE(S)  : All those to be archived with model values
!
! CALLS         : DEBUFR, ENBUFR, TABLEB, LOCALD, DESFXY,           !2.2
!                 MERMDB, MERSY9, MERVAL, MERBITX,                  !2.2
!                 MODELA, MODELB, MODLVA, MODLVB,
!                 AIRSTO, BUFREP,                                    !5
!                 BUFVAL, ASC2EB, EB2ASC, INDLALO, LATBOX,          !2.2
!                 CHAR2, ICHAR2.                                    !2.2
!                 PARM sets MERGE or SWEEP mode.
!                 METDB_COPEN,METDB_CREAD_DIR
!
! INPUTS        :  FT01 - merge table
!                 (FT03 reserved for MDB)
!                  FT11-FT14 model inputs
!                  FT10 - request file
!                  FT20 - namelist input (optional)
! OUTPUT           FT02 - data base
!
! STRUCTURE     : Read merge table,
!                   keeping names of elements to retrieve from MDB
!                     & arrays of pointers to sources of values
!                     needed to make output BUFR message.
!                 Read extraction request with cutoff,
!                   add element list made above
!                     & decide how many obs to put in this message
!                     (this depends on maximum number in merge table
!                     and on grouping of original data).
!                 Check coordinate(s) to make sure model data is
!                 in step with data from MDB,
!                   encode message
!                     & call appropriate program to store it
!                     after starting index entry for this data type
!                     (including any non-standard fields!)
!
! REVISION INFO :
!
! $Workfile: merge.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 13/02/2013 16:10:51$
!
! CHANGE RECORD :
!
! $Log:
!  6    Met_DB_Project 1.5         13/02/2013 16:10:51    Brian Barwell
!       Modification for storage of AMDAR merge data in new format data set.
!  5    MetDB_Refresh 1.4         29/06/2012 10:02:08    Brian Barwell   ERS-2
!       related code removed. Small modification for OPENROAD data.
!  4    MetDB_Refresh 1.3         14/06/2011 10:09:29    Sheila Needham  Fix
!       for CSTR array with multiple reports
!  3    MetDB_Refresh 1.2         02/06/2011 14:34:13    Sheila Needham  Tidied
!        up
!  2    MetDB_Refresh 1.1         10/05/2011 17:46:28    Sheila Needham
!       Changes for F95
!  1    MetDB_Refresh 1.0         11/04/2011 14:54:22    Sheila Needham
!       Initial F77 versions
! $
! Revision 2.9  2006/02/06 11:53:37  usmdb
! 2.9.  20 February 2006.  Brian Barwell.  CHG021506.
! Increase the size of the DBDATA array.
!
! Revision 2.8  2003/10/07 10:02:09  usmdb
! 2.8.  20 October 2003.  Brian Barwell.  Change 97/03.
! Parameter MAXARR increased from 25000 to 27000 for MODIS.
!
! Revision 2.7  2003/05/06  08:36:56  08:36:56  usmdb (MetDB account c/o usjh)
! 2.7.  19 May 2003.  Brian Barwell.  Change 58/03.
! Remove call to SATIND. Ensure NEWFMT is initialised before use.
!
! Revision 2.6  2003/03/31  09:04:18  09:04:18  usmdb (MetDB account c/o usjh)
! If the background and observed longitude do not match, check
! for a background longitude of -180.0 as opposed to an observed
! longitude of +180.0 and if this is the case the merge can
! continue. S.Cox.
!
! Revision 2.5  2003/03/05  16:23:49  16:23:49  usmdb (MetDB account c/o usjh)
! 2.5.  17 March 2003.  Brian Barwell.  Change 29/03.
! New code to pass data selection value to created index entries.
! Call to SSMIND removed.
!
! Revision 2.4  2002/11/04  14:30:49  14:30:49  usmdb (MetDB account c/o usjh)
! Removed ATOVS-specific code as ATOVS merge now uses BUFREP.
! Added checks of return codes from BUFREP and a summary - S.Cox
! Rewrote dodgy code when calling MERVAL (marked !2.4a) - B.R.B.
!
! Revision 2.3  2002/07/11  09:56:29  09:56:29  usmdb (Generic MetDB account)
! Change 59/02.  Brian Barwell.  New LOGIGAL variable used to check
! if storage data set is opened (after SATOBS failure, 10/7/02).
!
! Revision 2.2  2002/04/09  11:32:43  11:32:43  usmdb (Generic MetDB account)
! 2.2.  15 April 2002.  Brian Barwell.  Change 32/02.
! Extend job to merging of data using new format storage data sets.
! One message now made for each call to MDB, i.e. no 'splitting'.
!
! Revision 2.1  2002/02/04  11:42:41  11:42:41  usmdb (Generic MetDB account)
! Changed declaration of DTYPE and MERGE_TABLE and put them
! within a pre-processor statement - S.Cox
!
! Revision 2.0  2001/12/05  09:12:57  09:12:57  usmdb (Generic MetDB account)
! Calls AIRSTO instead of AMDSTO for SATOB, BATHY, TESAC - S.Cox
!
! Revision 1.29  2001/10/31  09:59:49  usmdb
! Increase size of MetDB request variable from 3000 to 10000 to
! cope with longer OPS request strings for ATOVSG - S.Cox
!
! Revision 1.28  2001/05/08  13:10:54  usmdb
! Change STOP 999 to STOP for NBATCH=0. This type of merge failure has
! merge failure has been downgraded from an ERROR to a WARNING - S.Cox
!
! Revision 1.27  2001/03/23  10:32:09  usmdb
! 26 March 2001    C Long
! For WINPRO call AIRSTO rather than AMDSTO.
!
! Revision 1.26  2001/01/08  14:29:36  usmdb
! 22 Jan 2001    C Long
! Avoid merging nothing when there is one & only one ob
! (as often happens with mesoscale AIREPs)
!
! Revision 1.25  2000/11/07  12:04:08  usmdb
! 20 Nov 2000    C Long
! The 1.24 messages show that the 1.23 check stops adding model
! values when a height has been corrected.  Improve the check so that
! only one element per level (in this case pressure, not height, as
! pressure comes first) is checked.
!
! Revision 1.24  2000/10/04  15:51:34  usmdb
! 16 Oct 2000    C Long
! Print error message (but still carry on) if levels out of step
!
! Revision 1.23  2000/08/09  15:06:06  usmdb
! 21 August 2000
! Check that levels are in step (if merge table says so) and stop
! adding model values if they are out of step (but otherwise
! continue) - C.Long
! 1.23b Addition of subtype BUOYPROF (for FOAM merge). Also
!       move section of code that sets IBOYLEV so it now gets
!       set for BUOY with drogues - S.Cox
! 1.23c Set SATLITE if ESAUWI (& remove LASS72 from list) - C.Long
!
! Revision 1.22  2000/07/10  11:20:32  usmdb
! 17 July 2000     C Long
! Get delayed replication count (e.g. number of levels) from model
! input, from replication descriptor output by decode.
!
! Revision 1.21  2000/06/14  13:23:23  usmdb
! 19 June 2000    C Long
! Do not set the fast encoding flag for BUOYs. Keeping the profile
! rules out fast encoding.
!
! Revision 1.20  2000/06/14  11:00:56  usmdb
! 14 June 2000
! Increase MAXARR array size to 25000 - C.Long
!
! Revision 1.19  2000/04/07  08:47:41  usmdb
! Increase size of MDB request character string from CHAR*999 to
! CHAR*3000. Ref MetDB task 354 - S.Cox
!
! Revision 1.18  2000/03/10  10:25:46  usmdb
! 20 March 2000     C Long
! 1.18  Call INDLALO to put lat/long in index entry.
! 1.18a Store BUOYs with AIRSTO rather than AMDSTO.
! 1.18b Set buoy profile flag from number of levels.
!
! Revision 1.17  99/12/17  11:19:24  usmdb
! Operational 20-03-2000 - S.Cox
! Initialise K and NOBSREQ before calling DEBUFR. Not doing this
! caused problems on the IBMSP machine.
!
! Revision 1.16  99/09/17  07:48:24  usmdb
! 20 Sept 99      C Long
! Put flight level rather than seconds at end of identifier in index
! entry for AMDARs if seconds not reported.
!
! Revision 1.15  99/09/09  10:16:37  usmdb
! 20-09-1999 - S.Cox
! a) Addition of subtype ESAHRWVW
! b) Correct setting of index ENTRY for passing to AIRSTO.
!
! Revision 1.14  99/07/12  16:06:14  usmdb
! Keep the 9-groups (if any) as a character string with the merged message
!
! Revision 1.13  99/06/10  14:41:21  usmdb
! 21st June 1999 - S.Cox
! Small change so that a COND code=999 is generated if the
! job stops due to a short assoc file - S.Cox
!
! Revision 1.12  99/05/06  14:58:07  usmdb
! 17-05-1999 S.Cox
! a) Addition of SATIND storage to MERGE routine to allow the merge of
!    the EUMETSAT products (ESAHRVW, ESACMW, ESACSWVW).
! b) Terminate merge if NBATCH.LE.0
!
! Revision 1.11  99/02/23  15:13:16  usmdb
! 23 February 1999, Brian Barwell.
! Change to allow PAOBS merge.
!
! Revision 1.10  99/02/11  12:06:25  usmdb
! 15-02-1999 C.Long, S.Cox
! a) Call MERVAL rather than VALOUT to avoid confusion over
!    versions - C.Long
! b) Change format of "data out of step" message. Can now report on
!    ob number out of step up to ob 999999 - S.Cox
!
! Revision 1.9  99/01/14  14:09:31  usmdb
! 18-01-1999 S.Cox
! a) Change to cope with new MAP overflow system.
! b) Change to allow SSMI merge.
!
! Revision 1.8  98/10/15  10:16:07  usmdb
! 19-10-98 S.Cox
! a) Addition of subtypes ATOVSG & ATOVSL to the merge software. They
!    are treated in the same way as other satellite subtypes, except
!    the storage is through ATOVIND, not ERSIND.
! b) Production of associated data on the T3E is to change to run on
!    more than one PE. This has the consequence of producing more than
!    one BUFR message containing call strings. This change is to update
!    the MERGE code to cope.
! c) Y2K correct the TOR year that is stored in BUFR section 1 of the
!    message. In BUFR section 1, the year 2000 = 100, not 00!
!
! Revision 1.7  98/08/12  08:26:39  usmdb
! BATHY/TESAC ident is either characters or 5-fig number (SHPSYN
! should be same, but number set in characters!)                      !O
!
! Revision 1.6  1998/06/19 11:32:29  usmdb
! Various
!
! Revision 1.5  98/06/11  15:53:44  usmdb
! Abend with dump if model & retrieved data out of step. Correct
! subscript used to work out SATOB lat/long box. Avoid missing
! identifiers in AMDAR index entries. Set SATOB section in byte 17 to 4
! if one retrieved. Make change !k apply to GOESAMW too. Message to
! ignore Fortran error if model input missing. Return with print if
! I/O error reading model file                                        !N
! reading model file.
!
! Revision 1.4  98/03/13  15:08:07  usmdb
! Set SATOB section in byte 17 to 4 if none retrieved                 !M
! Avoid missing identifiers in AMDAR index entries                    !L
! Correct subscript used to work out SATOB lat/long box               !K
! Abend with dump if model & retrieved data out of step               !J
! Treat GOESAMW like SATOB (but store with AIRSTO)                    !I
! Set missing CCCC in section 1 of message to all ones                !H
! Dont use retrieved replication count greater than max in request    !G
! Bigger values array, needed for GLOSS & SATOB sweep. Yet another
! attempt to recognise end of MDB data! Dont unset byte 17 for SATOBS !F
! Do not leave byte 17 of index entry unset or lose last ob when
! MDBRC=0.                                                            !E
!
! Revision 1.3  1998/02/06 11:34:23  uspm
! Longer string for requests                                          !C
!
! Revision 1.2  1997/10/29 12:39:07  uspm
! Skip encode when message has values in same place as last message
! made - just set values that are different                           !A
!
! Revision 1.1  1997/09/23 13:08:10  uspm
! Initial revision
!
! Made from ERSMERGE with some features of AMERGE (AMDARs) Nov 96
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2013 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE desfxy_mod
USE char2_mod
USE ichar2_mod
USE debufr_mod
USE tableb_mod
USE eb2asc_mod
USE asc2eb_mod
USE indlalo_mod
USE latbox_mod
USE locald_mod
USE enbufr_mod
USE bufrep_mod
USE airsto_mod
USE mermdb_mod
USE modlva_mod
USE modlvb_mod
USE modela_mod
USE modelb_mod
USE bufval_mod
USE merval_mod
USE merbitx_mod
USE mersy9_mod

      IMPLICIT NONE

INTEGER,PARAMETER ::  MAXARR=27000          !
INTEGER,PARAMETER ::  MAXND=500           !
INTEGER,PARAMETER ::  MAXDIM=2*MAXARR
INTEGER,PARAMETER ::  CDIM=1000
INTEGER,PARAMETER ::  MAXBLK=27998

INTEGER ::BITS(MAXND)     ! from MERBITX: bits before value     !a
INTEGER ::BLKSIZ          ! blocksize of output data set
CHARACTER(MAXBLK):: BLOCK ! to read descriptors from output d/set
REAL    ::BOX(4)          ! lat/long box to go in index entry
INTEGER ::BSQ             ! BUFR displacement number         !1.8b
CHARACTER(MAXBLK):: BUFMES_A  ! BUFR message for anal i/p        !1.8b
CHARACTER(MAXBLK)::BUFMES_B  ! BUFR message for bkgd i/p        !1.8b
CHARACTER(MAXBLK):: BUFMES_LA ! BUFR message for lev anal i/p    !1.8b
CHARACTER(MAXBLK):: BUFMES_LB ! BUFR message for lev bkgd i/p    !1.8b
REAL      ::BVAL            ! Background value for lat/lon check!2.6
LOGICAL   ::CALL_STRING     ! TRUE to read MDB callstring      !1.8b
CHARACTER(4):: CCCC          ! collecting centre (retrieved)
LOGICAL ::CLOSED          ! TRUE if storage data set not open !2.3
LOGICAL ::  CMPRES          ! to compress output message if >1 ob
CHARACTER(20):: CNAME        ! file name
CHARACTER(8):: CONST         ! to read constants from merge table
CHARACTER(24):: CSTR(CDIM)   ! to retrieve character values
CHARACTER(10000) :: CSTR1    ! to equivalence for enbufr
REAL    ::  DANAL(7*MAXARR)   ! model input from analysis file
CHARACTER(8):: DATA          ! data type for MDB call
INTEGER ::  DATIME(5)       ! date/time of data (retrieved)
REAL    ::  DBDATA(MAXDIM) ! values retrieved from MDB         !2.9
REAL    ::  DBKGD(7*MAXARR)   ! model input from background file
REAL    ::  DIFF            ! model/retrieval coordinate difference
REAL    ::  DIFFL           ! model/retrieval coordinate diff   !2.6
REAL    ::  DLEVLA(7*MAXARR)  ! model input: analysis, model levels
REAL     :: DLEVLB(7*MAXARR) ! model input: background, model levels
CHARACTER(23):: ENTRY        ! index entry
CHARACTER(1):: FIGURE(8)     ! used to set SATOB section in index
LOGICAL  :: FAST_ENCODE     ! set if 1 ob/message & no profile    !a
LOGICAL  :: FIRST_REQ       ! TRUE if 1st request string read  !1.8b
LOGICAL  :: FLAGS(9)        ! Processing flags (BUFREP)          !5
CHARACTER(10000)::FOR      ! MDB request from model input     !1.29
INTEGER ::  FS(MAXND)       ! from MERBITX: F in descriptor       !a
CHARACTER(9) :: GETCHR        ! to get character value from MDB string
INTEGER ::  I               ! used to loop round merge table lines,
!                                  first reading then getting values
INTEGER  :: IBLOCK          ! WMO block number
INTEGER  :: IBOYLEV         ! subscript of count in MDB input !1.18b
INTEGER  :: IBUOY           ! buoy identifier
INTEGER  :: ICCCC           ! subscript of CCCC from MDB
CHARACTER(9):: ID            ! identifier for AIRSTO
INTEGER  :: IDAY            ! subscript of day from MDB
INTEGER  :: IDENT           ! subscript of ident from MDB
INTEGER  :: IDSEL           ! subscript of data selection value !2.5
INTEGER  :: IDENT2          ! subscript of 2nd poss AMDAR ident  !l
INTEGER  :: IERROR          ! IO error status
INTEGER  :: IDESC           ! descriptor from BUFR message     !1.8b
INTEGER  :: IFF             ! F from FXXYYY                    !1.8b
INTEGER  :: IFT             ! FT number: output data base
INTEGER  :: IFTA            ! FT number: model input (analysis)
INTEGER  :: IFTB            ! FT number: model input (background)
INTEGER  :: IFTLVA          ! FT number: model levels (analysis)
INTEGER  :: IFTLVB          ! FT number: model levels (background)
INTEGER  :: IHOUR           ! subscript of hour in MDB input
INTEGER  :: ILAT            ! subscript of latitude in MDB input
INTEGER  :: ILATVAL         ! latitude subscript in SATOB output  !k
INTEGER  :: ILEVEL          ! subscript of level in MDB input !1.16
INTEGER  :: ILONG           ! subscript of longitude in MDB input
INTEGER  :: IMIN            ! subscript of minute in MDB input
INTEGER  :: IMONTH          ! subscript of month in MDB input
INTEGER  :: IOBA            ! obs used from model input record (anal
INTEGER  :: IOBB            ! obs used from model input record (back
INTEGER  :: IOBLA           ! obs used from model-level record (anal
INTEGER  :: IOBLB           ! obs used from model-level record (back
INTEGER  :: IOBMDB          ! obs used from MDB call
INTEGER  :: IOPEN           ! I/O mode
INTEGER  :: IPOINT          ! pointer to character values from MDB
INTEGER  ::  IPRSEN          ! subscript of press-sensor flag (MDB)
INTEGER  ::  IOS             ! I/O return code
INTEGER  ::  IRC             ! return code from merge table read
INTEGER  ::  IRCA            ! return code from model input read
INTEGER  ::  IRCB            ! return code from model input read
INTEGER  ::  IRCLA           ! return code from model input read
INTEGER  ::  IRCLB           ! return code from model input read
INTEGER  ::  IREPL           ! pointer to latest replication in list
INTEGER  ::  IREPTXT         ! pointer to report text          !1.14
INTEGER  ::  IREQ            ! pointer to element list being read
INTEGER  ::  ISATID          ! satellite identifier
INTEGER  ::  ISECNO          ! subscript of SATOB section number
INTEGER  ::  ISECOND         ! subscript of second in MDB input
INTEGER  ::  ISTN            ! WMO station number
INTEGER  ::  ITEMS(12)       ! Processing control data for BUFREP!2.2
INTEGER  ::  ITOR(5)         ! subscripts of time of receipt from MDB
INTEGER  ::  ITTAA           ! subscript of TTAA from MDB
INTEGER  ::  ITUP            ! subscript of u/a land/ship from MDB
INTEGER  ::  IUATYPE         ! byte 17 for u/a index entry
INTEGER  ::  IVERT           ! not set for SATOB section 7
INTEGER  ::  IVER            ! Table version number
INTEGER  ::  IVALUE          ! integer form of value to go in message
INTEGER  ::  IXX             ! XX from FXXYYY                   !1.8b
INTEGER  ::  IYEAR           ! subscript of year in MDB input
INTEGER  ::  IYY             ! YYY from FXXYYY                  !1.8b
INTEGER  ::  J
INTEGER  ::  K
INTEGER  ::  L               ! length of model input message
INTEGER  ::  LANAL(MAXND)    ! model input subscripts (analysis)
INTEGER  ::  LAST_LEVEL_CHECKED ! to avoid checking both press !1.25
                             ! & height in step in profile  !1.25
INTEGER   :: LASTVAL         ! number of values put in output array
INTEGER   LBKGD(MAXND)    ! model input subscripts (background)
CHARACTER(1)::  LCHAR(MAXND)  ! set to C if value is characters
INTEGER   :: LCONST(MAXND)   ! constant values from merge table
INTEGER   :: LENAME          ! length of element name from table
INTEGER   :: LEVEL           ! flight level (for AMDAR index)  !1.16
LOGICAL   :: LEVELS_OUT_OF_STEP!(only for reported TEMP levels)!1.23
INTEGER   :: LEVLA(MAXND)    ! model-level input subscripts (anal)
INTEGER   :: LEVLB(MAXND)    ! model-level input subscripts (b/g)
CHARACTER(1)::  LF            ! F (in FXXYYY) of descriptor in table
CHARACTER(1):: LINDEX(MAXND) ! set to X if value used in index
INTEGER   :: LMDB(MAXND)     ! subscripts of values from MDB input
CHARACTER(3000)::  LMLIST     ! element list made from table (short
CHARACTER(32)::  LNAME        ! element name just read from table
INTEGER   :: LREPTXT         ! length of report text           !1.14
INTEGER   :: LX(MAXND)       ! XX (in FXXYYY) of descriptor in table
INTEGER   :: LY(MAXND)       ! YYY (in FXXYYY) of descriptor in table
INTEGER   :: MAXNOBS         ! obs per MDB call (from table)
INTEGER   :: MDBRC           ! return code from MERMDB
INTEGER   :: MDESCR(3*MAXARR)
INTEGER   :: MERGER          ! sequence descriptor for output message
REAL      :: MISSIN          ! missing data indicator -9999999
INTEGER   :: MOBMAR          ! mobile/marine flag (REPRT_IDNY)
CHARACTER(5)::  MODE          ! 'MERGE' or 'SWEEP'
INTEGER   :: NANAL           ! number of analysis subscripts in table
INTEGER   :: NBATCH          ! no. of obs in batch to copy to VALUES
INTEGER   :: NBITS           ! no. of bits already in OUTMESS   !2.4a
INTEGER   :: NBKGD           ! number of backgrnd subscripts in table
INTEGER   :: NBLOKS          ! number of blocks in output data set
INTEGER   :: NBOYLEV         ! count of levels in BUOY profile !1.18b
INTEGER   :: NCCCC           ! code figure for collecting centre
INTEGER   :: NCHARS          ! number of characters to go in bit string
INTEGER   :: NDA             ! number of descriptors in model input
INTEGER   :: NDB             ! number of descriptors in model input
INTEGER   :: NDLA            ! number of descriptors in model input
INTEGER   :: NDLB            ! number of descriptors in model input
INTEGER   :: NDONE           ! number of obs (not messages) merged
INTEGER   :: NELM            ! no. of elements & repls in table
INTEGER   :: NELM_THIS_OB    ! NELM adjusted for replications
LOGICAL   :: NEWFMT          ! flag for new format storage d.s.  !2.2
INTEGER   :: NL              ! table line number in value loop
INTEGER   :: NLA             ! anal subscript increment in replicatio
INTEGER   :: NLB             ! b/g subscript increment in replication
INTEGER   :: NLEVLA          ! number of model-level subscripts (anal
INTEGER   :: NLEVLB          ! number of model-level subscripts (b/g)
INTEGER   :: NLINES          ! governs value loop (-1 unless repl)
INTEGER   :: NLLA            ! subscript increment in replication
INTEGER   :: NLLB            ! subscript increment in replication
INTEGER   :: NLMDB           ! MDB subscript increment in replication
INTEGER   :: NLMODL          ! number of model levels           !1.22
INTEGER   :: NLMYYY          ! descriptor subscript for NLMYYY  !1.22
INTEGER   :: NLREPT          ! number of reported levels        !1.22
INTEGER   :: NLRYYY          ! descriptor subscript for NLRYYY  !1.22
INTEGER   :: NM              ! number of descriptors for ENBUFR
INTEGER   :: NMDB            ! number of elements (for MDB call)
INTEGER   :: NOBSA           ! number of obs from this anal message
INTEGER   :: NOBSB           ! number of obs from this b/g message
INTEGER   :: NOBSLA          ! number of obs from this mod-lev anal
INTEGER   :: NOBSLB          ! number of obs from this mod-lev b/g
INTEGER   :: NOBSMDB         ! number of obs from this MDB call
INTEGER   :: NOBSREQ         ! only used in DEBUFR call for request
INTEGER   :: NOW(8)          ! current date/time
INTEGER   :: NREPL           ! number of replications listed
INTEGER   :: NSEC            ! SATOB section number (from MDB)
INTEGER   :: NSEQBL          ! block number of descriptor sequence
INTEGER   :: NSTORED         ! No. of messages stored            !2.4
INTEGER   :: NTIMES          ! governs value loop (-1 unless repl)
INTEGER   :: NUMBAD          ! No. of uncreatable index entries  !2.4
INTEGER   :: NUMDUP          ! No. of duplicate msgs rejected    !2.4
INTEGER   :: NUMFUL          ! No. of msgs rejected by full d/s  !2.4
INTEGER   :: NUMOLD          ! No. of messages too old to store  !2.4
INTEGER   :: NUMPRE          ! No. of msgs with date in future   !2.4
INTEGER   :: NUMREC          ! Database record number
CHARACTER(MAXBLK)::  OUTMESS   ! BUFR message to be stored
INTEGER   :: REFVALS(MAXND)  ! from MERBITX                        !a
INTEGER   :: REPL(99)        ! replications from table (line numbers)
INTEGER   :: REPL_BITS       ! number of bits in one replication   !a
INTEGER   :: REPL_EXTRA      ! number of bits added by replications!a
INTEGER   :: REPLEN
INTEGER   :: REPLEN9         ! length of LNDSYN 9-groups       !1.14
CHARACTER(333)::  REPTXT      ! LNDSYN report text              !1.1
REAL     ::  RVAL            ! retrieved value for lat/lon check !2.6
INTEGER :: SATOB_ELEMENTS(8) ! to flag elements in SATOB sections
INTEGER   :: SCALE           ! from BUFR Table B
INTEGER   :: SCALES(MAXND)   ! from MERBITX                        !a
INTEGER   :: SECOND          ! second of data time (AMDARs)
REAL      :: TOL             ! tolerance adjusted for width
REAL      :: TOLERA(-1:5)    ! tolerances depending on scale
INTEGER   :: TOR(5)          ! time of receipt from MDB
LOGICAL   :: TRAILER         ! set if data stored with trailers
CHARACTER(4) ::  TTAA          ! bulletin ident (retrieved from traile
LOGICAL   :: UPDATE_MESSAGE  ! set if 1st message to be updated    !a
REAL      :: V               ! MDB value adjusted if character pointr
LOGICAL   :: VALUE_CONSTANT  ! set if this value always the same   !a
REAL      :: VALUES(7*MAXARR)                                      !f
INTEGER   :: WIDTH           ! from BUFR Table B
INTEGER   :: WIDTHS(MAXND)   ! from MERBITX                        !a
INTEGER   :: X               ! dummy argument for TABLEB & LOCALD
INTEGER   :: XARR(1)         ! dummy argument for  LOCALD
INTEGER   :: XS(MAXND)       ! from MERBITX: X in descriptor       !a
INTEGER   :: YS(MAXND)       ! from MERBITX: Y in descriptor       !a

NAMELIST /PARM/MODE


DATA TOLERA/10.,1.,.1,.01,.001,.0001,.00001/
DATA MISSIN/-9999999./
DATA MODE/'MERGE'/
DATA IFT/02/, IFTA/11/,IFTB/12/,IFTLVA/13/,IFTLVB/14/
DATA ENTRY/' '/   ! blank index entry for request string at start
DATA FIRST_REQ/.TRUE./, CLOSED/.TRUE./                        !2.3
DATA ISECOND/0/   ! seconds (AMDARs) may not be found
!
DATA FIGURE/'1','2','3','4','5','6','7','8'/ ! SATOB sections 1-8
DATA SATOB_ELEMENTS/0,12,4,8,10,0,1,0/ ! 8=T,4=wind,2=cloud,1=RH

DATA FLAGS/.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,4*.FALSE./    !2.2
DATA ITEMS/1,4*0,-1,-1,-1,4*0/                                !2.2
DATA NSTORED, NUMBAD, NUMDUP, NUMFUL, NUMOLD, NUMPRE /6*0/    !2.4
DATA IBOYLEV, ICCCC, IDSEL, ILEVEL, ITTAA /5*0/               !2.5
DATA ITUP,IREPTXT,IVERT,IPRSEN,ISECNO /5*0/
DATA IVER/13/
!
! Numbers of elements in model inputs (zero if sweep)
!
DATA NOBSA/0/,NOBSB/0/,NOBSLA/0/,NOBSLB/0/,NMDB/0/
DATA NANAL/0/,NBKGD/0/,NLEVLA/0/,NLEVLB/0/
DATA IRCA/0/,IRCB/0/,IRCLA/0/,IRCLB/0/
DATA NOBSMDB/0/,IOBMDB/0/

EQUIVALENCE (CSTR(1),CSTR1)
DATA UPDATE_MESSAGE/.FALSE./                                    !a
IYEAR=0
IMONTH=0
IDAY=0
IHOUR=0
IMIN=0
ISECOND=0
CNAME(1:20)=' '
IDENT2=MISSIN                                                   !l
IDENT=MISSIN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Read header of merge table (data type, sequence descriptor for merged
! data, max NOBS for retrievals, blocksize of output data set),
! skipping blank lines before & after.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
OPEN(20,FILE='DD:FT20F001',ACTION='READ',IOSTAT=IOS)
IF (IOS == 0)READ(20,PARM)
OPEN(1,FILE='DD:FT01F001',ACTION='READ')
READ (1,'(80X)',IOSTAT=IERROR)
READ (1,'(15X, A1,I2,I3, 10X,A8,2I9)',IOSTAT=IERROR)      &
    LF,LX(1),LY(1), DATA, MAXNOBS, BLKSIZ
READ (1,'(80X)',IOSTAT=IERROR)
READ (1,'(80X)',IOSTAT=IERROR)
READ (1,'(80X)',IOSTAT=IERROR)
READ (1,'(80X)',IOSTAT=IERROR)
MERGER=3*16384+LX(1)*256+LY(1)
!
! Can only cut corners in encode if one ob per message & no profile   !a
! (so what about BUOYs with profiles when merged with ocean data?!)   !a
!
FAST_ENCODE=DATA.EQ.'LNDSYN'.OR.DATA.EQ.'SHPSYN'.OR.         &
            DATA.EQ.'MOBSYN'.OR.DATA.EQ.'METARS'.OR.         &
            DATA.EQ.'AIREPS'.OR.DATA.EQ.'AMDARS'             !1.21
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Read lines of merge table: each gives desriptor, name etc for an
! element, plus various pointers; blank lines are ignored.
! Each element line (except TOR etc) corresponds to a column in the
! VALUES array, i.e. an element in the output BUFR message.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
I=1
LMLIST=' ELEMENTS '
IREQ=10              ! pointer to LMLIST
NREPL=0
IRC=0
DO WHILE (IRC.EQ.0)
  READ (1,1,IOSTAT=IRC) LF,LX(I),LY(I),              &
        LCHAR(I),LINDEX(I),LNAME,CONST,LMDB(I),      &
        LANAL(I),LBKGD(I),LEVLA(I),LEVLB(I)
    1   FORMAT (A1,I2,I3, 1X,A1,1X,A1,1X, A32,A8,5I5)

  IF (CONST(8:8).GE.'0') THEN
    READ (CONST,'(I8)',IOSTAT=IERROR) LCONST(I)
  ELSE
    LCONST(I)=MISSIN
  ENDIF

! If the MDB subscript is set, put the element name in a request string
! for retrievals.  (For convenience the first element in a replication
! starts with a bracket and the last ends with )*nn, where nn=max...

  IF (LMDB(I).GT.0) THEN
    LENAME=INDEX(LNAME,' ')
    IF (LENAME.NE.1) THEN
      IF (LENAME.EQ.0) LENAME=LEN(LNAME)+1
      IF(IREQ+LENAME > LEN(LMLIST))CALL SYSABN(1001)
      LMLIST(IREQ+1:IREQ+LENAME)=LNAME
      IREQ=IREQ+LENAME
      NMDB=NMDB+1
    ENDIF
  ENDIF

! The descriptor may be a replication.  If so, its X-value says how
! many lines to replicate; the number of times is either a constant
! on the same line as the replication descriptor or retrievable (on
! the line before), in which case the current line gives a maximum.
!   If the number of values to replicate is too big for the XX field,
! then XX is zero and the count is at the start of NAME.
! Adjust NMBD, which otherwise would only include the elements once,
! adding number of MDB elements in replication times one less than
! (maximum) number of times; NMDB is for the MDB call.

  IF (LF.EQ.'1') THEN
    NREPL=NREPL+1
    IF (NREPL > 99) CALL SYSABN(1002)
    REPL(NREPL)=I ! keep line number of replication descriptor
    NMDB=NMDB+(LCONST(I)-1)*LMDB(I)
    IF (LX(I).EQ.0) READ (LNAME(1:4),'(I4)') LX(I)
    I=I+1

! Keep the date/time elements marked with an 'X' to pass the data
! date/time to the storage program.  Same for identifier & lat/long.

  ELSE IF (LF.EQ.'0') THEN
    IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.4) THEN
      IF (LY(I).EQ.1) IYEAR=LMDB(I)
      IF (LY(I).EQ.2) IMONTH=LMDB(I)
      IF (LY(I).EQ.3) IDAY=LMDB(I)
      IF (LY(I).EQ.4) IHOUR=LMDB(I)
      IF (LY(I).EQ.5) IMIN=LMDB(I)
      IF (LY(I).EQ.6) ISECOND=LMDB(I)
    ENDIF

! If this element is a delayed replication count (031001), there   !1.22
! may be a negative number in one of the background columns.       !1.22
! This points to a descriptor containing the count - keep it.      !1.22

    IF (LX(I).EQ.31 .AND. LY(I).EQ.1) THEN                   !1.22
      NLRYYY=LBKGD(I)                                        !1.22
      NLMYYY=LEVLB(I)                                        !1.22
    ENDIF                                                    !1.22

! For AMDARs the preferred identifier field is marked X, but a second
! field marked Y will be used in the index if the first is missing.   !l

    IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.1) IDENT=LMDB(I)
    IF (LINDEX(I).EQ.'Y' .AND. LX(I).EQ.1) IDENT2=LMDB(I)       !l
    IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.5) ILAT=LMDB(I)
    IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.6) ILONG=LMDB(I)
    IF (LINDEX(I).EQ.'X' .AND. LX(I).EQ.7) ILEVEL=LMDB(I)    !1.16

! See if any model-level subscripts are set: if not, no model messages.

    IF (LANAL(I).GT.NANAL) NANAL=LANAL(I)
    IF (LBKGD(I).GT.NBKGD) NBKGD=LBKGD(I)
    IF (LEVLA(I).GT.NLEVLA) NLEVLA=LEVLA(I)
    IF (LEVLB(I).GT.NLEVLB) NLEVLB=LEVLB(I)

! Check for high accuracy lat/long (information needed by BUFREP).  !2.2

    IF (LNAME.EQ.'LTTD'.AND.LX(I).EQ.5)& !latitude descriptor !2.2
        FLAGS(5)=LY(I).EQ.1             ! 005001 = high res.  !2.2

! For SATOBs note subscript of section number (to go in data & index)

    IF (LNAME(1:9).EQ.'SCTN_NMBR') ISECNO=LMDB(I)

    IF (LNAME(1:26).EQ.'TMPR_SALNY_LEVL_RPLTN_CONT') THEN   !1.23b
      IBOYLEV=LMDB(I)                                       !1.23b
    ENDIF                                                   !1.23b

    I=I+1

! Put time-of-receipt elements in a separate array. Keep fields from
! bulletin header for data stored with trailers.
! (COLTN_CNTR covers both number & characters: LCHAR(I)='C' if chars,
!  otherwise ICCCC<0 to put number in message, not string in trailer.)
! RPRT_IDNY (index flag rather than element, so no descriptor) is
! needed to distinguish between land, mobile & ship U/A reports.
! SCTN_NMBR is needed for SATOB index entry.

  ELSE IF (LF.EQ.' ') THEN
    IF (LNAME(1:9).EQ.'RCPT_YEAR') ITOR(1)=LMDB(I)
    IF (LNAME(1:9).EQ.'RCPT_MNTH') ITOR(2)=LMDB(I)
    IF (LNAME(1:8).EQ.'RCPT_DAY')  ITOR(3)=LMDB(I)
    IF (LNAME(1:9).EQ.'RCPT_HOUR') ITOR(4)=LMDB(I)
    IF (LNAME(1:9).EQ.'RCPT_MINT') ITOR(5)=LMDB(I)

    IF (LNAME(1:10).EQ.'COLTN_CNTR') THEN
      ICCCC=LMDB(I)
      IF (LCHAR(I).NE.'C') ICCCC=-ICCCC
    ENDIF

    IF (LNAME(1:9).EQ.'BLTN_IDNY') ITTAA=LMDB(I)
    IF (LNAME(1:9).EQ.'RPRT_IDNY') ITUP=LMDB(I)
    IF (LNAME(1:9).EQ.'RPRT_TEXT') IREPTXT=LMDB(I)           !1.14
    IF (LNAME(1:9).EQ.'VRCL_SGFC') IVERT=LMDB(I)
    IF (LNAME(1:10).EQ.'DATA_SLCTN') IDSEL=LMDB(I)            !2.5
    IF (LNAME(1:14).EQ.'PESR_SNSR_FLAG') IPRSEN=LMDB(I)

  ENDIF
ENDDO
if(I > MAXND )CALL SYSABN(1003)

! Zero counts of elements from model inputs in sweep mode.

IF (MODE.EQ.'SWEEP') THEN
  NANAL=0
  NBKGD=0
  NLEVLA=0
  NLEVLB=0
ENDIF
NELM=I    ! Total number of elements
!
! Set flag if data stored with trailers (to decide storage program)
!
TRAILER=DATA.EQ.'LNDSYN'.OR.DATA.EQ.'SHPSYN'.OR.            &
        DATA.EQ.'MOBSYN'.OR.                                &
        DATA.EQ.'TEMP'.OR.DATA.EQ.'PILOT'.OR.DATA.EQ.'DROPSOND'

!-----------------------------------------------------------------------
! Read MDB request (it is in every model input, so read them all to
! skip that record). If no model input at all, stop, leaving data to
! be swept; if one or more model inputs missing but not all, carry on.
!-----------------------------------------------------------------------

IF (MODE.EQ.'MERGE') THEN

!-----------------------------------------------------------------------
! Read MDB requests from model level analysis file
!-----------------------------------------------------------------------
  IF (NLEVLA.GT.0) THEN                                      !1.8b
    CALL_STRING = .TRUE.                                     !1.8b
    DO WHILE (CALL_STRING)                                   !1.8b
      CALL MODLVA(IFTLVA,BUFMES_LA,L,IRCLA)                  !1.8b
      IF (IRCLA.LE.0) THEN                                   !1.8b
        BSQ=0                                                !1.8b
        IF (ICHAR(BUFMES_LA(8:8)).GT.1) BSQ=4                !1.8b
        IDESC=ICHAR2(BUFMES_LA(30+BSQ:31+BSQ))                !2.2
        CALL DESFXY(IDESC,IFF,IXX,IYY)                       !1.8b
        CALL_STRING =  (IFF.EQ.2 .AND. IXX.EQ.5)             !1.8b

        IF (FIRST_REQ) THEN                                  !1.8b
          K=3*MAXARR                                         !1.17
          NOBSREQ=7*MAXARR                                   !1.17
          CALL DEBUFR(MDESCR,DANAL,FOR,K,NOBSREQ,            &
                      BUFMES_LA,.TRUE.)                     !1.8b
!         print*,'MERGE:after DEBUFR,K,NOBSREQ',K,NOBSREQ
          FIRST_REQ = .FALSE.                                !1.8b
        ENDIF                                                !1.8b
      ELSE                                                   !1.8b
        CALL_STRING = .FALSE.  !- no model file.             !1.8b
      ENDIF                                                  !1.8b

    ENDDO                                                    !1.8b
  ENDIF                                                      !1.8b

!-----------------------------------------------------------------------
! Read MDB requests from model level background file
!-----------------------------------------------------------------------
  IF (NLEVLB.GT.0) THEN                                      !1.8b
    CALL_STRING = .TRUE.                                     !1.8b
    DO WHILE (CALL_STRING)                                   !1.8b
      CALL MODLVB(IFTLVB,BUFMES_LB,L,IRCLB)                  !1.8b
      IF (IRCLB.LE.0) THEN                                   !1.8b
        BSQ=0                                                !1.8b
        IF (ICHAR(BUFMES_LB(8:8)).GT.1) BSQ=4                !1.8b
        IDESC=ICHAR2(BUFMES_LB(30+BSQ:31+BSQ))                !2.2
        CALL DESFXY(IDESC,IFF,IXX,IYY)                       !1.8b
        CALL_STRING =  (IFF.EQ.2 .AND. IXX.EQ.5)

        IF (FIRST_REQ) THEN                                  !1.8b
          K=3*MAXARR                                         !1.17
          NOBSREQ=7*MAXARR                                   !1.17
          CALL DEBUFR(MDESCR,DANAL,FOR,K,NOBSREQ,            &
                      BUFMES_LB,.TRUE.)                     !1.8b
          FIRST_REQ = .FALSE.                                !1.8b
        ENDIF                                                !1.8b
      ELSE                                                   !1.8b
        CALL_STRING = .FALSE.  !- no model file.             !1.8b
      ENDIF                                                  !1.8b

    ENDDO                                                    !1.8b
  ENDIF                                                      !1.8b

!-----------------------------------------------------------------------
! Read MDB requests from analysis file
!-----------------------------------------------------------------------
  IF (NANAL.GT.0) THEN                                       !1.8b
    CALL_STRING = .TRUE.                                     !1.8b
    DO WHILE (CALL_STRING)                                   !1.8b
      CALL MODELA(IFTA,BUFMES_A,L,IRCA)                      !1.8b
      IF (IRCA.LE.0) THEN                                    !1.8b
        BSQ=0                                                !1.8b
        IF (ICHAR(BUFMES_A(8:8)).GT.1) BSQ=4                 !1.8b
        IDESC=ICHAR2(BUFMES_A(30+BSQ:31+BSQ))                 !2.2
        CALL DESFXY(IDESC,IFF,IXX,IYY)                       !1.8b
        CALL_STRING =  (IFF.EQ.2 .AND. IXX.EQ.5)             !1.8b

        IF (FIRST_REQ) THEN                                  !1.8b
          K=3*MAXARR                                         !1.17
          NOBSREQ=7*MAXARR                                   !1.17
          CALL DEBUFR(MDESCR,DANAL,FOR,K,NOBSREQ,            &
                      BUFMES_A,.TRUE.)                      !1.8b
          FIRST_REQ = .FALSE.                                !1.8b
        ENDIF                                                !1.8b
      ELSE                                                   !1.8b
        CALL_STRING = .FALSE.  !- no model file.             !1.8b
      ENDIF                                                  !1.8b
    ENDDO                                                    !1.8b
  ENDIF                                                      !1.8b

!-----------------------------------------------------------------------
! Read MDB requests from background file
!-----------------------------------------------------------------------
  IF (NBKGD.GT.0) THEN                                       !1.8b
    CALL_STRING = .TRUE.                                     !1.8b
    DO WHILE (CALL_STRING)                                   !1.8b
      CALL MODELB(IFTB,BUFMES_B,L,IRCB)                      !1.8b
      IF (IRCB.LE.0) THEN                                    !1.8b
        BSQ=0                                                !1.8b
        IF (ICHAR(BUFMES_B(8:8)).GT.1) BSQ=4                 !1.8b
        IDESC=ICHAR2(BUFMES_B(30+BSQ:31+BSQ))                 !2.2
        CALL DESFXY(IDESC,IFF,IXX,IYY)                       !1.8b
        CALL_STRING =  (IFF.EQ.2 .AND. IXX.EQ.5)             !1.8b

        IF (FIRST_REQ) THEN                                  !1.8b
          K=3*MAXARR                                         !1.17
          NOBSREQ=7*MAXARR                                   !1.17
          CALL DEBUFR(MDESCR,DANAL,FOR,K,NOBSREQ,            &
                      BUFMES_B,.TRUE.)                      !1.8b
          FIRST_REQ = .FALSE.                                !1.8b
        ENDIF                                                !1.8b
      ELSE                                                   !1.8b
        CALL_STRING = .FALSE.  !- no model file.             !1.8b
      ENDIF                                                  !1.8b
    ENDDO                                                    !1.8b
  ENDIF                                                      !1.8b

  IF (IRCA.GT.0.AND.IRCB.GT.0.AND.IRCLA.GT.0.AND.IRCLB.GT.0) THEN
    PRINT *,DATA,' - NO MODEL DATA: MERGE STOPPED'
    GO TO 999
  ELSE                                                         !n
    PRINT *,' '
    IF (NLEVLA.GT.0 .AND. IRCLA.GT.0) PRINT *,           &
       'No model-level analysis file'                        !1.8b
    IF (NLEVLB.GT.0 .AND. IRCLB.GT.0) PRINT *,           &
       'No model-level background file'                      !1.8b
    IF (NANAL.GT.0 .AND. IRCA.GT.0) PRINT *,             &
       'No analysis file'                                    !1.8b
    IF (NBKGD.GT.0 .AND. IRCB.GT.0) PRINT *,             &
       'No background file'                                  !1.8b
    PRINT *,' '
  ENDIF

ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! This completes the preliminaries.  Now loop round mdb & model data,
! making messages with the same batches of soundings as returned by the
! MDB.  MDB & model data are in the same order, but the MDB returns
! batches as in the original messages (rather than filling the buffer)
! to avoid repeating the sort on hour, satellite, land/sea etc in the
! case of TOVS data.
!
! This loop (or rather the loop round the lines of the merge table once
! NBATCH has been decided) is governed by the following variables:
!
!    NL            merge table line number
!    LASTVAL       location in VALUES of an element of the last ob  !2.2
!                  from the last batch transferred                  !2.2
!    IOBx & NOBSx: whenever IOBx (pointer) reaches NOBSx (total),
!                  get more data from that input.
!    NBATCH        number of obs to transfer to output array this   !2.2
!                  time round the DO WHILE loop                     !2.2
!    NTIMES,NLINES to handle replicated lines in merge table
!     (& corresponding NLx, increments in the various input arrays)
!
! First read model data & call MDB if necessary.  Then set NBATCH and
! starting points, and pick values to be encoded for these soundings.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Loop over input data while there are still messages to read (MDBRC=4)
! or still obs to extract from the current message (IOBMDB<NOBSMDB).
!
NDONE=0              ! count of observations merged
MDBRC=4              ! to get DO WHILE loop started           !2.2
NEWFMT=.FALSE.       ! Default until storage d.s. is opened   !2.7
DO WHILE (MDBRC.EQ.4 .OR. IOBMDB.LT.NOBSMDB)                  !2.2
  REPL_EXTRA=0       ! zero replication bits for new message    !a
!
! If more model data of some kind is needed, decode an input
! BUFR message (background or analysis, maybe model-level...)
!
! If data from this input expected, then
!   if still at start or more data needed, then
!     get another message.
! If there are no more messages from this input, then
!       zero the count of expected elements to show this,
! else decode the message & zero count of obs used from this message
!
  IF (NANAL.GT.0 .AND. IRCA.EQ.0) THEN
    IF (NDONE.EQ.0 .OR. IOBA.GE.NOBSA) THEN
      IF (NDONE.NE.0) CALL MODELA(IFTA,BUFMES_A,L,IRCA)      !1.8b
      IF (IRCA.EQ.0) THEN
        NDA=MAXARR*3
        NOBSA=MAXARR*7
        CALL DEBUFR(MDESCR,DANAL,BLOCK,NDA,NOBSA,          &
                    BUFMES_A,.FALSE.)                        !1.8b
        IOBA=0
      ENDIF
    ENDIF
  ENDIF
!
  IF (NBKGD.GT.0 .AND. IRCB.EQ.0) THEN
    IF (NDONE.EQ.0 .OR. IOBB.GE.NOBSB) THEN
      IF (NDONE.NE.0) CALL MODELB(IFTB,BUFMES_B,L,IRCB)      !1.8b
      IF (IRCB.EQ.0) THEN
        NDB=MAXARR*3
        NOBSB=MAXARR*7
        CALL DEBUFR(MDESCR,DBKGD,BLOCK,NDB,NOBSB,          &
                    BUFMES_B,.FALSE.)                       !1.8b
        IOBB=0
!                                                                  !1.22
! If there is a pointer to a replication count in a descriptor,    !1.22
! set it (descriptor is 100YYY, so subtract 16384 to remove F=1)   !1.22
!                                                                  !1.22
        IF (NLRYYY.LT.0) NLREPT=MDESCR(-NLRYYY)-16384        !1.22
      ENDIF
    ENDIF
  ENDIF
!
  IF (NLEVLA.GT.0 .AND. IRCLA.EQ.0) THEN
    IF (NDONE.EQ.0 .OR. IOBLA.GE.NOBSLA) THEN
      IF (NDONE.NE.0) CALL MODLVA(IFTLVA,BUFMES_LA,L,IRCLA)  !1.8b
      IF (IRCLA.EQ.0) THEN
        NDLA=MAXARR*3
        NOBSLA=MAXARR*7
        CALL DEBUFR(MDESCR,DLEVLA,BLOCK,NDLA,NOBSLA,      &
                    BUFMES_LA,.FALSE.)                      !1.8b
        IOBLA=0
      ENDIF
    ENDIF
  ENDIF
!
  IF (NLEVLB.GT.0 .AND. IRCLB.EQ.0) THEN
    IF (NDONE.EQ.0 .OR. IOBLB.GE.NOBSLB) THEN
      IF (NDONE.NE.0) CALL MODLVB(IFTLVB,BUFMES_LB,L,IRCLB)  !1.8b
      IF (IRCLB.EQ.0) THEN
        NDLB=MAXARR*3
        NOBSLB=MAXARR*7
        CALL DEBUFR(MDESCR,DLEVLB,BLOCK,NDLB,NOBSLB,      &
                    BUFMES_LB,.FALSE.)                      !1.8b
        IOBLB=0
!                                                                  !1.22
! If there is a pointer to a replication count in a descriptor,    !1.22
! set it (descriptor is 100YYY, so subtract 16384 to remove F=1)   !1.22
!                                                                  !1.22
        IF (NLMYYY.LT.0) NLMODL=MDESCR(-NLMYYY)-16384        !1.22
      ENDIF
    ENDIF
  ENDIF
!
! If more MDB data is needed, call MDB for up to MAXNOBS soundings.
! (MAXNOBS is from merge table, so e.g. =1 for U/A)
!
  IF (NDONE.EQ.0 .OR. IOBMDB.GE.NOBSMDB) THEN

    CALL MERMDB(DATA,MODE,FOR,LMLIST,NMDB,MAXNOBS,NOBSMDB,  &
                DBDATA,MAXDIM,CSTR,CDIM,REPTXT,IVERT,MDBRC)

    IOBMDB=0

    IF (MDBRC.EQ.0 .AND. NOBSMDB.EQ.0) THEN                 !1.26
      IF (NDONE.EQ.0) THEN                ! if still at start
        PRINT *,DATA(1:6),' MERGE ABANDONED - NO MDB DATA'
        GO TO 999                         ! stop
      ELSE                                                  !1.26
        GO TO 998                         ! print count & stop  !f
      ENDIF                                                 !1.26
    ENDIF
  ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Decide how many obs to encode in this message.  Do not cross input
! file boundaries.  So go to the end of the input array with fewest
! obs left to handle; this array will be refilled next time round.
! (i.e. NBATCH is just the minimum of the numbers of obs left in
! the various inputs, but with checks for inputs not in use.)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
  NBATCH=NOBSMDB-IOBMDB
  IF (NOBSA.GT.0 .AND.  NOBSA-IOBA .LT.NBATCH) NBATCH= NOBSA-IOBA
  IF (NOBSB.GT.0 .AND.  NOBSB-IOBB .LT.NBATCH) NBATCH= NOBSB-IOBB
  IF(NOBSLA.GT.0 .AND. NOBSLA-IOBLA.LT.NBATCH) NBATCH=NOBSLA-IOBLA
  IF(NOBSLB.GT.0 .AND. NOBSLB-IOBLB.LT.NBATCH) NBATCH=NOBSLB-IOBLB

  IF (NBATCH.LE.0) THEN                                     !1.12b
    WRITE(6,*)                                              !1.12b
    WRITE(6,*)'MERGE: TERMINATING, NBATCH = ',NBATCH        !1.12b
    WRITE(6,*)'further info follows:'                       !1.12b
    WRITE(6,*)'NOBSMDB, IOBMDB = ',NOBSMDB,IOBMDB           !1.12b
    WRITE(6,*)'NOBSA, IOBA     = ',NOBSA,IOBA               !1.12b
    WRITE(6,*)'NOBSB, IOBB     = ',NOBSB,IOBB               !1.12b
    WRITE(6,*)'NOBSLA, IOBLA   = ',NOBSLA,IOBLA             !1.12b
    WRITE(6,*)'NOBSLB, IOBLB   = ',NOBSLB,IOBLB             !1.12b
    STOP                                                     !1.28
  ENDIF                                                     !1.12b

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! BUFR message will contain NOBSMDB obs. Loop round the table lines
! putting values in an (NOBSMDB,) array starting from where the last
! batch left off. To handle replications (not nested!) without
! repeating code, treat each line as replicated once unless we are in
! a replication.  So first set line/times counts.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
  NL=1                           ! line number
  IREPL=1                        ! first replication
  IF (NREPL.EQ.0) REPL(1)=NELM+1 ! to skip code if no replications
  LASTVAL=IOBMDB                 ! subscript in VALUES array  !2.2
  NELM_THIS_OB=NELM              ! incremented if any replication
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! The number of times to replicate is either a constant on the same
! line or (if Y is zero) a value in the retrieved array pointed
! to by the previous line (for 031001) - though there may be a
! maximum for retrieval on the replication line.
! (Beware of missing counts - they have occurred for PILOTs!
! And beware of retrieved counts greater than the max in the request!)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
  DO WHILE (NL.LE.NELM)
    IF (NL.LT.REPL(IREPL)) THEN
      NLINES=1
      NTIMES=1
    ELSE IF (NL.EQ.REPL(IREPL)) THEN
      NLINES=LX(NL)
      IF (LY(NL).NE.0) THEN       ! not delayed replication
        NTIMES=LCONST(NL)
      ELSE                        ! if 1xx000, use retrieved
        K=LASTVAL-NOBSMDB+1       ! count 031001 value from   !2.2
        NTIMES=VALUES(K)          ! first ob in message       !2.2
        IF (LCONST(NL).GT.0 .AND. NTIMES.GT.LCONST(NL)) THEN    !g
          print *,ntimes,'levels reported, only',       &
                  lconst(nl),'retrieved'                        !g
          NTIMES=LCONST(NL)       ! reset count for use below   !g
          VALUES(LASTVAL)=NTIMES  ! reset count in array too!   !g
        ENDIF                                                   !g
      ENDIF

      IF (NTIMES.LT.0) THEN        ! set missing count to zero
        NTIMES=0
        VALUES(LASTVAL)=0
      ENDIF

! For streamlined encoding keep the number of bits replicated         !a

      IF (UPDATE_MESSAGE) REPL_BITS=WIDTHS(NL)                  !a

! Keep the corresponding increments in the various input arrays

      NLA=LANAL(NL)
      NLB=LBKGD(NL)
      NLLA=LEVLA(NL)
      NLLB=LEVLB(NL)
      NLMDB=LMDB(NL)

! Increment value count (for ENBUFR) to allow for replication

      NELM_THIS_OB=NELM_THIS_OB+NLINES*(NTIMES-1)

! Go on to first line replicated & point to next replication (if any)

      NL=NL+1
      IREPL=IREPL+1
      IF (IREPL.GT.NREPL) REPL(IREPL)=NELM+1 ! (SEE ABOVE)
    ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Now fill the values array element by element, looping round the lines
! of the merge table.
! These are the possible sources of data:
! - the values of this element are constant
! - values are in a model input only (one of the four)
! - values are in the MDB array only
! - values are from both MDB & model: model value will be taken, and
!                                              coordinates checked...
!
! All the arrays - MDB, model & output - have all values of an element
! together rather than all elements in an observation.
!
!  The code below sets NBATCH consecutive values, the next NBATCH in
! the VALUES array.  In a replication the same is done for NLINES
! elements, then another NLINES... until NLINES*NTIMES*NBATCH values
! have been set.  This is handled by incrementing LASTVAL, the point
! after which to start in VALUES, before the next NBATCH values.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! The 1-dimensional subscripts used below can be understood as follows:
!  In an (NOBS,NELEMS) array from a BUFR decode, values for the N-th
! element come after values for the previous N-1 elements, NOBS values
! for each, so after (N-1)*NOBS values: they are values (N-1)*NOBS+K,
! where K=1,NOBS. We only want NBATCH of these values, starting after
! the IOBx-th.  This explains the terms Lxxxx(I)*NOBSx+IOBx+K, where
! K=1,NBATCH.
!  The remaining term in the subscript is only nonzero in replications;
! it adjusts the number of preceding elements to be multiplied by NOBS.
! NLx is the number of elements replicated in the input concerned, only
! a subset of the elements replicated by the table are from any one
! input.  NLx is multiplied by the times the replication has already
! been done.
!  The MDB array is not the direct result of a BUFR decode: its first
! dimension MAXNOBS may be greater than the number retrieved NOBSMDB,
! but MAXNOBS is one dimension of the array and hence must be used in
! working out the subscript - unused rows will simply have no data
! taken from them, because MDB will be called again once the row (ob)
! subscript reaches NOBSMDB.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
    LEVELS_OUT_OF_STEP=.FALSE.                              !1.23
    LAST_LEVEL_CHECKED=0                                    !1.25
    DO J=1,NTIMES               ! nested loops for replication
      DO I=NL,NL+NLINES-1       ! if no repl, both ranges are 1
        VALUE_CONSTANT=.FALSE.  ! may be set to .TRUE. later   !a
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!(Following IF block moved from end of replication loop in change !1.23)
!
! First check coordinate(s) if both model & MDB pointers given, & !1.23
! stop if out of step unless this is a replication over levels,   !1.23
! in which case ignore model values for rest of replication.      !1.23
!  In a profile (replication over levels) only check one          !1.25
! coordinate, not both pressure & height.                         !1.25
!   Do not expect real values                                     !1.23
! of a coordinate to be equal, allow a tolerance depending on the scale
! and also on the width if BUFR allows more precision than real numbers
! (assumed to have 24 bits for fraction).
! Assume only the background input from the model has coordinates.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
        IF (MODE.EQ.'MERGE' .AND. LX(I).LE.10 .AND.      &
           LMDB(I).GT.0 .AND. LBKGD(I).GT.0 .AND.        &
           J.GT.LAST_LEVEL_CHECKED) THEN                     !1.25
          LAST_LEVEL_CHECKED=J                               !1.25
          CALL TABLEB(LX(I),LY(I),IVER,SCALE,X,WIDTH,   &
          BLOCK,BLOCK,BLOCK)
          TOL=TOLERA(SCALE)
          IF (WIDTH.GT.24) TOL=AMAX1(TOL*2**(WIDTH-24),1E-4)
!
          DO K=1,NBATCH

!-----------------------------------------------------------------------
! Difference between background and observation value.
!-----------------------------------------------------------------------

            BVAL=DBKGD((LBKGD(I)+NLB*(J-1)-1)*NOBSB+IOBB+K)   !2.6
            RVAL=DBDATA((LMDB(I)+NLMDB*(J-1)-1)*           &
                 MAXNOBS+IOBMDB+K)                            !2.6
            DIFF=BVAL-RVAL                                    !2.6

!-----------------------------------------------------------------------
! Change !2.6
!
! If the value is a longitude (ILONG.EQ.LMDB(I)) and the background
! value differs from the observation value by more than tol and the
! background value is between -180.0 and -180.1, then a check is
! performed to see if the background value is -180.0 and the
! observation is +180.0. This can happen for SSMI data that has been
! retrieved using BUFR retrieval on the T3E due to rounding and change
! of sign by UM routines.
!
! - Add 360.0 to the background value (to convert -180.0 to +180.0)
! - Check again if the background differs observation value by more
!   than tol.
! - If it does not, set old difference equal to new difference (so
!   that the observation does not fail the next difference check).
!-----------------------------------------------------------------------

            IF (ILONG.EQ.LMDB(I) .AND. ABS(DIFF).GT.TOL .AND. &
                BVAL.LE.-180.0 .AND. BVAL.GT.-180.1) THEN     !2.6
              WRITE(6,*)'Bkgd long <= -180.0. added 360.0'    !2.6
              BVAL=BVAL+360.0                                 !2.6
              DIFFL=BVAL-RVAL                                 !2.6
              IF (ABS(DIFFL).LE.TOL) DIFF=DIFFL               !2.6
            ENDIF                                             !2.6

!-----------------------------------------------------------------------
! Check Difference between background and observation value. For
! longitude this will have already been done once - see above.
!-----------------------------------------------------------------------

            IF (ABS(DIFF).GT.TOL) THEN
              IF (.NOT.LEVELS_OUT_OF_STEP) THEN              !1.24
                WRITE(*,'(A8,A,I7.6,A,I7.6)') DATA,            &
                  ' model & MDB data out of step at', NDONE+K, &
                  '-th value of check element',LX(I)*1000+LY(I) !2
                PRINT *,'Background value is',BVAL            !2.6
                PRINT *,' Retrieved value is',RVAL            !2.6
                print *,'      difference is',DIFF
                print *,'       tolerance is',TOL
                print*,'MERGE:SCALE,WIDTH', &
                       SCALE,WIDTH
              ENDIF                                          !1.24

! Print an extra message (once & carry on) if we are in a profile. !1.24
! (Unfortunately it is not easy to say which level we stopped at!) !1.24

              IF (NLINES.EQ.1) THEN                          !1.24
                STOP 999                                     !1.24
              ELSE                                           !1.23
                IF (.NOT.LEVELS_OUT_OF_STEP) THEN            !1.24
                  WRITE (*,'(A,I4,A)') ' Not all', NTIMES,   &
            ' levels in this profile will have model data'      !2
                ENDIF                                        !1.24
                LEVELS_OUT_OF_STEP=.TRUE.                    !1.23
              ENDIF                                          !1.23
            ENDIF
          ENDDO
        ENDIF
!
! See if value is from one of the model inputs                     !1.23
! (Check that the input is expected, i.e. subscripts are set, that
! it is present in this run and that this element subscript is set !1.23
! - and that levels are not out of step because of different       !1.23
! maxima in model & merge requests.)                               !1.23
!
        IF (NANAL.GT.0 .AND. IRCA.EQ.0 .AND. LANAL(I).GT.0  &
            .AND. .NOT.LEVELS_OUT_OF_STEP) THEN              !1.23
          DO K=1,NBATCH
            VALUES(LASTVAL+K)=                              &
             DANAL((LANAL(I)+NLA*(J-1)-1)*NOBSA+IOBA+K)
          ENDDO
        ELSE IF (NBKGD.GT.0.AND.IRCB.EQ.0.AND.LBKGD(I).GT.0 &
            .AND. .NOT.LEVELS_OUT_OF_STEP) THEN              !1.23
          DO K=1,NBATCH
            VALUES(LASTVAL+K)=                              &
             DBKGD((LBKGD(I)+NLB*(J-1)-1)*NOBSB+IOBB+K)
          ENDDO
        ELSE IF (NLEVLA.GT.0.AND.IRCLA.EQ.0.AND.LEVLA(I).GT.0)THEN
          DO K=1,NBATCH
            VALUES(LASTVAL+K)=                              &
             DLEVLA((LEVLA(I)+NLLA*(J-1)-1)*NOBSLA+IOBLA+K)
          ENDDO
        ELSE IF (NLEVLB.GT.0.AND.IRCLB.EQ.0.AND.LEVLB(I).GT.0)THEN
          DO K=1,NBATCH
            VALUES(LASTVAL+K)=                        &
             DLEVLB((LEVLB(I)+NLLB*(J-1)-1)*NOBSLB+IOBLB+K)
          ENDDO
!
! If the value is not from a model input, try MDB.
! If value is from MDB, it may be characters.  Leave the values
! themselves where they are, just remove lengths from pointers.
! (Pointers from MDB are to strings for each ob in batch, so adjust
! pointer by multiple of string length in loop round batch.)
!
        ELSE IF (LMDB(I).GT.0) THEN
          DO K=1,NBATCH
            V=DBDATA((LMDB(I)+NLMDB*(J-1)-1)*MAXNOBS+IOBMDB+K)
            IF (LCHAR(I).EQ.'C' .AND. V.NE.MISSIN) THEN
              IPOINT=V
              IPOINT=MOD(IPOINT,65536)
              V=IPOINT+LEN(CSTR(1))*(IOBMDB+K-1)               !3
            ENDIF
            VALUES(LASTVAL+K)=V
          ENDDO
!
! If not model or MDB, the value is either constant or missing.
! LCONST(I) is missing if not set, so use it in either case.
!
        ELSE
          DO K=1,NBATCH
            VALUES(LASTVAL+K)=LCONST(I)
          ENDDO
          VALUE_CONSTANT=.TRUE.                                 !a
        ENDIF

! If this element is a delayed replication, & the count is from    !1.22
! a model input, set it as got from a decode descriptor above.     !1.22
! (No nested replications, so no loop to set values.)              !1.22

        IF (LX(I).EQ.31 .AND. LY(I).EQ.1) THEN               !1.22
          IF (LBKGD(I).LT.0) VALUES(LASTVAL+1)=NLREPT        !1.22
          IF (LEVLB(I).LT.0) VALUES(LASTVAL+1)=NLMODL        !1.22
        ENDIF                                                !1.22

! If the previous message is to be updated to avoid a further encode  !a
! and this value is not constant, put it in the bit string after any  !a
! necessary operations on it (only possible if only one ob in message)
! Number of bits before value is given by table made by MERBITX and   !a
! incremented to cope with any replication done or being done.        !a
! Data section starts (if no total length!) at OUTMESS(33:).          !a
! Change character values a character at a time.                      !a

        IF (UPDATE_MESSAGE .AND. NBATCH.EQ.1          &
            .AND. .NOT.VALUE_CONSTANT) THEN                     !a
          IF (LCHAR(I).NE.'C') THEN                             !a
            CALL BUFVAL(VALUES(LASTVAL+1),SCALES(I),REFVALS(I), &
                        WIDTHS(I),XS(I),IVALUE)                 !a
            NBITS = BITS(I) + REPL_EXTRA                     !2.4a
            CALL MERVAL(OUTMESS(33:),NBITS,WIDTHS(I),IVALUE) !2.4a
          ELSE                                                  !a
            NCHARS=WIDTHS(I)/8                                  !a
            CALL EB2ASC(NCHARS,CSTR(1)(IPOINT:IPOINT+NCHARS-1)) !a
            DO K=1,NCHARS                                       !a
              IVALUE=ICHAR(CSTR(1)(IPOINT+K-1:IPOINT+K-1))      !a
              IF (VALUES(LASTVAL+1).EQ.MISSIN) IVALUE=255       !a
              NBITS = BITS(I) + REPL_EXTRA + 8*(K-1)         !2.4a
              CALL MERVAL(OUTMESS(33:),NBITS,8,IVALUE)       !2.4a
            ENDDO                                               !a
            CALL ASC2EB(NCHARS,CSTR(1)(IPOINT:IPOINT+NCHARS-1)) !a
          ENDIF                                                 !a
        ENDIF

        LASTVAL=LASTVAL+NOBSMDB                               !2.2

! For SATOBs we need the latitude subscript in VALUES rather than the
! input array to make the lat/long box.                               !k

        IF (LMDB(I).EQ.ILAT) ILATVAL=LASTVAL/NOBSMDB          !2.2
      ENDDO   ! end of loop round replicated lines (or only one)

! For streamlined encoding adjust the number of bits before values    !a
! to cope with replications (add REPL_BITS each time but one round    !a
! the loop).                                                          !a

      IF (UPDATE_MESSAGE .AND. J.LT.NTIMES) THEN                !a
        REPL_EXTRA=REPL_EXTRA+REPL_BITS                         !a

      ENDIF                                                     !a

    ENDDO     ! end of loop round replications (if any)
    NL=NL+NLINES   ! on to next line or past replicated lines
  ENDDO       ! end of DO WHILE round lines

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! If the number of observations in VALUES including the current     !2.2
! batch (IOBMDB+NBATCH) has reached the number extracted from the   !2.2
! MDB file (NOBSMDB) it is time to make an output BUFR message.     !2.2
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**

  IF (IOBMDB+NBATCH.GE.NOBSMDB) THEN                          !2.2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Make a 23-byte index entry (12-byte entries will be made by        !5
! SSMIND etc., and BUFREP also makes its own index entries).
!
! All 23-byte index entries have the following fields:
!  bytes 1 & 2: hour and minute of data (from start of index block)
!  bytes 3-11 : identifier (never uses all 9 bytes)
!  byte 12    : either number of obs in chain or number in message
!                or number of good values
!  bytes 13-16: lat/long, 2 bytes each, twos-complement form
!                or lat/long box, four 1-byte values
!  bytes 18-19: time of receipt (in minutes from start of index block)
! The time fields and pointer in bytes 20-23 are set by storage.
!
! This leaves flags in byte 17 to distinguish various kinds of data
! (need to be set for U/A, SATOBs, BUOYs and TESACs)
! and some unconventional practices at the end of the identifier field:
!  - AIREPs have minutes (binary) in last byte
!  - U/A has hour of ascent (readable) in last two bytes
! (binary hour & minute if dropsonde)
!  - SYNOPs with call sign 'SHIP' have lat/long repeated after'SHIP'
!  - SATOBs have a part number (readable) in the last byte but one
!     and the number of obs in byte 12 can overflow into the last byte
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Put lat & long information in index entry using INDLALO if BUFR   !2.2
! message contains only 1 ob, or LATBOX if it has more than one.    !2.2
! (This is not required for satellite data received in BUFR as the  !2.2
! storage routine (e.g. BUFREP) will create the index entry.)       !2.2
!
    IF (NOBSMDB.EQ.1) THEN                                           !5
      CALL INDLALO(ENTRY,DBDATA((ILAT-1)*MAXNOBS+IOBMDB+1),  &
                         DBDATA((ILONG-1)*MAXNOBS+IOBMDB+1))      !1.18
    ELSE
      CALL LATBOX(VALUES,NOBSMDB,ILATVAL,BOX)                     !2.2
      ENTRY(13:13)=CHAR(IFIX(BOX(1))+90)    ! s lat
      ENTRY(14:14)=CHAR(IFIX(BOX(2))+90)    ! n lat
      ENTRY(15:15)=CHAR(IFIX(BOX(3))/2+90)  ! w long
      ENTRY(16:16)=CHAR(IFIX(BOX(4))/2+90)  ! e long
    ENDIF
!
! Identifiers can be either characters, WMO block & station number,
! or other combinations of numbers, 5 figures for buoys or 3 for
! satellite.  If identifier is 'SHIP', copy lat/long after it.
!  Retrieval just matches the first so many characters, so no
! significant differences in handling of identifiers after any
! conversion needed to get them into characters.
!  Upper air data has a mix of ship call signs and station numbers,
! so look for a third class 1 element if station number missing.
!  Use call sign as AMDAR identifier if registration number missing.  !l
!  And 5-figure number for BATHY/TESAC if no character string         !o
!   (SHPSYN should be like that too!)
!  Missing 5-figure BATHY/TESAC ident stored as ***** (as originally) !o
!
    IF (LCHAR(IDENT).EQ.'C') THEN
      ENTRY(3:11)=GETCHR(DBDATA((IDENT-1)*MAXNOBS+IOBMDB+1),  &
                         CSTR(IOBMDB+1))
      IF (ENTRY(3:11).EQ.' '.AND. IDENT2.GT.0) THEN            !l
        IF (LCHAR(IDENT2).EQ.'C') THEN                          !o
         ENTRY(3:11)=GETCHR(DBDATA((IDENT2-1)*MAXNOBS+IOBMDB+1), &
                            CSTR(IOBMDB+1))                     !l
        ELSE                                                    !o
          IBUOY=DBDATA((IDENT2-1)*MAXNOBS+IOBMDB+1)             !o
!               IF (IBUOY.GT.0) WRITE (ENTRY(3:7),'(I5.5)') IBUOY     !o
          WRITE (ENTRY(3:7),'(I5.5)') IBUOY                     !o
        ENDIF                                                   !o
      ENDIF                                                     !l
      IF (ENTRY(3:11).EQ.'SHIP') ENTRY(7:10)=ENTRY(13:16)

!                          block/station number, 5 figs (001001,001002)
    ELSE IF (LY(IDENT).EQ.1 .AND. LY(IDENT+1).EQ.2) THEN
      IBLOCK=DBDATA((IDENT-1)*MAXNOBS+IOBMDB+1)
      ISTN=DBDATA((IDENT)*MAXNOBS+IOBMDB+1)

      IF (IBLOCK.GT.0 .AND. ISTN.GT.0) THEN
        WRITE (ENTRY(3:4),'(I2.2)') IBLOCK
        WRITE (ENTRY(5:7),'(I3.3)') ISTN
        ENTRY(8:11)=' '
      ELSE
        IF (LX(IDENT+2).EQ.1 .AND. LCHAR(IDENT+2).EQ.'C') THEN
          ENTRY(3:11)=GETCHR(DBDATA((IDENT+1)*MAXNOBS+IOBMDB+1),&
                             CSTR(IOBMDB+1))
        ENDIF
      ENDIF

    ELSE IF (LY(IDENT).EQ.5) THEN      ! buoy, 5 figures (001005)
      IBUOY=DBDATA((IDENT-1)*MAXNOBS+IOBMDB+1)
      WRITE (ENTRY(3:7),'(I5.5)') IBUOY

    ELSE IF (LY(IDENT).EQ.7) THEN    ! satellite, 3 figs (001007)
      ISATID=DBDATA((IDENT-1)*MAXNOBS+IOBMDB+1)
      WRITE (ENTRY(3:5),'(I3.3)') ISATID
    ENDIF
!
! Set date/time in array for storage calls,
! Put minute for AIREPs and hour for U/A at end of identifier field.
! And seconds for BUFR AMDARs, flight level for character AMDARs.  !1.16
!
    DATIME(1)=DBDATA((IYEAR-1)*MAXNOBS+IOBMDB+1)
    DATIME(2)=DBDATA((IMONTH-1)*MAXNOBS+IOBMDB+1)
    DATIME(3)=DBDATA((IDAY-1)*MAXNOBS+IOBMDB+1)
    DATIME(4)=DBDATA((IHOUR-1)*MAXNOBS+IOBMDB+1)
    DATIME(5)=DBDATA((IMIN-1)*MAXNOBS+IOBMDB+1)
    IF (DATIME(5).LT.0) DATIME(5)=0

    IF (DATA.EQ.'AIREPS') THEN
      ENTRY(11:11)=CHAR(DATIME(5))   ! binary minute
    ELSE IF (DATA.EQ.'AMDARS') THEN  ! second rather than minute
      IF (ISECOND.GT.0) THEN
        SECOND=DBDATA((ISECOND-1)*MAXNOBS+IOBMDB+1)
        IF (SECOND.GE.0) ENTRY(11:11)=CHAR(SECOND)
      ENDIF
      IF (ISECOND.LE.0 .OR. SECOND.LT.0) THEN                !1.16
        IF (ILEVEL.GT.0) THEN                                !1.16
          LEVEL=                                             &
           (DBDATA((ILEVEL-1)*MAXNOBS+IOBMDB+1)+0.005)/30.48 !1.16
          IF (LEVEL.LT.0) LEVEL=0                            !1.16
          IF (LEVEL.GT.255) LEVEL=255                        !1.16
          ENTRY(11:11)=CHAR(LEVEL)                           !1.16
        ENDIF                                                !1.16
      ENDIF                                                  !1.16
    ELSE IF (DATA.EQ.'TEMP' .OR. DATA.EQ.'PILOT') THEN
      WRITE (ENTRY(10:11),'(I2.2)') DATIME(4)
    ELSE IF (DATA.EQ.'DROPSOND') THEN
      ENTRY(10:10)=CHAR(DATIME(4))   ! binary hour for dropsonde
      ENTRY(11:11)=CHAR(DATIME(5))   ! & binary minute too
    ENDIF

    ENTRY(12:12)=CHAR(MOD(NOBSMDB,256))                       !2.2

! For SATOBs set flags for elements in byte 17 of index entry (from
! retrieved section number, stored as figure on end of identifier)
! (Section number may be missing (mid July 97). If so, set it to 4:   !m
! only sections 4 & 7 have that element missing, and 7 (RH) is not    !m
! retrieved at present for the model - but beware of future changes!)

    IF (DATA.EQ.'SATOB' .OR. DATA.EQ.'GOESAMW') THEN            !i
      ENTRY(11:11)=CHAR(NOBSMDB/256)                          !2.2
      NSEC=DBDATA((ISECNO-1)*MAXNOBS+IOBMDB+1)
      IF (NSEC.LE.0) NSEC=4                                     !m
      ENTRY(10:10)=FIGURE(NSEC)
      ENTRY(17:17)=CHAR(SATOB_ELEMENTS(NSEC))

! Set flags to distinguish between TEMP, PILOT & DROPSOND
! (the bits with values 2 & 1 are set for mobile and sea)
! distinguish TESAC from BATHY & between buoys with & without profiles
! - otherwise set byte 17 of index entry to zero.                    !e

    ELSE IF (DATA.EQ.'TEMP' .OR. DATA.EQ.'PILOT') THEN         !f
      MOBMAR=DBDATA((ITUP-1)*MAXNOBS+IOBMDB+1)  ! RPRT_IDNY
      IF (DATA.EQ.'TEMP ') IUATYPE=4
      IF (DATA.EQ.'PILOT') IUATYPE=0
      IF (MOBMAR.EQ.2) IUATYPE=IUATYPE+2        ! mobile
      IF (MOBMAR.EQ.3) IUATYPE=IUATYPE+3        ! mobile & marine
      ENTRY(17:17)=CHAR(IUATYPE)

    ELSE IF (DATA.EQ.'DROPSOND') THEN                          !e
      ENTRY(17:17)=CHAR(10)

    ELSE IF (DATA.EQ.'TESAC   ') THEN                          !e
      ENTRY(17:17)=CHAR(1)

    ELSE IF (DATA.EQ.'BUOY    ' .OR. DATA.EQ.'BUOYPROF')THEN!1.23b
      ENTRY(17:17)=CHAR(2)                      ! no profile!1.18b
      IF (IBOYLEV.GT.0) THEN                                !1.18b
        NBOYLEV=DBDATA((IBOYLEV-1)*MAXNOBS+IOBMDB+1)        !1.18b
        IF (NBOYLEV.GT.0) ENTRY(17:17)=CHAR(3)  ! profile   !1.18b
      ENDIF                                                 !1.18b

    ELSE                                                        !e
      ENTRY(17:17)=CHAR(0)                                      !e
    ENDIF

! Get TTAA & CCCC before character string passed to ENCODE
! (ICCCC<0 means there is only a code figure, not characters;
! if it is missing, a value of 2**16-1 sets all ones in the bytes.)   !h

    IF (ITTAA.GT.0) THEN
      TTAA =                               &
        GETCHR(DBDATA((ITTAA-1)*MAXNOBS+IOBMDB+1),CSTR(IOBMDB+1))
    ENDIF

    IF (ICCCC.GT.0) THEN
      CCCC =                               &
        GETCHR(DBDATA((ICCCC-1)*MAXNOBS+IOBMDB+1),CSTR(IOBMDB+1))
    ELSE IF (ICCCC.LT.0) THEN
      NCCCC=DBDATA((-ICCCC-1)*MAXNOBS+IOBMDB+1)
      IF (NCCCC.EQ.MISSIN) NCCCC=65535                          !h
    ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! If this is the first time round the loop to make & store messages,
! open the output data base for the storage program called later,
! putting any BUFR sequence in the second record into local Table D.
! (The first record is only read to check the data set format and to
! get the sequence record.)
! Read the model MDB request and for some types insert 'message'.
!  (Cannot call LOCALD until retrieval has been started, because it
! calls LOCALD with 'NEW', not 'ADD', & loses any sequence kept!)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
    IF (CLOSED) THEN                                          !2.3
      CNAME='FT  F001'
      WRITE(CNAME(3:4),'(I2.2)') IFT
      IOPEN=3   ! read/write access

      CALL METDB_COPEN(IFT,'DD:'//CNAME//CHAR(0),IOPEN,IERROR)

      CLOSED = .FALSE.                                        !2.3
      NUMREC=1

      CALL METDB_CREAD_DIR(IFT,BLOCK(1:BLKSIZ),BLKSIZ,NUMREC, &
                           IERROR)

      NBLOKS=ICHAR2(BLOCK(1:2)) ! Block count in old format   !2.2

      IF (NBLOKS.EQ.0) THEN  ! New data set format            !2.2
        NEWFMT=.TRUE.                                         !2.2
        NSEQBL=2                                              !2.2

      ELSE                   ! Old data set format            !2.2
        NEWFMT=.FALSE.                                        !2.2
!-----------------------------------------------------------------------
! If the number of blocks in the merge dataset is less than 12000, the
! sequence record indicator will be in the map block immediately
! after the data block pointers, otherwise it will be in byte 9 of
! the map block.
!-----------------------------------------------------------------------

        IF (NBLOKS.LE.12000) THEN                            !1.9a
          NSEQBL=ICHAR(BLOCK(NBLOKS+8:NBLOKS+8))             !1.9a
        ELSE                                                 !1.9a
          NSEQBL=ICHAR(BLOCK(9:9))                           !1.9a
        ENDIF                                                !1.9a
      ENDIF

      IF (NSEQBL.GT.0) THEN                       ! IF BYTE SET,
                                                  ! READ SEQUENCES
        CALL METDB_CREAD_DIR(IFT,BLOCK(1:BLKSIZ),BLKSIZ,   &
                             NSEQBL,IERROR)

        CALL LOCALD(0,0,XARR,X,BLOCK,'ADD')          ! & KEEP THEM
      ENDIF                                       ! FOR ENCODING.
    ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Make a BUFR message (setting time in header to time of receipt)
! (Pass character string for first ob in batch: ENBUFR will treat that
! argument as a single string of length LEN(CSTR)*NOBSMDB, not an array)
! If a code figure for CCCC has been retrieved, put it in the message
! (section 1, byte 6 - beware of change of version & displacement!)
! ENBUFR leaves mod(year,100) in TOR(1), so reset for use later.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**

    DO I=1,5
      TOR(I)=DBDATA((ITOR(I)-1)*MAXNOBS+IOBMDB+1)
      OUTMESS(16+I:16+I)=CHAR(MOD(TOR(I),100))                  !a
    ENDDO

    IF (MOD(TOR(1),100).EQ.0) OUTMESS(17:17)=CHAR(100)        !2.2

    CMPRES=MAXNOBS.GT.1                                       !2.2

! Encode a message.  If this is the first message and there is only
! one ob per message, make subsequent messages by changing this one:
! set flag and make bit index to show where to set changed values.

    NM=1
    MDESCR(1)=MERGER
    IF (NOBSMDB.GT.1 .OR. .NOT.FAST_ENCODE               &
                     .OR. .NOT.UPDATE_MESSAGE) THEN             !a

      CALL ENBUFR(MDESCR,VALUES,NM,NELM_THIS_OB,NOBSMDB,    &
                CSTR1,TOR,OUTMESS,CMPRES,REPLEN,IVER)  !3
      IF (FAST_ENCODE .AND. NDONE.EQ.0 .AND. MAXNOBS.EQ.1) THEN !o
        UPDATE_MESSAGE=.TRUE.                                   !a
        CALL MERBITX                                     &
             (MERGER,NL,FS,XS,YS,SCALES,REFVALS,WIDTHS,BITS)    !a
      ENDIF                                                     !a
    ENDIF                                                       !a

! Put collecting centre code in bytes 5-6 of section 1                !h

    IF (ICCCC.LT.0) OUTMESS(9:10)=CHAR2(NCCCC)                !2.2

     WRITE(6,'(I6,A20,A7,I6,A10)')   &
       replen,' byte message made ', &
       entry(3:9),NOBSMDB,' obs '
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
!
! Finally store the message.  Use AIRSTO even for data stored in the
! first stage by TAFREP or a similarly complicated SYNOP program: we
! only need something simple at this stage!  (Sorted input stream
! with no chaining needed, so sequential storage best.)
!
! (The time of receipt is for the first ob in the batch (set above
! and passed to ERSREP in the message rather than the TOR array);
! so are the TTAA & CCCC, passed in the identifier slot in the index
! entry if data is stored with trailers, i.e. by AIRSTO.)
! (TTAA means TTAAii(3:6) for all except U/A, which keeps TTAAii(1:4)!)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**
    IF (NM.GT.0 .AND. REPLEN.LE.BLKSIZ-6) THEN

!-----------------------------------------------------------------------
! NEWFMT = TRUE indicates new format datasets for satellite storage.
! Call BUFREP to store the data. Check the return code from BUFREP
! for any problems.                                                 !2.4
!-----------------------------------------------------------------------

      IF (NEWFMT) THEN                                        !2.2
!                                  Put time of receipt in NOW array !2.2
        DO J=1,5                                              !2.2
          NOW(9-J)=TOR(J)                                     !2.2
        END DO ! J                                            !2.2
        NOW(3)=0                                              !2.2
        NOW(2)=0                                              !2.2
        NOW(1)=0                                              !2.2
!                                    Put data selection in ITEMS(3) !2.5
        IF (IDSEL.GT.0) THEN                                  !2.5
          ITEMS(3) = DBDATA((IDSEL-1)*MAXNOBS+IOBMDB+1)       !2.5
        ELSE                                                  !2.5
          ITEMS(3) = 0                                        !2.5
        END IF                                                !2.5
!                                Put ID processing code in ITEMS(9)  !5
!                        (and put number based on altitude in data   !6
!                         selection for AMDARS as in unmerged data)  !6

        IF (DATA.EQ.'TAMDAR') THEN                             !5
          ITEMS(9) = 10                                        !5
        ELSE IF (DATA.EQ.'AMDARS') THEN                              !6
          ITEMS(3) = MOD(NINT(0.1*DBDATA(21)), 250) + 1              !6
          IF (ITEMS(3) < 0) ITEMS(3) = -ITEMS(3)                     !6
          ITEMS(9) = 16                                              !6
        ELSE IF (DATA.EQ.'GPSIWV') THEN                        !5
          ITEMS(9) = 11                                        !5
        ELSE IF (DATA.EQ.'OPENROAD') THEN                            !5
          ITEMS(9) = 12                                              !5
        ELSE                                                   !5
          ITEMS(9) = 0                                         !5
        END IF                                                 !5

        CALL BUFREP (IFT,BLKSIZ,NOW,FLAGS,ITEMS,-1,      &
                     OUTMESS(1:REPLEN),K)                     !2.2

        IF (K.LE.10) THEN      ! Message stored               !2.4
          NSTORED = NSTORED + 1                               !2.4
        ELSEIF (K.EQ.11) THEN  ! Rejected - old data          !2.4
          NUMOLD = NUMOLD + 1                                 !2.4
        ELSEIF (K.EQ.12) THEN  ! Rejected - duplicate         !2.4
          NUMDUP = NUMDUP + 1                                 !2.4
        ELSEIF (K.EQ.31) THEN  ! Rejected - no room           !2.4
          IF (NUMFUL.EQ.0) WRITE(6,'(/A/)')             &
             'MERGE: DATA BASE IS FULL !!!'                   !2.4
          NUMFUL = NUMFUL + 1                                 !2.4
        ELSEIF (K.EQ.43) THEN  ! Rejected - future time       !2.4
          NUMPRE = NUMPRE + 1                                 !2.4
        ELSEIF (K.EQ.44) THEN  ! Rejected - bad data          !2.4
          NUMBAD = NUMBAD + 1                                 !2.4
        ENDIF                                                 !2.4

      ELSE IF (DATA.EQ.'AIREPS' .OR. DATA.EQ.'AMDARS' .OR.  &
               DATA.EQ.'BUOY'   .OR. DATA.EQ.'SHPSYN' .OR.  &
               DATA.EQ.'MOBSYN' .OR. DATA.EQ.'METARS' .OR.  &
               DATA.EQ.'SALTSSH'.OR.                        &
               DATA.EQ.'TEMP  ' .OR. DATA.EQ.'PILOT ' .OR.  &
               DATA.EQ.'WINPRO' .OR. DATA.EQ.'SATOB ' .OR.  &
               DATA.EQ.'BATHY ' .OR. DATA.EQ.'TESAC ' .OR.  &
           DATA.EQ.'DROPSOND' .OR. DATA.EQ.'BUOYPROF') THEN !1.23b
        ID=ENTRY(3:11)
        ENTRY(3:11)=TTAA//CHAR(0)//CCCC                     !1.15b
        CALL AIRSTO                                      &
               (DATIME,ENTRY,OUTMESS(1:REPLEN),IFT,BLKSIZ,ID,TOR)

! For LNDSYN put any 9-groups in front of the BUFR message.        !1.14

      ELSE IF (DATA.EQ.'LNDSYN') THEN                        !1.14
        ID=ENTRY(3:11)                                       !1.14
        ENTRY(3:11)=TTAA                                     !1.14
        LREPTXT=DBDATA(IREPTXT)                              !1.14
        CALL MERSY9(REPTXT(1:LREPTXT),OUTMESS,REPLEN,REPLEN9)!1.14
        IF (REPLEN9.GT.0) THEN                               !1.14
          CALL AIRSTO(DATIME,ENTRY,                        &
                     OUTMESS(REPLEN+1:REPLEN+REPLEN9+REPLEN),&
                      IFT,BLKSIZ,ID,TOR)                     !1.14
        ELSE                                                 !1.14
          CALL AIRSTO(DATIME,ENTRY,                        &
                      OUTMESS(1:REPLEN),                   &
                      IFT,BLKSIZ,ID,TOR)                     !1.14
        ENDIF                                                !1.14
      ENDIF                                                  !1.14

    ELSE IF (REPLEN.GT.BLKSIZ-6) THEN
      PRINT *,DATA(1:6),' MESSAGE TOO LONG',REPLEN,'IS LENGTH'
    ENDIF
  ENDIF                                                       !2.2

! Increment numbers of obs used from the various input messages

  IOBA   = IOBA   + NBATCH
  IOBB   = IOBB   + NBATCH
  IOBLA  = IOBLA  + NBATCH
  IOBLB  = IOBLB  + NBATCH
  IOBMDB = IOBMDB + NBATCH
!                                  and total number of obs merged
  NDONE  = NDONE  + NBATCH

END DO                                                        !2.2

  998 IF (MODE.EQ.'MERGE') PRINT *,NDONE,DATA(1:6),' OBS MERGED'      !f
IF (MODE.EQ.'SWEEP') PRINT *,NDONE,DATA(1:6),' OBS SWEPT'

!-----------------------------------------------------------------------
! If merge dataset in new format (NEWFMT = TRUE), output summary
! information.
!-----------------------------------------------------------------------

IF (NEWFMT .AND. &
   (NUMOLD.GT.0).OR.(NUMDUP.GT.0).OR.(NUMFUL.GT.0).OR. &
   (NUMPRE.GT.0).OR.(NUMBAD.GT.0)) THEN
  WRITE(6,'(/1X,''Summary for new format datasets'' )')       !2
  WRITE(6,'( 1X,''-------------------------------''/)')       !2
  IF (NUMOLD.GT.0) THEN                ! Old data             !2.4
     WRITE (6,'(T3,I6,1X,A)') NUMOLD,                      &
      'bulletins rejected by check for old data.'             !2.4
  ENDIF                                                       !2.4
  IF (NUMDUP.GT.0) THEN                ! Duplicate bulletins  !2.4
     WRITE (6,'(T3,I6,1X,A)') NUMDUP,                      &
      'bulletins rejected by duplicate data check.'           !2.4
  ENDIF                                                       !2.4
  IF (NUMFUL.GT.0) THEN                ! Storage ds full      !2.4
     WRITE (6,'(T3,I6,1X,A)') NUMFUL,                      &
      'bulletins rejected because storage data set was full.' !2.4
  ENDIF                                                       !2.4
  IF (NUMPRE.GT.0) THEN                ! Data time in future  !2.4
     WRITE (6,'(T3,I6,1X,A)') NUMPRE,                      &
      'bulletins rejected with data times in the future.'     !2.4
  ENDIF                                                       !2.4
  IF (NUMBAD.GT.0) THEN                ! Uncreatable indexes  !2.4
     WRITE (6,'(T3,I6,1X,2A)') NUMBAD, 'index entries ',   &
      'could not be made owing to bad or missing data.'       !2.4
  ENDIF                                                       !2.4
ENDIF !- newfmt                                               !2.4

  999 STOP
END
