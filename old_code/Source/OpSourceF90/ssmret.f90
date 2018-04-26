SUBROUTINE SSMRET(ITIME,IRTIM,AREA,ISAT,ILAND,ARRAY,NOBS,NELEM,&
&IOBS,IDESC,NDES,ILVL,ISTAT,IERR,IDSK,LMSG,&
&LFLAG,QCREQ,CSTR,CREPT,SUBTYPE,NEWMDBCALL,&
&LAT_SUBSCRIPT,LON_SUBSCRIPT,RPOLE,FOUND,&
&ELIST)

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
! LAT_SUBSCRIPT : integer   (ip) : users lat subscript                !D
! LON_SUBSCRIPT : integer   (ip) : users lon subscript                !D
! RPOLE(2)      : real      (ip) : rotated pole lat/lon coords        !D
! FOUND(*)      : logical   (ip) : MetDB keywords                     !J
! ELIST         : char*8    (ip) : Element index member name        !2.6
!
! change record :
!
! 07-11-96      : Written S.Cox. Based on SATRET.
!
! 17-02-97  !A  : Call to S120EXP changed to SUBSLOCD which can cater
!               : for more than just SAT120 data. Call to S120LOCD
!               : changed to GETLOCD which can cater for more than
!               : just SAT120 data. Added new argument NEWMDBCALL which
!               : is needed by BUFINDX. - S.Cox
!
! 18-03-97  !B  : Rename dynamic common blocks - S.Cox
!
! 09-04-97  !C  : Only call GETLOCD if no sequence in record 2 AND
!               : subtype is SAT120 - S.Cox
!
! 28-07-97  !D  : Change loop structure to let obs be weeded out of
!               : message: NSEND returned by VALARR, not set before.
!               : Code after call simplified, LMID set if NSEND<NSOUND.
!               : LAT_SUBSCRIPT, LON_SUBSCRIPT and RPOLE now passed in
!               : as arguments for the selection of observations by
!               : rotated lat/lon area. Integer variable IAREA changed
!               : to Real variable AREA - J.Lewthwaite
!
! 04-08-97  !E  : Amend for Year 2000 - use commmon routine to determine
!               : century - Jim Arnott.
!
! 15-09-97  !F  : Add check on subtype = RTOVS and call GETLOCD if no
!               : sequence in record 2 and SUBSLOCD if there isn't a
!               : sequence descriptor in BUFR section 3 - S.Cox
!
! 20-10-97  !G  : Get the BUFR subtype from BUFR section 1 and put in
!               : IRCVD(6) to pass to VALARR. This is wanted for SAT120
!               : & RTOVS retrieval - S.Cox
!
! 16-03-98  !H  : Remove NSOUND <> NREP check. NSOUND is the no. of obs
!               : in the index entry, NREP is the no. of obs in the
!               : BUFR message. Work using NREP everywhere rather than
!               : NSOUND as NSOUND has a max of 1024, and with ESAHRVW,
!               : this can be exceeded! - S.Cox
!
! 15-06-98  !I  : Increase MDES from 1000 to 2000 to handle more
!               : user requested elements. This brings SSMRET in line
!               : with BUSRET which needs the change for WINPRO
!               : retrieval - S.Cox
!
! 20-07-98  !J  : Allow retrieval of BUFR messages. The BUFR messages
!               : are returned in the raw report string - S.Cox
!
! 17-08-98  !K  : Retrieval with 'MESSAGE' option: remove check which
!                 stops if >NOBS in message.  This means 'MESSAGE'
!                 will still not return obs from different messages
!                 in the same call (which was the aim of the option,
!                 for the merge), but now will not fail if a message
!                 happens to be bigger than expected - C.Long
!
!                 Check number of obs returned by routine BUFINDX. If
!                 zero, there was a problem decoding the BUFR message,
!                 so skip the message/index entry - S.Cox
!
! The use of the change history section was stopped in August 1998.
! All changes are now recorded in the RCS $log section below.
!
!-----------------------------------------------------------------------
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.6  2003/05/02 15:25:56  usmdb
! Element index member name ELIST passed in and passed to
! READIDX - S.Cox.
!
! Revision 2.5  2003/03/06  09:16:49  09:16:49  usmdb (MetDB account c/o usjh)
! Do not reset II to 0 for BUFR retrieval as this causes problems
! when spanning off-line datasets - S.Cox
!
! Revision 2.4  2003/02/03 15:16:05  usmdb
! Set IERR=16 instead of ISTAT=16 if no match in READIDX - S.Cox
!
! Revision 2.3  2002/08/07  09:18:30  09:18:30  usmdb (MetDB account c/o usjh)
! 2.3.  19 August 2002.  Brian Barwell.  Change 60/02.
! Prevent retrieval of truncated messages with RETBUFR.
!
! Revision 2.2  2002/02/04  11:47:31  11:47:31  usmdb (Generic MetDB account)
! 2.2.  18 February 2002.  Brian Barwell.  Change 9/02.
! Facilities added to do retrievals for a limited area crossing the
! dateline and to specify retrieval of undecoded BUFR messages.
!
! Revision 2.1  2001/05/08  13:12:22  13:12:22  usmdb (Generic MetDB account)
! Increase number of index entries allowed to the number that can
! be held in 4 index blocks to be consistent with SSMREP - S.Cox
!
! Revision 2.0  2001/01/08  11:59:12  11:59:12  usmdb (Generic MetDB account)
! Removed argument XTYPE from call to BUFINDX, removed
! unused variable J, removed EXTERNAL statements,
! added copyright - S.Cox
!
! Revision 1.21  2000/08/04  15:44:45  15:44:45  usmdb (Generic MDB account)
! 7 Aug 2000 - S.Cox
! Increase number of index entries allowed to the number that
! can be held in 3 index blocks.
!
! Revision 1.20  99/09/09  10:17:59  10:17:59  usmdb (Generic MDB account)
! Change date: 20-09-1999
! Check if the total no of elements requested exceeds MDES. If it
! does, SSMRET reports an error and exits.
! Also Decrease the size of array IEXTRA to save REGION. This
! array does not need to be very large. Now declared of size
! MEXT. This size is passed down to other routines if
! required - S.Cox
!
! Revision 1.19  99/07/12  16:10:53  16:10:53  usmdb (Generic MDB account)
! Implemented 19/07/1999 - S.Cox
! Correct usage of "MESSAGE" keyword so the routine does
! not return to the user calling program if 0 obs are
! found in a BUFR message - it instead moves onto the
! next message. This shortcoming came to light in
! Mesoscale merge testing - where obs from a small area
! are required. If none were found in the BUFR message
! (which was often the case), the routine returned to
! the merge program with ISTAT=4, NOBS=0. The merge
! program stopped at this point.
!
! Revision 1.18  99/02/11  12:09:08  12:09:08  usmdb (Generic MDB account)
! 15-02-1999 S.Cox
! Add subtype to NO DATA FOR THIS PERIOD warning message
! v(G)=82, ev(G)=35
!
! Revision 1.17  99/01/14  14:16:45  14:16:45  usmdb (Generic MDB account)
! 18-01-99 S.Cox
! Increase the size of array to pass to DEBUFR from 160000 to 180000. This
! is the max size needed for EUMETSAT unified wind format.
! V(G)=82, ev(G)=35
!
! Revision 1.16  98/11/12  14:05:13  14:05:13  usmdb (Generic MDB account)
! 16-11-98 S.Cox
! a) Increase size of array to pass to DEBUFR from 80000 to 160000. This
!    is so that EUMETSAT wind messages in the unified format can be decoded.
!    They are rather large!
! b) Change declaration of CINDX - now an array of element indexes.
!
! Revision 1.15  98/10/15  11:35:25  11:35:25  usmdb (Generic MDB account)
! 19-10-98 : SSMRET index entries array increased to cope with index
! overflow blocks. Initially only 1 overflow block per index block is
! allowed for - S.Cox
! v(G)=82, ev(G)=35
!
! Revision 1.14  98/09/16  16:11:17  16:11:17  usmdb (Generic MDB account)
! 21-09-98  S.Cox  Ref. Problem 260 (a), 144 (b)
! a) Set IERR=16 when there is a failure in routine BUFINDX.
! b) Correct setting of IOBS if a retrieval spans more than one
!    dataset.
!
! Revision 1.13  98/08/12  08:50:04  08:50:04  usmdb (Generic MDB account)
! Retrieval with 'MESSAGE' option
!
! Revision 1.12  98/07/23  08:44:35  08:44:35  usmdb (Generic MDB account)
! changes to allow retrieval of BUFR messages
!
! Revision 1.11  98/06/11  13:39:25  13:39:25  usmdb (Generic MDB account)
! Increase MDES from 1000 to 2000 to handle more
! user requested elements.
!
! Revision 1.10  98/03/12  08:47:58  08:47:58  usmdb (Generic MDB account)
! Remove NSOUND <> NREP check.
!
! Revision 1.9  1998/03/05 10:47:28  usjl
! Remove spurious NDES declaration.
!
! Revision 1.8  1997/10/24 13:17:14  usjl
! Get the BUFR subtype from BUFR section 1 and put in
! IRCVD(6) to pass to VALARR.
!
! Revision 1.7  1997/09/22 11:21:42  uspm
! Change order of variables type declarations to satisfy NAG F90 compiler
!
! Revision 1.6  1997/09/10 15:19:35  uspm
! Add check on subtype = RTOVS
! Call GETLOCD if no sequence in record 2 and SUBSLOCD
! if there isn't a sequence descriptor in BUFR section 3
!
! Revision 1.5  1997/08/04 13:34:09  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.4  1997/04/18 15:20:55  uspm
! Version dated 9-4-97 copied from COSMOS
!
! Revision 1.3  1997/04/07 12:33:43  uspm
! Version dated 18-3-97 copied from COSMOS
!
! Revision 1.2  1997/02/27 12:16:40  uspm
! Latest version from COSMOS
!
! Revision 1.1  1997/02/20 13:35:41  uspm
! Initial revision
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
! parameter statements
!-----------------------------------------------------------------------

INTEGER      BLKSIZ   ! max block size of dataset
INTEGER      INDLEN   ! index entry length
INTEGER      MDATA    ! max size of values array
INTEGER      MDES     ! max number of descriptors
INTEGER      MEXT     ! size of IEXTRA array                 !1.20
INTEGER      MXINDX   ! max number of index entries per block

PARAMETER    (BLKSIZ = 27998)
PARAMETER    (INDLEN = 12)
PARAMETER    (MDATA  = 180000)                               !1.17
PARAMETER    (MDES   = 2000)                                    !I
PARAMETER    (MEXT   = 99)                                   !1.20
PARAMETER    (MXINDX = BLKSIZ/INDLEN)

!-----------------------------------------------------------------------
! declare variables used as dimensions for arrays
!-----------------------------------------------------------------------

INTEGER NDES              !- no. of user in-line elements

!-----------------------------------------------------------------------
! Declare integer variables in alphabetical order.
! (See also note at end for different meanings of some variables    !2.2
!  when retrieving BUFR messages.)                                  !2.2
!-----------------------------------------------------------------------

INTEGER BSQ               !- BUFR displacement number
INTEGER BufrEdition       !- BUFR edition number
INTEGER BufrEnd           !- End of BUFR message                !J
INTEGER BufrStart         !- Start of BUFR message
INTEGER DISPL(MDES)       !- Displacements from BUFINDX
INTEGER DUMMY2(1)         !- Dummy integer array
INTEGER F                 !- F from FXXYYY BUFR descriptor
INTEGER IBLOCK            !- physical block number to read
INTEGER ICOPY             !- bytes to copy (BUFR retrievals)  !2.2
INTEGER ID                !- day
INTEGER ID2               !- day (end of index block)        !1.18
INTEGER IDATHR            !- time tag of index read
INTEGER IDAY              !- day from index block
INTEGER IDESC(NDES)       !- array of in-line elems pointers
INTEGER IDHR              !- hour from index entry
INTEGER IDREC             !- logical record number (maprd)
INTEGER IDSK(5)           !- dataset details
INTEGER IENT1             !- used in index entry looping
INTEGER IERR              !- MetDB error indicator
INTEGER IEXTRA(MEXT)      !- array of extra values           !1.20
INTEGER IFAIL             !- status flag from BUFINDX
INTEGER IFLAG             !- index entry byte 1
INTEGER IH                !- hour
INTEGER IH2               !- hour (end of index block)       !1.18
INTEGER IHOUR             !- hour from index block
INTEGER II                !- observation position
INTEGER ILAND             !- user land/sea indicator
INTEGER ILAT1             !- max lat of box
INTEGER ILAT2             !- min lat of box
INTEGER ILOCD             !- sequence descriptor (integer)
INTEGER ILON1             !- min lon of box
INTEGER ILON2             !- max lon of box
INTEGER ILSEA             !- index entry land/sea indicator
INTEGER ILVL(NDES)        !- user in-line elems replications
INTEGER IM                !- month
INTEGER IM2               !- month (end of index block)      !1.18
INTEGER INCR              !- users increment (itime(9))
INTEGER INDHR1            !- index block start time
INTEGER INDHR2            !- index block end time
INTEGER INXBLK            !- no. of index blocks in dataset
INTEGER INXHRS            !- no. of hours per index block
INTEGER IOB               !- last ob in users array
INTEGER IOBS              !- next ob in users array
INTEGER IRCVD(6)          !- Data from BUFR section 1           !G
INTEGER IRCV1             !- TOR in century hours
INTEGER IREG              !- limarea from index entry
INTEGER IRTIM(10)         !- users TOR request
INTEGER IRTIM1            !- users start TOR in century mins
INTEGER IRTIM2            !- users end TOR in century mins
INTEGER IS                !- bytes 3 & 4 from index entry
INTEGER ISAT(10)          !- list of users satids
INTEGER ISATID            !- index entry satid
INTEGER ISHR1             !- users time as start of index block
INTEGER ISHR2             !- users time as end of index block
INTEGER ISND1             !- 1st sounding number
INTEGER ISTAT             !- MetDB status indicator
INTEGER ITIME(9)          !- users request times
INTEGER ITIM1             !- user start time in century hours
INTEGER ITIM1S            !- saved 1st ITIM1
INTEGER ITIM2             !- users end time in century hours
INTEGER ITIM2S            !- save 1st ITIM2
INTEGER ITOR              !- index entry TOR
INTEGER ITORM             !- index entry TOR in minutes
INTEGER IY                !- year
INTEGER IY2               !- year (index block end)          !1.18
INTEGER J1                !- index block loop counter
INTEGER J2                !- index entry loop counter
INTEGER J3                !- loop counter for users satids
INTEGER K                 !- general loop counter
INTEGER LASTREP           !- last byte used in CREPT element  !2.2
INTEGER LENCREP           !- Length of elements of CREPT array!2.2
INTEGER LAT_SUBSCRIPT     !- lat subscript in users request     !D
INTEGER LON_SUBSCRIPT     !- lon subscript in users request     !D
INTEGER NAMHR             !- start hour of 1st index block
INTEGER NBLOCK            !- physical data block number
INTEGER NELEM             !- users no. of elements (ARRAY)
INTEGER NELMIX            !- block number for element index
INTEGER NELREQ            !- elements required from BUFINDX
INTEGER NOBS              !- users no. of obs (ARRAY)
INTEGER NREP              !- no. of BUFR obs (BUFINDX op)
INTEGER NSEND             !- no. of obs put in users array
INTEGER NSOUND            !- no. of obs in BUFR message
INTEGER NSQ               !- 1 or 0 if local descriptor or not
INTEGER NTRIES            !- no. of index entries
INTEGER NXTENT            !- next index entry
INTEGER NXTIME            !- next start time
INTEGER NXTSND            !- next ob
INTEGER RECLEN            !- dataset record length
INTEGER SOURCE(MDES)      !- source array from BUFINDX
INTEGER X                 !- XX from FXXYYY BUFR descriptor
INTEGER Y                 !- YYY from FXXYYY BUFR descriptor

! Note: For retrievals in BUFR, NSEND refers to a byte number in    !2.2
!       a data record rather than an ob. number in a message.       !2.2
!       Also II and IOBS refer to elements of CREPT rather than     !2.2
!       the number of obs. whose data has been put in VALUES.       !2.2

!-----------------------------------------------------------------------
! declare real variables in alphabetical order
!-----------------------------------------------------------------------

REAL         AREA(5)            !- user defined area            !D
REAL         ARRAY(NOBS,NELEM)  !- users array
REAL         RPOLE(2)           !- Rotated pole Lat Long coords !D
REAL         VALUES(MDATA)      !- DEBUFR values array

!-----------------------------------------------------------------------
! declare logical variables in alphabetical order
!-----------------------------------------------------------------------

LOGICAL      ELIDXFOUND     ! TRUE if element index found
LOGICAL      FIRST          ! TRUE if first call to SSMRET
LOGICAL      FOUND(*)       ! MetDB keywords selected           !J
LOGICAL      LCONT          ! TRUE if a continuation
LOGICAL      LFLAG          ! TRUE for diagnostics on
LOGICAL      LMID           ! TRUE if part of message passed
LOGICAL      LMSG           ! TRUE if MESSAGE in CREQ
LOGICAL      LOCDFG         ! TRUE if dataset has local seq
LOGICAL      LTOR           ! TRUE for accurate TOR check
LOGICAL      NEWMDBCALL     ! TRUE for new MetDB call           !A
LOGICAL      QCREQ          ! TRUE if QC bits required

!-----------------------------------------------------------------------
! declare character variables in alphabetical order
!-----------------------------------------------------------------------

CHARACTER*4          BUFR            ! 'BUFR' in EBCDIC
CHARACTER*10000      CINDX(12)       ! holds element index  !1.16b
CHARACTER*(BLKSIZ)   CMSG            ! BUFR message
CHARACTER*1          CNAM            ! characters from BUFR
CHARACTER*12         CNTRY(MXINDX*4) ! index entries          !2.1
CHARACTER*(*)        CREPT(NOBS)     ! users report text
CHARACTER*(*)        CSTR(NOBS)      ! users characters
CHARACTER*1          DUMMY1(1)       ! dummy character array
CHARACTER*8          ELIST           ! Element index member   !2.6
CHARACTER*6          LOCD            ! local D descriptor
CHARACTER*4          SEVENS          ! '7777' in EBCDIC         !J
CHARACTER*8          SUBTYPE         ! data subtype
CHARACTER*1          XTYPE           ! type of index 'A','B'

!-----------------------------------------------------------------------
! declare functions.
!-----------------------------------------------------------------------

INTEGER      CENTURY        ! Determine century from 2-fig year !E
INTEGER      DT2HRS         ! A function to return century hour

!-----------------------------------------------------------------------
! declare commons. Compile with FPARMS='DC(*)'
!-----------------------------------------------------------------------

COMMON /SSMCOM1/ VALUES                                         !B
COMMON /SSMCOM2/ CMSG,CNTRY,CINDX                               !B
COMMON /SSMCOM3/ SOURCE,IEXTRA,DISPL                            !B

!-----------------------------------------------------------------------
! SAVE needed to ensure that contents of variables/arrays are still
! available on next entry to subroutine.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! data statements
!-----------------------------------------------------------------------

DATA FIRST  /.TRUE./            !- first call to SSMRET
DATA NXTIME /0/
DATA NXTENT /0/
DATA NXTSND /0/

CHARACTER*132 HEAD

!-----------------------------------------------------------------------
! First time only:  Initialise revision information, BUFR and SEVENS
!-----------------------------------------------------------------------

IF (FIRST) THEN                                               !2.2
  HEAD='$RCSfile: ssmret.f,v $ ' //&
      &'$Revision: 1$ $Date: 26/01/2010 10:18:13$'
  BUFR   = CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82) ! 'BUFR'    !2.2
  SEVENS = CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55) ! '7777'    !2.2
  FIRST=.FALSE.                                               !2.2
END IF                                                        !2.2

! ----------------------------------------------------------------------
! diagnostic output to say SSMRET has been called.
! ----------------------------------------------------------------------

IF (LFLAG) THEN
  WRITE(*,'(/1X,''In MetDB subroutine SSMRET'' )')
  WRITE(*,'( 1X,''==========================''/)')
  WRITE(*,'( 1X,''Data subtype = '',A8/)')SUBTYPE
ENDIF

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

IERR = 0
IF (.NOT.FOUND(34)) II=0  !- no reset of II in BUFR retrieval !2.5
IOB  = IOBS-1

IF (LFLAG) WRITE(*,*)'In SSMRET: Entry: II,IOB,IOBS,NOBS = ',&
                     &II,IOB,IOBS,NOBS

!-----------------------------------------------------------------------
! Check for continuation and set up loop counters
!-----------------------------------------------------------------------

IF (ISTAT.EQ.16) THEN
  IF (LFLAG) WRITE(*,*)'In SSMRET: New dataset'
  ISTAT = 0
ELSE IF (ISTAT.EQ.4) THEN
  IF (LFLAG) WRITE(*,*)'In SSMRET: Continuation'
  ISHR1 = NXTIME     ! start time
  IENT1 = NXTENT     ! first entry number
  ISND1 = NXTSND     ! first sounding number
  NSEND = NXTSND-1
  LCONT = .TRUE.     ! this is a continuation
  IF (LFLAG) WRITE(*,*)'In SSMRET: ISHR1,IENT1,ISND1,NSEND = ',&
 &ISHR1,IENT1,ISND1,NSEND
ELSE
  CONTINUE
ENDIF

!=======================================================================
! if ISTAT=0 (new call to MetDB) so start here.
!=======================================================================

IF (ISTAT.EQ.0) THEN

  IF (LFLAG) THEN
    WRITE(*,*)'In SSMRET:'
    WRITE(*,*)' Descriptor  Replication'
    WRITE(*,'(1X,I8,8X,I3/)')(IDESC(K),ILVL(K),K=1,NDES)
    WRITE(*,*)'In SSMRET: AREA: ',AREA
  ENDIF

!-----------------------------------------------------------------------
! check if NDES > MDES. If so, output an error message & exit the
! subroutine. MDES is the max number of elements that can be handled
! by this routine. Exceeding this limit will lead to overwriting in
! routines downstream to this. MDES is enough to service any user
! request as providing they have observed replication count
! sizes described in subtype documentation.                        !1.20
!-----------------------------------------------------------------------

  IF (NDES.GT.MDES) THEN                                     !1.20
    IERR=16                                                  !1.20
    WRITE(*,'(1X,''SSMRET: MDB ERROR: NDES > MDES! '',&
   &''NUMBER OF ELEMENTS IN REQUEST STRING (NES)'')')        !1.20
    WRITE(*,'(1X,''EXCEEDS MAX PERMITTED (MDES). '',&
   &''CHECK REQUEST AGAINST SUBTYPE DOCUMENTATION.'')')      !1.20
    WRITE(*,'(1X,''CONTACT METDB TEAM FOR ADVICE IF '',&
   &''NECESSARY.'')')                                        !1.20
    WRITE(*,'(1X,''INFO: SUBTYPE = '',A8,''  NDES = '',I6,&
   &''  MDES = '',I6)')SUBTYPE,NDES,MDES                     !1.20
    GOTO 999                                                 !1.20
  ENDIF                                                      !1.20

!-----------------------------------------------------------------------
! check that NELEM (2nd dimension in users array ARRAY) is big enough
! to hold the number of search sequence index numbers NDES. If not.
! output an error and exit subroutine.
!-----------------------------------------------------------------------

  IF (NELEM.LT.NDES) THEN
    IERR = 16
    WRITE(*,'(1X,''MDB ERROR: ARRAY NOT LARGE ENOUGH !'')')
    WRITE(*,'(1X,''TRY ARRAY ('',I3,'','',I3,'')'')')NOBS,NDES
    GOTO 999
  ENDIF

!-----------------------------------------------------------------------
! read map block to find dataset details. If LOCDFG is TRUE, then there
! is a local table D descriptor. Set NSQ=1.
!-----------------------------------------------------------------------

  CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,&
            &NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,&
            &RECLEN,CMSG,'MAPRD')

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

    IF (SUBTYPE(1:6).EQ.'SAT120') CALL GETLOCD (SUBTYPE)      !2.2

  ENDIF  !- locdfg

!-----------------------------------------------------------------------
! read the element index from the the element index dataset.
!-----------------------------------------------------------------------

  CALL READIDX(ELIST,CINDX,XTYPE,ELIDXFOUND)                  !2.6

  IF (.NOT.ELIDXFOUND) THEN
    WRITE(*,*)'In SSMRET: MDB ERROR: Cannot find element ',&
   &'index for subtype,elist = ',SUBTYPE,ELIST
    IERR=16                                                   !2.4
    GOTO 999
  ENDIF

  IF (LFLAG) THEN
    WRITE(*,*)'In SSMRET: after call readidx, XTYPE=',XTYPE
  ENDIF

!-----------------------------------------------------------------------
! Convert times to century hours
!-----------------------------------------------------------------------

  ITIM1=DT2HRS(ITIME(1),ITIME(2),ITIME(3),ITIME(4))

  IF (ITIME(5).EQ.0) THEN
    ITIM2=ITIM1
  ELSE
    ITIM2=DT2HRS(ITIME(5),ITIME(6),ITIME(7),ITIME(8))
  ENDIF

  INCR=ITIME(9)

  IF (INCR.NE.1) THEN
    WRITE(*,*)'In SSMRET: MDB ERROR: Invalid increment -',INCR
    INCR=1
  ENDIF

  IF (LFLAG) WRITE(*,'(1X,''In SSMRET: Start '',I10,'' End '',&
            &I10)')ITIM1,ITIM2

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

  IF (LFLAG) THEN
    WRITE(*,'('' No of index blocks      (INXBLK)   '',I8/,&
             &'' hours per block         (INXHRS)   '',I8/,&
             &'' start of 1st slot       (NAMHR)    '',I8/,&
             &'' hour offset             (INDHR1/2) '',2I4/,&
             &'' slot hours for request  (ISHR1/2)  '',2I8)')&
             &INXBLK,INXHRS,NAMHR,INDHR1,INDHR2,ISHR1,ISHR2

  ENDIF

!-----------------------------------------------------------------------
! time of receipt in century minutes
!-----------------------------------------------------------------------

  IF (IRTIM(1).EQ.0) THEN
    IRTIM1=0
  ELSE
    IRTIM1=DT2HRS(IRTIM(1),IRTIM(2),IRTIM(3),IRTIM(4))
    IRTIM1=(IRTIM1-1)*60 + IRTIM(5)
  ENDIF

  IF (IRTIM(6).EQ.0) THEN
    IRTIM2=0
  ELSE
    IRTIM2=DT2HRS(IRTIM(6),IRTIM(7),IRTIM(8),IRTIM(9))
    IRTIM2=(IRTIM2-1)*60 + IRTIM(10)
  ENDIF

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

ENDIF   !- istat=0

ISTAT=0

!=======================================================================
!    INITIALISE OUTPUT ARRAYS AND POINTERS FOR RETRIEVALS IN BUFR   !2.2
!=======================================================================

!                                Clear out CREPT and ARRAY if necessary
!
IF (FOUND(34) .AND. (LMSG.OR.IOB.LE.0)) THEN                  !2.2
  LENCREP = LEN(CREPT(1)) ! Length of elements of CREPT       !2.2
  II = 0                  ! Return to start of user's ARRAY   !2.2
  LASTREP = LENCREP       ! Put new data in new CREPT element !2.2
!
!                                                 Reset CREPT and ARRAY
  DO K=1,LENCREP          ! First element                     !2.2
    CREPT(1)(K:K) = CHAR(0)                                   !2.2
  END DO ! K                                                  !2.2
  ARRAY(1,1) = 0.0                                            !2.2
!
  DO K=2,NOBS             ! Other elements                    !2.2
    CREPT(K) = CREPT(1)                                       !2.2
    ARRAY(K,1) = 0.0                                          !2.2
  END DO ! K                                                  !2.2
END IF                                                        !2.2

!=======================================================================
! main loop over index blocks
!=======================================================================

IF (LFLAG) WRITE(*,*) 'In SSMRET: Looping over times ',&
                      &ISHR1,ISHR2,INXHRS

DO 799 J1=ISHR1,ISHR2,INXHRS

!-----------------------------------------------------------------------
! read index block for this period
!-----------------------------------------------------------------------

  IBLOCK = MOD(J1/INXHRS,INXBLK)+2+NSQ

  CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,&
            &NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,&
            &RECLEN,CMSG,'IDXRD')

!-----------------------------------------------------------------------
! use time-tag to check that correct date/time has been read
!-----------------------------------------------------------------------

  IDAY  = IDATHR/256
  IHOUR = IDATHR-IDAY*256

  CALL HRS2DT(IY,IM,ID,IH,J1)

  IF (ID.NE.IDAY.OR.IH.NE.IHOUR) THEN
    CALL HRS2DT(IY2,IM2,ID2,IH2,J1+INXHRS-1)                 !1.18
    WRITE(6,9003)SUBTYPE,IY,IM,ID,IH,IY2,IM2,ID2,IH2         !1.18
    GOTO 799
  ENDIF

!=======================================================================
! main loop over index entries for this period
!=======================================================================

  IF (LFLAG) THEN
    WRITE(*,*)'In SSMRET: Index block ',IBLOCK,' Entries ',NTRIES
    WRITE(*,*)'In SSMRET: Looping over entries ',IENT1,NTRIES
  ENDIF

  DO 699 J2=IENT1,NTRIES

    IS=ICHAR(CNTRY(J2)(3:3))*256+ICHAR(CNTRY(J2)(4:4))

    NSOUND=MOD(IS,1024)
    IF (NSOUND.LT.0) NSOUND = NSOUND + 1024
    IF (LFLAG) THEN
      WRITE(*,*) 'In SSMRET: Entry ',J2,' Soundings ',NSOUND
    ENDIF

!-----------------------------------------------------------------------
! if message hasn't already been decoded...
!-----------------------------------------------------------------------

    IF (.NOT.LMID) THEN

!-----------------------------------------------------------------------
! if only part of block needed, check hour
!-----------------------------------------------------------------------

      IDHR=ICHAR(CNTRY(J2)(2:2))
      IDHR=MOD(IDHR,32)

      IF ((J1.EQ.ISHR1).AND.(ISHR1.EQ.ITIM1S)) THEN

        IF (LFLAG) THEN
          WRITE(*,'(1X,''In SSMRET: Entry hour offset (1) '',I5)')&
         &IDHR
        ENDIF
        IF (IDHR.LT.INDHR1) GOTO 699
      ENDIF

      IF (J1.EQ.ISHR2) THEN
        IF (LFLAG) THEN
          WRITE(*,'(1X,''In SSMRET: Entry hour offset (2) '',I5)')&
         &IDHR
        ENDIF
        IF (IDHR.GT.INDHR2) GOTO 699
      ENDIF

!-----------------------------------------------------------------------
! check satellite identifier
!-----------------------------------------------------------------------

      ISATID=MOD(ICHAR(CNTRY(J2)(1:1)),64)*8+&
     &ICHAR(CNTRY(J2)(2:2))/32

      IF (LFLAG) WRITE(*,*)'In SSMRET: sat id',ISAT,ISATID

      DO 123 J3=1,10
        IF (ISAT(J3).EQ.0) THEN
          IF (J3.EQ.1) THEN
            GOTO 125    ! no sat ids to match
          ELSE
            GOTO 699    ! end of list and no match
          ENDIF
        ENDIF
  IF (ISATID.EQ.ISAT(J3)) GOTO 125   ! matched
123         CONTINUE

      GOTO 699    ! 10 ids with no match!

125         CONTINUE

      IF (LFLAG) WRITE(*,*)'In SSMRET: No sats to match'

!-----------------------------------------------------------------------
! now check areas
!-----------------------------------------------------------------------

      ILAT2=ICHAR(CNTRY(J2)(5:5))-90
      ILAT1=ICHAR(CNTRY(J2)(6:6))-90
      ILON1=(ICHAR(CNTRY(J2)(7:7))-90)*2
      ILON2=(ICHAR(CNTRY(J2)(8:8))-90)*2

      IF (AREA(1).EQ.0.0) THEN                                  !D
        IF (ILAT2.GT.AREA(2) .OR.&         ! Obs too far N    !2.2
           &ILAT1.LT.AREA(4)) THEN         ! Obs too far S    !2.2
          GO TO 699                                           !2.2
!                            Neither box crosses the dateline !2.2

        ELSE IF (ILON1.LE.ILON2 .AND. AREA(3).LE.AREA(5)) THEN!2.2
          IF (ILON2.LT.AREA(3) .OR.&       ! Obs too far W    !2.2
             &ILON1.GT.AREA(5)) GO TO 699  ! Obs too far E    !2.2

!                                One box crosses the dateline !2.2

        ELSE IF (ILON1.LE.ILON2 .OR. AREA(3).LE.AREA(5)) THEN !2.2
          IF (ILON2.LT.AREA(3) .AND.&      ! Obs too far W    !2.2
             &ILON1.GT.AREA(5)) GO TO 699  ! Obs too far E    !2.2
        END IF                                                !2.2
      END IF

      IF (LFLAG) THEN
        WRITE(*,*)'In SSMRET: Area DB',ILAT1,ILON1,ILAT2,ILON2
        WRITE(*,*)'In SSMRET: Area RQ',(AREA(K),K=2,5)          !D
      ENDIF

!-----------------------------------------------------------------------
! land/sea and fine mesh flags
!-----------------------------------------------------------------------

      IFLAG = ICHAR(CNTRY(J2)(1:1))
      ILSEA = IFLAG/128              ! ILSEA=1 for sea
      IREG  = MOD(IFLAG/64,2)        ! IREG=1 for outside the area

      IF (LFLAG) WRITE(*,*)'In SSMRET: land/sea and reg:',&
     &ILSEA,IREG

      IF (AREA(1).EQ.-1.0 .AND. IREG.EQ.1) GOTO 699             !D

      IF (ILAND.EQ.1 .AND. ILSEA.NE.0 .OR.&
         &ILAND.EQ.2 .AND. ILSEA.NE.1) GOTO 699

!-----------------------------------------------------------------------
! Rough time of receipt check. LTOR is set true for a more accurate
! check later on. TOR can be missing data if ob is received more
! than 12 hours after base time of slot.
!
! Change users RECEIVED BEFORE check slightly. Change ITORM.GT.IRTIM2
! to ITORM.GE.IRTIM2                                                  !A
!-----------------------------------------------------------------------

      LTOR=.FALSE.
      IF (IRTIM1+IRTIM2.NE.0) THEN
        ITOR=ICHAR(CNTRY(J2)(9:9))
        IF (ITOR.GE.255) THEN
          LTOR=.TRUE.
        ELSE
          ITOR=ICHAR(CNTRY(J2)(9:9))*3
          ITORM=(J1-1)*60+ITOR
          IF (LFLAG) WRITE(*,*)'In SSMRET: ITORM:',ITORM

          IF ((IRTIM1.NE.0 .AND. (ITORM+2).LT.IRTIM1) .OR.&
             &(IRTIM2.NE.0 .AND. ITORM.GE.IRTIM2)) GOTO 699     !A

          IF (ABS(IRTIM1-ITORM).LT.3 .OR.&
             &ABS(IRTIM2-ITORM).LT.3) LTOR=.TRUE.

        ENDIF
      ENDIF

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

      CALL MAPRD(IDSK,INXBLK,INXHRS,NAMHR,INDLEN,LFLAG,LOCDFG,&
                &NELMIX,IBLOCK,NTRIES,CNTRY,IDATHR,IDREC,&
                &RECLEN,CMSG,'MSGRD')

      IF (LFLAG) THEN
        WRITE(*,*)'In SSMRET: Block ',IBLOCK,' Record ',IDREC
      ENDIF

!-----------------------------------------------------------------------
! Get BUFR edition number from message, byte 8 in BUFR message.
!-----------------------------------------------------------------------

      BufrEdition=ICHAR(CMSG(8:8))

      IF (BufrEdition.LE.1) THEN        ! BUFR edition 0 or 1
        BSQ=0
      ELSE
        BSQ=4                           ! BUFR edition 2 or 3
      ENDIF

      IF (LFLAG) WRITE(*,*)'In SSMRET: BufrEdition = ',BufrEdition

!-----------------------------------------------------------------------
! check TOR in section 1. There is a crude year 2000 check here!
!-----------------------------------------------------------------------

      IRCVD(1)=ICHAR(CMSG(17+BSQ:17+BSQ))
      IRCVD(1)=IRCVD(1)+CENTURY(IRCVD(1))                       !E

      IRCVD(2)=ICHAR(CMSG(18+BSQ:18+BSQ))
      IRCVD(3)=ICHAR(CMSG(19+BSQ:19+BSQ))
      IRCVD(4)=ICHAR(CMSG(20+BSQ:20+BSQ))
      IRCVD(5)=ICHAR(CMSG(21+BSQ:21+BSQ))

!-----------------------------------------------------------------------
! extract the BUFR subtype from BUFR section 1                        !G
!-----------------------------------------------------------------------

      IRCVD(6)=ICHAR(CMSG(14+BSQ:14+BSQ))

!-----------------------------------------------------------------------
! Additional date/time check. If the TOR is more than a day older than
! the index block data/time, reject the sounding. S.Cox 3/9/96
!-----------------------------------------------------------------------

      IRCV1=DT2HRS(IRCVD(1),IRCVD(2),IRCVD(3),IRCVD(4))

      IF ((J1-IRCV1).GE.24) THEN
        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: TOR at least 24 hrs older than'
          WRITE(*,*)'data time. Rejecting sounding! TOR = ',IRCV1,&
                   &'data time = ',J1
        ENDIF
        GOTO 699
      ENDIF

      IF (LTOR) THEN
        IRCV1=(IRCV1-1)*60+IRCVD(5)
        LTOR=.FALSE.

        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: TOR = ',IRCV1,IRTIM1,IRTIM2
        ENDIF

!-----------------------------------------------------------------------
! Change users RECEIVED BEFORE check slightly. Change IRCV1.GT.IRTIM2
! to IRCV1.GE.IRTIM2                                                  !A
!-----------------------------------------------------------------------

        IF ((IRTIM1.NE.0.AND.IRCV1.LT.IRTIM1) .OR.&
           &(IRTIM2.NE.0.AND.IRCV1.GE.IRTIM2)) GOTO 699         !A
      ENDIF

!-----------------------------------------------------------------------
! Get the local D sequence number to identify the message type.
! (Not required if retrieving undecoded BUFR messages.)
!-----------------------------------------------------------------------

      IF (.NOT.FOUND(34)) THEN                                !2.2
        ILOCD=ICHAR(CMSG(30+BSQ:30+BSQ))*256+&
             &ICHAR(CMSG(31+BSQ:31+BSQ))
        CALL DESFXY(ILOCD,F,X,Y)

!-----------------------------------------------------------------------
! Patch for SAT120 data. If there is not a sequence descriptor in the
! BUFR message, and the subtype is SAT120, then we need to expand the
! BUFR descriptors to determine the correct sequence descriptor to pass
! to BUFINDX. This patch will not be needed for the TOMS project, as
! the SAT120 on-line dataset will have BUFR messages containing local
! sequence descriptors.
!-----------------------------------------------------------------------

        IF (F.NE.3) THEN
          IF (SUBTYPE(1:6).EQ.'SAT120') THEN                  !2.2
            CALL SUBSLOCD(SUBTYPE,CMSG,ILOCD)                   !A
            WRITE(LOCD,'(I6)')ILOCD
            IF (LFLAG) THEN
              WRITE(*,*)'In SSMRET, LOCD from SUBSLOCD = ',ILOCD
            ENDIF
            IF (ILOCD.EQ.0) THEN
             WRITE(*,*)'In SSMRET: MDB ERROR: ILOCD from SUBSLOCD'
              WRITE(*,*)'is equal to zero! Retrieval halted'
              IERR = 16
              GOTO 999
            ENDIF
          ELSE
            WRITE(*,*)'In SSMRET: MDB ERROR: No sequence '
            WRITE(*,*)'descriptor in BUFR message and subtype '
            WRITE(*,*)'is not SAT120'                         !2.2
            IERR = 16
            GOTO 999
          ENDIF
        ELSE
          WRITE(LOCD,'(I1,I2.2,I3.3)')F,X,Y
        ENDIF

        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: LocalD from message=',ILOCD,LOCD
          WRITE(*,*)'In SSMRET: Cmsg=',CMSG(1:31+BSQ)
        ENDIF

!-----------------------------------------------------------------------
! call BUFINDX to decide the displacements within the message.
!-----------------------------------------------------------------------

        BufrStart = INDEX(CMSG,BUFR)                            !J
        BufrEnd   = INDEX(CMSG,SEVENS)+3                        !J

        CALL BUFINDX(CINDX,IDESC,NDES,ILVL,QCREQ,IFAIL,&     !2.0
                    &LFLAG,LOCD,CMSG(BufrStart:),NELREQ,DISPL,&
                    &SOURCE,VALUES,MDATA,CNAM,IEXTRA,NREP,&  !1.20
                    &MDES,NEWMDBCALL,MEXT)                   !1.20

        IF (NREP.LE.0) GOTO 699                                 !K

        IF (IFAIL.EQ.16) THEN   ! element index not found
          IERR=16                                           !1.14a
          GOTO 999
        ENDIF

      ENDIF   ! .not.lmid
    ENDIF

!-----------------------------------------------------------------------
! Retrieve BUFR messages (FOUND(34) is true):  Copy data to CREPT array.
!-----------------------------------------------------------------------

    IF (FOUND(34)) THEN                                         !J
!                                 Check that CREPT can hold the message
!
      IF (RECLEN.GT.NOBS*LENCREP) THEN                        !2.3
         WRITE (6,'(T2,2A,I7)') 'SSMRET: Message skipped ',&  !2.3
                 &'as too big for CREP. Size =', RECLEN       !2.3
         NSEND = RECLEN  ! To skip message                    !2.3
!
!                   Check that free space in CREPT can hold the message
!
      ELSE IF (RECLEN.LE.(NOBS+1-II)*LENCREP-LASTREP) THEN    !2.3
!
!                             Transfer batches of data until either all
!                                has been copied or CREPT array is full
!
        DO WHILE (NSEND.LT.RECLEN .AND.&                      !2.2
                 &(LASTREP.LT.LENCREP .OR. II.LT.NOBS))       !2.2
!
!                           If CREPT element full, move to next element
!
          IF (LASTREP.EQ.LENCREP) THEN ! Full                 !2.2
            II = II + 1                ! Next element         !2.2
            LASTREP = 0                ! It's empty           !2.2
          END IF
!                            Find number of bytes to copy in next batch
!
!                       Min of (Bytes left, Space in CREPT)         !2.2
          ICOPY = MIN0 (RECLEN-NSEND,LENCREP-LASTREP)         !2.2
!
!                               Transfer batch of data to CREPT element
!
          CREPT(II)(LASTREP+1:LASTREP+ICOPY) =&               !2.2
              &CMSG(  NSEND+1:  NSEND+ICOPY)                  !2.2
!
!                                             Update pointers and ARRAY
          NSEND = NSEND + ICOPY                               !2.2
          LASTREP = LASTREP + ICOPY                           !2.2
          ARRAY(II,1) = FLOAT(LASTREP)                        !2.2
        END DO                                                !2.2
      END IF                                                  !2.3
      NREP = RECLEN ! so that test after VALARR is OK         !2.2
    ELSE                                                        !J

!-----------------------------------------------------------------------
! Retrieve decoded elements: Call VALARR to put data into VALUES array.
!-----------------------------------------------------------------------

      CALL VALARR(DISPL,NELREQ,SOURCE,IRCVD,DUMMY1,&            !J
                 &DUMMY2,IEXTRA,VALUES,CNAM,'    1 ',&        !2.2
                 &ISND1,NSEND,NREP,ARRAY,NOBS,NELEM,&           !J
                 &IOB,II,CSTR,CREPT,LFLAG,LAT_SUBSCRIPT,&       !J
                 &LON_SUBSCRIPT,AREA,RPOLE)                 !J!H!D
    ENDIF                                                       !J

!-----------------------------------------------------------------------
! save next loop counts to be processed by next entry
!-----------------------------------------------------------------------

    IF (NSEND.LT.NREP) THEN                                   !H!D
      LMID=.TRUE.                                               !D
!           IF (LMSG) THEN                                            !K
!             WRITE(*,*)'In SSMRET: MDB ERROR: ARRAY NOT BIG ENOUGH ',!K
!    &                  'FOR 1 MESSAGE'                               !K
!             IERR=16                                                 !K
!             GOTO 999                                                !K
!           ENDIF                                                     !K

      IF (LFLAG) WRITE(*,*)'In SSMRET: No more room in array',&
     &IOB

      NXTSND=NSEND+1
      NXTIME=J1
      NXTENT=J2

      ISTAT=4
      IF (LFLAG) THEN
        WRITE(*,*)'In SSMRET: NXTSND,NXTENT,NXTIME=',&
       &NXTSND,NXTENT,NXTIME
      ENDIF
      GOTO 999
    ELSE
      LMID=.FALSE.                                              !D
      IOB=II         !- changed from IOB=IOB+(NSEND-ISND1)+1    !D
      NSEND=0
      NXTSND=1
      ISND1=1
      IF ((.NOT.FOUND(34).AND.IOB.EQ.NOBS)& ! ARRAY filled    !2.2
        &.OR. (LMSG.AND.IOB.GT.0)) THEN     ! 1 msg. copied   !2.2
        IF (J2+1.GT.NTRIES) THEN
          NXTENT=1
          IF (J1+INXHRS.GT.ISHR2) GOTO 999
          NXTIME=J1+INXHRS
        ELSE
          NXTIME=J1
          NXTENT=J2+1
        ENDIF
        ISTAT=4
        IF (LFLAG) THEN
          WRITE(*,*)'In SSMRET: Array full: NXTSND',&
         &'NXTENT,NXTIME=',NXTSND,NXTENT,NXTIME
        ENDIF
        GOTO 999
      ENDIF
    ENDIF

699     CONTINUE   ! End of loop over entries this hour

  IENT1=1

799   CONTINUE   ! End of loop over hours

IF (IOB.EQ.0) THEN
  IF (LCONT) THEN
    ISTAT=0
  ELSE
    ISTAT=8
  ENDIF
ENDIF

!-----------------------------------------------------------------------
! return number of soundings
!-----------------------------------------------------------------------

999   CONTINUE

IF (II.NE.0) THEN                                           !1.14b
  IOBS=II                                                   !1.14b
ELSE                                                        !1.14b
  IOBS=IOBS-1                                               !1.14b
ENDIF                                                       !1.14b

IF (LFLAG) THEN
  WRITE(*,*)'In SSMRET: EXIT. II,IOB,IOBS,NOBS',II,IOB,IOBS,NOBS
ENDIF

9003  FORMAT(' NO ',A8, ' DATA IN MDB FOR PERIOD ',I4,'/',&
     &I2.2,'/',I2.2,1X,I2.2,'Z - ',I4,'/',I2.2,'/',&
     &I2.2,1X,I2.2,'Z')                                     !1.18

RETURN
END SUBROUTINE SSMRET
