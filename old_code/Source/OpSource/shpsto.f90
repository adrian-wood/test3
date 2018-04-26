SUBROUTINE SHPSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : SHPSTO   (SAME AS SYNSTO BUT DIFFERENT COMMON)
!
!                (NOTE: PRE-PROCESSING STATEMENTS HAVE NOW BEEN REMOVED)
!
! PURPOSE       : Store Synops in the MDB (23-byte index entries,
!                 Reports chained in data blocks)
!
! DATA TYPES    : SHIPS' SYNOPS
!
! CALLED BY     : SYNIND
!
! CALLS         : DATE31, SORTCH, CENTURY
!
! PARAMETERS    : (1) DATE/TIME (YEAR, MONTH...)
!                 (2) INDEX ENTRY (WITHOUT TIMES & BLOCK/RECORD NO)
!                 (3) REPORT TO BE STORED
!                 (4) FT NUMBER (ASSUME IT'S ALWAYS THE SAME)
!                 (5) BLOCKSIZE OF OUTPUT DATA SET
!                 (6) IDENTIFIER TO GO IN INDEX ENTRY
!
! REVISION INFO :
!
! $Workfile: shpsto.f90$ $Folder: OpSource$
! $Revision: 15$ $Date: 15/06/2012 16:37:24$
! $Author: Alison Weir$
!
! CHANGE RECORD :
!
! $Log:
!  15   MetDB_Refresh 1.14        15/06/2012 16:37:24    Alison Weir     Sort
!       ID ranges using adjusted IDs for 'AMOUK' IDs.
!  14   MetDB_Refresh 1.13        03/02/2012 10:35:50    Sheila Needham
!       Removed PRINT* with binary data
!  13   MetDB_Refresh 1.12        23/03/2011 16:27:21    Brian Barwell   BUFFER
!        length changed to MAXBLK. SAVE added. DO loop rewritten.
!  12   MetDB_Refresh 1.11        07/03/2011 17:14:00    Brian Barwell
!       Modified for C I/O.
!  11   MetDB_Refresh 1.10        26/01/2011 11:15:23    Brian Barwell
!       Correction to reviewer's recommended rework.
!  10   MetDB_Refresh 1.9         25/01/2011 09:39:26    Stan Kellett
!       Corrected error in rework
!  9    MetDB_Refresh 1.8         25/01/2011 09:28:37    Stan Kellett    Rework
!        done after review of MDBSTORBatch10
!  8    MetDB_Refresh 1.7         18/01/2011 11:08:53    Stan Kellett    basic
!       porting done and comments put in review form for batch 10 so now ready
!        for review
!  7    MetDB_Refresh 1.6         18/01/2011 09:06:49    Stan Kellett
!       removed unused variables NTOTL and LFLAGS
!  6    MetDB_Refresh 1.5         17/01/2011 16:44:12    Stan Kellett    added
!       use statements
!  5    MetDB_Refresh 1.4         17/01/2011 14:48:04    Stan Kellett
!       variable declarations all moved to f95 standards, all compiles before
!       adding of use statements and maintainance enhancements.
!  4    MetDB_Refresh 1.3         31/12/2010 15:16:02    Stan Kellett
!       Implicit none added.
!       Declare variables not declared.
!       Remove all old revision info and 3/4 of way through setting local
!       variables in f95 standard.
!  3    MetDB_Refresh 1.2         31/12/2010 12:25:28    Stan Kellett    Long
!       do and If Labels added. got rid of some gotos with do while loops,
!       corrected some indenting.
!  2    MetDB_Refresh 1.1         30/12/2010 16:19:41    Stan Kellett
!       comments changed to !
!       HEAD removed
!       changed to free format by removing leading spaces
!       Continuation lines moved to end of lines rather than start
!  1    MetDB_Refresh 1.0         30/12/2010 14:58:49    Stan Kellett
!       initial f77 version of files before porting
! $
!
! MADE FROM TAFREP OCT 1995
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE zpdate_mod
USE datim_mod
USE eb2asc_mod
use sortch_mod
use century_mod

IMPLICIT NONE

!Parameters

INTEGER, PARAMETER                :: MAXBLK=27998   ! Max Blksize set by max record on GPCS
INTEGER, PARAMETER                :: INDLEN=23      ! Index Length in bytes
INTEGER, PARAMETER                :: INDHED=6       ! Index Head
INTEGER, PARAMETER                :: MAXOFL=3       ! Maximum overflow

!Arguments in, inout or out

INTEGER, INTENT(INOUT)            :: DATIME(5)
CHARACTER(LEN=*),INTENT(INOUT)    :: ENTRY
CHARACTER(LEN=*),INTENT(IN)       :: BULL
INTEGER, INTENT(IN)               :: IFT
INTEGER, INTENT(IN)               :: BLKSIZ
CHARACTER(LEN=*),INTENT(INOUT)    :: IDENT

!Local Variables

INTEGER                           :: AMSTAR
INTEGER                           :: BLKTAG
INTEGER                           :: BLOCKS
INTEGER                           :: CENDAY
INTEGER                           :: CENTHR
INTEGER                           :: CURENT
INTEGER                           :: DISPL
INTEGER                           :: HOURNOW
INTEGER                           :: I
INTEGER                           :: IDINB
INTEGER                           :: IFLAGS
INTEGER                           :: IND
INTEGER                           :: INDENT
INTEGER                           :: INDHOR          ! Index Hour
INTEGER                           :: IORC
INTEGER                           :: IPOS            ! Pointer to position in string
INTEGER                           :: ISTART
INTEGER                           :: ITOR
INTEGER                           :: IX
INTEGER                           :: IY
INTEGER                           :: J
INTEGER                           :: K
INTEGER                           :: L
INTEGER                           :: LASTHR
INTEGER                           :: LASTIX          ! Current Index Block
INTEGER                           :: LATEST = 0      ! LATEST=0 AT START
INTEGER                           :: LEFT
INTEGER                           :: LEFX
INTEGER                           :: LENBUF
INTEGER                           :: LIDENT
INTEGER                           :: LISTAG          ! List of ranges
INTEGER                           :: LL
INTEGER                           :: LN
INTEGER                           :: LOB
INTEGER                           :: LX
INTEGER                           :: N
INTEGER                           :: NAFTER
INTEGER                           :: NB
INTEGER                           :: NBLIND
INTEGER                           :: NBLOC           ! Number of Blocks
INTEGER                           :: NBLOCK
INTEGER                           :: NBLOCX
INTEGER                           :: NBLOK
INTEGER                           :: NCHAIN
INTEGER                           :: NEXTIX
INTEGER                           :: NFLIX
INTEGER                           :: NFULL
INTEGER                           :: NIBL
INTEGER                           :: NINBLK
INTEGER                           :: NINBLX
INTEGER                           :: NIND
INTEGER                           :: NL
INTEGER                           :: NOB
INTEGER                           :: NOBS
INTEGER                           :: NTOTAL
INTEGER                           :: NOTPTR
INTEGER                           :: NOW(8)
INTEGER                           :: NPREF
INTEGER                           :: NREP
INTEGER                           :: NSEQBL
INTEGER                           :: NSTART
INTEGER                           :: NTIMES          ! number of blank index entries
INTEGER                           :: NSQ
INTEGER                           :: NX
INTEGER                           :: NXBLOK          ! Number of index blocks
INTEGER                           :: OFLOWS
INTEGER                           :: POINTR
INTEGER                           :: RECDIS(MAXBLK/2)
INTEGER                           :: REC             ! record number
INTEGER                           :: RECNO
INTEGER                           :: SLODAY
INTEGER                           :: SLOTHR
INTEGER                           :: START
INTEGER                           :: X
INTEGER                           :: XBLOKS
INTEGER                           :: XHOURS
INTEGER                           :: Y
LOGICAL                           :: INSERT
CHARACTER(LEN=4)                  :: BUFR = 'BUFR'
CHARACTER(LEN=2)                  :: CHARBL
CHARACTER(LEN=2)                  :: CHARBLX
CHARACTER(LEN=INDLEN)             :: EMPTYX = ' '
CHARACTER(LEN=4)                  :: IDENT4          ! 4 character ident
CHARACTER(LEN=INDLEN)             :: INDEKS(MAXOFL*MAXBLK/INDLEN,3) ! INDEX ENTRIES
CHARACTER(LEN=6)                  :: INDHDR
CHARACTER(LEN=2)                  :: INDOVR
CHARACTER(LEN=INDLEN)             :: MASK = '  XXXXX                '  ! TO SORT ON IDENT
CHARACTER(LEN=INDLEN)             :: MASX = '           X           '  ! TO SORT ON COUNT
CHARACTER(LEN=1)                  :: MEDIAN
CHARACTER(LEN=5)                  :: MID
CHARACTER(LEN=14)                 :: MSGHDR = '....Z SHPSTO: '
CHARACTER(LEN=4)                  :: POINTER
CHARACTER(LEN=1)                  :: QUARTL
CHARACTER(LEN=MAXBLK)             :: BUFFER    ! Buffer for use in C I/O
! INDEXES FOR THREE 6-HOUR PERIODS ARE KEPT IN CORE: SUBSCRIPT LX SAYS
! WHICH IS IN USE FOR THE CURRENT REPORT.  SO ALL ITEMS READ FROM THE
! INDEX ARE IN ARRAYS OF DIMENSION 3, AS ARE THE CORRESPONDING BLOCK
! NUMBERS AND CENTURY HOURS.
! THE OLDEST SLOT WILL ALWAYS BE REUSED TO READ AN INDEX FOR A PERIOD
! NOT IN CORE, SO USUALLY THE LATEST TWO 6-HOUR PERIODS AND ONE OTHER
! WILL BE IN CORE.

INTEGER                           :: TIMTAG(3)
INTEGER                           :: NTRIES(3)
INTEGER                           :: NREPS(3)                       ! INDEX HEADERS
INTEGER                           :: NOFLOW(MAXOFL,3)               ! OVERFLOW POINTERS
INTEGER                           :: NINDEX(3)                      ! BLOCK NUMBERS
INTEGER                           :: IXHOUR(3)                      ! CENTURY-HOURS
INTEGER                           :: IDSK(5)                        ! ARRAY FOR MAPRD (IFT ETC)
INTEGER                           :: NEWVALS                        ! number of good values in new report
INTEGER                           :: OLDVALS                        ! number of good values in old report
LOGICAL                           :: NEWUS                          ! true if new report from a US centre
LOGICAL                           :: OLDUS                          ! true if old report from a US centre
LOGICAL                           :: FIRST = .TRUE.                 ! true if first call to subroutine

CHARACTER(LEN=MAXBLK)             :: MAP
CHARACTER(LEN=MAXBLK)             :: BLOCK
CHARACTER(LEN=MAXBLK)            :: BLOCKX
CHARACTER(LEN=4)                 :: BLOKID(1000)
CHARACTER(LEN=4)                 :: ALLNIL
CHARACTER(LEN=4)                 :: ALLONE

COMMON /SHPIX/ IDSK, IXHOUR,NTRIES, INDEKS
COMMON /SHPDC/ MAP, BLOCK, BLOCKX, RECDIS
EQUIVALENCE (BLOKID,MAP(MAXBLK-4004:))

! USE BLOCKX AS A WORK AREA FOR SORTS, EQUIVALENCING THE ARRAYS NEEDED

CHARACTER(LEN=1)                 :: MAPX(MAXBLK)
CHARACTER(LEN=6)                 :: SORTX(MAXBLK/6)
EQUIVALENCE (BLOCKX,MAPX)
EQUIVALENCE (BLOCKX,SORTX)

SAVE

IF (FIRST) THEN
  ALLNIL = CHAR(0) // CHAR(0) // CHAR(0) // CHAR(0)
  ALLONE = CHAR(255) // CHAR(255) // CHAR(255) // CHAR(255)
  FIRST = .FALSE.
END IF

! FIND LENGTH OF IDENT FOR USE IN CONSISTENCY CHECK LATER

LIDENT=LEN(IDENT)
DO WHILE (IDENT(LIDENT:LIDENT) == ' ')
  LIDENT=LIDENT-1
END DO

! IF CALL SIGN IS 'SHIP', COPY LAT/LONG FROM INDEX ENTRY AFTER IT, SO
! SHIPS IN DIFFERENT PLACES ARE NOT CHAINED & ARE RETRIEVABLE BY AREA.

IF (IDENT == 'SHIP     ') IDENT(5:8)=ENTRY(13:16)

! GET 4-CHARACTER ID FOR WHEN CHECKING/SETTING ID RANGES
IDENT4 = IDADJ(IDENT,4)

!**********************************************************************
!
! IF FIRST TIME, READ IN MAP BLOCK (TO GET NUMBER OF INDEX BLOCKS)
!
! MAP BLOCK:             (THE BYTE FOR EACH BLOCK IS SET TO ITS INDEX
!                        BLOCK NUMBER - SO LESS THAN 256*XHOURS DATA!)
! ------------------------------------------------------------ - - - -
! : NO. OF : NO. OF : HOURS : START OF : FIRST :  2ND  :     : 1ST   :
! : BLOCKS : INDEX  : PER   : 1ST SLOT : INDEX : INDEX :     : DATA  :
! : IN D/S : BLOCKS : BLOCK : AFTER 0Z : BLOCK : BLOCK :     : BLOCK :
! ------------------------------------------------------------ - - - -
! 0        2        4       6          8       9      10    8+XBLOKS
!
! SET TIME OF LATEST DATA STORED TO CLOCK TIME AT START OF RUN; IT WILL
! BE RESET TO THE LATEST CENTURY-HOUR STORED WHENEVER THAT IS GREATER.
!
!**********************************************************************
CALL DATIM(NOW)
WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
HOURNOW=(CENDAY-1)*24+NOW(5)           ! CURRENT CENTURY-HOUR
                                       ! FIRST TIME READ MAP
LatestCheck: &
IF (LATEST == 0) THEN
  CALL EB2ASC(4,BUFR)   ! CONVERT 'BUFR' TO ASCII FIRST TIME ONLY
  REC = 1
  CALL METDB_CREAD_DIR(IFT,MAP,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (861)

  BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))
  XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))
  XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))
  AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))

  NB=ICHAR(MAP(BLKSIZ-4006:BLKSIZ-4006))*256 &
    +ICHAR(MAP(BLKSIZ-4005:BLKSIZ-4005))
  OFLOWS=ICHAR(MAP(BLKSIZ-3:BLKSIZ-3))*256 &
        +ICHAR(MAP(BLKSIZ-2:BLKSIZ-2))
  INDENT=ICHAR(MAP(BLKSIZ-1:BLKSIZ-1))*256 &
        +ICHAR(MAP(BLKSIZ:BLKSIZ))

! SET NSQ TO CALCULATE BLOCK NUMBERS ALLOWING FOR SEQUENCE RECORD.

  NSEQBL=ICHAR(MAP(8+BLOCKS:8+BLOCKS))
  IF(NSEQBL > 0)THEN
    NSQ=1
  ELSE
    NSQ=0
  END IF

! SET UP AN ARRAY OF DATA SET PARAMETERS AS IN RETRIEVAL FOR USE BY
! MAPRD IN SEQUENCE CHECKS.

  IDSK(2)=BLKSIZ                       ! BLOCKSIZE
  IDSK(3)=IFT                          ! FT NUMBER
  IDSK(5)=1                            ! DIRECT ACCESS

  NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
  IF (LATEST == 0) LATEST=HOURNOW      ! LATEST CENTURY-HOUR
END IF LatestCheck
!**********************************************************************
!
! COMPLETE TIME FIELDS IN INDEX ENTRY (TIME & TIME OF RECEIPT),
! FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.
! TIME OF RECEIPT IS IN MINUTES FROM THE SLOT TIME (HOURNOW-SLOTHR,
! WHERE HOURNOW IS THE CURRENT CENTURY-HOUR) AND CAN BE <0 OR >1440.
!
!**********************************************************************
INDHOR=MOD(DATIME(4)+24-AMSTAR,XHOURS) ! HOUR MINUS SLOT START
IF (MOD(ICHAR(ENTRY(7:7)),16) == 0) THEN
  ENTRY(1:1)=CHAR(INDHOR)              ! HOUR RELATIVE TO SLOT
ELSE                                   ! IF COR NUMBER NONZERO,
  ENTRY(1:1)=CHAR(INDHOR+128)          ! SET COR IN SAME BYTE.
END IF
ENTRY(2:2)=CHAR(DATIME(5))             ! MINUTES

IF (DATIME(1) < 1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))

CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
CENTHR=(CENDAY-1)*24+DATIME(4)         ! CENTURY-HOUR OF DATA
SLOTHR=CENTHR-INDHOR                   ! SLOT START (CENTURY-HOUR)

ITOR=HOURNOW-SLOTHR                    ! TOR HOUR RELATIVE TO SLOT
CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! DAY OF MONTH FOR SLOT
SLOTHR=MOD(SLOTHR,24)                  ! CENT-HOUR TO HOUR OF DAY
CURENT=SLODAY*256+SLOTHR               ! TAG FOR OB TO BE STORED

ITOR=ITOR*60+NOW(4)                    ! CONVERT TOR TO MINUTES
IF (ITOR < 0) ITOR=65536+ITOR          ! TWOS COMPLEMENT
ENTRY(18:18)=CHAR(ITOR/256)            ! PUT HALFWORD TOR IN ENTRY
ENTRY(19:19)=CHAR(MOD(ITOR,256))       ! PUT HALFWORD TOR IN ENTRY

! IF THE DATA IS FOR TOO LONG BEFORE THE THE MOST RECENT DATA STORED
! ('TOO LONG' MEANS MORE THAN THE PERIOD COVERED BY THE DATA BASE),
! THEN REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH OLD DATA!).
! IF THE NEW DATA IS MORE RECENT, UPDATE THE LATEST HOUR - AND THE
! INDEX BLOCK TAG FOR WHICH THE LIST OF RANGES CAN BE UPDATED (SET
! WHEN A NEW INDEX BLOCK IS STARTED, BUT THIS COPES UNTIL THEN...).
!
!  (XBLOKS-1 RATHER THAN XBLOKS BECAUSE ALTHOUGH WE CAN STORE DATA
! EXTENDING OVER A PERIOD OF XBLOKS*XHOURS IF THAT PERIOD STARTS AT
! THE START OF AN INDEX BLOCK, IN GENERAL SUCH A PERIOD WOULD REUSE
! THE LATEST INDEX BLOCK FOR THE OLDEST DATA.)

IF (CENTHR <= LATEST-(XBLOKS-1)*XHOURS) THEN
  PRINT *,MSGHDR,LATEST-CENTHR,'HOURS OLD   ',BULL(:60)
  RETURN
END IF

IF (CENTHR > LATEST) THEN
  LATEST=CENTHR
  LISTAG=CURENT                        ! LATEST FOR RANGE UPDATE
END IF
!**********************************************************************
!
! THE INDEX IS DIVIDED INTO N-HOURLY SEGMENTS. WORK OUT WHICH SEGMENT
! FROM THE CENTURY-HOUR AND READ IN THE CORRESPONDING INDEX BLOCK.
!
! INDEX BLOCK:
! ------------------------------------------------- - - - - - ---------
! : DATE/ : NO. OF : NO. OF  : 23-BYTE : 23-BYTE :             : OVER :
! : TIME  : ENTRIES: REPORTS :  ENTRY  :  ENTRY  : ENTRIES...  : FLOW :
! ------------------------------------------------- - - - - - ---------
! 0       2        4         6        29        52         LAST 2 BYTES
!
!**********************************************************************
NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2  ! INDEX BLOCK NUMBER

! SEE IF THE INDEX IS IN ONE OF THE BUFFERS IN CORE.

LX=0
DO I=1,3
  IF (NXBLOK == NINDEX(I)) LX=I
END DO

! IF NOT, REUSE BUFFER WITH OLDEST INDEX  (FOLLOWING CODE MUST SET LX!)
!                                                         ====
IndexCheck: &
IF (LX == 0) THEN
  DO I=1,3
    IF (IXHOUR(I) == MIN(IXHOUR(1),IXHOUR(2),IXHOUR(3))) LX=I
  END DO

  IXHOUR(LX)=CENTHR-INDHOR             ! KEEP INDEX CENTURY-HOUR
  NINDEX(LX)=NXBLOK                    ! KEEP INDEX BLOCK NUMBER

  IX=NINDEX(LX)                        ! TO VARY IN READ LOOP
  NIND=1                               ! CONTINUATION NUMBER
 30 CONTINUE
  NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN

  REC = IX+NSQ
  CALL METDB_CREAD_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (862)
  INDHDR = BUFFER(1:6)
  IPOS = 7
  DO I=NIBL+1,NIBL+NBLIND
    INDEKS(I,LX) = BUFFER(IPOS:IPOS+22)
    IPOS = IPOS+23
  END DO
  INDOVR = BUFFER(IPOS:IPOS+1)

! Get NTRIES & NREPS from the first index block rather than the
! last if there is index overflow: the first is always updated,
! the last not, so taking NREPS from the last risks getting a low
! value (not equal to the sum of the counts for the index entries)
! which spoils the distribution for a new index period.

  IF (NIND == 1) THEN
    TIMTAG(LX)=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
    NTRIES(LX)=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
    NREPS(LX)=ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))
  END IF
  NOFLOW(NIND,LX)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

  IF (NOFLOW(NIND,LX) > 0) THEN        ! INDEX OVERFLOW?
    IX=NOFLOW(NIND,LX)                 ! IF SO, RESET BLOCK NO
    NIND=NIND+1                        ! FURTHER CONTINUATION
    IF(NIND > MAXOFL)THEN
      PRINT*,MSGHDR,'MAX INDEX OVERFLOW REACHED - SYNOPS'
      RETURN
    END IF
    IF (TIMTAG(LX) == CURENT) GO TO 30 ! READ ON IF RIGHT TIME
  END IF
END IF IndexCheck
!**********************************************************************
! THE STRUCTURE OF THE FOLLOWING CODE IS:                             *
!                                                                     *
! 100 IF (NO CURRENT INDEX BLOCK EXISTS) THEN                         *
!       RELEASE OLD DATA BLOCKS & SET UP NEW ALLOCATION TABLE:        *
!         110 RELEASE DATA & INDEX BLOCKS BY ZEROING MAP BYTES.       *
!         120 DECIDE WHICH PAST INDEX BLOCK TO USE FOR DISTRIBUTION.  *
!         130 READ IT IN (ALLOWING FOR OVERFLOW).                     *
!             SORT INDEX ENTRIES INTO IDENTIFIER ORDER.               *
!         140 BREAK THIS LIST INTO IDENTIFIER RANGES WITH ROUGHLY     *
!             EQUAL NUMBERS OF REPORTS.                               *
!     END IF                                                          *
!                                                                     *
! 200 IF (STATION IS INDEXED) THEN                                    *
!         210 ALLOCATE SAME DATA BLOCK & CHAIN POINTERS;              *
!             KEEP NUMBER OF INDEX ENTRY FOR USE LATER.               *
!     ELSE                                                            *
!         220 FIND WHICH RANGE THE IDENTIFIER IS IN                   *
!         230 & FIND THE CORRESPONDING BLOCK NUMBER FROM THE MAP;     *
!             SET THE POINTER TO ZERO.                                *
!     END IF                                                          *
!                                                                     *
! 310 READ IN THE DATA BLOCK                                          *
!                                                                     *
! 320 IF THERE'S AN INDEX ENTRY, CHECK FOR DUPLICATE: RETURN IF SAME. *
!     IF (THERE IS ROOM IN THE DATA BLOCK) THEN                       *
!       PUT REPORT IN DATA BLOCK, INSERTING IT IN THE CHAIN IN        *
!       TIME ORDER, AND ADD OR UPDATE INDEX ENTRY.                    *
!       IF THE EXTRA ENTRY MEANS ANOTHER INDEX BLOCK, CLAIM AN        *
!        OVERFLOW BLOCK FOR WHEN THE INDEX IS WRITTEN OUT.            *
!     ELSE                                                            *
! 400   SPLIT THE DATA BLOCK AS FOLLOWS:                              *
!         410 LIST NUMBER OF REPORTS FOR EACH IDENTIFIER IN BLOCK.    *
!         420 SORT INTO IDENTIFIER ORDER & SPLITTING THE LIST TO      *
!              GIVE ROUGHLY EQUAL NUMBERS IN BOTH BLOCKS.             *
!         430 MOVE OBS IN THE 2ND HALF OF THE LIST TO THE NEW BLOCK,  *
!              FOLLOWING CHAINS; ZERO THESE LENGTHS IN THE OLD BLOCK. *
!         440 WRITE OUT NEW BLOCK & INDEX WITH POINTERS TO IT.        *
!         450 COMPRESS OLD BLOCK BY MOVING REMAINING REPORTS TO END,  *
!              BUT LEAVING ZERO LENGTHS SO THAT NO CHANGE TO POINTERS.*
!         460 WRITE BACK OLD BLOCK & UPDATE LIST OF IDENTIFIER RANGES *
!         470  MADE AT 140 TO REFLECT THE SPLIT.                      *
!       GO BACK TO 200 TO STORE REPORT                                *
!     END IF                                                          *
! 500 WRITE OUT INDEX & DATA BLOCKS & MAP BLOCK IF UPDATED            *
!                                                                     *
!**********************************************************************
!
! THE TIME TAG (DATE/TIME IN INDEX BLOCK) IS (DAY OF MONTH)*256+HOUR.
! THE TIME TAG IS THE SLOT THIS SEGMENT WAS LAST USED FOR.  IF IT WAS
! LAST USED FOR THE OLDEST DATA IN THE BANK, FREE THE BLOCKS ATTACHED
! TO IT BEFORE STORING NEW DATA.
!
! NON-INDEX BLOCK:    (LENGTHS OF RECORDS AT START, DATA ITSELF AT END)
! ---------------------------------- - - - - - -----------------------
! :TIME: NUM OF : LENGTH : L1 : L2 :     FREE       : SECOND : FIRST  :
! :TAG : RECORDS:  FREE  :    :    :     SPACE      : RECORD : RECORD :
! ---------------------------------- - - - - - -----------------------
! 0    2        4        6    8   10            END-L1-L2  END-L1   END

! 100 in code structure notes
TimeLagCheck: &
IF (TIMTAG(LX) /= CURENT) THEN
  NBLOK=1
 110 CONTINUE
  N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(NINDEX(LX)))
  IF (N > 0) THEN
    NBLOK=NBLOK+(N-1)
    MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
    IF (1+XBLOKS+NBLOK < BLOCKS) GO TO 110
  END IF
!                      NOW DO SAME FOR INDEX OVERFLOWS (TOP BIT SET)
  NBLOK=1
! 111 in code structure notes
 111 CONTINUE
  N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(128+NINDEX(LX)))
  IF (N > 0) THEN
    NBLOK=NBLOK+(N-1)
    MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
    IF (1+XBLOKS+NBLOK < BLOCKS) GO TO 111
  END IF

! CHOOSE A PREVIOUS INDEX BLOCK TO ALLOCATE STATIONS TO DATA BLOCKS,
! AVOIDING LOW TOTALS & VERY HIGH ONES BY TAKING A BLOCK WITH A TOTAL
! BETWEEN THE MEDIAN & THE UPPER QUARTILE, EITHER THAT FOR 24 HOURS
! AGO OR THE MOST RECENT IN THE RANGE.

  DO I=1,XBLOKS
    MAPX(I)=MAP(8+I:8+I)               ! TO WORK AREA FOR SORT
  END DO

  CALL SORTCH(MAPX,1,XBLOKS,MASK(3:3))
  MEDIAN=MAPX(XBLOKS/2)                ! MIDDLE COUNT AFTER SORT
  QUARTL=MAPX(3*XBLOKS/4)
!
!       IF (MEDIAN == CHAR(0)) THEN          ! USE MAX IF ZERO MEDIAN
  IF (ICHAR(MAPX(XBLOKS)) >= 2*ICHAR(MEDIAN)) THEN
    LASTIX=1+INDEX(MAP(8+1:8+XBLOKS),MAPX(XBLOKS))
  ELSE
    I=0                                ! BLOCKS TO GO BACK
    J=NINDEX(LX)-24/XHOURS             ! FIRST TRY 24 HOURS BACK,
! 120 in code structure notes
 120 CONTINUE
    IF (J <= 1) J=XBLOKS+J             ! WRAPPING ROUND IF NEEDED
    IF (J <= 1 .OR. &                  ! IF TOO FEW INDEX BLOCKS
        MAP(J+7:J+7) < MEDIAN .OR. &   ! OR COUNT TOO SMALL
        MAP(J+7:J+7) > QUARTL) THEN    ! OR COUNT TOO BIG,
      I=I+1                            ! GO BACK A(NOTHER) BLOCK
      J=NINDEX(LX)-I                   ! FROM CURRENT INDEX BLOCK
      IF (I <= XBLOKS) GO TO 120       ! IF ANY NOT YET TRIED...
    END IF                             ! N.B. J=1:N, NINDEX=2:N+1
    LASTIX=J                           ! INDEX BLOCK TO USE
  END IF

! READ IN THE INDEX BLOCK(S) FOR THE PERIOD CHOSEN ABOVE FOR ITS COUNT.

  IX=LASTIX                            ! TO VARY IN READ LOOP
  NIND=1                               ! CONTINUATION NUMBER
! 130 in code structure notes
 130 CONTINUE
  NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN

  REC = IX+NSQ
  CALL METDB_CREAD_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (863)
  INDHDR = BUFFER(1:6)
  IPOS = 7
  DO I=NIBL+1,NIBL+NBLIND
    INDEKS(I,LX) = BUFFER(IPOS:IPOS+22)
    IPOS = IPOS+23
  END DO
  INDOVR = BUFFER(IPOS:IPOS+1)

  TIMTAG(LX)=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
  NTRIES(LX)=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
  NREPS(LX)=ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))
  NOFLOW(NIND,LX)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

  IF (NOFLOW(NIND,LX) > 0) THEN        ! INDEX OVERFLOW?
    IX=NOFLOW(NIND,LX)                 ! IF SO, RESET BLOCK NO
    NIND=NIND+1                        ! FURTHER CONTINUATION
    IF(NIND > MAXOFL)THEN
      PRINT*,MSGHDR,'MAX INDEX OVERFLOW REACHED - SYNOPS'
      RETURN
     END IF
    GO TO 130                          ! & READ MORE ENTRIES.
  END IF

! BREAK THE LIST OF INDEX ENTRIES INTO NB PARTS WITH ROUGHLY EQUAL
! NUMBERS OF REPORTS (LOB=NREPS/NB), WHERE NB IS THE NUMBER OF DATA
! BLOCKS (NOT COUNTING OVERFLOWS); KEEP THE IDENTIFIER RANGES.
!
! IF THERE ARE NO ENTRIES IN THE PREVIOUS INDEX BLOCK, I.E. WE ARE
! STARTING UP, ASSIGN ONLY ONE BLOCK AND THEN SPLIT WHEN NECESSARY;
! THE NEXT INDEX BLOCK WILL BE ABLE TO USE THIS DISTRIBUTION.
!
! N.B. BLOKID IS A LIST OF PAIRS OF IDENTIFIERS DEFINING RANGES RATHER
! THAN SINGLE IDENTIFIERS AS BREAK POINTS. THIS MAKES IT EASIER TO UP-
! DATE: IF A RANGE IS SPLIT THE SECOND HALF CAN JUST BE PUT AT THE END.

  NB=(BLOCKS-1-NSQ-XBLOKS-OFLOWS)/XBLOKS
  IF (NTRIES(LX) == 0) NB=1
  MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
  MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
  BLOKID(1)=ALLNIL            ! START OF FIRST RANGE (MIN POSS)
  BLOKID(NB*2)=ALLONE         ! END OF LAST RANGE    (MAX POSS)

  IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))  ! GET FIRST
  MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))     ! DATA BLOCK

NumBlockCheck: &
  IF (NB > 1) THEN                     ! SORT INTO COUNT ORDER
    CALL SORTCH(INDEKS(1,LX),INDENT,NTRIES(LX),MASX)

! THE SORT PUTS ENTRIES WITH BIG COUNTS AT THE END OF THE LIST.
! ANY IDENT WITH MORE THAN NREPS/NB WILL BE ASSIGNED A WHOLE BLOCK.
! THE REST WILL BE RE-SORTED IN IDENT ORDER & SHARED OUT BETWEEN
! THE REMAINING BLOCKS.

    NL=NTRIES(LX)                      ! NUMBER OF ENTRIES
    LOB=NREPS(LX)/NB                   ! MEAN OBS PER BLOCK
! 135 in code structure notes
 135 CONTINUE
    NREP=ICHAR(INDEKS(NL,LX)(12:12))   ! OBS FOR LAST IDENT
    IF (NREP > LOB) THEN               ! IF COUNT > MEAN,
      NL=NL-1                          ! LEAVE ENTRY AT END
      NREPS(LX)=NREPS(LX)-NREP         ! & IGNORE ITS COUNT.
      IF (NL > 1) GO TO 135            ! BACK PAST BIG COUNTS
    END IF
!                                            ! SORT SMALLER COUNTS
    CALL SORTCH(INDEKS(1,LX),INDENT,NL,MASK)      ! BY IDENT
!                                            ! & RECALCULATE MEAN
    LOB=NREPS(LX)/(NB-(NTRIES(LX)-NL)) ! IGNORING BIG COUNTS.
    K=1
    NOB=0
! 140 in code structure notes
    DO I=1,NTRIES(LX)
     NOB=NOB+ICHAR(INDEKS(I,LX)(12:12))
     IF (NOB >= LOB*K .AND. K < NB) THEN
! use 4-char adjusted id if appropriate
       BLOKID(K*2)=IDADJ(INDEKS(I,LX)(3:11),4)   ! END OF ONE RANGE
       BLOKID(K*2+1)=IDADJ(INDEKS(I,LX)(3:11),4) ! START OF NEXT -
       K=K+1                           ! ONE MORE RANGE DONE
       IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))
       MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))
     END IF
    END DO
  END IF NumBlockCheck

! FINALLY REINITIALISE THE INDEX BUFFER FOR THE NEW HOUR.
! KEEP THE TIME TAG FOR THIS LIST IN CASE A BLOCK IS SPLIT: THE RANGES
! WILL ONLY BE UPDATED FOR DATA WITH THIS TAG, FOR THE LATEST PERIOD.

  TIMTAG(LX)=CURENT
  NTRIES(LX)=0
  NREPS(LX)=0
  NOFLOW(1,LX)=0

  LISTAG=CURENT
END IF TimeLagCheck
!**********************************************************************
!
! IF THE INDEX IS NOT EMPTY (IF DATA ALREADY STORED FOR THIS HOUR), SEE
! IF THERE IS ALREADY AN ENTRY FOR THIS STATION.  IF SO, TRY TO STORE
! THE REPORT IN THE SAME DATA BLOCK.  MAKE THE INDEX ENTRY POINT TO THE
! NEW REPORT, AND SET THE POINTER ON THE END OF THIS REPORT TO POINT TO
! THE PREVIOUS REPORT, I.E. COPY THE OLD POINTER FROM THE INDEX.  ADD 1
! TO THE NUMBER OF REPORTS CHAINED TO THIS INDEX ENTRY.
!
!**********************************************************************

! BRANCH BACK TO 200 TO STORE OB IF BLOCK WAS FULL & HAD TO BE SPLIT

! 200 in code structure notes
 200 CONTINUE
IND=0
! 210 in code structure notes
DO I=1,NTRIES(LX)
  IF (INDEKS(I,LX)(3:11) == IDENT) THEN
    NBLOC=ICHAR(INDEKS(I,LX)(22:22))*256+ICHAR(INDEKS(I,LX)(23:23))
    POINTER=INDEKS(I,LX)(20:23)
    IND=I
  END IF
END DO

! IF NOT, ALLOCATE A BLOCK NUMBER FROM THE LIST, USING THE IDENTIFIER.
! FIRST FIND WHICH OF THE CURRENT NB RANGES THE IDENTIFIER IS IN. THIS
! USES THE (POSSIBLY ADJUSTED) 4-CHARACTER ID.
! (THE RANGE INCLUDES THE ENDPOINT BUT NOT THE STARTING POINT)
IndCheck: &
IF (IND == 0) THEN
  J = 1
! 220 in code structure notes
  DO WHILE ((J <= NB) .AND.  &
       .NOT.(IDENT4 > BLOKID(J*2-1) .AND. &
       IDENT4 <= BLOKID(J*2)))
    J = J + 1
  END DO

! THEN GET THE CORRESPONDING DATA BLOCK NUMBER FOR THIS INDEX BLOCK
! FROM THE MAP (WE CAN STORE REPORTS FOR LOTS OF INDEX BLOCKS AT THE
! SAME TIME, BUT THE LIST ONLY APPLIES STRICTLY TO THE LATEST.
! HOWEVER, IF EARLIER TIMES ALREADY HAVE MOST STATIONS INDEXED, THIS
! SHOULDN'T LEAVE THE SPREAD OF STATIONS OVER DATA BLOCKS TOO UNTIDY.)
! IF THERE ARE MORE RANGES THAN DATA BLOCKS ALLOCATED TO THIS INDEX
! BLOCK, THEN USE THE LAST DATA BLOCK FOUND (& SPLIT IT WHEN FULL).

  IX=1
! 230 in code structure notes
  DO I=1,J
    IF (IX > BLOCKS-1-NSQ-XBLOKS) EXIT
    IY=INDEX(MAP(8+XBLOKS+IX:8+BLOCKS-1-NSQ),CHAR(NINDEX(LX)))
    IF (IY==0) EXIT
    IX=IX+IY
  END DO
  NBLOC=IX-1

! AS THERE IS ONLY ONE REPORT FOR THIS STATION, ZERO THE POINTER ON
! THE END OF THE REPORT.  (A POINTER IS RECORD/BLOCK, AS IN THE INDEX.)

  POINTER(1:1)=CHAR(0)
  POINTER(2:2)=CHAR(0)
  POINTER(3:3)=CHAR(0)
  POINTER(4:4)=CHAR(0)
END IF IndCheck
!**********************************************************************
!
! IF THERE'S ROOM IN THE BLOCK, STORE THE REPORT (AFTER CHECKING FOR
! DUPLICATES) & INDEX IT. INDEX DETAILS ARE FOR THE LATEST REPORT,
! THOSE BETWEEN REPORT & POINTER FOR THE REPORT THEY FOLLOW.
! 5
!**********************************************************************

! FIRST READ IN THE DATA BLOCK.  (IT MAY HAVE BEEN ALLOCATED WHEN THE
! INDEX BLOCK WAS STARTED & STILL BE EMPTY - CAN'T TELL WITHOUT READ)
! 300 in code structure notes
IndexBlockCheck: &
  IF (NBLOCK /= NBLOC) THEN
    NBLOCK=NBLOC
    REC = 1+XBLOKS+NBLOCK+NSQ
    CALL METDB_CREAD_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (864)

    BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
    NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
    LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))

! IF THE BLOCK HAS NO DATA FOR THIS INDEX PERIOD,
! SET TAG, NINBLK & LEFT IN BLOCK HEADER & CLEAR REST OF BLOCK
! - UNLESS AN INDEX ENTRY POINTS TO THIS BLOCK, IN WHICH CASE
! THE DATA BASE HAS BEEN CORRUPTED.
DataConsistentCheck: &
    IF (NINBLK == 0 .OR. BLKTAG /= CURENT) THEN
      IF (IND > 0) THEN
        PRINT *,MSGHDR,'+++++++ DATA BASE INCONSISTENT! +++++++'
        PRINT *,NBLOCK,'TH DATA BLOCK POINTED TO BY INDEX ENTRY'
        PRINT *,CURENT,'IS DATA TIME TAG',BLKTAG,'IS BLOCK TAG'
        RETURN
      END IF

      NINBLK=0
      BLKTAG=CURENT
      LEFT=BLKSIZ-INDHED
      BLOCK(1:1)=CHAR(BLKTAG/256)
      BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
      BLOCK(7:BLKSIZ)=' '
    ELSE

! WORK OUT THE DISPLACEMENTS CORRESPONDING TO THE RECORD LENGTHS:
! (RECDIS(I) IS THE START OF THE I-TH REPORT FROM THE END OF THE BLOCK)

      RECDIS(1)=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))
! 310 in code structure notes
      DO I=2,NINBLK
        RECDIS(I)=RECDIS(I-1)+ICHAR(BLOCK(6+I*2-1:6+I*2-1))*256 &
                             +ICHAR(BLOCK(6+I*2:6+I*2))
      END DO
    END IF DataConsistentCheck
  END IF IndexBlockCheck

! IF THERE IS AN INDEX ENTRY (IN WHICH CASE IND IS NONZERO),
! CHECK FOR DUPLICATES, FOLLOWING CHAIN AS LONG AS POINTER (RECORD
! NUMBER - ALL IN SAME BLOCK) IS NONZERO. COMPARE THE REPORT TEXTS
! AND ALSO THE HOURS/MINUTES THAT FOLLOW (INCLUDING THE COR FLAG).
! (N.B. THERE WON'T BE EQUALITY UNLESS THE LENGTHS ARE EQUAL.)
!
! FIRST SEE IF THE REPORT IS OUT OF ORDER (EARLIER THAN THE LATEST
! REPORT INDEXED).  IF SO, IT WILL BE INSERTED IN THE CHAIN IN TIME
! ORDER: NOTE THE RECORD NUMBERS OF THE REPORTS BEFORE & AFTER IT
! IN TIME DURING THE DUPLICATE SEARCH & LATER SET THE POINTERS
! ACCORDINGLY.  (N.B. THE NEW REPORT MAY BE THE EARLIEST IN THE
! CHAIN, IN WHICH CASE ONLY ONE RECORD NUMBER WILL BE SET.)
! THE HOUR AND MINUTE FIELDS IN INDEX ENTRIES MUST BE COMPARED
! SEPARATELY, BECAUSE THE HOUR MAY HAVE FLAGS SET IN THE SAME BYTE.

IndCheck2: &
IF (IND > 0) THEN
  LASTHR=MOD(ICHAR(INDEKS(IND,LX)(1:1)),32)
  IF (LASTHR > INDHOR .OR. &
    (LASTHR == INDHOR.AND.INDEKS(IND,LX)(2:2) > ENTRY(2:2))) THEN
    INSERT=.TRUE.
  ELSE
    INSERT=.FALSE.
  END IF

! NAFTER MAY BE SET IF THIS IS NOT THE LATEST REPORT IN THE
! CHAIN, NPREF IF THERE IS ALREADY A PREFERRED REPORT FOR THIS TIME.
! USE THE COUNT IN THE INDEX ENTRY TO CONTROL THE LOOP ROUND THE CHAIN
! RATHER THAN RELYING ON RECAHING A ZERO POINTER!

  NAFTER=0
  NPREF=0
  NCHAIN=ICHAR(INDEKS(IND,LX)(12:12))

! LOOP BACK TO HERE TO LOOK AT NEXT REPORT IN CHAIN

! 320 in code structure notes
 320 CONTINUE
  POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
  DISPL=BLKSIZ-RECDIS(POINTR)

! IF AN IDENT IN THE CHAIN IS NOT AS IN THE INDEX, SOMETHING'S WRONG!
! (Check that the ident in the index is somewhere in the report:
!  60 is an arbitrary number.  Previous code blanked out index
!  ident but then didn't write index back, so achieved nothing.
!  And issued message when report correctly indexed but string
!  started with day/hour from BBXX line - very misleading!)

  IF(INDEX(BLOCK(DISPL+1:DISPL+60),IDENT(1:LIDENT)) == 0)THEN
    PRINT *,MSGHDR,' >>>>>>> DATA BASE INCONSISTENT! <<<<<<<<'
    PRINT *,'INDEX ENTRY FOR ',IDENT,'POINTED TO ', &
            BLOCK(DISPL+1:DISPL+60)
    RETURN
  END IF

  IF (POINTR == 1) THEN
    L=RECDIS(1)
  ELSE
    L=RECDIS(POINTR)-RECDIS(POINTR-1)
  END IF
  RECNO=POINTR

  LN=INDEX(BULL,BUFR)-1      ! LENGTH OF REPORT IN CHARACTERS
  IF (LN <= 0) LN=LEN(BULL)  ! (TOTAL LENGTH IF NO BUFR MESSAGE)
  LENBUF=LEN(BULL)-LN        ! LENGTH OF BUFR MESSAGE (MAY BE 0)

! IF SAME TIME (FIRST TWO BYTES OF INDEX ENTRY), DO DUPLICATE CHECK:
! IF STRING OF SAME LENGTH, AND STRING ITSELF IS SAME, THEN DUPLICATE

  LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
  IF (LASTHR == INDHOR .AND. &
     (BLOCK(DISPL+L-21:DISPL+L-21) == ENTRY(2:2))) THEN
    IF (LN == L-LENBUF-23) THEN
      IF (BULL(1:LN) == BLOCK(DISPL+1:DISPL+LN)) RETURN
    END IF

! SEE IF PREFERRED FLAG IS SET (RIGHT-HAND BIT OF BYTE 17 IN TRAILER)
! AND IF SO KEEP THE RECORD NUMBER SO THAT THE COUNTS CAN BE COMPARED
! AND THE NEW REPORT PREFERRED IF IT IS BETTER.

    IF (ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)) >= 128) THEN
      NPREF=RECNO
    END IF
  END IF

  POINTER=BLOCK(DISPL+L-3:DISPL+L)

! IF INSERT HAS BEEN SET, GO BACK PAST AT LEAST ONE REPORT (THAT POINTED
! TO BY THE INDEX ENTRY WHICH SET INSERT) UNTIL A REPORT FOR AN EARLIER
! TIME (OR THE SAME TIME) IS FOUND.  KEEP THE LAST RECORD NUMBER PASSED.

  IF (INSERT) THEN
    LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
    IF (LASTHR > INDHOR .OR. (LASTHR == INDHOR .AND. &
        BLOCK(DISPL+L-21:DISPL+L-21) > ENTRY(2:2))) THEN
      NAFTER=RECNO
    ELSE
      INSERT=.FALSE.
    END IF
  END IF

! LOOP ROUND UNLESS REPORTS ARE CHAINED ACROSS BLOCKS
! (ONLY 'DREG' CHAINS UNDER A NULL IDENTIFIER ARE LIKELY TO BE LONG
! ENOUGH TO CROSS BLOCKS.)  AT END, RESET POINTER TO START OF CHAIN.

  NCHAIN=NCHAIN-1
  IF (POINTER(3:4) == INDEKS(IND,LX)(22:23) &
      .AND. NCHAIN > 0) GO TO 320
  POINTER=INDEKS(IND,LX)(20:23)
END IF IndCheck2

! IF A PREFERRED REPORT FOR THE SAME TIME HAS BEEN FOUND, COMPARE THE
! NUMBER OF GOOD VALUES AND THE COR NUMBER AND RESET THE PREFERRED
! FLAG IF THE NEW REPORT IS BETTER.
! SET THE "HISTORICAL" PREFERRED FLAG (X'20', SO ADD 32), NEVER TO BE
! UNSET, TO SHOW THAT THE NEW REPORT HAS BEEN (OR STILL IS) PREFERRED.
IndNprefCheck: &
IF (IND > 0 .AND. NPREF > 0) THEN
  IF (NPREF == 1) THEN
    DISPL=BLKSIZ-INDLEN
  ELSE
    DISPL=BLKSIZ-RECDIS(NPREF-1)-INDLEN             ! --> TRAILER
  END IF

  NEWUS=ENTRY(8:8) == 'K' .OR. ENTRY(8:11) == 'EGWR'
  OLDUS=BLOCK(DISPL+8:DISPL+8) == 'K' .OR. &
        BLOCK(DISPL+8:DISPL+11) == 'EGWR'
  NEWVALS=ICHAR(ENTRY(12:12))
  OLDVALS=ICHAR(BLOCK(DISPL+12:DISPL+12))

! If the new report is a COR (or has a higher COR number)
! and either if has more good values
! or it has as many good values and is not a US redistibution of
!                                an ob originally from elsewhere,
! then prefer the new report.

  IF (BLOCK(DISPL+7:DISPL+7)< ENTRY(7:7).OR.&! COR FLAG
    NEWVALS > OLDVALS .OR. (NEWVALS == OLDVALS.AND.&
    .NOT. (NEWUS.AND..NOT.OLDUS))) THEN
    IFLAGS=ICHAR(BLOCK(DISPL+17:DISPL+17))          ! OLD FLAGS
    BLOCK(DISPL+17:DISPL+17)=CHAR(IFLAGS-128)       ! UNSET PREF
    IFLAGS=ICHAR(ENTRY(17:17))                      ! NEW FLAGS
    IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)   ! PREF
  END IF
ELSE                                                ! IF NO PREF,
  IFLAGS=ICHAR(ENTRY(17:17))                        ! PREFER THIS
  IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)
END IF IndNprefCheck
!**********************************************************************
!
! IF THERE'S ROOM IN THE BLOCK & THE REPORT'S NOT A DUPLICATE, STORE IT.
!
!**********************************************************************
RoomLeftInBlockCheck: &
IF (LEN(BULL)+INDENT+2 <= LEFT) THEN
  NINBLK=NINBLK+1
  L=LEN(BULL)+INDENT
  LEFT=LEFT-L-2
  BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)
  BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))

  IF (NINBLK > 1) THEN
    RECDIS(NINBLK)=RECDIS(NINBLK-1)+L
  ELSE
    RECDIS(NINBLK)=L
  END IF

  START=INDHED+NINBLK*2+LEFT
  BLOCK(START+1:START+LEN(BULL))=BULL

  BLOCK(START+LEN(BULL)+1:START+L-4)=ENTRY(1:INDENT-4)
  BLOCK(START+L-3:START+L)=POINTER
!**********************************************************************
!
! UPDATE THIS STATION'S INDEX ENTRY TO POINT TO THIS REPORT, IF IT IS
! THE LATEST.  IF NOT, NAFTER IS NONZERO.  MAKE THE
! NAFTER-TH REPORT POINT TO THE NEW ONE & THE NEW ONE TO
! ZERO (BY TAKING THE POINTER THAT WAS IN THE NAFTER-TH).
! (OR ADD AN ENTRY FOR THIS STATION IF THERE'S NONE IN THIS SLOT).
!
!**********************************************************************
  POINTER(1:1)=CHAR(NINBLK/256)
  POINTER(2:2)=CHAR(MOD(NINBLK,256))
  POINTER(3:3)=CHAR(NBLOCK/256)
  POINTER(4:4)=CHAR(MOD(NBLOCK,256))

! COUNT OF OBS IS ONLY ONE BYTE; LEAVE IT AS 255 IF MORE THAN 255!
IndCheck3: &
  IF (IND > 0) THEN
    IF (POINTER < BLOCK(START+L-3:START+L)) THEN
      NOTPTR=ICHAR(BLOCK(START+L-3:START+L-3))*256 &
            +ICHAR(BLOCK(START+L-2:START+L-2))
      PRINT *,MSGHDR,' ******* DATA BASE INCONSISTENT ********'
      PRINT *,NOTPTR,'IS RECORD NUMBER IN POINTER, BUT ONLY'
      PRINT *,NINBLK-1,'OBS IN BLOCK'
      INDEKS(IND,LX)(3:7)=' '    ! ????  RESET ENTRY & REUSE?
      RETURN                     ! ????
    END IF

    NOBS=ICHAR(INDEKS(IND,LX)(12:12))
    IF (NOBS < 255) INDEKS(IND,LX)(12:12)=CHAR(NOBS+1)

NafterCheck: &
    IF (NAFTER == 0) THEN
      INDEKS(IND,LX)(1:2)=ENTRY(1:2)
      INDEKS(IND,LX)(18:19)=ENTRY(18:19)
      INDEKS(IND,LX)(20:23)=POINTER
    ELSE
      DISPL=BLKSIZ-RECDIS(NAFTER)
      IF (NAFTER == 1) THEN
        LL=RECDIS(1)
      ELSE
        LL=RECDIS(NAFTER)-RECDIS(NAFTER-1)
      END IF
      BLOCK(START+L-3:START+L)=BLOCK(DISPL+LL-3:DISPL+LL)
      BLOCK(DISPL+LL-3:DISPL+LL)=POINTER
    END IF NafterCheck
  ELSE

! OR MAKE A NEW INDEX ENTRY (PUT IDENT & COUNT OF 1 IN THE INPUT ENTRY)

    NTRIES(LX)=NTRIES(LX)+1
    INDEKS(NTRIES(LX),LX)(1:2)=ENTRY(1:2)
    INDEKS(NTRIES(LX),LX)(3:11)=IDENT
    INDEKS(NTRIES(LX),LX)(12:12)=CHAR(1)
    INDEKS(NTRIES(LX),LX)(13:19)=ENTRY(13:19)
    INDEKS(NTRIES(LX),LX)(20:23)=POINTER

! IF THIS ENTRY NEEDS TO START A NEW INDEX BLOCK, IT'S ENOUGH HERE TO
! CLAIM A BLOCK NUMBER FROM THE OVERFLOW POOL AND LIST IT AS THE NEXT
! BLOCK TO CONTINUE THE INDEX; THIS NUMBER WILL BE SET AS A POINTER
! AT THE END OF THE PREVIOUS BLOCK WHEN THE INDEX IS WRITTEN BACK.

    NIND=NTRIES(LX)/NBLIND           ! NO OF FULL INDEX BLOCKS
NewIndexBlockRequiredCheck: &
    IF (NIND >= 1 .AND. NTRIES(LX)-NIND*NBLIND == 1) THEN
      NX=INDEX(MAP(8+BLOCKS-NSQ-OFLOWS:8+BLOCKS-NSQ-1),CHAR(0))
      IF (NX == 0) THEN
        PRINT *,MSGHDR,'NO MORE OVERFLOWS FOR INDEX'
        NTRIES(LX)=NTRIES(LX)-1
        RETURN
      ELSE
        NOFLOW(NIND,LX)=BLOCKS-NSQ-OFLOWS+NX
        NOFLOW(NIND+1,LX)=0
        NFLIX=128+NINDEX(LX)   ! TO SET FLAG IN MAP BYTE
        MAP(8+NOFLOW(NIND,LX)-1:8+NOFLOW(NIND,LX)-1)=CHAR(NFLIX)
      END IF
    END IF NewIndexBlockRequiredCheck
  END IF IndCheck3

! UPDATE REPORT COUNT IN INDEX & COPY TO MAP BYTE FOR INDEX BLOCK
! (USE N/256 IN CASE THE COUNT IS LARGE; ADD 1 SO THAT THE MAP BYTE
! IS NONZERO EVEN IF THE COUNT IS SMALL)

  NREPS(LX)=NREPS(LX)+1
  MAP(8+NINDEX(LX)-1:8+NINDEX(LX)-1)=CHAR(NREPS(LX)/256+1)
ELSE
!**********************************************************************
!
! IF NO ROOM, SPLIT THE DATA MORE OR LESS EQUALLY BETWEEN 2 BLOCKS.
! (GET THE SECOND FROM THE OVERFLOW POOL: BECAUSE THE INDEX OPERATION
! STARTS FURTHER ON, INDEX(...) MUST BE ADJUSTED TO GIVE BLOCK NUMBER!)
! FOR REPORTS IN THE CURRENT BLOCK, LIST STATION & NUMBER OF REPORTS,
! SORT LIST BY STATION & SPLIT INTO TWO WITH SIMILAR NUMBERS OF OBS.
!
!**********************************************************************
  NX=INDEX(MAP(8+BLOCKS-NSQ-OFLOWS:8+BLOCKS-NSQ-1),CHAR(0))
  IF (NX == 0) THEN
    PRINT *,MSGHDR,'NO DATA OVERFLOWS TILL BLOCKS RELEASED - ID>', &
    IDENT,'< datime>',DATIME,'< rejected '
    RETURN
  END IF
  NBLOCX=BLOCKS-NSQ-OFLOWS-(XBLOKS+1)+NX
  CHARBLX(1:1)=CHAR(NBLOCX/256)
  CHARBLX(2:2)=CHAR(MOD(NBLOCX,256))

  K=0
  NTOTAL=0
  CHARBL(1:1)=CHAR(NBLOCK/256)
  CHARBL(2:2)=CHAR(MOD(NBLOCK,256))
! 410 in code structure notes
  DO I=1,NTRIES(LX)
   IF (INDEKS(I,LX)(22:23) == CHARBL) THEN
     K=K+1
! Use adjusted id if appropriate
     SORTX(K)(1:5)=IDADJ(INDEKS(I,LX)(3:11),5)
     SORTX(K)(6:6)=INDEKS(I,LX)(12:12)
     NTOTAL=NTOTAL+ICHAR(INDEKS(I,LX)(12:12))
   END IF
  END DO

  IDINB=K         ! NUMBER OF INDEX POINTERS TO THE TARGET BLOCK
  IF (IDINB <= 1 .AND. IND == 0) THEN
    PRINT *,MSGHDR,'WRONG BLOCK FOR ',IDENT,'  - SYNOPS'
    PRINT *,'ONLY IDENT IN BLOCK TO BE SPLIT IS ',SORTX(1)(1:5)
    RETURN
  END IF

! SORT COUNTS BY IDENTIFIER & PICK THE IDENTIFIER WHICH SPLITS THE
! COUNTS EVENLY (SKIP THIS IF ALL THE IDENTIFIERS ARE THE SAME!)
! (N.B. BECAUSE OF GT.MID LATER THE SITUATION IS NOT SYMMETRICAL:
! IT'S OK IF THE FIRST IDENTIFIER IS MID BUT NOT THE LAST!  FOR SHIPS
! WE CAN'T ASSUME THAT ALL THE ITEMS SORTED ARE DIFFERENT, THEY MAY
! JUST HAVE THE SAME FIRST 5 CHARACTERS)

  CALL SORTCH(SORTX,5,IDINB,MASK(3:7))
  IF (IDINB == 2) THEN               ! IF ONLY 2, TAKE THE FIRST
    MID=SORTX(1)(1:5)                ! - IF GT.MID AT DO 430!
  ELSE IF (IDINB > 2) THEN
    NOBS=0
! 420 in code structure notes
    DO I=1,IDINB-1
     NOBS=NOBS+ICHAR(SORTX(I)(6:6))
     IF (NOBS >= NTOTAL/2 .OR.             &
       SORTX(I+1)(1:5) == SORTX(IDINB)(1:5)) EXIT
    END DO
    MID=SORTX(I)(1:5)
  END IF

! MID WILL BE PUT IN THE ASSIGNMENT LIST LATER. INITIALISE NEW BLOCK.
! (CAN'T INITIALISE TILL NOW BECAUSE AREA USED AS WORK SPACE)
! NINBLX & LEFX WILL BE SET IN BLOCKX BEFORE IT'S WRITTEN; SET TAG NOW.

  NINBLX=0
  LEFX=BLKSIZ-INDHED
  BLOCKX(1:2)=BLOCK(1:2)
  BLOCKX(7:BLKSIZ)=' '
  MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(NINDEX(LX))

! IF THE FULL BLOCK HAS ONLY ONE IDENT, IT CAN'T BE SPLIT, SO SET THE
! TOP BIT OF THE MAP BYTE AS FOR INDEX OVERFLOW TO STOP STORING IN IT.
FullBlockHasOneIndentCheck: &
  IF (IDINB == 1) THEN
    MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(128+NINDEX(LX))

! IF ALL THE IDENTIFIERS ARE THE SAME, JUST STORE THE REPORT IN THE
! NEW BLOCK, UPDATING THE RANGES (ADDING ANOTHER WITH BOTH ENDPOINTS
! SAME, EFFECTIVELY NULL - THE IDENT'S IN THE INDEX, SO THE RANGE WON'T
! BE USED) & CHAINING ACROSS BLOCKS FROM THIS REPORT TO THE LATEST IN
! THE FULL BLOCK. WRITE THE NEW BLOCK AND THE MAP BLOCK OUT NOW; THE
! FULL BLOCK IS UNCHANGED, BUT WILL STILL BE WRITTEN OUT AT THE END.

    NB=NB+1
    MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
    MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
    BLOKID(NB*2-1)=IDENT4        ! SINGLE ID FOR NEW BLOCK
    BLOKID(NB*2)=IDENT4

    BLOCKX(3:3)=CHAR(0)
    BLOCKX(4:4)=CHAR(1)              ! ONE OB IN NEW DATA BLOCK
    L=LEN(BULL)+INDENT
    LEFX=LEFX-L-2
    BLOCKX(5:5)=CHAR(LEFX/256)
    BLOCKX(6:6)=CHAR(MOD(LEFX,256))
    BLOCKX(7:7)=CHAR(L/256)          ! SET FIRST LENGTH TO L
    BLOCKX(8:8)=CHAR(MOD(L,256))
!                                          ! (IND IS SET IF IDINB=1!)
    POINTER=INDEKS(IND,LX)(20:23)    ! CHAIN TO LATEST IN INDEX
    INDEKS(IND,LX)(20:20)=CHAR(0)
    INDEKS(IND,LX)(21:21)=CHAR(1)    ! FIRST RECORD IN BLOCK
    INDEKS(IND,LX)(22:23)=CHARBLX

    BLOCKX(BLKSIZ-L+1:BLKSIZ-INDENT)=BULL
    BLOCKX(BLKSIZ-INDENT+1:BLKSIZ-4)=ENTRY(1:INDENT-4)
    BLOCKX(BLKSIZ-3:BLKSIZ)=POINTER
    NOBS=ICHAR(INDEKS(IND,LX)(12:12)) ! UPDATE INDEX COUNT
    IF (NOBS < 255) INDEKS(IND,LX)(12:12)=CHAR(NOBS+1)

    REC = 1+XBLOKS+NBLOCX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCKX,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (865)

    REC = 1
    CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (866)
  ELSE IF (IDINB > 1) THEN

! COPY OBS IN 2ND HALF OF LIST TO NEW BLOCK, WHICH MEANS FOLLOWING
! POINTER CHAIN FOR EACH STATION (THE OBS WILL BE STORED IN REVERSE
! ORDER - DOESN'T MATTER), CHANGING THE CORRESPONDING INDEX POINTERS.
! KNOWING THE DISPLACEMENTS (RECDIS), WE CAN ZERO LENGTHS IN RECLEN.
!
! GO THROUGH THE INDEX ENTRIES, PICKING OBS IN THE FULL BLOCK WITH
! IDENTIFIERS AFTER THE BREAK POINT FOR TRANSFER TO THE NEW BLOCK.
! ZERO THE LENGTHS OF THE TRANSFERRED REPORTS AT THE START OF THE
! FULL BLOCK TO LET POINTERS TO REPORTS LEFT IN THIS BLOCK TO STAY
! THE SAME (POINTERS IN THE INDEX AND ON END OF OBS).
! 430 in code structure notes
IndexEntryTransferLoop: &
    DO I=1,NTRIES(LX)

IndexEntryAfterBreakCheck: &
      IF (INDEKS(I,LX)(22:23) == CHARBL .AND. &
        IDADJ(INDEKS(I,LX)(3:11),5) > MID) THEN
        RECNO=ICHAR(INDEKS(I,LX)(20:20))*256  &
            +ICHAR(INDEKS(I,LX)(21:21))
        INDEKS(I,LX)(22:23)=CHARBLX
        INDEKS(I,LX)(20:20)=CHAR((NINBLX+1)/256)
        INDEKS(I,LX)(21:21)=CHAR(MOD(NINBLX+1,256))
 431 CONTINUE
        DISPL=BLKSIZ-RECDIS(RECNO)

! STORE REPORT IN SECOND BLOCK, ZEROING ITS LENGTH IN FIRST BLOCK.

        NINBLX=NINBLX+1
        IF (RECNO == 1) THEN
          L=RECDIS(1)
        ELSE
          L=RECDIS(RECNO)-RECDIS(RECNO-1)
        END IF
        LEFX=LEFX-L-2

        BLOCKX(6+NINBLX*2-1:6+NINBLX*2-1)=CHAR(L/256)
        BLOCKX(6+NINBLX*2:6+NINBLX*2)=CHAR(MOD(L,256))
        START=INDHED+NINBLX*2+LEFX
        BLOCKX(START+1:START+L-4)=BLOCK(DISPL+1:DISPL+L-4)
        BLOCK(6+RECNO*2-1:6+RECNO*2-1)=CHAR(0)
        BLOCK(6+RECNO*2:6+RECNO*2)=CHAR(0)

! FOLLOW CHAIN IF OLD POINTER IS TO SAME BLOCK.  STOP IF ZERO POINTER
! OR DIFFERENT BLOCK (COPYING THE POINTER IF IT GOES ACROSS BLOCKS)

        RECNO=ICHAR(BLOCK(DISPL+L-3:DISPL+L-3))*256 &
          +ICHAR(BLOCK(DISPL+L-2:DISPL+L-2))
        IF (BLOCK(DISPL+L-1:DISPL+L) == CHARBL) THEN
          N=NINBLX+1
          BLOCKX(START+L-3:START+L-3)=CHAR(N/256)
          BLOCKX(START+L-2:START+L-2)=CHAR(MOD(N,256))
          BLOCKX(START+L-1:START+L)=CHARBLX
          GO TO 431
        ELSE IF (RECNO == 0) THEN
          BLOCKX(START+L-3:START+L-3)=CHAR(0)
          BLOCKX(START+L-2:START+L-2)=CHAR(0)
          BLOCKX(START+L-1:START+L-1)=CHAR(0)
          BLOCKX(START+L:START+L)=CHAR(0)
        ELSE           ! IF NOT SAME BLOCK & NOT ZERO, COPY POINTER
          BLOCKX(START+L-3:START+L)=BLOCK(DISPL+L-3:DISPL+L)
        END IF
      END IF IndexEntryAfterBreakCheck
    END DO IndexEntryTransferLoop

! SET NUMBER OF RECORDS & SPARE BYTES IN CHARACTER STRING,
! WRITE OUT SECOND DATA BLOCK, THEN INDEX BLOCK WITH POINTERS TO IT.

    BLOCKX(3:3)=CHAR(NINBLX/256)
    BLOCKX(4:4)=CHAR(MOD(NINBLX,256))
    BLOCKX(5:5)=CHAR(LEFX/256)
    BLOCKX(6:6)=CHAR(MOD(LEFX,256))

    REC = 1+XBLOKS+NBLOCX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCKX,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (867)

! POINTERS HAVE BEEN CHANGED, SO WRITE THE WHOLE INDEX FOR THIS TIME.
! FIRST WRITE ANY FULL INDEX BLOCKS

    INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
    INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
    INDHDR(3:3)=CHAR(NTRIES(LX)/256)
    INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
    INDHDR(5:5)=CHAR(NREPS(LX)/256)
    INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

    NFULL=NTRIES(LX)/NBLIND            ! NUMBER OF FULL BLOCKS
    IX=NINDEX(LX)                      ! TO VARY BLOCK IN LOOP
! 440 in code structure notes
    DO NIND=1,NFULL
      NIBL=(NIND-1)*NBLIND             ! ENTRIES ALREADY WRITTEN
      NEXTIX=NOFLOW(NIND,LX)           ! OVERFLOW BLOCK NO

      BUFFER=INDHDR
      IPOS=7
      DO I=NIBL+1,NIBL+NBLIND
        BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
        IPOS=IPOS+23
      END DO
      BUFFER(IPOS:IPOS+1)= CHAR(NEXTIX/256)//CHAR(MOD(NEXTIX,256))
      REC = IX+NSQ
      CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
      IF (IORC /= 0) CALL SYSABN (868)
      IX=NEXTIX                        ! OVERFLOW BLOCK NO
    END DO

! THEN THE LAST INDEX BLOCK (WHICH MAY BE THE ONLY ONE)

    IF (NTRIES(LX) > NFULL*NBLIND) THEN
      BUFFER=INDHDR
      IPOS=7
      DO I=NFULL*NBLIND+1,NTRIES(LX)
        BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
        IPOS=IPOS+23
      END DO
      NTIMES = ((NFULL+1)*NBLIND) - NTRIES(LX)
      BUFFER(IPOS:IPOS+(NTIMES*23)-1)= REPEAT(EMPTYX,NTIMES)
      IPOS=IPOS+(NTIMES*23)
      BUFFER(IPOS:IPOS+1)=CHAR(0)//CHAR(0)
      REC = IX+NSQ
      CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
      IF (IORC /= 0) CALL SYSABN (869)
    END IF

! REMAKE THE ORIGINAL DATA BLOCK BY GOING THROUGH THE REPORTS IN ORDER
! & MOVING ONLY THOSE WITH NON-ZERO LENGTHS.  (N.B. WE MUST DO THIS
! DIFFERENTLY - NOT FOLLOWING POINTER CHAINS AS ABOVE - TO LEAVE THE
! ZERO LENGTHS).  FINALLY WRITE BACK THE COMPRESSED DATA BLOCK.
!
! THE OLD BLOCK IS COMPRESSED IN PLACE.  THE NEW BLOCK HAS BEEN WRITTEN
! OUT, SO ITS AREA CAN BE USED AS WORK SPACE TO COPY ANY REPORT WHOSE
! OLD & NEW POSITIONS OVERLAP (OTHERWISE CHARACTERS WOULD BE REPEATED
! FROM LEFT TO RIGHT).  IF THE CURRENT REPORT IS TO BE STORED IN THE
! NEW BLOCK, THAT WILL BE READ IN AGAIN INTO THE BUFFER CALLED BLOCK.

    NSTART=BLKSIZ               ! INITIALISE NEW START
    LN=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))

! 450 in code structure notes
NinblkCheck: &
    DO I=1,NINBLK
      IF (LN > 0) THEN
        ISTART=BLKSIZ-RECDIS(I)  ! INPUT START (STILL OLD RECDIS!)
        NSTART=NSTART-LN         ! ADJUST NEW (OUTPUT) START
        IF (NSTART > ISTART) THEN
          IF (NSTART-ISTART < LN) THEN
            BLOCKX(BLKSIZ-LN+1:BLKSIZ)=BLOCK(ISTART+1:ISTART+LN)
            BLOCK(NSTART+1:NSTART+LN)=BLOCKX(BLKSIZ-LN+1:BLKSIZ)
          ELSE
            BLOCK(NSTART+1:NSTART+LN)=BLOCK(ISTART+1:ISTART+LN)
          END IF
        END IF
      END IF
! NOW RESET RECDIS (OLD VALUE USED IN LOOP ABOVE!) & FIND NEXT LENGTH
      IF (I == 1) THEN
        RECDIS(1)=LN
      ELSE
        RECDIS(I)=RECDIS(I-1)+LN
      END IF
      LN=ICHAR(BLOCK(7+I*2:7+I*2))*256+ICHAR(BLOCK(8+I*2:8+I*2))
    END DO NinblkCheck

    LEFT=NSTART-2*NINBLK-INDHED
    BLOCK(5:5)=CHAR(LEFT/256)
    BLOCK(6:6)=CHAR(MOD(LEFT,256))
    BLOCK(INDHED+2*NINBLK+1:INDHED+2*NINBLK+LEFT)=' '

    REC = 1+XBLOKS+NBLOCK+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (870)

! UPDATE THE LIST OF RANGES OF IDENTIFIERS ASSIGNED TO DATA BLOCKS,
! ADDING THE SECOND PART OF THE SPLIT RANGE TO THE END OF THE LIST.

    IF (LISTAG == CURENT) THEN
      NB=NB+1
      MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
      MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
      I = 1
! 470 in code structure notes
      DO WHILE (I <= NB .AND. .NOT. &
          (MID(1:4) > BLOKID(I*2-1)) .AND. &
          (MID(1:4) <= BLOKID(I*2)))
        I = I + 1
      END DO
      BLOKID(NB*2-1)=MID(1:4)
      BLOKID(NB*2)=BLOKID(I*2)
      BLOKID(I*2)=MID(1:4)
    END IF

! FINALLY WRITE BACK THE MAP BLOCK & GO BACK TO STORE THE REPORT

    REC = 1
    CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (871)
    GO TO 200
  END IF FullBlockHasOneIndentCheck
END IF RoomLeftInBlockCheck
!**********************************************************************
!
! WRITE BACK TO DATA SET IN FOLLOWING ORDER:
!      (THE ORDER MATTERS BECAUSE  A SYSTEM FAILURE AFTER 1 OR 2 OF THE
!        3 WRITES COULD LEAVE THE DATA BASE IN AN INCONSISTENT STATE.)
!**** DATA BLOCK (D), MAP OF BLOCKS IN USE (M), INDEX FOR HOUR (I) *****
! (ARGUING AS FOLLOWS: BETTER D BUT NOT I, DATA STORED BUT INACCESSIBLE,
!  THAN I BUT NOT D, INDEX ENTRY FOR LOST DATA; BETTER D BUT NOT M, DATA
!  STORED BUT MAY BE OVERWRITTEN BECAUSE BLOCK NOT CLAIMED IN MAP, THAN
!  M BUT NOT D, BLOCK TIED UP BUT DATA LOST; BETTER M BUT NOT I, BLOCK
!  TIED UP BUT DATA INACCESSIBLE BECAUSE NO INDEX ENTRY, THAN I BUT NOT
!  M, INDEX POINTS TO DATA WHICH MAY BE LOST BECAUSE BLOCK RECLAIMED.

BLOCK(3:3)=CHAR(NINBLK/256)
BLOCK(4:4)=CHAR(MOD(NINBLK,256))
BLOCK(5:5)=CHAR(LEFT/256)
BLOCK(6:6)=CHAR(MOD(LEFT,256))

REC = 1+XBLOKS+NBLOCK+NSQ
CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
IF (IORC /= 0) CALL SYSABN (872)

! WRITE THE MAP BLOCK BACK ONLY IF A NEW DATA OR INDEX BLOCK HAS BEEN
! CLAIMED - OR IF THE NUMBER OF REPORTS FOR THIS INDEX BLOCK IS N*256.

IF (NINBLK == 1 .OR. NTRIES(LX)-(NTRIES(LX)/NBLIND)*NBLIND == 1 &
                .OR. NREPS(LX) == (NREPS(LX)/256)*256) THEN
  REC = 1
  CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (873)
END IF

! FINALLY THE INDEX.  IF IT EXTENDS OVER MORE THAN ONE BLOCK, THERE
! MAY BE NO NEED TO WRITE EVERYTHING.  ONLY ONE ENTRY HAS BEEN CHANGED
! OR ADDED, SO WE MUST WRITE THE BLOCK WITH THAT ENTRY IN.  AN ADDITION
! CHANGES THE COUNT, SO WRITE THE FIRST BLOCK (THOUGH EVERY BLOCK HAS
! THE COUNT IN, IT'S ACTUALLY READ FROM THE LAST) AND IF A NEW ENTRY
! STARTS A NEW BLOCK, WRITE THE BLOCK BEFORE WITH ITS POINTER SET.
! HENCE THE FOLLOWING STRUCTURE:
!          WORK OUT THE NUMBER OF FULL BLOCKS.                  (NFULL)
!          IF AN EXISTING ENTRY WAS UPDATED, THEN               (IND>0)
!            FIND WHICH BLOCK THE ENTRY IS IN.                  (NIND)
!            IF IN A FULL BLOCK (NOT THE LAST) WRITE THE FULL BLOCK.
!            IF IT IS IN THE LAST BLOCK, WRITE THE LAST BLOCK.
!          ELSE (IF AN ENTRY WAS ADDED)
!            IF THERE IS A FULL BLOCK, WRITE THE FIRST BLOCK.
!            IF THERE IS MORE THAN ONE FULL BLOCK, THEN
!              IF THIS IS THE FIRST ENTRY IN A BLOCK, THEN
!                WRITE THE LAST FULL BLOCK.
!            WRITE THE LAST BLOCK (NOT FULL) WITH THE NEW ENTRY IN.

INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
INDHDR(3:3)=CHAR(NTRIES(LX)/256)
INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
INDHDR(5:5)=CHAR(NREPS(LX)/256)
INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

NFULL=NTRIES(LX)/NBLIND                ! NUMBER OF FULL BLOCKS

EntryUpdatedCheck: &
IF (IND > 0) THEN                      ! IF ENTRY UPDATED,
  NIND=(IND+NBLIND-1)/NBLIND           ! IT'S IN NIND-TH BLOCK.
  IF (NIND == 1) THEN                  ! IF IT'S THE FIRST BLOCK,
    IX=NINDEX(LX)                      ! GET BASIC BLOCK NUMBER.
  ELSE                                 ! IF NOT THE FIRST,
    IX=NOFLOW(NIND-1,LX)               ! GET OVERFLOW BLOCK NUMBER
  END IF

  IF (NIND <= NFULL) THEN              ! IF IT'S IN A FULL BLOCK
    NIBL=(NIND-1)*NBLIND               ! ENTRIES TO SKIP
    BUFFER=INDHDR
    IPOS=7
    DO I=NIBL+1,NIBL+NBLIND
      BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
      IPOS=IPOS+23
    END DO
    BUFFER(IPOS:IPOS+1)= CHAR(NOFLOW(NIND,LX)/256)// &
                         CHAR(MOD(NOFLOW(NIND,LX),256))
    REC = IX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
  ELSE                                 ! IF IN THE LAST BLOCK
    BUFFER=INDHDR
    IPOS=7
    DO I=NFULL*NBLIND+1,NTRIES(LX)
      BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
      IPOS=IPOS+23
    END DO
    NTIMES = ((NFULL+1)*NBLIND) - NTRIES(LX)
    BUFFER(IPOS:IPOS+(NTIMES*23)-1)= REPEAT(EMPTYX,NTIMES)
    IPOS=IPOS+(NTIMES*23)
    BUFFER(IPOS:IPOS+1)=CHAR(0)//CHAR(0)
    REC = IX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
  END IF
!                                            ! IF ENTRY ADDED,
ELSE                                   ! WRITE FIRST BLOCK
  IF (NFULL >= 1) THEN
    IX=NINDEX(LX)                      ! GET BASIC BLOCK NUMBER.
    BUFFER=INDHDR
    IPOS=7
    DO I=1,NBLIND
      BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
      IPOS=IPOS+23
    END DO
    BUFFER(IPOS:IPOS+1)= CHAR(NOFLOW(1,LX)/256)// &
                         CHAR(MOD(NOFLOW(1,LX),256))
    REC = IX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (874)
  END IF
!                                            ! WRITE LAST FULL BLOCK
  IF (NFULL > 1 .AND. MOD(0+NTRIES(LX),NBLIND) == 1) THEN
    IX=NOFLOW(NFULL-1,LX)
    NIBL=(NFULL-1)*NBLIND              ! ENTRIES TO SKIP

    BUFFER=INDHDR
    IPOS=7
    DO I=NIBL+1,NIBL+NBLIND
      BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
      IPOS=IPOS+23
    END DO
    BUFFER(IPOS:IPOS+1)= CHAR(NOFLOW(NFULL,LX)/256)// &
                         CHAR(MOD(NOFLOW(NFULL,LX),256))
    REC = IX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (875)
  END IF
!                                            ! WRITE LAST OR ONLY BLOCK

! The last block is always written, so if the last block is also a
! full block, reduce NFULL to get the range of entries right.

  IF (NFULL == 0 .OR. NTRIES(LX) == NBLIND) THEN
    IX=NINDEX(LX)                      ! IF ONLY BLOCK...
  ELSE IF (NTRIES(LX) == NFULL*NBLIND) THEN
    IX=NOFLOW(NFULL-1,LX)              ! IF LAST BLOCK IS FULL
    NFULL=NFULL-1
  ELSE IF (NTRIES(LX) > NFULL*NBLIND) THEN
    IX=NOFLOW(NFULL,LX)                ! IF LAST BLOCK NOT FULL
  END IF

  BUFFER=INDHDR
  IPOS=7
  DO I=NFULL*NBLIND+1,NTRIES(LX)
    BUFFER(IPOS:IPOS+22)=INDEKS(I,LX)
    IPOS=IPOS+23
  END DO
  NTIMES = ((NFULL+1)*NBLIND) - NTRIES(LX)
  BUFFER(IPOS:IPOS+(NTIMES*23)-1)= REPEAT(EMPTYX,NTIMES)
  IPOS=IPOS+(NTIMES*23)
  BUFFER(IPOS:IPOS+1)=CHAR(0)//CHAR(0)
  REC = IX+NSQ
  CALL METDB_CWRITE_DIR(IFT,BUFFER,BLKSIZ,REC,IORC)
END IF EntryUpdatedCheck
IF (IORC /= 0) CALL SYSABN (876)

RETURN

CONTAINS

FUNCTION IDADJ(ID,L)
!
! TO RETURN THE FIRST L CHARACTERS OF AN ID (ASSUMED TO BE > L). FOR
! CERTAIN IDS, RETURN AN ADJUSTED 4 CHARACTER ID FOLLOWED BY SPACES IF
! NECESSARY TO BRING IT UP TO L CHARACTERS.
!
CHARACTER(LEN=*),INTENT(IN) :: ID      ! ID TO ADJUST
INTEGER,INTENT(IN)          :: L       ! REQUIRED LENGHTH
CHARACTER(LEN=L)            :: IDADJ

IDADJ = ' '
IF (ID(1:5) == 'AMOUK') THEN
  IDADJ(1:4) = ID(1:2)//ID(6:7)
ELSE
  IDADJ = ID(1:L)   !DEFAULT
END IF
END FUNCTION IDADJ

END SUBROUTINE SHPSTO
