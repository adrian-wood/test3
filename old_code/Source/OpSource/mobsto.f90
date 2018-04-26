SUBROUTINE MOBSTO (DATIME, ENTRY, BULL, IFT, BLKSIZ, IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : MOBSTO   (SIMILAR TO SHPSTO BUT DIFFERENT COMMON)
!
! PURPOSE       : Store Mobile SYNOPs in the MDB (23-byte index
!                 entries, Reports chained in data blocks)
!
! DATA TYPES    : MOBILE SYNOPS
!
! CALLED BY     : SYNIND
!
! CALLS         : DATIM,DATE31,DATE13,EB2ASC,SYSABN,SORTCH,CENTURY
!
! ARGUMENTS     : (1) DATE/TIME (YEAR, MONTH...)                    I/O
!                 (2) INDEX ENTRY (WITHOUT TIMES & BLOCK/RECORD NO) I/O
!                 (3) REPORT TO BE STORED                           I
!                 (4) FT NUMBER (ASSUME IT'S ALWAYS THE SAME)       I
!                 (5) BLOCKSIZE OF OUTPUT DATA SET                  I
!                 (6) IDENTIFIER TO GO IN INDEX ENTRY               I
!
! REVISION INFO :
!
! $Workfile: mobsto.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 03/02/2012 10:33:05$
!
! CHANGE RECORD :
! $Log:
!  6    MetDB_Refresh 1.5         03/02/2012 10:33:05    Sheila Needham
!       Removed PRINT* with binary data
!  5    MetDB_Refresh 1.4         23/03/2011 16:26:18    Brian Barwell   Length
!        of BUFFER changed to MAXBLK. SAVE added.
!  4    MetDB_Refresh 1.3         08/03/2011 11:17:57    Sheila Needham
!       Updated for C I/O routines
!  3    MetDB_Refresh 1.2         25/01/2011 09:04:29    Sheila Needham  Added
!       Log info.
!  2    MetDB_Refresh 1.1         22/01/2011 15:36:55    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         17/01/2011 16:06:08    Sheila Needham
!       Initial F77 version
! $ 
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

! Interfaces

USE datim_mod
USE zpdate_mod
USE eb2asc_mod
USE sortch_mod
USE century_mod

IMPLICIT NONE

! Arguments

INTEGER,         INTENT(INOUT) :: DATIME(5)
CHARACTER(LEN=*),INTENT(INOUT) :: ENTRY
CHARACTER(LEN=*),INTENT(IN)    :: BULL
INTEGER,         INTENT(IN)    :: IFT
INTEGER,         INTENT(IN)    :: BLKSIZ
CHARACTER(LEN=*),INTENT(IN)    :: IDENT

! Local Variables

INTEGER,PARAMETER :: MAXBLK=27998
INTEGER,PARAMETER :: INDLEN=23
INTEGER,PARAMETER :: MAXOFL=3
INTEGER,PARAMETER :: INDHED=6
INTEGER :: AMSTAR
INTEGER :: BLKTAG
INTEGER :: BLOCKS
INTEGER :: CENDAY
INTEGER :: CENTHR
INTEGER :: CURENT
INTEGER :: DISPL
INTEGER :: HOURNOW
INTEGER :: I
INTEGER :: IDINB
INTEGER :: IFLAGS
INTEGER :: IND
INTEGER :: INDENT
INTEGER :: INDHOR
LOGICAL :: INSERT
INTEGER :: IORC
INTEGER :: IPOS    ! Pointer to position in string
INTEGER :: ISTART
INTEGER :: ITOR
INTEGER :: IX
INTEGER :: IY
INTEGER :: J
INTEGER :: K
INTEGER :: L
INTEGER :: LASTHR
INTEGER :: LASTIX
INTEGER :: LATEST=0
INTEGER :: LEFT
INTEGER :: LEFX
INTEGER :: LENBUF
INTEGER :: LIDENT
INTEGER :: LISTAG
INTEGER :: LL
INTEGER :: LN
INTEGER :: LOB
INTEGER :: LX
INTEGER :: N
INTEGER :: NAFTER
INTEGER :: NB
INTEGER :: NBLIND
INTEGER :: NBLOC
INTEGER :: NBLOCK
INTEGER :: NBLOCX
INTEGER :: NBLOK
INTEGER :: NCHAIN
INTEGER :: NEXTIX
INTEGER :: NFLIX
INTEGER :: NFULL
INTEGER :: NIBL
INTEGER :: NINBLK
INTEGER :: NINBLX
INTEGER :: NIND
INTEGER :: NL
INTEGER :: NOB
INTEGER :: NOBS
INTEGER :: NOTPTR
INTEGER :: NOW(8)
INTEGER :: NPREF
INTEGER :: NREP
INTEGER :: NSEQBL
INTEGER :: NSQ
INTEGER :: NSTART
INTEGER :: NTIMES   !Count of blank index entries to write back
INTEGER :: NTOTAL
INTEGER :: NX
INTEGER :: NXBLOK
INTEGER :: OFLOWS
INTEGER :: POINTR
INTEGER :: REC      ! C I/O record number
INTEGER :: RECDIS(MAXBLK/2)
INTEGER :: RECNO
INTEGER :: SLODAY
INTEGER :: SLOTHR
INTEGER :: START
INTEGER :: X
INTEGER :: XBLOKS
INTEGER :: XHOURS
INTEGER :: Y

CHARACTER(LEN=4)  :: BUFR='BUFR'
CHARACTER(LEN=2)  :: CHARBL
CHARACTER(LEN=2)  :: CHARBLX
CHARACTER(LEN=23) :: EMPTYX=' '
CHARACTER(LEN=23) :: INDEKS(MAXOFL*MAXBLK/INDLEN,3) ! INDEX ENTRIES
CHARACTER(LEN=6)  :: INDHDR
CHARACTER(LEN=2)  :: INDOVR
CHARACTER(LEN=23) :: MASK='  XXXXX                ' !ident sort
CHARACTER(LEN=23) :: MASX='           X           ' !count sort
CHARACTER(LEN=1)  :: MEDIAN
CHARACTER(LEN=5)  :: MID
CHARACTER(LEN=14) :: MSGHDR='....Z MOBSTO: '
CHARACTER(LEN=4)  :: POINTER
CHARACTER(LEN=1)  :: QUARTL
CHARACTER(LEN=MAXBLK) :: BUFFER    ! Buffer for use in C I/O calls

! Indexes for three 6-hour periods are kept in core: subscript LX says
! which is in use for the current report.  So all items read from the
! index are in arrays of dimension 3, as are the corresponding block
! numbers and century hours.
! The oldest slot will always be reused to read an index for a period
! not in core, so usually the latest two 6-hour periods and one other
! will be in core.

INTEGER :: TIMTAG(3)             ! INDEX HEADERS
INTEGER :: NTRIES(3)             ! INDEX HEADERS
INTEGER :: NREPS(3)              ! INDEX HEADERS
INTEGER :: NOFLOW(MAXOFL,3)      ! OVERFLOW POINTERS
INTEGER :: NINDEX(3)             ! BLOCK NUMBERS & CENTURY-HOURS
INTEGER :: IXHOUR(3)             ! BLOCK NUMBERS & CENTURY-HOURS
INTEGER :: IDSK(5)               ! ARRAY FOR MAPRD (IFT ETC)
LOGICAL :: FIRST=.TRUE. ! .TRUE. if first call to subroutine

CHARACTER(LEN=MAXBLK):: MAP
CHARACTER(LEN=MAXBLK):: BLOCK
CHARACTER(LEN=MAXBLK):: BLOCKX
CHARACTER(LEN=4)     :: BLOKID(1000)
CHARACTER(LEN=4)     :: ALLNIL
CHARACTER(LEN=4)     :: ALLONE

COMMON /MOBIX/ IDSK, IXHOUR,NTRIES, INDEKS
COMMON /MOBDC/ MAP, BLOCK, BLOCKX, RECDIS
EQUIVALENCE (BLOKID,MAP(MAXBLK-4004:))

! Use BLOCKX as a work area for sorts, equivalencing the arrays needed

CHARACTER(LEN=1) ::  MAPX(MAXBLK)
CHARACTER(LEN=6) ::  SORTX(MAXBLK/6)
EQUIVALENCE (BLOCKX,MAPX)
EQUIVALENCE (BLOCKX,SORTX)

SAVE

IF (FIRST) THEN
  ALLNIL = CHAR(0) // CHAR(0) // CHAR(0) // CHAR(0)
  ALLONE = CHAR(255) // CHAR(255) // CHAR(255) // CHAR(255)
  FIRST = .FALSE.
END IF

! Find length of IDENT for use in consistency check later

LIDENT=LEN(IDENT)
DO WHILE (IDENT(LIDENT:LIDENT) == ' ')
  LIDENT=LIDENT-1
END DO

!----------------------------------------------------------------------
!
! If first time, read in map block (to get number of index blocks)
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
! Set time of latest data stored to clock time at start of run; it will
! be reset to the latest century-hour stored whenever that is greater.
!
!----------------------------------------------------------------------
CALL DATIM(NOW)
WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
HOURNOW=(CENDAY-1)*24+NOW(5)           ! CURRENT CENTURY-HOUR
!                                            ! FIRST TIME READ MAP
Iflabel1: &
IF (LATEST == 0) THEN
  CALL EB2ASC(4,BUFR)   ! CONVERT 'BUFR' TO ASCII FIRST TIME ONLY
  REC = 1
  CALL METDB_CREAD_DIR(IFT,MAP,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (861)

  BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))
  XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))
  XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))
  AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))

  NB=ICHAR(MAP(BLKSIZ-4006:BLKSIZ-4006))*256   &
    +ICHAR(MAP(BLKSIZ-4005:BLKSIZ-4005))
  OFLOWS=ICHAR(MAP(BLKSIZ-3:BLKSIZ-3))*256     &
        +ICHAR(MAP(BLKSIZ-2:BLKSIZ-2))
  INDENT=ICHAR(MAP(BLKSIZ-1:BLKSIZ-1))*256     &
        +ICHAR(MAP(BLKSIZ:BLKSIZ))

! Set NSQ to calculate block numbers allowing for sequence record.

  NSEQBL=ICHAR(MAP(8+BLOCKS:8+BLOCKS))
  IF(NSEQBL > 0)THEN
    NSQ=1
  ELSE
    NSQ=0
  END IF

! Set up an array of data set parameters as in retrieval for use by
! MAPRD in sequence checks.

  IDSK(2)=BLKSIZ                       ! BLOCKSIZE
  IDSK(3)=IFT                          ! FT NUMBER
  IDSK(5)=1                            ! DIRECT ACCESS

  NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
  IF (LATEST == 0) LATEST=HOURNOW      ! LATEST CENTURY-HOUR
END IF Iflabel1
!----------------------------------------------------------------------
!
! COMPLETE TIME FIELDS IN INDEX ENTRY (TIME & TIME OF RECEIPT),
! FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.
! TIME OF RECEIPT IS IN MINUTES FROM THE SLOT TIME (HOURNOW-SLOTHR,
! WHERE HOURNOW IS THE CURRENT CENTURY-HOUR) AND CAN BE <0 OR >1440.
!
!----------------------------------------------------------------------
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

!---------------------------------------------------------------------
! If the data is for too long before the the most recent data stored
! ('too long' means more than the period covered by the data base),
! then reject it (to avoid overwriting current data with old data!).
! If the new data is more recent, update the latest hour - and the
! index block tag for which the list of ranges can be updated (set
! when a new index block is started, but this copes until then...).
!
!  (XBLOKS-1 rather than XBLOKS because although we can store data
! extending over a period of XBLOKS*XHOURS if that period starts at
! the start of an index block, in general such a period would reuse
! the latest index block for the oldest data.)
!---------------------------------------------------------------------

IF (CENTHR <= LATEST-(XBLOKS-1)*XHOURS) THEN
  PRINT *,MSGHDR,LATEST-CENTHR,'HOURS OLD   ',BULL(:60)
  RETURN
END IF

IF (CENTHR > LATEST) THEN
  LATEST=CENTHR
  LISTAG=CURENT                        ! LATEST FOR RANGE UPDATE
END IF
!----------------------------------------------------------------------
!
! The index is divided into N-hourly segments. Work out which segment
! from the century-hour and read in the corresponding index block.
!
! INDEX BLOCK:
! ------------------------------------------------- - - - - - ---------
! : DATE/ : NO. OF : NO. OF  : 23-BYTE : 23-BYTE :             : OVER :
! : TIME  : ENTRIES: REPORTS :  ENTRY  :  ENTRY  : ENTRIES...  : FLOW :
! ------------------------------------------------- - - - - - ---------
! 0       2        4         6        29        52         LAST 2 BYTES
!
!----------------------------------------------------------------------
NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2  ! INDEX BLOCK NUMBER

! See if the index is in one of the buffers in core.

LX=0
DO I=1,3
  IF (NXBLOK == NINDEX(I)) LX=I
END DO

! If not, reuse buffer with oldest index  (following code must set LX!)
!                                                         ====
Iflabel2: &
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

! Get NTRIES & NREPS from the first index block rather than the last
! if there is index overflow: the first is always updated, the last
! not, so taking NREPS from the last risks getting a low value, a low
! value of NREPS (not equal to the sum of the counts for the index
! entries) spoils the distribution for a new index period.

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
END IF Iflabel2
!----------------------------------------------------------------------
! THE STRUCTURE OF THE FOLLOWING CODE IS:
!
! 100 IF (NO CURRENT INDEX BLOCK EXISTS) THEN
!       RELEASE OLD DATA BLOCKS & SET UP NEW ALLOCATION TABLE:
!         110 RELEASE DATA & INDEX BLOCKS BY ZEROING MAP BYTES.
!         120 DECIDE WHICH PAST INDEX BLOCK TO USE FOR DISTRIBUTION.
!         130 READ IT IN (ALLOWING FOR OVERFLOW).
!             SORT INDEX ENTRIES INTO IDENTIFIER ORDER.
!         140 BREAK THIS LIST INTO IDENTIFIER RANGES WITH ROUGHLY
!             EQUAL NUMBERS OF REPORTS.
!     END IF
!
! 200 IF (STATION IS INDEXED) THEN
!         210 ALLOCATE SAME DATA BLOCK & CHAIN POINTERS;
!             KEEP NUMBER OF INDEX ENTRY FOR USE LATER.
!     ELSE
!         FIND WHICH RANGE THE IDENTIFIER IS IN
!         & FIND THE CORRESPONDING BLOCK NUMBER FROM THE MAP;
!             SET THE POINTER TO ZERO.
!     END IF
!
!     READ IN THE DATA BLOCK
!
! 320 IF THERE'S AN INDEX ENTRY, CHECK FOR DUPLICATE: RETURN IF SAME.
!     IF (THERE IS ROOM IN THE DATA BLOCK) THEN
!       PUT REPORT IN DATA BLOCK, INSERTING IT IN THE CHAIN IN
!       TIME ORDER, AND ADD OR UPDATE INDEX ENTRY.
!       IF THE EXTRA ENTRY MEANS ANOTHER INDEX BLOCK, CLAIM AN
!        OVERFLOW BLOCK FOR WHEN THE INDEX IS WRITTEN OUT.
!     ELSE
! 400   SPLIT THE DATA BLOCK AS FOLLOWS:
!         LIST NUMBER OF REPORTS FOR EACH IDENTIFIER IN BLOCK.
!         SORT INTO IDENTIFIER ORDER & SPLITTING THE LIST TO
!              GIVE ROUGHLY EQUAL NUMBERS IN BOTH BLOCKS.
!         430 MOVE OBS IN THE 2ND HALF OF THE LIST TO THE NEW BLOCK,
!              FOLLOWING CHAINS; ZERO THESE LENGTHS IN THE OLD BLOCK.
!         440 WRITE OUT NEW BLOCK & INDEX WITH POINTERS TO IT.
!         450 COMPRESS OLD BLOCK BY MOVING REMAINING REPORTS TO END,
!              BUT LEAVING ZERO LENGTHS SO THAT NO CHANGE TO POINTERS.
!         460 WRITE BACK OLD BLOCK & UPDATE LIST OF IDENTIFIER RANGES
!         470  MADE AT 140 TO REFLECT THE SPLIT.
!       GO BACK TO 200 TO STORE REPORT
!     END IF
! 500 WRITE OUT INDEX & DATA BLOCKS & MAP BLOCK IF UPDATED
!
!----------------------------------------------------------------------

! The time tag (date/time in index block) is (day of month)*256+hour.
! The time tag is the slot this segment was last used for.  If it was
! last used for the oldest data in the bank, free the blocks attached
! to it before storing new data.
!
! NON-INDEX BLOCK:    (LENGTHS OF RECORDS AT START, DATA ITSELF AT END)
! ---------------------------------- - - - - - -----------------------
! :TIME: NUM OF : LENGTH : L1 : L2 :     FREE       : SECOND : FIRST  :
! :TAG : RECORDS:  FREE  :    :    :     SPACE      : RECORD : RECORD :
! ---------------------------------- - - - - - -----------------------
! 0    2        4        6    8   10            END-L1-L2  END-L1   END

Iflabel3: &
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
111 CONTINUE
  N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(128+NINDEX(LX)))
  IF (N > 0) THEN
    NBLOK=NBLOK+(N-1)
    MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
    IF (1+XBLOKS+NBLOK < BLOCKS) GO TO 111
  END IF

! Choose a previous index block to allocate stations to data blocks,
! avoiding low totals & very high ones by taking a block with a total
! between the median & the upper quartile, either that for 24 hours
! ago or the most recent in the range.

  DO I=1,XBLOKS
    MAPX(I)=MAP(8+I:8+I)               ! TO WORK AREA FOR SORT
  END DO

  CALL SORTCH(MAPX,1,XBLOKS,MASK(3:3))
  MEDIAN=MAPX(XBLOKS/2)                ! MIDDLE COUNT AFTER SORT
  QUARTL=MAPX(3*XBLOKS/4)

!       IF (MEDIAN == CHAR(0)) THEN          ! USE MAX IF ZERO MEDIAN
Iflabel4: &
  IF (ICHAR(MAPX(XBLOKS)) >= 2*ICHAR(MEDIAN)) THEN
    LASTIX=1+INDEX(MAP(8+1:8+XBLOKS),MAPX(XBLOKS))
  ELSE
    I=0                                ! BLOCKS TO GO BACK
    J=NINDEX(LX)-24/XHOURS             ! FIRST TRY 24 HOURS BACK,
120 CONTINUE
    IF (J <= 1) J=XBLOKS+J             ! WRAPPING ROUND IF NEEDED
    IF (J <= 1 .OR.                  & ! IF TOO FEW INDEX BLOCKS
        MAP(J+7:J+7) < MEDIAN .OR.   & ! OR COUNT TOO SMALL
        MAP(J+7:J+7) > QUARTL) THEN    ! OR COUNT TOO BIG,
      I=I+1                            ! GO BACK A(NOTHER) BLOCK
      J=NINDEX(LX)-I                   ! FROM CURRENT INDEX BLOCK
      IF (I <= XBLOKS) GO TO 120       ! IF ANY NOT YET TRIED...
    END IF                             ! N.B. J=1:N, NINDEX=2:N+1
    LASTIX=J                           ! INDEX BLOCK TO USE
  END IF Iflabel4

! Read in the index block(s) for the period chosen above for its count.

  IX=LASTIX                            ! TO VARY IN READ LOOP
  NIND=1                               ! CONTINUATION NUMBER
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

! Break the list of index entries into NB parts with roughly equal
! numbers of reports (LOB=NREPS/NB), where NB is the number of data
! blocks (not counting overflows); keep the identifier ranges.
!
! Ff there are no entries in the previous index block, i.e. we are
! starting up, assign only one block and then split when necessary;
! the next index block will be able to use this distribution.
!
! N.B. BLOKID is a list of pairs of identifiers defining ranges rather
! than single identifiers as break points. This makes it easier to up-
! date: if a range is split the second half can just be put at the end.

  NB=(BLOCKS-1-NSQ-XBLOKS-OFLOWS)/XBLOKS
  IF (NTRIES(LX) == 0) NB=1
  MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
  MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
  BLOKID(1)=ALLNIL            ! START OF FIRST RANGE (MIN POSS)
  BLOKID(NB*2)=ALLONE         ! END OF LAST RANGE    (MAX POSS)

  IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))  ! GET FIRST
  MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))     ! DATA BLOCK

Iflabel5: &
  IF (NB > 1) THEN                     ! SORT INTO COUNT ORDER
    CALL SORTCH(INDEKS(1,LX),INDENT,NTRIES(LX),MASX)

! The sort puts entries with big counts at the end of the list.
! Any ident with more than NREPS/NB will be assigned a whole block.
! The rest will be re-sorted in ident order & shared out between
! the remaining blocks.

    NL=NTRIES(LX)                      ! NUMBER OF ENTRIES
    LOB=NREPS(LX)/NB                   ! MEAN OBS PER BLOCK
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
    DO I=1,NTRIES(LX)
     NOB=NOB+ICHAR(INDEKS(I,LX)(12:12))
     IF (NOB >= LOB*K .AND. K < NB) THEN
       BLOKID(K*2)=INDEKS(I,LX)(3:6)   ! END OF ONE RANGE
       BLOKID(K*2+1)=INDEKS(I,LX)(3:6) ! START OF NEXT - SAME
       K=K+1                           ! ONE MORE RANGE DONE
       IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))
       MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))
     END IF
    END DO
  END IF Iflabel5

! Finally reinitialise the index buffer for the new hour.
! Keep the time tag for this list in case a block is split: the ranges
! will only be updated for data with this tag, for the latest period.

  TIMTAG(LX)=CURENT
  NTRIES(LX)=0
  NREPS(LX)=0
  NOFLOW(1,LX)=0

  LISTAG=CURENT
END IF Iflabel3
!----------------------------------------------------------------------
!
! If the index is not empty (if data already stored for this hour), see
! if there is already an entry for this station.  If so, try to store
! the report in the same data block.  Make the index entry point to the
! new report, and set the pointer on the end of this report to point to
! the previous report, i.e. copy the old pointer from the index.  Add 1
! to the number of reports chained to this index entry.
!
!----------------------------------------------------------------------
!
! BRANCH BACK TO 200 TO STORE OB IF BLOCK WAS FULL & HAD TO BE SPLIT

200 CONTINUE
IND=0
Dolabel210: &
DO I=1,NTRIES(LX)
  IF (INDEKS(I,LX)(3:11) == IDENT) THEN
    NBLOC=ICHAR(INDEKS(I,LX)(22:22))*256+ICHAR(INDEKS(I,LX)(23:23))
    POINTER=INDEKS(I,LX)(20:23)
    IND=I
  END IF
END DO Dolabel210

! If not, allocate a block number from the list, using the identifier.
! First find which of the current NB ranges the identifier is in.
! (The range includes the endpoint but not the starting point)

Iflabel6: &
IF (IND == 0) THEN
  DO J=1,NB
    IF (IDENT(1:4) > BLOKID(J*2-1) .AND.     &
        IDENT(1:4) <= BLOKID(J*2)) EXIT
  END DO

! Then get the corresponding data block number for this index block
! from the map (we can store reports for lots of index blocks at the
! same time, but the list only applies strictly to the latest.
! However, if earlier times already have most stations indexed, this
! shouldn't leave the spread of stations over data blocks too untidy.)
! If there are more ranges than data blocks allocated to this index
! block, then use the last data block found (& split it when full).

  IX=1
  DO I=1,J
    IF (IX > BLOCKS-1-NSQ-XBLOKS) EXIT
    IY=INDEX(MAP(8+XBLOKS+IX:8+BLOCKS-1-NSQ),CHAR(NINDEX(LX)))
    IF (IY == 0) EXIT
    IX=IX+IY
  END DO
  NBLOC=IX-1

! As there is only one report for this station, zero the pointer on
! the end of the report.  (A pointer is record/block, as in the index.)

  POINTER(1:1)=CHAR(0)
  POINTER(2:2)=CHAR(0)
  POINTER(3:3)=CHAR(0)
  POINTER(4:4)=CHAR(0)
END IF Iflabel6
!----------------------------------------------------------------------
!
! If there's room in the block, store the report (after checking for
! duplicates) & index it. Index details are for the latest report,
! those between report & pointer for the report they follow.
!
!----------------------------------------------------------------------
!
! First read in the data block.  (It may have been allocated when the
! index block was started & still be empty - can't tell without read)

300   CONTINUE
Iflabel7: &
IF (NBLOCK /= NBLOC) THEN
  NBLOCK=NBLOC
  REC = 1+XBLOKS+NBLOCK+NSQ
  CALL METDB_CREAD_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (864)

  BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
  NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
  LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))

! If the block has no data for this index period,
! set TAG, NINBLK & LEFT in block header & clear rest of block
! - unless an index entry points to this block, in which case
! the data base has been corrupted.

Iflabel8: &
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

! Work out the displacements corresponding to the record lengths:
! (RECDIS(I) is the start of the i-th report from the end of the block)

    RECDIS(1)=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))
    DO I=2,NINBLK
      RECDIS(I)=RECDIS(I-1)+ICHAR(BLOCK(6+I*2-1:6+I*2-1))*256  &
                     +ICHAR(BLOCK(6+I*2:6+I*2))
    END DO
  END IF Iflabel8
END IF Iflabel7

! If there is an index entry (in which case IND is nonzero),
! check for duplicates, following chain as long as pointer (record
! number - all in same block) is nonzero. Compare the report texts
! and also the hours/minutes that follow (including the cor flag).
! (N.B. There won't be equality unless the lengths are equal.)
!
! First see if the report is out of order (earlier than the latest
! report indexed).  If so, it will be inserted in the chain in time
! order: note the record numbers of the reports before & after it
! in time during the duplicate search & later set the pointers
! accordingly.  (N.B. the new report may be the earliest in the
! chain, in which case only one record number will be set.)
! The hour and minute fields in index entries must be compared
! separately, because the hour may have flags set in the same byte.

Iflabel9: &
IF (IND > 0) THEN
  LASTHR=MOD(ICHAR(INDEKS(IND,LX)(1:1)),32)
  IF (LASTHR > INDHOR .OR.    &
    (LASTHR == INDHOR.AND.INDEKS(IND,LX)(2:2) > ENTRY(2:2))) THEN
    INSERT=.TRUE.
  ELSE
    INSERT=.FALSE.
  END IF

! NAFTER may be set if this is not the latest report in the
! chain, NPREF if there is already a preferred report for this time.
! Use the count in the index entry to control the loop round the chain
! rather than relying on reaching a zero pointer!

  NAFTER=0
  NPREF=0
  NCHAIN=ICHAR(INDEKS(IND,LX)(12:12))

! Loop back to here to look at next report in chain

320 CONTINUE
  POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
  DISPL=BLKSIZ-RECDIS(POINTR)

! IF AN IDENT IN THE CHAIN IS NOT AS IN THE INDEX, SOMETHING'S WRONG!
! (Check that the ident in the index is somewhere in the report:
!  60 is an arbitrary number.)

  IF(INDEX(BLOCK(DISPL+1:DISPL+60),IDENT(1:LIDENT)) == 0)THEN
    PRINT *,MSGHDR,' >>>>>>> DATA BASE INCONSISTENT! <<<<<<<<'
    PRINT *,'INDEX ENTRY FOR ',IDENT,' POINTED TO ',  &
            BLOCK(DISPL+1:DISPL+60)
    RETURN                       ! ????
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

! If same time (first two bytes of index entry), do duplicate check:
! If string of same length, and string itself is same, then duplicate

  LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
  IF (LASTHR == INDHOR .AND.   &
     (BLOCK(DISPL+L-21:DISPL+L-21) == ENTRY(2:2))) THEN
    IF (LN == L-LENBUF-23) THEN
      IF (BULL(1:LN) == BLOCK(DISPL+1:DISPL+LN)) RETURN
    END IF

! See if preferred flag is set (right-hand bit of byte 17 in trailer)
! and if so keep the record number so that the counts can be compared
! and the new report preferred if it is better.

    IF (ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)) >= 128) THEN
      NPREF=RECNO
    END IF
  END IF

  POINTER=BLOCK(DISPL+L-3:DISPL+L)

! If INSERT has been set, go back past at least one report (that pointed
! to by the index entry which set INSERT) until a report for an earlier
! time (or the same time) is found.  Keep the last record number passed.

  IF (INSERT) THEN
    LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
    IF (LASTHR > INDHOR .OR. (LASTHR == INDHOR .AND.   &
        BLOCK(DISPL+L-21:DISPL+L-21) > ENTRY(2:2))) THEN
      NAFTER=RECNO
    ELSE
      INSERT=.FALSE.
    END IF
  END IF

! Loop round unless reports are chained across blocks
! (only 'dreg' chains under a null identifier are likely to be long
! enough to cross blocks.)  At end, reset pointer to start of chain.

  NCHAIN=NCHAIN-1
  IF (POINTER(3:4) == INDEKS(IND,LX)(22:23)   &
      .AND. NCHAIN > 0) GO TO 320
  POINTER=INDEKS(IND,LX)(20:23)
END IF Iflabel9

! If a preferred report for the same time has been found, compare the
! number of good values and the cor number and reset the preferred
! flag if the new report is better.
! Set the "historical" preferred flag (x'20', so add 32), never to be
! unset, to show that the new report has been (or still is) preferred.

Iflabel10: &
IF (IND > 0 .AND. NPREF > 0) THEN
  IF (NPREF == 1) THEN
    DISPL=BLKSIZ-INDLEN
  ELSE
    DISPL=BLKSIZ-RECDIS(NPREF-1)-INDLEN             ! --> TRAILER
  END IF

! If the new report is a COR (or has a higher COR number)
! or has more good values then prefer the new report.

  IF (BLOCK(DISPL+7:DISPL+7) < ENTRY(7:7) .OR.   &  ! COR FLAG
     BLOCK(DISPL+12:DISPL+12) <= ENTRY(12:12)) THEN ! GOOD VALUES
    IFLAGS=ICHAR(BLOCK(DISPL+17:DISPL+17))          ! OLD FLAGS
    BLOCK(DISPL+17:DISPL+17)=CHAR(IFLAGS-128)       ! UNSET PREF
    IFLAGS=ICHAR(ENTRY(17:17))                      ! NEW FLAGS
    IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)   ! PREF
  END IF
ELSE                                                ! IF NO PREF,
  IFLAGS=ICHAR(ENTRY(17:17))                        ! PREFER THIS
  IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)
END IF Iflabel10
!----------------------------------------------------------------------
!
! IF THERE'S ROOM IN THE BLOCK & THE REPORT'S NOT A DUPLICATE, STORE IT.
!
!----------------------------------------------------------------------

Iflabel11: &
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
!----------------------------------------------------------------------
!
! Update this station's index entry to point to this report, if it is
! the latest.  If not, NAFTER is nonzero.  Make the
! NAFTER-th report point to the new one & the new one to
! zero (by taking the pointer that was in the NAFTER-th).
! (or add an entry for this station if there's none in this slot).
!
!----------------------------------------------------------------------
  POINTER(1:1)=CHAR(NINBLK/256)
  POINTER(2:2)=CHAR(MOD(NINBLK,256))
  POINTER(3:3)=CHAR(NBLOCK/256)
  POINTER(4:4)=CHAR(MOD(NBLOCK,256))

! Count of obs is only one byte; leave it as 255 if more than 255!

Iflabel12: &
  IF (IND > 0) THEN
    IF (POINTER < BLOCK(START+L-3:START+L)) THEN
      NOTPTR=ICHAR(BLOCK(START+L-3:START+L-3))*256   &
            +ICHAR(BLOCK(START+L-2:START+L-2))
      PRINT *,MSGHDR,' ******* DATA BASE INCONSISTENT ********'
      PRINT *,NOTPTR,'IS RECORD NUMBER IN POINTER, BUT ONLY'
      PRINT *,NINBLK-1,'OBS IN BLOCK'
      INDEKS(IND,LX)(3:7)=' '    ! ????  RESET ENTRY & REUSE?
      RETURN                     ! ????
    END IF

    NOBS=ICHAR(INDEKS(IND,LX)(12:12))
    IF (NOBS < 255) INDEKS(IND,LX)(12:12)=CHAR(NOBS+1)

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
    END IF
  ELSE

! Or make a new index entry (put IDENT & count of 1 in the input entry)

    NTRIES(LX)=NTRIES(LX)+1
    INDEKS(NTRIES(LX),LX)(1:2)=ENTRY(1:2)
    INDEKS(NTRIES(LX),LX)(3:11)=IDENT
    INDEKS(NTRIES(LX),LX)(12:12)=CHAR(1)
    INDEKS(NTRIES(LX),LX)(13:19)=ENTRY(13:19)
    INDEKS(NTRIES(LX),LX)(20:23)=POINTER

! If this entry needs to start a new index block, it's enough here to
! claim a block number from the overflow pool and list it as the next
! block to continue the index; this number will be set as a pointer
! at the end of the previous block when the index is written back.

    NIND=NTRIES(LX)/NBLIND           ! NO OF FULL INDEX BLOCKS
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
    END IF
  END IF Iflabel12

! update report count in index & copy to map byte for index block
! (use N/256 in case the count is large; add 1 so that the map byte
! is nonzero even if the count is small)

  NREPS(LX)=NREPS(LX)+1
  MAP(8+NINDEX(LX)-1:8+NINDEX(LX)-1)=CHAR(NREPS(LX)/256+1)
ELSE
!----------------------------------------------------------------------
!
! If no room, split the data more or less equally between 2 blocks.
! (Get the second from the overflow pool: because the index operation
! starts further on, INDEX(...) must be adjusted to give block number!)
! For reports in the current block, list station & number of reports,
! sort list by station & split into two with similar numbers of obs.
!
!----------------------------------------------------------------------
  NX=INDEX(MAP(8+BLOCKS-NSQ-OFLOWS:8+BLOCKS-NSQ-1),CHAR(0))
  IF (NX == 0) THEN
    PRINT *,MSGHDR,'NO DATA OVERFLOWS TILL BLOCKS RELEASED'
    RETURN
  END IF
  NBLOCX=BLOCKS-NSQ-OFLOWS-(XBLOKS+1)+NX
  CHARBLX(1:1)=CHAR(NBLOCX/256)
  CHARBLX(2:2)=CHAR(MOD(NBLOCX,256))

  K=0
  NTOTAL=0
  CHARBL(1:1)=CHAR(NBLOCK/256)
  CHARBL(2:2)=CHAR(MOD(NBLOCK,256))
  DO I=1,NTRIES(LX)
   IF (INDEKS(I,LX)(22:23) == CHARBL) THEN
     K=K+1
     SORTX(K)(1:5)=INDEKS(I,LX)(3:7)
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

! Sort counts by identifier & pick the identifier which splits the
! counts evenly (skip this if all the identifiers are the same!)
! (N.B. because of gt.mid later the situation is not symmetrical:
! it's ok if the first identifier is mid but not the last! For MOBILE
! SYNOPS we can't assume that all the items sorted are different, they
! may just have the same first 5 characters.)

  CALL SORTCH(SORTX,5,IDINB,MASK(3:7))
  IF (IDINB == 2) THEN               ! IF ONLY 2, TAKE THE FIRST
    MID=SORTX(1)(1:5)                ! - IF GT.MID AT DO 430!
  ELSE IF (IDINB > 2) THEN
    NOBS=0
    DO I=1,IDINB-1
     NOBS=NOBS+ICHAR(SORTX(I)(6:6))
     IF (NOBS >= NTOTAL/2 .OR.             &
       SORTX(I+1)(1:5) == SORTX(IDINB)(1:5)) EXIT
    END DO
    MID=SORTX(I)(1:5)
  END IF

! MID will be put in the assignment list later. Initialise new block.
! (Can't initialise till now because area used as work space)
! NINBLX & LEFX will be set in BLOCKX before it's written; set TAG now.

  NINBLX=0
  LEFX=BLKSIZ-INDHED
  BLOCKX(1:2)=BLOCK(1:2)
  BLOCKX(7:BLKSIZ)=' '
  MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(NINDEX(LX))

! If the full block has only one ident, it can't be split, so set the
! top bit of the map byte as for index overflow to stop storing in it.

Iflabel13: &
  IF (IDINB == 1) THEN
    MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(128+NINDEX(LX))

! If all the identifiers are the same, just store the report in the
! new block, updating the ranges (adding another with both endpoints
! same, effectively null - the ident's in the index, so the range won't
! be used) & chaining across blocks from this report to the latest in
! the full block. Write the new block and the map block out now; the
! full block is unchanged, but will still be written out at the end.

    NB=NB+1
    MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
    MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
    BLOKID(NB*2-1)=IDENT(1:4)        ! SINGLE ID FOR NEW BLOCK
    BLOKID(NB*2)=IDENT(1:4)

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
!
    REC = 1+XBLOKS+NBLOCX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCKX,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (865)
!
    REC = 1
    CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (866)
  ELSE IF (IDINB > 1) THEN

! Copy obs in 2nd half of list to new block, which means following
! pointer chain for each station (the obs will be stored in reverse
! order - doesn't matter), changing the corresponding index pointers.
! knowing the displacements (RECDIS), we can zero lengths in RECLEN.
!
! Go through the index entries, picking obs in the full block with
! identifiers after the break point for transfer to the new block.
! Zero the lengths of the transferred reports at the start of the
! full block to let pointers to reports left in this block to stay
! the same (pointers in the index and on end of obs).

Dolabel430: &
    DO I=1,NTRIES(LX)
Iflabel14: &
      IF (INDEKS(I,LX)(22:23) == CHARBL .AND.   &
        INDEKS(I,LX)(3:7) > MID) THEN
        RECNO=ICHAR(INDEKS(I,LX)(20:20))*256    &
             +ICHAR(INDEKS(I,LX)(21:21))
        INDEKS(I,LX)(22:23)=CHARBLX
        INDEKS(I,LX)(20:20)=CHAR((NINBLX+1)/256)
        INDEKS(I,LX)(21:21)=CHAR(MOD(NINBLX+1,256))
431     CONTINUE
        DISPL=BLKSIZ-RECDIS(RECNO)

! Store report in second block, zeroing its length in first block.

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

! Follow chain if old pointer is to same block.  Stop if zero pointer
! or different block (copying the pointer if it goes across blocks)

        RECNO=ICHAR(BLOCK(DISPL+L-3:DISPL+L-3))*256   &
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
      END IF Iflabel14
    END DO Dolabel430

! Set number of records & spare bytes in character string,
! write out second data block, then index block with pointers to it.

    BLOCKX(3:3)=CHAR(NINBLX/256)
    BLOCKX(4:4)=CHAR(MOD(NINBLX,256))
    BLOCKX(5:5)=CHAR(LEFX/256)
    BLOCKX(6:6)=CHAR(MOD(LEFX,256))

    REC = 1+XBLOKS+NBLOCX+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCKX,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (867)

! Pointers have been changed, so write the whole index for this time.
! first write any full index blocks

    INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
    INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
    INDHDR(3:3)=CHAR(NTRIES(LX)/256)
    INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
    INDHDR(5:5)=CHAR(NREPS(LX)/256)
    INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

    NFULL=NTRIES(LX)/NBLIND            ! NUMBER OF FULL BLOCKS
    IX=NINDEX(LX)                      ! TO VARY BLOCK IN LOOP
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

! Then the last index block (which may be the only one)

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

! Remake the original data block by going through the reports in order
! & moving only those with non-zero lengths.  (N.B. we must do this
! differently - not following pointer chains as above - to leave the
! zero lengths).  Finally write back the compressed data block.
!
! The old block is compressed in place.  The new block has been written
! out, so its area can be used as work space to copy any report whose
! old & new positions overlap (otherwise characters would be repeated
! from left to right).  If the current report is to be stored in the
! new block, that will be read in again into the buffer called block.

    NSTART=BLKSIZ               ! INITIALISE NEW START
    LN=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))

Dolabel450: &
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
    END DO Dolabel450

    LEFT=NSTART-2*NINBLK-INDHED
    BLOCK(5:5)=CHAR(LEFT/256)
    BLOCK(6:6)=CHAR(MOD(LEFT,256))
    BLOCK(INDHED+2*NINBLK+1:INDHED+2*NINBLK+LEFT)=' '

    REC = 1+XBLOKS+NBLOCK+NSQ
    CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (870)

! Update the list of ranges of identifiers assigned to data blocks,
! adding the second part of the split range to the end of the list.

    IF (LISTAG == CURENT) THEN
      NB=NB+1
      MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
      MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
      DO I=1,NB
       IF (MID(1:4) > BLOKID(I*2-1) .AND.    &
           MID(1:4) <= BLOKID(I*2)) EXIT
      END DO
      BLOKID(NB*2-1)=MID(1:4)
      BLOKID(NB*2)=BLOKID(I*2)
      BLOKID(I*2)=MID(1:4)
    END IF

! Finally write back the map block & go back to store the report

    REC = 1
    CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
    IF (IORC /= 0) CALL SYSABN (871)
    GO TO 200
  END IF Iflabel13
END IF Iflabel11
!----------------------------------------------------------------------
!
! Write back to data set in following order:
!      (the order matters because  a system failure after 1 or 2 of the
!        3 writes could leave the data base in an inconsistent state.)
!**** data block (D), map of blocks in use (M), index for hour (I) *****
! (Arguing as follows: better D but not I, data stored but inaccessible,
!  than I but not D, index entry for lost data; better D but not M, data
!  stored but may be overwritten because block not claimed in map, than
!  M but not D, block tied up but data lost; better M but not I, block
!  tied up but data inaccessible because no index entry, than I but not
!  M, index points to data which may be lost because block reclaimed.
!
!----------------------------------------------------------------------

BLOCK(3:3)=CHAR(NINBLK/256)
BLOCK(4:4)=CHAR(MOD(NINBLK,256))
BLOCK(5:5)=CHAR(LEFT/256)
BLOCK(6:6)=CHAR(MOD(LEFT,256))

REC = 1+XBLOKS+NBLOCK+NSQ
CALL METDB_CWRITE_DIR(IFT,BLOCK,BLKSIZ,REC,IORC)
IF (IORC /= 0) CALL SYSABN (872)

! Write the map block back only if a new data or index block has been
! claimed - or if the number of reports for this index block is N*256.

IF (NINBLK == 1 .OR. NTRIES(LX)-(NTRIES(LX)/NBLIND)*NBLIND == 1 &
                .OR. NREPS(LX) == (NREPS(LX)/256)*256) THEN
  REC = 1
  CALL METDB_CWRITE_DIR(IFT,MAP,BLKSIZ,REC,IORC)
  IF (IORC /= 0) CALL SYSABN (873)
END IF

!----------------------------------------------------------------------
! Finally the index.  If it extends over more than one block, there
! may be no need to write everything.  Only one entry has been changed
! or added, so we must write the block with that entry in.  An addition
! changes the count, so write the first block (though every block has
! the count in, it's actually read from the last) and if a new entry
! starts a new block, write the block before with its pointer set.
! Hence the following structure:
!          work out the number of full blocks.                  (NFULL)
!          if an existing entry was updated, then               (IND>0)
!            find which block the entry is in.                  (NIND)
!            if in a full block (not the last) write the full block.
!            if it is in the last block, write the last block.
!          else (if an entry was added)
!            if there is a full block, write the first block.
!            if there is more than one full block, then
!              if this is the first entry in a block, then
!                write the last full block.
!            write the last block (not full) with the new entry in.
!----------------------------------------------------------------------

INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
INDHDR(3:3)=CHAR(NTRIES(LX)/256)
INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
INDHDR(5:5)=CHAR(NREPS(LX)/256)
INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

NFULL=NTRIES(LX)/NBLIND                ! NUMBER OF FULL BLOCKS
Iflabel15: &
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
                                       ! IF ENTRY ADDED,
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
                                       ! WRITE LAST FULL BLOCK
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
!
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

END IF Iflabel15
IF (IORC /= 0) CALL SYSABN (876)

RETURN
END SUBROUTINE MOBSTO
