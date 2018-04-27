SUBROUTINE TAFREP (DATIME, ENTRY, BULL, IFT, BLKSIZ, IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TAFREP
!
! PURPOSE       : STORE 23-BYTE CHAINED ENTRIES IN THE MDB
!
! DATA TYPES    : TAFS,METARS,U/A,SAMOSX,SREWS,NCM,CLIMAT,
!                 BOGUS,ENHSAWS,HRR,TBUS,TRACKOB
!
! CALLED BY     : TAFIND,UAEDIT,SRWBUL,NCMBUL,CLMBUL,
!                 BOGIND,ENHBUL,HRRBUL,TBUSBUL,TRKSTO
!                                (SAMBUL CALLS TAFIND)
!
! CALLS         : DATE31,SORTCH,ICOBRV,INDLALO,UADUPS, CENTURY
!                 METDB_CREAD_DIR & METDB_CWRITE_DIR in MetDB_c_utils.c
!
! ARGUMENTS     : (1) DATE/TIME (YEAR, MONTH...)
!                 (2) TRAILER (WITHOUT TIMES & BLOCK/RECORD NO & WITH
!                      ENTRY(3:11)=TTAAII(3:6)
!                 (3) REPORT TO BE STORED
!                 (4) FT NUMBER (FROM TT IN BULLETIN HEADING)
!                     (3:TAF & 7:ATAFS in MDBTAFS;
!                      4:METAR in MDBMETAR; 21:U/A in MDBUAIR;
!                      others assigned by storage job MDBOTHRS)
!                 (5) BLOCKSIZE OF OUTPUT DATA SET
!                 (6) IDENTIFIER TO REPLACE TTAAII ETC IN INDEX ENTRY
!                     (WITH BYTE 17 FLAGS AS BYTE 10 FOR U/A)
!
! REVISION INFO :
!
! $Workfile: tafrep.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 07/03/2011 10:05:51$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         07/03/2011 10:05:51    Brian Barwell
!       Modified for C I/O.
!  4    MetDB_Refresh 1.3         14/01/2011 10:12:47    Brian Barwell   Work
!       arrays introduced to overcome problems with calls to SORTCH.
!  3    MetDB_Refresh 1.2         07/01/2011 09:47:37    John Norton     Post
!       MDBSTOR batch 4 porting. Updates for differing ranks
!  2    MetDB_Refresh 1.1         07/01/2011 09:42:39    John Norton
!       Original f77 pre-porting version!
!  1    MetDB_Refresh 1.0         10/12/2010 16:42:02    John Norton     After
!       MDBSTOR batch 4 porting.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE century_mod
USE icobrv_mod
USE indlalo_mod
USE sortch_mod
USE uadups_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: DATIME(5) !a1
CHARACTER(LEN=*), INTENT(INOUT) :: ENTRY     !a2
CHARACTER(LEN=*), INTENT(IN)    :: BULL      !a3
INTEGER,          INTENT(IN)    :: IFT       !a4
INTEGER,          INTENT(IN)    :: BLKSIZ    !a5
CHARACTER(LEN=*), INTENT(INOUT) :: IDENT     !a6

! Local declarations:

INTEGER, PARAMETER :: INDHED = 6     ! Index header length = 6 bytes
INTEGER, PARAMETER :: INDLEN = 23    ! Index entry length = 23 bytes
INTEGER, PARAMETER :: MAXLEN = 27998 ! Max. record length = 27998 bytes
INTEGER, PARAMETER :: MAXOFL = 7     ! Max. overflow index records = 7
INTEGER, PARAMETER :: MAXIND = MAXOFL*MAXLEN/INDLEN ! Max. index entries

!-----------------------------------------------------------------------
! declare integer variables
!-----------------------------------------------------------------------

INTEGER          ::  AMSTAR
INTEGER          ::  BLKTAG
INTEGER          ::  BLKTAX
INTEGER          ::  BLOCKS
INTEGER          ::  CENDAY
INTEGER          ::  CENTHR
INTEGER          ::  CURENT
INTEGER          ::  DISPL
INTEGER          ::  HOURNOW
INTEGER          ::  I
INTEGER          ::  IBIT
INTEGER          ::  IBITS
INTEGER          ::  IBUF     ! Location in BUFFER
INTEGER          ::  IDINB
INTEGER          ::  IFLAGS
INTEGER          ::  IHT      ! Station height from ICAO list
INTEGER          ::  IJBITS
INTEGER          ::  IND
INTEGER          ::  INDENT
INTEGER          ::  INDHOR
INTEGER          ::  INDXFT=0
INTEGER          ::  IORC
INTEGER          ::  ISTART
INTEGER          ::  ITOR
INTEGER          ::  ITPBITS
INTEGER          ::  IX
INTEGER          ::  IY
INTEGER          ::  J
INTEGER          ::  JBIT
INTEGER          ::  JBITS
INTEGER          ::  JRANGE       ! ident of new ob was in this range
INTEGER          ::  JTPBITS
INTEGER          ::  K
INTEGER          ::  L
INTEGER          ::  LASTHR
INTEGER          ::  LASTIX
INTEGER          ::  LATEST=0
INTEGER          ::  LEFT
INTEGER          ::  LEFX
INTEGER          ::  LENBUF
INTEGER          ::  LL
INTEGER          ::  LN
INTEGER          ::  LOB
INTEGER          ::  MAPFT=0
INTEGER          ::  MESSFT=0
INTEGER          ::  N
INTEGER          ::  NAFTER
INTEGER          ::  NB
INTEGER          ::  NBEFORE
INTEGER          ::  NBLIND
INTEGER          ::  NBLOC
INTEGER          ::  NBLOCX
INTEGER          ::  NBLOCK
INTEGER          ::  NFULL
INTEGER          ::  NIBL
INTEGER          ::  NINBLK
INTEGER          ::  NINBLX
INTEGER          ::  NIND
INTEGER          ::  NINDEX
INTEGER          ::  NL
INTEGER          ::  NMAX
INTEGER          ::  NOB
INTEGER          ::  NOBS
INTEGER          ::  NOFLOW(MAXOFL)
INTEGER          ::  NOLD
INTEGER          ::  NOLDIX
INTEGER          ::  NOW(8)
INTEGER          ::  NPREF
INTEGER          ::  NREP
INTEGER          ::  NREPS
INTEGER          ::  NSEQBL
INTEGER          ::  NSTART
INTEGER          ::  NSQ
INTEGER          ::  NTOTAL
INTEGER          ::  NTRIES
INTEGER          ::  NUMREC   ! Record number for direct access I/O
INTEGER          ::  NX
INTEGER          ::  NXBLOK
INTEGER          ::  OFLOWS
INTEGER          ::  POINTB
INTEGER          ::  POINTR
INTEGER          ::  RANGE_CENTHR ! century-hour for ranges in map      c
INTEGER          ::  RC
INTEGER          ::  RECDIS(14000)

! RECLEN is the array of report length slots after the block header.
! its dimension is arbitrary, it only needs to be big enough for the
! reports that will fit in a block.  we don't know where the lengths
! will end and the reports, slotted in from the end, will meet them!

INTEGER          ::  RECLEN(14000)
INTEGER          ::  RECNO
INTEGER          ::  SLODAY
INTEGER          ::  SLOTHR
INTEGER          ::  START
INTEGER          ::  TIMTAG       ! Time tag in record (256*day + hour)
INTEGER          ::  X            ! unused o/p from DATE13
INTEGER          ::  XBLOKS       ! Number of index records in data set
INTEGER          ::  XHOURS       ! Number of hours in index period
INTEGER          ::  Y            ! unused o/p from DATE13
INTEGER          ::  ZEROBL       ! block number to free by zero in map

!-----------------------------------------------------------------------
! declare logical variables
!-----------------------------------------------------------------------

LOGICAL          ::  FIRST=.TRUE. !- true if first call to routine.
LOGICAL          ::  INSERT

!-----------------------------------------------------------------------
! declare real variables
!-----------------------------------------------------------------------

REAL             ::  POSITION(2)  ! Observation latitude & longitude

!-----------------------------------------------------------------------
! declare character variables
!-----------------------------------------------------------------------

CHARACTER(LEN=MAXLEN) ::  BLOCK
CHARACTER(LEN=4)      ::  BLOKID(1000)
CHARACTER(LEN=MAXLEN) ::  BLOCKX
CHARACTER(LEN=MAXLEN) ::  BUFFER   ! Buffer for direct access I/O
CHARACTER(LEN=4)      ::  BUFR
CHARACTER(LEN=2)      ::  CHARBL
CHARACTER(LEN=2)      ::  CHARBLX
CHARACTER(LEN=4)      ::  CH4      ! Variable for use in TRANSFERs
CHARACTER(LEN=23)     ::  EMPTYX=' '
CHARACTER(LEN=23)     ::  INDEKS(MAXIND) ! index entries
CHARACTER(LEN=6)      ::  INDHDR
CHARACTER(LEN=2)      ::  INDOVR
CHARACTER(LEN=9)      ::  LASTID
CHARACTER(LEN=MAXLEN) ::  MAP
CHARACTER(LEN=8)      ::  MAPHDR
CHARACTER(LEN=4)      ::  MAPOVR
CHARACTER(LEN=23)     ::  MASK
CHARACTER(LEN=23)     ::  MASX
CHARACTER(LEN=1)      ::  MEDIAN
CHARACTER(LEN=9)      ::  MID
CHARACTER(LEN=4)      ::  POINTER
CHARACTER(LEN=1)      ::  QUARTL
CHARACTER(LEN=1)      ::  WORK1(128)     ! C*1 work array (for map sort)
CHARACTER(LEN=10)     ::  WORK10(MAXLEN/10) ! C*10 work array (ID sort)

!-----------------------------------------------------------------------
! dynamic common and SAVE statement
!-----------------------------------------------------------------------

COMMON /MDBUFS/ MAP, RECLEN, BLOCK, BLOCKX, INDEKS, RECDIS, BLOKID, &
                WORK1, WORK10
SAVE

!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

DATA MASK/'  XXXXXXXXXX           '/ !TO SORT ON IDENT
DATA MASX/'           X           '/ !TO SORT ON COUNT

!-----------------------------------------------------------------------
! Initialise 'BUFR' in ASCII on first call only.
!-----------------------------------------------------------------------

IF (FIRST) THEN
  BUFR = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)
  FIRST=.FALSE.
END IF

!**********************************************************************
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
!**********************************************************************

CALL DATIM(NOW)                            ! Current date and time
CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)   ! Current century day
HOURNOW=(CENDAY-1)*24+NOW(5)               ! Current century hour

IFLABEL1: &
IF (IFT /= MAPFT) THEN
  NUMREC = 1
  CALL METDB_CREAD_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(901)
  MAPFT=IFT

  MAPHDR = BUFFER(1:8)
  BLOCKS=ICHAR(MAPHDR(1:1))*256+ICHAR(MAPHDR(2:2))
  XBLOKS=ICHAR(MAPHDR(3:3))*256+ICHAR(MAPHDR(4:4))
  XHOURS=ICHAR(MAPHDR(5:5))*256+ICHAR(MAPHDR(6:6))
  AMSTAR=ICHAR(MAPHDR(7:7))*256+ICHAR(MAPHDR(8:8))

  MAP(1:BLKSIZ-4020) = BUFFER(9:BLKSIZ-4012)
  IBUF = BLKSIZ - 4011
  RANGE_CENTHR = TRANSFER(BUFFER(IBUF:IBUF+3),RANGE_CENTHR)
  NB = TRANSFER(BUFFER(IBUF+4:IBUF+7),NB)
  IBUF = IBUF + 8
  DO I=1,1000
    BLOKID(I) = BUFFER(IBUF:IBUF+3)
    IBUF = IBUF + 4
  END DO

  MAPOVR = BUFFER(IBUF:IBUF+3)
  OFLOWS=ICHAR(MAPOVR(1:1))*256+ICHAR(MAPOVR(2:2))
  INDENT=ICHAR(MAPOVR(3:3))*256+ICHAR(MAPOVR(4:4))

  NSEQBL=ICHAR(MAP(BLOCKS:BLOCKS))
  IF(NSEQBL > 0)THEN
    NSQ=1
  ELSE
    NSQ=0
  END IF

  NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
  IF (LATEST == 0) LATEST=HOURNOW      ! LATEST CENTURY-HOUR
END IF IFLABEL1

!**********************************************************************
! COMPLETE TIME FIELDS IN INDEX ENTRY (TIME & TIME OF RECEIPT),
! FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.
! TIME OF RECEIPT IS IN MINUTES FROM THE SLOT TIME (HOURNOW-SLOTHR,
! WHERE HOURNOW IS THE CURRENT CENTURY-HOUR) AND CAN BE <0 OR >1440.
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
ENTRY(18:18)=CHAR(ITOR/256)            ! HALFWORD IN ENTRY
ENTRY(19:19)=CHAR(MOD(ITOR,256))       ! HALFWORD IN ENTRY

! IF THE DATA IS FOR TOO LONG BEFORE THE THE MOST RECENT DATA STORED
! ('TOO LONG' MEANS MORE THAN THE PERIOD COVERED BY THE DATA BASE),
! THEN REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH OLD DATA!).
! IF THE NEW DATA IS MORE RECENT, UPDATE THE LATEST HOUR - AND THE
! INDEX BLOCK TAG FOR WHICH THE LIST OF RANGES CAN BE UPDATED (SET
! WHEN A NEW INDEX BLOCK IS STARTED, BUT THIS COPES UNTIL THEN...).

IF (CENTHR <= LATEST-(XBLOKS-1)*XHOURS) THEN
  PRINT *,LATEST-CENTHR,'HOURS OLD   ', BULL(:MIN(60,LEN(BULL)))
  RETURN
END IF

IF (CENTHR > LATEST) LATEST = CENTHR

!**********************************************************************
! THE INDEX IS DIVIDED INTO N-HOURLY SEGMENTS. WORK OUT WHICH SEGMENT
! FROM THE CENTURY-HOUR AND READ IN THE CORRESPONDING INDEX BLOCK.
!
! INDEX BLOCK:
! ------------------------------------------------- - - - - - ---------
! : DATE/ : NO. OF : NO. OF  : 23-BYTE : 23-BYTE :             : OVER :
! : TIME  : ENTRIES: REPORTS :  ENTRY  :  ENTRY  : ENTRIES...  : FLOW :
! ------------------------------------------------- - - - - - ---------
! 0       2        4         6        29        52         LAST 2 BYTES
!**********************************************************************

NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2 ! index block no.
IFLABEL2: &
IF (IFT /= INDXFT .OR. NINDEX /= NXBLOK) THEN
  NINDEX=NXBLOK
  IX=NINDEX                            ! TO VARY IN READ LOOP
  NIND=1                               ! CONTINUATION NUMBER
30 NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
  NUMREC = IX+NSQ
  CALL METDB_CREAD_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(902)

  INDHDR = BUFFER(1:6)
  TIMTAG=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
  NTRIES=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
  NREPS =ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))

  IBUF = 7
  DO I=NIBL+1,NIBL+NBLIND
    INDEKS(I) = BUFFER(IBUF:IBUF+22)
    IBUF = IBUF + 23
  END DO

  INDOVR = BUFFER(IBUF:IBUF+1)
  NOFLOW(NIND)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

  INDXFT=IFT
  IF (NOFLOW(NIND) > 0) THEN           ! INDEX OVERFLOW?
    IX=NOFLOW(NIND)                    ! IF SO, RESET BLOCK NO
    NIND=NIND+1                        ! FURTHER CONTINUATION
    IF(NIND > MAXOFL)THEN
      PRINT*,' MAX INDEX OVERFLOW REACHED',IFT,'IS FT NUMBER'
      RETURN
    END IF
    GO TO 30                           ! & READ MORE ENTRIES.
  END IF
END IF IFLABEL2

!**********************************************************************
! THE CODE THAT FOLLOWS DIFFERS FROM ERSREP IN TWO IMPORTANT WAYS:    *
! 1) AN INDEX ENTRY POINTS NOT TO A SINGLE REPORT, BUT TO A SET OF    *
!    CHAINED REPORTS FOR THE SAME AIRFIELD                            *
! 2) REPORTS ARE NOT STORED SEQUENTIALLY, BUT IN SUCH A WAY THAT      *
!    REPORTS FROM THE SAME AIRFIELD STAY IN THE SAME BLOCK, AND       *
!    AIRFIELDS IN THE SAME REGION STAY CLOSE TOGETHER.  THIS IS       *
!    DONE BY ASSIGNING A RANGE OF AIRFIELD IDENTIFIERS TO A BLOCK     *
!    ON THE BASIS OF THE NUMBERS OF REPORTS RECEIVED IN THE PERIOD    *
!    COVERED BY THE LAST INDEX BLOCK.                                 *
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
! 200 IF (AIRFIELD IS INDEXED) THEN                                   *
!         210 ALLOCATE SAME DATA BLOCK & CHAIN POINTERS;              *
!             KEEP NUMBER OF INDEX ENTRY FOR USE LATER.               *
!     ELSE                                                            *
!         220 FIND WHICH RANGE THE IDENTIFIER IS IN                   *
!         230 & FIND THE CORRESPONDING BLOCK NUMBER FROM THE MAP;     *
!             SET THE POINTER TO ZERO.                                *
!         240 FIND THE POSITION EITHER FROM THE LIST MADE AT THE START*
!             OR BY LOOKING UP STATION MASTER.                        *
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

IFLABEL3: &
IF (TIMTAG /= CURENT) THEN
  ZEROBL=1
110 N=INDEX(MAP(XBLOKS+ZEROBL:BLOCKS-1-NSQ),CHAR(NINDEX))
  IF (N > 0) THEN
    ZEROBL=ZEROBL+(N-1)
    MAP(XBLOKS+ZEROBL:XBLOKS+ZEROBL)=CHAR(0)
    IF (1+XBLOKS+ZEROBL < BLOCKS) GO TO 110
  END IF
!                      NOW DO SAME FOR INDEX OVERFLOWS (TOP BIT SET)
  ZEROBL=1
111 N=INDEX(MAP(XBLOKS+ZEROBL:BLOCKS-1-NSQ),CHAR(128+NINDEX))
  IF (N > 0) THEN
    ZEROBL=ZEROBL+(N-1)
    MAP(XBLOKS+ZEROBL:XBLOKS+ZEROBL)=CHAR(0)
    IF (1+XBLOKS+ZEROBL < BLOCKS) GO TO 111
  END IF

! CHOOSE A PREVIOUS INDEX BLOCK TO ALLOCATE AIRFIELDS TO DATA BLOCKS.
! FIND THE BIGGEST COUNT IN INDEX BLOCKS FOR THE SAME TIME (THE MOST
! RECENT IF SEVERAL HAVE THE SAME ROUGH COUNT IN THE MAP)

  N=XBLOKS
  NMAX=0                               ! MAX COUNT
  DO I=1,XBLOKS*XHOURS/24              ! I: DAYS BACK
    NOLDIX=MOD((CENTHR-I*24-INDHOR)/XHOURS,N)+2 ! OLD INDEX BLOCK
    NOLD=ICHAR(MAP(NOLDIX-1:NOLDIX-1)) ! COUNT FROM MAP
    IF (NOLD > NMAX) THEN              ! IF COUNT IS BIGGER,
      NMAX=NOLD                        ! KEEP COUNT TO CHECK
      LASTIX=NOLDIX                    ! & KEEP BLOCK NUMBER
    END IF                             ! TO READ FOR DISTRIBUTION
  END DO

! IF ALL INDEX BLOCKS FOR THE SAME TIME HAVE ZERO COUNTS, JUST WORK
! BACK AN INDEX BLOCK AT A TIME,
! AVOIDING LOW TOTALS & VERY HIGH ONES BY TAKING A BLOCK WITH A TOTAL
! BETWEEN THE MEDIAN & THE UPPER QUARTILE

IFLABEL4: &
  IF (NMAX == 0) THEN
    DO I=1,N
      WORK1(I) = MAP(I:I)
    END DO
    CALL SORTCH (WORK1, 1, N, MASK(3:3))
    MEDIAN = WORK1((N+1)/2)               ! Middle count after sort
    QUARTL = WORK1(3*(N+1)/4)             ! Third quartile after sort

IFLABEL5: &
    IF (MEDIAN == CHAR(0)) THEN        ! USE MAX IF ZERO MEDIAN
      LASTIX = 1 + INDEX(MAP(1:N),WORK1(N))
    ELSE
      I=1                              ! BLOCKS TO GO BACK
120   J=NINDEX-I
      IF (J <= 1) J=XBLOKS+J           ! WRAPPING ROUND IF NEEDED
      IF (J <= 1 .OR.               &  ! IF TOO FEW INDEX BLOCKS
        MAP(J-1:J-1) < MEDIAN .OR. &   ! OR COUNT TOO SMALL
        MAP(J-1:J-1) > QUARTL) THEN    ! OR COUNT TOO BIG,
        I=I+1                          ! GO BACK A(NOTHER) BLOCK
        IF (I <= XBLOKS) GO TO 120     ! IF ANY NOT YET TRIED...
      END IF                           ! N.B. J=1:N, NINDEX=2:N+1
      LASTIX=J                         ! INDEX BLOCK TO USE
    END IF IFLABEL5
  END IF IFLABEL4
!
! READ IN THE INDEX BLOCK(S) FOR THE PERIOD CHOSEN ABOVE FOR ITS COUNT.
!
  IX=LASTIX                            ! TO VARY IN READ LOOP
  NIND=1                               ! CONTINUATION NUMBER
130 NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
  NUMREC = IX+NSQ
  CALL METDB_CREAD_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(903)

  INDHDR = BUFFER(1:6)
  TIMTAG=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
  NTRIES=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
  NREPS =ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))

  IBUF = 7
  DO I=NIBL+1,NIBL+NBLIND
    INDEKS(I) = BUFFER(IBUF:IBUF+22)
    IBUF = IBUF + 23
  END DO

  INDOVR = BUFFER(IBUF:IBUF+1)
  NOFLOW(NIND)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

  IF (NOFLOW(NIND) > 0) THEN           ! INDEX OVERFLOW?
    IX=NOFLOW(NIND)                    ! IF SO, RESET BLOCK NO
    NIND=NIND+1                        ! FURTHER CONTINUATION
    IF(NIND > MAXOFL)THEN
      PRINT*,' MAX INDEX OVERFLOW REACHED',IFT,'IS FT NUMBER'
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
! Set start of first range and start of last to hex zeros & hex
! ones (255 in each character), the maximum possible range.
!
! N.B. BLOKID IS A LIST OF PAIRS OF IDENTIFIERS DEFINING RANGES RATHER
! THAN SINGLE IDENTIFIERS AS BREAK POINTS. THIS MAKES IT EASIER TO UP-
! DATE: IF A RANGE IS SPLIT THE SECOND HALF CAN JUST BE PUT AT THE END.

  NB=(BLOCKS-1-NSQ-XBLOKS-OFLOWS)/XBLOKS
  IF (NTRIES == 0) NB=1
  BLOKID(1)=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
  BLOKID(NB*2)=CHAR(255)//CHAR(255)//CHAR(255)//CHAR(255)

  IX=INDEX(MAP(XBLOKS+1:BLOCKS-1-NSQ),CHAR(0))  ! GET FIRST
  MAP(XBLOKS+IX:XBLOKS+IX)=CHAR(NINDEX)         ! DATA BLOCK

IFLABEL6: &
  IF (NB > 1) THEN
    L=INDENT
    N=NTRIES
    CALL SORTCH(INDEKS(1),L,N,MASX)    ! SORT IN COUNT ORDER

! THE SORT PUTS ENTRIES WITH BIG COUNTS AT THE END OF THE LIST.
! ANY IDENT WITH MORE THAN NREPS/NB WILL BE ASSIGNED A WHOLE BLOCK.
! THE REST WILL BE RE-SORTED IN IDENT ORDER & SHARED OUT BETWEEN
! THE REMAINING BLOCKS.
! There's a problem here: ranges are defined in terms of the first four
! characters of the identifier, as TAFREP was developed for TAF/METARs,
! so for other data types identifiers with the same 4 letters may give
! null ranges (start=end).  No data will be stored in data blocks with
! null ranges, hence wasted space.  We could (with a lot of extra code)
! recognise that a full data block has a range that is followed by a
! null range and overflow or split into that block as into an overflow.
! Alternatively we could omit null ranges from the list & claim extra
! blocks from before the overflow pool as needed.  But both solutions
! mean further complications, probably only for the sake of CLIMAT data
! - not worth it?                                       (CL, Dec 98)

    NL=N                               ! NUMBER OF ENTRIES
    LOB=NREPS/NB                       ! MEAN OBS PER BLOCK
135 NREP=ICHAR(INDEKS(NL)(12:12))      ! OBS FOR LAST IDENT
    IF (NREP > LOB) THEN               ! IF COUNT > MEAN,
      NL=NL-1                          ! LEAVE ENTRY AT END
      NREPS=NREPS-NREP                 ! & IGNORE ITS COUNT.
      IF (NL > 1) GO TO 135            ! BACK PAST BIG COUNTS
    END IF
!                                      ! SORT SMALLER COUNTS
    CALL SORTCH(INDEKS(1),L,NL,MASK)   ! IN ORDER OF IDENT
!                                      ! & RECALCULATE MEAN
    LOB=NREPS/(NB-(N-NL))              ! IGNORING BIG COUNTS.
    K=1
    NOB=0
    DO I=1,NTRIES
      NOB=NOB+ICHAR(INDEKS(I)(12:12))
      IF (NOB >= LOB*K .AND. K < NB) THEN
        BLOKID(K*2)=INDEKS(I)(3:6)     ! END OF ONE RANGE
        BLOKID(K*2+1)=INDEKS(I)(3:6)   ! START OF NEXT - SAME
        K=K+1                          ! ONE MORE RANGE DONE
        IX=INDEX(MAP(XBLOKS+1:BLOCKS-1-NSQ),CHAR(0))
        MAP(XBLOKS+IX:XBLOKS+IX)=CHAR(NINDEX)
      END IF
    END DO
  END IF IFLABEL6

! FINALLY REINITIALISE THE INDEX BUFFER FOR THE NEW HOUR.
! KEEP THE TIME TAG FOR THIS LIST IN CASE A BLOCK IS SPLIT: THE RANGES
! WILL ONLY BE UPDATED FOR DATA WITH THIS TAG, FOR THE LATEST PERIOD.

  TIMTAG=CURENT
  NTRIES=0
  NREPS=0
  NOFLOW(1)=0

  RANGE_CENTHR=CENTHR
END IF IFLABEL3

!**********************************************************************
! IF THE INDEX IS NOT EMPTY (IF DATA ALREADY STORED FOR THIS HOUR), SEE
! IF THERE IS ALREADY AN ENTRY FOR THIS STATION.  IF SO, TRY TO STORE
! THE REPORT IN THE SAME DATA BLOCK.  MAKE THE INDEX ENTRY POINT TO THE
! NEW REPORT, AND SET THE POINTER ON THE END OF THIS REPORT TO POINT TO
! THE PREVIOUS REPORT, I.E. COPY THE OLD POINTER FROM THE INDEX.  ADD 1
! TO THE NUMBER OF REPORTS CHAINED TO THIS INDEX ENTRY.
!**********************************************************************

! BRANCH BACK TO 200 TO STORE OB IF BLOCK WAS FULL & HAD TO BE SPLIT

200 IND=0
IFLABEL7: &
IF (IFT == 21) THEN
  CALL UADUPS(INDEKS,NTRIES,IDENT,ENTRY,IND)
  IF (IND > 0) POINTER=INDEKS(IND)(20:23)
ELSE
  J = LEN(IDENT)     ! Length of identifier
  J = MIN0(J,9)      ! Don't use >9 characters
  DO I=1,NTRIES
    IF (INDEKS(I)(3:J+2)  ==  IDENT(1:J)) THEN
      POINTER=INDEKS(I)(20:23)
      IND=I
    END IF
  END DO
END IF IFLABEL7

POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))
NBLOC=POINTB

! IF NOT, ALLOCATE A BLOCK NUMBER FROM THE LIST, USING THE IDENTIFIER.
! FIRST FIND WHICH OF THE CURRENT NB RANGES THE IDENTIFIER IS IN.
! (THE RANGE INCLUDES THE ENDPOINT BUT NOT THE STARTING POINT)

IFLABEL8: &
IF (IND == 0) THEN
  DO J=1,NB
    IF (IDENT(1:4) > BLOKID(J*2-1) .AND. &
        IDENT(1:4) <= BLOKID(J*2)) GO TO 221
  END DO
221 IX=1
  JRANGE=J

! THEN GET THE CORRESPONDING DATA BLOCK NUMBER FOR THIS INDEX BLOCK
! FROM THE MAP (WE CAN STORE REPORTS FOR LOTS OF INDEX BLOCKS AT THE
! SAME TIME, BUT THE LIST ONLY APPLIES STRICTLY TO THE LATEST.
! HOWEVER, IF EARLIER TIMES ALREADY HAVE MOST STATIONS INDEXED, THIS
! SHOULDN'T LEAVE THE SPREAD OF STATIONS OVER DATA BLOCKS TOO UNTIDY.)
! IF THERE ARE MORE RANGES THAN DATA BLOCKS ALLOCATED TO THIS INDEX
! BLOCK, THEN USE THE LAST DATA BLOCK FOUND (& SPLIT IT WHEN FULL).

  DO I=1,J
    IF (IX > BLOCKS-1-NSQ-XBLOKS) GO TO 231
    IY=INDEX(MAP(XBLOKS+IX:BLOCKS-1-NSQ),CHAR(NINDEX))
    IF (IY == 0) GO TO 231
    IX=IX+IY
  END DO
231 NBLOC=IX-1

! AS THERE IS ONLY ONE REPORT FOR THIS AIRFIELD, ZERO THE POINTER ON
! THE END OF THE REPORT.  (A POINTER IS RECORD/BLOCK, AS IN THE INDEX.)

  POINTR=0
  POINTB=0
  POINTER=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)

! TAFS & METARS ONLY: SEE IF A MISSING POSITION IS IN THE LIST MADE AT
! THE START, PUT LAT/LONG IN ENTRY IF IT'S THERE OR IN STATION MASTER.

!----------------------------------------------------------------------
! For TAFS/METARS always call the ICAO abbreviated list ICOBRV.
!----------------------------------------------------------------------

  IF (IFT == 3 .OR. IFT == 4 .OR. IFT == 7) THEN
    CALL ICOBRV (IDENT(1:4), POSITION, IHT, RC)
    IF (RC  /=  0) THEN
      PRINT *,'TAFREP: Failed to find ',IDENT(1:5),' in ICAO List'
    ELSE
      CALL INDLALO(ENTRY,POSITION(1),POSITION(2))
    END IF
  END IF
END IF IFLABEL8

!**********************************************************************
! IF THERE'S ROOM IN THE BLOCK, STORE THE REPORT (AFTER CHECKING FOR
! DUPLICATES) & INDEX IT. INDEX DETAILS ARE FOR THE LATEST REPORT,
! THOSE BETWEEN REPORT & POINTER FOR THE REPORT THEY FOLLOW.
!**********************************************************************
!
! FIRST READ IN THE DATA BLOCK.  (IT MAY HAVE BEEN ALLOCATED WHEN THE
! INDEX BLOCK WAS STARTED & STILL BE EMPTY - CAN'T TELL WITHOUT READ)
!
! ABEND rather than just issue warning if tags don't match and
! index entry found, in which case they should match.
! It may be a variable in core that's wrong, not the data set.
! If so, carrying on could corrupt a data set that's still OK.

300 CONTINUE
IFLABEL9: &
IF (IFT /= MESSFT .OR. NBLOCK /= NBLOC) THEN
  NBLOCK=NBLOC
  IF (NBLOCK <= 0) THEN
    PRINT *,'TAFREP: data block number not positive',NBLOCK
    CALL SYSABN(904)
  END IF

  NUMREC = 1+XBLOKS+NBLOCK+NSQ
  CALL METDB_CREAD_DIR (IFT, BLOCK(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(905)
  MESSFT=IFT

  BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
  NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
  LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))

  DO I=1,NINBLK
    RECLEN(I)=ICHAR(BLOCK(6+I*2-1:6+I*2-1))*256 &
             +ICHAR(BLOCK(6+I*2:6+I*2))
  END DO

IFLABEL10: &
  IF (NINBLK == 0 .OR. BLKTAG /= CURENT) THEN
    IF (IND > 0) THEN
      PRINT *,'TAFREP: index points to data block that is'
      PRINT *,'unstarted',INDEKS(IND),NINBLK,BLKTAG,CURENT
      PRINT *,IFT,'is FT number'
      CALL SYSABN(906)
    ELSE
      NINBLK=0
      BLKTAG=CURENT
      LEFT=BLKSIZ-INDHED
      BLOCK(:BLKSIZ)=' '
    END IF
  ELSE

! WORK OUT THE DISPLACEMENTS CORRESPONDING TO THE RECORD LENGTHS:
! (RECDIS(I) IS THE START OF THE I-TH REPORT FROM THE END OF THE BLOCK)

    RECDIS(1)=RECLEN(1)
    DO I=2,NINBLK
      RECDIS(I)=RECDIS(I-1)+RECLEN(I)
    END DO
  END IF IFLABEL10
END IF IFLABEL9

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

IFLABEL11: &
IF (IND > 0) THEN
  LASTHR=MOD(ICHAR(INDEKS(IND)(1:1)),32)
  IF (LASTHR > INDHOR .OR. &
    (LASTHR == INDHOR .AND. INDEKS(IND)(2:2) > ENTRY(2:2))) THEN
    INSERT=.TRUE.
  ELSE
    INSERT=.FALSE.
  END IF

! NBEFORE & NAFTER MAY BE SET IF THIS IS NOT THE LATEST REPORT IN THE
! CHAIN, NPREF IF THERE IS ALREADY A PREFERRED REPORT FOR THIS TIME.

  NBEFORE=0
  NAFTER=0
  NPREF=0

! LOOP BACK TO HERE TO LOOK AT NEXT REPORT IN CHAIN

320 DISPL=BLKSIZ-RECDIS(POINTR)
  L=RECLEN(POINTR)
  RECNO=POINTR

! THE DUPLICATE CHECK BELOW COPES WITH METARS WHICH ARE IDENTICAL EXCEPT
! THAT ONE HAS A TIME GROUP AFTER THE IDENTIFIER AND THE OTHER NOT.

  LN=INDEX(BULL,BUFR)-1      ! LENGTH OF REPORT IN CHARACTERS
  IF (LN <= 0) LN=LEN(BULL)  ! (TOTAL LENGTH IF NO BUFR MESSAGE)
  LENBUF=LEN(BULL)-LN        ! LENGTH OF BUFR MESSAGE (MAY BE 0)

! IF TIMES ARE SAME (FIRST TWO BYTES OF INDEX ENTRY), THEN
!   IF LENGTHS ARE SAME (CHARACTERS ONLY, NOT BUFR MESSAGE OR TRAILER),
!     THEN DUPLICATES IF WHOLE STRING IDENTICAL;
!   if lengths differ by 6 or 8,
!     then duplicates if longer string has hhmmZ or ddhhmmZ
!       after the identifier & the rest of the string the same.
!
!       If it's upper air & the same part (bottom bits of byte 17,
!                                  TEMP/PILOT bit as well as part)
!       or not upper air & same hour (ignoring COR bit) & minute
!           (for upper air all parts in chain are for same ascent,
!            but some may have launch time set & others not,
!            so can't safely compare times!)
!       then it could be a duplicate.

  LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
IFLABEL12: &
  IF ((IFT == 21 .AND. (MOD(ICHAR(ENTRY(17:17)),16) ==     &
          MOD(ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)),16))) .OR. &
      (IFT /= 21 .AND. LASTHR == INDHOR .AND.              &
      ENTRY(2:2) == BLOCK(DISPL+L-21:DISPL+L-21))) THEN

! same length & identical strings - so duplicates

IFLABEL13: &
    IF (LN-(L-LENBUF-23) == 0) THEN
      IF (BULL(1:LN) == BLOCK(DISPL+1:DISPL+LN)) RETURN

! hhmmZ in new but not old, otherwise same - so duplicates

    ELSE IF (LN-(L-LENBUF-23) == 6) THEN
      IF (BULL(10:11) == 'Z '.AND. &
          BULL(11:LN) == BLOCK(DISPL+5:DISPL+LN-6)) RETURN

! ddhhmmZ in new but not old, otherwise same - so duplicates

    ELSE IF (LN-(L-LENBUF-23) == 8) THEN
      IF (BULL(12:13) == 'Z '.AND. &
          BULL(13:LN) == BLOCK(DISPL+5:DISPL+LN-8)) RETURN

! hhmmZ in old but not new, otherwise same - so duplicates

    ELSE IF (LN-(L-LENBUF-23) == -6) THEN
      IF (BLOCK(DISPL+10:DISPL+11) == 'Z '.AND. &
          BULL(5:LN) == BLOCK(DISPL+11:DISPL+LN+6)) RETURN

! ddhhmmZ in old but not new, otherwise same - so duplicates

    ELSE IF (LN-(L-LENBUF-23) == -8) THEN
      IF (BLOCK(DISPL+12:DISPL+13) == 'Z '.AND. &
          BULL(5:LN) == BLOCK(DISPL+13:DISPL+LN+8)) RETURN
    END IF IFLABEL13

! SEE IF PREFERRED FLAG IS SET (RIGHT-HAND BIT OF BYTE 17 IN TRAILER)
! AND IF SO KEEP THE RECORD NUMBER SO THAT THE COUNTS CAN BE COMPARED
! AND THE NEW REPORT PREFERRED IF IT IS BETTER.

    IF (ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)) >= 128) THEN
      NPREF=RECNO
    END IF
  END IF IFLABEL12

  POINTER=BLOCK(DISPL+L-3:DISPL+L)
  POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
  POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))

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

  IF (POINTR > 0 .AND. POINTB == NBLOCK) GO TO 320
  POINTER=INDEKS(IND)(20:23)
  POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
  POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))
END IF IFLABEL11

! IF A PREFERRED REPORT FOR THE SAME TIME HAS BEEN FOUND, COMPARE THE
! NUMBER OF GOOD VALUES AND THE COR NUMBER AND RESET THE PREFERRED
! FLAG IF THE NEW REPORT IS BETTER.
! SET THE "HISTORICAL" PREFERRED FLAG (X'20', SO ADD 32), NEVER TO BE
! UNSET, TO SHOW THAT THE NEW REPORT HAS BEEN (OR STILL IS) PREFERRED.

IFLABEL14: &
IF (IND > 0 .AND. NPREF > 0) THEN
  DISPL=BLKSIZ-RECDIS(NPREF)+RECLEN(NPREF)-INDLEN   ! --> TRAILER
  IF (BLOCK(DISPL+7:DISPL+7) < ENTRY(7:7) .OR. &    ! COR FLAG
     BLOCK(DISPL+12:DISPL+12) <= ENTRY(12:12)) THEN ! GOOD VALUES
    IFLAGS=ICHAR(BLOCK(DISPL+17:DISPL+17))          ! OLD FLAGS
    BLOCK(DISPL+17:DISPL+17)=CHAR(IFLAGS-128)       ! UNSET PREF
    IFLAGS=ICHAR(ENTRY(17:17))                      ! NEW FLAGS
    IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)   ! PREF
  END IF
ELSE                                                ! IF NO PREF,
  IFLAGS=ICHAR(ENTRY(17:17))                        ! PREFER THIS
  IF (IFLAGS < 160) ENTRY(17:17)=CHAR(IFLAGS+128+32)
END IF IFLABEL14

! IF THERE'S ROOM IN THE BLOCK & THE REPORT'S NOT A DUPLICATE, STORE IT.

IFLABEL15: &
IF (LEN(BULL)+INDENT+2 <= LEFT) THEN
  NINBLK=NINBLK+1
  L=LEN(BULL)+INDENT
  LEFT=LEFT-L-2

  RECLEN(NINBLK)=L
  BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)
  BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))

  IF (NINBLK > 1) THEN
    RECDIS(NINBLK)=RECDIS(NINBLK-1)+L
  ELSE
    RECDIS(NINBLK)=L
  END IF

  START=INDHED+NINBLK*2+LEFT
  BLOCK(START+1:START+L-INDENT)=BULL
  BLOCK(START+L-INDENT+1:START+L-4)=ENTRY(1:INDENT-4)
  BLOCK(START+L-3:START+L)=POINTER

!**********************************************************************
! UPDATE THIS AIRFIELD'S INDEX ENTRY TO POINT TO THIS REPORT, IF IT IS
! THE LATEST.  IF NOT, NAFTER & PERHAPS NBEFORE ARE NONZERO.  MAKE THE
! NAFTER-TH REPORT POINT TO THE NEW ONE & THE NEW ONE TO THE NBEFORE-TH
! OR ZERO (BY TAKING THE POINTER THAT WAS IN THE NAFTER-TH).
! (OR ADD AN ENTRY FOR THIS AIRFIELD IF THERE'S NONE IN THIS SLOT).
!**********************************************************************

  POINTR=NINBLK
  POINTB=NBLOCK
  POINTER(1:1)=CHAR(NINBLK/256)
  POINTER(2:2)=CHAR(MOD(NINBLK,256))
  POINTER(3:3)=CHAR(NBLOCK/256)
  POINTER(4:4)=CHAR(MOD(NBLOCK,256))

! COUNT OF OBS IS ONLY ONE BYTE; LEAVE IT AS 255 IF MORE THAN 255!
! Don't reset time in index unless minutes are less than 60.
! Upper air data can have missing minute (=255) and time is
! reset in UADUPS if necessary.

IFLABEL16: &
  IF (IND > 0) THEN
    NOBS=ICHAR(INDEKS(IND)(12:12))
    IF (NOBS < 255) INDEKS(IND)(12:12)=CHAR(NOBS+1)

    IF (NAFTER == 0) THEN
      IF (ENTRY(2:2) < CHAR(60)) THEN
        INDEKS(IND)(1:2)=ENTRY(1:2)
      END IF
      INDEKS(IND)(18:23)=ENTRY(18:19)//POINTER
    ELSE
      DISPL=BLKSIZ-RECDIS(NAFTER)
      LL=RECLEN(NAFTER)
      BLOCK(START+L-3:START+L)=BLOCK(DISPL+LL-3:DISPL+LL)
      BLOCK(DISPL+LL-3:DISPL+LL)=POINTER
    END IF

! 'OR' THE FLAG BYTES OF THE CHAINED OBS TO SET THE INDEX ENTRY FLAGS
! 'OR' THE BIT(S) ALREADY IN BYTE 17 WITH THOSE IN THE NEW ENTRY (IF
! TAF OR METAR) OR ON THE END OF THE IDENTIFIER (IF UPPER AIR)
! (DO THIS FROM FIRST PRINCIPLES BECAUSE IOR IS NOT PORTABLE!)
!
! FOR UPPER AIR THERE ARE 2 BITS SET TO 1 FOR TEMP & 0 FOR PILOT:
! IF A PILOT APPEARS IN A TEMP CHAIN OR VICE VERSA, SET BOTH BITS.

IFLABEL17: &
    IF (IFT == 3.OR.IFT == 4.OR.IFT == 7.OR.IFT == 21) THEN
      IBITS=ICHAR(INDEKS(IND)(17:17))
IFLABEL18: &
      IF (IFT == 21) THEN                      ! UPPER AIR
        JBITS=ICHAR(IDENT(10:10))
        ITPBITS=MOD(IBITS/4,3)                 ! BITS 5 & 6
        JTPBITS=MOD(JBITS/4,3)
        IF (ITPBITS == 1 .AND. JTPBITS == 0) THEN
          JBITS=JBITS+8+4                      ! SET BOTH BITS
        ELSE IF (ITPBITS == 0 .AND. JTPBITS == 1) THEN
          JBITS=JBITS+8                        ! SET BIT 5
        END IF
      ELSE
        JBITS=ICHAR(ENTRY(17:17))              ! TAF/METAR
      END IF IFLABEL18

      IJBITS=0
      DO I=0,7
        IBIT=MOD(IBITS/2**I,2)
        JBIT=MOD(JBITS/2**I,2)
        IF (IBIT == 1 .OR. JBIT == 1) IJBITS=IJBITS+2**I
      END DO
      INDEKS(IND)(17:17)=CHAR(IJBITS)
    END IF IFLABEL17
  ELSE

! OR MAKE A NEW INDEX ENTRY (PUT IDENT & COUNT OF 1 IN THE INPUT ENTRY)
! (U/A HAS BYTE 17 TO GO IN INDEX AFTER IDENT; ELSE LEN(IDENT)=5 OR 9)

    NTRIES=NTRIES+1
    INDEKS(NTRIES)(1:2)=ENTRY(1:2)
    INDEKS(NTRIES)(3:11)=IDENT
    INDEKS(NTRIES)(12:12)=CHAR(1)
    INDEKS(NTRIES)(13:19)=ENTRY(13:19)
    INDEKS(NTRIES)(20:23)=POINTER
    IF (LEN(IDENT) > 9) INDEKS(NTRIES)(17:17)=IDENT(10:10)

! FOR METARS, PUT THE STATION HEIGHT IN BYTES 7&8 OF THE IDENTIFIER

    IF (IFT == 4 .AND. IHT > -999) THEN
      I = IHT
      IF (I < 0) I = I + 65536
      INDEKS(NTRIES)(9:10) = CHAR(I/256) // CHAR(MOD(I,256))
    END IF

! IF THIS ENTRY NEEDS TO START A NEW INDEX BLOCK, IT'S ENOUGH HERE TO
! CLAIM A BLOCK NUMBER FROM THE OVERFLOW POOL AND LIST IT AS THE NEXT
! BLOCK TO CONTINUE THE INDEX; THIS NUMBER WILL BE SET AS A POINTER
! AT THE END OF THE PREVIOUS BLOCK WHEN THE INDEX IS WRITTEN BACK.

    NIND=NTRIES/NBLIND               ! NO OF FULL INDEX BLOCKS
IFLABEL19: &
    IF (NIND >= 1 .AND. NTRIES-NIND*NBLIND == 1) THEN
      NX=INDEX(MAP(BLOCKS-OFLOWS:BLOCKS-1-NSQ),CHAR(0))
      IF (NX == 0) THEN
        PRINT *,'NO MORE OVERFLOWS FOR INDEX',IFT,'IS FT NUMBER'
        NTRIES=NTRIES-1
        RETURN
      ELSE
        NOFLOW(NIND)=BLOCKS-OFLOWS+NX
        NOFLOW(NIND+1)=0
        MAP(NOFLOW(NIND)-1:NOFLOW(NIND)-1)=CHAR(128+NINDEX)
      END IF
    END IF IFLABEL19
  END IF IFLABEL16

! UPDATE REPORT COUNT IN INDEX & COPY TO MAP BYTE FOR INDEX BLOCK
! (USE N/256 IN CASE THE COUNT IS LARGE; ADD 1 SO THAT THE MAP BYTE
! IS NONZERO EVEN IF THE COUNT IS SMALL)

  NREPS=NREPS+1
  MAP(NINDEX-1:NINDEX-1)=CHAR(NREPS/256+1)
ELSE

!**********************************************************************
! IF THERE'S NOT, SPLIT THE DATA MORE OR LESS EQUALLY BETWEEN 2 BLOCKS.
! (GET THE SECOND FROM THE OVERFLOW POOL: BECAUSE THE INDEX OPERATION
! STARTS FURTHER ON, INDEX(...) MUST BE ADJUSTED TO GIVE BLOCK NUMBER!)
! FOR REPORTS IN THE CURRENT BLOCK, LIST AIRFIELD & NUMBER OF REPORTS,
! SORT LIST BY AIRFIELD & SPLIT INTO TWO WITH SIMILAR NUMBERS OF OBS.
!**********************************************************************

  NX=INDEX(MAP(BLOCKS-OFLOWS:BLOCKS-1-NSQ),CHAR(0))
  IF (NX == 0) THEN
    PRINT *,'NO OVERFLOWS TILL SOME RELEASED',IFT,'IS FT NUMBER'
    RETURN
  END IF
  NBLOCX=BLOCKS-OFLOWS-(XBLOKS+1)+NX
  CHARBLX=CHAR(NBLOCX/256)//CHAR(MOD(NBLOCX,256))

  K=0
  NTOTAL=0
  CHARBL=CHAR(NBLOCK/256)//CHAR(MOD(NBLOCK,256))
  DO I=1,NTRIES
    IF (INDEKS(I)(22:23) == CHARBL) THEN
      K=K+1
      WORK10(K) = INDEKS(I)(3:12)
      NTOTAL=NTOTAL+ICHAR(INDEKS(I)(12:12))
    END IF
  END DO
  IDINB=K

! SORT COUNTS BY IDENTIFIER & PICK THE IDENTIFIER WHICH SPLITS THE
! COUNTS EVENLY (SKIP THIS IF ALL THE IDENTIFIERS ARE THE SAME!)
! TAF & METAR IDENTIFIERS HAVE 4 LETTERS, SO ALL THE ITEMS SORTED ARE
! DIFFERENT (OBS FOR THE SAME IDENTIFIER ALL BEING CHAINED); BUT OTHER
! DATA TYPES USE TAFREP AND E.G. AIREPS CAN HAVE LOTS OF DIFFERENT
! IDENTIFIERS WITH THE FIRST 4 CHARACTERS THE SAME, AND IN THIS CASE
! WE MUST AVOID TAKING THE LAST AS MIDPOINT! (GT.MID MEANS FIRST OK)

  CALL SORTCH (WORK10, 10, IDINB, MASK(3:12))
  LASTID = WORK10(IDINB)(1:9)

  IF (IDINB <= 2) THEN         ! IF ONLY 2, TAKE THE FIRST
    MID = WORK10(1)(1:9)
  ELSE IF (IDINB > 2) THEN
    NOBS=0
    DO I=1,IDINB-1
      NOBS = NOBS + ICHAR(WORK10(I)(10:10))
      IF (NOBS >= NTOTAL/2 .OR. WORK10(I+1)(1:9) == LASTID) GO TO 421
    END DO
421 CONTINUE
    MID = WORK10(I)(1:9)
  END IF

! MID WILL BE PUT IN THE ASSIGNMENT LIST LATER. INITIALISE NEW BLOCK.

  BLKTAX=CURENT
  NINBLX=0
  LEFX=BLKSIZ-INDHED
  BLOCKX(:BLKSIZ)=' '
  MAP(XBLOKS+NBLOCX:XBLOKS+NBLOCX)=CHAR(NINDEX)

! IF THE FULL BLOCK HAS ONLY ONE IDENT, IT CAN'T BE SPLIT, SO SET THE
! TOP BIT OF THE MAP BYTE AS FOR INDEX OVERFLOW TO STOP STORING IN IT.

IFLABEL20: &
  IF (IDINB == 1) THEN
    MAP(XBLOKS+NBLOCK:XBLOKS+NBLOCK)=CHAR(128+NINDEX)

! If all the identifiers are the same, the ob to be stored may have the
! same identifier again; but it can have a different identifier - after
! obs for only one identifier in the range just happen to have filled a
! block!  So we mustn't assume that IDINB=1 implies IND>0.  Both cases
! need a null range (start=end) for the full block, to stop storing in
! it, and the original range for the new block.  But only chain back to
! the full block if the ident is the same.
!    Write the new block and the map block out now; the full block is
! unchanged, but will still be written out at the end.

IFLABEL21: &
    IF (IND > 0) THEN
      NINBLX=1                       ! ONE OB IN NEW DATA BLOCK
      L=LEN(BULL)+INDENT
      LEFX=LEFX-L-2

      BLOCKX(7:7)=CHAR(L/256)
      BLOCKX(8:8)=CHAR(MOD(L,256))
!                                    ! (IND IS SET IF IDINB=1!)
      POINTER=INDEKS(IND)(20:23)     ! CHAIN TO LATEST IN INDEX
      INDEKS(IND)(20:23)=CHAR(0)//CHAR(1)//CHARBLX
      NOBS=ICHAR(INDEKS(IND)(12:12)) ! UPDATE INDEX COUNT
      IF (NOBS < 255) INDEKS(IND)(12:12)=CHAR(NOBS+1)
      NREPS=NREPS+1

      BLOCKX(BLKSIZ-L+1:BLKSIZ-INDENT)=BULL
      BLOCKX(BLKSIZ-INDENT+1:BLKSIZ-4)=ENTRY(1:INDENT-4)
      BLOCKX(BLKSIZ-3:BLKSIZ)=POINTER

      BLOCKX(1:1)=CHAR(BLKTAX/256)
      BLOCKX(2:2)=CHAR(MOD(BLKTAX,256))
      BLOCKX(3:3)=CHAR(NINBLX/256)
      BLOCKX(4:4)=CHAR(MOD(NINBLX,256))
      BLOCKX(5:5)=CHAR(LEFX/256)
      BLOCKX(6:6)=CHAR(MOD(LEFX,256))

      NUMREC = 1+XBLOKS+NBLOCX+NSQ
      CALL METDB_CWRITE_DIR(IFT, BLOCKX(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
      IF (IORC /= 0) CALL SYSABN(907)

! Find which range NBLOC is for (IDENT is in).  This is set
! already if IND=0, but hasn't been needed yet if IND>0.

      JRANGE=1
      DO WHILE (IDENT(1:4) <= BLOKID(JRANGE*2-1) &
           .OR. IDENT(1:4) > BLOKID(JRANGE*2))
        JRANGE=JRANGE+1
      END DO
    END IF IFLABEL21

! Set the range for the new block to the original range for the
! full block & nullify the full block's range (start=end)

    NB=NB+1
    BLOKID(NB*2-1)=BLOKID(JRANGE*2-1)
    BLOKID(NB*2)=BLOKID(JRANGE*2)
    BLOKID(JRANGE*2-1)=MID(1:4)
    BLOKID(JRANGE*2)=MID(1:4)
!                                              Set up BUFFER for output
    BUFFER = MAPHDR
    BUFFER(9:BLKSIZ-4012) = MAP(1:BLKSIZ-4020)
    IBUF = BLKSIZ - 4011
    BUFFER(IBUF:IBUF+3) = TRANSFER(RANGE_CENTHR,CH4)
    BUFFER(IBUF+4:IBUF+7) = TRANSFER(NB,CH4)
    IBUF = IBUF + 8
    DO I=1,1000
      BUFFER(IBUF:IBUF+3) = BLOKID(I)
      IBUF = IBUF + 4
    END DO
    BUFFER(IBUF:IBUF+3) = MAPOVR
!                                                      Write map record
    NUMREC = 1
    CALL METDB_CWRITE_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
    IF (IORC /= 0) CALL SYSABN(908)
    IF (IND == 0) GO TO 200
  ELSE IF (IDINB > 1) THEN

! COPY OBS IN 2ND HALF OF LIST TO NEW BLOCK, WHICH MEANS FOLLOWING
! POINTER CHAIN FOR EACH AIRFIELD (THE OBS WILL BE STORED IN REVERSE
! ORDER - DOESN'T MATTER), CHANGING THE CORRESPONDING INDEX POINTERS.
! KNOWING THE DISPLACEMENTS (RECDIS), WE CAN ZERO LENGTHS IN RECLEN.
!
! GO THROUGH THE INDEX ENTRIES, PICKING OBS IN THE FULL BLOCK WITH
! IDENTIFIERS AFTER THE BREAK POINT FOR TRANSFER TO THE NEW BLOCK.
! ZERO THE LENGTHS OF THE TRANSFERRED REPORTS AT THE START OF THE
! FULL BLOCK TO LET POINTERS TO REPORTS LEFT IN THIS BLOCK TO STAY
! THE SAME (POINTERS IN THE INDEX AND ON END OF OBS).

DOLABEL1: &
    DO I=1,NTRIES
IFLABEL22: &
      IF (INDEKS(I)(22:23) == CHARBL .AND. INDEKS(I)(3:11) > MID) THEN
        RECNO=ICHAR(INDEKS(I)(20:20))*256+ICHAR(INDEKS(I)(21:21))
        INDEKS(I)(22:23)=CHARBLX
        INDEKS(I)(20:20)=CHAR((NINBLX+1)/256)
        INDEKS(I)(21:21)=CHAR(MOD(NINBLX+1,256))
431     DISPL=BLKSIZ-RECDIS(RECNO)

!   STORE REPORT IN SECOND BLOCK, ZEROING ITS LENGTH IN FIRST BLOCK.

        NINBLX=NINBLX+1
        L=RECLEN(RECNO)
        LEFX=LEFX-L-2

        BLOCKX(6+NINBLX*2-1:6+NINBLX*2-1)=CHAR(L/256)
        BLOCKX(6+NINBLX*2:6+NINBLX*2)=CHAR(MOD(L,256))

        START=INDHED+NINBLX*2+LEFX
        BLOCKX(START+1:START+L-4)=BLOCK(DISPL+1:DISPL+L-4)

        RECLEN(RECNO)=0
        BLOCK(6+RECNO*2-1:6+RECNO*2-1)=CHAR(0)
        BLOCK(6+RECNO*2:6+RECNO*2)=CHAR(0)

!   Follow chain while old pointer is to same block, but go no further
!   if pointer is zero or to a different block.
!   Zero pointer & warning if record number is impossible!
!   (if not that many records in block)

        RECNO=ICHAR(BLOCK(DISPL+L-3:DISPL+L-3))*256 &
             +ICHAR(BLOCK(DISPL+L-2:DISPL+L-2))
IFLABEL23: &
        IF (BLOCK(DISPL+L-1:DISPL+L) == CHARBL) THEN
          N=NINBLX+1
          BLOCKX(START+L-3:START+L-3)=CHAR(N/256)
          BLOCKX(START+L-2:START+L-2)=CHAR(MOD(N,256))
          BLOCKX(START+L-1:START+L)=CHARBLX
          GO TO 431
        ELSE IF (RECNO == 0  .OR. RECNO > NINBLK) THEN
          BLOCKX(START+L-3:START+L-2)=CHAR(0)//CHAR(0)
          BLOCKX(START+L-1:START+L)=CHAR(0)//CHAR(0)
          IF (RECNO > NINBLK) THEN
            print *,'TAFREP',RECNO,'=RECNO',NINBLK,'=NINBLK'
            print *,NBLOCK,'the block has bad pointer'
            PRINT *,IFT,'is FT number'
          END IF
        ELSE          ! IF NOT SAME BLOCK & NOT ZERO, COPY POINTER
          BLOCKX(START+L-3:START+L)=BLOCK(DISPL+L-3:DISPL+L)
        END IF IFLABEL23
      END IF IFLABEL22
    END DO DOLABEL1

! WRITE OUT SECOND DATA BLOCK, THEN INDEX BLOCK WITH POINTERS TO IT.

    BLOCKX(1:1)=CHAR(BLKTAX/256)
    BLOCKX(2:2)=CHAR(MOD(BLKTAX,256))
    BLOCKX(3:3)=CHAR(NINBLX/256)
    BLOCKX(4:4)=CHAR(MOD(NINBLX,256))
    BLOCKX(5:5)=CHAR(LEFX/256)
    BLOCKX(6:6)=CHAR(MOD(LEFX,256))

    NUMREC = 1+XBLOKS+NBLOCX+NSQ
    CALL METDB_CWRITE_DIR (IFT, BLOCKX(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
    IF (IORC /= 0) CALL SYSABN(909)

! FIRST WRITE ANY FULL INDEX BLOCKS

    INDHDR(1:1)=CHAR(TIMTAG/256)
    INDHDR(2:2)=CHAR(MOD(TIMTAG,256))
    INDHDR(3:3)=CHAR(NTRIES/256)
    INDHDR(4:4)=CHAR(MOD(NTRIES,256))
    INDHDR(5:5)=CHAR(NREPS/256)
    INDHDR(6:6)=CHAR(MOD(NREPS,256))

    NFULL=NTRIES/NBLIND                ! NUMBER OF FULL BLOCKS
    IX=NINDEX                          ! TO VARY BLOCK IN LOOP
    DO NIND=1,NFULL
      NIBL=(NIND-1)*NBLIND             ! ENTRIES ALREADY WRITTEN

      INDOVR(1:1)=CHAR(NOFLOW(NIND)/256)
      INDOVR(2:2)=CHAR(MOD(NOFLOW(NIND),256))

      BUFFER = INDHDR
      IBUF = 7
      DO I=NIBL+1,NIBL+NBLIND
        BUFFER(IBUF:IBUF+22) = INDEKS(I)
        IBUF = IBUF + 23
      END DO
      BUFFER(IBUF:IBUF+1) = INDOVR
!                                                    Write index record
      NUMREC = IX+NSQ
      CALL METDB_CWRITE_DIR(IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
      IF (IORC /= 0) CALL SYSABN(910)

      IX=NOFLOW(NIND)                  ! OVERFLOW BLOCK NO
    END DO

! THEN THE LAST INDEX BLOCK (WHICH MAY BE THE ONLY ONE)

    IF (NTRIES > NFULL*NBLIND) THEN
      BUFFER = INDHDR
      IBUF = 7
      DO I=NFULL*NBLIND+1,NTRIES
        BUFFER(IBUF:IBUF+22) = INDEKS(I)
        IBUF = IBUF + 23
      END DO
      DO I=NTRIES+1,(NFULL+1)*NBLIND
        BUFFER(IBUF:IBUF+22) = EMPTYX
        IBUF = IBUF + 23
      END DO
      BUFFER(IBUF:IBUF+1) = CHAR(0) // CHAR(0)
!                                                    Write index record
      NUMREC = IX+NSQ
      CALL METDB_CWRITE_DIR(IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
      IF (IORC /= 0) CALL SYSABN(911)
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

    NSTART=BLKSIZ
DOLABEL2: &
    DO I=1,NINBLK
IFLABEL24: &
      IF (RECLEN(I) > 0) THEN
        LN=RECLEN(I)
        ISTART=BLKSIZ-RECDIS(I)
        NSTART=NSTART-LN
        IF (NSTART > ISTART) THEN
          IF (NSTART-ISTART < LN) THEN
            BLOCKX(BLKSIZ-LN+1:BLKSIZ)=BLOCK(ISTART+1:ISTART+LN)
            BLOCK(NSTART+1:NSTART+LN)=BLOCKX(BLKSIZ-LN+1:BLKSIZ)
          ELSE
            BLOCK(NSTART+1:NSTART+LN)=BLOCK(ISTART+1:ISTART+LN)
          END IF
        END IF
      END IF IFLABEL24
    END DO DOLABEL2

    LEFT=NSTART-2*NINBLK-INDHED
    BLOCK(INDHED+2*NINBLK+1:INDHED+2*NINBLK+LEFT)=' '

    BLOCK(1:1)=CHAR(BLKTAG/256)
    BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
    BLOCK(3:3)=CHAR(NINBLK/256)
    BLOCK(4:4)=CHAR(MOD(NINBLK,256))
    BLOCK(5:5)=CHAR(LEFT/256)
    BLOCK(6:6)=CHAR(MOD(LEFT,256))

    NUMREC = 1+XBLOKS+NBLOCK+NSQ
    CALL METDB_CWRITE_DIR (IFT, BLOCK(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
    IF (IORC /= 0) CALL SYSABN(912)

! RECALCULATE THE DISPLACEMENTS IN CASE THE REPORT IS STORED IN THE
! COMPRESSED BLOCK WITHOUT READING THAT BLOCK IN AGAIN.

    RECDIS(1)=RECLEN(1)
    DO I=2,NINBLK
      RECDIS(I)=RECDIS(I-1)+RECLEN(I)
    END DO

! UPDATE THE LIST OF RANGES OF IDENTIFIERS ASSIGNED TO DATA BLOCKS,
! ADDING THE SECOND PART OF THE SPLIT RANGE TO THE END OF THE LIST.

    IF (RANGE_CENTHR == CENTHR) THEN
      NB=NB+1
      DO I=1,NB
       IF (MID(1:4) > BLOKID(I*2-1).AND. &
           MID(1:4) <= BLOKID(I*2)) GO TO 471
      END DO
471   BLOKID(NB*2-1)=MID(1:4)
      BLOKID(NB*2)=BLOKID(I*2)
      BLOKID(I*2)=MID(1:4)
    END IF

! FINALLY WRITE BACK THE MAP BLOCK & GO BACK TO STORE THE REPORT

    BUFFER = MAPHDR
    BUFFER(9:BLKSIZ-4012) = MAP(1:BLKSIZ-4020)
    IBUF = BLKSIZ - 4011
    BUFFER(IBUF:IBUF+3) = TRANSFER(RANGE_CENTHR,CH4)
    BUFFER(IBUF+4:IBUF+7) = TRANSFER(NB,CH4)
    IBUF = IBUF + 8
    DO I=1,1000
      BUFFER(IBUF:IBUF+3) = BLOKID(I)
      IBUF = IBUF + 4
    END DO
    BUFFER(IBUF:IBUF+3) = MAPOVR
!                                                      Write map record
    NUMREC = 1
    CALL METDB_CWRITE_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
    IF (IORC /= 0) CALL SYSABN(913)
    GO TO 200
  END IF IFLABEL20
END IF IFLABEL15

!**********************************************************************
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
!
! There have been errors with rogue pointers at the end of a data
! block: the pointer on the first ob stored should be zero unless
! it's a TTBB with nonzero minutes, in which case the chain might
! have been reordered, or unless the block is an overflow: chains
! are copied in reverse order by the split process.
!
! This check hasn't shown up anything, so commented out for the
! moment. If reinstated, it should probably be done for U/A only;
! if not U/A it should probably check for a zero hour too...
!
!     IF (BLOCK(BLKSIZ-3:BLKSIZ) /= CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
!    &    .AND. BLOCK(BLKSIZ-21:BLKSIZ-21) == CHAR(0)
!    &    .AND. 1+NSQ+XBLOKS+NBLOCK <= BLOCKS-OFLOWS)  THEN
!       PRINT *,NBLOCK,'th data block has first ob with nonzero'
!       PRINT *,'       pointer & zero minutes - anything wrong?'
!     END IF

BLOCK(1:1)=CHAR(BLKTAG/256)
BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
BLOCK(3:3)=CHAR(NINBLK/256)
BLOCK(4:4)=CHAR(MOD(NINBLK,256))
BLOCK(5:5)=CHAR(LEFT/256)
BLOCK(6:6)=CHAR(MOD(LEFT,256))

NUMREC = 1+XBLOKS+NBLOCK+NSQ
CALL METDB_CWRITE_DIR (IFT, BLOCK(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
IF (IORC /= 0) CALL SYSABN(914)

! WRITE THE MAP BLOCK BACK ONLY IF A NEW DATA OR INDEX BLOCK HAS BEEN
! CLAIMED - OR IF THE NUMBER OF REPORTS FOR THIS INDEX BLOCK IS N*256.

IF (NINBLK == 1 .OR. NTRIES-(NTRIES/NBLIND)*NBLIND == 1 &
                .OR. NREPS == (NREPS/256)*256) THEN
  BUFFER = MAPHDR
  BUFFER(9:BLKSIZ-4012) = MAP(1:BLKSIZ-4020)
  IBUF = BLKSIZ - 4011
  BUFFER(IBUF:IBUF+3) = TRANSFER(RANGE_CENTHR,CH4)
  BUFFER(IBUF+4:IBUF+7) = TRANSFER(NB,CH4)
  IBUF = IBUF + 8
  DO I=1,1000
    BUFFER(IBUF:IBUF+3) = BLOKID(I)
    IBUF = IBUF + 4
  END DO
  BUFFER(IBUF:IBUF+3) = MAPOVR
!                                                      Write map record
  NUMREC = 1
  CALL METDB_CWRITE_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(915)
END IF

! FINALLY THE INDEX BLOCK(S).  FIRST WRITE ANY FULL INDEX BLOCKS

INDHDR(1:1)=CHAR(TIMTAG/256)
INDHDR(2:2)=CHAR(MOD(TIMTAG,256))
INDHDR(3:3)=CHAR(NTRIES/256)
INDHDR(4:4)=CHAR(MOD(NTRIES,256))
INDHDR(5:5)=CHAR(NREPS/256)
INDHDR(6:6)=CHAR(MOD(NREPS,256))

NFULL=NTRIES/NBLIND                    ! NUMBER OF FULL BLOCKS
IX=NINDEX                              ! TO VARY BLOCK IN LOOP
DO NIND=1,NFULL
  NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY WRITTEN

  INDOVR(1:1)=CHAR(NOFLOW(NIND)/256)
  INDOVR(2:2)=CHAR(MOD(NOFLOW(NIND),256))

  BUFFER = INDHDR
  IBUF = 7
  DO I=NIBL+1,NIBL+NBLIND
    BUFFER(IBUF:IBUF+22) = INDEKS(I)
    IBUF = IBUF + 23
  END DO
  BUFFER(IBUF:IBUF+1) = INDOVR
!                                                    Write index record
  NUMREC = IX+NSQ
  CALL METDB_CWRITE_DIR(IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(916)
  IX=NOFLOW(NIND)                      ! OVERFLOW BLOCK NO
END DO

! THEN THE BLOCK WHICH ISN'T FULL (UNLESS NUMBER OF ENTRIES IS MULTIPLE
!                                                    OF BLOCK CAPACITY)
IF (NTRIES > NFULL*NBLIND) THEN
  BUFFER = INDHDR
  IBUF = 7
  DO I=NFULL*NBLIND+1,NTRIES
    BUFFER(IBUF:IBUF+22) = INDEKS(I)
    IBUF = IBUF + 23
  END DO
  DO I=NTRIES+1,(NFULL+1)*NBLIND
    BUFFER(IBUF:IBUF+22) = EMPTYX
    IBUF = IBUF + 23
  END DO
  BUFFER(IBUF:IBUF+1) = CHAR(0) // CHAR(0)
!                                                    Write index record
  NUMREC = IX+NSQ
  CALL METDB_CWRITE_DIR (IFT, BUFFER(1:BLKSIZ), BLKSIZ, NUMREC, IORC)
  IF (IORC /= 0) CALL SYSABN(917)
END IF

RETURN
END SUBROUTINE TAFREP
