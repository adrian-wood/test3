      SUBROUTINE TAFREP(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TAFREP
!
! PURPOSE       : STORE 23-BYTE CHAINED ENTRIES IN THE MDB
!
! DATA TYPES    : TAFS,METARS,U/A,SAMOSX,SREWS,NCM,CLIMAT,         !1.16
!                 BOGUS,ENHSAWS,HRR,TBUS,TRACKOB                   !1.16
!
! CALLED BY     : TAFIND,UAEDIT,SRWBUL,NCMBUL,CLMBUL,              !2.0
!                 BOGIND,ENHBUL,HRRBUL,TBUSBUL,TRKSTO              !1.16
!                                (SAMBUL CALLS TAFIND)
!
! CALLS         : DATE31,SORTCH,ICOBRV,INDLALO,UADUPS, CENTURY     !2.0
!
! PARAMETERS    : (1) DATE/TIME (YEAR, MONTH...)
!                 (2) TRAILER (WITHOUT TIMES & BLOCK/RECORD NO & WITH
!                      ENTRY(3:11)=TTAAII(3:6)
!                 (3) REPORT TO BE STORED
!                 (4) FT NUMBER (FROM TT IN BULLETIN HEADING)
!                     (3:TAF & 7:ATAFS in MDBTAFS;                 !1.16
!                      4:METAR in MDBMETAR; 21:U/A in MDBUAIR;     !1.16
!                      others assigned by storage job MDBOTHRS)    !1.16
!                 (5) BLOCKSIZE OF OUTPUT DATA SET
!                 (6) IDENTIFIER TO REPLACE TTAAII ETC IN INDEX ENTRY
!                     (WITH BYTE 17 FLAGS AS BYTE 10 FOR U/A)         !A
!
! REVISION INFO :
!
! $Workfile: tafrep.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 01/10/2010 16:31:02$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         01/10/2010 16:31:02    Brian Barwell
!       Additions to put station height in index entry as part of station ID.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:15    Sheila Needham  
! $
! Revision 2.1  2002/10/07 15:55:41  usmdb
! 16 Sept 2002    C Long
! 2.1 Don't check time in upper air duplicate check: all obs in
!      chain are for same ascent, but not all have launch time.
!
! Revision 2.0  2001/11/06  11:03:39  11:03:39  usmdb (Generic MetDB account)
! 15 Oct 2001     C Long
! 2.0  INTEGER*2 & EQUIVALENCEs removed, SYSABN called rather than ABEND
!      DO n replaced by loops with ENDDO, indentation & continuation
!      markers standardised. IMPLICIT NONE added and all variables
!      declared
! 2.0a Reports differing only in that one has a ddhhmmZ group & the othe
!      not are treated as duplicates, as well as any differing only by a
!      hhmmZ group. (METARs now have to code ddhhmmZ rather than hhmmZ.)
!
! Revision 1.16  2000/10/09  08:52:33  usmdb
! 09 Oct 2000 - Don't use variable NBLOCK to free blocks; keep it
! for data block in core.
!
! Revision 1.15  2000/09/06  11:00:04  usmdb
! 18 Sept 2000    C Long
! Correct 1.14 (set NBLOC as well as IND from UADUPS for U/A!).
! Plug old holes, e.g don't write to data block zero.
!
! Revision 1.14  2000/08/09  14:56:38  usmdb
! 17 July 2000    C Long
! Call UADUPS to find an existing index entry for TEMP
!
! Revision 1.13  2000/03/10  09:38:01  usmdb
! 20 March 2000      C Long
! Call INDLALO to put lat/long in index entry
!
! Revision 1.12  99/07/12  16:19:50  usmdb
! 19 July 1999     C Long
! Remove check added to catch U/A problems:
! message only appears for other data types!
! Add FT number to some other messages.
!
! Revision 1.11  99/02/11  09:03:56  usmdb
! 1.11a Combine long & short flags for ATAFS as for TAFs
! 1.11b Don't list known positions as ICOBRV always called now
! 1.11c Improve updating of ranges in map block
!        (by storing a century-hour before the range count)
! 1.11d Improve splitting of full data block by sorting on full ident
! 1.11e Don't lose block 11 METAR identifier in ICOBRV call
! 1.11  Remove expensive concatenations
!
! Revision 1.10  98/11/19  10:46:21  usmdb
! 23/11/98 Make sure that ATAF storage also calls STNICO to obtain
! station detaisl. Jlewthwaite. v(G)=124 ev(G)=52
!
! Revision 1.9  98/09/30  09:44:24  usmdb
! 30/10/1998 Remove old Station position array which is no longer
! required after change 1.8. Jon Lewtwhaite  v(g)=124 ev(G)=52
!
! Revision 1.8  98/09/16  16:42:21  usmdb
! 21/09/1998 Remove call to station master fro TAFS/METARS and
! replace it with a call to the ICAO abbreviated list. Also
! change from JSL82 to using SORTCH. v(G)=124 ev(G)=52
! Jon Lewthwaite
!
! Revision 1.7  98/02/04  16:00:07  usmdb
! Correct code to set preferred flag.
!
! Revision 1.6  1998/01/29 11:44:13  usmdb
! Addition of IBM preprocess directive.
!
! 16 Dec 97  : Correct code to set preferred flag (LT instead of LE!)
!              and to recognise duplicate (ignoring COR flag in hour
!              byte & checking part if upper air)                      !I
!
! Revision 1.5  97/09/22  13:58:09  13:58:09  uspm (Pat McCormack)
! Add check on OFLOWS before calculating NX to avoid out of bounds error
!
! Revision 1.4  1997/08/06 10:44:00  uspm
! Remove characters after column 72 as get wrapped on transfer to 1
!
! Revision 1.3  1997/08/01 08:24:50  uspm
! Amend for Year 2000 - use common routine to determine
! century - Jim Arnott.                                                !H
!
! Revision 1.2  1997/07/31 11:41:58  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 14:22:40  uspm
! Initial revision
!
!   2 APR 97 : CHANGE ELEMENTS LATITUDE LONGITUDE TO ELEMENTS LAT LON
!              IN STNMAS CALL TO MDB - S.COX                            G
!
!   3 JAN 97 : NOT ENOUGH SIMPLY TO OR BYTE 17 BITS IF UPPER AIR:       F
!              TEST FOR TEMP & PILOT IN SAME CHAIN & SET COMBINATION
!
!  16 OCT 96 : OR BITS IN BYTE 17 OF INDEX ENTRY FOR U/A RATHER THAN
!              SET IT FROM LATEST PART (MAY BE BOTH TEMP & PILOT!)      E
!
!   4 SEP 96 : TRY TO USE AN INDEX BLOCK FOR THE SAME TIME ON A
!              DIFFERENT DAY TO DISTRIBUTE IDENTS OVER DATA BLOCKS
!              RATHER THAN JUST GOING BACK A BLOCK AT A TIME.           D
!
!  29 AUG 96 : IF LEN(IDENT)>9, ONLY CHECK FIRST 9 CHARACTERS WHEN      C
!              LOOKING FOR AN EXISTING INDEX ENTRY
!
!  14 AUG 96 : AN AIREP DATA BLOCK CAN HAVE LOTS OF DIFFERENT IDENTS
!              ALL WITH THE SAME FIRST 4 CHARACTERS - ALLOW FOR THIS
!              WHEN SPLITTING (GOING INTO NEW BLOCK AS IF ALL SAME)     B
!
!   8 AUG 96 : U/A NEEDS BYTE 17 IN INDEX & TRAILER DIFFERENT - PASS    A
!              BYTE 17 FOR INDEX AS 10TH CHARACTER OF IDENT (6TH ARG)
!
!  16 JUN 95 : SET PREFERRED FLAG DURING DUPLICATE CHECK
!
!   5 JAN 95 : ABEND IF MAP WRITE FAILS; VARIABLE NOBS IN STNMAS CALL
!
!   6 DEC 94 : IF A DATA BLOCK HAS ALL ID'S THE SAME & CAN'T BE SPLIT,
!              JUST START A NEW OVERFLOW BLOCK, CHAIN ACROSS BLOCKS &
!              SET THE RANGES SO AS NOT TO STORE IN THE FULL BLOCK.
!
!   3 JUN 94 : ADD IDENT AS 6TH ARGUMENT SO AS NOT TO RESTRICT IDENT
!              TO 5 CHARACTERS AT START OF REPORT.
!              TREAT METARS AS DUPLICATES IF THE ONLY DIFFERENCE IS
!              THAT ONE HAS A TIME GROUP AND THE OTHER NOT.
!              ONLY LOOK UP STATION LAT/LONG HERE IF TAF OR METAR.
!
!  28 MAR 94 : GENERALISE CODE FOR MORE THAN ONE SUBTYPE
!
!   2 JUL 93 : CORRECT CHECK FOR DATA OUT OF RANGE OF DATA BASE
!
!  29 JUN 93 : FIND CENTURY-HOUR FOR EACH OB, NOT JUST WHEN MAP READ
!
!  10 MAY 93 : 2 BITS IN INDEX ENTRY TO FLAG TAF AS LONG OR SHORT:
!              1 BIT SET IN TRAILER, 'OR' BITS IN CHAIN FOR INDEX.
!
!   7 APR 93 : KEEP FT NUMBER OF DATA SET JUST READ FOR MAP, INDEX &
!              MESSAGE BLOCKS SEPARATELY, RATHER THAN ONE LAST FT NO
!              WHEN MAYBE ONLY MAP HAS BEEN READ (OLD DATA REJECTED!)
!              MADE FROM ERSREP NOV 1992
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-----------------------------------------------------------------------
! parameter statements
!-----------------------------------------------------------------------

      INTEGER INDHED
      INTEGER INDLEN
      INTEGER MAXLEN
      INTEGER MAXOFL

      PARAMETER (INDHED=6)
      PARAMETER (INDLEN=23)
      PARAMETER (MAXLEN=27998)
      PARAMETER (MAXOFL=7)

!-----------------------------------------------------------------------
! declare integer variables
!-----------------------------------------------------------------------

      INTEGER AMSTAR                                                !2.0
      INTEGER BLKSIZ
      INTEGER BLKTAG                                                !2.0
      INTEGER BLKTAX                                                !2.0
      INTEGER BLOCKS                                                !2.0
      INTEGER CENDAY
      INTEGER CENTHR
      INTEGER CENTURY     ! COMMON FUNCTION TO DETERMINE C20/C21.     !H
      INTEGER CURENT
      INTEGER DATIME(5)
      INTEGER DISPL
      INTEGER HOURNOW
      INTEGER I
      INTEGER IBIT
      INTEGER IBITS
      INTEGER IDINB
      INTEGER IFLAGS
      INTEGER IFT
      INTEGER IHT         ! STATION HEIGHT FROM ICAO LIST            !2
      INTEGER IJBITS
      INTEGER IND
      INTEGER INDENT                                                !2.0
      INTEGER INDHOR
      INTEGER INDXFT
      INTEGER IORC
      INTEGER ISTART
      INTEGER ITOR                                                  !2.0
      INTEGER ITPBITS
      INTEGER IX
      INTEGER IY
      INTEGER J
      INTEGER JBIT
      INTEGER JBITS
      INTEGER JRANGE       ! ident of new ob was in this range    !1.11d
      INTEGER JTPBITS
      INTEGER K
      INTEGER L
      INTEGER LASTHR
      INTEGER LASTIX
      INTEGER LATEST
      INTEGER LEFT                                                  !2.0
      INTEGER LEFX                                                  !2.0
      INTEGER LENBUF
      INTEGER LL
      INTEGER LN
      INTEGER LOB
      INTEGER MAPFT
      INTEGER MESSFT
      INTEGER N
      INTEGER NAFTER
      INTEGER NB
      INTEGER NBEFORE
      INTEGER NBLIND
      INTEGER NBLOC
      INTEGER NBLOCX
      INTEGER NBLOCK
      INTEGER NFULL
      INTEGER NIBL
      INTEGER NINBLK                                                !2.0
      INTEGER NINBLX                                                !2.0
      INTEGER NIND
      INTEGER NINDEX
      INTEGER NL
      INTEGER NMAX
      INTEGER NOB
      INTEGER NOBS
      INTEGER NOFLOW(MAXOFL)                                        !2.0
      INTEGER NOLD
      INTEGER NOLDIX
      INTEGER NOW(8)
      INTEGER NPREF
      INTEGER NREP
      INTEGER NREPS                                                 !2.0
      INTEGER NSEQBL
      INTEGER NSTART
      INTEGER NSQ
      INTEGER NTOTAL
      INTEGER NTRIES                                                !2.0
      INTEGER NX
      INTEGER NXBLOK
      INTEGER OFLOWS                                                !2.0
      INTEGER POINTB                                                !2.0
      INTEGER POINTR                                                !2.0
      INTEGER RANGE_CENTHR ! century-hour for ranges in map       !1.11c
      INTEGER RC
      INTEGER RECDIS(14000)                                         !2.0

! RECLEN is the array of report length slots after the block header.
! its dimension is arbitrary, it only needs to be big enough for the
! reports that will fit in a block.  we don't know where the lengths
! will end and the reports, slotted in from the end, will meet them!

      INTEGER RECLEN(14000)                                         !2.0
      INTEGER RECNO
      INTEGER SLODAY
      INTEGER SLOTHR
      INTEGER START
      INTEGER TIMTAG                                                !2.0
      INTEGER X          ! unused o/p from DATE13                   !2.0
      INTEGER XBLOKS                                                !2.0
      INTEGER XHOURS                                                !2.0
      INTEGER Y          ! unused o/p from DATE13                   !2.0
      INTEGER ZEROBL       ! block number to free by zero in map  !1.16

!-----------------------------------------------------------------------
! declare logical variables
!-----------------------------------------------------------------------

      LOGICAL FIRST     !- true if first call to routine.           !2.0
      LOGICAL INSERT                                               !1.11

!-----------------------------------------------------------------------
! declare real variables
!-----------------------------------------------------------------------

      REAL POSITION(2)

!-----------------------------------------------------------------------
! declare character variables
!-----------------------------------------------------------------------

      CHARACTER*(MAXLEN) BLOCK                                      !2.0
      CHARACTER*4  BLOKID(1000)                                   !1.11d
      CHARACTER*(MAXLEN) BLOCKX                                     !2.0
      CHARACTER*4  BUFR                                             !2.0
      CHARACTER    BULL*(*)
      CHARACTER*2  CHARBL
      CHARACTER*2  CHARBLX
      CHARACTER*23 EMPTYX                                           !2.0
      CHARACTER    ENTRY*(*)
      CHARACTER*80 HEAD                                              !2
      CHARACTER    IDENT*(*)
      CHARACTER*23 INDEKS(MAXOFL*MAXLEN/INDLEN) ! index entries
      CHARACTER*6  INDHDR                                           !2.0
      CHARACTER*2  INDOVR                                           !2.0
      CHARACTER*9  LASTID                                         !1.11d
      CHARACTER*(MAXLEN) MAP                                        !2.0
      CHARACTER*8  MAPHDR                                           !2.0
      CHARACTER*4  MAPOVR                                           !2.0
      CHARACTER*23 MASK
      CHARACTER*23 MASX
      CHARACTER*1  MEDIAN                                           !2.0
      CHARACTER*9  MID                                            !1.11d
      CHARACTER*4  POINTER                                        !1.11d
      CHARACTER*1  QUARTL                                           !2.0

!-----------------------------------------------------------------------
! dynamic common and SAVE statement
!-----------------------------------------------------------------------

      COMMON /MDBUFS/ MAP,RECLEN,BLOCK,BLOCKX,                      !2.0
     &                INDEKS,RECDIS,BLOKID                          !2.0
      SAVE                                                          !2.0

!-----------------------------------------------------------------------
! data statements.
!-----------------------------------------------------------------------

      DATA EMPTYX/' '/                                              !2.0
      DATA FIRST/.TRUE./
      DATA INDXFT/0/
      DATA LATEST/0/
      DATA MAPFT/0/
      DATA MASK/'  XXXXXXXXXX           '/ !TO SORT ON IDENT      !1.11d
      DATA MASX/'           X           '/ !TO SORT ON COUNT        !1.8
      DATA MESSFT/0/

!-----------------------------------------------------------------------
! Initialise variables on first call only.
!-----------------------------------------------------------------------

      IF (FIRST) THEN                                               !2.0
        BUFR = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)         !2.0
        HEAD = '$Workfile: tafrep.f$ ' //
     &         '$Revision: 2$ $Date: 01/10/2010 16:31:02$'
        FIRST=.FALSE.                                               !2.0
      ENDIF                                                         !2.0

***********************************************************************
*
* IF FIRST TIME, READ IN MAP BLOCK (TO GET NUMBER OF INDEX BLOCKS)
*
* MAP BLOCK:             (THE BYTE FOR EACH BLOCK IS SET TO ITS INDEX
*                        BLOCK NUMBER - SO LESS THAN 256*XHOURS DATA!)
* ------------------------------------------------------------ - - - -
* : NO. OF : NO. OF : HOURS : START OF : FIRST :  2ND  :     : 1ST   :
* : BLOCKS : INDEX  : PER   : 1ST SLOT : INDEX : INDEX :     : DATA  :
* : IN D/S : BLOCKS : BLOCK : AFTER 0Z : BLOCK : BLOCK :     : BLOCK :
* ------------------------------------------------------------ - - - -
* 0        2        4       6          8       9      10    8+XBLOKS
*
* SET TIME OF LATEST DATA STORED TO CLOCK TIME AT START OF RUN; IT WILL
* BE RESET TO THE LATEST CENTURY-HOUR STORED WHENEVER THAT IS GREATER.
*
***********************************************************************
      CALL DATIM(NOW)
      CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
      HOURNOW=(CENDAY-1)*24+NOW(5)           ! CURRENT CENTURY-HOUR
*
      IF (IFT.NE.MAPFT) THEN
        READ (IFT,REC=1,IOSTAT=IORC) MAPHDR,                       !2.0
     &                MAP(1:BLKSIZ-8-8-4000-4),                    !2.0
     &                RANGE_CENTHR,NB,(BLOKID(I),I=1,1000),MAPOVR  !2.0
        IF (IORC.NE.0) CALL SYSABN(901)                            !2.0
        MAPFT=IFT

        BLOCKS=ICHAR(MAPHDR(1:1))*256+ICHAR(MAPHDR(2:2))           !2.0
        XBLOKS=ICHAR(MAPHDR(3:3))*256+ICHAR(MAPHDR(4:4))           !2.0
        XHOURS=ICHAR(MAPHDR(5:5))*256+ICHAR(MAPHDR(6:6))           !2.0
        AMSTAR=ICHAR(MAPHDR(7:7))*256+ICHAR(MAPHDR(8:8))           !2.0

        OFLOWS=ICHAR(MAPOVR(1:1))*256+ICHAR(MAPOVR(2:2))           !2.0
        INDENT=ICHAR(MAPOVR(3:3))*256+ICHAR(MAPOVR(4:4))           !2.0

        NSEQBL=ICHAR(MAP(BLOCKS:BLOCKS))                           !2.0
        IF(NSEQBL.GT.0)THEN
          NSQ=1
        ELSE
          NSQ=0
        ENDIF

        NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
        IF (LATEST.EQ.0) LATEST=HOURNOW      ! LATEST CENTURY-HOUR
      ENDIF
***********************************************************************
*
* COMPLETE TIME FIELDS IN INDEX ENTRY (TIME & TIME OF RECEIPT),
* FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.
* TIME OF RECEIPT IS IN MINUTES FROM THE SLOT TIME (HOURNOW-SLOTHR,
* WHERE HOURNOW IS THE CURRENT CENTURY-HOUR) AND CAN BE <0 OR >1440.
*
***********************************************************************

      INDHOR=MOD(DATIME(4)+24-AMSTAR,XHOURS) ! HOUR MINUS SLOT START!2.0
      IF (MOD(ICHAR(ENTRY(7:7)),16).EQ.0) THEN
        ENTRY(1:1)=CHAR(INDHOR)              ! HOUR RELATIVE TO SLOT
      ELSE                                   ! IF COR NUMBER NONZERO,
        ENTRY(1:1)=CHAR(INDHOR+128)          ! SET COR IN SAME BYTE.
      ENDIF
      ENTRY(2:2)=CHAR(DATIME(5))             ! MINUTES
*
      IF (DATIME(1).LT.1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))   !H
*
      CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
      CENTHR=(CENDAY-1)*24+DATIME(4)         ! CENTURY-HOUR OF DATA
      SLOTHR=CENTHR-INDHOR                   ! SLOT START (CENTURY-HOUR)
*
      ITOR=HOURNOW-SLOTHR                    ! TOR HOUR RELATIVE TO SLOT
      CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! DAY OF MONTH FOR SLOT
      SLOTHR=MOD(SLOTHR,24)                  ! CENT-HOUR TO HOUR OF DAY
      CURENT=SLODAY*256+SLOTHR               ! TAG FOR OB TO BE STORED
*
      ITOR=ITOR*60+NOW(4)                    ! CONVERT TOR TO MINUTES
      IF (ITOR.LT.0) ITOR=65536+ITOR         ! TWOS COMPLEMENT     !2.0
      ENTRY(18:18)=CHAR(ITOR/256)            ! HALFWORD IN ENTRY   !2.0
      ENTRY(19:19)=CHAR(MOD(ITOR,256))       ! HALFWORD IN ENTRY   !2.0
*
* IF THE DATA IS FOR TOO LONG BEFORE THE THE MOST RECENT DATA STORED
* ('TOO LONG' MEANS MORE THAN THE PERIOD COVERED BY THE DATA BASE),
* THEN REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH OLD DATA!).
* IF THE NEW DATA IS MORE RECENT, UPDATE THE LATEST HOUR - AND THE
* INDEX BLOCK TAG FOR WHICH THE LIST OF RANGES CAN BE UPDATED (SET
* WHEN A NEW INDEX BLOCK IS STARTED, BUT THIS COPES UNTIL THEN...).
*
      IF (CENTHR.LE.LATEST-(XBLOKS-1)*XHOURS) THEN
        PRINT *,LATEST-CENTHR,'HOURS OLD   ',
     &          BULL(:MIN(60,LEN(BULL)))                           !2.0
        RETURN
      ENDIF
*
      IF (CENTHR.GT.LATEST) THEN
        LATEST=CENTHR
      ENDIF
***********************************************************************
*
* THE INDEX IS DIVIDED INTO N-HOURLY SEGMENTS. WORK OUT WHICH SEGMENT
* FROM THE CENTURY-HOUR AND READ IN THE CORRESPONDING INDEX BLOCK.
*
* INDEX BLOCK:
* ------------------------------------------------- - - - - - ---------
* : DATE/ : NO. OF : NO. OF  : 23-BYTE : 23-BYTE :             : OVER :
* : TIME  : ENTRIES: REPORTS :  ENTRY  :  ENTRY  : ENTRIES...  : FLOW :
* ------------------------------------------------- - - - - - ---------
* 0       2        4         6        29        52         LAST 2 BYTES
*
***********************************************************************

      NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2 ! index block no. !2.0
      IF (IFT.NE.INDXFT .OR. NINDEX.NE.NXBLOK) THEN
        NINDEX=NXBLOK
        IX=NINDEX                            ! TO VARY IN READ LOOP
        NIND=1                               ! CONTINUATION NUMBER
   30   NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
        READ (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,                  !2.0
     &                    (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR  !2.0

        IF (IORC.NE.0) CALL SYSABN(902)                            !2.0

        TIMTAG=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))           !2.0
        NTRIES=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))           !2.0
        NREPS =ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))           !2.0

        NOFLOW(NIND)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))     !2.0

        INDXFT=IFT
        IF (NOFLOW(NIND).GT.0) THEN          ! INDEX OVERFLOW?
          IX=NOFLOW(NIND)                    ! IF SO, RESET BLOCK NO
          NIND=NIND+1                        ! FURTHER CONTINUATION
          IF(NIND.GT.MAXOFL)THEN
            PRINT*,' MAX INDEX OVERFLOW REACHED',IFT,'IS FT NUMBER'
            RETURN
          ENDIF
          GO TO 30                           ! & READ MORE ENTRIES.
        ENDIF
      ENDIF
***********************************************************************
*                                                                     *
* THE CODE THAT FOLLOWS DIFFERS FROM ERSREP IN TWO IMPORTANT WAYS:    *
* 1) AN INDEX ENTRY POINTS NOT TO A SINGLE REPORT, BUT TO A SET OF    *
*    CHAINED REPORTS FOR THE SAME AIRFIELD                            *
* 2) REPORTS ARE NOT STORED SEQUENTIALLY, BUT IN SUCH A WAY THAT      *
*    REPORTS FROM THE SAME AIRFIELD STAY IN THE SAME BLOCK, AND       *
*    AIRFIELDS IN THE SAME REGION STAY CLOSE TOGETHER.  THIS IS       *
*    DONE BY ASSIGNING A RANGE OF AIRFIELD IDENTIFIERS TO A BLOCK     *
*    ON THE BASIS OF THE NUMBERS OF REPORTS RECEIVED IN THE PERIOD    *
*    COVERED BY THE LAST INDEX BLOCK.                                 *
*                                                                     *
***********************************************************************
* THE STRUCTURE OF THE FOLLOWING CODE IS:                             *
*                                                                     *
* 100 IF (NO CURRENT INDEX BLOCK EXISTS) THEN                         *
*       RELEASE OLD DATA BLOCKS & SET UP NEW ALLOCATION TABLE:        *
*         110 RELEASE DATA & INDEX BLOCKS BY ZEROING MAP BYTES.       *
*         120 DECIDE WHICH PAST INDEX BLOCK TO USE FOR DISTRIBUTION.  *
*         130 READ IT IN (ALLOWING FOR OVERFLOW).                     *
*             SORT INDEX ENTRIES INTO IDENTIFIER ORDER.               *
*         140 BREAK THIS LIST INTO IDENTIFIER RANGES WITH ROUGHLY     *
*             EQUAL NUMBERS OF REPORTS.                               *
*     ENDIF                                                           *
*                                                                     *
* 200 IF (AIRFIELD IS INDEXED) THEN                                   *
*         210 ALLOCATE SAME DATA BLOCK & CHAIN POINTERS;              *
*             KEEP NUMBER OF INDEX ENTRY FOR USE LATER.               *
*     ELSE                                                            *
*         220 FIND WHICH RANGE THE IDENTIFIER IS IN                   *
*         230 & FIND THE CORRESPONDING BLOCK NUMBER FROM THE MAP;     *
*             SET THE POINTER TO ZERO.                                *
*         240 FIND THE POSITION EITHER FROM THE LIST MADE AT THE START*
*             OR BY LOOKING UP STATION MASTER.                        *
*     ENDIF                                                           *
*                                                                     *
* 310 READ IN THE DATA BLOCK                                          *
*                                                                     *
* 320 IF THERE'S AN INDEX ENTRY, CHECK FOR DUPLICATE: RETURN IF SAME. *
*     IF (THERE IS ROOM IN THE DATA BLOCK) THEN                       *
*       PUT REPORT IN DATA BLOCK, INSERTING IT IN THE CHAIN IN        *
*       TIME ORDER, AND ADD OR UPDATE INDEX ENTRY.                    *
*       IF THE EXTRA ENTRY MEANS ANOTHER INDEX BLOCK, CLAIM AN        *
*        OVERFLOW BLOCK FOR WHEN THE INDEX IS WRITTEN OUT.            *
*     ELSE                                                            *
* 400   SPLIT THE DATA BLOCK AS FOLLOWS:                              *
*         410 LIST NUMBER OF REPORTS FOR EACH IDENTIFIER IN BLOCK.    *
*         420 SORT INTO IDENTIFIER ORDER & SPLITTING THE LIST TO      *
*              GIVE ROUGHLY EQUAL NUMBERS IN BOTH BLOCKS.             *
*         430 MOVE OBS IN THE 2ND HALF OF THE LIST TO THE NEW BLOCK,  *
*              FOLLOWING CHAINS; ZERO THESE LENGTHS IN THE OLD BLOCK. *
*         440 WRITE OUT NEW BLOCK & INDEX WITH POINTERS TO IT.        *
*         450 COMPRESS OLD BLOCK BY MOVING REMAINING REPORTS TO END,  *
*              BUT LEAVING ZERO LENGTHS SO THAT NO CHANGE TO POINTERS.*
*         460 WRITE BACK OLD BLOCK & UPDATE LIST OF IDENTIFIER RANGES *
*         470  MADE AT 140 TO REFLECT THE SPLIT.                      *
*       GO BACK TO 200 TO STORE REPORT                                *
*     ENDIF                                                           *
* 500 WRITE OUT INDEX & DATA BLOCKS & MAP BLOCK IF UPDATED            *
*                                                                     *
***********************************************************************
*
* THE TIME TAG (DATE/TIME IN INDEX BLOCK) IS (DAY OF MONTH)*256+HOUR.
* THE TIME TAG IS THE SLOT THIS SEGMENT WAS LAST USED FOR.  IF IT WAS
* LAST USED FOR THE OLDEST DATA IN THE BANK, FREE THE BLOCKS ATTACHED
* TO IT BEFORE STORING NEW DATA.
*
* NON-INDEX BLOCK:    (LENGTHS OF RECORDS AT START, DATA ITSELF AT END)
* ---------------------------------- - - - - - -----------------------
* :TIME: NUM OF : LENGTH : L1 : L2 :     FREE       : SECOND : FIRST  :
* :TAG : RECORDS:  FREE  :    :    :     SPACE      : RECORD : RECORD :
* ---------------------------------- - - - - - -----------------------
* 0    2        4        6    8   10            END-L1-L2  END-L1   END
*
      IF (TIMTAG.NE.CURENT) THEN
        ZEROBL=1                                                  !1.16
  110   N=INDEX(MAP(XBLOKS+ZEROBL:BLOCKS-1-NSQ),CHAR(NINDEX))     !1.16
        IF (N.GT.0) THEN
          ZEROBL=ZEROBL+(N-1)                                     !1.16
          MAP(XBLOKS+ZEROBL:XBLOKS+ZEROBL)=CHAR(0)                !1.16
          IF (1+XBLOKS+ZEROBL.LT.BLOCKS) GO TO 110                !1.16
        ENDIF
*                      NOW DO SAME FOR INDEX OVERFLOWS (TOP BIT SET)
        ZEROBL=1                                                  !1.16
  111   N=INDEX(MAP(XBLOKS+ZEROBL:BLOCKS-1-NSQ),CHAR(128+NINDEX)) !1.16
        IF (N.GT.0) THEN
          ZEROBL=ZEROBL+(N-1)                                     !1.16
          MAP(XBLOKS+ZEROBL:XBLOKS+ZEROBL)=CHAR(0)                !1.16
          IF (1+XBLOKS+ZEROBL.LT.BLOCKS) GO TO 111                !1.16
        ENDIF
*
* CHOOSE A PREVIOUS INDEX BLOCK TO ALLOCATE AIRFIELDS TO DATA BLOCKS.
* FIND THE BIGGEST COUNT IN INDEX BLOCKS FOR THE SAME TIME (THE MOST
* RECENT IF SEVERAL HAVE THE SAME ROUGH COUNT IN THE MAP)
*
        N=XBLOKS                                                   !2.0
        NMAX=0                               ! MAX COUNT              !D
        DO I=1,XBLOKS*XHOURS/24              ! I: DAYS BACK           !D
          NOLDIX=MOD((CENTHR-I*24-INDHOR)/XHOURS,N)+2 ! OLD INDEX BLOCK
          NOLD=ICHAR(MAP(NOLDIX-1:NOLDIX-1)) ! COUNT FROM MAP         !D
          IF (NOLD.GT.NMAX) THEN             ! IF COUNT IS BIGGER,    !D
            NMAX=NOLD                        ! KEEP COUNT TO CHECK    !D
            LASTIX=NOLDIX                    ! & KEEP BLOCK NUMBER    !D
          ENDIF                              ! TO READ FOR DISTRIBUTION
        ENDDO
*
* IF ALL INDEX BLOCKS FOR THE SAME TIME HAVE ZERO COUNTS, JUST WORK
* BACK AN INDEX BLOCK AT A TIME,
* AVOIDING LOW TOTALS & VERY HIGH ONES BY TAKING A BLOCK WITH A TOTAL
* BETWEEN THE MEDIAN & THE UPPER QUARTILE
*
        IF (NMAX.EQ.0) THEN                                           !D
          BLOCKX(BLKSIZ-N+1:BLKSIZ)=MAP(1:N) ! TO WORK AREA FOR SORT
          CALL SORTCH(BLOCKX(BLKSIZ-N+1:BLKSIZ-N+1),1,N,MASK(3:3)) !1.8
          MEDIAN=BLOCKX(BLKSIZ-N/2:BLKSIZ-N/2) ! MIDDLE COUNT AFTER SORT
          QUARTL=BLOCKX(BLKSIZ-N/4:BLKSIZ-N/4)
*
          IF (MEDIAN.EQ.CHAR(0)) THEN        ! USE MAX IF ZERO MEDIAN
            LASTIX=1+INDEX(MAP(1:N),BLOCKX(BLKSIZ:BLKSIZ))
          ELSE
            I=1                              ! BLOCKS TO GO BACK      !D
  120       J=NINDEX-I                                                !D
            IF (J.LE.1) J=XBLOKS+J           ! WRAPPING ROUND IF NEEDED
            IF (J.LE.1 .OR.                  ! IF TOO FEW INDEX BLOCKS
     &        MAP(J-1:J-1).LT.MEDIAN .OR.    ! OR COUNT TOO SMALL
     &        MAP(J-1:J-1).GT.QUARTL) THEN   ! OR COUNT TOO BIG,
              I=I+1                          ! GO BACK A(NOTHER) BLOCK
              IF (I.LE.XBLOKS) GO TO 120     ! IF ANY NOT YET TRIED...
            ENDIF                            ! N.B. J=1:N, NINDEX=2:N+1
            LASTIX=J                         ! INDEX BLOCK TO USE
          ENDIF
        ENDIF                                                         !D
*
* READ IN THE INDEX BLOCK(S) FOR THE PERIOD CHOSEN ABOVE FOR ITS COUNT.
*
        IX=LASTIX                            ! TO VARY IN READ LOOP
        NIND=1                               ! CONTINUATION NUMBER
  130   NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
        READ (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,                  !2.0
     &                    (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR  !2.0

        IF (IORC.NE.0) CALL SYSABN(903)                            !2.0

        TIMTAG=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))           !2.0
        NTRIES=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))           !2.0
        NREPS =ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))           !2.0

        NOFLOW(NIND)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))     !2.0

        IF (NOFLOW(NIND).GT.0) THEN          ! INDEX OVERFLOW?
          IX=NOFLOW(NIND)                    ! IF SO, RESET BLOCK NO
          NIND=NIND+1                        ! FURTHER CONTINUATION
          IF(NIND.GT.MAXOFL)THEN
            PRINT*,' MAX INDEX OVERFLOW REACHED',IFT,'IS FT NUMBER'
            RETURN
           ENDIF
          GO TO 130                          ! & READ MORE ENTRIES.
        ENDIF
*
* BREAK THE LIST OF INDEX ENTRIES INTO NB PARTS WITH ROUGHLY EQUAL
* NUMBERS OF REPORTS (LOB=NREPS/NB), WHERE NB IS THE NUMBER OF DATA
* BLOCKS (NOT COUNTING OVERFLOWS); KEEP THE IDENTIFIER RANGES.
*
* IF THERE ARE NO ENTRIES IN THE PREVIOUS INDEX BLOCK, I.E. WE ARE
* STARTING UP, ASSIGN ONLY ONE BLOCK AND THEN SPLIT WHEN NECESSARY;
* THE NEXT INDEX BLOCK WILL BE ABLE TO USE THIS DISTRIBUTION.
*
* Set start of first range and start of last to hex zeros & hex    !2.0
* ones (255 in each character), the maximum possible range.        !2.0
*
* N.B. BLOKID IS A LIST OF PAIRS OF IDENTIFIERS DEFINING RANGES RATHER
* THAN SINGLE IDENTIFIERS AS BREAK POINTS. THIS MAKES IT EASIER TO UP-
* DATE: IF A RANGE IS SPLIT THE SECOND HALF CAN JUST BE PUT AT THE END.
*
        NB=(BLOCKS-1-NSQ-XBLOKS-OFLOWS)/XBLOKS
        IF (NTRIES.EQ.0) NB=1
        BLOKID(1)=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)               !2.0
        BLOKID(NB*2)=CHAR(255)//CHAR(255)//CHAR(255)//CHAR(255)    !2.0
*
        IX=INDEX(MAP(XBLOKS+1:BLOCKS-1-NSQ),CHAR(0))  ! GET FIRST
        MAP(XBLOKS+IX:XBLOKS+IX)=CHAR(NINDEX)         ! DATA BLOCK
*
        IF (NB.GT.1) THEN
          L=INDENT                                                 !2.0
          N=NTRIES                                                 !2.0
          CALL SORTCH(INDEKS(1),L,N,MASX)    ! SORT IN COUNT ORDER !1.8
*
* THE SORT PUTS ENTRIES WITH BIG COUNTS AT THE END OF THE LIST.
* ANY IDENT WITH MORE THAN NREPS/NB WILL BE ASSIGNED A WHOLE BLOCK.
* THE REST WILL BE RE-SORTED IN IDENT ORDER & SHARED OUT BETWEEN
* THE REMAINING BLOCKS.
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
*
          NL=N                               ! NUMBER OF ENTRIES
          LOB=NREPS/NB                       ! MEAN OBS PER BLOCK
  135     NREP=ICHAR(INDEKS(NL)(12:12))      ! OBS FOR LAST IDENT
          IF (NREP.GT.LOB) THEN              ! IF COUNT > MEAN,
            NL=NL-1                          ! LEAVE ENTRY AT END
            NREPS=NREPS-NREP                 ! & IGNORE ITS COUNT.
            IF (NL.GT.1) GO TO 135           ! BACK PAST BIG COUNTS
          ENDIF
*                                            ! SORT SMALLER COUNTS
          CALL SORTCH(INDEKS(1),L,NL,MASK)   ! IN ORDER OF IDENT   !1.8
*                                            ! & RECALCULATE MEAN
          LOB=NREPS/(NB-(N-NL))              ! IGNORING BIG COUNTS.
          K=1
          NOB=0
          DO I=1,NTRIES                                            !2.0
            NOB=NOB+ICHAR(INDEKS(I)(12:12))
            IF (NOB.GE.LOB*K .AND. K.LT.NB) THEN
              BLOKID(K*2)=INDEKS(I)(3:6)     ! END OF ONE RANGE
              BLOKID(K*2+1)=INDEKS(I)(3:6)   ! START OF NEXT - SAME
              K=K+1                          ! ONE MORE RANGE DONE
              IX=INDEX(MAP(XBLOKS+1:BLOCKS-1-NSQ),CHAR(0))
              MAP(XBLOKS+IX:XBLOKS+IX)=CHAR(NINDEX)
            ENDIF
          ENDDO                                                    !2.0
        ENDIF
*
* FINALLY REINITIALISE THE INDEX BUFFER FOR THE NEW HOUR.
* KEEP THE TIME TAG FOR THIS LIST IN CASE A BLOCK IS SPLIT: THE RANGES
* WILL ONLY BE UPDATED FOR DATA WITH THIS TAG, FOR THE LATEST PERIOD.
*
        TIMTAG=CURENT
        NTRIES=0
        NREPS=0
        NOFLOW(1)=0
*
        RANGE_CENTHR=CENTHR                                       !1.11c
      ENDIF
***********************************************************************
*
* IF THE INDEX IS NOT EMPTY (IF DATA ALREADY STORED FOR THIS HOUR), SEE
* IF THERE IS ALREADY AN ENTRY FOR THIS STATION.  IF SO, TRY TO STORE
* THE REPORT IN THE SAME DATA BLOCK.  MAKE THE INDEX ENTRY POINT TO THE
* NEW REPORT, AND SET THE POINTER ON THE END OF THIS REPORT TO POINT TO
* THE PREVIOUS REPORT, I.E. COPY THE OLD POINTER FROM THE INDEX.  ADD 1
* TO THE NUMBER OF REPORTS CHAINED TO THIS INDEX ENTRY.
*
***********************************************************************
*
* BRANCH BACK TO 200 TO STORE OB IF BLOCK WAS FULL & HAD TO BE SPLIT
*
  200 IND=0
      IF (IFT.EQ.21) THEN                                         !1.14
        CALL UADUPS(INDEKS,NTRIES,IDENT,ENTRY,IND)                 !2.0
        IF (IND.GT.0) POINTER=INDEKS(IND)(20:23)                   !2.0
      ELSE                                                        !1.14
        J = LEN(IDENT)     ! Length of identifier                    !2
        J = MIN0(J,9)      ! Don't use >9 characters                 !2
        DO I=1,NTRIES                                             !1.14
          IF (INDEKS(I)(3:J+2) .EQ. IDENT(1:J)) THEN                 !2
            POINTER=INDEKS(I)(20:23)                               !2.0 B?
            IND=I
          ENDIF
        ENDDO                                                     !1.14
      ENDIF                                                       !1.14

      POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))           !2.0
      POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))           !2.0
      NBLOC=POINTB                                                 !2.0
*
* IF NOT, ALLOCATE A BLOCK NUMBER FROM THE LIST, USING THE IDENTIFIER.
* FIRST FIND WHICH OF THE CURRENT NB RANGES THE IDENTIFIER IS IN.
* (THE RANGE INCLUDES THE ENDPOINT BUT NOT THE STARTING POINT)
*
      IF (IND.EQ.0) THEN
        DO J=1,NB                                                  !2.0
          IF (IDENT(1:4).GT.BLOKID(J*2-1) .AND.
     &        IDENT(1:4).LE.BLOKID(J*2)) GO TO 221
        ENDDO                                                      !2.0
  221   IX=1
        JRANGE=J                                                !1.11d
*
* THEN GET THE CORRESPONDING DATA BLOCK NUMBER FOR THIS INDEX BLOCK
* FROM THE MAP (WE CAN STORE REPORTS FOR LOTS OF INDEX BLOCKS AT THE
* SAME TIME, BUT THE LIST ONLY APPLIES STRICTLY TO THE LATEST.
* HOWEVER, IF EARLIER TIMES ALREADY HAVE MOST STATIONS INDEXED, THIS
* SHOULDN'T LEAVE THE SPREAD OF STATIONS OVER DATA BLOCKS TOO UNTIDY.)
* IF THERE ARE MORE RANGES THAN DATA BLOCKS ALLOCATED TO THIS INDEX
* BLOCK, THEN USE THE LAST DATA BLOCK FOUND (& SPLIT IT WHEN FULL).
*
        DO I=1,J
          IF (IX.GT.BLOCKS-1-NSQ-XBLOKS) GO TO 231
          IY=INDEX(MAP(XBLOKS+IX:BLOCKS-1-NSQ),CHAR(NINDEX))
          IF (IY.EQ.0) GO TO 231
          IX=IX+IY
        ENDDO
  231   NBLOC=IX-1
*
* AS THERE IS ONLY ONE REPORT FOR THIS AIRFIELD, ZERO THE POINTER ON
* THE END OF THE REPORT.  (A POINTER IS RECORD/BLOCK, AS IN THE INDEX.)
*
        POINTR=0
        POINTB=0
        POINTER=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)                 !2.0
*
* TAFS & METARS ONLY: SEE IF A MISSING POSITION IS IN THE LIST MADE AT
* THE START, PUT LAT/LONG IN ENTRY IF IT'S THERE OR IN STATION MASTER.
*
!----------------------------------------------------------------------
! For TAFS/METARS always call the ICAO abbreviated list ICOBRV.
!----------------------------------------------------------------------

        IF (IFT.EQ.3 .OR. IFT.EQ.4 .OR. IFT.EQ.7) THEN             !1.10
          CALL ICOBRV (IDENT(1:4), POSITION, IHT, RC)                !2
          IF (RC .NE. 0) THEN
            PRINT *,'TAFREP: Failed to find ',IDENT(1:5),' in ICAO List')
          ELSE
            CALL INDLALO(ENTRY,POSITION(1),POSITION(2))            !1.13
          ENDIF
        ENDIF
      ENDIF
***********************************************************************
*
* IF THERE'S ROOM IN THE BLOCK, STORE THE REPORT (AFTER CHECKING FOR
* DUPLICATES) & INDEX IT. INDEX DETAILS ARE FOR THE LATEST REPORT,
* THOSE BETWEEN REPORT & POINTER FOR THE REPORT THEY FOLLOW.
*
***********************************************************************
*
* FIRST READ IN THE DATA BLOCK.  (IT MAY HAVE BEEN ALLOCATED WHEN THE
* INDEX BLOCK WAS STARTED & STILL BE EMPTY - CAN'T TELL WITHOUT READ)
*
* ABEND rather than just issue warning if tags don't match and    !1.15
* index entry found, in which case they should match.             !1.15
* It may be a variable in core that's wrong, not the data set.    !1.15
* If so, carrying on could corrupt a data set that's still OK.    !1.15
*
  300 IF (IFT.NE.MESSFT .OR. NBLOCK.NE.NBLOC) THEN
        NBLOCK=NBLOC
        IF (NBLOCK.LE.0) THEN                                     !1.15
          PRINT *,'TAFREP: data block number not positive',NBLOCK !1.15
          CALL SYSABN(904)                                         !2.0
        ENDIF                                                     !1.15

        READ (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC)             !2.0
     &         BLOCK(1:BLKSIZ)                                     !2.0
        IF (IORC.NE.0) CALL SYSABN(905)                            !2.0
        MESSFT=IFT

        BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))             !2.0
        NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))             !2.0
        LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))               !2.0

        DO I=1,NINBLK
          RECLEN(I)=ICHAR(BLOCK(6+I*2-1:6+I*2-1))*256              !2.0
     &             +ICHAR(BLOCK(6+I*2:6+I*2))                      !2.0
        ENDDO

        IF (NINBLK.EQ.0 .OR. BLKTAG.NE.CURENT) THEN
          IF (IND.GT.0) THEN                                      !1.11
            PRINT *,'TAFREP: index points to data block that is'  !1.11
            PRINT *,'unstarted',INDEKS(IND),NINBLK,BLKTAG,CURENT  !1.11
            PRINT *,IFT,'is FT number'                            !1.12
            CALL SYSABN(906)                                       !2.0
          ELSE                                                    !1.15
            NINBLK=0
            BLKTAG=CURENT
            LEFT=BLKSIZ-INDHED
            BLOCK(:BLKSIZ)=' '
          ENDIF                                                   !1.15
        ELSE
*
* WORK OUT THE DISPLACEMENTS CORRESPONDING TO THE RECORD LENGTHS:
* (RECDIS(I) IS THE START OF THE I-TH REPORT FROM THE END OF THE BLOCK)
*
          RECDIS(1)=RECLEN(1)
          DO I=2,NINBLK                                            !2.0
            RECDIS(I)=RECDIS(I-1)+RECLEN(I)
          ENDDO                                                    !2.0
        ENDIF
      ENDIF
*
* IF THERE IS AN INDEX ENTRY (IN WHICH CASE IND IS NONZERO),
* CHECK FOR DUPLICATES, FOLLOWING CHAIN AS LONG AS POINTER (RECORD
* NUMBER - ALL IN SAME BLOCK) IS NONZERO. COMPARE THE REPORT TEXTS
* AND ALSO THE HOURS/MINUTES THAT FOLLOW (INCLUDING THE COR FLAG).
* (N.B. THERE WON'T BE EQUALITY UNLESS THE LENGTHS ARE EQUAL.)
*
* FIRST SEE IF THE REPORT IS OUT OF ORDER (EARLIER THAN THE LATEST
* REPORT INDEXED).  IF SO, IT WILL BE INSERTED IN THE CHAIN IN TIME
* ORDER: NOTE THE RECORD NUMBERS OF THE REPORTS BEFORE & AFTER IT
* IN TIME DURING THE DUPLICATE SEARCH & LATER SET THE POINTERS
* ACCORDINGLY.  (N.B. THE NEW REPORT MAY BE THE EARLIEST IN THE
* CHAIN, IN WHICH CASE ONLY ONE RECORD NUMBER WILL BE SET.)
* THE HOUR AND MINUTE FIELDS IN INDEX ENTRIES MUST BE COMPARED
* SEPARATELY, BECAUSE THE HOUR MAY HAVE FLAGS SET IN THE SAME BYTE.
*
      IF (IND.GT.0) THEN
        LASTHR=MOD(ICHAR(INDEKS(IND)(1:1)),32)
        IF (LASTHR.GT.INDHOR .OR.
     &    (LASTHR.EQ.INDHOR .AND. INDEKS(IND)(2:2).GT.ENTRY(2:2))) THEN
          INSERT=.TRUE.
        ELSE
          INSERT=.FALSE.
        ENDIF
*
* NBEFORE & NAFTER MAY BE SET IF THIS IS NOT THE LATEST REPORT IN THE
* CHAIN, NPREF IF THERE IS ALREADY A PREFERRED REPORT FOR THIS TIME.
*
        NBEFORE=0
        NAFTER=0
        NPREF=0
*
* LOOP BACK TO HERE TO LOOK AT NEXT REPORT IN CHAIN
*
  320   DISPL=BLKSIZ-RECDIS(POINTR)
        L=RECLEN(POINTR)
        RECNO=POINTR
*
* THE DUPLICATE CHECK BELOW COPES WITH METARS WHICH ARE IDENTICAL EXCEPT
* THAT ONE HAS A TIME GROUP AFTER THE IDENTIFIER AND THE OTHER NOT.
*
        LN=INDEX(BULL,BUFR)-1      ! LENGTH OF REPORT IN CHARACTERS
        IF (LN.LE.0) LN=LEN(BULL)  ! (TOTAL LENGTH IF NO BUFR MESSAGE)
        LENBUF=LEN(BULL)-LN        ! LENGTH OF BUFR MESSAGE (MAY BE 0)
*
* IF TIMES ARE SAME (FIRST TWO BYTES OF INDEX ENTRY), THEN
*   IF LENGTHS ARE SAME (CHARACTERS ONLY, NOT BUFR MESSAGE OR TRAILER),
*     THEN DUPLICATES IF WHOLE STRING IDENTICAL;
*   if lengths differ by 6 or 8,                                  !2.0a
*     then duplicates if longer string has hhmmZ or ddhhmmZ       !2.0a
*       after the identifier & the rest of the string the same.   !2.0a
*
!       If it's upper air & the same part (bottom bits of byte 17, !2.1
!                                  TEMP/PILOT bit as well as part) !2.1
!       or not upper air & same hour (ignoring COR bit) & minute   !2.1
!           (for upper air all parts in chain are for same ascent, !2.1
!            but some may have launch time set & others not,       !2.1
!            so can't safely compare times!)                       !2.1
!       then it could be a duplicate.                              !2.1

        LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)           !I
        IF ((IFT.EQ.21 .AND. (MOD(ICHAR(ENTRY(17:17)),16).EQ.      !2.1
     &          MOD(ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)),16))) .OR.   !2.1
     &      (IFT.NE.21 .AND. LASTHR.EQ.INDHOR .AND.                !2.1
     &      ENTRY(2:2).EQ.BLOCK(DISPL+L-21:DISPL+L-21))) THEN      !2.1

! same length & identical strings - so duplicates                 !2.0a

          IF (LN-(L-LENBUF-23).EQ.0) THEN
            IF (BULL(1:LN).EQ.BLOCK(DISPL+1:DISPL+LN)) RETURN

! hhmmZ in new but not old, otherwise same - so duplicates        !2.0a

          ELSE IF (LN-(L-LENBUF-23).EQ.6) THEN
            IF (BULL(10:11).EQ.'Z '.AND.
     &          BULL(11:LN).EQ.BLOCK(DISPL+5:DISPL+LN-6)) RETURN

! ddhhmmZ in new but not old, otherwise same - so duplicates      !2.0a

          ELSE IF (LN-(L-LENBUF-23).EQ.8) THEN                    !2.0a
            IF (BULL(12:13).EQ.'Z '.AND.                          !2.0a
     &          BULL(13:LN).EQ.BLOCK(DISPL+5:DISPL+LN-8)) RETURN  !2.0a

! hhmmZ in old but not new, otherwise same - so duplicates        !2.0a

          ELSE IF (LN-(L-LENBUF-23).EQ.-6) THEN
            IF (BLOCK(DISPL+10:DISPL+11).EQ.'Z '.AND.
     &          BULL(5:LN).EQ.BLOCK(DISPL+11:DISPL+LN+6)) RETURN

! ddhhmmZ in old but not new, otherwise same - so duplicates      !2.0a

          ELSE IF (LN-(L-LENBUF-23).EQ.-8) THEN                   !2.0a
            IF (BLOCK(DISPL+12:DISPL+13).EQ.'Z '.AND.             !2.0a
     &          BULL(5:LN).EQ.BLOCK(DISPL+13:DISPL+LN+8)) RETURN  !2.0a
          ENDIF
*
* SEE IF PREFERRED FLAG IS SET (RIGHT-HAND BIT OF BYTE 17 IN TRAILER)
* AND IF SO KEEP THE RECORD NUMBER SO THAT THE COUNTS CAN BE COMPARED
* AND THE NEW REPORT PREFERRED IF IT IS BETTER.
*
          IF (ICHAR(BLOCK(DISPL+L-6:DISPL+L-6)).GE.128) THEN
            NPREF=RECNO
          ENDIF
        ENDIF
*
        POINTER=BLOCK(DISPL+L-3:DISPL+L)
        POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))         !2.0
        POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))         !2.0
*
* IF INSERT HAS BEEN SET, GO BACK PAST AT LEAST ONE REPORT (THAT POINTED
* TO BY THE INDEX ENTRY WHICH SET INSERT) UNTIL A REPORT FOR AN EARLIER
* TIME (OR THE SAME TIME) IS FOUND.  KEEP THE LAST RECORD NUMBER PASSED.
*
        IF (INSERT) THEN
          LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
          IF (LASTHR.GT.INDHOR .OR. (LASTHR.EQ.INDHOR .AND.
     &        BLOCK(DISPL+L-21:DISPL+L-21).GT.ENTRY(2:2))) THEN
            NAFTER=RECNO
          ELSE
            INSERT=.FALSE.
          ENDIF
        ENDIF
*
* LOOP ROUND UNLESS REPORTS ARE CHAINED ACROSS BLOCKS
* (ONLY 'DREG' CHAINS UNDER A NULL IDENTIFIER ARE LIKELY TO BE LONG
* ENOUGH TO CROSS BLOCKS.)  AT END, RESET POINTER TO START OF CHAIN.
*
        IF (POINTR.GT.0 .AND. POINTB.EQ.NBLOCK) GO TO 320
        POINTER=INDEKS(IND)(20:23)
        POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))         !2.0
        POINTB=ICHAR(POINTER(3:3))*256+ICHAR(POINTER(4:4))         !2.0
      ENDIF
*
* IF A PREFERRED REPORT FOR THE SAME TIME HAS BEEN FOUND, COMPARE THE
* NUMBER OF GOOD VALUES AND THE COR NUMBER AND RESET THE PREFERRED
* FLAG IF THE NEW REPORT IS BETTER.
* SET THE "HISTORICAL" PREFERRED FLAG (X'20', SO ADD 32), NEVER TO BE
* UNSET, TO SHOW THAT THE NEW REPORT HAS BEEN (OR STILL IS) PREFERRED.
*
      IF (IND.GT.0 .AND. NPREF.GT.0) THEN
        DISPL=BLKSIZ-RECDIS(NPREF)+RECLEN(NPREF)-INDLEN   ! --> TRAILER
        IF (BLOCK(DISPL+7:DISPL+7).LT.ENTRY(7:7) .OR.     ! COR FLAG  !I
     &     BLOCK(DISPL+12:DISPL+12).LE.ENTRY(12:12)) THEN ! GOOD VALUES
          IFLAGS=ICHAR(BLOCK(DISPL+17:DISPL+17))          ! OLD FLAGS
          BLOCK(DISPL+17:DISPL+17)=CHAR(IFLAGS-128)       ! UNSET PREF
          IFLAGS=ICHAR(ENTRY(17:17))                      ! NEW FLAGS
          IF (IFLAGS.LT.160) ENTRY(17:17)=CHAR(IFLAGS+128+32)  ! PREF
        ENDIF
      ELSE                                                ! IF NO PREF,
        IFLAGS=ICHAR(ENTRY(17:17))                        ! PREFER THIS
        IF (IFLAGS.LT.160) ENTRY(17:17)=CHAR(IFLAGS+128+32)
      ENDIF
*
* IF THERE'S ROOM IN THE BLOCK & THE REPORT'S NOT A DUPLICATE, STORE IT.
*
      IF (LEN(BULL)+INDENT+2.LE.LEFT) THEN
        NINBLK=NINBLK+1
        L=LEN(BULL)+INDENT
        LEFT=LEFT-L-2

        RECLEN(NINBLK)=L
        BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)               !2.0
        BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))              !2.0

        IF (NINBLK.GT.1) THEN
          RECDIS(NINBLK)=RECDIS(NINBLK-1)+L
        ELSE
          RECDIS(NINBLK)=L
        ENDIF

        START=INDHED+NINBLK*2+LEFT
        BLOCK(START+1:START+L-INDENT)=BULL                        !1.11
        BLOCK(START+L-INDENT+1:START+L-4)=ENTRY(1:INDENT-4)       !1.11
        BLOCK(START+L-3:START+L)=POINTER                          !1.11
***********************************************************************
*
* UPDATE THIS AIRFIELD'S INDEX ENTRY TO POINT TO THIS REPORT, IF IT IS
* THE LATEST.  IF NOT, NAFTER & PERHAPS NBEFORE ARE NONZERO.  MAKE THE
* NAFTER-TH REPORT POINT TO THE NEW ONE & THE NEW ONE TO THE NBEFORE-TH
* OR ZERO (BY TAKING THE POINTER THAT WAS IN THE NAFTER-TH).
* (OR ADD AN ENTRY FOR THIS AIRFIELD IF THERE'S NONE IN THIS SLOT).
*
***********************************************************************
        POINTR=NINBLK
        POINTB=NBLOCK
        POINTER(1:1)=CHAR(NINBLK/256)                              !2.0
        POINTER(2:2)=CHAR(MOD(NINBLK,256))                         !2.0
        POINTER(3:3)=CHAR(NBLOCK/256)                              !2.0
        POINTER(4:4)=CHAR(MOD(NBLOCK,256))                         !2.0
*
* COUNT OF OBS IS ONLY ONE BYTE; LEAVE IT AS 255 IF MORE THAN 255!
* Don't reset time in index unless minutes are less than 60.      !1.14
* Upper air data can have missing minute (=255) and time is       !1.14
* reset in UADUPS if necessary.                                   !1.14
*
        IF (IND.GT.0) THEN
          NOBS=ICHAR(INDEKS(IND)(12:12))
          IF (NOBS.LT.255) INDEKS(IND)(12:12)=CHAR(NOBS+1)
*
          IF (NAFTER.EQ.0) THEN
            IF (ENTRY(2:2).LT.CHAR(60)) THEN                      !1.14
              INDEKS(IND)(1:2)=ENTRY(1:2)
            ENDIF                                                 !1.14
            INDEKS(IND)(18:23)=ENTRY(18:19)//POINTER
          ELSE
            DISPL=BLKSIZ-RECDIS(NAFTER)
            LL=RECLEN(NAFTER)
            BLOCK(START+L-3:START+L)=BLOCK(DISPL+LL-3:DISPL+LL)
            BLOCK(DISPL+LL-3:DISPL+LL)=POINTER
          ENDIF
*
* 'OR' THE FLAG BYTES OF THE CHAINED OBS TO SET THE INDEX ENTRY FLAGS
* 'OR' THE BIT(S) ALREADY IN BYTE 17 WITH THOSE IN THE NEW ENTRY (IF !E
* TAF OR METAR) OR ON THE END OF THE IDENTIFIER (IF UPPER AIR)       !E
* (DO THIS FROM FIRST PRINCIPLES BECAUSE IOR IS NOT PORTABLE!)       !E
*
* FOR UPPER AIR THERE ARE 2 BITS SET TO 1 FOR TEMP & 0 FOR PILOT:    !F
* IF A PILOT APPEARS IN A TEMP CHAIN OR VICE VERSA, SET BOTH BITS.   !F
*
          IF (IFT.EQ.3.OR.IFT.EQ.4.OR.IFT.EQ.7.OR.IFT.EQ.21) THEN !1.11a
            IBITS=ICHAR(INDEKS(IND)(17:17))                          !E
            IF (IFT.EQ.21) THEN                      ! UPPER AIR     !E
              JBITS=ICHAR(IDENT(10:10))                              !E
              ITPBITS=MOD(IBITS/4,3)                 ! BITS 5 & 6    !F
              JTPBITS=MOD(JBITS/4,3)                                 !F
              IF (ITPBITS.EQ.1 .AND. JTPBITS.EQ.0) THEN              !F
                JBITS=JBITS+8+4                      ! SET BOTH BITS !F
              ELSE IF (ITPBITS.EQ.0 .AND. JTPBITS.EQ.1) THEN         !F
                JBITS=JBITS+8                        ! SET BIT 5     !F
              ENDIF                                                  !F
            ELSE
              JBITS=ICHAR(ENTRY(17:17))              ! TAF/METAR     !E
            ENDIF

            IJBITS=0                                                 !E
            DO I=0,7                                                 !E
              IBIT=MOD(IBITS/2**I,2)                                 !E
              JBIT=MOD(JBITS/2**I,2)                                 !E
              IF (IBIT.EQ.1 .OR. JBIT.EQ.1) IJBITS=IJBITS+2**I       !E
            ENDDO
            INDEKS(IND)(17:17)=CHAR(IJBITS)                          !E
          ENDIF                                                      !A
        ELSE
*
* OR MAKE A NEW INDEX ENTRY (PUT IDENT & COUNT OF 1 IN THE INPUT ENTRY)
* (U/A HAS BYTE 17 TO GO IN INDEX AFTER IDENT; ELSE LEN(IDENT)=5 OR 9)
*
          NTRIES=NTRIES+1
          INDEKS(NTRIES)(1:2)=ENTRY(1:2)                             !A
          INDEKS(NTRIES)(3:11)=IDENT                                 !A
          INDEKS(NTRIES)(12:12)=CHAR(1)
          INDEKS(NTRIES)(13:19)=ENTRY(13:19)                         !A
          INDEKS(NTRIES)(20:23)=POINTER                              !A
          IF (LEN(IDENT).GT.9) INDEKS(NTRIES)(17:17)=IDENT(10:10)    !E

! FOR METARS, PUT THE STATION HEIGHT IN BYTES 7&8 OF THE IDENTIFIER  !2

          IF (IFT.EQ.4 .AND. IHT.GT.-999) THEN                       !2
            I = IHT                                                  !2
            IF (I.LT.0) I = I + 65536                                !2
            INDEKS(NTRIES)(9:10) = CHAR(I/256) // CHAR(MOD(I,256))   !2
          END IF                                                     !2
*
* IF THIS ENTRY NEEDS TO START A NEW INDEX BLOCK, IT'S ENOUGH HERE TO
* CLAIM A BLOCK NUMBER FROM THE OVERFLOW POOL AND LIST IT AS THE NEXT
* BLOCK TO CONTINUE THE INDEX; THIS NUMBER WILL BE SET AS A POINTER
* AT THE END OF THE PREVIOUS BLOCK WHEN THE INDEX IS WRITTEN BACK.
*
          NIND=NTRIES/NBLIND               ! NO OF FULL INDEX BLOCKS
          IF (NIND.GE.1 .AND. NTRIES-NIND*NBLIND.EQ.1) THEN
            NX=INDEX(MAP(BLOCKS-OFLOWS:BLOCKS-1-NSQ),CHAR(0))
            IF (NX.EQ.0) THEN
              PRINT *,'NO MORE OVERFLOWS FOR INDEX',IFT,'IS FT NUMBER'
              NTRIES=NTRIES-1
              RETURN
            ELSE
              NOFLOW(NIND)=BLOCKS-OFLOWS+NX
              NOFLOW(NIND+1)=0
              MAP(NOFLOW(NIND)-1:NOFLOW(NIND)-1)=CHAR(128+NINDEX)
            ENDIF
          ENDIF
        ENDIF
*
* UPDATE REPORT COUNT IN INDEX & COPY TO MAP BYTE FOR INDEX BLOCK
* (USE N/256 IN CASE THE COUNT IS LARGE; ADD 1 SO THAT THE MAP BYTE
* IS NONZERO EVEN IF THE COUNT IS SMALL)
*
        NREPS=NREPS+1
        MAP(NINDEX-1:NINDEX-1)=CHAR(NREPS/256+1)
      ELSE
***********************************************************************
*
* IF THERE'S NOT, SPLIT THE DATA MORE OR LESS EQUALLY BETWEEN 2 BLOCKS.
* (GET THE SECOND FROM THE OVERFLOW POOL: BECAUSE THE INDEX OPERATION
* STARTS FURTHER ON, INDEX(...) MUST BE ADJUSTED TO GIVE BLOCK NUMBER!)
* FOR REPORTS IN THE CURRENT BLOCK, LIST AIRFIELD & NUMBER OF REPORTS,
* SORT LIST BY AIRFIELD & SPLIT INTO TWO WITH SIMILAR NUMBERS OF OBS.
*
***********************************************************************
        NX=INDEX(MAP(BLOCKS-OFLOWS:BLOCKS-1-NSQ),CHAR(0))
        IF (NX.EQ.0) THEN
          PRINT *,'NO OVERFLOWS TILL SOME RELEASED',IFT,'IS FT NUMBER'
          RETURN
        ENDIF
        NBLOCX=BLOCKS-OFLOWS-(XBLOKS+1)+NX
        CHARBLX=CHAR(NBLOCX/256)//CHAR(MOD(NBLOCX,256))
*
        K=0
        NTOTAL=0
        CHARBL=CHAR(NBLOCK/256)//CHAR(MOD(NBLOCK,256))
        DO I=1,NTRIES                                              !2.0
          IF (INDEKS(I)(22:23).EQ.CHARBL) THEN
            BLOCKX(K*10+1:K*10+10)=INDEKS(I)(3:12)                !1.11d
            K=K+1
            NTOTAL=NTOTAL+ICHAR(INDEKS(I)(12:12))
          ENDIF
        ENDDO                                                      !2.0
        IDINB=K
*
* SORT COUNTS BY IDENTIFIER & PICK THE IDENTIFIER WHICH SPLITS THE
* COUNTS EVENLY (SKIP THIS IF ALL THE IDENTIFIERS ARE THE SAME!)
* TAF & METAR IDENTIFIERS HAVE 4 LETTERS, SO ALL THE ITEMS SORTED ARE
* DIFFERENT (OBS FOR THE SAME IDENTIFIER ALL BEING CHAINED); BUT OTHER
* DATA TYPES USE TAFREP AND E.G. AIREPS CAN HAVE LOTS OF DIFFERENT
* IDENTIFIERS WITH THE FIRST 4 CHARACTERS THE SAME, AND IN THIS CASE
* WE MUST AVOID TAKING THE LAST AS MIDPOINT! (GT.MID MEANS FIRST OK)
*
        CALL SORTCH(BLOCKX(1:10),10,IDINB,MASK(3:12))             !1.11d
        LASTID=BLOCKX(IDINB*10-9:IDINB*10-1)                      !1.11d
*
        IF (IDINB.LE.2) THEN         ! IF ONLY 2, TAKE THE FIRST  !1.11d
          MID=BLOCKX(1:9)                                         !1.11d
        ELSE IF (IDINB.GT.2) THEN
          NOBS=0
          DO I=1,IDINB-1                                           !2.0
            NOBS=NOBS+ICHAR(BLOCKX(I*10:I*10))                    !1.11d
            IF (NOBS.GE.NTOTAL/2 .OR.
     &          BLOCKX(I*10+1:I*10+9).EQ.LASTID) GOTO 421         !1.11d
          ENDDO                                                    !2.0
  421     MID=BLOCKX(I*10-9:I*10-1)                               !1.11d
        ENDIF
*
* MID WILL BE PUT IN THE ASSIGNMENT LIST LATER. INITIALISE NEW BLOCK.
* (CAN'T INITIALISE TILL NOW BECAUSE AREA USED AS WORK SPACE)
*
        BLKTAX=CURENT
        NINBLX=0
        LEFX=BLKSIZ-INDHED
        BLOCKX(:BLKSIZ)=' '
        MAP(XBLOKS+NBLOCX:XBLOKS+NBLOCX)=CHAR(NINDEX)
*
* IF THE FULL BLOCK HAS ONLY ONE IDENT, IT CAN'T BE SPLIT, SO SET THE
* TOP BIT OF THE MAP BYTE AS FOR INDEX OVERFLOW TO STOP STORING IN IT.
*
        IF (IDINB.EQ.1) THEN                                      !1.11d
          MAP(XBLOKS+NBLOCK:XBLOKS+NBLOCK)=CHAR(128+NINDEX)          !B
!
! If all the identifiers are the same, the ob to be stored may have the
! same identifier again; but it can have a different identifier - after
! obs for only one identifier in the range just happen to have filled a
! block!  So we mustn't assume that IDINB=1 implies IND>0.  Both cases
! need a null range (start=end) for the full block, to stop storing in
! it, and the original range for the new block.  But only chain back to
! the full block if the ident is the same.
!    Write the new block and the map block out now; the full block is
! unchanged, but will still be written out at the end.
!
          IF (IND.GT.0) THEN                                     !1.11d
            NINBLX=1                       ! ONE OB IN NEW DATA BLOCK
            L=LEN(BULL)+INDENT
            LEFX=LEFX-L-2

            BLOCKX(7:7)=CHAR(L/256)                                !2.0
            BLOCKX(8:8)=CHAR(MOD(L,256))                           !2.0
*                                          ! (IND IS SET IF IDINB=1!)
            POINTER=INDEKS(IND)(20:23)     ! CHAIN TO LATEST IN INDEX
            INDEKS(IND)(20:23)=CHAR(0)//CHAR(1)//CHARBLX
            NOBS=ICHAR(INDEKS(IND)(12:12)) ! UPDATE INDEX COUNT
            IF (NOBS.LT.255) INDEKS(IND)(12:12)=CHAR(NOBS+1)
            NREPS=NREPS+1                                        !1.11d

            BLOCKX(BLKSIZ-L+1:BLKSIZ-INDENT)=BULL                !1.11
            BLOCKX(BLKSIZ-INDENT+1:BLKSIZ-4)=ENTRY(1:INDENT-4)   !1.11
            BLOCKX(BLKSIZ-3:BLKSIZ)=POINTER                      !1.11

            BLOCKX(1:1)=CHAR(BLKTAX/256)                           !2.0
            BLOCKX(2:2)=CHAR(MOD(BLKTAX,256))                      !2.0
            BLOCKX(3:3)=CHAR(NINBLX/256)                           !2.0
            BLOCKX(4:4)=CHAR(MOD(NINBLX,256))                      !2.0
            BLOCKX(5:5)=CHAR(LEFX/256)                             !2.0
            BLOCKX(6:6)=CHAR(MOD(LEFX,256))                        !2.0

            WRITE (IFT,REC=1+XBLOKS+NBLOCX+NSQ,IOSTAT=IORC)        !2.0
     &                           BLOCKX(1:BLKSIZ)                  !2.0
            IF (IORC.NE.0) CALL SYSABN(907)                        !2.0

! Find which range NBLOC is for (IDENT is in).  This is set      !1.11d
! already if IND=0, but hasn't been needed yet if IND>0.         !1.11d

            JRANGE=1                                             !1.11d
            DO WHILE (IDENT(1:4).LE.BLOKID(JRANGE*2-1)           !1.11d
     &           .OR. IDENT(1:4).GT.BLOKID(JRANGE*2))            !1.11d
              JRANGE=JRANGE+1                                    !1.11d
            ENDDO                                                !1.11d
          ENDIF                                                  !1.11d

! Set the range for the new block to the original range for the  !1.11d
! full block & nullify the full block's range (start=end)        !1.11d

          NB=NB+1                                                !1.11d
          BLOKID(NB*2-1)=BLOKID(JRANGE*2-1)                      !1.11d
          BLOKID(NB*2)=BLOKID(JRANGE*2)                          !1.11d
          BLOKID(JRANGE*2-1)=MID(1:4)                            !1.11d
          BLOKID(JRANGE*2)=MID(1:4)                              !1.11d

          WRITE (IFT,REC=1,IOSTAT=IORC) MAPHDR,                    !2.0
     &                MAP(1:BLKSIZ-8-8-4000-4),                    !2.0
     &                RANGE_CENTHR,NB,(BLOKID(I),I=1,1000),MAPOVR  !2.0
          IF (IORC.NE.0) CALL SYSABN(908)                          !2.0
          IF (IND.EQ.0) GO TO 200                                !1.11d
        ELSE IF (IDINB.GT.1) THEN
*
* COPY OBS IN 2ND HALF OF LIST TO NEW BLOCK, WHICH MEANS FOLLOWING
* POINTER CHAIN FOR EACH AIRFIELD (THE OBS WILL BE STORED IN REVERSE
* ORDER - DOESN'T MATTER), CHANGING THE CORRESPONDING INDEX POINTERS.
* KNOWING THE DISPLACEMENTS (RECDIS), WE CAN ZERO LENGTHS IN RECLEN.
*
* GO THROUGH THE INDEX ENTRIES, PICKING OBS IN THE FULL BLOCK WITH
* IDENTIFIERS AFTER THE BREAK POINT FOR TRANSFER TO THE NEW BLOCK.
* ZERO THE LENGTHS OF THE TRANSFERRED REPORTS AT THE START OF THE
* FULL BLOCK TO LET POINTERS TO REPORTS LEFT IN THIS BLOCK TO STAY
* THE SAME (POINTERS IN THE INDEX AND ON END OF OBS).
*
          DO I=1,NTRIES                                            !2.0
            IF (INDEKS(I)(22:23).EQ.CHARBL .AND.
     &          INDEKS(I)(3:11).GT.MID) THEN                     !1.11d
              RECNO=ICHAR(INDEKS(I)(20:20))*256+ICHAR(INDEKS(I)(21:21))
              INDEKS(I)(22:23)=CHARBLX
              INDEKS(I)(20:20)=CHAR((NINBLX+1)/256)
              INDEKS(I)(21:21)=CHAR(MOD(NINBLX+1,256))
  431         DISPL=BLKSIZ-RECDIS(RECNO)
*
*   STORE REPORT IN SECOND BLOCK, ZEROING ITS LENGTH IN FIRST BLOCK.
*
              NINBLX=NINBLX+1
              L=RECLEN(RECNO)
              LEFX=LEFX-L-2

              BLOCKX(6+NINBLX*2-1:6+NINBLX*2-1)=CHAR(L/256)        !2.0
              BLOCKX(6+NINBLX*2:6+NINBLX*2)=CHAR(MOD(L,256))       !2.0

              START=INDHED+NINBLX*2+LEFX
              BLOCKX(START+1:START+L-4)=BLOCK(DISPL+1:DISPL+L-4)

              RECLEN(RECNO)=0
              BLOCK(6+RECNO*2-1:6+RECNO*2-1)=CHAR(0)               !2.0
              BLOCK(6+RECNO*2:6+RECNO*2)=CHAR(0)                   !2.0

*   Follow chain while old pointer is to same block, but go no further
*   if pointer is zero or to a different block.
*   Zero pointer & warning if record number is impossible!        !1.11f
*   (if not that many records in block)                           !1.11f

              RECNO=ICHAR(BLOCK(DISPL+L-3:DISPL+L-3))*256
     &             +ICHAR(BLOCK(DISPL+L-2:DISPL+L-2))
              IF (BLOCK(DISPL+L-1:DISPL+L).EQ.CHARBL) THEN
                N=NINBLX+1
                BLOCKX(START+L-3:START+L-3)=CHAR(N/256)            !2.0
                BLOCKX(START+L-2:START+L-2)=CHAR(MOD(N,256))       !2.0
                BLOCKX(START+L-1:START+L)=CHARBLX
                GO TO 431
              ELSE IF (RECNO.EQ.0. OR. RECNO.GT.NINBLK) THEN      !1.11f
                BLOCKX(START+L-3:START+L-2)=CHAR(0)//CHAR(0)
                BLOCKX(START+L-1:START+L)=CHAR(0)//CHAR(0)
                IF (RECNO.GT.NINBLK) THEN                         !1.11f
                  print *,'TAFREP',RECNO,'=RECNO',NINBLK,'=NINBLK' !1.11f
                  print *,NBLOCK,'th block has bad pointer'       !1.11f
                  PRINT *,IFT,'is FT number'                      !1.12
                ENDIF                                             !1.11f
              ELSE          ! IF NOT SAME BLOCK & NOT ZERO, COPY POINTER
                BLOCKX(START+L-3:START+L)=BLOCK(DISPL+L-3:DISPL+L)
              ENDIF
            ENDIF
          ENDDO                                                    !2.0
*
* WRITE OUT SECOND DATA BLOCK, THEN INDEX BLOCK WITH POINTERS TO IT.
*
          BLOCKX(1:1)=CHAR(BLKTAX/256)                             !2.0
          BLOCKX(2:2)=CHAR(MOD(BLKTAX,256))                        !2.0
          BLOCKX(3:3)=CHAR(NINBLX/256)                             !2.0
          BLOCKX(4:4)=CHAR(MOD(NINBLX,256))                        !2.0
          BLOCKX(5:5)=CHAR(LEFX/256)                               !2.0
          BLOCKX(6:6)=CHAR(MOD(LEFX,256))                          !2.0

          WRITE (IFT,REC=1+XBLOKS+NBLOCX+NSQ,IOSTAT=IORC)          !2.0
     &                BLOCKX(1:BLKSIZ)                             !2.0
          IF (IORC.NE.0) CALL SYSABN(909)                          !2.0
*
* FIRST WRITE ANY FULL INDEX BLOCKS
*
          INDHDR(1:1)=CHAR(TIMTAG/256)                             !2.0
          INDHDR(2:2)=CHAR(MOD(TIMTAG,256))                        !2.0
          INDHDR(3:3)=CHAR(NTRIES/256)                             !2.0
          INDHDR(4:4)=CHAR(MOD(NTRIES,256))                        !2.0
          INDHDR(5:5)=CHAR(NREPS/256)                              !2.0
          INDHDR(6:6)=CHAR(MOD(NREPS,256))                         !2.0

          NFULL=NTRIES/NBLIND                ! NUMBER OF FULL BLOCKS
          IX=NINDEX                          ! TO VARY BLOCK IN LOOP
          DO NIND=1,NFULL                                          !2.0
            NIBL=(NIND-1)*NBLIND             ! ENTRIES ALREADY WRITTEN

            INDOVR(1:1)=CHAR(NOFLOW(NIND)/256)                     !2.0
            INDOVR(2:2)=CHAR(MOD(NOFLOW(NIND),256))                !2.0

            WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,             !2.0
     &               (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR       !2.0
            IF (IORC.NE.0) CALL SYSABN(910)                        !2.0
            IX=NOFLOW(NIND)                  ! OVERFLOW BLOCK NO   !2.0
          ENDDO                                                    !2.0
*
* THEN THE LAST INDEX BLOCK (WHICH MAY BE THE ONLY ONE)
*
          IF (NTRIES.GT.NFULL*NBLIND) THEN
            WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,             !2.0
     &                       (INDEKS(I),I=NFULL*NBLIND+1,NTRIES),
     &                       (EMPTYX,I=NTRIES+1,(NFULL+1)*NBLIND),
     &                       CHAR(0),CHAR(0)  ! no more overflow   !2.0
            IF (IORC.NE.0) CALL SYSABN(911)                        !2.0
          ENDIF
*
* REMAKE THE ORIGINAL DATA BLOCK BY GOING THROUGH THE REPORTS IN ORDER
* & MOVING ONLY THOSE WITH NON-ZERO LENGTHS.  (N.B. WE MUST DO THIS
* DIFFERENTLY - NOT FOLLOWING POINTER CHAINS AS ABOVE - TO LEAVE THE
* ZERO LENGTHS).  FINALLY WRITE BACK THE COMPRESSED DATA BLOCK.
*
* THE OLD BLOCK IS COMPRESSED IN PLACE.  THE NEW BLOCK HAS BEEN WRITTEN
* OUT, SO ITS AREA CAN BE USED AS WORK SPACE TO COPY ANY REPORT WHOSE
* OLD & NEW POSITIONS OVERLAP (OTHERWISE CHARACTERS WOULD BE REPEATED
* FROM LEFT TO RIGHT).  IF THE CURRENT REPORT IS TO BE STORED IN THE
* NEW BLOCK, THAT WILL BE READ IN AGAIN INTO THE BUFFER CALLED BLOCK.
*
          NSTART=BLKSIZ
          DO I=1,NINBLK                                            !2.0
            IF (RECLEN(I).GT.0) THEN
              LN=RECLEN(I)
              ISTART=BLKSIZ-RECDIS(I)
              NSTART=NSTART-LN
              IF (NSTART.GT.ISTART) THEN
                IF (NSTART-ISTART.LT.LN) THEN
                  BLOCKX(BLKSIZ-LN+1:BLKSIZ)=BLOCK(ISTART+1:ISTART+LN)
                  BLOCK(NSTART+1:NSTART+LN)=BLOCKX(BLKSIZ-LN+1:BLKSIZ)
                ELSE
                  BLOCK(NSTART+1:NSTART+LN)=BLOCK(ISTART+1:ISTART+LN)
                ENDIF
              ENDIF
            ENDIF
          ENDDO                                                    !2.0
*
          LEFT=NSTART-2*NINBLK-INDHED
          BLOCK(INDHED+2*NINBLK+1:INDHED+2*NINBLK+LEFT)=' '

          BLOCK(1:1)=CHAR(BLKTAG/256)                              !2.0
          BLOCK(2:2)=CHAR(MOD(BLKTAG,256))                         !2.0
          BLOCK(3:3)=CHAR(NINBLK/256)                              !2.0
          BLOCK(4:4)=CHAR(MOD(NINBLK,256))                         !2.0
          BLOCK(5:5)=CHAR(LEFT/256)                                !2.0
          BLOCK(6:6)=CHAR(MOD(LEFT,256))                           !2.0

          WRITE (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC)
     &               BLOCK(1:BLKSIZ)                               !2.0
          IF (IORC.NE.0) CALL SYSABN(912)                          !2.0
*
* RECALCULATE THE DISPLACEMENTS IN CASE THE REPORT IS STORED IN THE
* COMPRESSED BLOCK WITHOUT READING THAT BLOCK IN AGAIN.
*
          RECDIS(1)=RECLEN(1)
          DO I=2,NINBLK                                            !2.0
            RECDIS(I)=RECDIS(I-1)+RECLEN(I)
          ENDDO                                                    !2.0
*
* UPDATE THE LIST OF RANGES OF IDENTIFIERS ASSIGNED TO DATA BLOCKS,
* ADDING THE SECOND PART OF THE SPLIT RANGE TO THE END OF THE LIST.
*
          IF (RANGE_CENTHR.EQ.CENTHR) THEN                        !1.11c
            NB=NB+1
            DO I=1,NB                                             !1.11d
             IF (MID(1:4).GT.BLOKID(I*2-1).AND.                   !1.11d
     &           MID(1:4).LE.BLOKID(I*2)) GO TO 471               !1.11d
            ENDDO                                                 !1.11d
  471       BLOKID(NB*2-1)=MID(1:4)                               !1.11d
            BLOKID(NB*2)=BLOKID(I*2)
            BLOKID(I*2)=MID(1:4)                                  !1.11d
          ENDIF
*
* FINALLY WRITE BACK THE MAP BLOCK & GO BACK TO STORE THE REPORT
*
          WRITE (IFT,REC=1,IOSTAT=IORC) MAPHDR,                    !2.0
     &                MAP(1:BLKSIZ-8-8-4000-4),                    !2.0
     &                RANGE_CENTHR,NB,(BLOKID(I),I=1,1000),MAPOVR  !2.0
          IF (IORC.NE.0) CALL SYSABN(913)                          !2.0
          GO TO 200
        ENDIF
      ENDIF
***********************************************************************
*
* WRITE BACK TO DATA SET IN FOLLOWING ORDER:
*      (THE ORDER MATTERS BECAUSE  A SYSTEM FAILURE AFTER 1 OR 2 OF THE
*        3 WRITES COULD LEAVE THE DATA BASE IN AN INCONSISTENT STATE.)
***** DATA BLOCK (D), MAP OF BLOCKS IN USE (M), INDEX FOR HOUR (I) *****
* (ARGUING AS FOLLOWS: BETTER D BUT NOT I, DATA STORED BUT INACCESSIBLE,
*  THAN I BUT NOT D, INDEX ENTRY FOR LOST DATA; BETTER D BUT NOT M, DATA
*  STORED BUT MAY BE OVERWRITTEN BECAUSE BLOCK NOT CLAIMED IN MAP, THAN
*  M BUT NOT D, BLOCK TIED UP BUT DATA LOST; BETTER M BUT NOT I, BLOCK
*  TIED UP BUT DATA INACCESSIBLE BECAUSE NO INDEX ENTRY, THAN I BUT NOT
*  M, INDEX POINTS TO DATA WHICH MAY BE LOST BECAUSE BLOCK RECLAIMED.
!
! There have been errors with rogue pointers at the end of a data !1.11
! block: the pointer on the first ob stored should be zero unless !1.11
! it's a TTBB with nonzero minutes, in which case the chain might !1.11
! have been reordered, or unless the block is an overflow: chains !1.11
! are copied in reverse order by the split process.               !1.11
!
! This check hasn't shown up anything, so commented out for the   !1.12
! moment. If reinstated, it should probably be done for U/A only; !1.12
! if not U/A it should probably check for a zero hour too...      !1.12
!                                                                 !1.12
!     IF (BLOCK(BLKSIZ-3:BLKSIZ).NE.CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
!    &    .AND. BLOCK(BLKSIZ-21:BLKSIZ-21).EQ.CHAR(0)             !1.12
!    &    .AND. 1+NSQ+XBLOKS+NBLOCK.LE.BLOCKS-OFLOWS)  THEN       !1.12
!       PRINT *,NBLOCK,'th data block has first ob with nonzero'  !1.12
!       PRINT *,'       pointer & zero minutes - anything wrong?' !1.12
!     ENDIF                                                       !1.12

      BLOCK(1:1)=CHAR(BLKTAG/256)                                  !2.0
      BLOCK(2:2)=CHAR(MOD(BLKTAG,256))                             !2.0
      BLOCK(3:3)=CHAR(NINBLK/256)                                  !2.0
      BLOCK(4:4)=CHAR(MOD(NINBLK,256))                             !2.0
      BLOCK(5:5)=CHAR(LEFT/256)                                    !2.0
      BLOCK(6:6)=CHAR(MOD(LEFT,256))                               !2.0

      WRITE (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC)
     &               BLOCK(1:BLKSIZ)                               !2.0
      IF (IORC.NE.0) CALL SYSABN(914)                              !2.0
*
* WRITE THE MAP BLOCK BACK ONLY IF A NEW DATA OR INDEX BLOCK HAS BEEN
* CLAIMED - OR IF THE NUMBER OF REPORTS FOR THIS INDEX BLOCK IS N*256.
*
      IF (NINBLK.EQ.1 .OR. NTRIES-(NTRIES/NBLIND)*NBLIND.EQ.1
     &                .OR. NREPS.EQ.(NREPS/256)*256) THEN
        WRITE (IFT,REC=1,IOSTAT=IORC) MAPHDR,                      !2.0
     &                MAP(1:BLKSIZ-8-8-4000-4),                    !2.0
     &                RANGE_CENTHR,NB,(BLOKID(I),I=1,1000),MAPOVR  !2.0
        IF (IORC.NE.0) CALL SYSABN(915)                            !2.0
      ENDIF
*
* FINALLY THE INDEX BLOCK(S).  FIRST WRITE ANY FULL INDEX BLOCKS
*
      INDHDR(1:1)=CHAR(TIMTAG/256)                                 !2.0
      INDHDR(2:2)=CHAR(MOD(TIMTAG,256))                            !2.0
      INDHDR(3:3)=CHAR(NTRIES/256)                                 !2.0
      INDHDR(4:4)=CHAR(MOD(NTRIES,256))                            !2.0
      INDHDR(5:5)=CHAR(NREPS/256)                                  !2.0
      INDHDR(6:6)=CHAR(MOD(NREPS,256))                             !2.0

      NFULL=NTRIES/NBLIND                    ! NUMBER OF FULL BLOCKS
      IX=NINDEX                              ! TO VARY BLOCK IN LOOP
      DO NIND=1,NFULL                                              !2.0
        NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY WRITTEN

        INDOVR(1:1)=CHAR(NOFLOW(NIND)/256)                         !2.0
        INDOVR(2:2)=CHAR(MOD(NOFLOW(NIND),256))                    !2.0

        WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,                 !2.0
     &          (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR            !2.0
        IF (IORC.NE.0) CALL SYSABN(916)                            !2.0
        IX=NOFLOW(NIND)                      ! OVERFLOW BLOCK NO   !2.0
      ENDDO                                                        !2.0
*
* THEN THE BLOCK WHICH ISN'T FULL (UNLESS NUMBER OF ENTRIES IS MULTIPLE
*                                                    OF BLOCK CAPACITY)
      IF (NTRIES.GT.NFULL*NBLIND) THEN
        WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,                 !2.0
     &                    (INDEKS(I),I=NFULL*NBLIND+1,NTRIES),
     &                    (EMPTYX,I=NTRIES+1,(NFULL+1)*NBLIND),
     &                    CHAR(0),CHAR(0)    ! no more overflow    !2.0
        IF (IORC.NE.0) CALL SYSABN(917)                            !2.0
      ENDIF
*
      RETURN
      END
