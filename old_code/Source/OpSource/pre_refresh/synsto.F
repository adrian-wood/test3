      SUBROUTINE SYNSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT)

!-----------------------------------------------------------------------
!
! PROGRAM       : SYNSTO
!
! PURPOSE       : Store Synops in the MDB (23-byte index entries,
!                 Reports chained in data blocks)
!
! DATA TYPES    : SYNOPS  (ADAPTED FROM TAFREP USED FOR OTHER TYPES)
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
! $Revision: 1$
! $Date: 30/01/2006 20:25:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synsto.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:03    Sheila Needham  
! $
! Revision 2.1  2003/02/03 16:15:01  usmdb
! 17 Feb 2003    C Long
! 2.1  Set NREPS from first block, not last, when reading index.
!
! Revision 2.0  2001/07/03  10:44:08  10:44:08  usmdb
! Changed labelled DO loops so they end with a label CONTINUE
! statement. Added copyright and modified header - S.Cox
!
! Revision 1.8  99/07/12  16:22:59  16:22:59  usmdb (Generic MetDB account)
! 19 July 1999 C Long
! Tidy up message
!
! Revision 1.7  98/08/12  08:37:20  08:37:20  usmdb (Generic MDB account)
! Correct final write when entry just added has filled an index
! block (wrote blanks instead of last block, leaving inconsistency
! until next entry added!)                                            !G
! Correct preferred flag & duplicate code as in TAFREP.               !F
!
! Revision 1.6  98/04/20  07:12:03  07:12:03  usmdb (Generic MDB account)
! Correct preferred flag & duplicate code as in TAFREP
!
! Revision 1.5  98/01/29  11:38:37  11:38:37  usmdb (Generic MDB account)
! Addition of IBM preprocess directive
!
! Revision 1.4  97/09/22  13:52:29  13:52:29  uspm (Pat McCormack)
! Replace DATA statements using hex numbers for HP
!
! Revision 1.3  1997/09/10 15:57:42  uspm
! 1. CORRECT CODE TO PUT NEGATIVE TIME OF RECEIPT IN INDEX (TILL NOW
!    NEG TOR ENDS UP AS 256+TOR)                                      !E
! 2. Change call to sortch so that the length passed is the
!    same as the length of the mask being passed.
!
! Revision 1.2  1997/07/31 11:39:44  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 13:50:07  uspm
! Initial revision
!
! NOV 96 - AVOID ABEND WHEN PREFERRED REPORT IS FIRST IN BLOCK        !C
!
! SEP 96 - FOUR CHECKS FOR DATA BASE INCONSISTENCY                    !B
!
! MAR 96 - REMOVE CONCATENATIONS TO SET LONG CHARACTER STRINGS
!          (FASTER & AVOIDS LARGE WORK AREA).
!          START SEARCH FOR FREE OVERFLOW AT START OF POOL RATHER
!          THAN SKIPPING FIRST BLOCK                                  !A
!
! OCT 95 - SPECIFIC TAF/METAR CODE & CODE FOR CHANGES OF FT NUMBER
!          REMOVED, I/O ON CONTINUED INDEX BLOCKS REDUCED, INDEXES
!          FOR 3 PERIODS KEPT IN COMMON, FOR USE BY SEQUENCE CHECK),
!          SORTS DONE BY SORTCH RATHER THAN JSL82, WITH WORK AREAS
!          DEFINED DIFFERENTLY
!
! MADE FROM TAFREP OCT 1995
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER   CENTURY     ! Common function to determine C20/C21.
*
      INTEGER   DATIME(5), NOW(8)
      CHARACTER ENTRY*(*), BULL*(*), IDENT*(*)
      INTEGER   CENDAY,CENTHR, SLOTHR,SLODAY
      INTEGER   CURENT, HOURNOW, LATEST
      INTEGER   INDHED,BLKSIZ, START,DISPL, RECNO
      PARAMETER (MAXBLK=27998,INDLEN=23,INDHED=6,MAXOFL=7)
*
      INTEGER   RECDIS(MAXBLK/2)
      INTEGER   ITOR,POINTR
      LOGICAL   INSERT
*
      CHARACTER*23 INDEKS(MAXOFL*MAXBLK/INDLEN,3) ! INDEX ENTRIES
      CHARACTER*23 EMPTYX
      CHARACTER*23 MASK,MASX
*
      CHARACTER*5 MID
      CHARACTER*4 BUFR,POINTER
      CHARACTER*2 CHARBL,CHARBLX
      CHARACTER*1 MEDIAN,QUARTL
      CHARACTER   INDHDR*6,INDOVR*2
      CHARACTER*14 MSGHDR
*
      INTEGER BLOCKS,XBLOKS,XHOURS,AMSTAR, OFLOWS,INDENT
      INTEGER BLKTAG,NINBLK,LEFT
      INTEGER NINBLX,LEFX
*
* INDEXES FOR THREE 6-HOUR PERIODS ARE KEPT IN CORE: SUBSCRIPT LX SAYS
* WHICH IS IN USE FOR THE CURRENT REPORT.  SO ALL ITEMS READ FROM THE
* INDEX ARE IN ARRAYS OF DIMENSION 3, AS ARE THE CORRESPONDING BLOCK
* NUMBERS AND CENTURY HOURS.
* THE OLDEST SLOT WILL ALWAYS BE REUSED TO READ AN INDEX FOR A PERIOD
* NOT IN CORE, SO USUALLY THE LATEST TWO 6-HOUR PERIODS AND ONE OTHER
* WILL BE IN CORE.
*
      INTEGER TIMTAG(3),NTRIES(3),NREPS(3)        ! INDEX HEADERS
      INTEGER NOFLOW(MAXOFL,3)                    ! OVERFLOW POINTERS
      INTEGER NINDEX(3),IXHOUR(3)     ! BLOCK NUMBERS & CENTURY-HOURS
      INTEGER IDSK(5)                 ! ARRAY FOR MAPRD (IFT ETC)
*
      CHARACTER*(MAXBLK) MAP,BLOCK,BLOCKX
      CHARACTER*4 BLOKID(1000), ALLNIL,ALLONE
      CHARACTER*132 HEAD
      COMMON /SYNIX/ IDSK, IXHOUR,NTRIES, INDEKS
      COMMON /SYNDC/ MAP, BLOCK, BLOCKX, RECDIS
      EQUIVALENCE (BLOKID,MAP(MAXBLK-4004:))
*
* USE BLOCKX AS A WORK AREA FOR SORTS, EQUIVALENCING THE ARRAYS NEEDED
*
      CHARACTER*1 MAPX(MAXBLK)
      CHARACTER*6 SORTX(MAXBLK/6)
      EQUIVALENCE (BLOCKX,MAPX)
      EQUIVALENCE (BLOCKX,SORTX)

      DATA        MASK/'  XXXXX                '/  ! TO SORT ON IDENT
      DATA        MASX/'           X           '/  ! TO SORT ON COUNT
      DATA        BUFR/'BUFR'/
      DATA        LATEST/0/                        ! LATEST=0 AT START
      DATA        EMPTYX/' '/
      DATA        MSGHDR/'....Z SYNSTO: '/
#if defined (MVS)
      DATA        ALLNIL/Z00000000/, ALLONE/ZFFFFFFFF/
#else
      ALLNIL(1:4)=CHAR(0)
      ALLONE(1:4)=CHAR(255)
#endif

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synsto.F,v $
     &'//'$ $Date: 30/01/2006 20:25:03$ $Revision: 1$'

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
      WRITE (MSGHDR(1:4),'(I4.4)') NOW(5)*100+NOW(4)
      CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
      HOURNOW=(CENDAY-1)*24+NOW(5)           ! CURRENT CENTURY-HOUR
*                                            ! FIRST TIME READ MAP
      IF (LATEST.EQ.0) THEN
        CALL EB2ASC(4,BUFR)   ! CONVERT 'BUFR' TO ASCII FIRST TIME ONLY
        READ (IFT,REC=1,IOSTAT=IORC) MAP
        IF (IORC.NE.0) CALL ABEND

        BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))
        XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))
        XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))
        AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))

        NB=ICHAR(MAP(BLKSIZ-4006:BLKSIZ-4006))*256
     &    +ICHAR(MAP(BLKSIZ-4005:BLKSIZ-4005))
        OFLOWS=ICHAR(MAP(BLKSIZ-3:BLKSIZ-3))*256
     &        +ICHAR(MAP(BLKSIZ-2:BLKSIZ-2))
        INDENT=ICHAR(MAP(BLKSIZ-1:BLKSIZ-1))*256
     &        +ICHAR(MAP(BLKSIZ:BLKSIZ))
*
* SET NSQ TO CALCULATE BLOCK NUMBERS ALLOWING FOR SEQUENCE RECORD.
*
        NSEQBL=ICHAR(MAP(8+BLOCKS:8+BLOCKS))
        IF(NSEQBL.GT.0)THEN
          NSQ=1
        ELSE
          NSQ=0
        ENDIF
*
* SET UP AN ARRAY OF DATA SET PARAMETERS AS IN RETRIEVAL FOR USE BY
* MAPRD IN SEQUENCE CHECKS.
*
        IDSK(2)=BLKSIZ                       ! BLOCKSIZE
        IDSK(3)=IFT                          ! FT NUMBER
        IDSK(5)=1                            ! DIRECT ACCESS
*
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
      INDHOR=MOD(DATIME(4)+24-AMSTAR,XHOURS) ! HOUR MINUS SLOT START
      IF (MOD(ICHAR(ENTRY(7:7)),16).EQ.0) THEN
        ENTRY(1:1)=CHAR(INDHOR)              ! HOUR RELATIVE TO SLOT
      ELSE                                   ! IF COR NUMBER NONZERO,
        ENTRY(1:1)=CHAR(INDHOR+128)          ! SET COR IN SAME BYTE.
      ENDIF
      ENTRY(2:2)=CHAR(DATIME(5))             ! MINUTES
*
      IF (DATIME(1).LT.1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))   !D
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
      IF (ITOR.LT.0) ITOR=65536+ITOR         ! TWOS COMPLEMENT        !E
      ENTRY(18:18)=CHAR(ITOR/256)            ! PUT HALFWORD TOR IN ENTRY
      ENTRY(19:19)=CHAR(MOD(ITOR,256))       ! PUT HALFWORD TOR IN ENTRY
*
* IF THE DATA IS FOR TOO LONG BEFORE THE THE MOST RECENT DATA STORED
* ('TOO LONG' MEANS MORE THAN THE PERIOD COVERED BY THE DATA BASE),
* THEN REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH OLD DATA!).
* IF THE NEW DATA IS MORE RECENT, UPDATE THE LATEST HOUR - AND THE
* INDEX BLOCK TAG FOR WHICH THE LIST OF RANGES CAN BE UPDATED (SET
* WHEN A NEW INDEX BLOCK IS STARTED, BUT THIS COPES UNTIL THEN...).
*
*  (XBLOKS-1 RATHER THAN XBLOKS BECAUSE ALTHOUGH WE CAN STORE DATA
* EXTENDING OVER A PERIOD OF XBLOKS*XHOURS IF THAT PERIOD STARTS AT
* THE START OF AN INDEX BLOCK, IN GENERAL SUCH A PERIOD WOULD REUSE
* THE LATEST INDEX BLOCK FOR THE OLDEST DATA.)
*
      IF (CENTHR.LE.LATEST-(XBLOKS-1)*XHOURS) THEN
        PRINT *,MSGHDR,LATEST-CENTHR,'HOURS OLD   ',BULL(:60)
        RETURN
      ENDIF
*
      IF (CENTHR.GT.LATEST) THEN
        LATEST=CENTHR
        LISTAG=CURENT                        ! LATEST FOR RANGE UPDATE
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
      NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2  ! INDEX BLOCK NUMBER
*
* SEE IF THE INDEX IS IN ONE OF THE BUFFERS IN CORE.
*
      LX=0
      DO I=1,3
        IF (NXBLOK.EQ.NINDEX(I)) LX=I
      ENDDO
*
* IF NOT, REUSE BUFFER WITH OLDEST INDEX  (FOLLOWING CODE MUST SET LX!)
*                                                         ====
      IF (LX.EQ.0) THEN
        DO I=1,3
          IF (IXHOUR(I).EQ.MIN(IXHOUR(1),IXHOUR(2),IXHOUR(3))) LX=I
        ENDDO
*
        IXHOUR(LX)=CENTHR-INDHOR             ! KEEP INDEX CENTURY-HOUR
        NINDEX(LX)=NXBLOK                    ! KEEP INDEX BLOCK NUMBER
*
        IX=NINDEX(LX)                        ! TO VARY IN READ LOOP
        NIND=1                               ! CONTINUATION NUMBER
   30   NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN

        READ (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &      (INDEKS(I,LX),I=NIBL+1,NIBL+NBLIND),INDOVR
        IF (IORC.NE.0) CALL ABEND

! Get NTRIES & NREPS from the first index block rather than the    !2.1
! last if there's index overflow: the first is always updated,     !2.1
! the last not, so taking NREPS from the last risks getting a      !2.1
! low value, a low value of NREPS (not equal to the sum of the     !2.1
! counts for the index entries) spoils the distribution for a      !2.1
! new index period...                                              !2.1

        IF (NIND.EQ.1) THEN                                        !2.1
          TIMTAG(LX)=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
          NTRIES(LX)=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
          NREPS(LX)=ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))
        ENDIF                                                      !2.1
        NOFLOW(NIND,LX)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

        IF (NOFLOW(NIND,LX).GT.0) THEN       ! INDEX OVERFLOW?
          IX=NOFLOW(NIND,LX)                 ! IF SO, RESET BLOCK NO
          NIND=NIND+1                        ! FURTHER CONTINUATION
          IF(NIND.GT.MAXOFL)THEN
            PRINT*,MSGHDR,'MAX INDEX OVERFLOW REACHED - SYNOPS'
            RETURN
          ENDIF
          IF (TIMTAG(LX).EQ.CURENT) GO TO 30 ! READ ON IF RIGHT TIME
        ENDIF
      ENDIF
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
* 200 IF (STATION IS INDEXED) THEN                                    *
*         210 ALLOCATE SAME DATA BLOCK & CHAIN POINTERS;              *
*             KEEP NUMBER OF INDEX ENTRY FOR USE LATER.               *
*     ELSE                                                            *
*         220 FIND WHICH RANGE THE IDENTIFIER IS IN                   *
*         230 & FIND THE CORRESPONDING BLOCK NUMBER FROM THE MAP;     *
*             SET THE POINTER TO ZERO.                                *
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
      IF (TIMTAG(LX).NE.CURENT) THEN
        NBLOK=1
  110   N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(NINDEX(LX)))
        IF (N.GT.0) THEN
          NBLOK=NBLOK+(N-1)
          MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
          IF (1+XBLOKS+NBLOK.LT.BLOCKS) GO TO 110
        ENDIF
*                      NOW DO SAME FOR INDEX OVERFLOWS (TOP BIT SET)
        NBLOK=1
  111   N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(128+NINDEX(LX)))
        IF (N.GT.0) THEN
          NBLOK=NBLOK+(N-1)
          MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
          IF (1+XBLOKS+NBLOK.LT.BLOCKS) GO TO 111
        ENDIF
*
* CHOOSE A PREVIOUS INDEX BLOCK TO ALLOCATE STATIONS TO DATA BLOCKS,
* AVOIDING LOW TOTALS & VERY HIGH ONES BY TAKING A BLOCK WITH A TOTAL
* BETWEEN THE MEDIAN & THE UPPER QUARTILE, EITHER THAT FOR 24 HOURS
* AGO OR THE MOST RECENT IN THE RANGE.
*
        DO I=1,XBLOKS
          MAPX(I)=MAP(8+I:8+I)               ! TO WORK AREA FOR SORT
        ENDDO
*
        CALL SORTCH(MAPX,1,XBLOKS,MASK(3:3))
        MEDIAN=MAPX(XBLOKS/2)                ! MIDDLE COUNT AFTER SORT
        QUARTL=MAPX(3*XBLOKS/4)
*
*       IF (MEDIAN.EQ.CHAR(0)) THEN          ! USE MAX IF ZERO MEDIAN
        IF (ICHAR(MAPX(XBLOKS)).GE.2*ICHAR(MEDIAN)) THEN
          LASTIX=1+INDEX(MAP(8+1:8+XBLOKS),MAPX(XBLOKS))
        ELSE
          I=0                                ! BLOCKS TO GO BACK
          J=NINDEX(LX)-24/XHOURS             ! FIRST TRY 24 HOURS BACK,
  120     IF (J.LE.1) J=XBLOKS+J             ! WRAPPING ROUND IF NEEDED
          IF (J.LE.1 .OR.                    ! IF TOO FEW INDEX BLOCKS
     -        MAP(J+7:J+7).LT.MEDIAN .OR.    ! OR COUNT TOO SMALL
     -        MAP(J+7:J+7).GT.QUARTL) THEN   ! OR COUNT TOO BIG,
            I=I+1                            ! GO BACK A(NOTHER) BLOCK
            J=NINDEX(LX)-I                   ! FROM CURRENT INDEX BLOCK
            IF (I.LE.XBLOKS) GO TO 120       ! IF ANY NOT YET TRIED...
          ENDIF                              ! N.B. J=1:N, NINDEX=2:N+1
          LASTIX=J                           ! INDEX BLOCK TO USE
        ENDIF
*
* READ IN THE INDEX BLOCK(S) FOR THE PERIOD CHOSEN ABOVE FOR ITS COUNT.
*
        IX=LASTIX                            ! TO VARY IN READ LOOP
        NIND=1                               ! CONTINUATION NUMBER
  130   NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY READ IN
        READ (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &      (INDEKS(I,LX),I=NIBL+1,NIBL+NBLIND),INDOVR
        IF (IORC.NE.0) CALL ABEND

        TIMTAG(LX)=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
        NTRIES(LX)=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
        NREPS(LX)=ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))
        NOFLOW(NIND,LX)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

        IF (NOFLOW(NIND,LX).GT.0) THEN       ! INDEX OVERFLOW?
          IX=NOFLOW(NIND,LX)                 ! IF SO, RESET BLOCK NO
          NIND=NIND+1                        ! FURTHER CONTINUATION
          IF(NIND.GT.MAXOFL)THEN
            PRINT*,MSGHDR,'MAX INDEX OVERFLOW REACHED - SYNOPS'
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
* N.B. BLOKID IS A LIST OF PAIRS OF IDENTIFIERS DEFINING RANGES RATHER
* THAN SINGLE IDENTIFIERS AS BREAK POINTS. THIS MAKES IT EASIER TO UP-
* DATE: IF A RANGE IS SPLIT THE SECOND HALF CAN JUST BE PUT AT THE END.
*
        NB=(BLOCKS-1-NSQ-XBLOKS-OFLOWS)/XBLOKS
        IF (NTRIES(LX).EQ.0) NB=1
        MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
        MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
        BLOKID(1)=ALLNIL            ! START OF FIRST RANGE (MIN POSS)
        BLOKID(NB*2)=ALLONE         ! END OF LAST RANGE    (MAX POSS)
*
        IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))  ! GET FIRST
        MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))     ! DATA BLOCK
*
        IF (NB.GT.1) THEN                    ! SORT INTO COUNT ORDER
          CALL SORTCH(INDEKS(1,LX),INDENT,NTRIES(LX),MASX)
*
* THE SORT PUTS ENTRIES WITH BIG COUNTS AT THE END OF THE LIST.
* ANY IDENT WITH MORE THAN NREPS/NB WILL BE ASSIGNED A WHOLE BLOCK.
* THE REST WILL BE RE-SORTED IN IDENT ORDER & SHARED OUT BETWEEN
* THE REMAINING BLOCKS.
*
          NL=NTRIES(LX)                      ! NUMBER OF ENTRIES
          LOB=NREPS(LX)/NB                   ! MEAN OBS PER BLOCK
  135     NREP=ICHAR(INDEKS(NL,LX)(12:12))   ! OBS FOR LAST IDENT
          IF (NREP.GT.LOB) THEN              ! IF COUNT > MEAN,
            NL=NL-1                          ! LEAVE ENTRY AT END
            NREPS(LX)=NREPS(LX)-NREP         ! & IGNORE ITS COUNT.
            IF (NL.GT.1) GO TO 135           ! BACK PAST BIG COUNTS
          ENDIF
*                                            ! SORT SMALLER COUNTS
          CALL SORTCH(INDEKS(1,LX),INDENT,NL,MASK)      ! BY IDENT
*                                            ! & RECALCULATE MEAN
          LOB=NREPS(LX)/(NB-(NTRIES(LX)-NL)) ! IGNORING BIG COUNTS.
          K=1
          NOB=0
          DO 140 I=1,NTRIES(LX)
           NOB=NOB+ICHAR(INDEKS(I,LX)(12:12))
           IF (NOB.GE.LOB*K .AND. K.LT.NB) THEN
             BLOKID(K*2)=INDEKS(I,LX)(3:6)   ! END OF ONE RANGE
             BLOKID(K*2+1)=INDEKS(I,LX)(3:6) ! START OF NEXT - SAME
             K=K+1                           ! ONE MORE RANGE DONE
             IX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-1-NSQ),CHAR(0))
             MAP(8+XBLOKS+IX:8+XBLOKS+IX)=CHAR(NINDEX(LX))
           ENDIF
  140     CONTINUE
        ENDIF
*
* FINALLY REINITIALISE THE INDEX BUFFER FOR THE NEW HOUR.
* KEEP THE TIME TAG FOR THIS LIST IN CASE A BLOCK IS SPLIT: THE RANGES
* WILL ONLY BE UPDATED FOR DATA WITH THIS TAG, FOR THE LATEST PERIOD.
*
        TIMTAG(LX)=CURENT
        NTRIES(LX)=0
        NREPS(LX)=0
        NOFLOW(1,LX)=0
*
        LISTAG=CURENT
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
      DO 210 I=1,NTRIES(LX)
      IF (INDEKS(I,LX)(3:11).EQ.IDENT) THEN
        NBLOC=ICHAR(INDEKS(I,LX)(22:22))*256+ICHAR(INDEKS(I,LX)(23:23))
        POINTER=INDEKS(I,LX)(20:23)                                     ?
        IND=I
      ENDIF
  210 CONTINUE
*
* IF NOT, ALLOCATE A BLOCK NUMBER FROM THE LIST, USING THE IDENTIFIER.
* FIRST FIND WHICH OF THE CURRENT NB RANGES THE IDENTIFIER IS IN.
* (THE RANGE INCLUDES THE ENDPOINT BUT NOT THE STARTING POINT)
*
      IF (IND.EQ.0) THEN
        DO 220 J=1,NB
         IF (IDENT(1:4).GT.BLOKID(J*2-1) .AND.
     -       IDENT(1:4).LE.BLOKID(J*2)) GO TO 221
  220   CONTINUE
*
* THEN GET THE CORRESPONDING DATA BLOCK NUMBER FOR THIS INDEX BLOCK
* FROM THE MAP (WE CAN STORE REPORTS FOR LOTS OF INDEX BLOCKS AT THE
* SAME TIME, BUT THE LIST ONLY APPLIES STRICTLY TO THE LATEST.
* HOWEVER, IF EARLIER TIMES ALREADY HAVE MOST STATIONS INDEXED, THIS
* SHOULDN'T LEAVE THE SPREAD OF STATIONS OVER DATA BLOCKS TOO UNTIDY.)
* IF THERE ARE MORE RANGES THAN DATA BLOCKS ALLOCATED TO THIS INDEX
* BLOCK, THEN USE THE LAST DATA BLOCK FOUND (& SPLIT IT WHEN FULL).
*
  221   IX=1
        DO 230 I=1,J
          IF (IX.GT.BLOCKS-1-NSQ-XBLOKS) GO TO 231
          IY=INDEX(MAP(8+XBLOKS+IX:8+BLOCKS-1-NSQ),CHAR(NINDEX(LX)))
          IF (IY.EQ.0) GO TO 231
          IX=IX+IY                                                  !2.0
  230   CONTINUE                                                    !2.0
  231   NBLOC=IX-1
*
* AS THERE IS ONLY ONE REPORT FOR THIS STATION, ZERO THE POINTER ON
* THE END OF THE REPORT.  (A POINTER IS RECORD/BLOCK, AS IN THE INDEX.)
*
        POINTER(1:1)=CHAR(0)
        POINTER(2:2)=CHAR(0)
        POINTER(3:3)=CHAR(0)
        POINTER(4:4)=CHAR(0)
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
  300 IF (NBLOCK.NE.NBLOC) THEN
        NBLOCK=NBLOC
        READ (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC) BLOCK
        IF (IORC.NE.0) CALL ABEND

        BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
        NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
        LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))

! IF THE BLOCK HAS NO DATA FOR THIS INDEX PERIOD,
! SET TAG, NINBLK & LEFT IN BLOCK HEADER & CLEAR REST OF BLOCK
! - UNLESS AN INDEX ENTRY POINTS TO THIS BLOCK, IN WHICH CASE
! THE DATA BASE HAS BEEN CORRUPTED.

        IF (NINBLK.EQ.0 .OR. BLKTAG.NE.CURENT) THEN
          IF (IND.GT.0) THEN                                          !B
            PRINT *,MSGHDR,'+++++++ DATA BASE INCONSISTENT! +++++++'!1.8
            PRINT *,NBLOCK,'TH DATA BLOCK POINTED TO BY INDEX ENTRY'  !B
            PRINT *,INDEKS(IND,LX)                                    !B
            PRINT *,CURENT,'IS DATA TIME TAG',BLKTAG,'IS BLOCK TAG'   !B
            RETURN                                                    !B
          ENDIF                                                       !B

          NINBLK=0
          BLKTAG=CURENT
          LEFT=BLKSIZ-INDHED
          BLOCK(1:1)=CHAR(BLKTAG/256)
          BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
          BLOCK(7:BLKSIZ)=' '
        ELSE
*
* WORK OUT THE DISPLACEMENTS CORRESPONDING TO THE RECORD LENGTHS:
* (RECDIS(I) IS THE START OF THE I-TH REPORT FROM THE END OF THE BLOCK)
*
          RECDIS(1)=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))
          DO 310 I=2,NINBLK
            RECDIS(I)=RECDIS(I-1)+ICHAR(BLOCK(6+I*2-1:6+I*2-1))*256
     &                           +ICHAR(BLOCK(6+I*2:6+I*2))         !2.0
  310     CONTINUE                                                  !2.0
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
        LASTHR=MOD(ICHAR(INDEKS(IND,LX)(1:1)),32)
        IF (LASTHR.GT.INDHOR .OR.
     -    (LASTHR.EQ.INDHOR.AND.INDEKS(IND,LX)(2:2).GT.ENTRY(2:2))) THEN
          INSERT=.TRUE.
        ELSE
          INSERT=.FALSE.
        ENDIF
*
* NAFTER MAY BE SET IF THIS IS NOT THE LATEST REPORT IN THE
* CHAIN, NPREF IF THERE IS ALREADY A PREFERRED REPORT FOR THIS TIME.
* USE THE COUNT IN THE INDEX ENTRY TO CONTROL THE LOOP ROUND THE CHAIN
* RATHER THAN RELYING ON NONZERO POINTERS.
*
        NAFTER=0
        NPREF=0
        NCHAIN=ICHAR(INDEKS(IND,LX)(12:12))                           !B
*
* LOOP BACK TO HERE TO LOOK AT NEXT REPORT IN CHAIN
*
  320   POINTR=ICHAR(POINTER(1:1))*256+ICHAR(POINTER(2:2))
        DISPL=BLKSIZ-RECDIS(POINTR)
*
* IF AN IDENT IN THE CHAIN IS NOT AS IN THE INDEX, SOMETHING'S WRONG!
*
        IF (BLOCK(DISPL+1:DISPL+5).NE.IDENT) THEN                     !B
          PRINT *,MSGHDR,' >>>>>>> DATA BASE INCONSISTENT! <<<<<<<<'  !B
          PRINT *,'INDEX ENTRY FOR ',IDENT,'POINTED TO OBS FOR ',     !B
     &            BLOCK(DISPL+1:DISPL+5)                              !B
          PRINT *,'IDENT IN INDEX ENTRY NOW BLANK, REPORT NOT STORED' !B
          INDEKS(IND,LX)(3:7)=' '      ! ????  RESET ENTRY & REUSE?   !B
          RETURN                       ! ????                         !B
        ENDIF                                                         !B
*
        IF (POINTR.EQ.1) THEN
          L=RECDIS(1)
        ELSE
          L=RECDIS(POINTR)-RECDIS(POINTR-1)
        ENDIF
        RECNO=POINTR
*
        LN=INDEX(BULL,BUFR)-1      ! LENGTH OF REPORT IN CHARACTERS
        IF (LN.LE.0) LN=LEN(BULL)  ! (TOTAL LENGTH IF NO BUFR MESSAGE)
        LENBUF=LEN(BULL)-LN        ! LENGTH OF BUFR MESSAGE (MAY BE 0)
*
* IF SAME TIME (FIRST TWO BYTES OF INDEX ENTRY), DO DUPLICATE CHECK:
* IF STRING OF SAME LENGTH, AND STRING ITSELF IS SAME, THEN DUPLICATE
*
        LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)           !F
        IF (LASTHR.EQ.INDHOR .AND.                                   !F
     &      (BLOCK(DISPL+L-21:DISPL+L-21).EQ.ENTRY(2:2))) THEN        !F
          IF (LN.EQ.L-LENBUF-23) THEN
            IF (BULL(1:LN).EQ.BLOCK(DISPL+1:DISPL+LN)) RETURN
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
*
* IF INSERT HAS BEEN SET, GO BACK PAST AT LEAST ONE REPORT (THAT POINTED
* TO BY THE INDEX ENTRY WHICH SET INSERT) UNTIL A REPORT FOR AN EARLIER
* TIME (OR THE SAME TIME) IS FOUND.  KEEP THE LAST RECORD NUMBER PASSED.
*
        IF (INSERT) THEN
          LASTHR=MOD(ICHAR(BLOCK(DISPL+L-22:DISPL+L-22)),32)
          IF (LASTHR.GT.INDHOR .OR. (LASTHR.EQ.INDHOR .AND.
     -        BLOCK(DISPL+L-21:DISPL+L-21).GT.ENTRY(2:2))) THEN
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
        NCHAIN=NCHAIN-1                                               !B
        IF (POINTER(3:4).EQ.INDEKS(IND,LX)(22:23)                     !B
     &      .AND. NCHAIN.GT.0) GO TO 320                              !B
        POINTER=INDEKS(IND,LX)(20:23)
      ENDIF
*
* IF A PREFERRED REPORT FOR THE SAME TIME HAS BEEN FOUND, COMPARE THE
* NUMBER OF GOOD VALUES AND THE COR NUMBER AND RESET THE PREFERRED
* FLAG IF THE NEW REPORT IS BETTER.
* SET THE "HISTORICAL" PREFERRED FLAG (X'20', SO ADD 32), NEVER TO BE
* UNSET, TO SHOW THAT THE NEW REPORT HAS BEEN (OR STILL IS) PREFERRED.
*
      IF (IND.GT.0 .AND. NPREF.GT.0) THEN
        IF (NPREF.EQ.1) THEN                                          !C
          DISPL=BLKSIZ-INDLEN                                         !C
        ELSE                                                          !C
          DISPL=BLKSIZ-RECDIS(NPREF-1)-INDLEN             ! --> TRAILER
        ENDIF                                                         !C
        IF (BLOCK(DISPL+7:DISPL+7).LT.ENTRY(7:7) .OR.     ! COR FLAG  !F
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
***********************************************************************
*
* IF THERE'S ROOM IN THE BLOCK & THE REPORT'S NOT A DUPLICATE, STORE IT.
*
***********************************************************************
      IF (LEN(BULL)+INDENT+2.LE.LEFT) THEN
        NINBLK=NINBLK+1
        L=LEN(BULL)+INDENT
        LEFT=LEFT-L-2
        BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)
        BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))

        IF (NINBLK.GT.1) THEN
          RECDIS(NINBLK)=RECDIS(NINBLK-1)+L
        ELSE
          RECDIS(NINBLK)=L
        ENDIF

        START=INDHED+NINBLK*2+LEFT
        BLOCK(START+1:START+LEN(BULL))=BULL

        BLOCK(START+LEN(BULL)+1:START+L-4)=ENTRY(1:INDENT-4)
        BLOCK(START+L-3:START+L)=POINTER
***********************************************************************
*
* UPDATE THIS STATION'S INDEX ENTRY TO POINT TO THIS REPORT, IF IT IS
* THE LATEST.  IF NOT, NAFTER IS NONZERO.  MAKE THE
* NAFTER-TH REPORT POINT TO THE NEW ONE & THE NEW ONE TO
* ZERO (BY TAKING THE POINTER THAT WAS IN THE NAFTER-TH).
* (OR ADD AN ENTRY FOR THIS STATION IF THERE'S NONE IN THIS SLOT).
*
***********************************************************************
        POINTER(1:1)=CHAR(NINBLK/256)
        POINTER(2:2)=CHAR(MOD(NINBLK,256))
        POINTER(3:3)=CHAR(NBLOCK/256)
        POINTER(4:4)=CHAR(MOD(NBLOCK,256))
*
* COUNT OF OBS IS ONLY ONE BYTE; LEAVE IT AS 255 IF MORE THAN 255!
*
        IF (IND.GT.0) THEN
          IF (POINTER.LT.BLOCK(START+L-3:START+L)) THEN               !B
            NOTPTR=ICHAR(BLOCK(START+L-3:START+L-3))*256              !B
     &            +ICHAR(BLOCK(START+L-2:START+L-2))                  !B
            PRINT *,MSGHDR,' ******* DATA BASE INCONSISTENT ********' !B
            PRINT *,NOTPTR,'IS RECORD NUMBER IN POINTER, BUT ONLY'    !B
            PRINT *,NINBLK-1,'OBS IN BLOCK'                           !B
            INDEKS(IND,LX)(3:7)=' '    ! ????  RESET ENTRY & REUSE?   !B
            RETURN                     ! ????                         !B
          ENDIF                                                       !B
*
          NOBS=ICHAR(INDEKS(IND,LX)(12:12))
          IF (NOBS.LT.255) INDEKS(IND,LX)(12:12)=CHAR(NOBS+1)
*
          IF (NAFTER.EQ.0) THEN
            INDEKS(IND,LX)(1:2)=ENTRY(1:2)
            INDEKS(IND,LX)(18:19)=ENTRY(18:19)
            INDEKS(IND,LX)(20:23)=POINTER
          ELSE
            DISPL=BLKSIZ-RECDIS(NAFTER)
            IF (NAFTER.EQ.1) THEN
              LL=RECDIS(1)
            ELSE
              LL=RECDIS(NAFTER)-RECDIS(NAFTER-1)
            ENDIF
            BLOCK(START+L-3:START+L)=BLOCK(DISPL+LL-3:DISPL+LL)
            BLOCK(DISPL+LL-3:DISPL+LL)=POINTER
          ENDIF
        ELSE
*
* OR MAKE A NEW INDEX ENTRY (PUT IDENT & COUNT OF 1 IN THE INPUT ENTRY)
*
          NTRIES(LX)=NTRIES(LX)+1
          INDEKS(NTRIES(LX),LX)(1:2)=ENTRY(1:2)
          INDEKS(NTRIES(LX),LX)(3:11)=IDENT
          INDEKS(NTRIES(LX),LX)(12:12)=CHAR(1)
          INDEKS(NTRIES(LX),LX)(13:19)=ENTRY(13:19)
          INDEKS(NTRIES(LX),LX)(20:23)=POINTER
*
* IF THIS ENTRY NEEDS TO START A NEW INDEX BLOCK, IT'S ENOUGH HERE TO
* CLAIM A BLOCK NUMBER FROM THE OVERFLOW POOL AND LIST IT AS THE NEXT
* BLOCK TO CONTINUE THE INDEX; THIS NUMBER WILL BE SET AS A POINTER
* AT THE END OF THE PREVIOUS BLOCK WHEN THE INDEX IS WRITTEN BACK.
*
          NIND=NTRIES(LX)/NBLIND           ! NO OF FULL INDEX BLOCKS
          IF (NIND.GE.1 .AND. NTRIES(LX)-NIND*NBLIND.EQ.1) THEN
            NX=INDEX(MAP(8+BLOCKS-NSQ-OFLOWS:8+BLOCKS-NSQ-1),CHAR(0))
            IF (NX.EQ.0) THEN
              PRINT *,MSGHDR,'NO MORE OVERFLOWS FOR INDEX'
              NTRIES(LX)=NTRIES(LX)-1
              RETURN
            ELSE
              NOFLOW(NIND,LX)=BLOCKS-NSQ-OFLOWS+NX                   !A
              NOFLOW(NIND+1,LX)=0
              NFLIX=128+NINDEX(LX)   ! TO SET FLAG IN MAP BYTE
              MAP(8+NOFLOW(NIND,LX)-1:8+NOFLOW(NIND,LX)-1)=CHAR(NFLIX)
            ENDIF
          ENDIF
        ENDIF
*
* UPDATE REPORT COUNT IN INDEX & COPY TO MAP BYTE FOR INDEX BLOCK
* (USE N/256 IN CASE THE COUNT IS LARGE; ADD 1 SO THAT THE MAP BYTE
* IS NONZERO EVEN IF THE COUNT IS SMALL)
*
        NREPS(LX)=NREPS(LX)+1
        MAP(8+NINDEX(LX)-1:8+NINDEX(LX)-1)=CHAR(NREPS(LX)/256+1)
      ELSE
***********************************************************************
*
* IF NO ROOM, SPLIT THE DATA MORE OR LESS EQUALLY BETWEEN 2 BLOCKS.
* (GET THE SECOND FROM THE OVERFLOW POOL: BECAUSE THE INDEX OPERATION
* STARTS FURTHER ON, INDEX(...) MUST BE ADJUSTED TO GIVE BLOCK NUMBER!)
* FOR REPORTS IN THE CURRENT BLOCK, LIST STATION & NUMBER OF REPORTS,
* SORT LIST BY STATION & SPLIT INTO TWO WITH SIMILAR NUMBERS OF OBS.
*
***********************************************************************
        IF (OFLOWS.LT.1) THEN
          NX=0
        ELSE
        NX=INDEX(MAP(8+BLOCKS-NSQ-OFLOWS:8+BLOCKS-NSQ-1),CHAR(0))    !A
        ENDIF
        IF (NX.EQ.0) THEN
          PRINT *,MSGHDR,'NO DATA OVERFLOWS TILL BLOCKS RELEASED'
          RETURN
        ENDIF
        NBLOCX=BLOCKS-NSQ-OFLOWS-(XBLOKS+1)+NX                       !A
        CHARBLX(1:1)=CHAR(NBLOCX/256)
        CHARBLX(2:2)=CHAR(MOD(NBLOCX,256))
*
        K=0
        NTOTAL=0
        CHARBL(1:1)=CHAR(NBLOCK/256)
        CHARBL(2:2)=CHAR(MOD(NBLOCK,256))
        DO 410 I=1,NTRIES(LX)
         IF (INDEKS(I,LX)(22:23).EQ.CHARBL) THEN
           K=K+1
           SORTX(K)(1:5)=INDEKS(I,LX)(3:7)
           SORTX(K)(6:6)=INDEKS(I,LX)(12:12)
           NTOTAL=NTOTAL+ICHAR(INDEKS(I,LX)(12:12))
         ENDIF
  410   CONTINUE
*
        IDINB=K         ! NUMBER OF INDEX POINTERS TO THE TARGET BLOCK
        IF (IDINB.LE.1 .AND. IND.EQ.0) THEN
          PRINT *,MSGHDR,'WRONG BLOCK FOR ',IDENT
          PRINT *,'ONLY IDENT IN BLOCK TO BE SPLIT IS ',SORTX(1)(1:5)
          RETURN
        ENDIF
*
* SORT COUNTS BY IDENTIFIER & PICK THE IDENTIFIER WHICH SPLITS THE
* COUNTS EVENLY (SKIP THIS IF ALL THE IDENTIFIERS ARE THE SAME!)
* (N.B. BECAUSE OF GT.MID LATER THE SITUATION IS NOT SYMMETRICAL:
* IT'S OK IF THE FIRST IDENTIFIER IS MID BUT NOT THE LAST!  FOR LAND
* SYNOPS ALL THE ITEMS SORTED HAVE 5 FIGURES, SO MUST BE DIFFERENT.)
*
        CALL SORTCH(SORTX,5,IDINB,MASK(3:7))                         !E
        IF (IDINB.EQ.2) THEN               ! IF ONLY 2, TAKE THE FIRST
          MID=SORTX(1)(1:5)                ! - IF GT.MID AT DO 430!
        ELSE IF (IDINB.GT.2) THEN
          NOBS=0
          DO 420 I=1,IDINB-1
           NOBS=NOBS+ICHAR(SORTX(I)(6:6))
           IF (NOBS.GE.NTOTAL/2 .OR.
     &       SORTX(I+1)(1:5).EQ.SORTX(IDINB)(1:5)) GO TO 421
  420     CONTINUE
  421     MID=SORTX(I)(1:5)
        ENDIF
*
* MID WILL BE PUT IN THE ASSIGNMENT LIST LATER. INITIALISE NEW BLOCK.
* (CAN'T INITIALISE TILL NOW BECAUSE AREA USED AS WORK SPACE)
* NINBLX & LEFX WILL BE SET IN BLOCKX BEFORE IT'S WRITTEN; SET TAG NOW.
*
        NINBLX=0
        LEFX=BLKSIZ-INDHED
        BLOCKX(1:2)=BLOCK(1:2)
        BLOCKX(7:BLKSIZ)=' '
        MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(NINDEX(LX))
*
* IF THE FULL BLOCK HAS ONLY ONE IDENT, IT CAN'T BE SPLIT, SO SET THE
* TOP BIT OF THE MAP BYTE AS FOR INDEX OVERFLOW TO STOP STORING IN IT.
*
        IF (IDINB.EQ.1) THEN
          MAP(8+XBLOKS+NBLOCX:8+XBLOKS+NBLOCX)=CHAR(128+NINDEX(LX))
*
* IF ALL THE IDENTIFIERS ARE THE SAME, JUST STORE THE REPORT IN THE
* NEW BLOCK, UPDATING THE RANGES (ADDING ANOTHER WITH BOTH ENDPOINTS
* SAME, EFFECTIVELY NULL - THE IDENT'S IN THE INDEX, SO THE RANGE WON'T
* BE USED) & CHAINING ACROSS BLOCKS FROM THIS REPORT TO THE LATEST IN
* THE FULL BLOCK. WRITE THE NEW BLOCK AND THE MAP BLOCK OUT NOW; THE
* FULL BLOCK IS UNCHANGED, BUT WILL STILL BE WRITTEN OUT AT THE END.
*
          NB=NB+1
          MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
          MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
          BLOKID(NB*2-1)=IDENT(1:4)        ! SINGLE ID FOR NEW BLOCK
          BLOKID(NB*2)=IDENT(1:4)
*
          BLOCKX(3:3)=CHAR(0)
          BLOCKX(4:4)=CHAR(1)              ! ONE OB IN NEW DATA BLOCK
          L=LEN(BULL)+INDENT
          LEFX=LEFX-L-2
          BLOCKX(5:5)=CHAR(LEFX/256)
          BLOCKX(6:6)=CHAR(MOD(LEFX,256))
          BLOCKX(7:7)=CHAR(L/256)          ! SET FIRST LENGTH TO L
          BLOCKX(8:8)=CHAR(MOD(L,256))
*                                          ! (IND IS SET IF IDINB=1!)
          POINTER=INDEKS(IND,LX)(20:23)    ! CHAIN TO LATEST IN INDEX
          INDEKS(IND,LX)(20:20)=CHAR(0)
          INDEKS(IND,LX)(21:21)=CHAR(1)    ! FIRST RECORD IN BLOCK
          INDEKS(IND,LX)(22:23)=CHARBLX

          BLOCKX(BLKSIZ-L+1:BLKSIZ-INDENT)=BULL
          BLOCKX(BLKSIZ-INDENT+1:BLKSIZ-4)=ENTRY(1:INDENT-4)
          BLOCKX(BLKSIZ-3:BLKSIZ)=POINTER
          NOBS=ICHAR(INDEKS(IND,LX)(12:12)) ! UPDATE INDEX COUNT
          IF (NOBS.LT.255) INDEKS(IND,LX)(12:12)=CHAR(NOBS+1)
*
          WRITE (IFT,REC=1+XBLOKS+NBLOCX+NSQ,IOSTAT=IORC) BLOCKX
          IF (IORC.NE.0) CALL ABEND
*
          WRITE (IFT,REC=1,IOSTAT=IORC) MAP
          IF (IORC.NE.0) CALL ABEND
        ELSE IF (IDINB.GT.1) THEN
*
* COPY OBS IN 2ND HALF OF LIST TO NEW BLOCK, WHICH MEANS FOLLOWING
* POINTER CHAIN FOR EACH STATION (THE OBS WILL BE STORED IN REVERSE
* ORDER - DOESN'T MATTER), CHANGING THE CORRESPONDING INDEX POINTERS.
* KNOWING THE DISPLACEMENTS (RECDIS), WE CAN ZERO LENGTHS IN RECLEN.
*
* GO THROUGH THE INDEX ENTRIES, PICKING OBS IN THE FULL BLOCK WITH
* IDENTIFIERS AFTER THE BREAK POINT FOR TRANSFER TO THE NEW BLOCK.
* ZERO THE LENGTHS OF THE TRANSFERRED REPORTS AT THE START OF THE
* FULL BLOCK TO LET POINTERS TO REPORTS LEFT IN THIS BLOCK TO STAY
* THE SAME (POINTERS IN THE INDEX AND ON END OF OBS).
*
          DO 430 I=1,NTRIES(LX)
           IF (INDEKS(I,LX)(22:23).EQ.CHARBL .AND.
     -       INDEKS(I,LX)(3:7).GT.MID) THEN
             RECNO=ICHAR(INDEKS(I,LX)(20:20))*256
     -            +ICHAR(INDEKS(I,LX)(21:21))
             INDEKS(I,LX)(22:23)=CHARBLX
             INDEKS(I,LX)(20:20)=CHAR((NINBLX+1)/256)
             INDEKS(I,LX)(21:21)=CHAR(MOD(NINBLX+1,256))
  431        DISPL=BLKSIZ-RECDIS(RECNO)
*
* STORE REPORT IN SECOND BLOCK, ZEROING ITS LENGTH IN FIRST BLOCK.
*
             NINBLX=NINBLX+1
             IF (RECNO.EQ.1) THEN
               L=RECDIS(1)
             ELSE
               L=RECDIS(RECNO)-RECDIS(RECNO-1)
             ENDIF
             LEFX=LEFX-L-2

             BLOCKX(6+NINBLX*2-1:6+NINBLX*2-1)=CHAR(L/256)
             BLOCKX(6+NINBLX*2:6+NINBLX*2)=CHAR(MOD(L,256))
             START=INDHED+NINBLX*2+LEFX
             BLOCKX(START+1:START+L-4)=BLOCK(DISPL+1:DISPL+L-4)
             BLOCK(6+RECNO*2-1:6+RECNO*2-1)=CHAR(0)
             BLOCK(6+RECNO*2:6+RECNO*2)=CHAR(0)
*
* FOLLOW CHAIN IF OLD POINTER IS TO SAME BLOCK.  STOP IF ZERO POINTER
* OR DIFFERENT BLOCK (COPYING THE POINTER IF IT GOES ACROSS BLOCKS)
*
             RECNO=ICHAR(BLOCK(DISPL+L-3:DISPL+L-3))*256
     -          +ICHAR(BLOCK(DISPL+L-2:DISPL+L-2))
             IF (BLOCK(DISPL+L-1:DISPL+L).EQ.CHARBL) THEN
               N=NINBLX+1
               BLOCKX(START+L-3:START+L-3)=CHAR(N/256)
               BLOCKX(START+L-2:START+L-2)=CHAR(MOD(N,256))
               BLOCKX(START+L-1:START+L)=CHARBLX
               GO TO 431
             ELSE IF (RECNO.EQ.0) THEN
               BLOCKX(START+L-3:START+L-3)=CHAR(0)
               BLOCKX(START+L-2:START+L-2)=CHAR(0)
               BLOCKX(START+L-1:START+L-1)=CHAR(0)
               BLOCKX(START+L:START+L)=CHAR(0)
             ELSE           ! IF NOT SAME BLOCK & NOT ZERO, COPY POINTER
               BLOCKX(START+L-3:START+L)=BLOCK(DISPL+L-3:DISPL+L)
             ENDIF
           ENDIF
  430     CONTINUE
*
* SET NUMBER OF RECORDS & SPARE BYTES IN CHARACTER STRING,
* WRITE OUT SECOND DATA BLOCK, THEN INDEX BLOCK WITH POINTERS TO IT.
*
          BLOCKX(3:3)=CHAR(NINBLX/256)
          BLOCKX(4:4)=CHAR(MOD(NINBLX,256))
          BLOCKX(5:5)=CHAR(LEFX/256)
          BLOCKX(6:6)=CHAR(MOD(LEFX,256))

          WRITE (IFT,REC=1+XBLOKS+NBLOCX+NSQ,IOSTAT=IORC) BLOCKX
          IF (IORC.NE.0) CALL ABEND
*
* POINTERS HAVE BEEN CHANGED, SO WRITE THE WHOLE INDEX FOR THIS TIME.
* FIRST WRITE ANY FULL INDEX BLOCKS
*
          INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
          INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
          INDHDR(3:3)=CHAR(NTRIES(LX)/256)
          INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
          INDHDR(5:5)=CHAR(NREPS(LX)/256)
          INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

          NFULL=NTRIES(LX)/NBLIND            ! NUMBER OF FULL BLOCKS
          IX=NINDEX(LX)                      ! TO VARY BLOCK IN LOOP
          DO 440 NIND=1,NFULL
            NIBL=(NIND-1)*NBLIND             ! ENTRIES ALREADY WRITTEN
            NEXTIX=NOFLOW(NIND,LX)           ! OVERFLOW BLOCK NO

            WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &      (INDEKS(I,LX),I=NIBL+1,NIBL+NBLIND),
     &      CHAR(NEXTIX/256),CHAR(MOD(NEXTIX,256))
            IF (IORC.NE.0) CALL ABEND
            IX=NEXTIX                        ! OVERFLOW BLOCK NO    !2.0
  440     CONTINUE                                                  !2.0
*
* THEN THE LAST INDEX BLOCK (WHICH MAY BE THE ONLY ONE)
*
          IF (NTRIES(LX).GT.NFULL*NBLIND) THEN
            WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &         (INDEKS(I,LX),I=NFULL*NBLIND+1,NTRIES(LX)),
     &         (EMPTYX,I=NTRIES(LX)+1,(NFULL+1)*NBLIND),CHAR(0),CHAR(0)
            IF (IORC.NE.0) CALL ABEND
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
          NSTART=BLKSIZ               ! INITIALISE NEW START
          LN=ICHAR(BLOCK(7:7))*256+ICHAR(BLOCK(8:8))
*
          DO 450 I=1,NINBLK
           IF (LN.GT.0) THEN
             ISTART=BLKSIZ-RECDIS(I)  ! INPUT START (STILL OLD RECDIS!)
             NSTART=NSTART-LN         ! ADJUST NEW (OUTPUT) START
             IF (NSTART.GT.ISTART) THEN
               IF (NSTART-ISTART.LT.LN) THEN
                 BLOCKX(BLKSIZ-LN+1:BLKSIZ)=BLOCK(ISTART+1:ISTART+LN)
                 BLOCK(NSTART+1:NSTART+LN)=BLOCKX(BLKSIZ-LN+1:BLKSIZ)
               ELSE
                 BLOCK(NSTART+1:NSTART+LN)=BLOCK(ISTART+1:ISTART+LN)
               ENDIF
             ENDIF
           ENDIF
! NOW RESET RECDIS (OLD VALUE USED IN LOOP ABOVE!) & FIND NEXT LENGTH
           IF (I.EQ.1) THEN
             RECDIS(1)=LN
           ELSE
             RECDIS(I)=RECDIS(I-1)+LN
           ENDIF
           LN=ICHAR(BLOCK(7+I*2:7+I*2))*256+ICHAR(BLOCK(8+I*2:8+I*2))
  450     CONTINUE
*
          LEFT=NSTART-2*NINBLK-INDHED
          BLOCK(5:5)=CHAR(LEFT/256)
          BLOCK(6:6)=CHAR(MOD(LEFT,256))
          BLOCK(INDHED+2*NINBLK+1:INDHED+2*NINBLK+LEFT)=' '

          WRITE (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC) BLOCK
          IF (IORC.NE.0) CALL ABEND
*
* UPDATE THE LIST OF RANGES OF IDENTIFIERS ASSIGNED TO DATA BLOCKS,
* ADDING THE SECOND PART OF THE SPLIT RANGE TO THE END OF THE LIST.
*
          IF (LISTAG.EQ.CURENT) THEN
            NB=NB+1
            MAP(BLKSIZ-4006:BLKSIZ-4006)=CHAR(NB/256)
            MAP(BLKSIZ-4005:BLKSIZ-4005)=CHAR(MOD(NB,256))
            DO 470 I=1,NB
             IF (MID(1:4).GT.BLOKID(I*2-1) .AND.
     &           MID(1:4).LE.BLOKID(I*2)) GO TO 471
  470       CONTINUE
  471       BLOKID(NB*2-1)=MID(1:4)
            BLOKID(NB*2)=BLOKID(I*2)
            BLOKID(I*2)=MID(1:4)
          ENDIF
*
* FINALLY WRITE BACK THE MAP BLOCK & GO BACK TO STORE THE REPORT
*
          WRITE (IFT,REC=1,IOSTAT=IORC) MAP
          IF (IORC.NE.0) CALL ABEND
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
*
      BLOCK(3:3)=CHAR(NINBLK/256)
      BLOCK(4:4)=CHAR(MOD(NINBLK,256))
      BLOCK(5:5)=CHAR(LEFT/256)
      BLOCK(6:6)=CHAR(MOD(LEFT,256))

      WRITE (IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC) BLOCK
      IF (IORC.NE.0) CALL ABEND
*
* WRITE THE MAP BLOCK BACK ONLY IF A NEW DATA OR INDEX BLOCK HAS BEEN
* CLAIMED - OR IF THE NUMBER OF REPORTS FOR THIS INDEX BLOCK IS N*256.
*
      IF (NINBLK.EQ.1 .OR. NTRIES(LX)-(NTRIES(LX)/NBLIND)*NBLIND.EQ.1
     -                .OR. NREPS(LX).EQ.(NREPS(LX)/256)*256) THEN
        WRITE (IFT,REC=1,IOSTAT=IORC) MAP
        IF (IORC.NE.0) CALL ABEND
      ENDIF
*
* FINALLY THE INDEX.  IF IT EXTENDS OVER MORE THAN ONE BLOCK, THERE
* MAY BE NO NEED TO WRITE EVERYTHING.  ONLY ONE ENTRY HAS BEEN CHANGED
* OR ADDED, SO WE MUST WRITE THE BLOCK WITH THAT ENTRY IN.  AN ADDITION
* CHANGES THE COUNT, SO WRITE THE FIRST BLOCK (THOUGH EVERY BLOCK HAS
* THE COUNT IN, IT'S ACTUALLY READ FROM THE LAST) AND IF A NEW ENTRY
* STARTS A NEW BLOCK, WRITE THE BLOCK BEFORE WITH ITS POINTER SET.
* HENCE THE FOLLOWING STRUCTURE:
*          WORK OUT THE NUMBER OF FULL BLOCKS.                  (NFULL)
*          IF AN EXISTING ENTRY WAS UPDATED, THEN               (IND>0)
*            FIND WHICH BLOCK THE ENTRY IS IN.                  (NIND)
*            IF IN A FULL BLOCK (NOT THE LAST) WRITE THE FULL BLOCK.
*            IF IT IS IN THE LAST BLOCK, WRITE THE LAST BLOCK.
*          ELSE (IF AN ENTRY WAS ADDED)
*            IF THERE IS A FULL BLOCK, WRITE THE FIRST BLOCK.
*            IF THERE IS MORE THAN ONE FULL BLOCK, THEN
*              IF THIS IS THE FIRST ENTRY IN A BLOCK, THEN
*                WRITE THE LAST FULL BLOCK.
*            WRITE THE LAST BLOCK (NOT FULL) WITH THE NEW ENTRY IN.
*
      INDHDR(1:1)=CHAR(TIMTAG(LX)/256)
      INDHDR(2:2)=CHAR(MOD(TIMTAG(LX),256))
      INDHDR(3:3)=CHAR(NTRIES(LX)/256)
      INDHDR(4:4)=CHAR(MOD(NTRIES(LX),256))
      INDHDR(5:5)=CHAR(NREPS(LX)/256)
      INDHDR(6:6)=CHAR(MOD(NREPS(LX),256))

      NFULL=NTRIES(LX)/NBLIND                ! NUMBER OF FULL BLOCKS
      IF (IND.GT.0) THEN                     ! IF ENTRY UPDATED,
        NIND=(IND+NBLIND-1)/NBLIND           ! IT'S IN NIND-TH BLOCK.
        IF (NIND.EQ.1) THEN                  ! IF IT'S THE FIRST BLOCK,
          IX=NINDEX(LX)                      ! GET BASIC BLOCK NUMBER.
        ELSE                                 ! IF NOT THE FIRST,
          IX=NOFLOW(NIND-1,LX)               ! GET OVERFLOW BLOCK NUMBER
        ENDIF
*
        IF (NIND.LE.NFULL) THEN              ! IF IT'S IN A FULL BLOCK
          NIBL=(NIND-1)*NBLIND               ! ENTRIES TO SKIP
          WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I,LX),I=NIBL+1,NIBL+NBLIND),
     &       CHAR(NOFLOW(NIND,LX)/256),CHAR(MOD(NOFLOW(NIND,LX),256))
        ELSE                                 ! IF IN THE LAST BLOCK
          WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I,LX),I=NFULL*NBLIND+1,NTRIES(LX)),
     &       (EMPTYX,I=NTRIES(LX)+1,(NFULL+1)*NBLIND),CHAR(0),CHAR(0)
        ENDIF
*                                            ! IF ENTRY ADDED,
      ELSE                                   ! WRITE FIRST BLOCK
        IF (NFULL.GE.1) THEN
          IX=NINDEX(LX)                      ! GET BASIC BLOCK NUMBER.
          WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I,LX),I=1,NBLIND),
     &       CHAR(NOFLOW(1,LX)/256),CHAR(MOD(NOFLOW(1,LX),256))
          IF (IORC.NE.0) CALL ABEND
        ENDIF
*                                            ! WRITE LAST FULL BLOCK
        IF (NFULL.GT.1 .AND. MOD(0+NTRIES(LX),NBLIND).EQ.1) THEN
          IX=NOFLOW(NFULL-1,LX)
          NIBL=(NFULL-1)*NBLIND              ! ENTRIES TO SKIP
          WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I,LX),I=NIBL+1,NIBL+NBLIND),
     &       CHAR(NOFLOW(NFULL,LX)/256),CHAR(MOD(NOFLOW(NFULL,LX),256))
          IF (IORC.NE.0) CALL ABEND
        ENDIF
*                                            ! WRITE LAST OR ONLY BLOCK

! The last block is always written, so if the last block is also a   !g
! full block, reduce NFULL to get the range of entries right.        !g

        IF (NFULL.EQ.0) THEN                                         !g
          IX=NINDEX(LX)                      ! IF ONLY BLOCK...
        ELSE IF (NTRIES(LX).EQ.NFULL*NBLIND) THEN
          IF (NFULL.EQ.1) THEN                                       !g
            IX=NINDEX(LX)                    ! IF ONLY BLOCK...      !g
          ELSE                                                       !g
            IX=NOFLOW(NFULL-1,LX)            ! IF LAST BLOCK IS FULL
          ENDIF                                                      !g
          NFULL=NFULL-1                                              !g
        ELSE IF (NTRIES(LX).GT.NFULL*NBLIND) THEN
          IX=NOFLOW(NFULL,LX)                ! IF LAST BLOCK NOT FULL
        ENDIF
*
        WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &     (INDEKS(I,LX),I=NFULL*NBLIND+1,NTRIES(LX)),
     &     (EMPTYX,I=NTRIES(LX)+1,(NFULL+1)*NBLIND),CHAR(0),CHAR(0)
      ENDIF
      IF (IORC.NE.0) CALL ABEND
*
      RETURN
      END
