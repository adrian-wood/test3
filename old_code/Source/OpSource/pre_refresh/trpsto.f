      SUBROUTINE TRPSTO(DATIME,ENTRY,BULL,IFT,BLKSIZ,IDENT,TOR)       !B

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM       : TRPSTO
!
! PURPOSE       : STORE TCs IN THE MDB (23-BYTE INDEX ENTRIES,
!                 INDEX OVERFLOW ALLOWED, BUT NO CHAINING (EXCEPT
!                 FROM PREFERRED REPORT TO ANY PREVIOUS VERSION);
!                 DATA BLOCKS CLAIMED SEQUENTIALLY AS NEEDED)
!
! DATA TYPES    : TROPICAL ADVISORY
!
! CALLED BY     : ADVBUL
!
! CALLS         : DATE31
!
! PARAMETERS    : (1) DATE/TIME (YEAR, MONTH...)
!                 (2) INDEX ENTRY (WITHOUT TIMES & BLOCK/RECORD NO)
!                 (3) REPORT TO BE STORED
!                 (4) FT NUMBER (18 FOR AIREPS)
!                 (5) BLOCKSIZE OF OUTPUT DATA SET
!                 (6) IDENTIFIER TO GO IN INDEX ENTRY
!                 (7) T.O.R.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:34$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trpsto.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:34    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:29  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/04/17  15:58:25  15:58:25  usmdb (Generic MetDB account)
! Change dynamic common name from ARPDC to TRPDC so as to differ
! from AIRSTO DC - S.Cox
! 
! Revision 1.1  98/06/11  13:06:03  13:06:03  usmdb (Generic MDB account)
! Initial revision
!
! MADE FROM AIRSTO MAY 1998 but duplicate check has been removed.
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

!----------------------------------------------------------------------
!Declare common function
!----------------------------------------------------------------------
      INTEGER   CENTURY     ! Common function to determine C20/C21.

!----------------------------------------------------------------------
!Declare Integer
!----------------------------------------------------------------------
      INTEGER   DATIME(5)
      INTEGER   TOR(5)
      INTEGER   NOW(8)
      INTEGER   CENDAY
      INTEGER   CENTHR
      INTEGER   SLOTHR
      INTEGER   SLODAY
      INTEGER   CURENT
      INTEGER   HOURNOW
      INTEGER   LATEST
      INTEGER   INDHED
      INTEGER   BLKSIZ
      INTEGER   START
      INTEGER   BLOCKS
      INTEGER   XBLOKS
      INTEGER   XHOURS
      INTEGER   AMSTAR
      INTEGER   OFLOWS
      INTEGER   INDENT
      INTEGER   BLKTAG
      INTEGER   NINBLK
      INTEGER   LEFT
      INTEGER   NTRIES
      INTEGER   LASLEF
      INTEGER   TIMTAG              ! INDEX HEADERS
      INTEGER   MAXOFL
      INTEGER   MAXBLK
      INTEGER   INDLEN
      INTEGER   MAPFT
      INTEGER   INDXFT
      INTEGER   IFT
      INTEGER   IORC
      INTEGER   NB
      INTEGER   NSEQBL
      INTEGER   NSQ
      INTEGER   NBLIND
      INTEGER   INDHOR
      INTEGER   ITOR
      INTEGER   X,Y
      INTEGER   TORHOUR
      INTEGER   TORMIN
      INTEGER   NXBLOK
      INTEGER   NINDEX
      INTEGER   IX
      INTEGER   NIND
      INTEGER   NIBL
      INTEGER   I
      INTEGER   NBLOK
      INTEGER   N
      INTEGER   IND
      INTEGER   INDHR
      INTEGER   IFLAGS
      INTEGER   LASBLK
      INTEGER   NBLOCK
      INTEGER   NX
      INTEGER   L
      INTEGER   NFULL
      INTEGER   MESSFT
      PARAMETER (MAXBLK=27998,INDLEN=23,INDHED=6,MAXOFL=10)
      INTEGER   NOFLOW(MAXOFL)              ! OVERFLOW POINTERS
      INTEGER   RECDIS(MAXBLK/2)

!---------------------------------------------------------------------
!Declare Character
!---------------------------------------------------------------------
      CHARACTER BULL*(*)
      CHARACTER IDENT*(*)
      CHARACTER ENTRY*(*)
      CHARACTER*23 INDEKS(MAXOFL*MAXBLK/INDLEN) ! INDEX ENTRIES
      CHARACTER EMPTYX*23
      CHARACTER INDHDR*6
      CHARACTER INDOVR*2
      CHARACTER POINTER*4
      CHARACTER MSGHDR*14
      CHARACTER HEAD*132                      !Revision information
      CHARACTER*(MAXBLK) MAP,BLOCK

!---------------------------------------------------------------------
!Declare Logical
!---------------------------------------------------------------------
      LOGICAL FULL
      LOGICAL FAIL

!---------------------------------------------------------------------
!Declare common blocks and Data statements
!---------------------------------------------------------------------
      COMMON /TRPDC/ RECDIS, MAP,INDEKS,BLOCK                       !1.2

      DATA    LATEST/0/                         ! LATEST=0 AT START
      DATA    EMPTYX/' '/
      DATA    MSGHDR/'....Z TRPSTO: '/
      DATA    MAPFT/0/,INDXFT/0/,MESSFT/0/
      SAVE
      FAIL=.FALSE.

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/trpsto.F,v $
     &'//'$ $Date: 30/01/2006 20:25:34$ $Revision: 1$'

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
!                                            ! FIRST TIME READ MAP
      IF (LATEST.EQ.0 .OR. IFT.NE.MAPFT) THEN
        READ (IFT,REC=1,IOSTAT=IORC) MAP(1:BLKSIZ)
        IF (IORC.NE.0) THEN
          CALL ABEND
        ENDIF
        MAPFT=IFT

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

!----------------------------------------------------------------------
! Set nsq to calculate block numbers allowing for sequence record.
!----------------------------------------------------------------------

        NSEQBL=ICHAR(MAP(8+BLOCKS:8+BLOCKS))

        IF(NSEQBL.GT.0)THEN
          NSQ=1
        ELSE
          NSQ=0
        ENDIF

        NBLIND=(BLKSIZ-INDHED-2)/INDENT      ! MAX ENTRIES IN INDEX BLOK
        IF (LATEST.EQ.0) LATEST=HOURNOW      ! LATEST CENTURY-HOUR
      ENDIF

!----------------------------------------------------------------------
!
! Complete time fields in index entry (time & time of receipt),
! find first hour in slot (slothr) & make time tag.
! time of receipt is in minutes from the slot time (hournow-slothr,
! where hournow is the current century-hour) and can be <0 or >1440.
!
!----------------------------------------------------------------------

      INDHOR=MOD(DATIME(4)+24-AMSTAR,XHOURS) ! HOUR MINUS SLOT START
      IF (MOD(ICHAR(ENTRY(7:7)),16).EQ.0) THEN
        ENTRY(1:1)=CHAR(INDHOR)              ! HOUR RELATIVE TO SLOT
      ELSE                                   ! IF COR NUMBER NONZERO,
        ENTRY(1:1)=CHAR(INDHOR+128)          ! SET COR IN SAME BYTE.
      ENDIF
      ENTRY(2:2)=CHAR(DATIME(5))             ! MINUTES

      IF (DATIME(1).LT.1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))

      IF (TOR(1).LT.1900) TOR(1)=TOR(1)+CENTURY(TOR(1))

      CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
      CENTHR=(CENDAY-1)*24+DATIME(4)         ! CENTURY-HOUR OF DATA
      SLOTHR=CENTHR-INDHOR                   ! SLOT START (CENTURY-HOUR)

      ITOR=HOURNOW-SLOTHR                    ! TOR HOUR RELATIVE TO SLOT
      CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! DAY OF MONTH FOR SLOT
      CURENT=SLODAY*256+MOD(SLOTHR,24)

!--------------------------------------------------------------------
!So far we have delt with the time of the data. To complete the index
!entry we need time of receipt.
!--------------------------------------------------------------------

      CALL DATE31(TOR(3),TOR(2),TOR(1),CENDAY)
      TORHOUR=(CENDAY-1)*24+TOR(4)
      TORMIN=TORHOUR*60+TOR(5)

      ITOR=(TORHOUR-SLOTHR)*60+TOR(5)
      IF (ITOR.LT.0) ITOR=65536+ITOR         ! TWOS COMPLEMENT
      ENTRY(18:18)=CHAR(ITOR/256)            ! Put in TOR
      ENTRY(19:19)=CHAR(MOD(ITOR,256))       ! Put in TOR

!---------------------------------------------------------------------
! If the data is for too long before the the most recent data stored
! ('too long' means more than the period covered by the data base),
! then reject it (to avoid overwriting current data with old data!).
! if the new data is more recent, update the latest hour - and the
! index block tag for which the list of ranges can be updated (set
! when a new index block is started, but this copes until then...).
!
!  (xbloks-1 rather than xbloks because although we can store data
! extending over a period of xbloks*xhours if that period starts at
! the start of an index block, in general such a period would reuse
! the latest index block for the oldest data.)
!---------------------------------------------------------------------

      IF (CENTHR.LE.LATEST-(XBLOKS-1)*XHOURS) THEN
        PRINT *,MSGHDR,LATEST-CENTHR,'HOURS OLD   ',BULL(:60)
        GOTO 999
      ENDIF

      IF (CENTHR.GT.LATEST) THEN
        LATEST=CENTHR
      ENDIF

!----------------------------------------------------------------------
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
!----------------------------------------------------------------------

      NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,XBLOKS)+2  ! INDEX BLOCK NUMBER

      IF (NINDEX.NE.NXBLOK .OR. IFT.NE.INDXFT) THEN
        NINDEX=NXBLOK                    ! KEEP INDEX BLOCK NUMBER
        IX=NINDEX                        ! TO VARY IN READ LOOP
        NIND=1                           ! CONTINUATION NUMBER
   30   NIBL=(NIND-1)*NBLIND             ! ENTRIES ALREADY READ IN

        READ (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &      (INDEKS(I),I=NIBL+1,NIBL+NBLIND),INDOVR
        IF (IORC.NE.0) THEN
          CALL ABEND
        ENDIF
        INDXFT=IFT

        TIMTAG=ICHAR(INDHDR(1:1))*256+ICHAR(INDHDR(2:2))
        NTRIES=ICHAR(INDHDR(3:3))*256+ICHAR(INDHDR(4:4))
        LASLEF=ICHAR(INDHDR(5:5))*256+ICHAR(INDHDR(6:6))
        NOFLOW(NIND)=ICHAR(INDOVR(1:1))*256+ICHAR(INDOVR(2:2))

* IF THE INDEX BLOCK HAS ENTRIES FOR THE TIME WANTED, CARRY ON READING
* OVERFLOW BLOCKS (IF ANY); IF WRONG TIME, NO NEED TO READ ANY FURTHER
* - MAIN BLOCK WILL BE REINITIALISED AND OVERFLOWS RELEASED.

        IF (TIMTAG.EQ.CURENT .AND. NOFLOW(NIND).GT.0) THEN
          IX=NOFLOW(NIND)                ! IF SO, RESET BLOCK NO
          NIND=NIND+1                    ! FURTHER CONTINUATION
          IF(NIND.GT.MAXOFL)THEN
            PRINT*,MSGHDR,'TOO MANY INDEX ENTRIES FOR',SLOTHR,'Z'
            GOTO 999
          ENDIF
          GO TO 30
        ENDIF
      ENDIF
*                                                                     *
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
        NBLOK=1
  110   N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(NINDEX))
        IF (N.GT.0) THEN
          NBLOK=NBLOK+(N-1)
          MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
          IF (1+XBLOKS+NBLOK.LT.BLOCKS) GO TO 110
        ENDIF
*                      NOW DO SAME FOR INDEX OVERFLOWS (TOP BIT SET)
        NBLOK=1
  111   N=INDEX(MAP(8+XBLOKS+NBLOK:8+BLOCKS-1-NSQ),CHAR(128+NINDEX))
        IF (N.GT.0) THEN
          NBLOK=NBLOK+(N-1)
          MAP(8+XBLOKS+NBLOK:8+XBLOKS+NBLOK)=CHAR(0)
          IF (1+XBLOKS+NBLOK.LT.BLOCKS) GO TO 111
        ENDIF
*
* FINALLY REINITIALISE THE INDEX BUFFER FOR THE NEW HOUR.
*
        TIMTAG=CURENT
        NTRIES=0
        LASLEF=BLKSIZ-INDHED
        NOFLOW(1)=0
      ENDIF
***********************************************************************
*
* SEE IF THERE IS ALREADY AN INDEX ENTRY FOR THIS IDENT, TIME & PLACE.
* IF SO, ONLY STORE NEW OB IF IT'S A COR OR HAS MORE DATA, CHAINING BACK
* TO THE OLD VERSION TO LET OBS PREFERRED AT A GIVEN TIME BE RETRIEVED.
* JUST SEE HERE IF THERE'S AN INDEX ENTRY, DO THE CHAINING LATER.
*
* PREFERRED FLAGS ARE SET BECAUSE EXPECTED BY RETRIEVAL, BUT NOT UNSET
* (FLAGS COULD ONLY BE UNSET BY READING IN THE OLD REPORT TO GET AT ITS
* TRAILER - UNNECESSARY I/O!) - BECAUSE THE ORDER OF THE CHAIN DECIDES:
* THE PREFERRED REPORT IS ALWAYS AT THE START OF A CHAIN, AND ANY
* REPORT STORED BEFORE IT WAS PREFERRED WHEN STORED...)
*
***********************************************************************
      IND=0
      I=1
      DO WHILE (IND.EQ.0 .AND. I.LE.NTRIES)
        INDHR=MOD(ICHAR(INDEKS(I)(1:1)),128)  ! HOUR WITHOUT COR FLAG

! CHECK THAT HOUR, MINUTE, LAT/LONG & IDENT (ONLY 8 CHARS BECAUSE 9TH
! USED FOR MINUTES IN INDEX) & bottom 4 bits of byte 17 (data type)
! ARE ALL THE SAME - IF SO, SAME REPORT.

!       IF (INDHR.EQ.INDHOR .AND. ENTRY(2:2).EQ.INDEKS(I)(2:2)
!    &      .AND. ENTRY(13:16).EQ.INDEKS(I)(13:16)
!    &      .AND. IDENT(1:8).EQ.INDEKS(I)(3:10)
!    &      .AND. MOD(ICHAR(ENTRY(17:17)),16).EQ.
!    &            MOD(ICHAR(INDEKS(I)(17:17)),16)) THEN

! (COR FLAG IN FIRST BIT OF ENTRY, SO IF THE FIRST BYTE IS JUST THE
! HOUR, THE COR FLAG ISN'T SET; DATA COUNT IN BYTE 12)

!         IF (ENTRY(1:1).EQ.CHAR(INDHOR) .AND.        ! IF NOT COR AND
!    &        ENTRY(12:12).LE.INDEKS(I)(12:12)) THEN  ! NOT MORE DATA,
!           GOTO 999                                  ! DON'T KEEP IT.
!         ELSE
!           IND=I
!         ENDIF
!       ENDIF
        I=I+1
      ENDDO

! SET "IS PREFERRED" AND "WAS PREFERRED" FLAGS (NOT TO BE UNSET - OK!)

      IFLAGS=ICHAR(ENTRY(17:17))
      IF (IFLAGS.LT.96) ENTRY(17:17)=CHAR(IFLAGS+128+32)
***********************************************************************
*
* STORE IN THE SAME DATA BLOCK AS THE LAST OB IN THIS INDEX IF THERE'S
* ROOM; IF NOT, START A NEW DATA BLOCK.
*
***********************************************************************
      IF (LEN(BULL)+INDENT+2.LE.LASLEF .AND. NTRIES.GT.0) THEN
        LASBLK=ICHAR(INDEKS(NTRIES)(INDENT-1:INDENT-1))*256
     &        +ICHAR(INDEKS(NTRIES)(INDENT:INDENT))

! READ BLOCK IF IT'S NOT IN CORE (EITHER DIFFERENT DATA TYPE LAST TIME
! OR DIFFERENT INDEX PERIOD)

        IF (NBLOCK.NE.LASBLK .OR. IFT.NE.MESSFT) THEN
          NBLOCK=LASBLK
         READ(IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC)BLOCK(1:BLKSIZ)
          IF (IORC.NE.0) CALL ABEND
          MESSFT=IFT

          BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
          NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
          LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))
        ENDIF

        IF (LEFT .NE. LASLEF) THEN
          PRINT *,MSGHDR,'DATA & INDEX BLOCKS INCONSISTENT'
          PRINT *,'BLKTAG,LEFT,LASLEF:',BLKTAG,LEFT,LASLEF
          RETURN
        ENDIF
      ELSE

! GET NEW BLOCK NUMBER FROM MAP
! IF NO FREE BLOCK, GIVE UP, PRINTING MESSAGE FIRST TIME THIS HAPPENS
! (THOUGH ACTUALLY, IF WE GET AN OB FOR A DIFFERENT INDEX PERIOD THAT
! WE CAN STORE, THE MESSAGE WILL APPEAR AGAIN NEXT TIME WE GET AN OB
! FOR THIS PERIOD...)

        NBLOCK=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-NSQ-1),CHAR(0))
        IF (NBLOCK.EQ.0) THEN
          IF (.NOT.FULL) THEN
            FULL=.TRUE.
           PRINT *,MSGHDR,'NO FREE DATA BLOCKS',SLOTHR,'Z OB NOT STORED'
          ENDIF
          GOTO 999
        ENDIF
        FULL=.FALSE.

! SET TAG, NINBLK & LEFT IN BLOCK HEADER & CLEAR REST OF BLOCK

        NINBLK=0
        BLKTAG=CURENT
        LEFT=BLKSIZ-INDHED
        BLOCK(1:1)=CHAR(BLKTAG/256)
        BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
        BLOCK(7:BLKSIZ)=' '

        MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(NINDEX)
      ENDIF
***********************************************************************
*
* WE NOW HAVE A DATA BLOCK WITH SPACE FREE, SO INDEX & STORE THE REPORT
*
***********************************************************************
      NINBLK=NINBLK+1                      ! ONE MORE OB IN BLOCK

      POINTER(1:1)=CHAR(NINBLK/256)
      POINTER(2:2)=CHAR(MOD(NINBLK,256))
      POINTER(3:3)=CHAR(NBLOCK/256)
      POINTER(4:4)=CHAR(MOD(NBLOCK,256))

      IF (NTRIES.GE.NBLIND*MAXOFL) THEN
        PRINT *,MSGHDR,'NO MORE INDEX ENTRIES CAN BE MADE'
        PRINT *,SLOTHR,'Z INDEX',NTRIES,'ALREADY - ARRAY FULL!'
        RETURN
      ENDIF

! If an index entry existed, don't reuse it: make a new entry so that
! the last entry always points to the last ob stored, zeroing the old
! pointer so that the old entry is skipped.  Otherwise there's trouble
! deciding where to store the next ob if a chained ob started a new
! data block!

      IF (IND.GT.0) THEN
        ENTRY(20:23)=INDEKS(IND)(20:23)    ! OLD POINTER IN TRAILER
        INDEKS(IND)(20:23)=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
      ELSE
        ENTRY(20:23)=CHAR(0)//CHAR(0)//CHAR(0)//CHAR(0)
      ENDIF

      NTRIES=NTRIES+1                      ! one more index entry
      INDEKS(NTRIES)(1:2)=ENTRY(1:2)       ! TIME (& COR FLAG)
      INDEKS(NTRIES)(3:11)=IDENT           ! IDENT
      INDEKS(NTRIES)(12:19)=ENTRY(12:19)   ! DATA COUNT, LAT/LONG, TOR
      INDEKS(NTRIES)(20:23)=POINTER
*
* IF THIS ENTRY NEEDS TO START A NEW INDEX BLOCK, IT'S ENOUGH HERE TO
* CLAIM A BLOCK NUMBER AND LIST IT AS THE NEXT.  (FOR CONSISTENCY WITH
* INDEX BLOCK NUMBERING INCLUDE 1 FOR MAP BUT IGNORE SEQUENCE RECORD!)
* BLOCK TO CONTINUE THE INDEX; THIS NUMBER WILL BE SET AS A POINTER
* AT THE END OF THE PREVIOUS BLOCK WHEN THE INDEX IS WRITTEN BACK.
*
      NIND=NTRIES/NBLIND                   ! NO OF FULL INDEX BLOCKS
      IF (NIND.GE.1 .AND. NTRIES-NIND*NBLIND.EQ.1) THEN
        NX=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-NSQ-1),CHAR(0))
        IF (NX.EQ.0) THEN
          PRINT *,MSGHDR,'NO MORE OVERFLOWS FOR',SLOTHR,'Z INDEX'
          NTRIES=NTRIES-1
          RETURN
        ELSE
          NOFLOW(NIND)=1+XBLOKS+NX
          NOFLOW(NIND+1)=0
          MAP(8+XBLOKS+NX:8+XBLOKS+NX)=CHAR(128+NINDEX)
        ENDIF
      ENDIF

      L=LEN(BULL)+INDENT
      LEFT=LEFT-L-2
      LASLEF=LEFT
      BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)
      BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))

      IF (NINBLK.GT.1) THEN
        RECDIS(NINBLK)=RECDIS(NINBLK-1)+L
      ELSE
        RECDIS(NINBLK)=L
      ENDIF

      START=INDHED+NINBLK*2+LEFT
      BLOCK(START+1:START+LEN(BULL))=BULL

      BLOCK(START+LEN(BULL)+1:START+L)=ENTRY
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

      WRITE(IFT,REC=1+XBLOKS+NBLOCK+NSQ,IOSTAT=IORC)BLOCK(1:BLKSIZ)
      IF (IORC.NE.0) CALL ABEND
*
* WRITE THE MAP BLOCK BACK ONLY IF NEW DATA OR INDEX BLOCK CLAIMED.
*
      IF (NINBLK.EQ.1 .OR. NTRIES-(NTRIES/NBLIND)*NBLIND.EQ.1) THEN
        WRITE (IFT,REC=1,IOSTAT=IORC) MAP(1:BLKSIZ)
        IF (IORC.NE.0) CALL ABEND
      ENDIF
*
* FINALLY THE INDEX BLOCK(S), FIRST ANY FULL ONES & THEN THE LAST ONE
* (UNLESS THE LAST BLOCK IS ALSO A FULL ONE!)
*
      INDHDR(1:1)=CHAR(TIMTAG/256)
      INDHDR(2:2)=CHAR(MOD(TIMTAG,256))
      INDHDR(3:3)=CHAR(NTRIES/256)
      INDHDR(4:4)=CHAR(MOD(NTRIES,256))
      INDHDR(5:5)=CHAR(LASLEF/256)
      INDHDR(6:6)=CHAR(MOD(LASLEF,256))

      NFULL=NTRIES/NBLIND                    ! NUMBER OF FULL BLOCKS
      IX=NINDEX                              ! FIRST INDEX BLOCK
      DO NIND=1,NFULL                        ! WRITE ALL FULL BLOCKS
        NIBL=(NIND-1)*NBLIND                 ! ENTRIES ALREADY WRITTEN
        WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I),I=NIBL+1,NIBL+NBLIND),
     &       CHAR(NOFLOW(NIND)/256),CHAR(MOD(NOFLOW(NIND),256))
        IF (IORC.NE.0) CALL ABEND
        IX=NOFLOW(NIND)                      ! OVERFLOW (IF ANY)
      ENDDO

      IF (NTRIES.GT.NFULL*NBLIND) THEN       ! IF LAST BLOCK NOT FULL...
        WRITE (IFT,REC=IX+NSQ,IOSTAT=IORC) INDHDR,
     &       (INDEKS(I),I=NFULL*NBLIND+1,NTRIES),
     &       (EMPTYX,I=NTRIES+1,(NFULL+1)*NBLIND),CHAR(0),CHAR(0)
        IF (IORC.NE.0) CALL ABEND
      ENDIF

 999  RETURN
      END
