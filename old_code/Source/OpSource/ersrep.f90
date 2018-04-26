SUBROUTINE ERSREP(NOBS,DATIME,ENTRY,BULL,IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : ERSREP
!
! PURPOSE       : STORE BATCH OF ERS-1 SOUNDINGS
!
! CALLED BY     : ERSIND
!
! CALLS         : CENTURY, DATE31
!                 METDB_CWRITE_DIR and METDB_CREAD_DIR from MetDB_c_util
!
! ARGUMENTS     : (1) NUMBER OF OBS IN BATCH
!                 (2) DATE/TIME OF FIRST OB IN BATCH (YEAR, MONTH...)
!                 (3) INDEX ENTRY (INCOMPLETE)
!                 (4) BUFR MESSAGE WITH DESCRIPTORS IN
!                 (5) FT NUMBER
!
! REVISION INFO :
!
! $Workfile: ersrep.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 03/02/2012 10:30:38$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         03/02/2012 10:30:38    Sheila Needham  Remove
!        PRINT* with binary data
!  5    MetDB_Refresh 1.4         14/03/2011 16:56:49    Brian Barwell   SAVE
!       added to preserve MAP for next call.
!  4    MetDB_Refresh 1.3         07/03/2011 10:04:51    Brian Barwell
!       Modified for C I/O.
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 11:28:01    John Norton
!       Pre-porting f77 version
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

! Use statements:

USE century_mod ! function
USE datim_mod
USE zpdate_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,           INTENT(IN)    :: NOBS      !a01
INTEGER,           INTENT(INOUT) :: DATIME(5) !a02
CHARACTER(LEN=12), INTENT(INOUT) :: ENTRY     !a03
CHARACTER(LEN=*),  INTENT(INOUT) :: BULL      !a04
INTEGER,           INTENT(INOUT) :: IFT       !a05

! Local declarations:

INTEGER,     PARAMETER ::  BLKSIZ=23476
INTEGER,     PARAMETER ::  HEADER=6
INTEGER,     PARAMETER ::  INDENT=12

INTEGER          ::  AMSTAR
INTEGER          ::  BLKTAG
INTEGER          ::  BLOCKS   ! Number of records in storage data set
INTEGER          ::  CENDAY   ! Century day
INTEGER          ::  CENTHR   ! Century hour
INTEGER          ::  CURENT
INTEGER          ::  I !?
INTEGER          ::  IN !?
INTEGER          ::  INDHOR !?
INTEGER          ::  INDNOBS !?
INTEGER          ::  IOFLAG  ! Return flag from I/O
INTEGER          ::  ITOR !?
INTEGER          ::  J !?
INTEGER          ::  L !?
INTEGER          ::  LASBLK !?
INTEGER          ::  LASLEF
INTEGER          ::  LASTFT=0 !?
INTEGER          ::  LASTHR !?
INTEGER          ::  LEFT
INTEGER          ::  N !?
INTEGER          ::  NBLOCK !?
INTEGER          ::  NINBLK
INTEGER          ::  NINDEX !?
INTEGER          ::  NOW(8)  ! System clock time from call to DATIM
INTEGER          ::  NSEQBL !?
INTEGER          ::  NSQ    ! 0/1 = BUFR sequence record isn't/is present
INTEGER          ::  NTRIES
INTEGER          ::  NUMREC ! Record number for direct access I/0
INTEGER          ::  NXBLOK !?
INTEGER          ::  SLODAY
INTEGER          ::  SLOTHR
INTEGER          ::  START
INTEGER          ::  TIMTAG
INTEGER          ::  TOROUR  ! Hour of time of receipt
INTEGER          ::  TORMIN  ! Minute of time of receipt
INTEGER          ::  X       ! Dummy argument to CALL DATE13
INTEGER          ::  XBLOKS  ! Number of index records in storage data set
INTEGER          ::  XHOURS  ! Index period in hours
INTEGER          ::  Y       ! Dummy argument to CALL DATE13

CHARACTER(LEN=BLKSIZ) ::  BLOCK    ! Data record
CHARACTER(LEN=BLKSIZ) ::  INDEKS   ! Index record
CHARACTER(LEN=BLKSIZ) ::  MAP      ! Map record

LOGICAL          ::  FULL=.FALSE.

SAVE                 ! To preserve MAP from one call to the next

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! CHECK UNIT NUMBER TO SEE IF SSM/I DATA
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

CALL DATIM(NOW)

! COMPARE COUNT IN 4TH BYTE OF FIRST INDEX ENTRY WITH NOBS FOR THIS
! MESSAGE: IF THEY'RE DIFFERENT, ASSUME THE INDEX IS FOR THE WRONG
! DATA BASE AND READ THE MAP & INDEX BLOCKS EVEN IF SAME FT NUMBER.
! (N.B. THIS DOESN'T MAKE IT IMPOSSIBLE TO STORE PRODUCTS WITH NOBS
! VARYING FROM MESSAGE TO MESSAGE; IT JUST MEANS MORE READS.)

INDNOBS=ICHAR(INDEKS(10:10))
IF (INDNOBS /= MOD(NOBS,256) .AND. IFT == LASTFT &
  .AND. IFT >= 5.AND.IFT < 25) THEN
  PRINT *,'ERS MESSAGES SHOULD ALL HAVE SAME NUMBER OF OBS'
  WRITE (*,'(I9,''/'',I4.4,''Z   FT'',I2.2)') &
   NOW(8)*10000+NOW(7)*100+NOW(6), NOW(5)*100+NOW(4), IFT
  PRINT *,NOBS,'OBS IN MESSAGE TO BE STORED'
  PRINT *,INDNOBS,'OBS IN FIRST INDEX ENTRY FOR THAT DATE/TIME'
  LASTFT=0
END IF

! READ THE FILE PARAMETERS, MAP & LOCAL DESCRIPTOR SEQUENCES (IF ANY)

IFLABEL1: &
IF (IFT /= LASTFT) THEN
  NUMREC = 1
  CALL METDB_CREAD_DIR (IFT, MAP, BLKSIZ, NUMREC, IOFLAG)

  BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))
  XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))
  XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))
  AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))

  NSEQBL=ICHAR(MAP(BLOCKS+8:BLOCKS+8))
  IF (NSEQBL > 0) THEN
    NSQ=1
  ELSE
    NSQ=0
  END IF

  CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
  LASTHR=(CENDAY-1)*24+NOW(5)
END IF IFLABEL1

! SET TYPE OF MESSAGE (EQUAL TO THE FT NUMBER) IN CASE WE'RE MERGING
! & GET HOURS & MINUTES OF TIME OF RECEIPT FROM MESSAGE FOR INDEX.
! (DISPLACEMENTS AS IN BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)

BULL(14:14)=CHAR(IFT)                  ! BUFR SUBTYPE
TOROUR=ICHAR(BULL(24:24))              ! HOUR OF T.O.R.
TORMIN=ICHAR(BULL(25:25))              ! MINS OF T.O.R.

! COMPLETE TIME FIELDS IN INDEX ENTRY:
!  SECOND BYTE: PUT HOUR IN 5 BITS AFTER SATELLITE IDENTIFIER
!  NINTH BYTE: TOR IN MINUTES FROM START OF SLOT DIVIDED BY 3
! AND FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.

N=XHOURS                               ! FULLWORD FOR MOD
INDHOR=MOD(DATIME(4)+24-AMSTAR,N)      ! HOUR MINUS SLOT START
ENTRY(2:2)=CHAR(ICHAR(ENTRY(2:2))+INDHOR)

IF (DATIME(1) < 1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))

CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
CENTHR=(CENDAY-1)*24+DATIME(4)         ! CENTURY-HOUR
SLOTHR=CENTHR-INDHOR                   ! HOUR AT START OF SLOT

CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! DAY OF MONTH FOR SLOT
SLOTHR=MOD(SLOTHR,24)                  ! CENT-HOUR TO HOUR OF DAY
CURENT=SLODAY*256+SLOTHR               ! TIME TAG

ITOR=MOD(TOROUR+24-SLOTHR,24)          ! TOR HOUR RELATIVE TO SLOT
ITOR=(ITOR*60 + TORMIN)/3              ! CONVERT TO MINUTES & /3
IF (ITOR > 255) ITOR=255               ! MISSING IF TOO LATE
ENTRY(9:9)=CHAR(ITOR)

! IF THIS DATA IS 60 HOURS OR MORE OLDER THAN THE LATEST DATA STORED,
! REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH DATA 60 HOURS OLD!).
! IF IT'S MORE RECENT, UPDATE THE LATEST HOUR.

IF (CENTHR <= LASTHR-XBLOKS*XHOURS) THEN
  LASTFT=IFT
  RETURN
END IF
IF (CENTHR > LASTHR) LASTHR=CENTHR

! THE INDEX IS DIVIDED INTO HOURLY SEGMENTS. WORK OUT WHICH SEGMENT
! FROM THE CENTURY-HOUR AND READ IN THE CORRESPONDING INDEX BLOCK.
! (HOURLY SEGMENTS SHOULD AVOID INDEX OVERFLOW, WHICH IS NOT ALLOWED
!  FOR: USE SHORTER SEGMENTS IF OVERFLOW LOOKS LIKELY.)
!
! INDEX BLOCK:               (SEE BUFIND FOR STRUCTURE OF ENTRIES)
! ----------------------------------------------------------- - - - -
! : DATE/ : NO. OF : LENGTH FREE : FIRST 12-   : SECOND   :
! : TIME  : ENTRIES: IN LAST BLK : BYTE ENTRY  : ENTRY    :
! ----------------------------------------------------------- - - - -
! 0       2        4             6            18         30
!
! READ IN INDEX BLOCK FOR SLOT               (IF NOT IN CORE ALREADY)
!
N=XBLOKS                               ! FULLWORD FOR MOD
NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,N)+2 ! NUMBER OF INDEX BLOCK
IF (IFT /= LASTFT .OR. NINDEX /= NXBLOK) THEN
  NINDEX=NXBLOK
  NUMREC = NINDEX + NSQ
  CALL METDB_CREAD_DIR (IFT, INDEKS, BLKSIZ, NUMREC, IOFLAG)

  TIMTAG=ICHAR(INDEKS(1:1))*256+ICHAR(INDEKS(2:2))
  NTRIES=ICHAR(INDEKS(3:3))*256+ICHAR(INDEKS(4:4))
  LASLEF=ICHAR(INDEKS(5:5))*256+ICHAR(INDEKS(6:6))
END IF
!**********************************************************************
!                                                                     *
! IF THE INDEX HAS ANY ENTRIES, EITHER DELETE THEM (IF THEY'RE FOR AN *
! EARLIER HOUR) OR CHECK FOR DUPLICATES (IF CURRENT HOUR)             *
!                                                                     *
!**********************************************************************
!
! THE TIME TAG (DATE/TIME IN INDEX BLOCK) IS (DAY OF MONTH)*256+HOUR.
! THE TIME TAG IS THE SLOT THIS SEGMENT WAS LAST USED FOR.  IF IT WAS
! LAST USED FOR THE OLDEST DATA IN THE BANK (60 HOURS AGO IF 60 HOURS'
! DATA KEPT), CLEAR THE BLOCKS USED BEFORE STORING NEW DATA.
!
IFLABEL2: &
IF (TIMTAG /= CURENT) THEN
!
! FREE THE BLOCKS USED BY THIS SEGMENT
!
! NON-INDEX BLOCK:    (LENGTHS OF RECORDS AT START, DATA ITSELF AT END)
! ---------------------------------- - - - - - -----------------------
! :TIME: NUM OF : LENGTH : L1 : L2 :     FREE       : SECOND : FIRST  :
! :TAG : RECORDS:  FREE  :    :    :     SPACE      : RECORD : RECORD :
! ---------------------------------- - - - - - -----------------------
! 0    2        4        6    8   10            END-L1-L2  END-L1   END
!
  TIMTAG=CURENT
  NTRIES=0
  LASLEF=BLKSIZ-HEADER
!
  NBLOCK=1
10 CONTINUE
  N=INDEX(MAP(8+XBLOKS+NBLOCK:8+BLOCKS-2),CHAR(NINDEX))
  IF (N > 0) THEN
    NBLOCK=NBLOCK+(N-1)
    MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(0)
    IF (1+XBLOKS+NBLOCK < BLOCKS) GO TO 10
  END IF

!----------------------------------------------------------------------
!Check that we havent reached the maximum number of index entries
!If we have do not store this batch of reports.
!----------------------------------------------------------------------

ELSE IF ((NTRIES+1)*12 >  BLKSIZ-6) THEN
  WRITE(6,*)'ERSREP: INDEX BLOCK FULL FOR ',SLODAY,SLOTHR
  RETURN
END IF IFLABEL2

IFLABEL3: &
IF (TIMTAG == CURENT) THEN
!
! IF THE INDEX IS NOT EMPTY (IF DATA ALREADY STORED FOR THIS HOUR), SEE
! IF THIS REPORT IS A DUPLICATE, I.E IF THE COORDINATES OF THIS BATCH
! OF SOUNDINGS ARE ALREADY IN AN INDEX ENTRY.
!
  DO I=1,NTRIES
    J=HEADER+(I-1)*INDENT
    IF (INDEKS(J+1:J+8) == ENTRY(1:8)) THEN
      LASTFT=IFT
      RETURN
    END IF
  END DO
END IF IFLABEL3
!**********************************************************************
!                                                                     *
! EITHER INITIALISE NEW BLOCK OR READ IN BLOCK WITH ROOM TO STORE OB. *
!                                                                     *
!**********************************************************************
!
! IF IT'S NOT A DUPLICATE, STORE IT.  FIRST LOOK FOR ROOM IN THIS BLOCK.
! IF NO ROOM LEFT, START NEW BLOCK (NO NEED TO READ ANYTHING IN).
!  (THE NEW BLOCK IS THE FIRST UNUSED, I.E. WITH ZERO IN THE MAP BLOCK)
! IF THERE IS ROOM, READ OLD BLOCK (IF IT'S NOT IN CORE ALREADY: COMPARE
!                                   WITH BLOCK NUMBER SET FOR LAST READ)
!
IFLABEL4: &
IF (LASLEF < LEN(BULL)+2 .OR. NTRIES == 0) THEN
  NBLOCK=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-2),CHAR(0))
!
! IF NEW BLOCK NEEDED & NONE FREE, CAN'T STORE ANY MORE TILL BLOCKS
! FREED BY NEXT HOUR.  ISSUE MESSAGE FIRST TIME THIS HAPPENS.
!
  IF (NBLOCK == 0) THEN
    IF (.NOT.FULL) THEN
      FULL=.TRUE.
      WRITE (*,'(I3.2,A1,I2.2,A)') &
       DATIME(3),'/',DATIME(4),'Z: ERS DATA BASE FULL FOR THIS HOUR'
    END IF
    LASTFT=IFT
    RETURN
  ELSE
    FULL=.FALSE.
  END IF
!
  BLKTAG=CURENT
  NINBLK=0
  LEFT=BLKSIZ-HEADER
  BLOCK(7:BLKSIZ)=' '
  MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(NINDEX)
ELSE
  IN=HEADER+NTRIES*INDENT
  LASBLK=ICHAR(INDEKS(IN-1:IN-1))*256+ICHAR(INDEKS(IN:IN))
  IF (IFT /= LASTFT .OR. LASBLK /= NBLOCK) THEN
    NBLOCK=LASBLK
    NUMREC = 1+NSQ+XBLOKS+NBLOCK
    CALL METDB_CREAD_DIR (IFT, BLOCK, BLKSIZ, NUMREC, IOFLAG)

    BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))
    NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))
    LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))
  END IF
!
! THE INDEX & DATA BLOCKS SHOULD AGREE ABOUT HOW MANY BYTES ARE LEFT
! (LASLEF=LEFT). IF NOT, EITHER THE DATA BASE IS ALREADY INCONSISTENT
! OR STORING THIS MESSAGE WOULD MAKE IT SO - ISSUE WARNING & RETURN.
!
  IF (LASLEF /= LEFT) THEN
    PRINT *,'ERS DATA BASE INCONSISTENT',IFT,'IS FT NUMBER'
    PRINT *,NINDEX,'TH INDEX BLOCK',NBLOCK,'TH DATA BLOCK'
    PRINT *,'BYTES LEFT IN INDEX & DATA BLOCK',LASLEF,LEFT
    LASLEF=0
    LASTFT=IFT
    RETURN
  END IF
END IF IFLABEL4
!**********************************************************************
!
! PUT THE RECORD IN THE BLOCK AND ADD AN INDEX ENTRY FOR IT.
!
!**********************************************************************
NINBLK=NINBLK+1
L=LEN(BULL)
LEFT=LEFT-L-2
LASLEF=LEFT
BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)
BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))
START=HEADER+NINBLK*2+LEFT
BLOCK(START+1:START+L)=BULL
!
ENTRY(10:10)=CHAR(NINBLK+0)
ENTRY(11:11)=CHAR(NBLOCK/256)
ENTRY(12:12)=CHAR(NBLOCK-(NBLOCK/256)*256)
IN=HEADER+NTRIES*INDENT
INDEKS(IN+1:IN+INDENT)=ENTRY
NTRIES=NTRIES+1
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
!
BLOCK(1:1)=CHAR(BLKTAG/256)
BLOCK(2:2)=CHAR(MOD(BLKTAG,256))
BLOCK(3:3)=CHAR(NINBLK/256)
BLOCK(4:4)=CHAR(MOD(NINBLK,256))
BLOCK(5:5)=CHAR(LEFT/256)
BLOCK(6:6)=CHAR(MOD(LEFT,256))
NUMREC = 1+NSQ+XBLOKS+NBLOCK
CALL METDB_CWRITE_DIR (IFT, BLOCK, BLKSIZ, NUMREC, IOFLAG)

IF (NINBLK == 1) THEN
  NUMREC = 1
  CALL METDB_CWRITE_DIR (IFT, MAP, BLKSIZ, NUMREC, IOFLAG)
END IF

INDEKS(1:1)=CHAR(TIMTAG/256)
INDEKS(2:2)=CHAR(MOD(TIMTAG,256))
INDEKS(3:3)=CHAR(NTRIES/256)
INDEKS(4:4)=CHAR(MOD(NTRIES,256))
INDEKS(5:5)=CHAR(LASLEF/256)
INDEKS(6:6)=CHAR(MOD(LASLEF,256))
NUMREC = NINDEX + NSQ
CALL METDB_CWRITE_DIR (IFT, INDEKS, BLKSIZ, NUMREC, IOFLAG)
LASTFT=IFT
RETURN
END SUBROUTINE ERSREP
