      SUBROUTINE ERSREP(NOBS,DATIME,ENTRY,BULL,IFT)                 !2.0

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : ERSREP
!                                                                     
! PURPOSE       : STORE BATCH OF ERS-1 SOUNDINGS                      
!                                                                     
! CALLED BY     : ERSIND                                              
!                                                                     
! CALLS         : CENTURY                                             
!               : DATE31                                              
!                                                                     
! PARAMETERS    : (1) NUMBER OF OBS IN BATCH                          
!                 (2) DATE/TIME OF FIRST OB IN BATCH (YEAR, MONTH...) 
!                 (3) INDEX ENTRY (INCOMPLETE)                        
!                 (4) BUFR MESSAGE WITH DESCRIPTORS IN                
!                 (5) FT NUMBER                                       
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:25$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ersrep.F,v $
!                                                                     
! CHANGE RECORD :                                                     
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:25    Sheila Needham  
! $
! Revision 2.0  2001/07/03 11:08:35  usmdb
! Replaced INTEGER*2 and EQUIVALENCE code with INTEGER*4 and ICHAR
! code - C.Long.
! Removed unused variable CMPRS. Separated variable declaration
! and initialisation. Removed unused dummy arguments A, DESCR, ND.
! Added copyright and modified header - S.Cox.
! Corrected extraction of receipt time from message - B Barwell.
!
! Revision 1.3  97/12/22  08:14:44  08:14:44  usjl (Jon Lewthwaite)
! Check that we havent reached the max number of index entries. If
! we have do not store the reports. This will prevent the dataset
! from being corrupted by an out-of bounds condition.                 !B
!
! Revision 1.2  1997/07/31 09:26:51  uspm
! Amend for YEAR 2000 - Use common routine to determine
! century - Jim Arnott.                                               !A
!
! Revision 1.1  1997/07/04 12:40:23  uspm
! Initial revision
!
! JUL 96:  REMOVE DEC 93 CHANGE - READ SEQUENCES IN BUFRBUL          
!
! DEC 93:  PUT SEQUENCES IN SECOND BLOCK INTO LOCAL TABLE D.         
!
! JUN 92:  DON'T RESET T.O.R. TO CURRENT TIME.                       
!          DO OCT 91 CHECK ONLY FOR ERS DATA, NOT TOVS OR LASS...    
!
! OCT 91:  CORRUPTION PROBLEM STILL NOT SOLVED - DO CHECK BASED ON   
!          ASSUMPTION THAT EACH ERS PRODUCT ALWAYS HAS SAME NOBS.    
!
! 26/09/91 CORRECT SEARCH FOR EMPTY DATA BLOCK. REDUCE SEARCH BY 1. 
!
! SEP 91:  MAY PROBLEM STILL OCCURS - SET LAST FT NUMBER ON RETURN  
!          WHEN MESSAGE NOT STORED & ADD FURTHER CONSISTENCY CHECK  
!
! MAY 91:  CHECK THAT SAME DATA SET (FT NUMBER) BEFORE SKIPPING READ 
!
! DEC 90:  6-HOUR SLOTS IN INDEX                                     
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

      INTEGER   CENTURY                                               !A

      INTEGER   DATIME(5),TOROUR,TORMIN,NOW(8)
      CHARACTER ENTRY*12,BULL*(*), X*1
      CHARACTER*132 HEAD
      INTEGER   CENDAY,CENTHR, SLOTHR,SLODAY, CURENT
      INTEGER   HEADER,BLKSIZ, START

      INTEGER   TIMTAG,NTRIES,LASLEF                                !2.0
      INTEGER   BLKTAG,NINBLK,LEFT                                  !2.0
      INTEGER   BLOCKS,XBLOKS,XHOURS,AMSTAR                         !2.0
      LOGICAL   FULL                                                !2.0

      DATA      FULL/.FALSE./                                       !2.0
      DATA      LASTFT/0/

      PARAMETER (HEADER=6,INDENT=12,BLKSIZ=23476)

      CHARACTER*(BLKSIZ) MAP,INDEKS,BLOCK

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ersrep.F,v $
     &'//'$ $Date: 30/01/2006 20:22:25$ $Revision: 1$'

* CHECK UNIT NUMBER TO SEE IF SSM/I DATA
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
      CALL DATIM(NOW)
*
* COMPARE COUNT IN 4TH BYTE OF FIRST INDEX ENTRY WITH NOBS FOR THIS
* MESSAGE: IF THEY'RE DIFFERENT, ASSUME THE INDEX IS FOR THE WRONG
* DATA BASE AND READ THE MAP & INDEX BLOCKS EVEN IF SAME FT NUMBER.
* (N.B. THIS DOESN'T MAKE IT IMPOSSIBLE TO STORE PRODUCTS WITH NOBS
* VARYING FROM MESSAGE TO MESSAGE; IT JUST MEANS MORE READS.)
*
      INDNOBS=ICHAR(INDEKS(10:10))
      IF (INDNOBS.NE.MOD(NOBS,256) .AND. IFT.EQ.LASTFT
     -  .AND. IFT.GE.5.AND.IFT.LT.25) THEN
        PRINT *,'ERS MESSAGES SHOULD ALL HAVE SAME NUMBER OF OBS'
        WRITE (*,'(I9,''/'',I4.4,''Z   FT'',I2.2)')
     -   NOW(8)*10000+NOW(7)*100+NOW(6), NOW(5)*100+NOW(4), IFT
        PRINT *,NOBS,'OBS IN MESSAGE TO BE STORED'
        PRINT *,INDNOBS,'OBS IN FIRST INDEX ENTRY FOR THAT DATE/TIME'
        PRINT *,INDEKS(1:80)
        LASTFT=0
      ENDIF
*
* READ THE FILE PARAMETERS, MAP & LOCAL DESCRIPTOR SEQUENCES (IF ANY)
*
      IF (IFT.NE.LASTFT) THEN
        READ (IFT,REC=1) MAP

        BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))                  !2.0
        XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))                  !2.0
        XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))                  !2.0
        AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))                  !2.0

        NSEQBL=ICHAR(MAP(BLOCKS+8:BLOCKS+8))
        IF (NSEQBL.GT.0) THEN
          NSQ=1
        ELSE
          NSQ=0
        ENDIF
*
        CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
        LASTHR=(CENDAY-1)*24+NOW(5)
      ENDIF
*
* SET TYPE OF MESSAGE (EQUAL TO THE FT NUMBER) IN CASE WE'RE MERGING
* & GET HOURS & MINUTES OF TIME OF RECEIPT FROM MESSAGE FOR INDEX.
* (DISPLACEMENTS AS IN BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)
*
      BULL(14:14)=CHAR(IFT)                  ! BUFR SUBTYPE
      TOROUR=ICHAR(BULL(24:24))              ! HOUR OF T.O.R.       !2.0
      TORMIN=ICHAR(BULL(25:25))              ! MINS OF T.O.R.       !2.0
*
* COMPLETE TIME FIELDS IN INDEX ENTRY:
*  SECOND BYTE: PUT HOUR IN 5 BITS AFTER SATELLITE IDENTIFIER
*  NINTH BYTE: TOR IN MINUTES FROM START OF SLOT DIVIDED BY 3
* AND FIND FIRST HOUR IN SLOT (SLOTHR) & MAKE TIME TAG.
*
      N=XHOURS                               ! FULLWORD FOR MOD
      INDHOR=MOD(DATIME(4)+24-AMSTAR,N)      ! HOUR MINUS SLOT START
      ENTRY(2:2)=CHAR(ICHAR(ENTRY(2:2))+INDHOR)
*
      IF (DATIME(1).LT.1900) DATIME(1)=DATIME(1)+CENTURY(DATIME(1))   !A
*
      CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
      CENTHR=(CENDAY-1)*24+DATIME(4)         ! CENTURY-HOUR
      SLOTHR=CENTHR-INDHOR                   ! HOUR AT START OF SLOT
*
      CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! DAY OF MONTH FOR SLOT
      SLOTHR=MOD(SLOTHR,24)                  ! CENT-HOUR TO HOUR OF DAY
      CURENT=SLODAY*256+SLOTHR               ! TIME TAG
*
      ITOR=MOD(TOROUR+24-SLOTHR,24)          ! TOR HOUR RELATIVE TO SLOT
      ITOR=(ITOR*60 + TORMIN)/3              ! CONVERT TO MINUTES & /3
      IF (ITOR.GT.255) ITOR=255              ! MISSING IF TOO LATE
      ENTRY(9:9)=CHAR(ITOR)
*
* IF THIS DATA IS 60 HOURS OR MORE OLDER THAN THE LATEST DATA STORED,
* REJECT IT (TO AVOID OVERWRITING CURRENT DATA WITH DATA 60 HOURS OLD!).
* IF IT'S MORE RECENT, UPDATE THE LATEST HOUR.
*
      IF (CENTHR.LE.LASTHR-XBLOKS*XHOURS) THEN
        LASTFT=IFT
        RETURN
      ENDIF
      IF (CENTHR.GT.LASTHR) LASTHR=CENTHR
*
* THE INDEX IS DIVIDED INTO HOURLY SEGMENTS. WORK OUT WHICH SEGMENT
* FROM THE CENTURY-HOUR AND READ IN THE CORRESPONDING INDEX BLOCK.
* (HOURLY SEGMENTS SHOULD AVOID INDEX OVERFLOW, WHICH IS NOT ALLOWED
*  FOR: USE SHORTER SEGMENTS IF OVERFLOW LOOKS LIKELY.)
*
* INDEX BLOCK:               (SEE BUFIND FOR STRUCTURE OF ENTRIES)
* ----------------------------------------------------------- - - - -
* : DATE/ : NO. OF : LENGTH FREE : FIRST 12-   : SECOND   :
* : TIME  : ENTRIES: IN LAST BLK : BYTE ENTRY  : ENTRY    :
* ----------------------------------------------------------- - - - -
* 0       2        4             6            18         30
*
* READ IN INDEX BLOCK FOR SLOT               (IF NOT IN CORE ALREADY)
*
      N=XBLOKS                               ! FULLWORD FOR MOD
      NXBLOK=MOD((CENTHR-INDHOR)/XHOURS,N)+2 ! NUMBER OF INDEX BLOCK
      IF (IFT.NE.LASTFT .OR. NINDEX.NE.NXBLOK) THEN
        NINDEX=NXBLOK
        READ (IFT,REC=NSQ+NINDEX) INDEKS

        TIMTAG=ICHAR(INDEKS(1:1))*256+ICHAR(INDEKS(2:2))            !2.0
        NTRIES=ICHAR(INDEKS(3:3))*256+ICHAR(INDEKS(4:4))            !2.0
        LASLEF=ICHAR(INDEKS(5:5))*256+ICHAR(INDEKS(6:6))            !2.0
      ENDIF
***********************************************************************
*                                                                     *
* IF THE INDEX HAS ANY ENTRIES, EITHER DELETE THEM (IF THEY'RE FOR AN *
* EARLIER HOUR) OR CHECK FOR DUPLICATES (IF CURRENT HOUR)             *
*                                                                     *
***********************************************************************
*
* THE TIME TAG (DATE/TIME IN INDEX BLOCK) IS (DAY OF MONTH)*256+HOUR.
* THE TIME TAG IS THE SLOT THIS SEGMENT WAS LAST USED FOR.  IF IT WAS
* LAST USED FOR THE OLDEST DATA IN THE BANK (60 HOURS AGO IF 60 HOURS'
* DATA KEPT), CLEAR THE BLOCKS USED BEFORE STORING NEW DATA.
*
      IF (TIMTAG.NE.CURENT) THEN
*
* FREE THE BLOCKS USED BY THIS SEGMENT
*
* NON-INDEX BLOCK:    (LENGTHS OF RECORDS AT START, DATA ITSELF AT END)
* ---------------------------------- - - - - - -----------------------
* :TIME: NUM OF : LENGTH : L1 : L2 :     FREE       : SECOND : FIRST  :
* :TAG : RECORDS:  FREE  :    :    :     SPACE      : RECORD : RECORD :
* ---------------------------------- - - - - - -----------------------
* 0    2        4        6    8   10            END-L1-L2  END-L1   END
*
        TIMTAG=CURENT
        NTRIES=0
        LASLEF=BLKSIZ-HEADER
*
        NBLOCK=1
   10   N=INDEX(MAP(8+XBLOKS+NBLOCK:8+BLOCKS-2),CHAR(NINDEX))
        IF (N.GT.0) THEN
          NBLOCK=NBLOCK+(N-1)
          MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(0)
          IF (1+XBLOKS+NBLOCK.LT.BLOCKS) GO TO 10
        ENDIF

!----------------------------------------------------------------------
!Check that we havent reached the maximum number of index entries
!If we have do not store this batch of reports.
!----------------------------------------------------------------------

      ELSEIF ((NTRIES+1)*12 .GT. BLKSIZ-6) THEN                      !B
        WRITE(6,*)'ERSREP: INDEX BLOCK FULL FOR ',SLODAY,SLOTHR      !B
        RETURN                                                       !B
      ENDIF                                                          !B

      IF (TIMTAG.EQ.CURENT) THEN
*
* IF THE INDEX IS NOT EMPTY (IF DATA ALREADY STORED FOR THIS HOUR), SEE
* IF THIS REPORT IS A DUPLICATE, I.E IF THE COORDINATES OF THIS BATCH
* OF SOUNDINGS ARE ALREADY IN AN INDEX ENTRY.
*
        DO 20 I=1,NTRIES
        J=HEADER+(I-1)*INDENT
        IF (INDEKS(J+1:J+8).EQ.ENTRY(1:8)) THEN
          LASTFT=IFT
          RETURN
        ENDIF
   20   CONTINUE
      ENDIF
***********************************************************************
*                                                                     *
* EITHER INITIALISE NEW BLOCK OR READ IN BLOCK WITH ROOM TO STORE OB. *
*                                                                     *
***********************************************************************
*
* IF IT'S NOT A DUPLICATE, STORE IT.  FIRST LOOK FOR ROOM IN THIS BLOCK.
* IF NO ROOM LEFT, START NEW BLOCK (NO NEED TO READ ANYTHING IN).
*  (THE NEW BLOCK IS THE FIRST UNUSED, I.E. WITH ZERO IN THE MAP BLOCK)
* IF THERE IS ROOM, READ OLD BLOCK (IF IT'S NOT IN CORE ALREADY: COMPARE
*                                   WITH BLOCK NUMBER SET FOR LAST READ)
*
      IF (LASLEF.LT.LEN(BULL)+2 .OR. NTRIES.EQ.0) THEN
        NBLOCK=INDEX(MAP(8+XBLOKS+1:8+BLOCKS-2),CHAR(0))
*
* IF NEW BLOCK NEEDED & NONE FREE, CAN'T STORE ANY MORE TILL BLOCKS
* FREED BY NEXT HOUR.  ISSUE MESSAGE FIRST TIME THIS HAPPENS.
*
        IF (NBLOCK.EQ.0) THEN
          IF (.NOT.FULL) THEN
            FULL=.TRUE.
            WRITE (*,1) DATIME(3),DATIME(4)
    1       FORMAT (I3.2,'/',I2.2,'Z: ERS DATA BASE FULL FOR THIS HOUR')
          ENDIF
          LASTFT=IFT
          RETURN
        ELSE
          FULL=.FALSE.
        ENDIF
*
        BLKTAG=CURENT
        NINBLK=0
        LEFT=BLKSIZ-HEADER
        BLOCK(7:BLKSIZ)=' '
        MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(NINDEX)
      ELSE
        IN=HEADER+NTRIES*INDENT
        LASBLK=ICHAR(INDEKS(IN-1:IN-1))*256+ICHAR(INDEKS(IN:IN))
        IF (IFT.NE.LASTFT .OR. LASBLK.NE.NBLOCK) THEN
          NBLOCK=LASBLK
          READ (IFT,REC=1+NSQ+XBLOKS+NBLOCK) BLOCK

          BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))            !2.0
          NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))            !2.0
          LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))              !2.0
        ENDIF
*
* THE INDEX & DATA BLOCKS SHOULD AGREE ABOUT HOW MANY BYTES ARE LEFT
* (LASLEF=LEFT). IF NOT, EITHER THE DATA BASE IS ALREADY INCONSISTENT
* OR STORING THIS MESSAGE WOULD MAKE IT SO - ISSUE WARNING & RETURN.
*
        IF (LASLEF.NE.LEFT) THEN
          PRINT *,'ERS DATA BASE INCONSISTENT',IFT,'IS FT NUMBER'
          PRINT *,NINDEX,'TH INDEX BLOCK',NBLOCK,'TH DATA BLOCK'
          PRINT *,'BYTES LEFT IN INDEX & DATA BLOCK',LASLEF,LEFT
          LASLEF=0
          LASTFT=IFT
          RETURN
        ENDIF
      ENDIF
***********************************************************************
*
* PUT THE RECORD IN THE BLOCK AND ADD AN INDEX ENTRY FOR IT.
*
***********************************************************************
      NINBLK=NINBLK+1
      L=LEN(BULL)
      LEFT=LEFT-L-2
      LASLEF=LEFT
      BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(L/256)                  !2.0
      BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(L,256))                 !2.0
      START=HEADER+NINBLK*2+LEFT
      BLOCK(START+1:START+L)=BULL
*
      ENTRY(10:10)=CHAR(NINBLK+0)
      ENTRY(11:11)=CHAR(NBLOCK/256)
      ENTRY(12:12)=CHAR(NBLOCK-(NBLOCK/256)*256)
      IN=HEADER+NTRIES*INDENT
      INDEKS(IN+1:IN+INDENT)=ENTRY
      NTRIES=NTRIES+1
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
      BLOCK(1:1)=CHAR(BLKTAG/256)                                   !2.0
      BLOCK(2:2)=CHAR(MOD(BLKTAG,256))                              !2.0
      BLOCK(3:3)=CHAR(NINBLK/256)                                   !2.0
      BLOCK(4:4)=CHAR(MOD(NINBLK,256))                              !2.0
      BLOCK(5:5)=CHAR(LEFT/256)                                     !2.0
      BLOCK(6:6)=CHAR(MOD(LEFT,256))                                !2.0
      WRITE (IFT,REC=1+NSQ+XBLOKS+NBLOCK) BLOCK

      IF (NINBLK.EQ.1) WRITE (IFT,REC=1) MAP

      INDEKS(1:1)=CHAR(TIMTAG/256)                                  !2.0
      INDEKS(2:2)=CHAR(MOD(TIMTAG,256))                             !2.0
      INDEKS(3:3)=CHAR(NTRIES/256)                                  !2.0
      INDEKS(4:4)=CHAR(MOD(NTRIES,256))                             !2.0
      INDEKS(5:5)=CHAR(LASLEF/256)                                  !2.0
      INDEKS(6:6)=CHAR(MOD(LASLEF,256))                             !2.0
      WRITE (IFT,REC=NSQ+NINDEX) INDEKS
      LASTFT=IFT
      RETURN
      END
