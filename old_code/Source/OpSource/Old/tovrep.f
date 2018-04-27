      SUBROUTINE TOVREP(A,ND,NOBS,DESCR,DATIME,ENTRY,BULL,NFT)        !B

!-----------------------------------------------------------------------
!
! PROGRAM       : TOVREP
!
! PURPOSE       : Re-encode batch of SAT120 soundings and store it
!
! CALLED BY     : TOVIND
!
! CALLS         : DATE31, NBUFRI, IDES, TOVSEQ
!
! ARGUMENTS     : (1) array of decoded values (NOBS*ND)
!               : (2) numbers of descriptors
!               : (3) number of obs in batch
!               : (4) sequence of descriptors
!               : (5) date/time of first ob in batch (year, month...)
!               : (6) index entry (incomplete)
!               : (7) BUFR message with descriptors in
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 26/11/2007 11:10:38$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tovrep.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:10:38    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from MDBSTOR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:25:26    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:19  usmdb
! Replaced INTEGER*2 and EQUIVALENCE code with INTEGER*4 and
! CHAR code - C.Long
! Moved delcaration of ND and NOBS before declaration of array
! of dimension ND and NOBS. Added copyright and modified
! header - S.Cox
!
! Revision 1.5  97/10/16  09:18:48  09:18:48  usjl (Jon Lewthwaite)
! Add code to extract BUFR subtype from BUFR message before the BUFR
! encode routines are called as ENBUFR loses it. The subtype is
! then put back into the message for storage - S.Cox                  !C
!
! Revision 1.4  1997/09/22 14:00:53  uspm
! Pass NAMES into NBUFRI rather than X (wrong type).
! Avoid F90 invalid do construct -labelled executable statement
!
! Revision 1.3  1997/09/04 09:38:16  uspm
! Extra argument to pass Unit No. from BUFRBUL to this routine to
! remove hardwired unit numbers. Also add IMPLICIT NONE               !B
!
! Revision 1.2  1997/07/31 11:44:11  uspm
! change unit number from 3 to 4 to stop contention with test
! housekeeping dataset in BURBUL                                      !A
!
! Revision 1.1  1997/07/04 14:34:23  uspm
! Initial revision
!
! Jun 1991      : TOVSEQ called before encoding with single F=3
!               : descriptor
!
! Apr 1991      : ENBUFI called (instead of BUFR1) to use BUFR shell
!
! Jan 1991      : 6-Hour slots in index
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

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare Integer variables (in alphabetical order)
!-----------------------------------------------------------------------

      INTEGER   ND                                                  !2.0
      INTEGER   NOBS                                                !2.0
      INTEGER   A(NOBS,ND)
      INTEGER   AMSTAR                                              !2.0
      INTEGER   BLKSIZ
      INTEGER   BLKTAG                                              !2.0
      INTEGER   BLOCKS                                              !2.0
      INTEGER   BufrEditn        !- BUFR edition                      !C
      INTEGER   BufrStart        !- start pos of 'BUFR' in BULL       !C
      INTEGER   BufrSubtype      !- BUFR subtype                      !C
      INTEGER   CENDAY
      INTEGER   CENTHR
      INTEGER   CURENT
      INTEGER   DATIME(5)
      INTEGER   DESCR(*)
      INTEGER   HEADER
      INTEGER   I
      INTEGER   IDES
      INTEGER   IN
      INTEGER   INDENT
      INTEGER   INDHOR
      INTEGER   ITOR
      INTEGER   J
      INTEGER   LASBLK
      INTEGER   LASLEF                                              !2.0
      INTEGER   LASTHR
      INTEGER   LEFT                                                !2.0
      INTEGER   N
      INTEGER   NBLOCK
      INTEGER   NDES
      INTEGER   NFT
      INTEGER   NINBLK                                              !2.0
      INTEGER   NINDEX
      INTEGER   NOW(8)
      INTEGER   NTRIES                                              !2.0
      INTEGER   NVALS
      INTEGER   NXBLOK
!2.0 delete R*2 RECLEN(999)
      INTEGER   REPLEN
      INTEGER   SEQDES
      INTEGER   SLOTHR
      INTEGER   SLODAY
      INTEGER   START
      INTEGER   TIMTAG                                              !2.0
      INTEGER   TOR(5)
      INTEGER   X
      INTEGER   XBLOKS                                              !2.0
      INTEGER   XHOURS                                              !2.0
      INTEGER   Y

!-----------------------------------------------------------------------
! Declare Character variables (in alphabetical order)
!-----------------------------------------------------------------------

      CHARACTER BUFR*4           !- 'BUFR'                            !C
      CHARACTER BULL*(*)
      CHARACTER ENTRY*12
      CHARACTER HEAD*132
      CHARACTER NAMES*16

!-----------------------------------------------------------------------
! Declare Logical variables (in alphabetical order)
!-----------------------------------------------------------------------

      LOGICAL   CMPRES
      LOGICAL   DIFF
      LOGICAL   FIRSTCALL        !- TRUE for first call to TOVREP     !C
      LOGICAL   SEQOK

!-----------------------------------------------------------------------
! Ensure variables are still set on return to subroutine
!-----------------------------------------------------------------------

      SAVE

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

      DATA CMPRES    /.TRUE./
      DATA SEQOK     /.TRUE./
      DATA FIRSTCALL /.TRUE./    !- TRUE for first call to TOVREP     !C
      DATA BUFR      /'BUFR'/                                         !C

!2.0  7 lines deleted (comments)
      PARAMETER (HEADER=6,INDENT=12,BLKSIZ=23476)

      CHARACTER*(BLKSIZ) MAP,INDEKS,BLOCK
!2.0  11 lines deleted (equivalences)

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tovrep.F,v $
     &'//'$ $Date: 26/11/2007 11:10:38$ $Revision: 2$'

!-----------------------------------------------------------------------
! On first call to TOVREP only, convert 'BUFR' from EBCDIC to ASCII   !C
!-----------------------------------------------------------------------

      IF (FIRSTCALL) THEN                                             !C
        FIRSTCALL = .FALSE.                                           !C
        CALL EB2ASC(4,BUFR)                                           !C
      ENDIF                                                           !C

!-----------------------------------------------------------------------
! First re-encode batch of soundings (need to know message length)
! & (if first time) read in map block (to get number of index blocks)
!
! Map block:             (the byte for each block is set to its index
!                        block number - so less than 256*xhours data!)
! ------------------------------------------------------------ - - - -
! : no. of : no. of : hours : start of : first :  2nd  :     : 1st   :
! : blocks : index  : per   : 1st slot : index : index :     : data  :
! : in d/s : blocks : block : after 0z : block : block :     : block :
! ------------------------------------------------------------ - - - -
! 0        2        4       6          8      10      12    8+xbloks
!
! Read in the map block (first time only),
! set time of latest data stored to clock time at start of run; it will
! be reset to the latest century-hour stored whenever that is greater.
!-----------------------------------------------------------------------

      CALL DATIM(NOW)
      IF (BLOCKS.EQ.0) THEN
        READ (NFT,REC=1) MAP                                        !A!B
        BLOCKS=ICHAR(MAP(1:1))*256+ICHAR(MAP(2:2))                  !2.0
        XBLOKS=ICHAR(MAP(3:3))*256+ICHAR(MAP(4:4))                  !2.0
        XHOURS=ICHAR(MAP(5:5))*256+ICHAR(MAP(6:6))                  !2.0
        AMSTAR=ICHAR(MAP(7:7))*256+ICHAR(MAP(8:8))                  !2.0

        CALL DATE31(NOW(6),NOW(7),NOW(8),CENDAY)
        LASTHR=(CENDAY-1)*24+NOW(5)
      ENDIF

!-----------------------------------------------------------------------
! Complete time fields in index entry:
! Second byte : put hour in 5 bits after satellite identifier
! Ninth byte  : TOR in minutes from start of slot divided by 3
! and find first hour in slot (slothr) & make time tag.
!-----------------------------------------------------------------------

      N=XHOURS                               ! fullword for MOD
      INDHOR=MOD(DATIME(4)+24-AMSTAR,N)      ! hour minus slot start
      ENTRY(2:2)=CHAR(ICHAR(ENTRY(2:2))+INDHOR)

      CALL DATE31 (DATIME(3),DATIME(2),DATIME(1),CENDAY)
      CENTHR=(CENDAY-1)*24+DATIME(4)         ! century-hour
      SLOTHR=CENTHR-INDHOR                   ! hour at start of slot

      CALL DATE13 (SLOTHR/24+1,SLODAY,X,Y)   ! day of month for slot
      SLOTHR=MOD(SLOTHR,24)                  ! cent-hour to hour of day
      CURENT=SLODAY*256+SLOTHR               ! time tag (day/hour)

      ITOR=MOD(NOW(5)+24-SLOTHR,24)          ! TOR hour relative to slot
      ITOR=(ITOR*60 + NOW(4))/3              ! convert to minutes & /3
      IF (ITOR.GT.255) ITOR=255              ! missing if too late
      ENTRY(9:9)=CHAR(ITOR)

!-----------------------------------------------------------------------
! If this data is 60 hours or more older than the latest data stored,
! reject it (to avoid overwriting current data with data 60 hours old!).
! if it's more recent, update the latest hour.
!-----------------------------------------------------------------------

!!!   IF (CENTHR.LE.LASTHR-HOURS) RETURN    ***   Comment out for test
      IF (CENTHR.GT.LASTHR) LASTHR=CENTHR

      NVALS=ND*NOBS
      NDES=0

      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

!-----------------------------------------------------------------------
! Try to reencode with one f=3 descriptor.  this corrects wrong element
! descriptors for vertical coordinates and incidentally saves space. if
! the descriptors in the bulletin are inconsistent with the sequence,
! issue a message (once only) and reencode using those in the message.
!-----------------------------------------------------------------------

      SEQDES=IDES(310198)
      CALL TOVSEQ(SEQDES,BULL,DIFF)
      IF (.NOT.DIFF) THEN
        NDES=1
        DESCR(1)=SEQDES
      ELSE
        IF (SEQOK) THEN
          SEQOK=.FALSE.
          PRINT *,'SEQUENCE DOES NOT MATCH TRANSMITTED DESCRIPTORS'
          PRINT *,BULL(1:80)
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! Get the BUFR subtype from the message                               !C
!-----------------------------------------------------------------------

      BufrStart = INDEX(BULL(1:),BUFR)                                !C
      BufrEditn = ICHAR(BULL(BufrStart+7:BufrStart+7))                !C

      IF (BufrEditn.LT.2) THEN                                        !C
        BufrSubtype = ICHAR(BULL(BufrStart+13:BufrStart+13))          !C
      ELSE                                                            !C
        BufrSubtype = ICHAR(BULL(BufrStart+17:BufrStart+17))          !C
      ENDIF                                                           !C

      CALL NBUFRI(DESCR,A,NDES,NVALS,NOBS,NAMES,TOR,BULL,CMPRES,REPLEN)

!-----------------------------------------------------------------------
! Put the BUFR subtype back into the message. Always store as BUFR
! edition 1                                                           !C
!-----------------------------------------------------------------------

      BULL(14:14) = CHAR(BufrSubtype)                                 !C

!-----------------------------------------------------------------------
! The index is divided into hourly segments. work out which segment
! from the century-hour and read in the corresponding index block.
! (hourly segments should avoid index overflow, which is not allowed
! for: use shorter segments if overflow looks likely.)
!
! Index block:               (see TOVIND for structure of entries)
! ----------------------------------------------------------- - - - -
! : date/ : no. of : length free : first 12-   : second   :
! : time  : entries: in last blk : byte entry  : entry    :
! ----------------------------------------------------------- - - - -
! 0       2        4             6             18         30
!
! Read index block                          (if not in core already)
!-----------------------------------------------------------------------

      N=XBLOKS
      NXBLOK=(MOD((CENTHR-INDHOR)/XHOURS,N))+2
      IF (NINDEX .NE. NXBLOK) THEN
        NINDEX=NXBLOK
        READ (NFT,REC=NINDEX) INDEKS                                !A!B
        TIMTAG=ICHAR(INDEKS(1:1))*256+ICHAR(INDEKS(2:2))            !2.0
        NTRIES=ICHAR(INDEKS(3:3))*256+ICHAR(INDEKS(4:4))            !2.0
        LASLEF=ICHAR(INDEKS(5:5))*256+ICHAR(INDEKS(6:6))            !2.0
      ENDIF

!-----------------------------------------------------------------------
! If the index has any entries, either delete them (if they're for an
! earlier hour) or check for duplicates (if current hour)
!-----------------------------------------------------------------------
! The time tag (date/time in index block) is (day of month)*256+hour.
! The time tag is the hour this segment was last used for.  if it was
! last used for the oldest data in the bank (60 hours ago if 60 hours'
! data kept), clear the blocks used before storing new data.
!-----------------------------------------------------------------------

      IF (TIMTAG.NE.CURENT) THEN

!-----------------------------------------------------------------------
! Free the blocks used by this segment
!
! Non-index block:    (lengths of records at start, data itself at end)
! ---------------------------------- - - - - - -----------------------
! :time: num of : length : l1 : l2 :     free       : second : first  :
! :tag : records:  free  :    :    :     space      : record : record :
! ---------------------------------- - - - - - -----------------------
! 0    2        4        6    8   10            end-l1-l2  end-l1   end
!-----------------------------------------------------------------------

        TIMTAG=CURENT
        NTRIES=0
        LASLEF=BLKSIZ-HEADER

        NBLOCK=1
   10   N=INDEX(MAP(8+XBLOKS+NBLOCK:),CHAR(NINDEX))
        IF (N.GT.0) THEN
          NBLOCK=NBLOCK+(N-1)
          MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(0)
          IF (1+XBLOKS+NBLOCK.LT.BLOCKS) GO TO 10
        ENDIF
      ELSE IF (TIMTAG.EQ.CURENT) THEN

!-----------------------------------------------------------------------
! If the index is not empty (if data already stored for this hour), see
! if this report is a duplicate, i.e if the coordinates of this batch
! of soundings are already in an index entry.
!-----------------------------------------------------------------------

        DO I=1,NTRIES
          J=HEADER+(I-1)*INDENT
          IF (INDEKS(J+1:J+8).EQ.ENTRY(1:8)) RETURN
        ENDDO
      ENDIF

!-----------------------------------------------------------------------
! Either initialise new block or read in block with room to store ob.
!-----------------------------------------------------------------------
! If it's not a duplicate, store it.  first look for room in this block.
! If no room left, start new block (no need to read anything in).
! (the new block is the first unused, i.e. with zero in the map block)
! if there is room, read old block (if it's not in core already: compare
!                                   with block number set for last read)
!-----------------------------------------------------------------------

      IF (LASLEF.LT.REPLEN+2 .OR. NTRIES.EQ.0) THEN
        NBLOCK=INDEX(MAP(8+XBLOKS+1:8+BLOCKS),CHAR(0))
        BLKTAG=CURENT
        NINBLK=0
        LEFT=BLKSIZ-HEADER
        BLOCK(7:BLKSIZ)=' '
        MAP(8+XBLOKS+NBLOCK:8+XBLOKS+NBLOCK)=CHAR(NINDEX)
      ELSE
        IN=HEADER+NTRIES*INDENT
        LASBLK=ICHAR(INDEKS(IN-1:IN-1))*256+ICHAR(INDEKS(IN:IN))
        IF (LASBLK.NE.NBLOCK) THEN
          NBLOCK=LASBLK
          READ (NFT,REC=1+XBLOKS+NBLOCK) BLOCK                      !A!B
          BLKTAG=ICHAR(BLOCK(1:1))*256+ICHAR(BLOCK(2:2))            !2.0
          NINBLK=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))            !2.0
          LEFT=ICHAR(BLOCK(5:5))*256+ICHAR(BLOCK(6:6))              !2.0
        ENDIF
      ENDIF

!-----------------------------------------------------------------------
! Put the record in the block and add an index entry for it.
!-----------------------------------------------------------------------

      NINBLK=NINBLK+1
      LEFT=LEFT-REPLEN-2
      LASLEF=LEFT
      BLOCK(6+NINBLK*2-1:6+NINBLK*2-1)=CHAR(REPLEN/256)             !2.0
      BLOCK(6+NINBLK*2:6+NINBLK*2)=CHAR(MOD(REPLEN,256))            !2.0
      START=HEADER+NINBLK*2+LEFT
      BLOCK(START+1:START+REPLEN)=BULL

      ENTRY(10:10)=CHAR(NINBLK+0)
      ENTRY(11:11)=CHAR(NBLOCK/256)
      ENTRY(12:12)=CHAR(NBLOCK-(NBLOCK/256)*256)
      IN=HEADER+NTRIES*INDENT
      INDEKS(IN+1:IN+INDENT)=ENTRY
      NTRIES=NTRIES+1

!-----------------------------------------------------------------------
! Write back to data set in following order:
!
! (the order matters because  a system failure after 1 or 2 of the
! 3 writes could leave the data base in an inconsistent state.)
!
! data block (d), map of blocks in use (m), index for hour (i)
! (arguing as follows: better d but not i, data stored but inaccessible,
! than i but not d, index entry for lost data; better d but not m, data
! stored but may be overwritten because block not claimed in map, than
! m but not d, block tied up but data lost; better m but not i, block
! tied up but data inaccessible because no index entry, than i but not
! m, index points to data which may be lost because block reclaimed.
!-----------------------------------------------------------------------

      BLOCK(1:1)=CHAR(BLKTAG/256)                                   !2.0
      BLOCK(2:2)=CHAR(MOD(BLKTAG,256))                              !2.0
      BLOCK(3:3)=CHAR(NINBLK/256)                                   !2.0
      BLOCK(4:4)=CHAR(MOD(NINBLK,256))                              !2.0
      BLOCK(5:5)=CHAR(LEFT/256)                                     !2.0
      BLOCK(6:6)=CHAR(MOD(LEFT,256))                                !2.0
      WRITE (NFT,REC=1+XBLOKS+NBLOCK) BLOCK                         !A!B

      IF (NINBLK.EQ.1) WRITE (NFT,REC=1) MAP                        !A!B

      INDEKS(1:1)=CHAR(TIMTAG/256)                                  !2.0
      INDEKS(2:2)=CHAR(MOD(TIMTAG,256))                             !2.0
      INDEKS(3:3)=CHAR(NTRIES/256)                                  !2.0
      INDEKS(4:4)=CHAR(MOD(NTRIES,256))                             !2.0
      INDEKS(5:5)=CHAR(LASLEF/256)                                  !2.0
      INDEKS(6:6)=CHAR(MOD(LASLEF,2))                               !2.0
      WRITE (NFT,REC=NINDEX) INDEKS                                 !A!B

      RETURN
      END
