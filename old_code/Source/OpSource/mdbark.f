      PROGRAM MDBARK

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBARK
!
! PURPOSE       : Copy data for a given period from an MDB data set
!                 to a new data set with only the index blocks needed.
!                 First decide which blocks to copy, then copy them
!                 with sequential writes so that any unused tracks
!                 allocated for the output data set are released.
!
! CALLS         : ZPDATE,HRS2DT,
!                 EB2ASC (compile MET.SRCELIB(IBMISO8)),
!                 BLKCHK (only called if trailers & BUFR)
!
! PARAMETERS    : PARM field gives start date/tiem, end date/time,
!                 blocksize, length of index entry & options:
!                   'TRAILER' if obs are chained (and therefore
!                             pointers in trailers need conversion),
!                   'ASCII' for translation to ASCII
!                   'BUFR' if BUFR messages are stored (and so a
!                             check for corruption can be done)
!                  ('TAFMET' is now equivalent to TRAILER)
!                 INPUT: //OLD...     OUTPUT: //NEW...
!
! REVISION INFO :
!
! $Workfile: mdbark.f$ $Folder: OpSource$
! $Revision: 2$ $Date: 16/12/2008 11:28:44$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         16/12/2008 11:28:44    Brian Barwell   Set
!       BUFR sequence flag explicitly for TAFS/METARS.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:13    Sheila Needham  
! $
! Revision 2.1  2002/09/04  13:58:15  13:58:15  usmdb (Generic MetDB account)
! 16 Sept 2002     C Long
! 2.1  Instead of using a temporary data set (too much space)
!      list blocks to be copied & then copy them in a 2nd pass.
!
! Revision 2.0  2001/07/03 10:43:36  usmdb
! Separated variable declaration and initialisation. Removed
! unused local variables. Replaced statement functions with calls
! to functions ICHAR2 and CHAR2. Added copyright and modified
! header - S.Cox
!
! Revision 1.8  99/07/15  13:39:53  13:39:53  usmdb (Generic MetDB account)
! 15-07-1999 - S.Cox
! Emergency change - to cure 0C4 abend.
! Increase number of overflows from 10 to 12. This is to
! bring it in line with AIRSTO (Changed 12/7/99) for LNDSYN
! merged/swept obs.
!
! Revision 1.7  99/06/10  14:47:55  14:47:55  usmdb (Generic MDB account)
! 21-06-99 - S.Cox
! Increase size of internal array to prevent occassional out
! of bounds problems with archiving ATOVSG merge data.
!
! Revision 1.6  99/01/14  13:49:47  13:49:47  usmdb (Generic MDB account)
! Allow for map overflow if >12000 blocks - C Long
!
! Revision 1.5  98/02/03  10:05:48  10:05:48  usmdb (Generic MDB account)
! Allow more space for index entries.
!
! Revision 1.4  1997/07/31 09:29:33  uspm
! First revision for  1
!
! Revision 1.3  1997/07/25 13:55:54  uspm
! Latest version from  1  - correction to block number setting
!
! Revision 1.2  1997/07/04 12:53:30  uspm
! Latest version from  1  - Y2K check
!
! 03/02/98: allow more space for index entries (SYNOPs need 10000
!           entries rather than 5000-6000 when stored by AIRSTO
!           for new merge/sweep (separate entry for each hour
!           rather than one entry for a 6-hour chain)                 !h
!
! 16-07-97: Correct the way the block numbers are set                 !G
!
! 05-06-97: Set the block number correctly for Tafs and Metars which  !F
!           have already been processed
!
! 21/05/97: ONLY CALL BLKCHK IF DATATYPE IS STORED IN BUFR WITH
!           TRAILER                                                   !E
!
!  APR 97 - check data block consistency before copying               !d
!
!  APR 97 - Change read from unfromatted to formatted                 !c
!
!  APR 97 - cope with data like bathys with text but no trailers      !b
!
!  FEB 97 - REMOVE INTEGER*2
!
!  JAN 97 - ALLOW FOR SEQUENCE BLOCK.  IMPROVE STRUCTURE.
!           NO WRITE TO TAPE AT END.  PUT 'TRAILER' IN PARM IF
!           OBS ARE CHAINED, I.E. HAVE TRAILERS (ANY CHAIN WHICH
!           CROSSES BLOCKS WILL BE TRUNCATED.)
!           PUT 'ASCII' IN PARM FOR TRANSLATION OF CHARACTER FIELDS.
!
! MADE FROM MDBARC JAN 97
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! The arrays GETFROM & TOGOTO are the input block to read for a given
! output block (set in the first loop, used in the second) and the
! output block number corresponding to a given input data block (used
! to convert pointers in second loop & also to see if block is already
! listed for copying in first loop).  The dimension of the first
! array needs to be the maximum number of blocks expected in the
! output data set, that of the second the maximum input block number.
! For index blocks the second array is used for the overflow number.

      INTEGER   AMSTAR
      INTEGER   BLKSIZ
      INTEGER   CENDAY
      INTEGER   CENTHR
      INTEGER   GETFROM(33333)   ! see comment above
      INTEGER   I          ! short-term loop variable
      INTEGER   IB         ! shorter-term block number variable
      INTEGER   IBUFR      ! displacement of BUFR in string
      INTEGER   ICHAR2     ! function to get number from 2 bytes
      INTEGER   IN         ! short-term block number variable
      INTEGER   INDENT     ! length of index entry
      INTEGER   INDHOR
      INTEGER   FIRSTIX    ! first index block in period requested
      INTEGER   IYEAR,IMONTH,IDAY,IHOUR  ! start of period requested
      INTEGER   LYEAR,LMONTH,LDAY,LHOUR  ! end of period requested
      INTEGER   LAST       ! subscript of last byte on ob
      INTEGER   LEN        ! length of ob
      INTEGER   LASTIX     ! last index block in period requested
      INTEGER   MAXNID     ! max number of 23-byte entries in block
      INTEGER   MAXOFL     ! max number of overflow index blocks
      INTEGER   NBLIND     ! max number of entries in one index block
      INTEGER   NEWBLK     ! block number in output
      INTEGER   NEWIX      ! output index block number
      INTEGER   NEWNB      ! total number of blocks in output
      INTEGER   NEWNX      ! number of index blocks in output
      INTEGER   NIBL       ! number of index entries already read in
      INTEGER   NIND       ! number of index blocks (with overflows)
      INTEGER   NINDEX     ! block number in loop
      INTEGER   NINBLK     ! number of obs in data block
      INTEGER   NOFLOW     ! overflow block number (ignoring NSQ)
      INTEGER   NSQ        ! 1 if sequence record, 0 otherwise
      INTEGER   NTRIES     ! number of index entries for period
      INTEGER   OLDBLK     ! block number from input
      INTEGER   OLDIX      ! input index block number
      INTEGER   OLDNB      ! total number of blocks in input
      INTEGER   OLDNX      ! number of index blocks in input
      INTEGER   TIMTAG     ! index time tag (to check that not old data)
      INTEGER   TOGOTO(99999)    ! see comment above
      INTEGER   XHOURS     ! hours per index period

      PARAMETER (MAXOFL=12,MAXNID=1216)   ! 1216=27998/23
      CHARACTER*23 INDEKS(MAXOFL*MAXNID)
      CHARACTER*27998 MAP  ! map block (map itself not output)
      CHARACTER*27998 BLOCK! data block being copied
      CHARACTER EMPTYX*23  ! blank index block
      CHARACTER BUFR*4     ! 'BUFR' in ASCII
      CHARACTER INDHDR*8   ! header & overflow number from index block
      CHARACTER*2 CHAR2    ! finction to put integer into 2 bytes
      CHARACTER PARMIN*30  ! input PARM field
      CHARACTER IDATIM*14  ! start time as in PARM
      CHARACTER LDATIM*14  ! end time as in PARM

      LOGICAL   ASCII      ! set if EBCDIC-to-ASCII translation wanted
      LOGICAL   TRAILER    ! set if data has trailers
      LOGICAL   BUFFED     ! set if data includes BUFR messages

      COMMON /MDBUFS/ MAP,BLOCK,INDEKS

      DATA BUFR/'BUFR'/
      DATA ASCII/.FALSE./
      DATA TRAILER/.FALSE./
      DATA BUFFED/.FALSE./
      DATA EMPTYX/' '/

      CHARACTER*80 HEAD                                              !2
      HEAD = '$Workfile: mdbark.f$ ' //
     &       '$Revision: 2$ $Date: 16/12/2008 11:28:44$'

      CALL EB2ASC(4,BUFR)

! Zero the array which maps the input data set onto the blocks copied,
! so that zeros will be left for blocks not selected for copying.

      DO I=1,99999
        TOGOTO(I)=0
      ENDDO

!-----------------------------------------------------------------------
! Set flags & time range from PARM field.  Get BLKSIZ for OPEN.
!-----------------------------------------------------------------------

! Read start & end date/times, blocksize, options...  (Blocksize needed
! to open data sets, index entry length not always in map block.)

      READ (5,'(A14,1X, A14,1X, I5,1X, I2,1X, A30)') IDATIM,LDATIM,
     & BLKSIZ,INDENT,PARMIN
      PRINT *,'Earliest date/time:',IDATIM
      PRINT *,'Latest date/time:  ',LDATIM

! Is TAFMET only used for historical reasons? Could it be just TRAILER?

      IF (INDEX(PARMIN,'ASCII').GT.0) ASCII=.TRUE.
      IF (INDEX(PARMIN,'BUFR').GT.0) BUFFED=.TRUE.
      IF (INDEX(PARMIN,'TRAILER').GT.0) TRAILER=.TRUE.
      IF (INDEX(PARMIN,'TAFMET').GT.0) TRAILER=.TRUE.
      NBLIND=(BLKSIZ-8)/INDENT ! max number of entries in index block

!-----------------------------------------------------------------------
! Get parameters of input data set.
!-----------------------------------------------------------------------

! Read map to get data set parameters.  (Not all map blocks have length
! of index entry at end, so it was read in from PARM field above.)

      OPEN (1,FILE='OLD',ACCESS='DIRECT',RECL=BLKSIZ)
      OPEN (2,FILE='NEW',FORM='UNFORMATTED')
      READ (1,REC=1) MAP(1:BLKSIZ)

      OLDNB=ICHAR2(MAP(1:2))   ! total number of blocks in input
      OLDNX=ICHAR2(MAP(3:4))   ! number of index blocks in input
      XHOURS=ICHAR2(MAP(5:6))
      AMSTAR=ICHAR2(MAP(7:8))

! See if there is a sequence record.

      IF ((OLDNB.LE.12000 .AND. MAP(8+OLDNB:8+OLDNB).EQ.CHAR(2)) .OR.
     &    (OLDNB.GT.12000 .AND. MAP(9:9).EQ.CHAR(2))) THEN
        NSQ=1
      ELSE
        NSQ=0
      ENDIF

! But always set NSQ=0 for TAFS/METARS, even if >12000 records        !2

      IF (INDEX(PARMIN,'TAFMET').GT.0) NSQ = 0                        !2

!-----------------------------------------------------------------------
! Get index block numbers from time range
! (Can't do this until input data set parameters are known!)
!-----------------------------------------------------------------------

! Read year etc from start & end date/time character strings

      READ (IDATIM(1:4),'(I4)') IYEAR
      READ (IDATIM(5:6),'(I2)') IMONTH
      READ (IDATIM(7:8),'(I2)') IDAY
      READ (IDATIM(10:11),'(I2)') IHOUR

      READ (LDATIM(1:4),'(I4)') LYEAR
      READ (LDATIM(5:6),'(I2)') LMONTH
      READ (LDATIM(7:8),'(I2)') LDAY
      READ (LDATIM(10:11),'(I2)') LHOUR

! And find input index block numbers from corresponding century-hours

      CALL DATE31(LDAY,LMONTH,LYEAR,CENDAY)
      INDHOR=MOD(LHOUR+24-AMSTAR,XHOURS)
      CENTHR=(CENDAY-1)*24+LHOUR
      LASTIX=MOD((CENTHR-INDHOR)/XHOURS,OLDNX)+2   ! last index block

      CALL DATE31(IDAY,IMONTH,IYEAR,CENDAY)
      INDHOR=MOD(IHOUR+24-AMSTAR,XHOURS)
      CENTHR=(CENDAY-1)*24+IHOUR
      FIRSTIX=MOD((CENTHR-INDHOR)/XHOURS,OLDNX)+2  ! first index block

! Set number of output index blocks & initialise total number of blocks

      NEWNX=LASTIX-FIRSTIX+1   ! number of index blocks to be copied
      IF (NEWNX.LE.0) NEWNX=NEWNX+OLDNX     ! (adjust for wraparound)
      NEWNB=1+NSQ+NEWNX        ! output blocks before first data block

!-----------------------------------------------------------------------
! Loop round index blocks to be copied, listing the blocks to be
! copied to the output file, in order, with a mapping array to
! allow easy conversion of pointers.
! (N.B. Can't say DO from FIRSTIX to LASTIX in case LASTIX<FIRSTIX!)
!-----------------------------------------------------------------------

      DO NINDEX=FIRSTIX,FIRSTIX+NEWNX-1

! Read index block(s). If there is a continuation index block, it has
! the same format as the first; its entries follow on in the array,
! NTRIES being the total number of entries including any continuation.

        OLDIX=NINDEX           ! copy loop variable to adjust it below
        IF (OLDIX.GT.1+OLDNX) OLDIX=OLDIX-OLDNX   ! for wraparound
        IN=OLDIX               ! block number to be read
        NIND=1                 ! continuation number
  110   NIBL=(NIND-1)*NBLIND   ! entries already read in

        READ (1,REC=IN+NSQ) INDHDR(1:6),
     &      (INDEKS(I)(1:INDENT),I=NIBL+1,NIBL+NBLIND),INDHDR(7:8)
        TIMTAG=ICHAR2(INDHDR(1:2))
        IF (NIND.EQ.1) NTRIES=ICHAR2(INDHDR(3:4))

! Find output block number for this index block
! (This is the first index block for the period, one of the NEWNX
! blocks left after the map & sequence records)

        NEWIX=MOD(CENTHR/XHOURS,NEWNX)+2

! Check that entries are for right hours. If so, set output block
! number; if not, set zero, and a zero block will be written.

        IF (TIMTAG.NE.IDAY*256+IHOUR) THEN
          I=TIMTAG/256
          PRINT *,'Index day & hour wrong:',I,TIMTAG-I*256,
     &            '     not',IDAY,IHOUR
          GETFROM(NEWIX+NSQ)=0
        ELSE
          GETFROM(NEWIX+NSQ)=-(OLDIX+NSQ)

! If date/time ok, carry on with any overflows.
! List these to be copied (before the data blocks),
! using negative numbers to indicate index blocks, keeping input/ouput
! correspondence in one array & overflow pointer for copy in the other

! >>> N.B. The block numbers in index entries & trailers are data block
! >>> numbers, ignoring what comes before; & the index overflow pointer
! >>> ignores the sequence record.  The arrays GETFROM & TOGOTO
! >>> deal with block numbers from the start of the data set, except
! >>> for overflow pointers to be set in the copy - hence the many
! >>> adjustments by +NSQ or +1+NSQ+(number of index blocks)

          NOFLOW=ICHAR2(INDHDR(7:8))
          IF (NOFLOW.GT.0 .AND. NTRIES.GT.NIND*NBLIND) THEN
            NIND=NIND+1
            NEWNB=NEWNB+1
            GETFROM(NEWNB)=-(NOFLOW+NSQ)
            TOGOTO(IN+NSQ)=-(NEWNB-NSQ)      ! overflow pointer to set
            IN=NOFLOW          ! reset block to be read from pointer
            GO TO 110          ! read another block of this index
          ENDIF

! Loop round entries in index block, listing data blocks to be copied

          DO I=1,NTRIES
            OLDBLK=ICHAR2(INDEKS(I)(INDENT-1:INDENT))
            IF (OLDBLK.GT.0) THEN              ! if not suppressed
              OLDBLK=1+NSQ+OLDNX+OLDBLK        ! from start of data set
              IF (TOGOTO(OLDBLK).EQ.0) THEN    ! & not yet listed,
                NEWNB=NEWNB+1                  ! one more block to copy
                GETFROM(NEWNB)=OLDBLK          ! source of NEWNBth bloc
                TOGOTO(OLDBLK)=NEWNB
              ENDIF
            ENDIF
          ENDDO
        ENDIF

! Reset expected date to check next time tag.

        CENTHR=CENTHR+XHOURS
        CALL HRS2DT(IYEAR,IMONTH,IDAY,IHOUR,CENTHR)
      ENDDO

!-----------------------------------------------------------------------
! The blocks to be copied have now been listed.  Complete the map by
! setting NEWNB & write it out from core, then copy the remaining
! blocks, resetting pointers & translating where necessary.
!-----------------------------------------------------------------------

! Write out the map block (after setting byte for sequence block
! - the byte set reflecting the number of blocks used by the data,
! not the original size of the output data set.)
! Map here means the first block: if the map would be at the end
! (because >12000 blocks) there's no need to write it out,
! retrieval won't use it.  (The map bytes aren't set here anyway.)
! Set sequence record indicator (0 or 2) from 2*NSQ.

      IF (NEWNB.LE.12000) THEN
        WRITE (2) CHAR2(NEWNB),CHAR2(NEWNX),MAP(5:8),
     &    (CHAR(0),I=9,8+NEWNB-1),
     &    CHAR(2*NSQ), (CHAR(0),I=8+NEWNB+1,BLKSIZ-2),CHAR2(INDENT)
      ELSE
        WRITE (2) CHAR2(NEWNB),CHAR2(NEWNX),MAP(5:8),
     &    CHAR(2*NSQ), (CHAR(0),I=10,BLKSIZ-2),CHAR2(INDENT)
      ENDIF

! Copy the sequence record if there is one, translating if necessary.

      IF (NSQ.GT.0) THEN
        READ (1,REC=2) BLOCK(1:BLKSIZ)
        IF (ASCII) CALL EB2ASC(BLKSIZ,BLOCK)
        WRITE (2) BLOCK(1:BLKSIZ)
      ENDIF

! The remaining blocks are all either index or data blocks.
! A zero block number means no data for the index period: write zero...

      DO NEWBLK=1+NSQ+1,NEWNB
        IN=GETFROM(NEWBLK)

        IF (IN.EQ.0) THEN
          WRITE (2) IN

!----------------------------- Index block -----------------------------
! If it's an index block, reset pointers & translate character fields
! (No need to read overflow number; output number is already listed.)

        ELSE IF (IN.LT.0) THEN
          READ (1,REC=-IN) INDHDR(1:6),
     &      (INDEKS(I)(1:INDENT),I=1,NBLIND)
          NTRIES=ICHAR2(INDHDR(3:4))

! Change block number in index entry (by getting input block number &
! getting corresponding one from array) & translate ident if required.
! (Look at all possible entries in this block, unless only one block.)

          DO I=1,MIN(NTRIES,NBLIND)
            OLDBLK=ICHAR2(INDEKS(I)(INDENT-1:INDENT))
            IF (OLDBLK.GT.0) THEN
              OLDBLK=1+NSQ+OLDNX+OLDBLK
              IB=TOGOTO(OLDBLK)-1-NSQ-NEWNX
              INDEKS(I)(INDENT-1:INDENT)=CHAR2(IB)
            ENDIF
            IF (ASCII) CALL EB2ASC(9,INDEKS(I)(3:11))
          ENDDO

! Write out the index block with overflow block number reset.

          WRITE (2) INDHDR(1:6),
     &      (INDEKS(I)(1:INDENT),I=1,NBLIND),CHAR2(-TOGOTO(-IN))

!------------------------------ Data block -----------------------------
! If it's a data block, reset any trailer pointers & translate if nec.
! If obs are chained, go through the trailers resetting the block
! numbers; if a chain crosses blocks, zero the pointer.
!    Translate the character fields in the trailer (TTAAii & CCCC)
! and the report text itself to ASCII if required.
!    Remember that a zero length is possible if a block has been split!
!    First check data block consistency and unset index pointers
! if lengths don't add up (AIREPs occasionally overwritten...!)
! (Only do this if BUFR messages are stored & so the final 7777 can be
! recognised; otherwise it's not clear how such a check could be done)

        ELSE IF (IN.GT.0) THEN
          READ (1,REC=IN) BLOCK(1:BLKSIZ)
          IF (TRAILER .OR. ASCII) THEN
            IF (BUFFED) CALL BLKCHK(BLOCK,NEWBLK,INDEKS,NTRIES)

            NINBLK=ICHAR2(BLOCK(3:4))
            LAST=BLKSIZ                    ! point to end of first ob
            DO I=1,NINBLK                  ! loop round obs in block
              LEN=ICHAR2(BLOCK(6+I*2-1:6+I*2))
              IF (LEN.GT.0) THEN

! If there's a trailer, replace a nonzero block number in the pointer
! by that kept in the first loop.  (If a chain crosses blocks, this
! will be zero as the index has no reference to the overflow block.)

                IF (TRAILER) THEN
                  OLDBLK=ICHAR2(BLOCK(LAST-1:LAST))
                  IF (OLDBLK.GT.0) THEN
                    OLDBLK=1+NSQ+OLDNX+OLDBLK
                    IF (TOGOTO(OLDBLK).GT.0) THEN                    !2
                      IB=TOGOTO(OLDBLK)-1-NSQ-NEWNX
                    ELSE                                             !2
                      IB = 0                                         !2
                    END IF                                           !2
                    BLOCK(LAST-1:LAST)=CHAR2(IB)
                  ENDIF
                ENDIF

! Translate two character fields (TTAAii & CCCC) if there's a trailer.

                IF (TRAILER .AND. ASCII) THEN
                  CALL EB2ASC(4,BLOCK(LAST-20:LAST-17)) ! TTAA or AAii
                  CALL EB2ASC(4,BLOCK(LAST-15:LAST-12)) ! CCCC
                ENDIF

! If there is a BUFR message in this ob, translate anything before it.
! If no BUFR message, translate everything except trailer?

                IF (ASCII) THEN
                  IBUFR=INDEX(BLOCK(LAST-LEN+1:LAST),BUFR)
                  IF (IBUFR.GT.0) THEN
                   CALL EB2ASC(IBUFR-1,BLOCK(LAST-LEN+1:LAST-LEN+IBUFR))
                  ELSE
                    IF (TRAILER) THEN
                      CALL EB2ASC(LEN-23,BLOCK(LAST-LEN+1:LAST-23))
                    ELSE
                      CALL EB2ASC(LEN,BLOCK(LAST-LEN+1:LAST))
                    ENDIF
                  ENDIF
                ENDIF
                LAST=LAST-LEN ! point to end of next ob
              ENDIF
            ENDDO             ! end of loop round obs in data block
          ENDIF

! Write out the data block

          WRITE (2) BLOCK(1:BLKSIZ)
        ENDIF
      ENDDO                   ! end of loop round blocks in data set
      PRINT *,NEWNB,'blocks in output data set'
      STOP
      END
