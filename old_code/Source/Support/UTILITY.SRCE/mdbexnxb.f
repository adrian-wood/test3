      PROGRAM MDBEXTRA

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBEXTRA (to copy a storage data set with extra
!                           index blocks)
!
! PURPOSE       : Copy data for a given period from an MDB data set
!                 to a new data set (same blocksize, initialised).
!                 First list the blocks to be copied, then copy them.
!
!                 (Like MDBARK but aiming for more space rather than
!                  less, so changed to direct access writes to avoid
!                  releasing unused blocks.  Period input (to expand
!                  data set) should be new online period, so no data
!                  for older index periods)
!
!!! WARNING:    : This program has been tested on BUOYs, because a
!!!               faster alternative to MDBXTEND was essential, and
!!!               on BOGUS, which showed that for TAFREP-style data
!!!               sets the number of overflow blocks in the map had
!!!               to be kept too.  So the BOGUS test didn't go as
!!!               far as storage of further data.
!!!               The process of making an archive data set is well
!!!               tested, but for further data to be stored the map
!!!               block must have all fields set too.    Given that
!!!               warning, this program is worth further tests when
!!!               more index blocks are needed: it is both faster &
!!!               more general than MDBXTEND, the fall-back option.
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
! $Revision: 1$
! $Date: 28/02/2006 12:07:34$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbexnxb.f,v $
!
! CHANGE RECORD :
!
! Made from MDBARK Aug 2003
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
      INTEGER   ICENTHR    ! first century-hour
      INTEGER   LCENTHR    ! last century-hour
      INTEGER   NCENTHR    ! current century-hour
      INTEGER   GETFROM(33333)   ! see comment above
      INTEGER   MAPBYTE(33333)   ! to go in map
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
      INTEGER   NB         ! total number of blocks in output
      INTEGER   NBLIND     ! max number of entries in one index block
      INTEGER   NEWBLK     ! block number in output
      INTEGER   NEWIX      ! output index block number
      INTEGER   NEWNB      ! number of blocks in use
      INTEGER   NEWNX      ! number of index blocks in output
      INTEGER   NIBL       ! number of index entries already read in
      INTEGER   NIND       ! number of index blocks (with overflows)
      INTEGER   NINDEX     ! block number in loop
      INTEGER   NINBLK     ! number of obs in data block
      INTEGER   NOFLOS     ! number of blocks in overflow pool
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

      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/utility/RCS/mdbexnxb.f,v $
     &'//'$ $Date: 28/02/2006 12:07:34$ $Revision: 1$'

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
      OPEN (2,FILE='NEW',ACCESS='DIRECT',FORM='UNFORMATTED',RECL=BLKSIZ)

      READ (2,REC=1) MAP(1:BLKSIZ)
      NB=ICHAR2(MAP(1:2))      ! total number of blocks in output
      NOFLOS=ICHAR2(MAP(BLKSIZ-3:BLKSIZ-2))  ! size of overflow pool

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
      LCENTHR=(CENDAY-1)*24+LHOUR

      CALL DATE31(IDAY,IMONTH,IYEAR,CENDAY)
      INDHOR=MOD(IHOUR+24-AMSTAR,XHOURS)
      ICENTHR=(CENDAY-1)*24+IHOUR

! Set number of output index blocks & initialise number of blocks to be
! copied.

      NEWNX=(LCENTHR-ICENTHR)/XHOURS+1
      NEWNB=1+NSQ+NEWNX        ! output blocks before first data block

!-----------------------------------------------------------------------
! Loop round index blocks to be copied, listing the blocks to be
! copied to the output file, in order, with a mapping array to
! allow easy conversion of pointers.
!-----------------------------------------------------------------------

      DO NCENTHR=ICENTHR,LCENTHR,XHOURS
        OLDIX=MOD((NCENTHR-INDHOR)/XHOURS,OLDNX)+2 ! first index block
        CALL HRS2DT(IYEAR,IMONTH,IDAY,IHOUR,NCENTHR)

! Read index block(s). If there is a continuation index block, it has
! the same format as the first; its entries follow on in the array,
! NTRIES being the total number of entries including any continuation.

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

        NEWIX=MOD(NCENTHR/XHOURS,NEWNX)+2

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
            MAPBYTE(NEWNB)=NEWIX
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
                MAPBYTE(NEWNB)=NEWIX
                TOGOTO(OLDBLK)=NEWNB
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO

!-----------------------------------------------------------------------
! The blocks to be copied have now been listed.
! Write map block, assuming it's at the start.
!-----------------------------------------------------------------------

      WRITE (2,REC=1) CHAR2(NB),CHAR2(NEWNX),MAP(5:8),
     &    (CHAR(0),I=9,8+NEWNX),                          ! index blocks
     &    (CHAR(MAPBYTE(I)),I=1+NSQ+NEWNX+1,1+NSQ+NB-1),  ! data blocks
     &    CHAR(2*NSQ), (CHAR(0),I=8+NB+1,BLKSIZ-4),       ! flag, zeros
     &    CHAR2(NOFLOS),CHAR2(INDENT)        ! 2*2 at end

! Copy the sequence record if there is one, translating if necessary.

      IF (NSQ.GT.0) THEN
        READ (1,REC=2) BLOCK(1:BLKSIZ)
        IF (ASCII) CALL EB2ASC(BLKSIZ,BLOCK)
        WRITE (2,REC=2) BLOCK(1:BLKSIZ)
      ENDIF

! The remaining blocks are all either index or data blocks.
! A zero block number means no data for the index period: write zero...

      DO NEWBLK=1+NSQ+1,NEWNB
        IN=GETFROM(NEWBLK)

        IF (IN.EQ.0) THEN
          WRITE (2,REC=NEWBLK) IN

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

          WRITE (2,REC=NEWBLK) INDHDR(1:6),
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
                    IB=TOGOTO(OLDBLK)-1-NSQ-NEWNX
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

          WRITE (2,REC=NEWBLK) BLOCK(1:BLKSIZ)
        ENDIF
      ENDDO                   ! end of loop round blocks in data set
      PRINT *,NEWNB,'blocks written out'
      STOP
      END
