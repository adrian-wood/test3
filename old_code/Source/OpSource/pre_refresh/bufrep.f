      SUBROUTINE BUFREP (IUNIT, LENREC, NTTOR, FLAGS, ITEMS, NSEQ,
     &                   BULL, KODE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : BUFREP
!
! PURPOSE     : To store a BUFR bulletin in the MetDB.
!
! DESCRIPTION : BUFREP stores a BUFR bulletin (BULL) in the appropriate
!               MetDB storage data set. In brief, the following steps
!               are performed:
!
!               - Replace BUFR sequence in bulletin if required.
!               - Read header and map records from storage data set.
!               - Make an index entry for the new bulletin.
!               - Return if data is too old to store.
!               - Read index block(s) for appropriate index period.
!               - Release these if they are for an expired period.
!               - Return if bulletin has already been stored.
!               - Read the data block which is to contain the bulletin.
!               - Initialise any new index or data blocks required.
!               - Update index and data records.
!               - Write changed map, index and data blocks to MetDB.
!
!               Features:
!
!                1. Storage data set can have >65536 records.
!                2. Index period need not be a whole number of hours.
!                3. Index entries include time window with times
!                    including minutes and seconds (allows easier
!                    checking for duplicate bulletins).
!                4. Index times are stored with month and year.
!                5. Index entry contains full location of data.
!                6. Index entry allows for >1024 observations.
!
!               Restrictions:
!
!                1. Bulletins must be already BUFR encoded.
!                2. Ob times in each bulletin must not span >1 index
!                    block (so complete BUFR decoding is unnecessary).
!                3. Index periods start on whole minute.
!                4. No 'chaining' or checking for corrected bulletins
!                    is done and no trailers are output to storage.
!                5. Record length of storage data set must be <32768.
!                6. Data block overflow is not supported - yet.
!                7. Bulletins must contain satid, time (y,m,d,h,m,s),
!                    lat. & long.  Surface type (008012) is optional.
!                8. Each bulletins contains data for only one station
!                    (e.g. 1 satellite).
!
! USAGE       : CALL BUFREP (IUNIT, LENREC, NTTOR, FLAGS, ITEMS, NSEQ,
!                            BULL, KODE)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!               IUNIT  (I)   Unit number of storage data set.
!               LENREC (I)   Record length of storage data set.
!               NTTOR  (I)   Time of receipt (8-element integer array
!                             as output by a call to DATIM).
!               FLAGS  (I)   Processing flags (from HEADERS data set).
!               ITEMS  (I)   Processing items (from HEADERS data set).
!               NSEQ   (I)   Sequence descriptor ('FXXYYY').
!               BULL  (I/O)  BUFR bulletin to be stored.
!               KODE   (O)   Return code - numbering as follows:
!
!                     0  Bulletin stored without problem.
!                  1-10  Minor problem not preventing data storage.
!                 11-20  Bulletin rejected: storage not appropriate.
!                 21-30  Bulletin rejected: storage job limitation.
!                 31-40  Bulletin rejected: storage d/s. limitation.
!                 41-50  Bulletin rejected: problem with contents.
!
!               Specific return codes are tabulated below.
!
!                  1  BUFR sequence not replaced in message.
!                 11  Data too old to store.
!                 12  Bulletin already stored.
!                 21  VALUES array in GETVALS not big enough.
!                 22  Too many index blocks to hold in store.
!                 23  Unit no. of storage data set not in range 1-99.
!                 24  Storage data set not in new (Dec 2000) format.
!                 25  Unrecognised data set version number.
!                 26  Unrecognised index entry version number.
!                 27  Too many map blocks to hold in store.
!                 31  No free blocks in storage data set.
!                 32  BUFR message too big to store in data set.     !3
!                 41  Vital descriptor not found in BUFR bulletin.
!                 42  BUFR bulletin failed to decode.
!                 43  Data time is after time of receipt.
!                 44  Bad or missing data - couldn't make index entry.
!
!               Note: for details of 'flags' and 'items', see the
!                     documentation in the data set containing bulletin
!                     header information.
!
! CALLED BY   : BUFRDAT
!
! CALLS       : BUFSEQ, CHAR2, CHAR3, CLRMAP, DATE13, DATE31, DUPCHK,
!               FREEBLK, ICHAR2, ICHAR3, INDEX1, IOBLKS, SYSABN
!
! HISTORY     : Original version based on ERSREP, SATREP, SSMREP and
!               non-operational storage routines.
!               Brian Barwell, December 2000.
!
! REVISION INFO:
!
! $Workfile: bufrep.f$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 27/02/2008 10:04:01$
!
! CHANGE RECORD:
!
! $Log:
!  3    Met_DB_Project 1.2         27/02/2008 10:04:01    Brian Barwell
!       Return with return code 32 if bulletin is too long to store.
!  2    Met_DB_Project 1.1         01/05/2007 17:06:48    Brian Barwell   Move
!       statement so that NUMDAT is always set. 
!  1    Met_DB_Project 1.0         30/01/2006 20:21:23    Sheila Needham  
! $
! Revision 2.3  2003/01/06 16:19:10  usmdb
! The NAG compiler on the HP will not allow the internal read from
! character HDRBLK to integer values. Replace the internal read
! with an integer read direct from the dataset - S.Cox
!
! Revision 2.2  2002/10/04 10:26:58  usmdb
! 2.2a. Change parameter statement to allow a max of 20 map blocks
!       in the MetDB storage dataset - S.Cox
! 2.2b. Put extra item of data in index entry. Also add facility
!       to run storage without checking for old data - B.Barwell.
!
! Revision 2.1  2001/09/05  09:15:48  09:15:48  usmdb (Generic MetDB account)
! 17 Sept 2001.  2.1.  Brian Barwell.  Change 112/01.
! Correction to BUFREP to avoid out-of-bounds errors on HP.
!
! Revision 2.0  2001/06/06  10:13:59  10:13:59  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                  Parameter statements

      INTEGER LDHEAD        ! Length of header section of data blocks
      INTEGER LXHEAD        ! Length of header section of index blocks
      INTEGER MAXMAP        ! Maximum number of map blocks supported
      INTEGER MAXNDX        ! Maximum length of chain of index blocks
      INTEGER MAXLEN        ! Maximum length of index entry supported
      INTEGER MAXREC        ! Maximum data set record length supported
      INTEGER MAXDVER       ! Maximum data set version supported
      INTEGER MAXXVER       ! Maximum index entry version supported

      PARAMETER (LDHEAD=8)
      PARAMETER (LXHEAD=10) ! (Must agree with routine DUPCHK)
      PARAMETER (MAXMAP=20, MAXNDX=4, MAXLEN=34, MAXREC=27998)     !2.2a
      PARAMETER (MAXDVER=1, MAXXVER=2)
!                                                             Variables

      INTEGER I             ! General pointer to location in record
      INTEGER IBASE         ! Block no. of initial index block in store
      INTEGER IDOBS         ! Obs. minutes after start of index period
      INTEGER IDTOR         ! T.O.R. mins. after start of index period
      INTEGER IMOBS(2)      ! Century minutes of earliest & latest obs.
      INTEGER IMNDX         ! Century minute of start of index period
      INTEGER IMTOR         ! Century minute for time of receipt
      INTEGER IREC          ! Record number in storage data set
      INTEGER ITEMS(*)      ! Processing items for current data type
      INTEGER IUMAP         ! Unit from which map blocks were read
      INTEGER IUNDX         ! Unit from which index blocks were read
      INTEGER IUNIT         ! Unit number of storage data set
      INTEGER IUSED         ! Number of bytes used so far in data block
      INTEGER J             ! General loop variable
      INTEGER KODE          ! Return code (for a list, see above)
      INTEGER LENREC        ! Record length of storage data set
      INTEGER LENCHK        ! Length of part of entry for dup. check
      INTEGER LENTRY        ! Length of index entries in storage d/s
      INTEGER LENBUL        ! Total length of BUFR bulletin
      INTEGER MAXMIN        ! Total period of storage data set in mins
      INTEGER MAXPTR        ! Maximum no. of pointers in one map block
      INTEGER MAXNTRY       ! Maximum no. of index entries in 1 record
      INTEGER NBLKS         ! Number of blocks in storage data set
      INTEGER NDVER         ! Version no. for storage data set format
      INTEGER NEWDAT        ! Data block number for new bulletin
      INTEGER NEWNDX        ! Index block number for new index entry
      INTEGER NRDAT(2)      ! Record from which each data block was read
      INTEGER NRHDR(1)      ! Record from which header block was read
      INTEGER NRMAP(MAXMAP) ! Record from which each map block was read
      INTEGER NRNDX(MAXNDX) ! Record from which each index blk was read
      INTEGER NSEQ          ! BUFR sequence from storage d/s ('FXXYYY')
      INTEGER NTOBS(12)     ! Date/time range for obs. (y,m,d,h,m,s)*2
      INTEGER NTNDX(5)      ! Date/time of index period. (y,m,d,h,m)
      INTEGER NTRIES        ! Number of index entries in index block
      INTEGER NTTOR(8)      ! Date/time of receipt (from DATIM)
      INTEGER NUDAT(2)      ! Unit from which each data block was read
      INTEGER NUHDR(1)      ! Unit from which header block was read
      INTEGER NUMAP(MAXMAP) ! Unit from which each map block was read
      INTEGER NUNDX(MAXNDX) ! Unit from which each index block was read
      INTEGER NUMDAT        ! Number of data blocks in held store
      INTEGER NUMDAY        ! Century day (for DATE31 and DATE13)
      INTEGER NUMMAP        ! Number of map blocks in storage data set
      INTEGER NUMNDX        ! Length of chain of index blocks read in
      INTEGER NXBLKS        ! No. of index blocks in storage data set
      INTEGER NXMIN         ! Number of minutes in index period
      INTEGER NXVER         ! Version number for index entry format
      INTEGER NX00Z         ! Index period offset from 00Z (minutes)

      LOGICAL FIRST         ! .TRUE. if first call to subroutine
      LOGICAL RESET         ! Flag used in call to CLRMAP
      LOGICAL SAMETAG       ! Flag used to test tag times in index blks
      LOGICAL DIFF          ! 'Different BUFR sequence' flag from BUFSEQ
      LOGICAL FLAGS(*)      ! Processing flags for current data type

      CHARACTER*(*)      BULL          ! Bulletin to be stored
      CHARACTER*2        CHAR00        ! Hex '0000'
      CHARACTER*(MAXREC) DATBLK(2)     ! Data blocks
      CHARACTER*(MAXLEN) ENTRY         ! Index entry (not >MAXLEN bytes)
      CHARACTER*(MAXREC) HDRBLK(1)     ! Header block from storage d/s
      CHARACTER*80       HEAD          ! Revision information        !2
      CHARACTER*(MAXREC) MAPBLK(MAXMAP) ! Map blocks from storage d/s
      CHARACTER*(MAXREC) NDXBLK(MAXNDX) ! Index blocks from storage d/s
      CHARACTER*5        TIMNDX        ! Index time tag  (y,m,d,h,m)

!                                                    External functions

      INTEGER    ICHAR2     ! C*2 to I*4 conversion routine
      INTEGER    ICHAR3     ! C*3 to I*4 conversion routine
      CHARACTER*2 CHAR2     ! I*4 to C*2 conversion routine
      CHARACTER*3 CHAR3     ! I*4 to C*3 conversion routine

!                                                       Saved variables
      SAVE FIRST, CHAR00
      SAVE IUMAP, IUNDX, NDVER, NXVER, MAXMIN, LENCHK
      SAVE NRHDR, NRMAP, NRNDX, NRDAT, NUHDR, NUMAP, NUNDX, NUDAT
      SAVE NBLKS, NUMMAP, NXBLKS, NXMIN, NX00Z, LENTRY, NUMNDX, NUMDAT

!                                                       Data statements
      DATA FIRST /.TRUE./, NUHDR, IUMAP, IUNDX /3*0/
      DATA NRMAP, NRNDX, NRDAT /MAXMAP*0, MAXNDX*0, 2*0/ ! Record nos.
      DATA NUMAP, NUNDX, NUDAT /MAXMAP*0, MAXNDX*0, 2*0/ ! Unit nos.

!                            Common block (for dynamic allocation only)

      COMMON /COMBLK/ HDRBLK, MAPBLK, NDXBLK, DATBLK

!-----------------------------------------------------------------------
!      REVISION INFORMATION AND INITIALISATIONS  (FIRST CALL ONLY)
!-----------------------------------------------------------------------

      IF (FIRST) THEN
!                                                  Revision information
        HEAD = '$Workfile: bufrep.f$ ' //
     &         '$Revision: 3$ $Date: 27/02/2008 10:04:01$'
!                                                       Initialisations
        CHAR00 = CHAR(0) // CHAR(0)
        FIRST = .FALSE.
      END IF

!-----------------------------------------------------------------------
!  RETURN WITH WARNING MESSAGE IF UNIT NUMBER ISN'T IN THE RANGE 1-99.
!-----------------------------------------------------------------------

      IF (IUNIT.LE.0 .OR. IUNIT.GT.99) THEN
         WRITE (6,'(T5,A,T15,2A,I6)') 'BUFREP:', 'Unit number ',
     &            'of storage data set not in range 1-99:', IUNIT
         KODE = 23
         RETURN
      END IF

!-----------------------------------------------------------------------
!               REPLACE DESCRIPTOR SEQUENCE IN MESSAGE
!-----------------------------------------------------------------------

      IF (FLAGS(4) .AND. NSEQ.GT.0) THEN
         CALL BUFSEQ (NSEQ, BULL, DIFF, LENBUL)
      ELSE
         LENBUL = LEN(BULL)
         DIFF = .FALSE.
      END IF

!-----------------------------------------------------------------------
!        CHECK FOR BULLETIN TOO LONG TO FIT IN STORAGE DATA SET      !3
!-----------------------------------------------------------------------

      IF (LENBUL.GT.LENREC-LDHEAD) THEN                              !3
        KODE = 32                                                    !3
        RETURN                                                       !3
      END IF                                                         !3

      KODE = 0             ! No problems yet
      MAXPTR = LENREC/2    ! Max. pointers per map record

!=======================================================================
!               READ HEADER BLOCK (IF NOT ALREADY DONE)
!=======================================================================
!                               Check for header record already read in
      IF (NUHDR(1).NE.IUNIT) THEN
!                                Read header record of storage data set
         CALL IOBLKS
     &        (-1, IUNIT, LENREC, 1, 1, NUHDR(1), NRHDR(1), HDRBLK(1))

!                                            Read/Decode header details
         READ (IUNIT,REC=1)
     &         J, NBLKS, NUMMAP, NXBLKS, NXMIN, NX00Z, LENTRY       !2.3

         NDVER = J/256
         NXVER = MOD(J,256)
!                                            Check formats and versions
         IF (HDRBLK(1)(1:2).NE.CHAR00) THEN
            WRITE (6,'(/T5,A,T15,A,I3)') 'BUFREP:',
     &            'Storage data set is in old format.  Unit', IUNIT
            KODE = 24
         ELSE IF (NDVER.LE.0 .OR. NDVER.GT.MAXDVER) THEN
            WRITE (6,'(/T5,A,T15,A,I6)') 'BUFREP:',
     &            'Unsupported data set version number -', NDVER
            KODE = 26
         ELSE IF (NXVER.LE.0 .OR. NXVER.GT.MAXXVER) THEN
            WRITE (6,'(/T5,A,T15,A,I6)') 'BUFREP:',
     &            'Unsupported index entry version number -', NXVER
            KODE = 27
         END IF
!                                  Return if bad or unsupported version
         IF (KODE.GT.0) RETURN
!                                  Total on-line storage period (mins.)
         MAXMIN = NXBLKS*NXMIN
      END IF

!=======================================================================
!                READ MAP BLOCKS (IF NOT ALREADY DONE)
!=======================================================================

      IF (IUMAP.NE.IUNIT) THEN
!                                       Warning if too many map records
         IF (NUMMAP.GT.MAXMAP) THEN
            WRITE (6,'(T5,A,T15,A,I3,A,I3,A)') 'BUFREP:',
     &               'Can''t hold all map blocks - Unit', IUNIT,
     &               ' has', NUMMAP, ' map blocks.'
            KODE = 25
            RETURN
!                                                   Read the map blocks
         ELSE
            CALL IOBLKS
     &           (-2, IUNIT, LENREC, NUMMAP, 3, NUMAP, NRMAP, MAPBLK)
            IUMAP = IUNIT
         END IF
      END IF

!=======================================================================
!          CREATE MOST OF INDEX ENTRY AND CHECK FOR OLD DATA
!=======================================================================
!                                       26-byte 'satellite' index entry
      IF (NXVER.EQ.1) THEN
         CALL INDEX1 (BULL, FLAGS, ITEMS, NTOBS, ENTRY, KODE)
         LENCHK = 15                                               !2.2b
!                                     33-byte non-satellite index entry
      ELSE IF (NXVER.EQ.2) THEN
         CALL INDEX2 (BULL, FLAGS, ITEMS, NTOBS, ENTRY, KODE)
         LENCHK = 22                                               !2.2b
      END IF
!                                        Check return code for problems
      IF (KODE.GT.0) RETURN
!                                       No. of entries a block can hold
      MAXNTRY = (LENREC-LXHEAD)/LENTRY

!-----------------------------------------------------------------------
!             CHECK FOR DATA TOO OLD OR TOO NEW TO STORE
!-----------------------------------------------------------------------
!                                         Compute century minutes for:-
!                                                        Receipt time
      CALL DATE31 (NTTOR(6), NTTOR(7), NTTOR(8), NUMDAY)
      IMTOR = 1440*(NUMDAY-1) + 60*NTTOR(5) + NTTOR(4)
!                                                        Observations
      CALL DATE31 (NTOBS(3), NTOBS(2), NTOBS(1), NUMDAY)
      IMOBS(1) = 1440*(NUMDAY-1) + 60*NTOBS(4) + NTOBS(5)   ! earliest
      CALL DATE31 (NTOBS(9), NTOBS(8), NTOBS(7), NUMDAY)
      IMOBS(2) = 1440*(NUMDAY-1) + 60*NTOBS(10) + NTOBS(11) ! latest

!                                                        Index period
      IDOBS = MOD(IMOBS(1)-NX00Z,NXMIN)
      IMNDX = IMOBS(1) - IDOBS
!                                                    Check for old data
      IF (FLAGS(6) .AND. IMTOR-IMNDX.GE.MAXMIN) THEN               !2.2b
         KODE = 11
         RETURN
!                                              Check for data in future
      ELSE IF (IMOBS(2).GT.IMTOR) THEN
         KODE = 43
         RETURN
      END IF

!-----------------------------------------------------------------------
!   GET TIME TAG FOR BULLETIN AND INSERT EXTRA DATA (IF ANY) IN ENTRY
!-----------------------------------------------------------------------
!                                         Compute time tag for bulletin
      NUMDAY = IMNDX/1440 + 1
      CALL DATE13 (NUMDAY, NTNDX(3), NTNDX(2), NTNDX(1))
      NTNDX(1) = MOD(NTNDX(1),100)  ! 2-DIGIT YEAR
      NTNDX(4) = MOD(IMNDX/60,24)   ! HOUR
      NTNDX(5) = MOD(IMNDX,60)      ! MINUTE

      DO J=1,5
         TIMNDX(J:J) = CHAR(NTNDX(J))
      END DO ! J
!                                  Extra data (to go after no. of obs.)
      IF (ITEMS(3).NE.0)                                           !2.2b
     &    ENTRY(LENTRY-11:LENTRY-11) = CHAR(ITEMS(3))              !2.2b

!=======================================================================
!                       READ INDEX BLOCK(S)
!=======================================================================

! The index is divided into segments of 'NXMIN' minutes. The observation
! time is used to find the appropriate segment for the current bulletin
! and the corresponding index block and any overflows are read if not
! already held in store (as indicated by the value and sign of 'IUNDX').
!
! Index block format:-
!
!  ----------------------------------------------------------- - - - -
!  : Yr, Mnth, :  Index  : No. of next : First :  Further
!  : Day, Hr,  : entries : index block : index :   index   . . . .
!  :  Minute   : in blk. :  in chain   : entry :  entries
!  +-----------+---------+-------------+-------+-------------- - - - -
!  0           5         7            10    10+LENTRY
!                          Byte number
!
!-----------------------------------------------------------------------
!                                       Compute base index block number

      IBASE = MOD((IMNDX-NX00Z)/NXMIN, NXBLKS) + NUMMAP + 3

!                      Read chain of index blocks (if not already done)

      IF (IUNDX.NE.IUNIT .OR. NRNDX(1).NE.IBASE) THEN ! not already done
         IREC = IBASE
         NUMNDX = 0
         SAMETAG = .TRUE.
!                                       Loop over index blocks in chain

         DO WHILE (IREC.GT.0 .AND. NUMNDX.LT.MAXNDX .AND. SAMETAG)
            NUMNDX = NUMNDX + 1
            CALL IOBLKS (-3, IUNIT, LENREC, 1, IREC, NUNDX(NUMNDX),
     &                   NRNDX(NUMNDX), NDXBLK(NUMNDX))
!                                                        Check tag time
            SAMETAG = NDXBLK(NUMNDX)(1:5).EQ.NDXBLK(1)(1:5)

!                                     Get number of next block in chain

            IREC = ICHAR3(NDXBLK(NUMNDX)(8:10))
         END DO

!-----------------------------------------------------------------------
!               TESTS FOR PROBLEMS WITH INDEX RECORDS
!-----------------------------------------------------------------------
!                                            Time tags not all the same
         IF (.NOT.SAMETAG) THEN
            WRITE (6,'(T5,A,T15,2A,5I3,A,I3)') 'BUFREP:',
     &            'Tag times in chain of index blocks are not all ',
     &            'identical for time', NTNDX, ' on unit', IUNIT
            CALL SYSABN (830)
!                                    Too many records to hold in NDXBLK
         ELSE IF (IREC.GT.0) THEN
            WRITE (6,'(T5,A,T15,A,I4,2A,I3,A,5I3)') 'BUFREP:',
     &               'More than', MAXNDX, ' index blocks in chain.  ',
     &               'Unit', IUNIT, ', time', NTNDX
            IUNDX = 0                                                !2
            KODE = 22
            RETURN
!                                                 All OK so set pointer
         ELSE
            IUNDX = IUNIT
         END IF
      END IF

!-----------------------------------------------------------------------
!         RELEASE INDEX & DATA BLOCKS IF OBSOLETE INDEX PERIOD
!-----------------------------------------------------------------------

      IF (NDXBLK(1)(1:5).NE.TIMNDX) THEN  ! Old index period
         IREC = NUMMAP + NXBLKS + 3
         CALL CLRMAP (IBASE, IREC, NBLKS, LENREC, NUMMAP, MAPBLK,
     &                NUMAP, RESET)
!                                         Write out modified map blocks
         IF (RESET) CALL IOBLKS
     &             (2, IUNIT, LENREC, NUMMAP, 0, NUMAP, NRMAP, MAPBLK)

!                                                       Adjust pointers
         NUMNDX = 0      ! No index records in chain
         NUMDAT = 0      ! No data records for period
         IUSED  = LENREC ! No free space in 'previous' record
         NEWNDX = IBASE  ! New index block needed

!-----------------------------------------------------------------------
!             DUPLICATE DATA CHECK - RETURN IF DUPLICATE
!-----------------------------------------------------------------------

      ELSE
         IF (FLAGS(3)) THEN
            CALL DUPCHK (ENTRY, LENTRY, LENCHK, NDXBLK, NUMNDX, KODE)
            IF (KODE.GT.0) RETURN  ! (It's a duplicate)
         END IF

!-----------------------------------------------------------------------
!            FIND A NEW INDEX BLOCK IF CURRENT ONE IS FULL
!-----------------------------------------------------------------------

         NEWNDX = 0
         NTRIES = ICHAR2(NDXBLK(NUMNDX)(6:7))
         IF (NTRIES.GE.MAXNTRY) THEN ! full
!                                         Return if can't hold any more
            IF (NUMNDX.GE.MAXNDX) THEN
               WRITE (6,'(T5,A,T15,2A,I3,A,5I3)') 'BUFREP:',
     &                  'Can''t add another index block to chain.  ',
     &                  'Unit', IUNIT, ', time', NTNDX
               KODE = 22
               RETURN
            END IF
!                                                     Find a free block
            IREC = NUMMAP + NXBLKS + 3
            CALL FREEBLK (IREC, NBLKS, MAXPTR, NUMMAP, MAPBLK, NEWNDX)

!                                              Return if none available
            IF (NEWNDX.EQ.0) THEN
               KODE = 31
               RETURN
            END IF
         END IF

!=======================================================================
!                       READ DATA BLOCK(S)
!=======================================================================
! Data block format:-
!
!   ---------------------------------------------------------- - - - -
!   : Yr, Mnth, : No. of next :   First    :  Further
!   : Day, Hr,  : data block  :  message   :  messages   . . . .
!   :  Minute   : in chain    : (L1 bytes) :
!   +-----------+-------------+------------+------------------ - - - -
!   0           5             8           8+L1
!                          Byte number

!-----------------------------------------------------------------------
!      GET DETAILS OF LATEST DATA BLOCK USED FOR THIS INDEX PERIOD.
!-----------------------------------------------------------------------

         I = LXHEAD + NTRIES*LENTRY                 ! End of last entry
         IREC  = ICHAR3(NDXBLK(NUMNDX)(I-6:I-4))    ! Block number
         IUSED = ICHAR2(NDXBLK(NUMNDX)(I-3:I-2)) +  ! Bytes used so far
     &           ICHAR2(NDXBLK(NUMNDX)(I-1:I)) - 1

!-----------------------------------------------------------------------
!           READ LATEST DATA BLOCK IF NOT ALREADY IN STORE.
!-----------------------------------------------------------------------

         IF (NRDAT(1).NE.IREC .OR. NUDAT(1).NE.IUNIT) THEN ! Not held

!                                                   Read the data block
            CALL IOBLKS (-4, IUNIT, LENREC,
     &                    1, IREC, NUDAT(1), NRDAT(1), DATBLK(1))

!                                                    Check its time tag
            IF (DATBLK(1)(1:5).NE.TIMNDX) THEN
               WRITE (6,'(T5,A,T15,A,I7,A,5I3,A,I3)') 'BUFREP:',
     &               'Tag time in data block', NRDAT(1), ' not', NTNDX,
     &               ' as expected - unit', IUNIT
               CALL SYSABN (831)
            END IF
         END IF
         NUMDAT = 1                                                  !2
      END IF

!-----------------------------------------------------------------------
!   FIND A NEW DATA BLOCK IF CURRENT ONE HAS INSUFFICIENT FREE SPACE
!       OR IF THIS IS THE FIRST BULLETIN FOR A NEW INDEX PERIOD.
!-----------------------------------------------------------------------

      NEWDAT = 0
      IF (IUSED+LENBUL.GT.LENREC) THEN ! Not enough free space

!                      Find a free block (but not the same one as found
!                                   when looking for a new index block)
         IREC = NUMMAP + NXBLKS + 3
         IF (IREC.LE.NEWNDX) IREC = NEWNDX + 1
         CALL FREEBLK (IREC, NBLKS, MAXPTR, NUMMAP, MAPBLK, NEWDAT)

!                                              Return if none available
         IF (NEWDAT.EQ.0) THEN ! None free
            KODE = 31
            RETURN
         END IF
      END IF

!=======================================================================
!            BULLETIN ACCEPTED FOR STORAGE IN THE MET.D.B.
!=======================================================================
!   So far, some checks have been made on the data (e.g. for duplicate
! bulletins) which may have led to a premature return to the calling
! program with a non-zero return code.  While this has been possible,
! nothing in MAPBLK, NDXBLK or DATBLK has been altered since they were
! read from the storage data set so that their contents will be
! identical with that in the data set.
!   Now we have reached the point where we can say that the bulletin
! has definitely been accepted for storage in the MetDB and no further
! premature returns will be allowed.  MAPBLK, NDXBLK and DATBLK will
! be updated as necessary and written back to the data set before exit
! from this subroutine.
!-----------------------------------------------------------------------
!              RESERVE AND INITIALISE NEW INDEX RECORD
!-----------------------------------------------------------------------

      IF (NEWNDX.GT.0) THEN
!                                      If not the base index record ...
         IF (NUMNDX.GT.0) THEN
!                              Put new record number in previous record

            NDXBLK(NUMNDX)(8:10) = CHAR3(NEWNDX)
            NUNDX(NUMNDX) = -IUNIT
!                                             Set pointer in map record

            IREC = (NEWNDX-1)/MAXPTR + 1      ! 'MAPBLK' subscript
            J = 2*MOD((NEWNDX-1),MAXPTR) + 1  ! Location in map record
            MAPBLK(IREC)(J:J+1) = CHAR2(IBASE)
!                                            Update map record pointers
            IUMAP = 0
            NUMAP(IREC) = -IUNIT
            NRMAP(IREC) = IREC + 2
         END IF
!                                      Set time tag in new index record
         NUMNDX = NUMNDX + 1
         NDXBLK(NUMNDX)(1:5) = TIMNDX
!                                          Set rest of record to zeroes
         DO J=6,LENREC
            NDXBLK(NUMNDX)(J:J) = CHAR(0)
         END DO ! J
!                                          Update index record pointers
         IUNDX = 0
         NUNDX(NUMNDX) = -IUNIT
         NRNDX(NUMNDX) = NEWNDX
!                                              Set NTRIES for new block
         NTRIES = 0 ! No entries yet
      END IF

!-----------------------------------------------------------------------
!              RESERVE AND INITIALISE NEW DATA RECORD
!-----------------------------------------------------------------------

      IF (NEWDAT.GT.0) THEN
!                             If not first data block for index period,
!                              put new record number in previous record
         IF (NUMDAT.GT.0) THEN                                      !2.1
            DATBLK(NUMDAT)(6:8) = CHAR3(NEWDAT)                     !2.1
            NUDAT(NUMDAT) = -IUNIT
         END IF                                                     !2.1
!                                             Set pointer in map record

         IREC = (NEWDAT-1)/MAXPTR + 1      ! 'MAPBLK' subscript
         J = 2*MOD((NEWDAT-1),MAXPTR) + 1  ! Location in map record
         MAPBLK(IREC)(J:J+1) = CHAR2(IBASE)
!                                            Update map record pointers
         IUMAP = 0
         NUMAP(IREC) = -IUNIT
         NRMAP(IREC) = IREC + 2
!                                       Set time tag in new data record
         NUMDAT = NUMDAT + 1
         DATBLK(NUMDAT)(1:5) = TIMNDX
!                                          Set rest of record to zeroes
         DO J=6,LENREC
            DATBLK(NUMDAT)(J:J) = CHAR(0)
         END DO ! J
!                                           Update data record pointers
         NUDAT(NUMDAT) = -IUNIT
         NRDAT(NUMDAT) = NEWDAT
         IUSED = LDHEAD
      END IF

!=======================================================================
!             COMPLETE THE INDEX ENTRY FOR THIS BULLETIN
!=======================================================================
!                            Receipt time relative to index time (mins)
      IDTOR = IMTOR - IMNDX
      IDTOR = MIN0(IDTOR,65535)
      ENTRY(LENTRY-8:LENTRY-7) = CHAR2(IDTOR)
!                                                  Data record pointers

      ENTRY(LENTRY-6:LENTRY-4) = CHAR3(NRDAT(NUMDAT)) ! Record number
      ENTRY(LENTRY-3:LENTRY-2) = CHAR2(IUSED+1)       ! Start byte
      ENTRY(LENTRY-1:LENTRY)   = CHAR2(LENBUL)        ! Bulletin length

!=======================================================================
!               UPDATE INDEX AND DATA BLOCKS IN STORE
!=======================================================================
!                                        Update number of index entries
      IUNDX = 0
      NUNDX(NUMNDX) = -IUNIT
      NDXBLK(NUMNDX)(6:7) = CHAR2(NTRIES+1)  ! Number of entries

!                                        Add index entry to index block
      I = LXHEAD + NTRIES*LENTRY
      NDXBLK(NUMNDX)(I+1:I+LENTRY) = ENTRY(1:LENTRY)

!                                            Add bulletin to data block
      NUDAT(NUMDAT) = -IUNIT
      DATBLK(NUMDAT)(IUSED+1:IUSED+LENBUL) = BULL(1:LENBUL)

!=======================================================================
!          WRITE MAP, INDEX AND DATA BLOCKS TO STORAGE DATA SET
!=======================================================================
!
!     Write back to data set in following order:
!
!                  1. Data block (D),
!                  2. Map of blocks in use (M),
!                  3. Index for hour (I).
!
!     The order matters because a system failure after one or two of
!   the three writes could leave the data base in an inconsistent
!   state. The argument for the above order is as follows:
!
!   - Better D without I (data stored but inaccessible),
!       than I without D (index entry present but associated data lost):
!
!   - Better D without M (data stored but may be overwritten because
!                         block is not claimed in map),
!       than M without D (block tied up but data lost):
!
!   - Better M without I (block tied up but data inaccessible
!                         because no index entry),
!       than I without M (index points to data which may be lost
!                         because block has been reclaimed).
!
!-----------------------------------------------------------------------
!                                                     Write data blocks

      CALL IOBLKS (4, IUNIT, LENREC, NUMDAT, 0, NUDAT, NRDAT, DATBLK)

!                                                      Write map blocks

      CALL IOBLKS (2, IUNIT, LENREC, NUMMAP, 0, NUMAP, NRMAP, MAPBLK)
      IUMAP = IUNIT
!                                                    Write index blocks

      CALL IOBLKS (3, IUNIT, LENREC, NUMNDX, 0, NUNDX, NRNDX, NDXBLK)
      IUNDX = IUNIT
!                                            Set return code and return
      IF (DIFF) KODE = 1
      RETURN
      END
