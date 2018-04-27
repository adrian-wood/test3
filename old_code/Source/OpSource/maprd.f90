SUBROUTINE MAPRD(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LFLAG,     &
                 LOCDFG,NELMIX,IBLOCK,ITRIES,CNTRY,       &
                 IDATHR,IDREC,RECLEN,CMSG,CTYPE)

!-----------------------------------------------------------------------
!
! PROGRAM       : MAPRD
!
!               : ANSI standard except for '!' used for comments,
!                 IMPLICIT NONE statement, variable name lengths
!                 greater than 6 characters, END DO statements, and
!                 a common block which has a mixture of character and
!                 non-character variables, although neither the HP
!                 or Cray FORTRAN77 compilers complain.
!
! PURPOSE       : TO READ MAP BLOCK, RETURNING PARAMETERS AT START,
!                 AND CALLING LOCALD WITH ANY LOCAL SEQUENCE.
!                 FOR OFFLINE DATA MAKE AN INDEX DATA SET (ON FT85).
!
! DESCRIPTION   : MAPRD IS CALLED FIRST, FOLLOWED BY A NUMBER OF CALLS
!                 To read the index and data blocks
!
! DATA TYPE(S)  : ANY
!
! CALLED BY     : TFMRET, SYNRET, BUSRET, SSMRET, SYNSEQ
!
! CALLS         : LOCALD
!
! PARAMETERS    : (1)IDSK(5)  only 3 of the 5 elements are used:
!                      (2) blocksize, (3) ft number, (5) 1 if direct
!                 (2)NXBLK    NO OF INDEX BLOCKS IN D/S      (OUTPUT)
!                 (3)NXHRS    NO OF HOURS PER INDEX BLOCK    (OUTPUT)
!                 (4)NAMHR    START HOUR OF FIRST INDEX BLK  (OUTPUT)
!                 (5)INDLEN   LENGTH OF INDEX ENTRIES
!                 (6)LFLAG    TRUE FOR DIAGNOSTICS
!                 (7)LOCDFG   LOCAL TABLE D FLAG             (OUTPUT)
!                 (8)NELMIX   block number for element index (output)
!                 (9)iblock   physical block number to read  (input)
!                (10)itries   number of entries read         (output)
!                (11)cntry    char*(*) array of entries read (output)
!                (12)idathr   time tag of index read         (output)
!                (13)idrec    logical record number          (input)
!                (14)reclen   total length of message        (output)
!                (15)cmsg     char*(*) actual message        (output)
!                (16)ctype    type of block to read          (input)
!
! REVISION INFO :
!
!
! $Workfile: maprd.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 05/04/2011 14:11:43$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         05/04/2011 14:11:43    Alison Weir     Read
!       storage datasets with C routine.
!  9    MetDB_Refresh 1.8         19/01/2011 17:39:42    Sheila Needham
!       Further corrections to INTENTS
!  8    MetDB_Refresh 1.7         21/12/2010 15:52:27    Sheila Needham  ChanGE
!        INTENT to INOUT on NXBLK and IBLOCK
!  7    MetDB_Refresh 1.6         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  6    MetDB_Refresh 1.5         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  5    MetDB_Refresh 1.4         04/11/2010 15:41:00    Rosemary Lavery
!       Corrections after review 
!  4    MetDB_Refresh 1.3         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  3    MetDB_Refresh 1.2         18/10/2010 16:22:49    Rosemary Lavery
!       Further amendments before review
!  2    MetDB_Refresh 1.1         13/10/2010 13:33:29    Rosemary Lavery F90
!       amendments (before review)
!  1    MetDB_Refresh 1.0         12/10/2010 17:34:46    Rosemary Lavery
!       Initial F90 conversion
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE LOCALD_MOD

IMPLICIT NONE

INTEGER,           INTENT(IN)    :: IDSK(5)    ! DATA SET DETAILS
INTEGER,           INTENT(INOUT) :: NXBLK      ! NUMBER OF FIXED INDEX BLOCKS
INTEGER,           INTENT(INOUT) :: NXHRS      ! NO OF HOURS PER INDEX
INTEGER,           INTENT(INOUT) :: NAMHR      ! START OF FIRST INDEX AFTER 00Z
INTEGER,           INTENT(IN)    :: INDLEN     ! LENGTH OF INDEX ENTRY
LOGICAL,           INTENT(IN)    :: LFLAG      ! DIAGNOSTICS FLAG
LOGICAL,           INTENT(INOUT) :: LOCDFG     ! LOCAL TABLE D FLAG
INTEGER,           INTENT(INOUT) :: NELMIX     ! block number of element index
INTEGER,           INTENT(INOUT) :: IBLOCK     ! physical block number to read
INTEGER,           INTENT(INOUT) :: ITRIES     ! number of entries read
CHARACTER (LEN=*), INTENT(OUT)   :: CNTRY(:)   ! ARRAY OF INDEX ENTRIES
INTEGER,           INTENT(INOUT) :: IDATHR     ! time tag of index read
INTEGER,           INTENT(INOUT) :: IDREC      ! logical record number
INTEGER,           INTENT(OUT)   :: RECLEN     ! total length of message
CHARACTER (LEN=*), INTENT(OUT)   :: CMSG       ! THE MESSAGE ITSELF
CHARACTER (LEN=5), INTENT(IN)    :: CTYPE      ! type of block to read

! Local Parameters

INTEGER, PARAMETER        :: MAXINDX = 7000
INTEGER, PARAMETER        :: BLKSIZ = 27998   ! SET TO LARGEST MDB BLOCK SIZE
INTEGER, PARAMETER        :: INDHED = 6       ! LENGTH OF HEADER IN INDX BLOCK

! Local Variables

INTEGER                   :: NTRIES           ! NO. OF INDEX ENTRIES
INTEGER                   :: NBLKS            ! TOTAL NO OF RECORDS IN DATASET
INTEGER                   :: TOTREC
INTEGER                   :: IRECLN(MAXINDX)
CHARACTER (LEN=BLKSIZ)    :: BLOCK
CHARACTER (LEN=BLKSIZ)    :: CMAP             ! MAP BLOCK
CHARACTER (LEN=BLKSIZ)    :: CTABD            ! LOCAL TABLE D ENTRY
INTEGER                   :: ITABD            ! LOCAL TABLE D INDICATOR
INTEGER                   :: FLW1             ! START OF OVERFLOW NUM IN INDEX BLOCK
INTEGER                   :: FLW2             ! FLW1+1
INTEGER                   :: IRLEN            ! RECORD LENGTH
INTEGER                   :: IFT              ! INPUT FILE NUMBER, THEN FILE NUMBER
                                              ! FOR INDEX DATA SET IF INPUT OFFLINE

INTEGER                   :: ICREC            ! CURRENT SEQUENTIAL RECORD NUMBER

INTEGER                   :: IPIND            ! PREVIOUS INDEX BLOCK
INTEGER                   :: IPDAT            ! PREVIOUS DATA BLOCK
INTEGER                   :: NBLIND           ! MAX NO INDEX ENTRIES IN BLOCK
INTEGER                   :: COUNT
INTEGER                   :: IX
INTEGER                   :: NIND
INTEGER                   :: NIBL
INTEGER                   :: LIMIT
INTEGER                   :: NOFLOW           ! OVERFLOW RECORD NUMBER
INTEGER                   :: I,J,K
INTEGER                   :: ILEN
INTEGER                   :: JL
INTEGER                   :: START
INTEGER                   :: X                ! => integer for arg list
INTEGER                   :: XA(1)            ! => integer for arg list
INTEGER                   :: IORC             ! Return code from IO
INTEGER                   :: NUMREC           ! Block number

!-----------------------------------------------------------------------
! dynamic common
!-----------------------------------------------------------------------

COMMON /MAPRDDC1/ IRECLN,CMAP,CTABD,BLOCK

!-----------------------------------------------------------------------
! variables need to be static. SAVE statement added.
!-----------------------------------------------------------------------

SAVE

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE = 'MAPRD' or 'MAPR1' read map block.                ***
! *** (MAPR1 is like MAPRD but always looks for the BUFR sequence
! ***  indicator after the map even if there are >12000 records.)
! ***                                                              ***
! ********************************************************************
! ********************************************************************

IF_MAPRD: &
IF (CTYPE == 'MAPRD' .OR. CTYPE == 'MAPR1') THEN

!-----------------------------------------------------------------------
! initialise variables
!-----------------------------------------------------------------------

  IF (LFLAG) WRITE(6,*) 'IN MAPRD'

  NELMIX = 0
  IRLEN = IDSK(2)
  NBLIND = (IRLEN - INDHED - 2)/INDLEN
  FLW1 = (6 + NBLIND*INDLEN) + 1
  FLW2 = FLW1 + 1
  NOFLOW = 0
  ITRIES = 0

  IFT  = IDSK(3)
  IF (LFLAG) &
  WRITE(6,'('' RECORD LENGTH '',I6,'' FT'',I2.2)') IRLEN,IFT

  NUMREC = 1
  CALL METDB_CREAD_DIR (IFT, CMAP(1:IRLEN), IRLEN, NUMREC, IORC)

  NBLKS = ICHAR(CMAP(1:1)) * 256 + ICHAR(CMAP(2:2))
  NXBLK = ICHAR(CMAP(3:3)) * 256 + ICHAR(CMAP(4:4))
  NXHRS = ICHAR(CMAP(5:5)) * 256 + ICHAR(CMAP(6:6))
  NAMHR = ICHAR(CMAP(7:7)) * 256 + ICHAR(CMAP(8:8))

!-----------------------------------------------------------------------
! If the number of blocks in the merge dataset is less than 12000, the
! sequence record indicator will be in the map block immediately
! after the data block pointers, otherwise it will be in byte 9 of
! the map block unless 'MAPR1' was specified.
!-----------------------------------------------------------------------

  IF (NBLKS <= 12000 .OR. CTYPE == 'MAPR1') THEN
    ITABD = ICHAR(CMAP(NBLKS+8:NBLKS+8))
  ELSE
    ITABD = ICHAR(CMAP(9:9))
  END IF

  IF (LFLAG) THEN
    WRITE(6,*) 'NO OF RECORDS IN DATASET       = ',NBLKS
    WRITE(6,*) 'NO OF INDEX BLOCKS             = ',NXBLK
    WRITE(6,*) 'HOURS PER INDEX                = ',NXHRS
    WRITE(6,*) 'START OF FIRST INDEX AFTER 00Z = ',NAMHR
  END IF

!-----------------------------------------------------------------------
! CHECK FOR LOCAL TABLE D ENTRY:    0 -> NO ENTRY
!                                   2 -> ENTRY PRESENT IN SECOND RECORD
!
! IF AN ENTRY IS PRESENT, THEN ALL BLOCK NUMBERS MUST BE CALCULATED
! TO ALLOW FOR THIS EXTRA RECORD; THIS IS DONE AT A HIGHER LEVEL
!-----------------------------------------------------------------------

  LOCDFG = ITABD == 2
  IF (LOCDFG) THEN
    NUMREC = 2
    CALL METDB_CREAD_DIR (IFT, CTABD(1:IRLEN), IRLEN, NUMREC, IORC)
    IF (LFLAG) PRINT*,' LOCAL TABLED ENTRY IN MDB DATASET'
    IF(CTABD(1:1) == '3') CALL LOCALD(0,0,XA,X,CTABD,'NEW')
    ICREC = 2                          ! TWO RECORDS READ
  ELSE
    IF (LFLAG) WRITE(6,*) ' NO LOCAL SEQUENCE IN SECOND RECORD'
    ICREC = 1                          ! ONLY ONE RECORD READ
  END IF

!-----------------------------------------------------------------------
! A call to MAPRD implies a new data set, so unset I/O-saving variables
! so there's no chance of leaving block from previous data set in core.
! (ICREC is the last block read from the offline data set, IPDAT the
! data block now in core: both are needed in case the data set changes!)
!-----------------------------------------------------------------------

  IPIND = 0
  IPDAT = 0

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='IDXRD' read index block.                           ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE IF (CTYPE == 'IDXRD') THEN

  IF (LFLAG) PRINT*,' IN IDXRD',IBLOCK

IF_INDXRD: &
  IF (IBLOCK /= IPIND) THEN
    COUNT = 0

!-----------------------------------------------------------------------
! calculate maximum possible number of index entry in one block
!-----------------------------------------------------------------------

    NBLIND = (IRLEN - INDHED - 2)/INDLEN

    IX   = IBLOCK
    NIND = 1
    LIMIT = 0

!-----------------------------------------------------------------------
! loop over index entries and store them in array 'cntry'. the implicit
! index 'k' corresponds to the current index of 'cntry' because the
! lower limit (1 + nibl) and the upper limit, 'limit', are recalculated
! each time round the loop.
!-----------------------------------------------------------------------

   45     NIBL = (NIND - 1) * NBLIND
    NOFLOW = 0

    CALL METDB_CREAD_DIR (IFT, CMAP(1:IRLEN), IRLEN, IX, IORC)
    IF (LFLAG) PRINT *,IX,'TH BLOCK READ'

    IDATHR = ICHAR(CMAP(1:1)) * 256 + ICHAR(CMAP(2:2))
    NTRIES = ICHAR(CMAP(3:3)) * 256 + ICHAR(CMAP(4:4))

    IF (NTRIES - COUNT > NBLIND) THEN
      LIMIT = NIBL + NBLIND
      COUNT = COUNT + NBLIND
      NOFLOW = ICHAR(CMAP(FLW1:FLW1))*256 + ICHAR(CMAP(FLW2:FLW2))
    ELSE
      LIMIT = NTRIES
    END IF

    IF (LFLAG) THEN
      PRINT*,' IDATHR = ', IDATHR
      PRINT*,' NTRIES = ', NTRIES
      PRINT*,' NIBL   = ', NIBL
      PRINT*,' LIMIT  = ', LIMIT
      PRINT*,' NBLIND = ', NBLIND
      PRINT*,' NOFLOW  = ', NOFLOW
    END IF

    START = 7

    DO K = 1 + NIBL, LIMIT
      CNTRY(K) = CMAP(START:START+INDLEN-1)
      START  = START + INDLEN
    END DO

    IF (NOFLOW > 0) THEN
      IX = NOFLOW
      IF(LOCDFG)IX=IX+1
      NIND = NIND + 1
      GOTO 45
    END IF

    IPIND = IBLOCK
    ITRIES = NTRIES

    IF (LFLAG) PRINT*,' TIMTAG IS',IDATHR
  END IF IF_INDXRD

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='MSGRD' read data block.                            ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE IF (CTYPE == 'MSGRD') THEN

  IF (LFLAG) WRITE(6,*) 'IN MSGRD   READING BLOCK',IBLOCK

!-----------------------------------------------------------------------
! If this block is already in memory, don't read it again.
! If the data set is offline, check against the last block read (here
! or in MAPRD section) to see whether to read on or rewind & restart.
!-----------------------------------------------------------------------

IF_NOTSTORED: &
  IF (IBLOCK /= IPDAT) THEN
    CALL METDB_CREAD_DIR (IDSK(3), BLOCK(1:IRLEN), IRLEN, IBLOCK, IORC)
    IPDAT = IBLOCK

    TOTREC = ICHAR(BLOCK(3:3)) * 256 + ICHAR(BLOCK(4:4))

    J = 7

    DO I = 1, TOTREC
      IRECLN(I) = ICHAR(BLOCK(J:J)) * 256 + ICHAR(BLOCK(J+1:J+1))
      J = J + 2
    END DO
  END IF IF_NOTSTORED

  IF (LFLAG) PRINT*,' BLOCK ',IBLOCK,' RECORD ',IDREC

!-----------------------------------------------------------------------
! Block has lengths at start and messages at end.
! First message ends at end of block.
! Work back to end of message required.
! Return zero length if record number too big (possible in SYNSEQ call)
! If message is too long for string, truncate it - but assume it
! has trailer at end, so keep last 23 characters & truncate rest.
!-----------------------------------------------------------------------

  IF (IDREC > TOTREC) THEN
    RECLEN=0
    RETURN
  END IF

  ILEN = IRLEN

  DO JL = 1, IDREC - 1
    ILEN = ILEN - IRECLN(JL)
  END DO

  RECLEN = IRECLN(IDREC)
  IF (RECLEN > 0) THEN
    CMSG=BLOCK(ILEN-RECLEN+1:ILEN)
    IF (RECLEN > LEN(CMSG)) THEN
      CMSG(LEN(CMSG)-22:)=BLOCK(ILEN-22:ILEN)
      PRINT *,'MAPRD truncated',RECLEN,'-character bulletin'
      RECLEN=LEN(CMSG)
    END IF
  END IF

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** CTYPE not recognised                                         ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE
  WRITE(6,*) 'MAPRD: CTYPE NOT RECOGNISED, CTYPE = ',CTYPE
END IF IF_MAPRD

RETURN
END SUBROUTINE MAPRD
