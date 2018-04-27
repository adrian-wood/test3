SUBROUTINE MDBIO(IDSK,NXBLK,NXHRS,NAMHR,INDLEN,LOCDFG,    &
                 NELMIX,IBLOCK,ITRIES,CNTRY,IDATHR,IDREC, &
                 RECLEN,CMSG,CTYPE)

!-----------------------------------------------------------------------
!
! PROGRAM       : MDBIO     (made from MAPRD for use by storage)
!
! PURPOSE       : TO READ MAP BLOCK, RETURNING PARAMETERS AT START,
!                 then read indexes and messages in data blocks
!
! DESCRIPTION   : 'MAPRD' call first, then any number of 'IDXRD' or
!                 'MSGRD' calls
!
! DATA TYPE(S)  : SYNOPS (sequence checks), AIREPs (midpoint data)
!
! CALLED BY     : AIRIDX, SYNSEQ, SHPSEQ
!
! ARGUMENTS     : (1)IDSK(5)  IDSK(2) is blocksize, IDSK(3) FT number
!                 (2)NXBLK    NO OF INDEX BLOCKS IN D/S      (OUTPUT)
!                 (3)NXHRS    NO OF HOURS PER INDEX BLOCK    (OUTPUT)
!                 (4)NAMHR    START HOUR OF FIRST INDEX BLK  (OUTPUT)
!                 (5)INDLEN   LENGTH OF INDEX ENTRIES        (input)
!                 (6)LOCDFG   LOCAL TABLE D FLAG             (OUTPUT)
!                 (7)NELMIX   block number for element index (output)
!                 (8)iblock   physical block number to read  (input)
!                 (9)itries   number of entries read         (output)
!                (10)cntry    char*(*) array of entries read (output)
!                (11)idathr   time tag of index read         (output)
!                (12)idrec    logical record number          (input)
!                (13)reclen   total length of message        (output)
!                (14)cmsg     char*(*) actual message        (output)
!                (15)ctype    type of block to read          (input)
!
! REVISION INFO :
!
!
! $Workfile: mdbio.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 16/03/2011 12:24:03$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         16/03/2011 12:24:03    Alison Weir     Change
!        READs to C IO routine
!  1    MetDB_Refresh 1.0         07/01/2011 11:44:04    Rosemary Lavery
!       Initial port
! $
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

! Interface arguments

INTEGER, INTENT(IN)             :: IDSK(5)   ! DATA SET DETAILS
INTEGER, INTENT(OUT)            :: NXBLK     ! NUMBER OF FIXED INDEX BLOCKS
INTEGER, INTENT(OUT)            :: NXHRS     ! NO OF HOURS PER INDEX
INTEGER, INTENT(OUT)            :: NAMHR     ! START OF FIRST INDEX AFTER 00Z
INTEGER, INTENT(IN)             :: INDLEN    ! LENGTH OF INDEX ENTRY
LOGICAL, INTENT(OUT)            :: LOCDFG    ! LOCAL TABLE D FLAG
INTEGER, INTENT(OUT)            :: NELMIX    ! block number of element index
INTEGER, INTENT(IN)             :: IBLOCK
INTEGER, INTENT(OUT)            :: ITRIES    ! number of index entries
CHARACTER (LEN=*), INTENT(OUT)  :: CNTRY(:)  ! ARRAY OF INDEX ENTRIES
INTEGER, INTENT(OUT)            :: IDATHR
INTEGER, INTENT(IN)             :: IDREC
INTEGER, INTENT(OUT)            :: RECLEN
CHARACTER (LEN=*), INTENT(OUT)  :: CMSG      ! THE MESSAGE ITSELF
CHARACTER (LEN=5), INTENT(IN)   :: CTYPE     ! type of block to read

! Local parameters

INTEGER, PARAMETER  :: MAXINDX=7000
INTEGER, PARAMETER  :: BLKSIZ=27998
INTEGER, PARAMETER  :: INDHED=6

! Local variables

INTEGER    :: COUNT
INTEGER    :: FLW              ! START OF OVERFLOW NUM IN INDEX BLOCK
INTEGER    :: I
INTEGER    :: ILEN
INTEGER    :: INDBLK           ! PREVIOUS INDEX BLOCK
INTEGER    :: INDFT            ! ft number for INDEX BLOCK
INTEGER    :: IORC             ! RETURN CODE FROM IO ROUTINES
INTEGER    :: IRECLN(MAXINDX)
INTEGER    :: ITABD            ! LOCAL TABLE D INDICATOR
INTEGER    :: IX
INTEGER    :: J
INTEGER    :: LIMIT
INTEGER    :: MSGFT            ! ft number for DATA BLOCK
INTEGER    :: MSGBLK           ! PREVIOUS DATA BLOCK
INTEGER    :: NBLIND           ! MAX NO INDEX ENTRIES IN BLOCK
INTEGER    :: NBLKS            ! TOTAL NO OF RECORDS IN DATASET
INTEGER    :: NIBL
INTEGER    :: NIND
INTEGER    :: NOFLOW           ! OVERFLOW RECORD NUMBER
INTEGER    :: RECNO            ! STORAGE DS RECORD NUMBER
INTEGER    :: START
INTEGER    :: TOTREC           ! number of messages in data block

CHARACTER (LEN=BLKSIZ)  :: BLOCK
CHARACTER (LEN=BLKSIZ)  :: CMAP    ! MAP BLOCK

!  *******************************************************************
! COMPILE WITH DYNAMIC COMMON - FPARMS='DC(MAP1)'.
! THERE IS NO REFERENCE TO MAP1 ELSEWHERE (COMMON IS JUST TO SAVE SPACE
! IN THE LOAD LIBRARY), BUT BECAUSE MDBIO IS IN SEVERAL MDB RETRIEVAL
! MODULES, RETRIEVALS USING MORE THAN ONE OF THESE MODULES WILL FAIL
! IF THE MAP1 DEFINITIONS ARE INCONSISTENT - SO DON'T ASSUME THAT MAP1
! CAN BE CHANGED IN ONE MODULE AND OTHERS LEFT TILL LATER!!!
!  *******************************************************************

COMMON /MAP1/ IRECLN,CMAP,BLOCK

! --------------------------------------------------------------------
! --- variables need to be static. SAVE statement added.
! --------------------------------------------------------------------

SAVE

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='MAPRD' read map block.                             ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

IF_BLKTYPE: &
IF (CTYPE == 'MAPRD') THEN
! READ(IDSK(3),REC=1) CMAP(1:idsk(2))
  RECNO=1
  CALL METDB_CREAD_DIR(IDSK(3),CMAP,IDSK(2),RECNO,IORC)

  NBLKS=ICHAR(CMAP(1:1))*256+ICHAR(CMAP(2:2))
  NXBLK=ICHAR(CMAP(3:3))*256+ICHAR(CMAP(4:4))
  NXHRS=ICHAR(CMAP(5:5))*256+ICHAR(CMAP(6:6))
  NAMHR=ICHAR(CMAP(7:7))*256+ICHAR(CMAP(8:8))
  ITABD=ICHAR(CMAP(NBLKS+8:NBLKS+8))

! Map byte of one after index blocks can only be for element index:
! return block number (missing if no byte set to one was found)

  NELMIX=1+NXBLK+INDEX(CMAP(8+NXBLK+1:8+NBLKS-1),CHAR(1))
  IF (NELMIX == 1+NXBLK) THEN
    NELMIX=-9999999
  ELSE IF (ITABD > 1) THEN
    NELMIX=NELMIX+1
  END IF

  IF (ITABD > 0) THEN
    LOCDFG=.TRUE.
  ELSE
    LOCDFG=.FALSE.
  END IF

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='IDXRD' read index block.                           ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE IF (CTYPE == 'IDXRD') THEN

IF_INDEX: &
  IF (IDSK(3) /= INDFT .OR. IBLOCK /= INDBLK) THEN
    INDFT=IDSK(3)
    INDBLK=IBLOCK
    COUNT=0

! Calculate maximum possible number of index entries in one block

    NBLIND=(IDSK(2)-INDHED-2)/INDLEN
    FLW=(6+NBLIND*INDLEN)+1

    IX=IBLOCK
    NIND=1
    LIMIT=0

! Loop over index entries and store them in array CNTRY.
! The range of I is recalculated each time round the loop.

   45     CONTINUE

    NIBL=(NIND-1)*NBLIND

!   READ (INDFT,REC=IX) CMAP(1:IDSK(2))
    CALL METDB_CREAD_DIR(INDFT,CMAP,IDSK(2),IX,IORC)

    IDATHR=ICHAR(CMAP(1:1))*256+ICHAR(CMAP(2:2))
    ITRIES=ICHAR(CMAP(3:3))*256+ICHAR(CMAP(4:4))

    IF (ITRIES-COUNT > NBLIND) THEN
      LIMIT=NIBL+NBLIND
      COUNT=COUNT+NBLIND
      NOFLOW=ICHAR(CMAP(FLW:FLW))*256+ICHAR(CMAP(FLW+1:FLW+1))
    ELSE
      LIMIT=ITRIES
      NOFLOW=0
    END IF

    START=7
    DO I=1+NIBL,LIMIT
      CNTRY(I)=CMAP(START:START+INDLEN-1)
      START=START+INDLEN
    END DO

    IF (LIMIT /= ITRIES) THEN
      IX=NOFLOW
      IF(LOCDFG)IX=IX+1
      NIND=NIND+1
      GOTO 45
    END IF
  END IF IF_INDEX

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** If CTYPE='MSGRD' read data block.                            ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE IF (CTYPE == 'MSGRD') THEN

! If this block is already in memory, don't read it again.
IF_INMEM: &
  IF (IDSK(3) /= MSGFT .OR. IBLOCK /= MSGBLK) THEN
    MSGFT=IDSK(3)
!   READ(MSGFT,REC=IBLOCK)BLOCK(1:idsk(2))
    CALL METDB_CREAD_DIR(MSGFT,BLOCK,IDSK(2),IBLOCK,IORC)
    MSGBLK=IBLOCK
    TOTREC=ICHAR(BLOCK(3:3))*256+ICHAR(BLOCK(4:4))

! Lengths at start of block

    J=7
    DO I=1,TOTREC
      IRECLN(I)=ICHAR(BLOCK(J:J))*256+ICHAR(BLOCK(J+1:J+1))
      J=J+2
    END DO
  END IF IF_INMEM

! Block has lengths at start and messages at end.
! First message ends at end of block.
! Work back to end of message required by subtracting lengths.
! Return zero length if record number too big (possible in SYNSEQ call)

  IF (IDREC > TOTREC) THEN
    RECLEN=0
    RETURN
  END IF

  ILEN=idsk(2)
  DO I=1,IDREC-1
    ILEN=ILEN-IRECLN(I)
  END DO

  RECLEN=IRECLN(IDREC)
  IF (RECLEN > 0) CMSG=BLOCK(ILEN-RECLEN+1:ILEN)

! ********************************************************************
! ********************************************************************
! ***                                                              ***
! *** CTYPE not recognised                                         ***
! ***                                                              ***
! ********************************************************************
! ********************************************************************

ELSE
  WRITE(6,*) 'IN MDBIO: CTYPE NOT RECOGNISED, CTYPE=',CTYPE
END IF IF_BLKTYPE

RETURN
END SUBROUTINE MDBIO
