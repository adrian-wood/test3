      SUBROUTINE CLRMAP (NINDEX, NBLK1, NBLK2,
     &                   LENREC, NUMMAP, MAPBLK, NUMAP, RESET)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : CLRMAP
!
! PURPOSE     : To reset pointers in the map blocks for a specified
!               index period to indicate that the blocks can be reused.
!
! DESCRIPTION : CLRMAP looks through the 2-byte pointers in the map
!               blocks corresponding to blocks NBLK1 to NBLK2 of a
!               storage data set and resets to zeroes any that point
!               to index block NINDEX. This allows them to be reused
!               for new data.
!
!               If any map block is altered, the corresponding element
!               of NUMAP is set negative, and a logical variable RESET
!               is set .TRUE. to indicate to the calling program that
!               something has changed.
!
!               CLRMAP does not write out the new map blocks.
!
! USAGE       : CALL CLRMAP (NINDEX, NBLK1, NBLK2, LENREC, NUMMAP,
!                            MAPBLK, NUMAP, RESET)
!
! PARAMETERS  : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!               NINDEX  (I)  Number of base index block for index
!                            period for which blocks are to be freed.
!               NBLK1   (I)  Number of first block to check.
!               NBLK2   (I)  Number of last block to check.
!               LENREC  (I)  Record length of storage data set.
!               NUMMAP  (I)  Number of map blocks in storage data set.
!               MAPBLK (I/O) (CHARACTER*(*) array with NUMMAP elements)
!                            Map blocks read from storage data set
!               NUMAP  (I/O) (INTEGER array with NUMMAP elements)
!                            Unit numbers for each map block (reset
!                            negative when contents are altered).
!               RESET   (O)  LOGICAL flag indicating whether anything
!                            was changed in the map block(s).
!
! CALLED BY   : BUFREP
!
! CALLS       : CHAR2
!
! HISTORY     : Original version by Brian Barwell, 17 October 2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:50$
! $Source: /home/us0400/mdb/op/lib/source/RCS/clrmap.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:50    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:14:15  usmdb
! Initial version
!
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
!
      IMPLICIT NONE
!                                                             Variables
!
      INTEGER IPT1, IPT2     ! First & last pointers to check in map blk
      INTEGER JMAP           ! Loop variable for map blocks
      INTEGER JPT            ! Loop variable for index pointers
      INTEGER LENREC         ! Record length of storage data set
      INTEGER NBLK1, NBLK2   ! Block numbers in data set to be checked
      INTEGER NINBLK         ! Number of pointers a map block can hold
      INTEGER NINDEX         ! Number of base index block to be freed
      INTEGER MAP1, MAP2     ! First & last map blocks to search through
      INTEGER NUMMAP         ! Number of map blocks in storage data set
      INTEGER NUMAP(NUMMAP)  ! Unit nos. for map blocks (<0 if altered)
      INTEGER NBYTE          ! Byte no. of start of pointer in map block
!
      LOGICAL FIRST          ! .TRUE. if first call to this routine
      LOGICAL RESET          ! .TRUE. if any pointers have been reset
!
      CHARACTER*2 CHAR00     ! Hexadecimal '0000'
      CHARACTER*2 CHARNDX    ! 2-byte pointer of blocks to be freed
      CHARACTER HEAD*132     ! Revision details
      CHARACTER*(*) MAPBLK(NUMMAP) ! Map blocks
!                                                     External function
!
      CHARACTER*2 CHAR2      ! I*4 to C*2 conversion routine
!
!                                                       Saved variables
      SAVE FIRST, CHAR00
!                                                  Data initialisations
      DATA FIRST /.TRUE./
!
!-----------------------------------------------------------------------
!     REVISION INFORMATION (FIRST CALL ONLY) AND INITIALISATIONS
!-----------------------------------------------------------------------
!
      IF (FIRST) THEN
!                                                  Revision information
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/clrmap.F,v $
     &   '//'$Date: 30/01/2006 20:21:50$ $Revision: 1$'
         FIRST = .FALSE.
!
         CHAR00 = CHAR(0) // CHAR(0)
      END IF
!                                                       Initialisations
      NINBLK = LENREC/2       ! Max. pointers per block
      CHARNDX = CHAR2(NINDEX) ! String to look for in map blocks
!
!-----------------------------------------------------------------------
!     INITIALISE MAP BLOCK NUMBERS & POINTERS FOR THE FIRST MAP BLOCK
!-----------------------------------------------------------------------
!
!                                          Range of map blocks to check
!
      MAP1 = (NBLK1-1)/NINBLK + 1  ! First map block to look at
      MAP2 = (NBLK2-1)/NINBLK + 1  ! Last map block to look at
!
!                               Check that all map blocks are available
      IF (MAP2.GT.NUMMAP) THEN
         WRITE (6,'(T5,A,T15,A,I6,A,I5,A)') 'CLRMAP:',
     &            'Some map blocks unavailable.',
     &            MAP2, ' expected but only', NUMMAP, ' supplied'
         MAP2 = NUMMAP
      END IF
!                                 Range of pointers for first map block
!
      IPT1 = MOD(NBLK1-1,NINBLK) + 1 ! 1st pointer to check in map block
      IPT2 = NINBLK                  !Last pointer to check in map block
!
!-----------------------------------------------------------------------
!     LOOP OVER 2-BYTE POINTERS IN MAP BLOCKS AND CLEAR OLD ONES
!-----------------------------------------------------------------------
!
      RESET = .FALSE.  ! No changes made yet
!                                                  Loop over map blocks
      DO JMAP=MAP1,MAP2
         NBYTE = 2*IPT1 - 1  ! First byte to look at
         IF (JMAP.EQ.MAP2) IPT2 = MOD(NBLK2-1,NINBLK) + 1 ! Last map blk
!
!                                       Loop over pointers in map block
         DO JPT=IPT1,IPT2
!                          Reset indicator if for specified index block
!
            IF (MAPBLK(JMAP)(NBYTE:NBYTE+1).EQ.CHARNDX) THEN
               MAPBLK(JMAP)(NBYTE:NBYTE+1) = CHAR00
               IF (NUMAP(JMAP).GT.0) NUMAP(JMAP) = -NUMAP(JMAP)
               RESET = .TRUE.
            END IF
!                                  Update byte counter for next pointer
            NBYTE = NBYTE + 2
         END DO ! JPT
!                                Reset first pointer for next map block
         IPT1 = 1
      END DO ! JMAP
!                                             Return to calling program
      RETURN
      END
