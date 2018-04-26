      SUBROUTINE FREEBLK (NBLK1, NBLK2, MAXPTR, NUMMAP, MAPBLK, NFREE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE  : FREEBLK
!
! PURPOSE     : To find the next free block in a given range of block
!               numbers of a storage data set.
!
! DESCRIPTION : FREEBLK looks through the 2-byte pointers in the map
!               blocks corresponding to blocks NBLK1 to NBLK2 of a
!               storage data set until it finds a free one (i.e. one
!               whose pointer in the map block is zero). It then
!               returns the free block number to the calling program.
!
!               If no free block is found, a block number of zero is
!               returned.
!
!               FREEBLK does not alter any of the map blocks.
!
! USAGE      : CALL FREEBLK
!                   (NBLK1, NBLK2, MAXPTR, NUMMAP, MAPBLK, NFREE)
!
! PARAMETERS  : ('I'=Input, 'O'=Output)
!
!               NBLK1   (I)  Number of first block to check.
!               NBLK2   (I)  Number of last block to check.
!               MAXPTR  (I)  Maximum number of pointers a map block
!                            can hold (= record length divided by 2).
!               NUMMAP  (I)  Number of map blocks in storage data set.
!               MAPBLK  (I)  (CHARACTER*(*) array with NUMMAP elements)
!                            Map blocks read from storage data set.
!               NFREE   (O)  Block number of next free block.
!                            (Zero returned if none free.)
!
! CALLED BY   : BUFREP
!
! CALLS       : None
!
! HISTORY     : Original version by Brian Barwell, 17 October 2000.
!
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:30$
! $Source: /home/us0400/mdb/op/lib/source/RCS/freeblk.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:30    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:19:01  usmdb
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
      INTEGER MAP1, MAP2     ! First & last map blocks to search through
      INTEGER MAXPTR         ! Number of pointers a map block can hold
      INTEGER NBLK1, NBLK2   ! Block numbers in data set to be checked
      INTEGER NBYTE          ! Byte no. of start of pointer in map block
      INTEGER NFREE          ! Next free block in storage data set
      INTEGER NUMMAP         ! Number of map blocks in storage data set
!
      LOGICAL FIRST          ! .TRUE. if first call to this routine
!
      CHARACTER*2 CHAR00     ! Hexadecimal '0000'
      CHARACTER HEAD*132     ! Revision details
      CHARACTER*(*) MAPBLK(NUMMAP) ! Map blocks
!                                                       Saved variables
      SAVE FIRST, CHAR00
!                                                  Data initialisations
      DATA FIRST /.TRUE./
!
!-----------------------------------------------------------------------
!     REVISION INFORMATION AND INITIALISATIONS  (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
      IF (FIRST) THEN
!                                                  Revision information
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/freeblk.F,v $
     &   '//'$Date: 30/01/2006 20:22:30$ $Revision: 1$'
!                                                       Initialisations
         CHAR00 = CHAR(0) // CHAR(0)
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!     INITIALISE MAP BLOCK NUMBERS & POINTERS FOR THE FIRST MAP BLOCK
!-----------------------------------------------------------------------
!
!                                          Range of map blocks to check
!
      MAP1 = (NBLK1-1)/MAXPTR + 1  ! First map block to look at
      MAP2 = (NBLK2-1)/MAXPTR + 1  ! Last map block to look at
!
!                               Check that all map blocks are available
      IF (MAP2.GT.NUMMAP) THEN
         WRITE (6,'(T5,A,T15,A,I6,A,I5,A)') 'FREEBLK:',
     &            'Some map blocks unavailable.',
     &            MAP2, ' expected but only', NUMMAP, ' supplied'
         MAP2 = NUMMAP
      END IF
!                                 Range of pointers for first map block
!
      IPT1 = MOD(NBLK1-1,MAXPTR) + 1 ! 1st pointer to check in map block
      IPT2 = MAXPTR                  !Last pointer to check in map block
!
!-----------------------------------------------------------------------
!     LOOP OVER 2-BYTE POINTERS IN MAP BLOCKS UNTIL FREE BLOCK IS FOUND
!-----------------------------------------------------------------------
!
!                                                  Loop over map blocks
      DO JMAP=MAP1,MAP2
         NBYTE = 2*IPT1 - 1  ! First byte to look at
         IF (JMAP.EQ.MAP2) IPT2 = MOD(NBLK2-1,MAXPTR) + 1 ! Last map blk
!
!                                       Loop over pointers in map block
         DO JPT=IPT1,IPT2
!                               If free block, return with block number
!
            IF (MAPBLK(JMAP)(NBYTE:NBYTE+1).EQ.CHAR00) THEN ! Free
               NFREE = MAXPTR*(JMAP-1) + JPT
               RETURN
            END IF
!                                  Update byte counter for next pointer
            NBYTE = NBYTE + 2
         END DO ! JPT
!                                Reset first pointer for next map block
         IPT1 = 1
      END DO ! JMAP
!                     Return with block number 0 if no free block found
      NFREE = 0
      RETURN
      END
