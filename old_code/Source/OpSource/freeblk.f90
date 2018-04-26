SUBROUTINE FREEBLK (NBLK1, NBLK2, MAXPTR, NUMMAP, MAPBLK, NFREE)

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
! ARGUMENTS   : ('I'=Input, 'O'=Output)
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
! $Workfile: freeblk.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 25/01/2011 10:28:26$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         25/01/2011 10:28:26    Alison Weir     Ported
!        to f95 - BUFRDAT2
!  1    MetDB_Refresh 1.0         25/01/2011 09:53:08    Alison Weir
!       Initial f77 version
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

! Use statements: none

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)  :: NBLK1  !a1 First block number to be checked
INTEGER, INTENT(IN)  :: NBLK2  !a2 Last block number to be checked
INTEGER, INTENT(IN)  :: MAXPTR !a3 No of pointers a map block can hold
INTEGER, INTENT(IN)  :: NUMMAP !a4 No of map blocks in storage ds
CHARACTER(LEN=*), INTENT(IN) :: MAPBLK(NUMMAP) !a5 Map blocks
INTEGER, INTENT(OUT) :: NFREE  !a6 Next free block in storage dS

! Local declarations:

INTEGER      :: IPT1   ! First pointer to check in map blk
INTEGER      :: IPT2   ! Last pointer to check in map blk
INTEGER      :: JMAP   ! Loop variable for map blocks
INTEGER      :: JPT    ! Loop variable for index pointers
INTEGER      :: MAP1   ! First map block to search through
INTEGER      :: MAP2   ! Last map block to search through
INTEGER      :: NBYTE  ! Byte no. of start of pointer in map block

LOGICAL      :: FIRST=.TRUE.  ! TRUE if first call to this routine

CHARACTER(LEN=2) :: CHAR00    ! Hexadecimal '0000'
!                                                       Saved variables
SAVE FIRST, CHAR00
!
!-----------------------------------------------------------------------
!     INITIALISATIONS  (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
IF (FIRST) THEN
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
IF (MAP2 > NUMMAP) THEN
   WRITE (6,'(T5,A,T15,A,I6,A,I5,A)') 'FREEBLK:',   &
            'Some map blocks unavailable.',         &
            MAP2, ' expected but only', NUMMAP, ' supplied'
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
DOJMAP: &
DO JMAP=MAP1,MAP2
   NBYTE = 2*IPT1 - 1  ! First byte to look at
   IF (JMAP == MAP2) IPT2 = MOD(NBLK2-1,MAXPTR) + 1 ! Last map blk
!
!                                       Loop over pointers in map block
   DO JPT=IPT1,IPT2
!                               If free block, return with block number
!
      IF (MAPBLK(JMAP)(NBYTE:NBYTE+1) == CHAR00) THEN ! Free
         NFREE = MAXPTR*(JMAP-1) + JPT
         RETURN
      END IF
!                                  Update byte counter for next pointer
      NBYTE = NBYTE + 2
   END DO ! JPT
!                                Reset first pointer for next map block
   IPT1 = 1
END DO   DOJMAP
!                     Return with block number 0 if no free block found
NFREE = 0
RETURN
END SUBROUTINE FREEBLK
