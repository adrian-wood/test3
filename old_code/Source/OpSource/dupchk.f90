SUBROUTINE DUPCHK (ENTRY, LENTRY, LENCHK, NDXBLK, NUMNDX, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : DUPCHK
!
! PURPOSE     : To check whether a given index entry matches that for
!               a bulletin already stored.
!
! DESCRIPTION : DUPCHK compares the first LENCHK bytes of the index
!               entry ENTRY with the corresponding sections of all
!               entries in the NUMNDX index blocks held in the array
!               NDXBLK. If a match is found, the return code KODE is
!               set to 12; otherwise KODE will be zero.
!
! USAGE       : CALL DUPCHK
!                    (ENTRY, LENTRY, LENCHK, NDXBLK, NUMNDX, KODE)
!
! ARGUMENTS   : ('I'=Input, 'O'=Output)
!
!               ENTRY  (I) (CHARACTER*(*)) New index entry to be
!                          compared with entries in index block(s).
!               LENTRY (I) Total length of index entries (in bytes).
!               LENCHK (I) Length of part of index used for dup. check.
!               NDXBLK (I) (CHARACTER*(*) array with 'NUMNDX' elements)
!                          Chain of index blocks containing entries
!                          to be checked against 'ENTRY'.
!               NUMNDX (I) Number of index blocks in chain.
!               KODE   (O) Return code - values as follows:
!                             0  'ENTRY' not a duplicate entry,
!                            12  'ENTRY' matches data already stored.
!
! CALLED BY   : BUFREP
!
! CALLS       : ICHAR2
!
! HISTORY     : Original version by Brian Barwell, 27 November 2000.
!
! REVISION INFO:
!
! $Workfile: dupchk.f90$ $Folder: OpSource$
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

! Use statements:
USE ichar2_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN) :: ENTRY !a1 New index entry
INTEGER, INTENT(IN)    :: LENTRY !a2 Length of index entries (in bytes)
INTEGER, INTENT(IN)    :: LENCHK !a3 Len of index used for duplicate chk
INTEGER, INTENT(IN)    :: NUMNDX !a5 Number of index blocks in chain
CHARACTER(LEN=*), INTENT(IN)    :: NDXBLK(NUMNDX) !a4 Chain of idx blks
INTEGER, INTENT(OUT)   :: KODE   !a6 Return code (see above for values)

! Local declarations:

INTEGER, PARAMETER  ::  INDXHDR=10    ! Length of header section of
                       ! index blocks (must agree with routine 'BUFREP')

INTEGER    :: I1       ! Start pointer for duplicate check
INTEGER    :: I2       ! End pointer for duplicate check
INTEGER    :: JENTRY   ! Loop variable for index entries
INTEGER    :: JBLOCK   ! Loop variable for index blocks
INTEGER    :: NTRIES   ! Number of index entries in index block

!                                       Loop over index blocks in chain
DOJBLOCK: &
DO JBLOCK=1,NUMNDX
!                                 Find number of index entries in block
!
   NTRIES = ICHAR2(NDXBLK(JBLOCK)(6:7))
!                                                   Initialise pointers
   I1 = INDXHDR + 1      ! First byte to check
   I2 = INDXHDR + LENCHK ! Last byte to check
!
!                                      Loop over index entries in block
   DO JENTRY=1,NTRIES
!                                     Compare old and new index entries
!
      IF (NDXBLK(JBLOCK)(I1:I2) == ENTRY(1:LENCHK)) THEN ! same
         KODE = 12
         RETURN
      END IF
!                                  Update pointers for next index entry
      I1 = I1 + LENTRY
      I2 = I2 + LENTRY
   END DO ! JENTRY
END DO  DOJBLOCK
!                         Set return code and return to calling program
KODE = 0
RETURN
END SUBROUTINE DUPCHK
