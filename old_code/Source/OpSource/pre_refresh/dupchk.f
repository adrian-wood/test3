      SUBROUTINE DUPCHK (ENTRY, LENTRY, LENCHK, NDXBLK, NUMNDX, KODE)
!
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
! PARAMETERS  : ('I'=Input, 'O'=Output)
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
! CALLS       : CHAR2
!
! HISTORY     : Original version by Brian Barwell, 27 November 2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/dupchk.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:09    Sheila Needham  
! $
! Revision 2.0  2001/06/06 10:14:33  usmdb
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
!                                                            Parameters
!
      INTEGER    INDXHDR     ! Length of header section of index blocks
      PARAMETER (INDXHDR=10) !  (must agree with routine 'BUFREP')
!
!                                                             Variables
!
      INTEGER I1, I2         ! Start & end pointers for duplicate check
      INTEGER JENTRY         ! Loop variable for index entries
      INTEGER JBLOCK         ! Loop variable for index blocks
      INTEGER KODE           ! Return code (see above for values)
      INTEGER LENCHK         ! Length of index used for duplicate check
      INTEGER LENTRY         ! Length of index entries (in bytes)
      INTEGER NTRIES         ! Number of index entries in index block
      INTEGER NUMNDX         ! Number of index blocks in chain
!
      LOGICAL FIRST          ! .TRUE. if first call to this routine
!
      CHARACTER*(*) ENTRY          ! New index entry
      CHARACTER*132 HEAD           ! Revision information
      CHARACTER*(*) NDXBLK(NUMNDX) ! Chain of index blocks
!
!                                                     External function
!
      INTEGER ICHAR2         ! C*2 to I*4 conversion routine
!
!                                                        Saved variable
      SAVE FIRST
!                                                   Data initialisation
      DATA FIRST /.TRUE./
!                                Revision information (first call only)
      IF (FIRST) THEN
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/dupchk.F,v $
     &   '//'$Date: 30/01/2006 20:22:09$ $Revision: 1$'
         FIRST = .FALSE.
      END IF
!                                       Loop over index blocks in chain
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
            IF (NDXBLK(JBLOCK)(I1:I2).EQ.ENTRY(1:LENCHK)) THEN ! same
               KODE = 12
               RETURN
            END IF
!                                  Update pointers for next index entry
            I1 = I1 + LENTRY
            I2 = I2 + LENTRY
         END DO ! JENTRY
      END DO ! JBLOCK
!                         Set return code and return to calling program
      KODE = 0
      RETURN
      END
