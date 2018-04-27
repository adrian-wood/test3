SUBROUTINE BUFRSEQ(X,Y,MAXDES,N,ND,DESCR,IRC)

! ---------------------------------------------------------------------
!
! Program       : BUFRSEQ
!
! Called by     : BUFDATA
!
! Purpose       : to expand a BUFR sequence descriptor
!  (informal)
!
!    (formal)     First see if a local sequence is input.
!                 If not, look up Table D.
!                 In either case get NSEQ descriptors back & insert
!                 them, overwriting the sequence descriptor itself.
!                 So leave N pointing to the first descriptor in
!                 the expansion.
!                 (N.B. Sequences CAN'T be expanded before other
!                  operations are performed, because replication
!                  counts would have to be adjusted.)
!
! Calls         : LOCALD
!                 TABLED
!
! Parameters    :
!  (1) X        category of sequence to be expanded                (i)
!                (not changed)
!  (2) Y        number of sequence in category                     (i)
!                (not changed)
!  (3) MAXDES   dimension of DESCR array                           (i)
!                (not changed)
!  (4) N        subscript of current descriptor                    (i)
!                (not changed)
!  (5) ND       total number of expanded descriptors               (i)
!                (not changed, only used by PRINDT)
!  (6) DESCR    descriptor array                                  (i/o)
!                (returned with character flag set on DESCR(N))
!  (7) IRC      return code:                                       (o)
!         IRC=301: sequence not found in Table D
!         IRC=302: no room left in descriptor array
!
! REVISION INFO :
!
! $Workfile: bufrseq.f90$ $Folder: OpSource$
! $Revsision: $ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  7    MetDB_Refresh 1.6         21/12/2010 12:25:00    Sheila Needham
!       Initialise IRC
!  6    MetDB_Refresh 1.5         12/11/2010 16:09:16    Richard Weedon  Rework
!        updated from peer review
!  5    MetDB_Refresh 1.4         26/10/2010 09:18:13    Richard Weedon
!       updated to F95 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

USE locald_mod
USE tabled_mod

IMPLICIT  NONE

INTEGER,INTENT(INOUT) ::  DESCR(*) ! argument (6)
INTEGER               ::  SEQ(999) ! temporary array for
                                   !  for sequence expansion
INTEGER,INTENT(IN)    ::  X        ! argument (1)
INTEGER,INTENT(IN)    ::  Y        ! argument (2)
INTEGER,INTENT(IN)    ::  MAXDES   ! argument (3)
INTEGER,INTENT(IN)    ::  N        ! argument (4)
INTEGER,INTENT(INOUT) ::  ND       ! argument (5)
INTEGER               ::  NSEQ     ! number of descriptors in sequence
INTEGER               ::  I        ! loop variable
INTEGER,INTENT(OUT)   ::  IRC      ! argument (7)

IRC = 0

! Look up sequence in table, trying local table first in case Table D
! is overridden.

CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
IF (NSEQ == 0) CALL TABLED(X,Y,SEQ,NSEQ)

! If neither Table D nor a local table has this sequence, give up.
! And give up if there's no room left in the descriptor array.

IF (NSEQ == 0) THEN
  IRC=301
  PRINT *,N,'-TH DESCRIPTOR HAS F=3, BUT NOT IN TABLE D'
  RETURN
END IF

IF (ND+NSEQ-1 > MAXDES) THEN
  IRC=302
  PRINT *,' NO ROOM TO EXPAND SEQUENCE IN DESCRIPTOR ARRAY'
  RETURN
END IF

! Move the descriptors after the insertion point to make room for the
! sequence, working from the end of the array to avoid overwriting
! descriptors which have not yet been moved.  Then insert the
! sequence, overwriting the F=3 descriptor, now no longer needed,
! and adjust the total number of descriptors.

DO I=ND,N+1,-1
  DESCR(I+NSEQ-1)=DESCR(I)
END DO

DO I=1,NSEQ
  DESCR(N+I-1)=SEQ(I)
END DO

ND=ND+NSEQ-1
RETURN
END SUBROUTINE BUFRSEQ
