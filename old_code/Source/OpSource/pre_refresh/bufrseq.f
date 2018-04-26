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
! $Revision: 1$
! $Date:
! $Source:
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

      IMPLICIT  NONE

      INTEGER   DESCR(*)   ! argument (6)
      INTEGER   SEQ(999)   ! temporary array for sequence expansion
      INTEGER   X          ! argument (1)
      INTEGER   Y          ! argument (2)
      INTEGER   MAXDES     ! argument (3)
      INTEGER   N          ! argument (4)
      INTEGER   ND         ! argument (5)
      INTEGER   NSEQ       ! number of descriptors in sequence
      INTEGER   I          ! loop variable
      INTEGER   IRC        ! argument (7)

      LOGICAL   HEADSET
      DATA      HEADSET/.FALSE./
      CHARACTER HEAD*132

      IF (.NOT.HEADSET) THEN
        HEAD='$RCSfile: bufrseq.f,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:21:29$'
        HEADSET=.TRUE.
      ENDIF

! Look up sequence in table, trying local table first in case Table D
! is overridden.

      CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
      IF (NSEQ.EQ.0) CALL TABLED(X,Y,SEQ,NSEQ)

! If neither Table D nor a local table has this sequence, give up.
! And give up if there's no room left in the descriptor array.

      IF (NSEQ.EQ.0) THEN
        IRC=301
        PRINT *,N,'-TH DESCRIPTOR HAS F=3, BUT NOT IN TABLE D'
        RETURN
      ENDIF

      IF (ND+NSEQ-1.GT.MAXDES) THEN
        IRC=302
        PRINT *,' NO ROOM TO EXPAND SEQUENCE IN DESCRIPTOR ARRAY'
        RETURN
      ENDIF

! Move the descriptors after the insertion point to make room for the
! sequence, working from the end of the array to avoid overwriting
! descriptors which have not yet been moved.  Then insert the
! sequence, overwriting the F=3 descriptor, now no longer needed,
! and adjust the total number of descriptors.

      DO I=ND,N+1,-1
        DESCR(I+NSEQ-1)=DESCR(I)
      ENDDO

      DO I=1,NSEQ
        DESCR(N+I-1)=SEQ(I)
      ENDDO

      ND=ND+NSEQ-1
      RETURN
      END
