LOGICAL FUNCTION BUFRQOP(DESCR,ND)

!-----------------------------------------------------------------------
!
! ROUTINE       : BUFRQOP
!
! CALLED BY     : DECODE, BUFDATA
!
! PURPOSE       : To see if there are any quality operations in the
!  (informal)     descriptor sequence
!
!    (formal)     Loop round the descriptors, adding any expanded
!                 sequence on the end.  (Putting an expansion after
!                 the original sequence or after any sequence already
!                 added leaves the original descriptors as they 
!                 were on input.)
!                 As soon as an operator has an XX of 22-25 or 32,
!                 return with QUALOPS true.  If none found, it's false.
!
! CALLS         : DESFXY
!                 LOCALD
!                 TABLED
!
! PARAMETERS    :
!
!  (1) DESCR    descriptor array
!                (The ND descriptors are left as they are;
!                 the rest of the array is used as work space)
!  (2) ND       total number of expanded descriptors
!                (not changed)
!
! ERROR RETURNS : none
!
! REVISION INFO :
!
! $Workfile: bufrqop.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 09/03/2011 11:41:46$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         09/03/2011 11:41:46    Alison Weir
!       Removed use of assumed-size array section with high upper bound, as a
!       precaution.
!  9    MetDB_Refresh 1.8         07/03/2011 13:49:36    Alison Weir
!       parameter added to give largest size of array
!  8    MetDB_Refresh 1.7         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  7    MetDB_Refresh 1.6         21/12/2010 10:29:22    Sheila Needham  Change
!        INTENT to INOUT on DESCR array (because it's used as workspace)
!  6    MetDB_Refresh 1.5         27/10/2010 13:32:12    Richard Weedon  intent
!        added
!  5    MetDB_Refresh 1.4         25/10/2010 12:31:14    Richard Weedon
!       confirmed to f90 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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

USE desfxy_mod
USE locald_mod
USE tabled_mod

IMPLICIT NONE

INTEGER,INTENT(INOUT) ::  DESCR(*) ! descriptor array
INTEGER,INTENT(INOUT) ::  ND       ! input number of descriptors

INTEGER, PARAMETER    ::  MAXNSEQ=999  ! Maximum number of descriptors

INTEGER               ::  F,X,Y    ! components of current descriptor
INTEGER               ::  NDS      ! ND inc for any sequence expansion
INTEGER               ::  N        ! current descriptor number (in loop)
INTEGER               ::  NSEQ     ! number of descriptors in sequence
INTEGER               ::  SEQ(MAXNSEQ)  ! expanded sequences


NDS=ND
N=1
BUFRQOP=.FALSE.

!-----------------------------------------------------------------------
! Loop round the input descriptors (expanding any sequence), seeing
! if any F=2 descriptor is a quality operation.  Set flag if one is
! found - no need to loop any further.
! If a sequence descriptor is found, just put the expansion on the end
! for speed - the order of the descriptors is irrelevant for this check.
!-----------------------------------------------------------------------

DO WHILE (N <= NDS .AND. .NOT.BUFRQOP)
  CALL DESFXY(DESCR(N),F,X,Y)
  IF (F == 2) THEN
    IF ((X >= 22 .AND. X <= 25) .OR. X == 32) BUFRQOP=.TRUE.
  ELSE IF (F == 3) THEN
    CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
    IF (NSEQ == 0) CALL TABLED(X,Y,SEQ,NSEQ)
    DESCR(NDS+1:NDS+NSEQ) = SEQ(1:NSEQ)
    NDS=NDS+NSEQ
  END IF
  N=N+1
END DO

RETURN
END FUNCTION BUFRQOP
