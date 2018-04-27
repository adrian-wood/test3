SUBROUTINE BUFDELT(NDTOTAL,ND,DESCR)

! ---------------------------------------------------------------------
!
! Program       : BUFDELT
!
! Called by     : BUFDATA
!
! Purpose       : To delete any descriptors which don't need to be
!  (informal)     returned to the user
!
! Calls         : DESFXY
!
! Parameters    :
!  (1) NDTOTAL  number of descriptors for previous obs              (i)
!               (zero except for several obs without compression)
!                (not changed)
!  (2) ND       number of expanded descriptors including this ob   (i/o)
!                (changed to reflect deletions)
!  (3) DESCR    descriptor array                                   (i/o)
!                (returned with deletions done
!                 & replicated increments unflagged)
!
! REVISION INFO :
!
! $Workfile: bufdelt.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
! ---------------------------------------------------------------------

USE desfxy_mod

IMPLICIT NONE

INTEGER,INTENT(IN)    ::  NDTOTAL ! no of descriptors for previous obs
INTEGER,INTENT(INOUT) ::  ND    ! number including this ob (ND>NDTOTAL)
INTEGER,INTENT(INOUT) ::  DESCR(*)  ! descriptor array
INTEGER               ::  I         ! loop variable
INTEGER               ::  MOUT      ! to count descriptors to be output
INTEGER               ::  F,X,Y     ! components of descriptor FXXYYY
INTEGER               ::  FLINCR    ! flag on increment descriptor
INTEGER               ::  FLDEL     ! flag on descriptor to be deleted

DATA FLINCR/262144/   ! 2**18 to check or unset increment flag
DATA FLDEL/1073741824/! 2**30 to check or unset deletion flag


! Initialise variables for deletion loop.

MOUT=NDTOTAL
DO_CONSTR1 : &
DO I=NDTOTAL+1,ND

! First unset the increment flag on replicated increments.

  IF (MOD(DESCR(I)/FLINCR,2) == 1) DESCR(I)=DESCR(I)-FLINCR

! Then unflag replication operators, which are not to be deleted.
! The next 2 IFs CAN'T be combined: the first may unset the flag tested!

  IF (MOD(DESCR(I)/FLDEL,2) == 1) THEN
    CALL DESFXY(DESCR(I),F,X,Y)
    IF (F == 1) DESCR(I)=DESCR(I)-FLDEL
  END IF

! Move up the next descriptor if it's not to be deleted.

  IF (MOD(DESCR(I)/FLDEL,2) /= 1) THEN
    MOUT=MOUT+1
    DESCR(MOUT)=DESCR(I)
  END IF
END DO DO_CONSTR1
ND=MOUT
RETURN
END SUBROUTINE BUFDELT
