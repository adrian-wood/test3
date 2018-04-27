SUBROUTINE BUFD206(STRING,IBEFOR,DESCR,NOBS,CMPRES,Y,N,IRC)
! ---------------------------------------------------------------------
!
! Program       : BUFD206
!
! Called by     : BUFDATA
!
! Purpose       : Skip the value corresponding to a hidden descriptor.
!  (informal)     (The regulations say this operation only applies to
!                 a local element descriptor, but here the operation
!                 is carried out with no check for legitimacy.)
!
!    (formal)     Skip a Y-bit field after IBEFOR bits in STRING (plus
!                 an increment width & NOBS increments if the data is
!                 CMPRESsed).  Skip means move N past the descriptor
!                 (flagged for deletion) & IBEFOR past the value.
!                    (The regulations talk about a descriptor, which
!                 could be an element or sequence; it has been argued
!                 that only an element can be hidden by 206YYY, but
!                 there seems no reason why the YYY bits shouldn't
!                 cover a whole sequence in uncompressed data.)
!
! Calls         : VALUE (function) to get number from bit string
!               : DESFXY
!
! Parameters    :
!  (1) STRING   bit string from BUFR message                       (i)
!                (not changed)
!  (2) IBEFOR   number of bits in STRING before value concerned   (i/o)
!                (updated to skip value & any increments
!  (3) DESCR    descriptor array                                  (i/o)
!                (will be changed if deletion flags added)
!  (4) NOBS     number of reports in message, i.e. number of       (i)
!               values for each field if data is compressed
!                (not changed)
!  (5) CMPRES   true if data in message compressed                 (i)
!                (not changed)
!  (6) Y        number of bits to skip (from 206YYY)               (i)
!                (not changed)
!  (7) N        subscript of current descriptor                   (i/o)
!                (returned incremented by 1)
!  (8) IRC      return code:                                       (o)
!         IRC=206 descriptor is sequence, not element, & data compressed
!
! REVISION INFO :
!
! $Workfile: bufd206.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  8    MetDB_Refresh 1.7         20/12/2010 16:30:15    Sheila Needham
!       Initialise IRC
!  7    MetDB_Refresh 1.6         25/11/2010 11:36:59    Brian Barwell   Intent
!        for NOBS changed from INOUT to IN.
!  6    MetDB_Refresh 1.5         11/11/2010 16:08:55    Richard Weedon  rework
!        from peer review
!  5    MetDB_Refresh 1.4         25/10/2010 15:37:15    Richard Weedon
!       updated to f90 standard
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
!
USE value_mod
USE desfxy_mod
!
IMPLICIT NONE

LOGICAL,INTENT(IN)           :: CMPRES     ! argument (5)
INTEGER,INTENT(INOUT)        :: DESCR(*)   ! argument (3)
INTEGER                      :: FLDEL !to flag descriptor for deletion
INTEGER,INTENT(INOUT)        :: IBEFOR     ! argument (2)
INTEGER,INTENT(OUT)          :: IRC        ! argument (8)
INTEGER,INTENT(INOUT)        :: N          ! argument (7)
INTEGER                      :: NCREM      ! increment width
INTEGER                      :: NEXTF      ! F from next descriptor
INTEGER                      :: NEXTX      ! X from next descriptor
INTEGER                      :: NEXTY ! Y from next descriptor
INTEGER,INTENT(IN)           :: NOBS       ! argument (4)
CHARACTER(LEN=*),INTENT(IN)  :: STRING     ! argument (1)
! INTEGER ::VALUE      ! function to get value from data section
INTEGER,INTENT(IN)           :: Y          ! argument (6)

SAVE

DATA FLDEL/1073741824/   ! 2**30 (descriptor deletion flag)

IRC = 0

! If a descriptor is hidden by 206YYY, where Y is the field width,
! flag descriptor (element or sequence) for deletion & skip Y bits.

IF (MOD(DESCR(N)/FLDEL,2) /= 1) DESCR(N)=DESCR(N)+FLDEL
IBEFOR=IBEFOR+Y                        ! skip Y bits

! If the data is compressed, then skip the increments too if it's an
! element, give up if it's a sequence (impossible to complete skip).

IF (CMPRES) THEN
  CALL DESFXY(DESCR(N),NEXTF,NEXTX,NEXTY)

  IF (NEXTF == 0) THEN
    NCREM=VALUE(STRING,IBEFOR,6)       ! get increment width,
    IBEFOR=IBEFOR+NCREM*NOBS           ! skip the increments

  ELSE IF (NEXTF == 3) THEN
    PRINT *,'Sequence descriptor hidden by 206...,'
    PRINT *,'but data is compressed, so cannot skip.'
    IRC=206
  END IF
END IF

N=N+1                                  ! finally skip descriptor
RETURN
END SUBROUTINE BUFD206
