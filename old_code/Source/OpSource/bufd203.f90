SUBROUTINE BUFD203(Y,STRING,DESCR,CMPRES,          &
IBEFOR,N,NREF,REFDES,NEWREF,IRC)
! ---------------------------------------------------------------------
!
! Program       : BUFD203
!
! Called by     : BUFDATA
!
! Purpose       : List reference value changes with corresponding
!  (informal)     descriptors for later use, or empty the list if
!                 the operation is cancelled.
!    (formal)      Each reference value changed by one 203YYY
!                 is encoded in YYY bits (sign in top bit).
!                  Note the assumption (not stated in the Manual)
!                 that 203YYY is only valid with compression if
!                 the changes are the same for all obs.
!
! Calls         : VALUE to get values from data section
!
! Parameters    :
!  (1) Y        number of bits for each new reference value        (i)
!                (not changed)
!  (2) STRING   bit string from BUFR message                       (i)
!                (not changed)
!  (3) DESCR    descriptor array                                   (i)
!                (not changed)
!  (4) CMPRES   set if data compressed                             (i)
!                (not changed)
!  (5) IBEFOR   number of bits in STRING before values concerned  (i/o)
!                (updated to skip new reference values)
!  (6) N        subscript of current descriptor                   (i/o)
!                (returned past list of new reference values)
!  (7) NREF     number of reference values in list of changes     (i/o)
!                (updated to include changes in this operation)
!  (8) REFDES   descriptors with changes listed in NEWREF         (i/o)
!                (added to)
!  (9) NEWREF   reference value changes for elements in REFDES    (i/o)
!                (added to)
! (10) IRC      return code:                                       (o)
!         IRC=123: data compressed, but changes vary from ob to ob
!         IRC=203: no 203255, so don't know where list ends
!
! REVISION INFO :
!
! $Workfile: bufd203.f90$ $Folder: OpSource$
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
!
USE value_mod

IMPLICIT NONE

INTEGER,INTENT(IN)             ::  Y         ! argument (1)
CHARACTER(LEN=*),INTENT(IN)    ::  STRING    ! argument (2)
INTEGER,INTENT(INOUT)          ::  DESCR(*)  ! argument (3)
LOGICAL,INTENT(IN)             ::  CMPRES    ! argument (4)
INTEGER,INTENT(INOUT)          ::  IBEFOR    ! argument (5)
INTEGER,INTENT(INOUT)          ::  N         ! argument (6)
INTEGER,INTENT(INOUT)          ::  NREF      ! argument (7)
INTEGER,INTENT(INOUT)          ::  REFDES(:) ! argument (8)
INTEGER,INTENT(INOUT)          ::  NEWREF(:) ! argument (9)
INTEGER,INTENT(OUT)            ::  IRC       ! argument (10)

INTEGER ::  FLDEL        ! to flag descriptor for deletion
INTEGER ::  KINC         ! increment width (must be 0 for refval)
! INTEGER ::  VALUE        ! function to get value from data section

DATA FLDEL/1073741824/   ! 2**30 (descriptor deletion flag)

IRC = 0

! Y=0 cancels any reference value changes, so empty the list.

IF (Y == 0) THEN
  NREF=0
ELSE

! Y>0 means Y-bit values correspond to the following element descriptors
! Flag the element descriptors for deletion after listing them.

  DO WHILE (DESCR(N) < 64*256)     ! element descriptor, F=0
    NREF=NREF+1
    REFDES(NREF)=DESCR(N)
    IF (MOD(DESCR(N)/FLDEL,2) /= 1) DESCR(N)=DESCR(N)+FLDEL

! List the values (reference values) corresponding to the descriptors.
! (Top bit set means negative value.)

    NEWREF(NREF)=VALUE(STRING,IBEFOR,Y)
    IF (NEWREF(NREF) > 2**(Y-1)-1) THEN
      NEWREF(NREF)=2**(Y-1)-NEWREF(NREF)
    END IF

! Not obvious how to cope if new values vary from report to report,
! so error.  (Get 6-bit increment width: error if it's nonzero.)

    IF (CMPRES) THEN
      KINC=VALUE(STRING,IBEFOR,6)
      IF (KINC /= 0) THEN
        PRINT  *,' New reference values vary from report to report', &
                 ' in compressed data'
        IRC=123
        RETURN
      END IF
    END IF

    N=N+1
  END DO

! End of redefinition must be marked by descriptor 203255 (delete it).
! If the next non-element descriptor is not 203255, there's an error.
!
  IF (DESCR(N) == 2*16384+3*256+255) THEN
    IF (MOD(DESCR(N)/FLDEL,2)/=1) DESCR(N)=DESCR(N)+FLDEL
    N=N+1
  ELSE
    PRINT *,' Nothing to mark end of new reference values'
    IRC=203
  END IF
END IF
RETURN
END SUBROUTINE BUFD203
