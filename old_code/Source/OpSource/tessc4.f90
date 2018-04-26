SUBROUTINE TESSC4(REPORT,POS,EXPARR,ARRPOS3,ENDVALS)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC4
!
! PURPOSE       : To expand section 4 of TESAC (sea depth)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! ARGUMENTS     : REPORT   character string                        (I)
!                 POS      pointer to report                      (I/O)
!                 EXPARR   value array                            (I/O)
!                 ARRPOS3  array subscript: end of current profile (I)
!                 ENDVALS  sea depth (section 2) & instrumentation(I/O)
!
! REVISION INFO :
!
! $Workfile: tessc4.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 19/01/2011 10:25:53$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
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
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    ::  REPORT       !a01
INTEGER,          INTENT(INOUT) ::  POS          !a02
REAL,             INTENT(INOUT) ::  EXPARR(0:*)  !a03
INTEGER,          INTENT(IN)    ::  ARRPOS3      !a04
REAL,             INTENT(INOUT) ::  ENDVALS(3)   !a05

! Local declarations: none

! Should be pointing to 55555 at start of section 4.  If so, skip 55555.

IFLABEL1: &
IF (POS+4 <= LEN(REPORT)) THEN
  IF (REPORT(POS:POS+4) == '55555') THEN
    POS=POS+6

! Sea depth group starts with 1.
! (This group & 00000 in section 2 should be mutually exclusive)

    IF (POS+4 <= LEN(REPORT) .AND. REPORT(POS:POS) == '1') THEN
      ENDVALS(1)=IVALUE(REPORT(POS+1:POS+4))
      POS=POS+6
    END IF
  END IF
END IF IFLABEL1

! Put sea depth (and IxIxIx & XrXr) at end of array for encoding.

EXPARR(ARRPOS3+2)=ENDVALS(1)
EXPARR(ARRPOS3+4)=ENDVALS(2)
EXPARR(ARRPOS3+6)=ENDVALS(3)

RETURN
END SUBROUTINE TESSC4
