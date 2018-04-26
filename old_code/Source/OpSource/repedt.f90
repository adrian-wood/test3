SUBROUTINE REPEDT(BULL,REPORT,REPLEN)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : REPEDT
!
! PURPOSE       : TO TIDY UP A SINGLE REPORT  (NOT EDITING IN PLACE)
!
! DESCRIPTION   : FIGURE, LETTER, SPACE, SLASH, EQUALS, PLUS
!                 AND MINUS ARE ACCEPTABLE CHARACTERS.
!                 IF TWO SUCCESSIVE CHARACTERS ARE SPACE OR LESS,
!                 REDUCE THEM TO A SINGLE SPACE.  REPLACE ANY OTHER
!                 UNEXPECTED CHARACTER BY A SLASH.
!
! CALLED BY     : SYNBUL
!
! ARGUMENTS     : (1) BULL     REPORT IN BULLETIN           (I/O)
!                 (2) REPORT   EDITED REPORT                (O)
!                 (3) REPLEN   REPORT LENGTH (MAY BE RESET) (I/O)
!
! REVISION INFO :
!
! $Workfile: repedt.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 11/01/2011 11:50:59$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         11/01/2011 11:50:59    Alison Weir
!       Changes following review, including change to IF block to avoid
!       possible out-of-range errors
!  3    MetDB_Refresh 1.2         21/12/2010 16:56:07    Alison Weir
!       Correct BULL intent
!  2    MetDB_Refresh 1.1         21/12/2010 14:47:50    Alison Weir     Ported
!        to F95
!  1    MetDB_Refresh 1.0         21/12/2010 13:36:24    Alison Weir
!       Initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
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

CHARACTER(LEN=*), INTENT(INOUT) :: BULL   !A01
CHARACTER(LEN=*), INTENT(OUT)   :: REPORT !A02
INTEGER,          INTENT(INOUT) :: REPLEN !A03

! Local declarations:

CHARACTER(LEN=1) ::  CH
INTEGER          ::  IN
INTEGER          ::  N

!
REPORT=' '
N=0                    ! POINTER TO OUTPUT REPORT
!
DOLABEL1: &
DO IN=1,REPLEN
IFLABEL1: &
  IF (IN < REPLEN) THEN
    IF (BULL(IN:IN) <= ' ' .AND. BULL(IN+1:IN+1) <= ' ') THEN
      BULL(IN+1:IN+1)=' '
      CYCLE
    ELSE IF (IN < REPLEN .AND. &
      BULL(IN:IN) <= ' ' .AND. BULL(IN+1:IN+1) == '=') THEN
      CYCLE
    END IF
  END IF IFLABEL1
  CH=BULL(IN:IN)
  N=N+1
  IF (CH == ' ' .OR.                 &
      CH == '/' .OR.                 &
      CH == '=' .OR.                 &
      CH == '+' .OR.                 &
      CH == '-' .OR.                 &
     (CH >= 'A'.AND.CH <= 'I') .OR.  &
     (CH >= 'J'.AND.CH <= 'R') .OR.  &
     (CH >= 'S'.AND.CH <= 'Z') .OR.  &
     (CH >= '0'.AND.CH <= '9')) THEN
    REPORT(N:N)=CH
  ELSE
    REPORT(N:N)='/'
  END IF
END DO DOLABEL1
!
REPLEN=N
RETURN
END SUBROUTINE REPEDT
