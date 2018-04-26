SUBROUTINE BULLED(ISTART,IEND,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : BULLED
!
! PURPOSE       : TO TIDY UP BULLETIN AFTER HEADER INFORMATION FOUND.
!
! DESCRIPTION   : FIGURE, LETTER, SPACE, SLASH, PLUS, EQUALS, PLUS
!                 AND MINUS ARE ACCEPTABLE CHARACTERS.
!                 IF TWO SUCCESSIVE CHARACTERS ARE SPACE OR LESS,
!                 REDUCE THEM TO A SINGLE SPACE.  REPLACE ANY OTHER
!                 UNEXPECTED CHARACTER BY A SLASH.
!
! CALLED BY     : BULHED,BTHBUL,CLMBUL,BUOY,NCMBUL,SRWBUL,STMBUL,
!                 TESBUL
!
! CALLS         : NOTHING
!
! ARGUMENTS     : (1) ISTART   STARTING POINT IN BULLETIN
!                 (2) IEND     END POINT (RESET IF SPACES MERGED)
!                 (3) BULL     REPORT DATA
!
! REVISION INFO :
!
! $Workfile: bulled.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 16/03/2011 12:22:26$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         16/03/2011 12:22:26    Alison Weir     Amend
!       if statements to prevent substring out of range errors
!  2    MetDB_Refresh 1.1         16/12/2010 17:09:14    John Norton
!       Updated after rework identified by review of MDBSTOR batch 3 done.
!  1    MetDB_Refresh 1.0         07/12/2010 15:31:04    John Norton
!       MDBSTOR batch 3 code ready for review
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

! Use statements:
! <Interfaces>

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(IN)    :: ISTART !a1
INTEGER,          INTENT(INOUT) :: IEND !a2
CHARACTER(LEN=*), INTENT(INOUT) :: BULL !a3

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

CHARACTER(LEN=1) ::  CH
INTEGER          ::  IN
INTEGER          ::  N

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! TWO POINTERS TO THE SAME STRING ARE USED, IN & N (INPUT & OUTPUT).
! N IS NOT ADVANCED WHEN A CHARACTER IS SKIPPED, SO CAN BE LESS THAN IN.
!-----------------------------------------------------------------------

N=ISTART

DOLABEL1: &
DO IN=ISTART,IEND

IFLABEL1: &
  IF (IN < IEND) THEN
    IF (BULL(IN:IN) <= ' ' .AND. BULL(IN+1:IN+1) <= ' ') THEN
      BULL(IN+1:IN+1)=' '
      CYCLE
    ELSE IF (BULL(IN:IN) <= ' ' .AND. BULL(IN+1:IN+1) == '=') THEN
      CYCLE
    END IF
  END IF IFLABEL1

  CH=BULL(IN:IN)
IFLABEL2: &
  IF (CH == ' ' .OR. CH == '/' .OR. CH == '=' &
            .OR. CH == '+' .OR. CH == '-' &
            .OR. (CH >= 'A'.AND.CH <= 'I') &
            .OR. (CH >= 'J'.AND.CH <= 'R') &
            .OR. (CH >= 'S'.AND.CH <= 'Z') &
            .OR. (CH >= '0'.AND.CH <= '9')) THEN
    BULL(N:N)=CH

!-----------------------------------------------------------------------
! If the character < ' ', e.g. single LF, substitute it with a ' '
!-----------------------------------------------------------------------

  ELSE IF (CH < ' ') THEN
    BULL(N:N)=' '

  ELSE
    BULL(N:N)='/'
  END IF IFLABEL2

  N=N+1
END DO DOLABEL1

IEND=N

RETURN
END SUBROUTINE BULLED
