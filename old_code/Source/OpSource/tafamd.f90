SUBROUTINE TAFAMD(POINT,BEND,OAMD,AMDNUM,OCOR,CORNUM,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : TAFAMD
!
! PURPOSE       : To see if a bulletin or report is amended or
!                 corrected (by the group pointed to).
!                 (Like AMDCOR - but doesn't reset flags & numbers)
!
! DESCRIPTION   : If the next three characters are AMD or COR,
!                 or AA or CC followed by a letter, set a flag
!                 and remove the group, with any number after
!                 AMD or COR. If RTD, just delete it & number.
!
! CALLED BY     : TAFBUL
!
! CALLS         : NCHTST
!
! ARGUMENTS     : (1) POINT  pointer to group to be checked        I/O
!                 (2) BEND   pointer to end of bulletin or report  I/O
!                 (3) OAMD   set if AMD or AA. found                O
!                 (4) AMDNUM number of amendment                   I/O
!                 (5) OCOR   set if COR or CC. found                O
!                 (6) CORNUM number of correction                  I/O
!                 (7) BULL   report or bulletin                    I/O
!
! REVISION INFO :
!
! $Workfile: tafamd.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 21/03/2011 13:01:32$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         21/03/2011 13:01:32    Alison Weir     Change
!        intents of oamd and ocor to INOUT
!  2    MetDB_Refresh 1.1         20/12/2010 16:08:12    Alison Weir
!       Initialise OAMD and OCOR intent=out arguments
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
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
USE nchtst_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) ::  POINT   !A01
INTEGER,          INTENT(INOUT) ::  BEND    !A02
LOGICAL,          INTENT(INOUT) ::  OAMD    !A03
CHARACTER(LEN=*), INTENT(INOUT) ::  AMDNUM  !A04
LOGICAL,          INTENT(INOUT) ::  OCOR    !A05
CHARACTER(LEN=*), INTENT(INOUT) ::  CORNUM  !A06
CHARACTER(LEN=*), INTENT(INOUT) ::  BULL    !A07

! Local declarations:

INTEGER          ::     INUM
INTEGER          ::     NSP
INTEGER          ::     NDEL

LOGICAL          ::     OLFCR
LOGICAL          ::     ONUM
LOGICAL          ::     OSPACE

CHARACTER(LEN=3) ::     LET
CHARACTER(LEN=3) ::     FIG

! Move past any spaces before group to be checked
! (Come back here if AMD or COR has been found but not both,
!  in case deleting one has left the other in its place.)

10 NDEL=0
DO WHILE (POINT < BEND .AND. BULL(POINT:POINT) == ' ')
  POINT=POINT+1
END DO

! If 'AMD' or 'COR', set corresponding flag.  If 'RTD', just delete...

LET=BULL(POINT:POINT+2)
IFLABEL1: &
IF (LET == 'AMD' .OR. LET == 'COR' .OR. LET == 'RTD') THEN
  IF (LET == 'AMD') OAMD=.TRUE.
  IF (LET == 'COR') OCOR=.TRUE.

! See if next group (or rest of AMD/COR group) is 1 or 2 figures.

  IF (BULL(POINT+3:POINT+3) == ' ') THEN
    NSP=1
  ELSE
    NSP=0
  END IF

  FIG=BULL(POINT+3+NSP:POINT+5+NSP)
  CALL NCHTST(1,3,ONUM,INUM,OSPACE,OLFCR,FIG)

! If 1-2 figures are followed by a delimiter, set AMD/COR number.
! Also set number of characters to delete at end.

  IF ((OSPACE.OR.OLFCR) .AND. INUM == 2) THEN
    IF (LET == 'AMD') AMDNUM(2:2)=FIG(1:1)
    IF (LET == 'COR') CORNUM(2:2)=FIG(1:1)
    NDEL=3+NSP+2
  ELSE IF ((OSPACE.OR.OLFCR) .AND. INUM == 3) THEN
    IF (LET == 'AMD') AMDNUM(1:2)=FIG(1:2)
    IF (LET == 'COR') CORNUM(1:2)=FIG(1:2)
    NDEL=3+NSP+3
  ELSE
    NDEL=3+NSP
  END IF

! If there are 3 letters, AA. or CC. with the third letter in the
! range A-I, set the number corresponding to the third letter.
! (Check for <A & <0 rather than =' ' in case CRLF left...)
! If RR is followed by a letter, just delete the group.

ELSE IF (BULL(POINT+3:POINT+3) < 'A' .AND. &
         BULL(POINT+3:POINT+3) < '0' .AND. &
         LET(3:3) >= 'A') THEN IFLABEL1
  IF (LET(1:2) == 'AA') THEN
    OAMD=.TRUE.
    WRITE (AMDNUM,'(I2.2)') 1+ICHAR(LET(3:3))-ICHAR('A')
    NDEL=4
  ELSE IF (LET(1:2) == 'CC') THEN
    OCOR=.TRUE.
    WRITE (CORNUM,'(I2.2)') 1+ICHAR(LET(3:3))-ICHAR('A')
    NDEL=4
  ELSE IF (LET(1:2) == 'RR') THEN
    NDEL=4
  END IF
END IF IFLABEL1

! Delete any group recognised (including any following number)
! by moving up the rest of the report or bulletin to overwrite it.
! Then go round again to check the group that has been moved up.

IF (NDEL > 0) THEN
  BULL(POINT:BEND)=BULL(POINT+NDEL:BEND)
  BEND=BEND-NDEL
  GO TO 10
END IF
RETURN
END SUBROUTINE TAFAMD
