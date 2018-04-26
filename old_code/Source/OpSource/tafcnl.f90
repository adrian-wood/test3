SUBROUTINE TAFCNL(REPORT,REPLEN,C_RC)
!-----------------------------------------------------------------------
!
!   PROGRAM     : TAFCNL
!
!   PURPOSE     : This routine having been activated by the presence of
!                 Amendment indicator will check for the presence of a
!                 cancellation flag. This is normally at the end of the
!                 bulletin.
!
!   DESCRIPTION : Groups delimited by spaces are found by MTRLOC.
!
!  CALLED BY    : TAFEXP
!
!  CALLS        : MTRLOC  to delimit a group
!
!  ARGUMENTS    : 1 REPORT - TAF REPORT TO BE DECODED          (input)
!                 2 REPLEN - REPORT LENGTH                     (input)
!                 3 C_RC   - RETURN CODE                      (output)
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 18/11/2010 14:18:13$
! $Author: Sheila Needham$
! $Folder: OpSource$
! $Workfile: tafcnl.f90$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         18/11/2010 14:18:13    Sheila Needham  Add
!       USE stmts
!  1    MetDB_Refresh 1.0         04/11/2010 13:25:18    John Norton     MetDB
!       Refresh batch 8 before reviewing.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE mtrloc_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)    ::  REPORT ! Report
INTEGER,      INTENT(IN)    ::  REPLEN ! Report Length
INTEGER,      INTENT(OUT)   ::  C_RC ! Return code 1 = TAF cancelled
                                     ! 2= TAF VALID

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER       ::  IN     ! pointer to group position
INTEGER       ::  IN_OLD
INTEGER       ::  GRPLEN ! Group length

CHARACTER(20) ::  GROUP ! Group

LOGICAL       ::  LREPFL

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!

LREPFL=.FALSE.
C_RC=0
IN=1

CALL MTRLOC(REPLEN,REPORT,IN,GRPLEN,LREPFL)
GROUP=REPORT(IN:IN+GRPLEN-1)

DOLABEL1: &
DO WHILE (.NOT.LREPFL)
  IN_OLD=IN
  CALL MTRLOC(REPLEN,REPORT,IN,GRPLEN,LREPFL)
  GROUP=REPORT(IN_OLD:IN_OLD+GRPLEN-1)

  IF (GROUP(1:4) == 'CNCL') THEN
    C_RC=1
  ELSE IF (GROUP(1:3) == 'CNL') THEN
    C_RC=1
  ELSE IF (GROUP(1:2) == 'CL') THEN
    C_RC=1
  END IF

END DO DOLABEL1

RETURN
END SUBROUTINE TAFCNL
