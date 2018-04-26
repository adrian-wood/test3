SUBROUTINE AIRSGN(REPORT,REPLEN,POINT,SIGN)
!
!-----------------------------------------------------------------------
!
! PROGRAM       : AIRSGN
!
! PURPOSE       : to find the callsign in an AIREP report
!
! DESCRIPTION   : Accept 3 or more characters starting with a letter as
!                 an identifier, once ARP & any COR or RTD are removed.
!
! CALLED BY     : AIRARP
!
! CALLS TO      : AMDCOR, AIRLOC
!
! PARAMETERS    : 1. REPORT                                    (I/O)
!                 2. REPLEN  length of REPORT                  (I/O
!                 3. POINT   pointer to REPORT                 (I/O)
!                         (to group after call sign on output)
!                 4. SIGN    call sign                          (O)
!
! REVISION INFO:
!
! $Workfile: airsgn.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 13/01/2011 12:02:22$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         13/01/2011 12:02:22    Richard Weedon  module
!        state,emnts added. Intents to REPLEN & REPORT changed to INOUT
!  1    MetDB_Refresh 1.0         13/01/2011 11:23:43    Richard Weedon
!       initial version, passes basic compilation test
! $
!
!
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
!
! Modules
USE amdcor_mod
USE airloc_mod
!
!
IMPLICIT NONE
!
! Parameters
CHARACTER(LEN=*),INTENT(INOUT)    ::      REPORT
INTEGER,INTENT(INOUT)             ::      REPLEN    ! Report length
INTEGER,INTENT(INOUT)             ::      POINT     ! Position in AIREP
CHARACTER(LEN=*),INTENT(OUT)      ::      SIGN      ! Aircraft call sign
!
INTEGER                 ::  START  !Points to first gp (for final print)
INTEGER                 ::  GRPLEN ! Length of group in report
!
CHARACTER               ::  CORNUM ! For AMDCOR call - not used
CHARACTER               ::  AMDNUM ! For AMDCOR call - not used
!
LOGICAL                 ::  OAMD   ! For AMDCOR call - not used
LOGICAL                 ::  OCOR   ! For AMDCOR call - not used
LOGICAL                 ::  LREPFL ! Set if end of report
SAVE
!
!
DO WHILE (REPORT(POINT:POINT) == ' ')
  POINT=POINT+1
END DO
START=POINT

! Skip (A)IREP or ARP at start of report

IF (REPORT(POINT:POINT+3) == 'ARP ')   POINT=POINT+4
IF (REPORT(POINT:POINT+3) == 'ARS ')   POINT=POINT+4
IF (REPORT(POINT:POINT+5) == 'AIREP ') POINT=POINT+6
IF (REPORT(POINT:POINT+4) == 'IREP ')  POINT=POINT+5

! Delete any COR or RTD groups which follow AIREP or ARP

CALL AMDCOR(POINT,REPLEN,OAMD,AMDNUM,OCOR,CORNUM,REPORT)

! Take next group as call sign if it has 3 or more characters
! starting with a letter.
! If not, return dummy call sign & point back to unrecognised group.

CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)

SIGN=REPORT(POINT-GRPLEN-1:POINT-2)
IF (GRPLEN < 3 .OR. SIGN(1:1) < 'A' .OR. SIGN(1:1) > 'Z') THEN
  SIGN='AIREP'
  PRINT *,'AIRSGN: no call sign found ',&
     REPORT(START:MIN(START+50,REPLEN))
  POINT=POINT-GRPLEN-1
END IF
RETURN
END SUBROUTINE AIRSGN
