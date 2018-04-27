SUBROUTINE AIRICE(REPORT,POINT,OPT_ICE)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRICE
!
! PURPOSE       : Decode ice group in Aireps
!
! DESCRIPTION   : Simple search for specified 'KEYWORDS' to identify
!                 the icing group. If the group is not identified
!                 then the group is skipped and the missing flag set.
!
! CALLS         : None
!
! ARGUMENTS     : (1) REPORT   - Report text                   (i)
!                 (2) POINT    - position in report           (i/o)
!                 (3) OPT_ICE  - BUFR flag value returned      (o)
! REVISION INFO :
!
! $Workfile: airice.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/01/2011 16:43:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces (none)

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN) :: REPORT    !airep report
INTEGER,INTENT(INOUT)       :: POINT     !Position within airep report
REAL,INTENT(OUT)            :: OPT_ICE   !Decoded value for icing

! Local Variables (none)

OPT_ICE = 0

!-----------------------------------------------------------------------
! The search looks for the word 'ICE' followed by another word such as
! LGT. When found the OPT_ICE variable is set to the appropiate BUFR
! code table number for icing.Since we know the size of each group here
! we are able to specify the amount which pointer should move
!-----------------------------------------------------------------------

IF (REPORT(POINT:POINT+2)  ==  'ICE') THEN
  IF (REPORT(POINT+4:POINT+6)  ==  'LGT') THEN
    OPT_ICE=1
    POINT=POINT+7
  ELSE IF (REPORT(POINT+4:POINT+6) ==  'MOD') THEN
    OPT_ICE=4
    POINT=POINT+7
  ELSE IF (REPORT(POINT+4:POINT+6) ==  'SEV') THEN
    OPT_ICE=7
    POINT=POINT+7
  END IF
ELSE IF (REPORT(POINT:POINT+3) ==  'LITE') THEN
  OPT_ICE=1
  POINT=POINT+10
ELSE IF (REPORT(POINT:POINT+2) ==  'NIL') THEN
  OPT_ICE=15
  POINT=POINT+4
END IF

!-----------------------------------------------------------------------
! Return to AIROPT
!-----------------------------------------------------------------------

RETURN
END SUBROUTINE AIRICE
