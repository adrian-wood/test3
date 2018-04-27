SUBROUTINE AIRGRP(GROUP,LENGTH,NCHAR,CHARR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRGRP
!
! PURPOSE       : TO LOCATE NON-FIGURES IN A GROUP LOCATED BY AIRLOC &
!                 TO RETURN THEIR POSITION AND TOTAL NUMBER
!
! DESCRIPTION   : EACH BYTE OF THE GROUP IS CHECKED IN TURN. IF THE
!                 BYTE CONTAINS A FIGURE THEN CHARR='N'. IF THE BYTE
!                 CONTAINS ANYTHING ELSE, THEN CHARR='Y'. NCHAR GIVES
!                 THE TOTAL NUMBER OF CHARACTERS
!
! CALLED BY     : AIRELM (& others?)
!
! CALLS TO      : NONE
!
! PARAMETERS    : (1) group to be analysed                         (i)
!                 (2) length of group                              (i)
!                 (3) number of non-figures found                  (o)
!                 (4) string set to N for figure & Y otherwise     (o)
!
! REVISION INFO:
!
! $Workfile: airgrp.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 12/01/2011 14:56:34$
!
! CHANGE RECORD:
!
! $Log:
!  1    MetDB_Refresh 1.0         12/01/2011 14:56:34    Richard Weedon
!       initial draft. Passed basic compilation test
! $
!
!
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
!
IMPLICIT NONE
!
! Parameters

INTEGER,INTENT(IN)            ::   LENGTH    !len of gp being investig'd
INTEGER,INTENT(OUT)           ::   NCHAR     !tot no of non-digit chars
CHARACTER(LEN=*),INTENT(IN)   ::   GROUP*(*) !group being checked
CHARACTER(LEN=*),INTENT(OUT)  ::   CHARR     !group rep as 'N' or 'Y' s.
!
! Declare variables
INTEGER                       ::   CHARCHK   !no of byte being checked
INTEGER                       ::   I         !loop counter
CHARACTER(LEN=1)              ::   CH
!
! Initialize variables
!
NCHAR=0
CHARCHK=0
!
! Set default value of CHARR to 'N' (figure)
DO I=1,MIN(LENGTH,LEN(CHARR))
  CHARR(I:I)='N'
END DO

! Check each byte and if it contains a non-figure change CHARR to 'Y'
! and increment NCHAR. Otherwise leave CHARR and NCHAR as they are.
DO CHARCHK=1,MIN(LENGTH,LEN(CHARR))
  CH=GROUP(CHARCHK:CHARCHK)
  IF (.NOT.(CH  >=  '0' .AND. CH  <=  '9')) THEN
    CHARR(CHARCHK:CHARCHK)='Y'
    NCHAR=NCHAR+1
  END IF
END DO

RETURN
END SUBROUTINE AIRGRP
