SUBROUTINE MTRSHE(REPLEN,REPORT,POINT,GRPLEN,LREPFL,BADGRP,RWYUSE, &
                  RWYDIR,RWYPRL)

!-----------------------------------------------------------------------
!
! PROGRAM       : MTRSHE
!
! PURPOSE       : THIS IS A SPECIAL SUBROUTINE TO LOCATE THE
!                 SUBGROUPS WHICH MAKE UP A WINDSHEAR GROUP
!                 AND RETURN THE TOTAL LENGTH TO ALLOW THE
!                 WINDSHEAR GROUP TO BE TREATED AS A WHOLE BY THE
!                 MAIN EXPANSION ROUTINE, LIKE THE REST OF THE GROUPS.
!                 ALL OTHER CODE GROUPS ARE SEPARATE, WINDSHEAR IS
!                 UNIQUE.
!
! CALLED BY     : MTREXP
!
! ARGUMENTS     : (1)  REPLEN  REPORT LENGTH            (I)
!                 (2)  REPORT  REPORT                   (I)
!                 (3)  POINT   START POSITION OF GROUP  (I/O)
!                 (4)  GRPLEN  GROUP LENGTH             (O)
!                 (5)  LREPFL  END OF REPORT FLAG       (O)
!                 (6)  BADGRP  BAD WINDSHEAR GROUP FLAG (O)
!                 (7)  RWYUSE  DEFINES RUNWAYS AFFECTED (O)
!                 (8)  RWYDIR  DEFINES RUNWAY DIRECTION (O)
!                 (9)  RWYPRL  DEFINES PARALLEL RUNWAYS (O)
!
!Y2K  16.06.1997  MTREXP is Year 2000 compliant.
!
! REVISION INFO :
!
!
! $Workfile: mtrshe.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 12/11/2010 17:13:53$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  3    MetDB_Refresh 1.2         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  2    MetDB_Refresh 1.1         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  1    MetDB_Refresh 1.0         21/10/2010 16:38:08    Rosemary Lavery
!       Initial port
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

IMPLICIT NONE

! Interface Arguments

INTEGER,           INTENT(IN)    ::  REPLEN   ! Overall Metar report length
CHARACTER (LEN=*), INTENT(IN)    ::  REPORT   ! Complete Metar report
INTEGER,           INTENT(INOUT) ::  POINT    ! Current 'point' within report
INTEGER,           INTENT(OUT)   ::  GRPLEN   ! Exact length of wind shear group
LOGICAL,           INTENT(OUT)   ::  LREPFL   ! Set if POINT is beyond the report length
LOGICAL,           INTENT(OUT)   ::  BADGRP   ! Flag to bypass sections of code if
                                              ! wind shear group contains syntax errors
INTEGER,           INTENT(OUT)   ::  RWYUSE   ! Expansion value to define runway(s) affected
INTEGER,           INTENT(OUT)   ::  RWYDIR   ! Expansion value to define runway direction
INTEGER,           INTENT(OUT)   ::  RWYPRL   ! Expansion value to define parallel runways

! Local Scalars

INTEGER              ::  OFFSET   ! Position after last characte        r in wind shear group
LOGICAL              ::  PRL      ! TRUE to check for parallel runways

! Initialise variables

BADGRP=.FALSE.
LREPFL=.FALSE.
GRPLEN=0
OFFSET=0
PRL=.FALSE.
RWYUSE=-9999999
RWYDIR=-9999999
RWYPRL=-9999999

! If group is a take-off runway report, then increment OFFSET value,
! set runway use value, expand runway direction data (if available) and
! set flag to check for parallel runways.

IF_RWYTYPE: &
IF (REPORT(POINT+3:POINT+10) == 'TKOF RWY') THEN
  RWYUSE=1
  IF ((POINT+12) <= REPLEN .AND.               &
      REPORT(POINT+11:POINT+11) >= '0' .AND. &
      REPORT(POINT+11:POINT+11) <= '9' .AND. &
      REPORT(POINT+12:POINT+12) >= '0' .AND. &
      REPORT(POINT+12:POINT+12) <= '9') THEN
    READ(REPORT(POINT+11:POINT+12),'(I2)') RWYDIR
    OFFSET=13
    PRL=.TRUE.
  ELSE
    OFFSET=10
  END IF

! If group is a landing runway report, then increment OFFSET value,
! set runway use value, expand runway direction data (if available) and
! set flag to check for parallel runways.

ELSE IF (REPORT(POINT+3:POINT+9) == 'LDG RWY') THEN
  RWYUSE=2
  IF ((POINT+11) <= REPLEN .AND.               &
      REPORT(POINT+10:POINT+10) >= '0' .AND. &
      REPORT(POINT+10:POINT+10) <= '9' .AND. &
      REPORT(POINT+11:POINT+11) >= '0' .AND. &
      REPORT(POINT+11:POINT+11) <= '9') THEN
    READ(REPORT(POINT+10:POINT+11),'(I2)') RWYDIR
    OFFSET=12
    PRL=.TRUE.
  ELSE
    OFFSET=9
  END IF

! If group is an indesignated runway use group, then increment the
! OFFSET value, expand the runway direction data (if available) and
! set the flag to check for parallel runways.

ELSE IF (REPORT(POINT+3:POINT+5) == 'RWY') THEN
  IF ((POINT+7) <= REPLEN .AND.            &
      REPORT(POINT+6:POINT+6) >= '0' .AND. &
      REPORT(POINT+6:POINT+6) <= '9' .AND. &
      REPORT(POINT+7:POINT+7) >= '0' .AND. &
      REPORT(POINT+7:POINT+7) <= '9') THEN
    READ(REPORT(POINT+6:POINT+7),'(I2)') RWYDIR
    OFFSET=8
    PRL=.TRUE.
  ELSE
    OFFSET=5
  END IF

! If the group is an 'all runways' report, then set the OFFSET value
! and the parallel runways value.

ELSE IF (REPORT(POINT+3:POINT+9) == 'ALL RWY') THEN
  OFFSET=10
  RWYPRL=5

! If the group content did not satisfy any of the above conditions,
! it is deemed to be incorrectly formatted and no expansion is done.
! Set the offset value to skip the 'WS' in the report, preventing an
! infinite loop.

ELSE
  BADGRP=.TRUE.
  OFFSET=2
END IF IF_RWYTYPE

! Set the OFFSET value to the correct position within the report
! and the overall group length. Set POINT to the end of the group.

OFFSET=POINT+OFFSET
POINT=POINT+OFFSET
GRPLEN=OFFSET-1

! Check for parallel runways (not all aerodromes will have one runway)
! provided the flag was set.
! This section will not be processed should the wind shear report be
! corrupt because the flag PRL will never be true.

IF_PARA: &
IF (PRL) THEN
  IF (REPORT(OFFSET:OFFSET+1) == 'RR') THEN
    OFFSET=OFFSET+3
    RWYPRL=4
  ELSE IF (REPORT(OFFSET:OFFSET+1) == 'LL') THEN
    OFFSET=OFFSET+3
    RWYPRL=0
  ELSE IF (REPORT(OFFSET:OFFSET) == 'R') THEN
    OFFSET=OFFSET+2
    RWYPRL=3
  ELSE IF (REPORT(OFFSET:OFFSET) == 'L') THEN
    OFFSET=OFFSET+2
    RWYPRL=1
  ELSE IF (REPORT(OFFSET:OFFSET) == 'C') THEN
    OFFSET=OFFSET+2
    RWYPRL=2
  ELSE
    OFFSET=OFFSET+1
  END IF
END IF IF_PARA


RETURN
END SUBROUTINE MTRSHE
