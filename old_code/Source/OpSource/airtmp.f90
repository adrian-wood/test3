SUBROUTINE AIRTMP(REPLEN,REPORT,POINT,GRPLEN,CHARR,TEMP)
!
!
!---------------------------------------------------------------------
!
! PROGRAM       : AIRTMP
!
! PURPOSE       : TO DECODE THE TEMPERATURE GROUP IN AN AIREP REPORT
!
! DESCRIPTION   : THE TEMPERATURE GROUP IS DECODED BY USING A FIXED
!                 'PATTERN' TO CHECK AGAINST.
!
! CALLED BY     : AIRELM
!
! CALLS TO      : AIRLOC , AIRGRP
!
! PARAMETERS    : 1. REPLEN -LENGTH OF REPORT -I
!                 2. REPORT- REPORT BEING EXPANDED - I
!                 3. POINT -POINTER TO POSITION WITHIN REPORT -I/O
!                 4. GRPLEN - LENGTH OF GROUP BEING DECODED - I/O
!                 5. CHARR - 'PATTERN' OF Y'S AND N'S FOR GROUP-O
!                 4. TEMP -AIR TEMPERATURE DECODE -O
!
! REVISION INFO:
!
! $Workfile: airtmp.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 26/01/2011 16:49:26$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         26/01/2011 16:49:26    Brian Barwell   CHARR
!       intent changed to INOUT.
!  4    MetDB_Refresh 1.3         25/01/2011 16:58:02    Richard Weedon  CHARR
!       intent set to OUT
!  3    MetDB_Refresh 1.2         17/01/2011 16:15:23    Richard Weedon
!       Updated parameter call
!  2    MetDB_Refresh 1.1         17/01/2011 10:47:58    Richard Weedon
!       Updated for USE statements, airloc_mod & airgrp_mpd added
!  1    MetDB_Refresh 1.0         13/01/2011 16:08:10    Richard Weedon
!       Initial version, passed basic compilation test
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
! INTERFACE
USE airloc_mod
USE airgrp_mod
!
IMPLICIT NONE
!
! Parameters
INTEGER,INTENT(IN)              ::  REPLEN   !Len of airep report decoded
CHARACTER(LEN=*),INTENT(IN)     ::  REPORT   !Airep report
INTEGER,INTENT(INOUT)           ::  POINT    !Position within report
INTEGER,INTENT(INOUT)           ::  GRPLEN   !Length of group decoded
CHARACTER(LEN=20),INTENT(INOUT) ::  CHARR    !Dummy string for group
REAL,INTENT(OUT)                ::  TEMP     !Decoded temperature
!
!
!declare integers
INTEGER                      ::  NCHAR !No of non-numerics within gp
INTEGER                      ::  TEMP1 !Raw temperature value

!declare characters
CHARACTER(LEN=2)             ::  TEMP_SIGN*2 !Sign of AIR temperature

!declare logical
LOGICAL                      ::  LREPFL   !Indicates end of rep if T
LOGICAL                      ::  END_REP  !Indicates '=' found
LOGICAL                      ::  TEMP_OK

!initialise variables

TEMP=-9999999.
END_REP=.FALSE.
TEMP_OK=.TRUE.
TEMP1=-9999999
TEMP_SIGN(:)=' '

!----------------------------------------------------------------------
!the next section looks at the temperature group. Using known pattern
!to detect the group
!----------------------------------------------------------------------

!   if temperature group is the final group then the pointer points
!   to the end final '=' sign and not the beginning of the next
!   report as usual
!
IF (REPORT(POINT:POINT) == '=') POINT=POINT+2
!
if_constr1 : &
IF ((GRPLEN  ==  3) .AND. (CHARR(1:3)  ==  'YNN')) THEN
  READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1
  TEMP_SIGN(1:1)=REPORT(POINT-4:POINT-4)
ELSE IF ((GRPLEN ==  4) .AND. (CHARR(1:4)  ==  'YYNN')) THEN
  READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1
  TEMP_SIGN(1:2)=REPORT(POINT-5:POINT-4)
ELSE IF ((GRPLEN ==  2) .AND. (CHARR(1:2)  ==  'YN')) THEN
  READ(REPORT(POINT-2:POINT-2),'(I1)')TEMP1
  TEMP_SIGN(1:1)=REPORT(POINT-3:POINT-3)
ELSE IF ((GRPLEN ==  3) .AND. (CHARR(1:3)  ==  'YYN'))THEN
  READ(REPORT(POINT-2:POINT-2),'(I1)')TEMP1
  TEMP_SIGN(1:2)=REPORT(POINT-4:POINT-3)
ELSE IF ((GRPLEN ==  3) .AND. (CHARR(1:3)  ==  'YNY'))THEN
  READ(REPORT(POINT-3:POINT-3),'(I1)')TEMP1
  TEMP_SIGN(1:1)=REPORT(POINT-4:POINT-4)
ELSE IF ((GRPLEN ==  4) .AND. (CHARR(1:4)  ==  'YYNY'))THEN
  READ(REPORT(POINT-3:POINT-3),'(I1)')TEMP1
  TEMP_SIGN(1:2)=REPORT(POINT-5:POINT-4)
ELSE IF ((GRPLEN ==  5) .AND. (CHARR(1:5)  ==  'YYNNY'))THEN
  READ(REPORT(POINT-4:POINT-3),'(I2)')TEMP1
  TEMP_SIGN(1:2)=REPORT(POINT-6:POINT-5)
ELSE IF ((GRPLEN ==  4) .AND. (CHARR(1:4)  ==  'YNNY'))THEN
  READ(REPORT(POINT-4:POINT-3),'(I2)')TEMP1
  TEMP_SIGN(1:1)=REPORT(POINT-5:POINT-5)
ELSE IF ((GRPLEN ==  2) .AND. (CHARR(1:2)  ==  'YY')) THEN
  TEMP_SIGN(1:2)=REPORT(POINT-3:POINT-2)
  CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
  CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,CHARR)
  IF ((GRPLEN  ==  2) .AND. (CHARR(1:2)  ==  'NN')) THEN
    READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1
  END IF
ELSE IF ((GRPLEN ==  1) .AND. (CHARR(1:1)  ==  'Y')) THEN
  TEMP_SIGN(1:1)=REPORT(POINT-2:POINT-2)
  CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
  CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-2),GRPLEN,NCHAR,CHARR)
  IF ((GRPLEN  ==  2) .AND. (CHARR(1:2)  ==  'NN')) THEN
    READ(REPORT(POINT-3:POINT-2),'(I2)')TEMP1
  END IF
!----------------------------------------------------------------------
!a missing temperature group is usually reported as '///'.
!----------------------------------------------------------------------

ELSE IF (((GRPLEN ==  3) .AND.&
     (CHARR(:GRPLEN)  ==  'YYY')) .OR. ((CHARR(:GRPLEN)&
      ==  'YYYY') .AND. (GRPLEN  ==  4))) THEN
  TEMP=-9999999.
  TEMP_OK=.FALSE.
END IF if_constr1
!
! if have just processed final group then reset the pointer
!

IF (REPORT(POINT-2:POINT-2) == '=') POINT=POINT-2
!
!----------------------------------------------------------------------
!now that the temperature group has been found it needs to be
!converted from degrees C to Kelvin. A check for the sign of the
!temperature is done first and then the temperature is converted by
!adding 273.
!----------------------------------------------------------------------

IF (TEMP_OK) THEN
  IF ((TEMP_SIGN(1:1)  ==  'P') .OR. (TEMP_SIGN(1:2)  ==  'PS'))&
       THEN
    TEMP=FLOAT(TEMP1)+273.
 ELSE IF ((TEMP_SIGN(1:1) ==  'M').OR.(TEMP_SIGN(1:2)  ==  'MS'))&
      THEN
    TEMP=(FLOAT(TEMP1)*(-1))+273.
  ELSE
    TEMP=9999999.
  END IF
END IF
RETURN
END SUBROUTINE AIRTMP
