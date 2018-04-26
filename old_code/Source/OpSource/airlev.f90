SUBROUTINE AIRLEV(REPLEN,REPORT,POINT,LEVEL,DECERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRLEV
!
! PURPOSE       : TO DECODE THE FLIGHT LEVEL IN AN AIREP REPORT
!
! DESCRIPTION   : THE PROGRAM LOOKS FOR A PARTICULAT PATTERN WHICH
!                 IS EXPECTED FOR THE FLIGHT LEVEL. WHEN A MATCH IS
!                 MADE WITH ONE OF THE PATTERNS THE DECODE ROUTINE
!                 EXTRACTS THE FLIGHT LEVEL AND CONVERTS IT FROM FEET
!                 TO METERS.
!
! CALLED BY     : AIRARP
!
! CALL TO       : AIRLOC
!                 AIRGRP
!
! PARAMETERS    : 1. REPLEN - LENGTH OF REPORT - I
!                 2. REPORT -REPORT BEING EXPANDED -I
!                 3. POINT - POINTER WITHIN REPORT - I/O
!                 4. LEVEL - AIRCRAFT FLIGH LEVEL IN METERS - O
!                 5. DECERR - DECODE STATUS FLAG - O
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
USE airgrp_mod
USE airloc_mod
!
IMPLICIT NONE

! Parameters
REAL,INTENT(OUT)              :: LEVEL  !expanded flight level
INTEGER,INTENT(OUT)           :: DECERR !decode error flag
INTEGER,INTENT(INOUT)         :: POINT  !position within group in
INTEGER,INTENT(IN)            :: REPLEN !report length
CHARACTER(LEN=*),INTENT(IN)   :: REPORT !size passed on from airarp
!
!declare characters
CHARACTER(LEN=20)             :: CHARR  !dummy array
!
!declare integers
INTEGER                :: GRPLEN  !length of group within report
INTEGER                :: NCHAR   !no. of non-numerics in group
INTEGER                :: FLEVEL !flight level raw non-converted

!declare logical
LOGICAL                :: LREPFL !indicates end of report if = .true.

SAVE
!declare revision information
!initialise variables
GRPLEN=0                 !set initial grouplength =0
LEVEL=-9999999.
FLEVEL=-9999999.
CHARR=' '
LREPFL=.FALSE.

!----------------------------------------------------------------------
!This has three main formats; F xxx, Fxxx and xxx. Where F is an
!indicator and xxx represents the actual height. The flight level
!is reported by aircraft in Feet and for our purposes this is converted
!into meters.
!----------------------------------------------------------------------

CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
CALL AIRGRP(REPORT(POINT-GRPLEN-1:),&
     GRPLEN,NCHAR,CHARR)

!----------------------------------------------------------------------
!Check to see if this is the last group
!----------------------------------------------------------------------

IF (LREPFL) THEN
  DECERR=1
END IF

!----------------------------------------------------------------------
!Format is F_xxx. So 'F' found as a group on its own. We need to
!look to the next group to get the flight level.It will then be
!a group of three numbers with no non-numeric characters.
!----------------------------------------------------------------------

  IF_CONSTR1 : &
  IF ((GRPLEN  ==  1) .AND. (NCHAR  ==  1) .AND. (DECERR  ==  0)) &
      THEN
    CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
    CALL AIRGRP(REPORT(POINT-GRPLEN-1:1),&
        GRPLEN,NCHAR,CHARR)

    IF (NCHAR  ==  0) THEN
      READ(REPORT(POINT-GRPLEN-1:POINT-GRPLEN+1),'(I3)')FLEVEL
      LEVEL=FLEVEL*0.3048*100
    END IF

!---------------------------------------------------------------------
!Format of group is 'no F'.Check group starts with numbers
!---------------------------------------------------------------------

  ELSE IF ((GRPLEN ==  3) .AND. (NCHAR  ==  0)) THEN
    READ(REPORT(POINT-GRPLEN-1:POINT-2),'(I3)')FLEVEL
      LEVEL=FLEVEL*0.3048*100

!---------------------------------------------------------------------
!Format of group is Fxxx. Check first character is non-numeric
!---------------------------------------------------------------------

  ELSE IF ((GRPLEN ==  4) .AND. (CHARR(1:1)  ==  'Y') .AND.&
      (NCHAR  ==  1)) THEN
    READ(REPORT(POINT-GRPLEN:POINT-2),'(I3)')FLEVEL
      LEVEL=FLEVEL*0.3048*100
  END IF IF_CONSTR1

!---------------------------------------------------------------------
!This section checks the flight level is within limits
!---------------------------------------------------------------------

IF ((FLEVEL  >  600) .OR. (FLEVEL   <  001)) THEN
  LEVEL=-9999999.
  DECERR=1
END IF

RETURN                  !return to main program
END SUBROUTINE AIRLEV
