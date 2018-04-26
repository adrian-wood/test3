SUBROUTINE AIRTRB(REPORT,POINT,OPT_TRB)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRTRB
!
! PURPOSE       : TO DECODE THE TURBULENCE GROUP IN AN AIREP REPORT
!
! DESCRIPTION   : THE SEARCH USES 'KEYWORDS' TO IDENTIFY THE VARIOUS
!                 METHODS OF REPORTING TURBULENCE. ONCE A GROUP HAS
!                 BEEN IDENTIFIED AIRLOC IS CALLED TO MOVE THE POINTER
!                 ONTO THE NEXT GROUP AS WE DO NOT ALWAYS KNOW THE
!                 LENGTH OF THE GROUP DECODED AND THERE ARE MANY
!                 VARIATIONS IN THE LENGTHS
!
! CALLED BY     : AIROPT
!
! CALLS TO      :
!
! ARGUMENTS     : (1) REPORT - Passed to routine. Airep report
!                 (2) POINT  - Passed to routine. Positional pointer
!                 (3) OPT_TURB - Returned from routine. Decode Turb grp
!
! REVISION INFO :
!
! $Workfile: airtrb.f90$ $Folder: OpSource$
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

! Interfaces (none)

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN) :: REPORT  !character form of report
INTEGER,INTENT(INOUT)       :: POINT   !Position within report pointer
REAL,INTENT(OUT)            :: OPT_TRB !Decoded Turb. Value

! Local Variables

INTEGER TURB    !Indicates which turb group lgt/mod/sev

! Initialise

OPT_TRB=0.

!---------------------------------------------------------------------
!If the report starts 'TURB' then look st the next word for the start
!of 'L'gt, 'M'od or 'S'ev. The pointer is moved to the start of the
!group following TURB. AIRLOC is called to move the pointer to the
!next group after this. As we do not know what size it will be we
!cannot specify the position the pointer should move to.
!---------------------------------------------------------------------

iflabel1: &
IF(REPORT(POINT:POINT+3)  ==  'TURB') THEN
  IF(REPORT(POINT+5:POINT+5)  ==  'L') THEN
    TURB=1
    POINT=POINT+5
  ELSE IF(REPORT(POINT+5:POINT+5) ==  'M') THEN
    TURB=2
    POINT=POINT+5
  ELSE IF(REPORT(POINT+5:POINT+5) ==  'S') THEN
    TURB=3
    POINT=POINT+5
  END IF

!---------------------------------------------------------------------
!This search looks at groups that start 'TB'. This is followed by
!'CODE' and then followed by a number . The following list shows what
!the TURB codes stand for;
! 0 - smooth                           !coded as *nil*
! 1 - occasional light turbulence      !coded as *light*
! 2 - light turbulence                 !coded as *light*
! 3 - light to moderate turbulence     !coded as *light*
! 4 - moderate turbulence              !coded as *moderate*
! 5 - moderate to severe turbulence    !coded as *moderate*
! 6 - severe turbulence                !coded as *severe*
! 7 - extreme turbulence               !coded as *severe*
!---------------------------------------------------------------------

ELSE IF (REPORT(POINT:POINT+1) ==  'TB') THEN
iflabel2: &
  IF(REPORT(POINT+3:POINT+6)  ==  'CODE') THEN

iflabel3: &
    IF((REPORT(POINT+8:POINT+8)  ==  '1') .OR.   &
       (REPORT(POINT+8:POINT+10)  ==  'ONE')) THEN
      TURB=1
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '2') .OR. &
            (REPORT(POINT+8:POINT+10)  ==  'TWO')) THEN
      TURB=1
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '3') .OR. &
            (REPORT(POINT+8:POINT+12)  ==  'THREE')) THEN
      TURB=1
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '4') .OR. &
            (REPORT(POINT+8:POINT+11)  ==  'FOUR')) THEN
      TURB=2
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '5') .OR. &
            (REPORT(POINT+8:POINT+11)  ==  'FIVE')) THEN
      TURB=2
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '6') .OR. &
            (REPORT(POINT+8:POINT+10)  ==  'SIX')) THEN
      TURB=3
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '7') .OR. &
            (REPORT(POINT+8:POINT+12)  ==  'SEVEN')) THEN
      TURB=3
      POINT=POINT+9
    ELSE IF((REPORT(POINT+8:POINT+8) ==  '0') .OR.  &
            (REPORT(POINT+8:POINT+11)  ==  'ZERO')) THEN
      TURB=0
      POINT=POINT+9
    ELSE
      TURB=-1
    END IF iflabel3

  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '0') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '0')) THEN
    TURB=0
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '1') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '1')) THEN
    TURB=1
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '2') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '2')) THEN
    TURB=1
    POINT=POINT+4
  ELSE IF((REPORT(POINT+2:POINT+2) ==  '3') .OR.   &
         (REPORT(POINT+3:POINT+3)  ==  '3')) THEN
    TURB=1
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '4') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '4')) THEN
    TURB=2
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '5') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '5')) THEN
    TURB=2
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '6') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '6')) THEN
    TURB=3
    POINT=POINT+4
  ELSE IF ((REPORT(POINT+2:POINT+2) ==  '7') .OR.  &
         (REPORT(POINT+3:POINT+3)  ==  '7')) THEN
    TURB=3
    POINT=POINT+4
  ELSE
    TURB=-1
  END IF iflabel2

!-----------------------------------------------------------------
!This code is similar to above. Except that instead of 'TB' the
!reported turbulence starts 'CODE'
!-----------------------------------------------------------------

ELSE IF(REPORT(POINT:POINT+3) ==  'CODE') THEN

iflabel4: &
  IF((REPORT(POINT+5:POINT+5)  ==  '0') .OR.   &
     (REPORT(POINT+5:POINT+8)  ==  'ZERO')) THEN
    TURB=0
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '1') .OR. &
          (REPORT(POINT+5:POINT+7)  ==  'ONE')) THEN
    TURB=1
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '2') .OR.  &
          (REPORT(POINT+5:POINT+7)  ==  'TWO')) THEN
    TURB=1
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '3') .OR.  &
          (REPORT(POINT+5:POINT+9)  ==  'THREE')) THEN
    TURB=1
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '4') .OR.  &
          (REPORT(POINT+5:POINT+8)  ==  'FOUR')) THEN
    TURB=2
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '5') .OR.  &
          (REPORT(POINT+5:POINT+8)  ==  'FIVE')) THEN
    TURB=2
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '6') .OR.  &
          (REPORT(POINT+5:POINT+7)  ==  'SIX')) THEN
    TURB=3
    POINT=POINT+6
  ELSE IF((REPORT(POINT+5:POINT+5) ==  '7') .OR.  &
          (REPORT(POINT+5:POINT+9)  ==  'SEVEN')) THEN
    TURB=3
    POINT=POINT+6
  ELSE
    TURB=-1
  END IF iflabel4
END IF iflabel1

!----------------------------------------------------------------------
!The following code sets the value of OPT_TRB to the value in the BUFR
!code table for turbulence, where missing = 15
!                                 nil     =  8
!                                 light   =  9
!                                 moderate= 10
!                                 severe  = 11
!The flag TURB is set to -1 when this routine has been called by
!AIROPT but no turbulence group could be decode.
!----------------------------------------------------------------------

IF (TURB  ==  -1) THEN
  OPT_TRB=-9999999.
ELSE IF(TURB ==  0) THEN
  OPT_TRB=8.0
ELSE IF(TURB ==  1) THEN
  OPT_TRB=9.0
ELSE IF(TURB ==  2) THEN
  OPT_TRB=10.0
ELSE IF(TURB ==  3) THEN
  OPT_TRB=11.0
END IF

RETURN
END SUBROUTINE AIRTRB
