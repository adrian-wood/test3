SUBROUTINE MTRRWY(POINT,REPORT,GRPLEN,CHARR,RVRDIR,VISTEND, &
                  RVRPRL,MINQUAL,MINVIS,MAXQUAL,MAXVIS)

!-----------------------------------------------------------------------
!
! PROGRAM       : MTRRWY
!
! PURPOSE       : TO DECODE THE RUNWAY VISIBILITY GROUPS.
!                 IT ALLOWS FOR A MAXIMUM OF 2 GROUPS.
!
! CALLED BY     : MTREXP
!
! PARAMETERS    : (1)  POINT   CURRENT POSITION WITHIN REPORT   (IN)
!                 (2)  REPORT  METAR REPORT                     (IN)
!                 (3)  GRPLEN  GROUP LENGTH                     (IN)
!                 (4)  CHARR   CHARACTER DESCRIPTION OF GROUP   (IN)
!                 (5)  RVRDIR  RUNWAY DIRECTION                 (RET)
!                 (6)  VISTEND VISIBILITY TENDENCY              (RET)
!                 (7)  RVRPRL  PARALLEL RUNWAY IDENTFIER        (RET)
!                 (8)  MINQUAL MINIMUM VISIBILITY QUALIFIER     (RET)
!                 (9)  MINVIS  MINIMUM VISIBILITY               (RET)
!                (10)  MAXQUAL MAXIMUM VISIBILITY QUALIFIER     (RET)
!                (11)  MAXVIS  MAXIMUM VISIBILITY               (RET)
!
! REVISION INFO :
!
!
! $Workfile: mtrrwy.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 05/04/2011 14:15:23$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         05/04/2011 14:15:23    Alison Weir     Amend
!       IF to avoid out of range runtime error
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
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE IVALUE_MOD

IMPLICIT NONE

! Interface Arguments

INTEGER,           INTENT(IN)  ::  POINT        ! Point within report and group
CHARACTER (LEN=*), INTENT(IN)  ::  REPORT       ! Complete METAR report
INTEGER,           INTENT(IN)  ::  GRPLEN       ! Length of group
CHARACTER (LEN=*), INTENT(IN)  ::  CHARR        ! Character content of group.
REAL,              INTENT(OUT) ::  RVRDIR       ! Runway direction
REAL,              INTENT(OUT) ::  VISTEND      ! Visibility tendency.
REAL,              INTENT(OUT) ::  RVRPRL       ! Parallel runway identifier.
REAL,              INTENT(OUT) ::  MINQUAL      ! Minimum visibility qualifier
REAL,              INTENT(OUT) ::  MINVIS       ! Minimum visibility.
REAL,              INTENT(OUT) ::  MAXQUAL      ! Maximum visibility qualifier.
REAL,              INTENT(OUT) ::  MAXVIS       ! Maximum visibility, when variable.

! Local Parameters

INTEGER, PARAMETER      ::  MISSG = -9999999  ! Missing data ind icator

! Local Scalars

INTEGER                 ::  VALUE = MISSG     ! Visibility value (MAXVIS or MINVIS)
INTEGER                 ::  SOLIDI = 0        ! Position within group of '/' character
INTEGER                 ::  TIMES = 0         ! Number of times to loop visibility expansion.
INTEGER                 ::  VRB = 0           ! Position within group of 'V' character
INTEGER                 ::  LOOP              ! General loop variable
INTEGER                 ::  OFFSET            ! Ensures correct position within group
                                              ! is checked for variable visibilities.

! Initialise variables

RVRDIR=MISSG
VISTEND=MISSG
RVRPRL=MISSG
MINQUAL=MISSG
MINVIS=MISSG
MAXQUAL=MISSG
MAXVIS=MISSG


! Find position of SOLIDI, if any.

SOLIDI=INDEX(REPORT(POINT:GRPLEN+POINT-1),'/')

! Expand runway direction.

IF (CHARR(2:3) == 'NN') THEN
  RVRDIR = FLOAT(IVALUE(REPORT(POINT+1:POINT+2)))
END IF

! RVR's in post July 1993 code format.

IF_NEWFMT: &
IF ((CHARR(2:4) == 'NNY' .OR. CHARR(2:4) == 'NN/') &
    .AND. SOLIDI > 0) THEN

! Check for and expand parallel runway identifier.

IF_RWYID: &
  IF (CHARR(SOLIDI-2:SOLIDI-1) == 'YY') THEN
    IF (REPORT(POINT+3:POINT+4) == 'RR') THEN
      RVRPRL=4
    ELSE IF (REPORT(POINT+3:POINT+4) == 'LL') THEN
      RVRPRL=0
    END IF
  ELSE IF (CHARR(SOLIDI-2:SOLIDI-1) == 'NY') THEN
    IF (REPORT(POINT+3:POINT+3) == 'R') THEN
      RVRPRL=3
    ELSE IF (REPORT(POINT+3:POINT+3) == 'C') THEN
      RVRPRL=2
    ELSE IF (REPORT(POINT+3:POINT+3) == 'L') THEN
      RVRPRL=1
    END IF
  END IF IF_RWYID

! Check whether variations in visibility are reported.
! If there are then the check for qualifiers, visibilities and tendency
! needs done twice.
! Set the value of TIMES to ensure the correct number of loops.

  VRB=INDEX(REPORT(POINT:POINT+GRPLEN-1),'V')
  IF (VRB == 0) THEN
    TIMES=1
  ELSE
    TIMES=2
  END IF

! The value of OFFSET will ensure the correct position within the
! group is checked.

DO_TIMES: &
  DO LOOP=1,TIMES
    OFFSET=SOLIDI+(LOOP-1)*(VRB-SOLIDI)

! Check for visibility qualifiers.
! Set value for 'less than or equal to' if 'M' found, or value for
! 'greater than or equal to' if 'P' found.

IF_VISQUAL: &
    IF (CHARR(OFFSET+1:OFFSET+1) == 'Y') THEN
      IF (REPORT(POINT+OFFSET:POINT+OFFSET) == 'M') THEN
        IF (LOOP == 1) THEN
          MINQUAL=4
        ELSE
          MAXQUAL=4
        END IF
      ELSE IF (REPORT(POINT+OFFSET:POINT+OFFSET) == 'P') THEN
        IF (LOOP == 1) THEN
          MINQUAL=2
        ELSE
          MAXQUAL=2
        END IF
      END IF
    END IF IF_VISQUAL

! Check for runway visibility and any tendency.
! The position of the tendency indicator is different if a visibility
! qualifier is included in the report.
! Two seperate checks cater for the possibilities.

! Check for visibility and tendency in groups with qualifiers.

IF_QUAL: &
    IF (MAXQUAL > 0 .OR. MINQUAL > 0) THEN
      IF (CHARR(OFFSET+2:OFFSET+5) == 'NNNN') THEN
        VALUE = IVALUE(REPORT(POINT+OFFSET+1:POINT+OFFSET+4))
        IF (LOOP == 1) THEN
          MINVIS=VALUE
        ELSE
          MAXVIS=VALUE
        END IF
      END IF
      IF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) == 'D') THEN
        VISTEND=2.
      ELSE IF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) == 'U') THEN
        VISTEND=1.
      ELSE IF (REPORT(POINT+OFFSET+5:POINT+OFFSET+5) == 'N') THEN
        VISTEND=0.
      END IF

! Check for visibility and tendency in groups without qualifiers.

    ELSE
      IF (CHARR(OFFSET+1:OFFSET+4) == 'NNNN') THEN
        VALUE=IVALUE(REPORT(POINT+OFFSET:POINT+OFFSET+3))
        IF (LOOP == 1) THEN
          MINVIS=VALUE
        ELSE
          MAXVIS=VALUE
        END IF
      END IF
      IF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) == 'D') THEN
        VISTEND=2.
      ELSE IF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) == 'U') THEN
        VISTEND=1.
      ELSE IF (REPORT(POINT+OFFSET+4:POINT+OFFSET+4) == 'N') THEN
        VISTEND=0.
      END IF
    END IF IF_QUAL
  END DO DO_TIMES

! RVR's in pre July 1993 code format or RVR's with no variable
! visibility or no runway identifier.

ELSE

! Search for parallel runway identifiers and set appropriate expansion
! value if found.

IF_RWYID2: &
  IF (SOLIDI > 0) THEN
    IF (CHARR(SOLIDI+3:GRPLEN) == 'YY') THEN
      IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+3) == 'RR') THEN
        RVRPRL=4.
      ELSE IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+3) == 'LL')THEN
        RVRPRL=0.
      END IF
    ELSE IF (CHARR(SOLIDI+3:GRPLEN) == 'Y') THEN
      IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) == 'R') THEN
        RVRPRL=3.
      ELSE IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) == 'C')THEN
        RVRPRL=2.
      ELSE IF (REPORT(POINT+SOLIDI+2:POINT+SOLIDI+2) == 'L')THEN
        RVRPRL=1.
      END IF
    END IF
  END IF IF_RWYID2

! Check for visibility qualifiers.
! Set value for 'less than or equal to' if 'MM' found, or value for
! 'greater than or equal to' if 'P' found.
! Expand visibility value, if available.
! Note that only MINVIS and MINQUAL are used.

IF_VISQUAL2: &
  IF (CHARR(2:3) == 'YY') THEN
    IF (REPORT(POINT+1:POINT+2) == 'MM') THEN
      MINQUAL=4
    END IF
    IF (CHARR(4:7) == 'NNNN') THEN
      MINVIS = FLOAT(IVALUE(REPORT(POINT+3:POINT+6)))
    END IF
  ELSE IF (CHARR(2:3) == 'YN') THEN
    IF (REPORT(POINT+1:POINT+1) == 'P') THEN
      MINQUAL=2
    END IF
    IF (CHARR(3:6) == 'NNNN') THEN
      MINVIS = FLOAT(IVALUE(REPORT(POINT+2:POINT+5)))
    END IF
  ELSE IF (CHARR(2:5) == 'NNNN') THEN
    MINVIS = FLOAT(IVALUE(REPORT(POINT+1:POINT+4)))
  END IF IF_VISQUAL2
END IF IF_NEWFMT


RETURN
END SUBROUTINE MTRRWY
