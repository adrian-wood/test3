SUBROUTINE UAPART(REPORT,TT,PART,PART_TYPE,STANDRD,TEMP,PILOT)

!-----------------------------------------------------------------------
!
! PROGRAM       : UAPART
!
! PURPOSE       : Decide if upper air report is TEMP or PILOT
!                 & set part
!
! DESCRIPTION   : Look for MiMiMjMj and use it if found; if not, use TT.
!
! DATA TYPE(S)  : UPPER AIR (TEMP & PILOT, ALL PARTS)
!
! CALLED BY     : UAXPAND
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) report (starting 'TTAA' or 'PPBB' OR...?)     (i)
!                 (2) 'TT' from TTAAii                              (i)
!                 (3) part (A,B,C,D)                                (o)
!                 (4) numeric part indicator (0,1,2,3)              (o)
!                 (5) true if standard level data                   (o)
!                 (6) true if temp or dropsonde                     (o)
!                 (7) true if pilot                                 (o)
!
! REVISION INFO :
!
!
! $Workfile: uapart.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 26/01/2011 14:22:21$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
! $
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

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)   :: REPORT
CHARACTER (LEN=*), INTENT(IN)   :: TT
CHARACTER (LEN=*), INTENT(OUT)  :: PART
INTEGER, INTENT(OUT)            :: PART_TYPE
LOGICAL, INTENT(OUT)            :: STANDRD
LOGICAL, INTENT(OUT)            :: TEMP
LOGICAL, INTENT(OUT)            :: PILOT

! Local Variables

INTEGER :: PTR

CHARACTER (LEN=1)  :: MI


! ---------------------------------------------------------------------

!Initialize Variables

PTR=1

DO WHILE (PTR < 10 .AND.              &
    ((REPORT(PTR:PTR+1) /= 'TT' .AND. &
     REPORT(PTR:PTR+1) /= 'PP' .AND.  &
     REPORT(PTR:PTR+1) /= 'UU' .AND.  &
     REPORT(PTR:PTR+1) /= 'QQ' .AND.  &
     REPORT(PTR:PTR+1) /= 'XX' .AND.  &
     REPORT(PTR:PTR+1) /= 'II' .AND.  &
     REPORT(PTR:PTR+1) /= 'EE') .OR.  &
  (REPORT(PTR+2:PTR+3) /= 'AA' .AND.  &
   REPORT(PTR+2:PTR+3) /= 'BB' .AND.  &
   REPORT(PTR+2:PTR+3) /= 'CC' .AND.  &
   REPORT(PTR+2:PTR+3) /= 'DD')))
  PTR=PTR+1
END DO

! Set PART as last letter of MiMiMjMj & TEMP/PILOT from first 2 letters
! A dropsonde must be a TEMP, part A or B.

IF (PTR < 10) THEN
  MI=REPORT(PTR:PTR)
  PART=REPORT(PTR+3:PTR+3)
  TEMP=MI == 'T'.OR.MI == 'U'.OR.MI == 'I'
  PILOT=MI == 'P'.OR.MI == 'Q'.OR.MI == 'E'
  IF (TT == 'UZ') TEMP=.TRUE.

! If no MiMiMjMj found (one may be corrupted) use TT to decide part
! (not always possible: some bulletins can contain different parts)

ELSE
  TEMP=TT == 'US'.OR.TT == 'UK'.OR.TT == 'UL'.OR.TT == 'UE'  &
   .OR.TT == 'UZ'
  PILOT=TT == 'UP'.OR.TT == 'UG'.OR.TT == 'UH'.OR.TT == 'UQ'

  PART=' '
  IF (TT == 'US' .OR. TT == 'UP') PART='A'
  IF (TT == 'UK' .OR. TT == 'UG') PART='B'
  IF (TT == 'UL' .OR. TT == 'UH') PART='C'
  IF (TT == 'UE' .OR. TT == 'UQ') PART='D'
  IF (TT == 'UZ') PART='B'
END IF

! Finally set PART_TYPE & STANDRD from PART

IF (PART == 'A') PART_TYPE=0
IF (PART == 'B') PART_TYPE=1
IF (PART == 'C') PART_TYPE=2
IF (PART == 'D') PART_TYPE=3

STANDRD=PART == 'A'.OR.PART == 'C'

RETURN
END SUBROUTINE UAPART
