SUBROUTINE AIRDATE(BHOUR,BDAY,BMONTH,BYEAR)

!----------------------------------------------------------------------
!
! PROGRAM       : AIRDATE
!
! PURPOSE       : To decide the bulletin month & year for aircraft data
!                 (so that report day can then be found by going back
!                 up to 24 hours from bulletin day/hour)
!
! DESCRIPTION   : The month from the system clock is the default, but
!                 if the current time is after 23Z and the bulletin is
!                 for 0Z on the 1st, the date is next month if the 1st
!                 is tomorrow; or if the bulletin date/hour is later
!                 than the next hour set the date to last month.
!
! DATA TYPE(S)  : AMDAR (& AIREP?)
!
! CALLED BY     : AMDEXC (& AIR...?) once for each bulletin
!
! CALLS         : ZPDATE, DATIM
!
! ARGUMENTS     : (1) bulletin hour                                 (i)
!                 (2) bulletin day                                  (i)
!                 (3) bulletin month                                (o)
!                 (4) bulletin year                                 (o)
!
! REVISION INFO :
!
! $Workfile: airdate.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 21/01/2011 14:26:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
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

! Use statements:
USE datim_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)  :: BHOUR     !a01
INTEGER,          INTENT(IN)  :: BDAY      !a02
INTEGER,          INTENT(OUT) :: BMONTH    !a03
INTEGER,          INTENT(OUT) :: BYEAR     !a04

! Local declarations:

INTEGER         :: NOW(8)
INTEGER         :: CENDAY
INTEGER         :: NEXT_DAY
INTEGER         :: NEXT_MONTH
INTEGER         :: NEXT_YEAR
LOGICAL         :: TOMORROW

!-----------------------------------------------------------------------
! Set the month and year from the bulletin day and the current time.
! A bulletin may be for tomorrow if it comes in just before midnight,
! and tomorrow may be next month; otherwise it's for last month if
! day/hour greater than current.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! First set current month as default
!-----------------------------------------------------------------------

CALL DATIM(NOW)
BYEAR=NOW(8)
BMONTH=NOW(7)

!-----------------------------------------------------------------------
! If bulletin is for 0Z on the 1st and it's after 23Z now, see if
! the 1st is tomorrow: if so, the bulletin is for next month.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (BDAY == 1 .AND. BHOUR == 0 .AND.    &
  NOW(6) >= 28 .AND. NOW(5) == 23) THEN
  CALL DATE31(NOW(6),BMONTH,BYEAR,CENDAY)
  CENDAY=CENDAY+1
  CALL DATE13(CENDAY,NEXT_DAY,NEXT_MONTH,NEXT_YEAR)
  IF (NEXT_DAY == 1) THEN
    BYEAR=NEXT_YEAR
    BMONTH=NEXT_MONTH
    TOMORROW=.TRUE.
  ELSE
    TOMORROW=.FALSE.
  END IF
ELSE
  TOMORROW=.FALSE.
END IF IFLABEL1

!-----------------------------------------------------------------------
! If bulletin day greater than current day but not tomorrow,
! or same day but hour too far ahead for data to be a few minutes
! early (though that shouldn't happen with aircraft data!),
! then must be last month.
!-----------------------------------------------------------------------

IF (.NOT.TOMORROW .AND. (BDAY > NOW(6) .OR.      &
    (BDAY == NOW(6).AND.BHOUR > NOW(5)+1))) THEN
  BMONTH=BMONTH-1
  IF (BMONTH == 0) THEN                 ! back to December?
    BMONTH=12
    BYEAR=BYEAR-1                       ! if so, last year
  END IF
END IF

RETURN
END SUBROUTINE AIRDATE
