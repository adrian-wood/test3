SUBROUTINE AIRMID(TIMEDD,TIMEHH,TIMEMM,MID_YY,MID_MTH,MID_DD,  &
 MID_HH,MID_MM,LAT,LONG,MID_LAT,MID_LONG,SIGN,MATCH,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRMID
!
! PURPOSE       : MODULE TO ASSIST IN THE CALCULATION OF MID-POINT DATA
!                 FOR MDB AIREPS.
!
! DESCRIPTION   : THE CURRENT TIME AND POSITION ARE PASSED TO AIRIDX
!                 TO SEARCH THE INDEX ENTRIES FOR A MATCHING CALLSIGN
!                 IN A PREVIOUS REPORT. IF THERE IS NO MATCH IN THE
!                 CURRENT HOUR THE TIME IS ADJUSTED TO THE PREVIOUS
!                 HOUR AND AIRIDX CALLED WITH THE NEW DATA.
!
! ARGUMENTS     : (1) TIMEDD  - current date/time                 (i)
!                 (2) TIMEHH                                      (i)
!                 (3) TIMEMM                                      (i)
!                 (4) MID_YY   - midpt date/time                  (i/o)
!                 (5) MID_MTH                                     (i/o)
!                 (6) MID_DD                                      (i/o)
!                 (7) MID_HH                                      (i/o)
!                 (8) LAT     - current position                  (i)
!                 (9) LONG                                        (i)
!                 (10) MID_LAT  - midpt position                  (i/o)
!                 (11) MID_LONG                                   (i/o)
!                 (12) SIGN   - callsign                          (i)
!                 (13) MATCH  - set 1 if match is found           (o)
!                 (14) NFT   - MDB dataset unit number            (i)
!
! CALLED BY     : AIROPT
!
! CALLS TO      : DATE31,DATE13,AIRIDX,DATIM
!
! REVISION INFO :
!
! $Workfile: airmid.f90$ $Folder: OpSource$
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

! Interfaces

USE datim_mod
USE zpdate_mod
USE airidx_mod

IMPLICIT NONE

! Arguments

REAL,INTENT(IN)    :: TIMEDD       !present day
REAL,INTENT(IN)    :: TIMEHH       !present hour
REAL,INTENT(IN)    :: TIMEMM       !present minutes
REAL,INTENT(INOUT) :: MID_YY
REAL,INTENT(INOUT) :: MID_MTH
REAL,INTENT(INOUT) :: MID_DD       !Mid point day
REAL,INTENT(INOUT) :: MID_HH       !Mid point hour
REAL,INTENT(INOUT) :: MID_MM       !Mid point minutes
REAL,INTENT(IN)    :: LAT          !present Latitude
REAL,INTENT(IN)    :: LONG         !present Longitude
REAL,INTENT(INOUT) :: MID_LAT      !Mid point Latitude
REAL,INTENT(INOUT) :: MID_LONG     !Mid point Longitude
CHARACTER(LEN=8),INTENT(INOUT) :: SIGN    !Aircraft callsign
INTEGER,INTENT(OUT) ::  MATCH
INTEGER,INTENT(IN)    ::  NFT

! Local Variables

INTEGER :: CENDAY
INTEGER :: ID
INTEGER :: IM
INTEGER :: IY
INTEGER :: INT_LONG
INTEGER :: NOW(8)
INTEGER :: REMAIN
INTEGER :: REQ_YY
INTEGER :: REQ_MTH
INTEGER :: REQ_DD
INTEGER :: REQ_HH
INTEGER :: TIMEYY
INTEGER :: TIMETH

!---------------------------------------------------------------------
!It is too difficult to produce reliable MID point data for all data.
!In agreement with customers it has been decided that MID point data
!will only be calculated if the current Lat and previous LAT are
!multiples of 10 degrees. So, check now that the current Lat is a
!multiple of 10 degrees. If not don't attempt to find a MID point.
!---------------------------------------------------------------------

INT_LONG=INT(LONG*100)
REMAIN=MOD(INT_LONG,1000)
MATCH = 0

ifremain: &
IF (REMAIN == 0) THEN    !MULTIPLE OF 10 DEGREES
  MATCH=0
!----------------------------------------------------------------------
!First we need to check the current index entry. There will be no
!change in the time data but we still need to assign all variables
!A call to DATIM gets the current Month and Year. Since we are looking
!at this hours index entry there is no need to check if we are
!spanning a day. - Index entries hold 1hrs data
!----------------------------------------------------------------------

  CALL DATIM(NOW)

  REQ_YY=NOW(8)
  REQ_MTH=NOW(7)
  REQ_DD=TIMEDD
  REQ_HH=TIMEHH
  TIMEYY=NOW(8)
  TIMETH=NOW(7)

  CALL AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,REQ_MTH, &
  REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,LAT,LONG,    &
  MID_LAT,MID_LONG,SIGN,MATCH,NFT)

!----------------------------------------------------------------------
!On return from AIRIDX the match variable is checked. If match = 0
!there were no suitable entries in the current hour and so a search of
!the previous hours index will occur. If match = 1 then a suitable
!entry has been found in the current hour and there is no need to
!to do further searching.
!---------------------------------------------------------------------
  IF (MATCH  ==  0) THEN

    REQ_HH=TIMEHH-1                   !go back 1 hour
    IF (REQ_HH  <  0) THEN            !indicates spanning a day
      REQ_HH=REQ_HH+24                !correct the hour
      CALL DATE31(INT(TIMEDD),NOW(7),NOW(8),CENDAY)
      CENDAY=CENDAY-1                 !go back a day
      CALL DATE13(CENDAY,ID,IM,IY)
      REQ_DD=ID
      REQ_MTH=IM
      REQ_YY=IY
    ELSE
      REQ_MTH=NOW(7)
      REQ_YY=NOW(8)
    END IF

   CALL AIRIDX(TIMEYY,TIMETH,TIMEDD,TIMEHH,TIMEMM,REQ_YY,REQ_MTH, &
    REQ_DD,REQ_HH,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,LAT,LONG,   &
    MID_LAT,MID_LONG,SIGN,MATCH,NFT)
  END IF
END IF ifremain

RETURN
END SUBROUTINE AIRMID
