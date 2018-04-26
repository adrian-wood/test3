SUBROUTINE MTRPER(Report,Pointer,RXpanAray,MoreTrend, &
                  ReportLength,Displace)

!----------------------------------------------------------------------
!
! Subroutine    : MTRPER
!
! Purpose       : To decode METAR TREND PERIOD information
!
! Description   :
!
! Called By     : MTRTRD
!
! Arguments     : (1) Report       - METAR Report being decoded.
!                 (2) Pointer      - Points to curent pos. in Report
!                 (3) RXpanAray    - Expansion Array for decoded values
!                 (4) MoreTrend    - Indicates valid decode
!                 (5) ReportLength - Length of Report
!
! Calls to      : NONE
!
! REVISION INFO :
!
!
! $Workfile: mtrper.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 19/06/2011 10:30:25$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         19/06/2011 10:30:25    Sheila Needham  Add
!       explicit string lengths
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

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

! Local variables

INTEGER            PointerIncrement
INTEGER            GroupLength
INTEGER            I
REAL               Missing
REAL               FromHour
REAL               FromMin
REAL               TillHour
REAL               TillMin
REAL               AtHour
REAL               AtMin


!Save all values
SAVE

MoreTrend=.TRUE.
Missing=-9999999.
FromHour=Missing
FromMin=Missing
TillHour=Missing
TillMin=Missing
AtHour=Missing
AtMin=Missing
PointerIncrement=0
GroupLength=0

!----------------------------------------------------------------------
!Obtain the length of the group for further checks (see below)
!----------------------------------------------------------------------

GroupLength=INDEX(Report(Pointer:ReportLength),' ')

!----------------------------------------------------------------------
!Decode the time period group. Ref: WMO Code Manual FM15 METAR for
!Information on valid groups. Some METARS still have the old style
!time groups eg 1221 meaning 1200 to 2100Z. The only other four
!figure group is the horizontal visibility which always ends in 0. So
!if we have a four figure group here then check as follows;
!   1) If it ends in any figure but a zero then its a time group
!   2) If it ends in a zero search the rest of this part of the trend
!      to see if another four figure group is present. If there is then
!      assume this group is time and the next 4-fogure group is VVVV.
!----------------------------------------------------------------------
IF_BLK1: &
IF((Report(Pointer:Pointer+1) /= 'FM').OR. &
(Report(Pointer:Pointer+1) /= 'TL').OR. &
(Report(Pointer:Pointer+1) /= 'AT' ))THEN

IF_BLK2: &
  IF (GroupLength == 4 ) THEN
    IF((Report(Pointer+3:Pointer+3) /= '0').AND. &
       (Report(Pointer+2:Pointer+2) /= '0').AND. &
       (Report(Pointer+2:Pointer+2) /= '5')) THEN
      DO I=Displace,Displace+28
        RXpanAray(I)=Missing
      END DO
      MoreTrend=.FALSE.
    ELSE
      PointerIncrement=0
    END IF
  ELSE
    PointerIncrement=0
  END IF IF_BLK2

ELSE IF((Report(Pointer:Pointer+1) == 'FM').AND. &
(Report(Pointer+7:Pointer+8) == 'TL'))THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')FromHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')FromMin
  READ(Report(Pointer+9:Pointer+10),'(F2.0)')TillHour
  READ(Report(Pointer+11:Pointer+12),'(F2.0)')TillMin
  PointerIncrement=14
ELSE IF((Report(Pointer:Pointer+1) == 'FM').AND. &
(Report(Pointer+7:Pointer+8) /= 'TL'))THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')FromHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')FromMin
  PointerIncrement=7
ELSE IF(Report(Pointer:Pointer+1) == 'TL') THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')TillHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')TillMin
  PointerIncrement=7
ELSE IF((Report(Pointer:Pointer+1) == 'AT').AND. &
RXpanAray(86) == 1.0) THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')AtHour
  READ(Report(Pointer+4:POinter+5),'(F2.0)')AtMin
  PointerIncrement=7
ELSE
  MoreTrend=.FALSE.
END IF IF_BLK1


!----------------------------------------------------------------------
!Pass the decoded Trend Period Information to the expansion array.
!Any values not found will remian set to Missing
!----------------------------------------------------------------------

IF_TREND: &
IF (MoreTrend) THEN
  RXpanAray(Displace+1)=FromHour
  RXpanAray(Displace+2)=FromMin
  RXpanAray(Displace+3)=TillHour
  RXpanAray(Displace+4)=TillMin
  RXpanAray(Displace+5)=AtHour
  RXpanAray(Displace+6)=AtMin

!----------------------------------------------------------------------
!Increment the Pointer to skip over any trend period groups and test to
!see if we have reached the end of the report.
!----------------------------------------------------------------------

  IF (Pointer+PointerIncrement  >=  ReportLength) THEN
    MoreTrend=.FALSE.
  ELSE
    Pointer=Pointer+PointerIncrement
  END IF
END IF IF_TREND

RETURN
END SUBROUTINE MTRPER
