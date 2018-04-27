SUBROUTINE MTRPER(Report,Pointer,RXpanAray,MoreTrend,&
&ReportLength,Displace)

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
! Arguments     : (1) Report  - METAR Report being decoded.
!                 (2) Pointer  - Points to curent pos. in Report
!                 (3)RXpanAray - Expansion Array for decoded values
!                 (4)MoreTrend - Indicates valid decode
!                 (5)ReportLength - Length of Report
!
! Calls to      : NONE
!
! Revision Info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrper.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:57  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/06/11  13:34:49  13:34:49  usmdb (Generic MDB account)
! Initial revision
!
! 15/05/98  First Version of this code
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

IMPLICIT NONE

!Declare Character
CHARACTER*(*)      Report

!Declare Integer
INTEGER            Pointer
INTEGER            ReportLength
INTEGER            PointerIncrement
INTEGER            GroupLength
INTEGER            I
INTEGER            Displace

!Declare Real
REAL               RXpanAray(*)
REAL               Missing
REAL               FromHour
REAL               FromMin
REAL               TillHour
REAL               TillMin
REAL               AtHour
REAL               AtMin

!Declare Logical
LOGICAL            MoreTrend

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

GroupLength=INDEX(Report(Pointer:),' ')

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

IF((Report(Pointer:Pointer+1) .NE. 'FM').OR.&
&(Report(Pointer:Pointer+1) .NE. 'TL').OR.&
&(Report(Pointer:Pointer+1) .NE. 'AT' ))THEN
  IF (GroupLength .EQ. 4 ) THEN
    IF((Report(Pointer+3:Pointer+3) .NE. '0').AND.&
      &(Report(Pointer+2:Pointer+2) .NE. '0').AND.&
       (Report(Pointer+2:Pointer+2) .NE. '5')) THEN
      DO I=Displace,Displace+28
        RXpanAray(I)=Missing
      ENDDO
      MoreTrend=.FALSE.
    ELSE
      PointerIncrement=0
    ENDIF
  ELSE
    PointerIncrement=0
  ENDIF
ELSEIF((Report(Pointer:Pointer+1) .EQ. 'FM').AND.&
&(Report(Pointer+7:Pointer+8) .EQ. 'TL'))THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')FromHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')FromMin
  READ(Report(Pointer+9:Pointer+10),'(F2.0)')TillHour
  READ(Report(Pointer+11:Pointer+12),'(F2.0)')TillMin
  PointerIncrement=14
ELSEIF((Report(Pointer:Pointer+1) .EQ. 'FM').AND.&
&(Report(Pointer+7:Pointer+8) .NE. 'TL'))THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')FromHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')FromMin
  PointerIncrement=7
ELSEIF(Report(Pointer:Pointer+1) .EQ. 'TL') THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')TillHour
  READ(Report(Pointer+4:Pointer+5),'(F2.0)')TillMin
  PointerIncrement=7
ELSEIF((Report(Pointer:Pointer+1) .EQ. 'AT').AND.&
&RXpanAray(86) .EQ. 1.0) THEN
  READ(Report(Pointer+2:Pointer+3),'(F2.0)')AtHour
  READ(Report(Pointer+4:POinter+5),'(F2.0)')AtMin
  PointerIncrement=7
ELSE
  MoreTrend=.FALSE.
ENDIF


!----------------------------------------------------------------------
!Pass the decoded Trend Period Information to the expansion array.
!Any values not found will remian set to Missing
!----------------------------------------------------------------------
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

  IF (Pointer+PointerIncrement .GE. ReportLength) THEN
    MoreTrend=.FALSE.
  ELSE
    Pointer=Pointer+PointerIncrement
  ENDIF
ENDIF

RETURN
END SUBROUTINE MTRPER
