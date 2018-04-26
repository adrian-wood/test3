SUBROUTINE MTRWTR(Report,Pointer,RXpanAray,MoreTrend,  &
                  ReportLength,Displace)

!----------------------------------------------------------------------
! Program       : MTRWTR
!
! Purpose       : Decode the Forecast weather (wx) groups in the
!                 TREND section
!
! Description   : Look for and decode any wx code. No qulaity control
!                 is performed so invalid combinations will be returned
!                 to the user to decide upon appropriate action.
!
! Called By     : MTRTRD
!
! Arguments     : Number Type  I-O Description
!                 (1)    CHAR  I   Metar Report being decoded
!                 (2)    INT   I/O Pointer within REPORT
!                 (3)    REAL  I/O Expansion Array
!                 (4)    LOG   I/O Indicates more report to come
!                 (5)    INT   I   Report Length
!                 (6)    INT   I   Displacement in expansion array
!
! Calls to      : NONE
!
! REVISION INFO :
!
!
! $Workfile: mtrwtr.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 19/06/2011 10:34:25$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         19/06/2011 10:34:25    Sheila Needham  Add
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

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(INOUT) ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

! Local Variables

CHARACTER (LEN=2)    ::  Descriptor(8)
CHARACTER (LEN=2)    ::  Weather(20)

INTEGER              ::  COUNT = 0
INTEGER              ::  GroupLength = 0
INTEGER              ::  ID = 1
INTEGER              ::  ODD = 0
INTEGER              ::  QUAL = -9999999       ! Default intensity is missing
INTEGER              ::  Disp = 0
INTEGER              ::  WXCount = 1
INTEGER              ::  PointerIncrement

LOGICAL              ::  ERROR = .FALSE.
LOGICAL              ::  EXPAND = .FALSE.
LOGICAL              ::  GotDescriptor = .FALSE.
LOGICAL              ::  GotWeather = .FALSE.

!----------------------------------------------------------------------
!The following DATA statements contain a list of the possible
!descriptors and weather phenomena. The DATA statement is in the same
!order as BUFR CODE TABLES 020193 and 020194. Since the displacement
!within the DATA statement represents the code table figure the order
!should not be changed.
!----------------------------------------------------------------------

DATA Descriptor/'MI','BC','DR','BL','SH','TS','FZ','PR'/

DATA Weather   /'DZ','RA','SN','SG','IC','PE','GR','GS','BR','FG',  &
             &  'FU','VA','DU','SA','HZ','PO','SQ','FC','SS','DS'/

SAVE

!----------------------------------------------------------------------
! Calculate the GroupLength
!----------------------------------------------------------------------

GroupLength=INDEX(Report(Pointer:ReportLength),' ')-1
IF (GroupLength <= 1) THEN
  EXPAND=.FALSE.
END IF

IF (GroupLength > 9) THEN
  IF((Report(Pointer:Pointer) == '+').OR.    &
  (Report(Pointer:Pointer) == '-')) THEN
    GroupLength=9
  ELSE
    GroupLength=8
  END IF
END IF

!----------------------------------------------------------------------
! Check whether group represents weather in the vicinty.
! Move Pointer past first two characters (not in Descriptors or Weather)
! reduce the GroupLength by 2 and set the intensity value.
!----------------------------------------------------------------------

IF (REPORT(Pointer:Pointer+1) == 'VC' .AND.    &
GroupLength >= 4) THEN
  QUAL=5
  Pointer=Pointer+2
  GroupLength=GroupLength-2
ELSE
  ODD=MOD(GroupLength,2)
END IF

!----------------------------------------------------------------------
! If it is odd, check the first character is either a '+' or a '-', and
! set the intensity value accordingly.
!----------------------------------------------------------------------

IF_ODD: &
IF (ODD /= 0) THEN

IF_SIGN: &
  IF (REPORT(Pointer:Pointer) == '+') THEN
    QUAL=3
    Pointer=Pointer+1
    GroupLength=Grouplength-1
    EXPAND=.TRUE.
  ELSE IF (REPORT(Pointer:Pointer) == '-') THEN
    QUAL=1
    Pointer=Pointer+1
    GroupLength=GroupLength-1
    EXPAND=.TRUE.
  ELSE
    EXPAND=.FALSE.
  END IF IF_SIGN

!----------------------------------------------------------------------
! If it is even, check the first two characters of the group against
! phenomenon known not to have an intensity value.
!----------------------------------------------------------------------

ELSE
  IF (REPORT(Pointer:Pointer+1) /= 'MI' .AND. &
  REPORT(Pointer:Pointer+1) /= 'BC' .AND.       &
  REPORT(Pointer:Pointer+1) /= 'FG' .AND.       &
  REPORT(Pointer:Pointer+1) /= 'PR' .AND.       &
  REPORT(Pointer:Pointer+1) /= 'BR' .AND.       &
  REPORT(Pointer:Pointer+1) /= 'FU' .AND.       &
  REPORT(Pointer:Pointer+1) /= 'HZ') THEN
    QUAL=2
  END IF
  EXPAND=.TRUE.
END IF IF_ODD

!----------------------------------------------------------------------
! Put the The intensity value in the output array
!----------------------------------------------------------------------

RXpanAray(Displace+13)=QUAL

!----------------------------------------------------------------------
! If the expansion flag is set, then expand group. Otherwise exit.
!----------------------------------------------------------------------

IF_EXPAND: &
IF (EXPAND) THEN

  IF (GroupLength == 2) THEN
    COUNT=1
  ELSE IF (GroupLength == 4) THEN
    COUNT=2
  ELSE IF (GroupLength == 6) THEN
    COUNT=3
  ELSE IF (GroupLength == 8) THEN
    COUNT=4
  ELSE
    ERROR=.TRUE.
  END IF

!----------------------------------------------------------------------
! If the group length is greater than 2 then loop round the  descriptors
!----------------------------------------------------------------------

  IF (Count > 1) THEN
    DO WHILE (.NOT. GotDescriptor .AND. ID <= 8)
      IF(Report(Pointer:Pointer+1) == Descriptor(ID))THEN
        RXpanAray(Displace+14)=2**(ID-1)
        GotDescriptor=.TRUE.
        Pointer=Pointer+2
        Count=Count-1
      ELSE
        ID=ID+1
      END IF
    END DO
  END IF

!----------------------------------------------------------------------
! Since there can only be one descriptor the rest of the group must be
! weather phenomona.
!----------------------------------------------------------------------

DO_WTHR: &
  DO WHILE (Count > 0)
    Disp=Disp+1
    DO WHILE (WXCount <= 20 .AND. .NOT. GotWeather)
      IF (Report(Pointer:Pointer+1) == Weather(WXCount)) THEN
        RXpanAray(Displace+14+Disp)=2**(WXCount-1)
        GotWeather=.TRUE.
        Pointer=Pointer+2
        WXCount=1
      ELSE
        WXCount=WXCount+1
      END IF
    END DO
    GotWeather=.FALSE.
    Count=Count-1
  END DO DO_WTHR
END IF IF_EXPAND

!----------------------------------------------------------------------
! The pointer may not be incremented if weather groups havent been
! decoded. It may also point to the space after the weather groups if
! they have been decoded. In either case look for the space.
!----------------------------------------------------------------------

PointerIncrement=INDEX(Report(Pointer:ReportLength),' ')
IF ((Pointer+PointerIncrement) > ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+PointerIncrement
END IF

RETURN
END SUBROUTINE MTRWTR
