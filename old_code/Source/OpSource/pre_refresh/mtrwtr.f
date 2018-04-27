      SUBROUTINE MTRWTR(Report,Pointer,RXpanAray,MoreTrend,
     &                  ReportLength,Displace)                     !2.0

!----------------------------------------------------------------------
! Program       : MTRWTR
!
! Purpose       : To decode the Forecast wx groups in the TREND section
!
! Description   : Look for and decode any wx code. No qulaity control
!                 is performed so invalid combinations will be returned
!                 to the user to decide upon appropriate action.
!
! Called By     : MTRTRD
!
! Arguments     : Number Type  I/O Description
!                 (1)    CHAR  I   Metar Report being decoded
!                 (2)    INT   I/O Pointer within REPORT
!                 (3)    REAL  I/O Expansion Array
!                 (4)    LOG   I/O Indicates more report to come
!                 (5)    INT   I   Report Length
!                 (6)    INT   I   Displacement in expansion array
!
! Calls to      : NONE
!
! Revision Info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:39$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrwtr.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:39    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:05:03  usmdb
! End DO WHILE loop if WXCount exceeds 20 instead of 21, as
! array Weather is only declared size 20 - S.Cox
!
! Revision 2.0  2001/01/08  11:59:00  11:59:00  usmdb (Generic MetDB account)
! Moved data statements before executable statements.
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/06/11  13:34:50  13:34:50  usmdb (Generic MDB account
! Initial revision
!
! 15/06/98 First Version
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

      CHARACTER     HEAD*132
      CHARACTER     REPORT*(*)
      CHARACTER*2   Descriptor(8)
      CHARACTER*2   Weather(20)

!Declare Integer

      INTEGER       COUNT
      INTEGER       GroupLength
      INTEGER       ID
      INTEGER       ODD
      INTEGER       Pointer
      INTEGER       QUAL
      INTEGER       Disp
      INTEGER       WXCount
      INTEGER       ReportLength
      INTEGER       PointerIncrement
      INTEGER       Displace

!Declare Logical

      LOGICAL       ERROR
      LOGICAL       EXPAND
      LOGICAL       GotDescriptor
      LOGICAL       GotWeather
      LOGICAL       MoreTrend

!Declare Real
      REAL          RXpanAray(*)

!----------------------------------------------------------------------
!The following DATA statements contain a list of the possible
!descriptors and weather phenomena. The DATA statement is in the same
!order as BUFR CODE TABLES 020193 and 020194. Since the displacement
!within the DATA statement represents the code table figure the order
!should not be changed.
!----------------------------------------------------------------------

      DATA Descriptor/'MI','BC','DR','BL','SH','TS','FZ','PR'/
      DATA Weather/'DZ','RA','SN','SG','IC','PE','GR',
     &'GS','BR','FG','FU','VA','DU','SA','HZ','PO','SQ','FC','SS','DS'/

      SAVE

! Initialise variables.
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrwtr.F,v $
     &'//'$ $Date: 30/01/2006 20:23:39$ $Revision: 1$'
      ERROR=.FALSE.
      EXPAND=.FALSE.
      QUAL=-9999999.           ! Default intensity is missing
      GroupLength=0
      Disp=0
      GotDescriptor=.FALSE.
      GotWeather=.FALSE.
      ID=1
      WXCount=1
      Count=0
      ODD=0

!----------------------------------------------------------------------
!Calculate the GroupLength
!----------------------------------------------------------------------

      GroupLength=INDEX(Report(Pointer:),' ')-1
      IF (GroupLength .LE. 1) THEN
        EXPAND=.FALSE.
      ENDIF

      IF (GroupLength .GT. 9) THEN
        IF((Report(Pointer:Pointer) .EQ. '+').OR.
     &  (Report(Pointer:Pointer) .EQ. '-')) THEN
          GroupLength=9
        ELSE
          GroupLength=8
        ENDIF
      ENDIF

!----------------------------------------------------------------------
! Check whether group represents weather in the vicinty.
! Move Pointer past first two characters (not in Descriptors or Weather)
! reduce the GroupLength by 2 and set the intensity value.
!----------------------------------------------------------------------

      IF (REPORT(Pointer:Pointer+1) .EQ. 'VC' .AND.
     &GroupLength .GE. 4) THEN
        QUAL=5
        Pointer=Pointer+2
        GroupLength=GroupLength-2
      ELSE
        ODD=MOD(GroupLength,2)
      ENDIF

!----------------------------------------------------------------------
! If it is odd, check the first character is either a '+' or a '-', and
! set the intensity value accordingly.
!----------------------------------------------------------------------

      IF (ODD .NE. 0) THEN
        IF (REPORT(Pointer:Pointer) .EQ. '+') THEN
          QUAL=3
          Pointer=Pointer+1
          GroupLength=Grouplength-1
          EXPAND=.TRUE.
        ELSEIF (REPORT(Pointer:Pointer) .EQ. '-') THEN
          QUAL=1
          Pointer=Pointer+1
          GroupLength=GroupLength-1
          EXPAND=.TRUE.
        ELSE
          EXPAND=.FALSE.
        ENDIF

!----------------------------------------------------------------------
! If it is even, check the first two characters of the group against
! phenomenon known not to have an intensity value.
!----------------------------------------------------------------------

      ELSE
        IF (REPORT(Pointer:Pointer+1) .NE. 'MI' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'BC' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'FG' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'PR' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'BR' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'FU' .AND.
     &  REPORT(Pointer:Pointer+1) .NE. 'HZ') THEN
          QUAL=2
        ENDIF
        EXPAND=.TRUE.
      ENDIF

!----------------------------------------------------------------------
!Put the The intensity value in the output array
!----------------------------------------------------------------------

      RXpanAray(Displace+13)=QUAL

!----------------------------------------------------------------------
! If the expansion flag is set, then expand group. Otherwise exit.
!----------------------------------------------------------------------

      IF (EXPAND) THEN

        IF (GroupLength .EQ. 2) THEN
          COUNT=1
        ELSEIF (GroupLength .EQ. 4) THEN
          COUNT=2
        ELSEIF (GroupLength .EQ. 6) THEN
          COUNT=3
        ELSEIF (GroupLength .EQ. 8) THEN
          COUNT=4
        ELSE
          ERROR=.TRUE.
        ENDIF

!----------------------------------------------------------------------
!If the group length is greater than 2 then loop round the  descriptors
!----------------------------------------------------------------------

        IF (Count .GT. 1) THEN
          DO WHILE (.NOT. GotDescriptor .AND. ID .LE. 8)
            IF(Report(Pointer:Pointer+1) .EQ. Descriptor(ID))THEN
              RXpanAray(Displace+14)=2**(ID-1)
              GotDescriptor=.TRUE.
              Pointer=Pointer+2
              Count=Count-1
            ELSE
              ID=ID+1
            ENDIF
          ENDDO
        ENDIF

!----------------------------------------------------------------------
!Since there can only be one descriptor the rest of the group must be
!weather phenomona.
!----------------------------------------------------------------------

        DO WHILE (Count .GT. 0)
          Disp=Disp+1
          DO WHILE (WXCount .LE. 20 .AND. .NOT. GotWeather)         !2.1
            IF (Report(Pointer:Pointer+1) .EQ. Weather(WXCount)) THEN
              RXpanAray(Displace+14+Disp)=2**(WXCount-1)
              GotWeather=.TRUE.
              Pointer=Pointer+2
              WXCount=1
            ELSE
              WXCount=WXCount+1
            ENDIF
          ENDDO
          GotWeather=.FALSE.
          Count=Count-1
        ENDDO
      ENDIF

!----------------------------------------------------------------------
!The pointer may not be incremented if weather groups havent been
!decoded. It may also point to the space after the weather groups if
!they have been decoded. In either case look for the space.
!----------------------------------------------------------------------

      PointerIncrement=INDEX(Report(Pointer:),' ')
      IF ((Pointer+PointerIncrement) .GT. ReportLength) THEN
        MoreTrend=.FALSE.
      ELSE
        Pointer=Pointer+PointerIncrement
      ENDIF

      RETURN
      END
