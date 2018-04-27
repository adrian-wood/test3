      SUBROUTINE MTRTRD(Report,Pointer,ReportLength,
     &                  RXpanAray,MoreTrend,Displace)

!----------------------------------------------------------------------
! Subroutine  : MTRTRD
!
! Purpose     : To decode METAR TREND information
!
! Description :
!
! Called By   : MTREXP
!
! Arguments     : NUM  TYPE I/O DESCRIPTION
!                 (1)  CHAR I   Metar Report being expanded
!                 (2)  INT  I/O Pointer to groups within report
!                 (3)  INT  I   REPORT length
!                 (4)  REAL I/O Expansion array
!                 (5)  LOG  O   Indicates more report to decode
!                 (6)  INT  I   Expansion array displacement
!
! NOTES:Report(Pointer:Pointer+3) looks at either NOSI, TEMP or BECM
!       This will be the starting point for the decode of the TREND info
!
! Calls to      : MTRPER, MTRWND, MTRWTR, MTRSKY
!
! Revision Info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:38$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrtrd.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:38    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:59  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:19:57  10:19:57  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
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
      INTEGER            WindGroupUnits
      INTEGER            WindKT
      INTEGER            WindMPS
      INTEGER            WindKMH
      INTEGER            CAVOK
      INTEGER            UnitLength
      INTEGER            I
      INTEGER            Displace
      INTEGER            IVALUE        !1.2 function to return integer
                                       !1.2 from a string.
!Declare Real
      REAL               RXpanAray(*)
      REAL               Missing
      REAL               FromHour
      REAL               FromMin
      REAL               TillHour
      REAL               TillMin
      REAL               AtHour
      REAL               AtMin
      REAL               VVVV
      REAL               RawSpeed2MS

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
      VVVV=Missing

      DO I=Displace,Displace+28
        RXpanAray(I)=Missing
      ENDDO

!----------------------------------------------------------------------
!If Report(Pointer:Pointer+3) .eq. NOSI then there is no further TREND
!info to decode. NOSIG is coded as 0 in tableb 008016. If its
!BECM then code 1 and TEMP is code 2
!----------------------------------------------------------------------

      IF (Report(Pointer:Pointer+3) .EQ. 'NOSI') THEN
        RXpanAray(Displace)=0.0
        MoreTrend=.FALSE.
      ELSEIF (Report(Pointer:Pointer+3) .EQ. 'BECM') THEN
        RXpanAray(Displace)=1.0
      ELSEIF (Report(Pointer:Pointer+3) .EQ. 'TEMP') THEN
        RXpanAray(Displace)=2.0
      ELSE
        RXpanAray(Displace)=Missing
        MoreTrend=.FALSE.
      ENDIF

!----------------------------------------------------------------------
!Decode the time period group. Ref: WMO Code Manual FM15 METAR for
!Information on valid groups.
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        Pointer=Pointer+6
        CALL MTRPER(Report,Pointer,RXpanAray,MoreTrend,ReportLength,
     &  Displace)
      ENDIF

!----------------------------------------------------------------------
!There maybe a wind/gust group present. If there is it must have the
!UNITS specified as either KT, MPS or KMH. An INDEX search is done
!for each of these in turn. Since only ever one of these Units can be
!present they are all added together into one variable. (If INDEX does
!not find the requested string it returns a value of zero)
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        WindKMH=INDEX(Report(Pointer:Pointer+10),'KMH')
        WindKT=INDEX(Report(Pointer:Pointer+10),'KT')
        WindMPS=INDEX(Report(Pointer:Pointer+10),'MPS')
        IF (WindKMH .GT. 0) THEN
          RawSpeed2MS=0.2778
          UnitLength=3
        ELSEIF (WindKT .GT. 0) THEN
          RawSpeed2MS=0.5145
          UnitLength=2
        ELSE
          RawSpeed2MS=1.0
          UnitLength=3
        ENDIF
        WindGroupUnits=WindKMH+WindKT+WindMPS
        IF (WindGroupUnits .GT. 0) THEN
          CALL MTRWND(Report,Pointer,WindGroupUnits,RXpanAray,
     &    RawSpeed2MS,MoreTrend,ReportLength,UnitLength,Displace)
        ENDIF
      ENDIF

!----------------------------------------------------------------------
!We may now have the word CAVOK or the Horizontal Visibility
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        CAVOK=INDEX(REPORT(Pointer:Pointer+4),'CAVOK')
        IF (CAVOK .GT. 0) THEN
          RXpanAray(Displace+11)=2
          RXpanAray(Displace+10)=Missing
          MoreTrend=.FALSE.
        ELSE
          RXpanAray(Displace+11)=Missing
        ENDIF
      ENDIF

      IF (MoreTrend) THEN
        IF((Report(Pointer:Pointer) .GE. '0').AND.
     &  (Report(Pointer:Pointer) .LE. '9').AND.
     &  (Report(Pointer+3:Pointer+3) .EQ. '0').AND.
     &  ((Report(Pointer+2:Pointer+2) .EQ. '0') .OR.
     &  (Report(Pointer+2:Pointer+2) .EQ. '5')).OR.
     &  (Report(Pointer:Pointer+3) .EQ. '9999')) THEN

!1.2 check that Report(POINTER:POINTER+3) has only numeric chars.
!1.2 and assign to VVVV.
            VVVV = FLOAT(IVALUE(Report(Pointer:Pointer+3)))     !1.2
            IF (VVVV .EQ. 9999.0) THEN                            !1.2
              VVVV=10000.                                       !1.2
            ENDIF                                               !1.2

          RXpanAray(Displace+10)=VVVV
          PointerIncrement=5
        ELSEIF((Report(Pointer:Pointer) .EQ. 'S') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'O') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'B') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'F') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'P') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'T') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'D') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'M') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'H') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'V') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'G') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'I') .OR.
     &        (Report(Pointer:Pointer) .EQ. 'R')) THEN
          PointerIncrement=0
        ELSE
          DO I=Displace,Displace+28
            RXpanAray(I)=Missing
          ENDDO
          MoreTrend=.FALSE.
        ENDIF
      ENDIF

      IF ((Pointer+PointerIncrement) .GE. ReportLength) THEN
        MoreTrend=.FALSE.
      ELSE
        Pointer=Pointer+PointerIncrement
      ENDIF

!----------------------------------------------------------------------
!Weather group w'w' or the word NSW (No Significant Weather)
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        IF (Report(Pointer:Pointer+2) .EQ. 'NSW') THEN
!         RXpanAray(Displace+12)=0.0
          RXpanAray(Displace+12)=8
          PointerIncrement=4
          IF (Pointer+PointerIncrement .GE. ReportLength) THEN
            MoreTrend=.FALSE.
          ELSE
            Pointer=Pointer+PointerIncrement
            MoreTrend=.TRUE.
          ENDIF
        ELSEIF (Report(Pointer:Pointer+2) .NE. 'SKC' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'NSC' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'FEW' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'SCT' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'BKN' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'OVC' .AND.
     &  Report(Pointer:Pointer+2) .NE. 'RMK' .AND.
     &  Report(Pointer:Pointer+1) .NE. 'VV' .AND.
     &  (Report(Pointer:Pointer) .GT. '9' .OR.
     &  Report(Pointer:Pointer) .LT. '0')) THEN
          RXpanAray(98)=Missing
          CALL MTRWTR(Report,Pointer,RXpanAray,MoreTrend,ReportLength,
     &    Displace)
        ENDIF
      ENDIF

!----------------------------------------------------------------------
!Decode cloud group(s)
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        IF(Report(Pointer:Pointer+2) .NE. 'RMK') THEN
          CALL MTRSKY(Report,Pointer,RXpanAray,MoreTrend,ReportLength,
     &    Displace)
        ENDIF
      ENDIF

!----------------------------------------------------------------------
!Decide if there is more change group info to decode up to a max of
!3 groups
!----------------------------------------------------------------------

      IF (MoreTrend) THEN
        IF((Report(Pointer:Pointer+3) .EQ. 'TEMP').OR.
     &    (Report(Pointer:Pointer+3) .EQ. 'BECM')) THEN
          MoreTrend=.TRUE.
        ELSE
          MoreTrend=.FALSE.
        ENDIF
      ENDIF

      RETURN
      END
