SUBROUTINE MTRTRD(Report,Pointer,ReportLength, &
                  RXpanAray,MoreTrend,Displace)

!----------------------------------------------------------------------
! Subroutine  : MTRTRD
!
! Purpose     : To decode METAR TREND information
!
! Description :
!
! Called By   : MTREXP
!
! Arguments     : NUM  TYPE I-O DESCRIPTION
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
! REVISION INFO :
!
!
! $Workfile: mtrtrd.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 18/11/2010 13:37:42$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         18/11/2010 13:37:42    John Norton
!       Updated as part of merge batch 13.
!  5    MetDB_Refresh 1.4         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  4    MetDB_Refresh 1.3         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  3    MetDB_Refresh 1.2         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  2    MetDB_Refresh 1.1         22/10/2010 15:54:44    Rosemary Lavery USE
!       stmts added
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

USE MTRPER_MOD
USE MTRWND_MOD
USE MTRWTR_MOD
USE MTRSKY_MOD
USE IVALUE_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
INTEGER,           INTENT(IN)    ::  ReportLength
REAL,              INTENT(INOUT) ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  Displace

! Local Variables

INTEGER   ::  PointerIncrement
INTEGER   ::  WindGroupUnits
INTEGER   ::  WindKT
INTEGER   ::  WindMPS
INTEGER   ::  WindKMH
INTEGER   ::  CAVOK
INTEGER   ::  UnitLength
INTEGER   ::  I
REAL      ::  Missing
REAL      ::  FromHour
REAL      ::  FromMin
REAL      ::  TillHour
REAL      ::  TillMin
REAL      ::  AtHour
REAL      ::  AtMin
REAL      ::  VVVV
REAL      ::  RawSpeed2MS

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
END DO

!----------------------------------------------------------------------
!If Report(Pointer:Pointer+3) == NOSI then there is no further TREND
!info to decode. NOSIG is coded as 0 in tableb 008016. If its
!BECM then code 1 and TEMP is code 2
!----------------------------------------------------------------------

IF (Report(Pointer:Pointer+3) == 'NOSI') THEN
  RXpanAray(Displace)=0.0
  MoreTrend=.FALSE.
ELSE IF (Report(Pointer:Pointer+3) == 'BECM') THEN
  RXpanAray(Displace)=1.0
ELSE IF (Report(Pointer:Pointer+3) == 'TEMP') THEN
  RXpanAray(Displace)=2.0
ELSE
  RXpanAray(Displace)=Missing
  MoreTrend=.FALSE.
END IF

!----------------------------------------------------------------------
!Decode the time period group. Ref: WMO Code Manual FM15 METAR for
!Information on valid groups.
!----------------------------------------------------------------------

IF (MoreTrend) THEN
  Pointer=Pointer+6
  CALL MTRPER(Report,Pointer,RXpanAray,MoreTrend,ReportLength,  &
  Displace)
END IF

!----------------------------------------------------------------------
!There maybe a wind/gust group present. If there is it must have the
!UNITS specified as either KT, MPS or KMH. An INDEX search is done
!for each of these in turn. Since only ever one of these Units can be
!present they are all added together into one variable. (If INDEX does
!not find the requested string it returns a value of zero)
!----------------------------------------------------------------------

IF_WINDGP: &
IF (MoreTrend) THEN
  WindKMH=INDEX(Report(Pointer:Pointer+10),'KMH')
  WindKT=INDEX(Report(Pointer:Pointer+10),'KT')
  WindMPS=INDEX(Report(Pointer:Pointer+10),'MPS')
  IF (WindKMH > 0) THEN
    RawSpeed2MS=0.2778
    UnitLength=3
  ELSE IF (WindKT > 0) THEN
    RawSpeed2MS=0.5145
    UnitLength=2
  ELSE
    RawSpeed2MS=1.0
    UnitLength=3
  END IF
  WindGroupUnits=WindKMH+WindKT+WindMPS
  IF (WindGroupUnits > 0) THEN
    CALL MTRWND(Report,Pointer,WindGroupUnits,RXpanAray,  &
    RawSpeed2MS,MoreTrend,ReportLength,UnitLength,Displace)
  END IF
END IF IF_WINDGP

!----------------------------------------------------------------------
!We may now have the word CAVOK or the Horizontal Visibility
!----------------------------------------------------------------------

IF (MoreTrend) THEN
  CAVOK=INDEX(REPORT(Pointer:Pointer+4),'CAVOK')
  IF (CAVOK > 0) THEN
    RXpanAray(Displace+11)=2
    RXpanAray(Displace+10)=Missing
    MoreTrend=.FALSE.
  ELSE
    RXpanAray(Displace+11)=Missing
  END IF
END IF

IF_VISGP: &
IF (MoreTrend) THEN
  IF((Report(Pointer:Pointer) >= '0').AND.     &
  (Report(Pointer:Pointer) <= '9').AND.        &
  (Report(Pointer+3:Pointer+3) == '0').AND.    &
  ((Report(Pointer+2:Pointer+2) == '0') .OR. &
  (Report(Pointer+2:Pointer+2) == '5')).OR.    &
  (Report(Pointer:Pointer+3) == '9999')) THEN

!1.2 check that Report(POINTER:POINTER+3) has only numeric chars.
!1.2 and assign to VVVV.
      VVVV = FLOAT(IVALUE(Report(Pointer:Pointer+3)))
      IF (VVVV == 9999.0) THEN
        VVVV=10000.
      END IF

    RXpanAray(Displace+10)=VVVV
    PointerIncrement=5
  ELSE IF((Report(Pointer:Pointer) == 'S') .OR. &
        (Report(Pointer:Pointer) == 'O') .OR.    &
        (Report(Pointer:Pointer) == 'B') .OR.    &
        (Report(Pointer:Pointer) == 'F') .OR.    &
        (Report(Pointer:Pointer) == 'P') .OR.    &
        (Report(Pointer:Pointer) == 'T') .OR.    &
        (Report(Pointer:Pointer) == 'D') .OR.    &
        (Report(Pointer:Pointer) == 'M') .OR.    &
        (Report(Pointer:Pointer) == 'H') .OR.    &
        (Report(Pointer:Pointer) == 'V') .OR.    &
        (Report(Pointer:Pointer) == 'G') .OR.    &
        (Report(Pointer:Pointer) == 'I') .OR.    &
        (Report(Pointer:Pointer) == 'R')) THEN
    PointerIncrement=0
  ELSE
    DO I=Displace,Displace+28
      RXpanAray(I)=Missing
    END DO
    MoreTrend=.FALSE.
  END IF
END IF IF_VISGP

IF ((Pointer+PointerIncrement) >= ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+PointerIncrement
END IF

!----------------------------------------------------------------------
!Weather group w'w' or the word NSW (No Significant Weather)
!----------------------------------------------------------------------

IF_WTHRGP: &
IF (MoreTrend) THEN
  IF (Report(Pointer:Pointer+2) == 'NSW') THEN
!         RXpanAray(Displace+12)=0.0
    RXpanAray(Displace+12)=8
    PointerIncrement=4
    IF (Pointer+PointerIncrement >= ReportLength) THEN
      MoreTrend=.FALSE.
    ELSE
      Pointer=Pointer+PointerIncrement
      MoreTrend=.TRUE.
    END IF
  ELSE IF (Report(Pointer:Pointer+2) /= 'SKC' .AND. &
  Report(Pointer:Pointer+2) /= 'NSC' .AND.           &
  Report(Pointer:Pointer+2) /= 'FEW' .AND.           &
  Report(Pointer:Pointer+2) /= 'SCT' .AND.           &
  Report(Pointer:Pointer+2) /= 'BKN' .AND.           &
  Report(Pointer:Pointer+2) /= 'OVC' .AND.           &
  Report(Pointer:Pointer+2) /= 'RMK' .AND.           &
  Report(Pointer:Pointer+1) /= 'VV' .AND.            &
  (Report(Pointer:Pointer) > '9' .OR.                &
  Report(Pointer:Pointer) < '0')) THEN
    RXpanAray(98)=Missing
    CALL MTRWTR(Report,Pointer,RXpanAray,MoreTrend,ReportLength,  &
    Displace)
  END IF
END IF IF_WTHRGP

!----------------------------------------------------------------------
!Decode cloud group(s)
!----------------------------------------------------------------------

IF (MoreTrend) THEN
  IF(Report(Pointer:Pointer+2) /= 'RMK') THEN
    CALL MTRSKY(Report,Pointer,RXpanAray,MoreTrend,ReportLength,  &
    Displace)
  END IF
END IF

!----------------------------------------------------------------------
!Decide if there is more change group info to decode up to a max of
!3 groups
!----------------------------------------------------------------------

IF (MoreTrend) THEN
  IF((Report(Pointer:Pointer+3) == 'TEMP').OR.    &
    (Report(Pointer:Pointer+3) == 'BECM')) THEN
    MoreTrend=.TRUE.
  ELSE
    MoreTrend=.FALSE.
  END IF
END IF

RETURN
END SUBROUTINE MTRTRD
