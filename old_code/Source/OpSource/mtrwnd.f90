SUBROUTINE MTRWND(Report,Pointer,WindGroupUnits,RXpanAray,  &
                  RawSpeed2MS,MoreTrend,ReportLength,       &
                  UnitLength,Displace)

!----------------------------------------------------------------------
! Program       : MTRWND
!
! Purpose       : To decode wind group in METAR TREND
!
! Description   : Look for possible wind groups and decode into the array
!
! Called By     : MTRTRD
!
! Arguments     : Number Type I/O Description
!                 (1)   CHR  I   Metar Report being decoded
!                 (2)   INT  I/O Pointer in REPORT
!                 (3)   INT  I   Pointer to start of UNITS value in wind
!                 (4)   REAL I/O Expansion Array
!                 (5)   INT  I   Conversion to use to get ms-1
!                 (6)   LOG  I/O Indicate More Trend/Report to come
!                 (7)   INT  I   Length of REPORT
!                 (8)   INT  I   Length of UNITS group
!                 (9)   INT  I   Displacement value in expansion array
!
! REVISION INFO :
!
!
! $Workfile: mtrwnd.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/11/2010 17:13:53$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  1    MetDB_Refresh 1.0         10/11/2010 11:46:56    Rosemary Lavery
!       Initial Port to F90
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

USE IVALUE_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
INTEGER,           INTENT(IN)    ::  WindGroupUnits
REAL,              INTENT(INOUT) ::  RXpanAray(:)
REAL,              INTENT(IN)    ::  RawSpeed2MS
LOGICAL,           INTENT(INOUT) ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  UnitLength
INTEGER,           INTENT(IN)    ::  Displace

! Local Parameters

REAL, PARAMETER      ::  MISSING = -9999999.

! Local Scalars

INTEGER              ::  PointerIncrement = 0
REAL                 ::  RawWindSpeed = MISSING
REAL                 ::  RawGustSpeed = MISSING

SAVE

!----------------------------------------------------------------------
!WindGroupUnits indicates the first character of the UNITS used for
!measuring the wind speed. The value of Pointer indicates the start of
!the wind group. 5 = dddff 6=dddfff 8=dddffGff 9=dddffGfff
!----------------------------------------------------------------------

IF (Report(Pointer:Pointer+2) /= 'VRB') THEN
  RXpanAray(Displace+7)=IVALUE(Report(Pointer:Pointer+2))
END IF

!1.2 Before each Read check using IVALUE that numeric characters
!    in relevent part of REPORT

IF_WINDUNIT: &
IF (WindGroupUnits == 6) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))
  PointerIncrement=6+UnitLength
ELSE IF (WindGroupUnits == 7) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+5)))
  PointerIncrement=7+UnitLength
ELSE IF (WindGroupUnits == 9) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))
  RawGustSpeed = FLOAT(IVALUE(REPORT(POINTER+6:POINTER+7)))
  PointerIncrement=9+UnitLength
ELSE IF (WindGroupUnits == 10) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))
  RawGustSpeed = FLOAT(IVALUE(REPORT(POINTER+6:POINTER+8)))
  PointerIncrement=9+UnitLength+1
END IF IF_WINDUNIT

!----------------------------------------------------------------------
!Now convert the speed values to MS-1 and insert them into the
!expansion array.
!----------------------------------------------------------------------

IF (RawWindSpeed > MISSING) THEN
  RXpanAray(Displace+8)=RawWindSpeed*RawSpeed2MS
END IF

IF (RawGustSpeed > MISSING) THEN
  RXpanAray(Displace+9)=RawGustSpeed*RawSpeed2MS
END IF

!----------------------------------------------------------------------
!Check for End of report before inrementing Pointer
!----------------------------------------------------------------------
IF (Pointer+PointerIncrement >= ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+PointerIncrement
END IF

RETURN
END SUBROUTINE MTRWND
