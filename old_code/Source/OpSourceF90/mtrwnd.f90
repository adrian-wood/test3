SUBROUTINE MTRWND(Report,Pointer,WindGroupUnits,RXpanAray,&
&RawSpeed2MS,MoreTrend,ReportLength,&                        !2.0
&UnitLength,Displace)                                       !2.0

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
! Revision Info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrwnd.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:59  usmdb
! Removed unused argument. Added initialisation for HEAD.
! Added copyright and modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:21:09  10:21:09  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
!
! Revision 1.1  98/06/11  13:34:50  13:34:50  usmdb (Generic MDB account)
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
CHARACTER*(*) Report

!Declare Integer
INTEGER       ReportLength
INTEGER       Pointer
INTEGER       WindGroupUnits
INTEGER       PointerIncrement
INTEGER       UnitLength
INTEGER       Displace
INTEGER       IVALUE

!Declare Logical
LOGICAL       MoreTrend

!Declare Real
REAL          RXpanAray(*)
REAL          RawWindSpeed
REAL          RawGustSpeed
REAL          RawSpeed2MS
REAL          Missing

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/mtrwnd.F,v $&
&'//' $Date: 26/01/2010 10:18:13$ $Revision: 1$'

Missing=-9999999.
RawWindSpeed=Missing
RawGustSpeed=Missing
PointerIncrement=0

!----------------------------------------------------------------------
!WindGroupUnits indicates the first character of the UNITS used for
!measuring the wind speed. The value of Pointer indicates the start of
!the wind group. 5 = dddff 6=dddfff 8=dddffGff 9=dddffGfff
!----------------------------------------------------------------------

IF (Report(Pointer:Pointer+2) .NE. 'VRB') THEN
  RXpanAray(Displace+7)=IVALUE(Report(Pointer:Pointer+2))
ENDIF

!1.2 Before each Read check using IVALUE that numeric characters
!    in relevent part of REPORT

IF (WindGroupUnits .EQ. 6) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))  !1.2
  PointerIncrement=6+UnitLength
ELSEIF (WindGroupUnits .EQ. 7) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+5)))  !1.2
  PointerIncrement=7+UnitLength
ELSEIF (WindGroupUnits .EQ. 9) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))  !1.2
  RawGustSpeed = FLOAT(IVALUE(REPORT(POINTER+6:POINTER+7)))  !1.2
  PointerIncrement=9+UnitLength
ELSEIF (WindGroupUnits .EQ. 10) THEN
  RawWindSpeed = FLOAT(IVALUE(REPORT(POINTER+3:POINTER+4)))  !1.2
  RawGustSpeed = FLOAT(IVALUE(REPORT(POINTER+6:POINTER+8)))  !1.2
  PointerIncrement=9+UnitLength+1
ENDIF

!----------------------------------------------------------------------
!Now convert the speed values to MS-1 and insert them into the
!expansion array.
!----------------------------------------------------------------------

IF (RawWindSpeed .GT. Missing) THEN
  RXpanAray(Displace+8)=RawWindSpeed*RawSpeed2MS
ENDIF

IF (RawGustSpeed .GT. Missing) THEN
  RXpanAray(Displace+9)=RawGustSpeed*RawSpeed2MS
ENDIF

!----------------------------------------------------------------------
!Check for End of report before inrementing Pointer
!----------------------------------------------------------------------
IF (Pointer+PointerIncrement .GE. ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+PointerIncrement
ENDIF

RETURN
END SUBROUTINE MTRWND
