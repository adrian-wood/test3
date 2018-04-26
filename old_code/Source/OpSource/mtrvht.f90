SUBROUTINE MTRVHT(Report,Pointer,RXpanAray,Displace)

!----------------------------------------------------------------------
!
! Program       : MTRVHT
!
! Purpose       : To decode vertical visibility in METAR TREND
!
! Decsription   : The vertical vis. (if reported) has the format VVxxx
!                 where VV are letters indicating vertical vis. and xxx
!                 is the height in feet.
!
! Called By     : MTRTRD
!
! Arguments     : NUM  TYPE  I-O  DESCRIPTION
!                 (1)  CHAR  I    METAR report being decoded.
!                 (2)  INT   I/O  Pointer to VVxxx group in REPORT
!                 (3)  REAL  I/O  Decoded values array
!                 (4)  INT   I    Displacement for expansion array
!
! Calls To      : None
!
! REVISION INFO :
!
!
! $Workfile: mtrvht.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 12/11/2010 17:13:53$
!
! CHANGE RECORD :
!
! $Log:
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

USE IVALUE_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  REPORT
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(INOUT) ::  RXpanAray(:)
INTEGER,           INTENT(IN)    ::  Displace

! Local Scalars

INTEGER              ::  PointerIncrement
REAL                 ::  VVHeight = -9999999.

SAVE

!----------------------------------------------------------------------
!Pointer should be looking at VV but check this first. If not then
!dont increment the pointer and return to the calling program. If it
!does then decode the vertical vis.
!----------------------------------------------------------------------
IF (Report(Pointer:Pointer+1) == 'VV') THEN

!1.2 Check that Pointer+2:Pointer+4 does have numeric Characters
!1.2 using IVALUE assigning to VVHEIGHT
  VVHEIGHT = FLOAT(IVALUE(REPORT(POINTER+2:POINTER+4)))

  PointerIncrement=6
ELSE
  PointerIncrement=0
END IF

!----------------------------------------------------------------------
!Pass the decoded value to the expansion array and return to calling
!routine.
!----------------------------------------------------------------------

RXpanAray(Displace+28)=VVHeight*30.

RETURN
END SUBROUTINE MTRVHT
