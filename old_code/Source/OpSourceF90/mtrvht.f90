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
! Arguments     : NUM  TYPE  I/O  DESCRIPTION
!                 (1)  CHAR  I    METAR report being decoded.
!                 (2)  INT   I/O  Pointer to VVxxx group in REPORT
!                 (3)  REAL  I/O  Decoded values array
!                 (4)  INT   I    Displacement for expansion array
!
! Calls To      : None
!
! Revision Info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrvht.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:59  usmdb
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.2  2000/03/10  10:12:30  10:12:30  usmdb (Generic MDB account)
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
CHARACTER*132    HEAD
CHARACTER*(*)    REPORT

!Declare Integer
INTEGER          PointerIncrement
INTEGER          Pointer
INTEGER          Displace
INTEGER          IVALUE !1.2 Function to read integer from string

!Declare Real
REAL             RXpanAray(*)
REAL             VVHeight

SAVE

HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/mtrvht.F,v $&
&'//'$Date: 26/01/2010 10:18:13$ $Revision: 1$'
VVHeight=-9999999.

!----------------------------------------------------------------------
!Pointer should be looking at VV but check this first. If not then
!dont increment the pointer and return to the calling program. If it
!does then decode the vertical vis.
!----------------------------------------------------------------------
IF (Report(Pointer:Pointer+1) .EQ. 'VV') THEN

!1.2 Check that Pointer+2:Pointer+4 does have numeric Characters
!1.2 using IVALUE assigning to VVHEIGHT
        VVHEIGHT = FLOAT(IVALUE(REPORT(POINTER+2:POINTER+4)))     !1.2

  PointerIncrement=6
ELSE
  PointerIncrement=0
ENDIF

!----------------------------------------------------------------------
!Pass the decoded value to the expansion array and return to calling
!routine.
!----------------------------------------------------------------------

RXpanAray(Displace+28)=VVHeight*30.

RETURN
END SUBROUTINE MTRVHT
