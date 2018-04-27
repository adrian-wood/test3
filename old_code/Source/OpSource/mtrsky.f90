SUBROUTINE MTRSKY(Report,Pointer,RXpanAray,MoreTrend,  &
                  ReportLength,Displace)

!---------------------------------------------------------------------
! Program       : MTRSKY
!
! Purpose       : First routine in cloud decode for METAR TREND
!
! Description   : Detects what cloud information (if any) we have in
!                 the report and calls appropriate decode routine.
!
! Called By     : MTRTRD
!
! Arguments     : Num Type i-o Description
!                 (1) CHAR I   Metar Report being expanded
!                 (2) INT  I/O Pointer to groups within report
!                 (3) REAL O   Expansion Array
!                 (4) LOG  O   Indicates more report to expand
!                 (5) INT  I   Length of report being expanded
!                 (6) INT  I   Expansion array displacement
!
! Call to       : None
!
! REVISION INFO :
!
!
! $Workfile: mtrsky.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 19/11/2010 14:26:27$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         19/11/2010 14:26:27    Alison Weir     Add
!       USE statements, following merge review
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


USE MTRCLD_MOD
USE MTRVHT_MOD


IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(IN)    ::  Report
INTEGER,           INTENT(INOUT) ::  Pointer
REAL,              INTENT(OUT)   ::  RXpanAray(:)
LOGICAL,           INTENT(OUT)   ::  MoreTrend
INTEGER,           INTENT(IN)    ::  ReportLength
INTEGER,           INTENT(IN)    ::  Displace

! Local Variables and initialized flags

INTEGER               ::  PointerIncrement
LOGICAL               ::  CloudGroups =.FALSE.
LOGICAL               ::  VisHeight =.FALSE.

SAVE

!----------------------------------------------------------------------
!Check to see if we have any forecasts cloud information. The F/c may
!be Sky Clear (SKC), No. Sig. Cloud (NSC), one or more cloud amount/
!height/type groups or Horizontal Vis&Height Base group. SKC and NSC
!are set in the expansion array here - other cloud information is dealt
!with in different routines. If its not recognised as a cloud info.
!group then the pointer will not be incremented (It may be RMK)
!----------------------------------------------------------------------

IF_CLOUD: &
IF (Report(Pointer:Pointer+2) == 'NSC') THEN
  RXpanAray(Displace+18)=1
  PointerIncrement=4
ELSE IF (Report(Pointer:Pointer+2) == 'SKC') THEN
  RXpanAray(Displace+18)=4
  PointerIncrement=4
ELSE IF((Report(Pointer:Pointer+2) == 'SCT').OR. &
(Report(Pointer:Pointer+2) == 'BKN').OR.          &
(Report(Pointer:Pointer+2) == 'FEW').OR.          &
(Report(Pointer:Pointer+2) == 'OVC')) THEN
  CloudGroups=.TRUE.
ELSE IF(Report(Pointer:Pointer+1) == 'VV') THEN
  VisHeight=.TRUE.
ELSE
  PointerIncrement=0
END IF IF_CLOUD


!----------------------------------------------------------------------
!Call subroutines to decode cloud groups with cloud or vis.
!----------------------------------------------------------------------

IF (CloudGroups) THEN
  CALL MTRCLD(Report,Pointer,RXpanAray,MoreTrend, &
              ReportLength,Displace)
ELSE IF(VisHeight) THEN
  CALL MTRVHT(Report,Pointer,RXpanAray,Displace)
END IF

!----------------------------------------------------------------------
!Check that we dont increment the pointer beyond the end of the report.
!If its okay increment the pointer past the cloud information groups
!and then return to calling routine.
!----------------------------------------------------------------------

IF((Pointer+PointerIncrement) >= ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+PointerIncrement
END IF

RETURN
END SUBROUTINE MTRSKY
