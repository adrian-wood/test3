      SUBROUTINE MTRSKY(Report,Pointer,RXpanAray,MoreTrend,
     &                  ReportLength,Displace)

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
! Arguments     : Num Type I/O Description
!                 (1) CHAR I   Metar Report being expanded
!                 (2) INT  I/O Pointer to groups within report
!                 (3) REAL O   Expansion Array
!                 (4) LOG  O   Indicates more report to expand
!                 (5) INT  I   Length of report being expanded
!                 (6) INT  I   Expansion array displacement
!
! Call to       : None
!
! Revision Info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrsky.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:37    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:58  usmdb
! Remove POINTERINCREMENT argument from MTRCLD call as it
! is not used within MTRCLD. Added missing HEAD= line. Added
! copyright & modified header - S.Cox
!
! Revision 1.1  98/06/11  13:34:49  13:34:49  usmdb (Generic MDB account)
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
      CHARACTER*(*)    Report

!Declare Integer
      INTEGER          ReportLength
      INTEGER          Pointer
      INTEGER          PointerIncrement
      INTEGER          Displace

!Declare Real
      REAL             RXpanAray(*)

!Declare Logical
      LOGICAL          MoreTrend
      LOGICAL          CloudGroups
      LOGICAL          VisHeight

      SAVE

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrsky.F,v $
     &'//' $Date: 30/01/2006 20:23:37$ $Revision: 1$'                                 !2.0

      CloudGroups=.FALSE.
      VisHeight=.FALSE.

!----------------------------------------------------------------------
!Check to see if we have any forecasts cloud information. The F/c may
!be Sky Clear (SKC), No. Sig. Cloud (NSC), one or more cloud amount/
!height/type groups or Horizontal Vis&Height Base group. SKC and NSC
!are set in the expansion array here - other cloud information is dealt
!with in different routines. If its not recognised as a cloud info.
!group then the pointer will not be incremented (It may be RMK)
!----------------------------------------------------------------------

      IF (Report(Pointer:Pointer+2) .EQ. 'NSC') THEN
        RXpanAray(Displace+18)=1
        PointerIncrement=4
      ELSEIF (Report(Pointer:Pointer+2) .EQ. 'SKC') THEN
        RXpanAray(Displace+18)=4
        PointerIncrement=4
      ELSEIF((Report(Pointer:Pointer+2) .EQ. 'SCT').OR.
     &(Report(Pointer:Pointer+2) .EQ. 'BKN').OR.
     &(Report(Pointer:Pointer+2) .EQ. 'FEW').OR.
     &(Report(Pointer:Pointer+2) .EQ. 'OVC')) THEN
        CloudGroups=.TRUE.
      ELSEIF(Report(Pointer:Pointer+1) .EQ. 'VV') THEN
        VisHeight=.TRUE.
      ELSE
        PointerIncrement=0
      ENDIF


!----------------------------------------------------------------------
!Call subroutines to decode cloud groups with cloud or vis.
!----------------------------------------------------------------------

      IF (CloudGroups) THEN
        CALL MTRCLD(Report,Pointer,RXpanAray,MoreTrend,            !2.0
     &              ReportLength,Displace)                         !2.0
      ELSEIF(VisHeight) THEN
        CALL MTRVHT(Report,Pointer,RXpanAray,Displace)
      ENDIF

!----------------------------------------------------------------------
!Check that we dont increment the pointer beyond the end of the report.
!If its okay increment the pointer past the cloud information groups
!and then return to calling routine.
!----------------------------------------------------------------------

      IF((Pointer+PointerIncrement) .GE. ReportLength) THEN
        MoreTrend=.FALSE.
      ELSE
        Pointer=Pointer+PointerIncrement
      ENDIF

      RETURN
      END
