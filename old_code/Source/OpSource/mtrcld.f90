SUBROUTINE MTRCLD(Report,Pointer,RXpanAray,MoreTrend, &
                  ReportLength,Displace)                     !2.0

!----------------------------------------------------------------------
! Program       : MTRCLD
!
! Purpose       : To decode cloud groups in a METAR TREND
!
! Description   : Loop round all the cloud groups and set appropriate
!                 values in the output array.
!
! Called By     : MTRSKY
!
! Calls To      : NONE
!
! Revision info :
!
! $Revision: 3$
! $Date: 19/06/2011 10:22:57$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrcld.F,v $
!
! Change Record :
!
! $Log:
!  3    MetDB_Refresh 1.2         19/06/2011 10:22:57    Sheila Needham  Add
!       explicit string lengths
!  2    MetDB_Refresh 1.1         22/11/2010 17:59:36    Stan Kellett    REPORT
!        REPORTLEN and DISPLACE arguments changed to intent(IN)
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.0  2001/01/08 11:58:54  usmdb
! Removed unused dummy argument POINTERINCREMENT. Added
! copyright & modified header - S.Cox
!
! Revision 1.2  2000/03/10  10:20:55  10:20:55  usmdb (Generic MDB account)
! 20/03/2000 Addition of error checking, to check that elements
!            stored, have expected numerical values.
!
! Revision 1.1  98/06/11  13:35:07  13:35:07  usmdb (Generic MDB account)
! Initial revision
!
! 15/06/98  First Version - Jon Lewthwaite
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

! Use statements:
! <Interfaces>

!None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN) ::  Report
INTEGER,      INTENT(INOUT) ::  Pointer
REAL,         INTENT(INOUT) ::  RXpanAray(*)
LOGICAL,      INTENT(INOUT) ::  MoreTrend
INTEGER,      INTENT(IN) ::  ReportLength
INTEGER,      INTENT(IN) ::  Displace

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  METRES = 30

!Declare Integer
INTEGER      ::  LoopCount
INTEGER      ::  PtrCloudGroup_1
INTEGER      ::  PtrGroup_2
INTEGER      ::  PtrCloudGroup_2
INTEGER      ::  PtrGroup_3
INTEGER      ::  PtrCloudGroup_3
INTEGER      ::  CloudGroups
INTEGER      ::  IVALUE !1.2 Function to read integer from string

!Declare Logical

!Decale Real
REAL         ::  CLDAMT
REAL         ::  CLDTYPE
REAL         ::  CLDHT

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Initialise variables
SAVE


CLDAMT=-9999999.
CLDTYPE=-9999999.
CLDHT=-9999999.
LoopCount=0
CloudGroups=1

!----------------------------------------------------------------------
!There maybe more than one layer of significant cloud. Discover how
!many cloud groups there are to decode (MAX=3)
!Since only one of FEW, SCT, BKN or OVC can be present in any one
!cloud group we can use this to determine how many cloud groups there
!are and also how far in the string Report these groups start.
!----------------------------------------------------------------------
 PtrCloudGroup_1=Pointer          !Keep current position

 PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:ReportLength),'FEW')
 IF (PtrGroup_2  ==  0) THEN
   PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:ReportLength),'SCT')
 END IF
 IF (PtrGroup_2  ==  0) THEN
   PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:ReportLength),'BKN')
 END IF
 IF (PtrGroup_2  ==  0) THEN
   PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:ReportLength),'OVC')
 END IF
 IF (PtrGroup_2  >  0) THEN
   PtrCloudGroup_2=PtrGroup_2+PtrCloudGroup_1+2
   CloudGroups=2
 END IF
IFLABEL1: &
 IF (CloudGroups  ==  2) THEN
   PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:ReportLength),'FEW')
   IF (PtrGroup_3  ==  0) THEN
     PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:ReportLength),'SCT')
   END IF
   IF (PtrGroup_3  ==  0) THEN
     PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:ReportLength),'BKN')
   END IF
   IF (PtrGroup_3  ==  0) THEN
     PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:ReportLength),'OVC')
   END IF
   IF (PtrGroup_3  >  0) THEN
     PtrCloudGroup_3=PtrGroup_3+PtrCloudGroup_2+2
     CloudGroups=3
   END IF
 END IF IFLABEL1
!----------------------------------------------------------------------
!Loop round the total number of cloud groups.
!----------------------------------------------------------------------

DOLABEL1: &
DO WHILE (CloudGroups  >  0)
   LoopCount=LoopCount+1
!----------------------------------------------------------------------
!Depending on which cloud group is being expanded - reset pointer to
!the start of the cloud group to expand.
!----------------------------------------------------------------------

   IF (LoopCount  ==  1) THEN
     Pointer=PtrCloudGroup_1
   ELSE IF (LoopCount ==  2) THEN
     Pointer=PtrCloudGroup_2
   ELSE IF (LoopCount ==  3) THEN
     Pointer=PtrCloudGroup_3
   END IF
!----------------------------------------------------------------------
!  get cloud amount
!----------------------------------------------------------------------

  IF (REPORT(Pointer:Pointer+2)  ==  'SCT') THEN
    CLDAMT=11.
  ELSE IF (REPORT(Pointer:Pointer+2) ==  'BKN') THEN
    CLDAMT=12.
  ELSE IF (REPORT(Pointer:Pointer+2) ==  'OVC') THEN
    CLDAMT=8.
  ELSE IF (REPORT(Pointer:Pointer+2) ==  'FEW') THEN
    CLDAMT=13.
  END IF

!----------------------------------------------------------------------
! Get cloud height
!----------------------------------------------------------------------

!1.2 Check that READ is of numeric Character
  CLDHT = FLOAT(IVALUE(REPORT(Pointer+3:Pointer+5)))        !1.2
  IF (CLDHT /= -9999999.0) THEN                             !1.2
    CLDHT=CLDHT*METRES                                      !1.2
  END IF                                                    !1.2

!----------------------------------------------------------------------
! Check whether group is deep convective cloud type Towering Cumulus
!(TCU) or CB
!----------------------------------------------------------------------

  IF (REPORT(Pointer+6:Pointer+8)  ==  'TCU') THEN
    CLDTYPE=8.
  ELSE IF (REPORT(Pointer+6:Pointer+7) ==  'CB') THEN
    CLDTYPE=9.
  END IF

!----------------------------------------------------------------------
!Set the expanded values in the expansion array. Values 102-110
!----------------------------------------------------------------------
  RXpanAray(Displace+16+(LoopCount*3))=CLDAMT
  RXpanAray(Displace+17+(LoopCount*3))=CLDTYPE
  RXpanARAY(Displace+18+(LoopCount*3))=CLDHT

!----------------------------------------------------------------------
!Decrement the number of cloud groups left to expand and loop round
!----------------------------------------------------------------------
  CloudGroups=CloudGroups-1
END DO DOLABEL1

!----------------------------------------------------------------------
!Check that we dont go beyond the end of the report - increment pointer
!if this is okay.
!----------------------------------------------------------------------

IF ((Pointer+7)  >  ReportLength) THEN
  MoreTrend=.FALSE.
ELSE
  Pointer=Pointer+7
END IF

RETURN
END SUBROUTINE MTRCLD
