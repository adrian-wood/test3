      SUBROUTINE MTRCLD(Report,Pointer,RXpanAray,MoreTrend,
     &                  ReportLength,Displace)                     !2.0

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
! $Revision: 1$
! $Date: 30/01/2006 20:23:30$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mtrcld.F,v $
!
! Change Record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:30    Sheila Needham  
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

      IMPLICIT NONE

! Declare Character
      CHARACTER*132    HEAD
      CHARACTER*(*)    Report

!Declare Integer
      INTEGER          ReportLength
      INTEGER          Pointer
      INTEGER          LoopCount
      INTEGER          PtrCloudGroup_1
      INTEGER          PtrGroup_2
      INTEGER          PtrCloudGroup_2
      INTEGER          PtrGroup_3
      INTEGER          PtrCloudGroup_3
      INTEGER          CloudGroups
      INTEGER          METRES
      INTEGER          Displace
      INTEGER          IVALUE !1.2 Function to read integer from string

!Declare Logical
      LOGICAL          MoreTrend

!Decale Real
      REAL             RXpanAray(*)
      REAL             CLDAMT
      REAL             CLDTYPE
      REAL             CLDHT

! Initialise variables
      SAVE
      PARAMETER (METRES=30)

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/mtrcld.F,v $
     &'//'$ $Date: 30/01/2006 20:23:30$ $Revision: 1$'

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

       PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:),'FEW')
       IF (PtrGroup_2 .EQ. 0) THEN
         PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:),'SCT')
       ENDIF
       IF (PtrGroup_2 .EQ. 0) THEN
         PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:),'BKN')
       ENDIF
       IF (PtrGroup_2 .EQ. 0) THEN
         PtrGroup_2=INDEX(Report(PtrCloudGroup_1+3:),'OVC')
       ENDIF
       IF (PtrGroup_2 .GT. 0) THEN
         PtrCloudGroup_2=PtrGroup_2+PtrCloudGroup_1+2
         CloudGroups=2
       ENDIF
       IF (CloudGroups .EQ. 2) THEN
         PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:),'FEW')
         IF (PtrGroup_3 .EQ. 0) THEN
           PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:),'SCT')
         ENDIF
         IF (PtrGroup_3 .EQ. 0) THEN
           PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:),'BKN')
         ENDIF
         IF (PtrGroup_3 .EQ. 0) THEN
           PtrGroup_3=INDEX(Report(PtrCloudGroup_2+3:),'OVC')
         ENDIF
         IF (PtrGroup_3 .GT. 0) THEN
           PtrCloudGroup_3=PtrGroup_3+PtrCloudGroup_2+2
           CloudGroups=3
         ENDIF
       ENDIF
!----------------------------------------------------------------------
!Loop round the total number of cloud groups.
!----------------------------------------------------------------------

       DO WHILE (CloudGroups .GT. 0)
         LoopCount=LoopCount+1
!----------------------------------------------------------------------
!Depending on which cloud group is being expanded - reset pointer to
!the start of the cloud group to expand.
!----------------------------------------------------------------------

         IF (LoopCount .EQ. 1) THEN
           Pointer=PtrCloudGroup_1
         ELSEIF (LoopCount .EQ. 2) THEN
           Pointer=PtrCloudGroup_2
         ELSEIF (LoopCount .EQ. 3) THEN
           Pointer=PtrCloudGroup_3
         ENDIF
!----------------------------------------------------------------------
!  get cloud amount
!----------------------------------------------------------------------

        IF (REPORT(Pointer:Pointer+2) .EQ. 'SCT') THEN
          CLDAMT=11.
        ELSEIF (REPORT(Pointer:Pointer+2) .EQ. 'BKN') THEN
          CLDAMT=12.
        ELSEIF (REPORT(Pointer:Pointer+2) .EQ. 'OVC') THEN
          CLDAMT=8.
        ELSEIF (REPORT(Pointer:Pointer+2) .EQ. 'FEW') THEN
          CLDAMT=13.
        ENDIF

!----------------------------------------------------------------------
! Get cloud height
!----------------------------------------------------------------------

!1.2 Check that READ is of numeric Character
        CLDHT = FLOAT(IVALUE(REPORT(Pointer+3:Pointer+5)))        !1.2
        IF (CLDHT.NE.-9999999.0) THEN                             !1.2
          CLDHT=CLDHT*METRES                                      !1.2
        ENDIF                                                     !1.2

!----------------------------------------------------------------------
! Check whether group is deep convective cloud type Towering Cumulus
!(TCU) or CB
!----------------------------------------------------------------------

        IF (REPORT(Pointer+6:Pointer+8) .EQ. 'TCU') THEN
          CLDTYPE=8.
        ELSEIF (REPORT(Pointer+6:Pointer+7) .EQ. 'CB') THEN
          CLDTYPE=9.
        ENDIF

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
      ENDDO

!----------------------------------------------------------------------
!Check that we dont go beyond the end of the report - increment pointer
!if this is okay.
!----------------------------------------------------------------------

      IF ((Pointer+7) .GT. ReportLength) THEN
        MoreTrend=.FALSE.
      ELSE
        Pointer=Pointer+7
      ENDIF

      RETURN
      END
