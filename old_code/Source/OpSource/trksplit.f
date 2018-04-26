       SUBROUTINE TRKSPLIT (REPORT,NUM_REPS,REPORT_ARRAY)

       IMPLICIT NONE

!-----------------------------------------------------------------------
!
! ROUTINE       : TRKSPLIT
!
! PURPOSE       : A TRACKOB may report may contain several reports.
!                 These reports are split from each other and stored in
!                 an array for further processing.
!
! METHOD        : Each report within the main report start NNXX so loop
!                 round looking for this as the start of each report.
!
! CALLED BY     : TRKEXC in SYNOPT
!
! CALLS TO      : None
!
! PARAMETERS    : REPORT       CharacterString      Input
!                 BULEND       Integer              Input
!                 NUM_REPS     Integer              Output
!                 REPORT_ARRAY CharacterStringArray Output
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:30$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trksplit.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:30    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:27  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/06/11  09:46:04  09:46:04  usmdb (Generic MetDB account)
! Initial revision
! 
! April 1998  : First Version J. Lewthwaite
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!Declare Character
       CHARACTER     REPORT*(*)      !Report to be split up
       CHARACTER*(*) REPORT_ARRAY(*) !Array of individual reports

!Declare Integer
       INTEGER       NUM_REPS        !No. Reports in REPORT_ARRAY
       INTEGER       START_POINT     !Pointer to start of report
       INTEGER       END_POINT       !Pointer to end of report
       INTEGER       BULEND          !End of REPORT
       LOGICAL       EndPntReached

       SAVE
       START_POINT=1
       NUM_REPS=0
       EndPntReached=.FALSE.
!----------------------------------------------------------------------
!Get length of REPORT
!----------------------------------------------------------------------

       BULEND=LEN(REPORT)

!----------------------------------------------------------------------
!Point to start of first report. Look on through REPORT until NNXX is
!found. If START_POINT ever equals BULEND-3 then we will not have a
!a valid report so either then end of REPORT is dropped or no reports
!are processed.
!----------------------------------------------------------------------

      DO WHILE (START_POINT .LT. (BULEND-3) .AND.
     &(.NOT. EndPntReached))
        DO WHILE (START_POINT .LT. (BULEND-3) .AND. (
     &  (REPORT(START_POINT:START_POINT+3) .NE. 'NNXX')))
          START_POINT=START_POINT+1
        ENDDO
        END_POINT=START_POINT+1
        IF (END_POINT .GE. (BULEND-3)) THEN
          EndPntReached=.TRUE.
        ELSE
          DO WHILE (END_POINT .LT. (BULEND-3) .AND. (
     &    (REPORT(END_POINT:END_POINT+3) .NE. 'NNXX')))
            END_POINT=END_POINT+1
          ENDDO
        ENDIF

!----------------------------------------------------------------------
!Allow for the last report not having a following NNXX. Set the end
!pointer so that it will point to the end of REPORT when the report is
!passed to REPORT_ARRAY
!----------------------------------------------------------------------
        IF (.NOT. EndPntReached) THEN
          IF (END_POINT .EQ. (BULEND-3)) THEN
            END_POINT=BULEND-1
          ENDIF

          END_POINT=END_POINT-2
          NUM_REPS=NUM_REPS+1
          REPORT_ARRAY(NUM_REPS)=REPORT(START_POINT:END_POINT)
          START_POINT=END_POINT+2
        ENDIF
      ENDDO


!----------------------------------------------------------------------
!If no valid reports have been found then the NUM_REPS value will be
!zero. TRKEXC should detect this and not attempt any further
!processeing on the current report.
!----------------------------------------------------------------------

      RETURN                               !Back to TRKEXC
      END
