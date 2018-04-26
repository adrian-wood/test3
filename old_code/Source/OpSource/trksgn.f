      SUBROUTINE TRKSGN(REPORT,SingleReportLength,CALL_SIGN,
     &                  ReturnCode)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! ROUTINE      : TRKSGN
!
! PURPOSE      : DECODE THE CALLSIGN GROUP IN TRACKOB REPORTS
!
! DESCRIPTION  : THE CALLSIGN APPEARS AT THE END OF THE REPORT. FIND THE
!                END OF THE REPORT AND WORK BACK UNTIL SPACE FIGURE IS
!                FOUND. TAKE THE LAST GROUP AS BEING THE CALLSIGN AND
!                PASS THIS BACK TO THE MAIN PROGRAM.
!
! PARAMETERS   : 1. REPORT - TRKOB REPORT BEING DECODED
!                2. CALL_SIGN - DECODE CALL_SIGN FROM THE REPORT.
!                3. RETURN CODE - 0 Callsign Obtained from report
!                                 1 Callsign defaults to 'TRACKOB'
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:29$
! $Source: /home/us0400/mdb/op/lib/source/RCS/trksgn.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:29    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:27  usmdb
! Removed unused variables. Removed completely unwanted diagnostics
! (previously commented out in !A). Added copyright and modified
! header - S.Cox
!
! Revision 1.2  98/07/23  08:05:33  08:05:33  usmdb (Generic MetDB account)
! Remove unwanted write statements
! 
! Revision 1.1  98/06/11  09:46:04  09:46:04  usmdb (Generic MDB account)
! Initial revision
!
! 27/07/98 Remove unwanted diagnostic write statements. J.Lewthwaite  !A
!
! Implemented on 18th May 1998. Author: Jon Lewthwaite
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
      CHARACTER*(*) REPORT
      CHARACTER*9   CALL_SIGN
      CHARACTER*132 HEAD

!Declare Integer
      INTEGER       PtrEndReport
      INTEGER       PtrCurPos
      INTEGER       EndTstCallsign
      INTEGER       ReturnCode
      INTEGER       SingleReportLength
!Declare Logical
      LOGICAL       GotCallSign

      SAVE

!Initialize variables

      GotCallSign=.FALSE.
      ReturnCode=1

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/trksgn.F,v $'
     &//'$Revsion: $ $Date: 30/01/2006 20:25:29$'

      PtrEndReport=SingleReportLength-1

!----------------------------------------------------------------------
!Set lower and upper bounds of the search in REPORT for the callsign
!----------------------------------------------------------------------

      EndTstCallsign = PtrEndReport-15
      PtrCurPos = PtrEndReport

      DO WHILE ((PtrCurPos .GE. EndTstCallsign) .AND.
     &(.NOT. GotCallSign))
        PtrCurPos=PtrCurPos-1
        IF (REPORT(PtrCurPos:PtrCurPos) .EQ. ' ') THEN
          CALL_SIGN=REPORT(PtrCurPos+1:PtrEndReport)

          GotCallSign=.TRUE.
          ReturnCode=0
        ENDIF
      ENDDO

      IF (ReturnCode .EQ. 1) THEN
        CALL_SIGN='TRACKOB'
      ENDIF

      RETURN
      END
