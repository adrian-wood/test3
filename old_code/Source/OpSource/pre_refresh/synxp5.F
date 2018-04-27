      SUBROUTINE SYNXP5(REPORT,POINT,NGRPS,WNDKTS,T1GUST,T2GUST,
     &                  VGUST,N,ICL,CLOUD,VSIF,VERTV)               !2.0

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP5
!
! PURPOSE       : EXPAND SECTION 5 OF SYNOP (NATIONAL SECTION)
!                 LOOKING FOR GUST & CLOUD.                         !1.7
!                 THIS IS BASED ON UK PRACTICE IN JUNE 96:
!                 MANY STATIONS ONLY HAVE A 9-GROUP WITH A TIME
!                 WITHIN A FEW MINUTES OF THE NOMINAL TIME, SO
!                 BETTER IGNORED.
!                 SOME STATIONS IN THE FAR NORTH HAVE SEVERAL
!                 1-GROUPS, SO ALL BUT THE FIRST ARE IGNORED.
!                 (INFO FROM BEAUFORT PARK SAYS FIRST IS VIS/GUST!)
!                 OTHERWISE THE GROUPS USED ARE AS BELOW, EXCEPT
!                 FOR THE ODD 2-GROUP (WATER TEMP?).
!
! CALLED BY     : SYNEXP
!
! CALLS         : SYNCLD
!
! PARAMETERS    : (1) REPORT
!                 (2) START OF FIRST GROUP IN REPORT AFTER 555
!                 (3) NUMBER OF GROUPS (after 555) IN SECTION 5
!                 (4) WIND SPEED INDICATOR (KNOTS OR M/S)
!                 (5) START OF GUST PERIOD (60 MINUTES BACK
!                 (6) END OF GUST PERIOD (0 MINS BACK: REPORT TIME)
!                 (7) GUST SPEED IN LAST HOUR
!                 (8) TOTAL CLOUD AMOUNT FROM 7-GROUP
!                 (9) NUMBER OF 8-GROUPS WITH CLOUD (NOT VERTICAL VIS)
!                (10) CLOUD DATA FROM 8-GROUPS (3*4 ARRAY)
!                (11) LAYER INDICATORS FOR 8-GROUPS
!                (12) VERTICAL VISIBILITY FROM 8-GROUP
!
!
! REVISION INFO :
!
! $Revision: 4$
! $Date: 20/07/2009 11:01:42$
! $Author: John Norton$
! $Folder: pre_refresh$
! $Workfile: synxp5.F$
!
! CHANGE RECORD :
!
! £Log: synxp5.F,v £
! Revision 2.0  2001/07/03 10:44:12  usmdb
! Changed preprocessor statement to if (1 ), else.....
! Removed unused dummy argument VV. Added copyright and modified
! header - S.Cox
!
! Revision 1.7  2000/03/10  09:55:03  09:55:03  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! Remove misleading code & comments referring to visibility:
! SYNXP5 can only find gust speed.
!
! Revision 1.6  98/06/11  11:12:07  11:12:07  usmdb (Generic MDB account
! start with ig=1, not ig=2 (NGRPS doesn't include 555)               !B
!
! Revision 1.5  98/01/29  11:42:59  11:42:59  usmdb (Generic MDB account
! Addition of IBM preprocess directive.
!
! Revision 1.4  97/07/31  11:40:43  11:40:43  uspm (Pat McCormack)
! no entry
!
! FEB 97 - REMOVE TEST LINES
!
! NOV 96 - USE 1/ AS IDENTIFICATION FOR GUST GROUP.                   !A
!
! JUN 96 - DON''T LOOK FOR TIME HERE (UNHELPFUL UK PRACTICE!)
!          ALLOW FOR 1//FF AS WELL AS 1/FFF
!          IGNORE 1-GROUPS IF THERE ARE MORE THAN ONE OF THEM
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

      IMPLICIT NONE

      CHARACTER REPORT*(*)
      INTEGER POINT,NGRPS
      LOGICAL WNDKTS
      REAL T1GUST,T2GUST,VGUST                                      !2.0
      INTEGER IG,IVALUE,ICL
      REAL N,CLOUD(3,4), VSIF(4), VERTV
      LOGICAL ONEGP ! SET IF A 1-GROUP IS FOUND, UNSET IF ANOTHER FOUND
      LOGICAL NONGR ! SET BEFORE N VALUE FOUND, UNSET IF FOUND      !ST2

#if defined (MVS)
      INCLUDE (SYNCOM)  ! CONSTANTS
#else
      INCLUDE 'SYNCOM'  ! CONSTANTS
#endif

      CHARACTER*132 CHEAD

      CHEAD='
     &£Source: /home/us0400/mdb/op/lib/storage/RCS/synxp5.F,v £
     &'//'£ £Date: 2001/07/03 10:44:12 £ £Revision: 2.0 £'
!
! LOOP ROUND THE GROUPS IN SECTION 5 (APART FROM THE 555 ITSELF)
!
      IG=1    ! this group number may be incremented in SYNCLD too !1.7
      ONEGP=.FALSE.
      NONGR=.FALSE.                                                !ST2
!
! Gust group (accept 1//ff, 1/fff)                                 !1.7
! If there's more than one 1-group, ignore all but the first.      !1.7
!
   10 IF (REPORT(POINT:POINT+1).EQ.'1/') THEN                      !A
        IF (.NOT.ONEGP) THEN
          ONEGP=.TRUE.
          IF (REPORT(POINT+1:POINT+2).EQ.'//') THEN
            VGUST=IVALUE(REPORT(POINT+3:POINT+4))    ! 1//ff       !1.7
          ELSE                                                     !1.7
            VGUST=IVALUE(REPORT(POINT+2:POINT+4))    ! 1/fff       !1.7
          ENDIF
!
! IF GUST SPEED FOUND, CONVERT TO M/S IF NECESSARY & SET GUST PERIOD.
!
          IF (VGUST.NE.MISSIN) THEN
            IF (WNDKTS) VGUST=0.5148*VGUST
            T1GUST=-60.   ! GUST IN LAST HOUR, SO 60 MINUTES BACK
            T2GUST=0.
          ENDIF
        ENDIF
!
! SAMOS Q/C GROUP 7/VQN (SKIP VIS CODE & QUALITY, JUST GET CLOUD AMOUNT)
!
      ELSE IF (REPORT(POINT:POINT+1).EQ.'7/') THEN
        N=IVALUE(REPORT(POINT+4:POINT+4))
        NONGR=.FALSE.                                               !ST2
!                                                                   !ST2
! SAMOS Q/C GROUP 77VQN (SKIP VIS CODE & QUALITY, JUST GET CLOUD AMOUNT)
!                                                                   !ST2
      ELSE IF (REPORT(POINT:POINT+1).EQ.'77'.AND.IG.LT.4.AND.NONGR) THEN
        N=IVALUE(REPORT(POINT+4:POINT+4))                           !ST2
        NONGR=.FALSE.                                               !ST2
!
! CLOUD GROUPS 8....  (AS IN SECTION 3, SO CALL SAME EXPANSION ROUTINE)
!
      ELSE IF (REPORT(POINT:POINT).EQ.'8') THEN
        CALL SYNCLD(POINT,REPORT,IG,NGRPS,ICL,CLOUD,VSIF,VERTV)
      ENDIF
!
      IF (IG.LT.NGRPS) THEN
        IG=IG+1
        POINT=POINT+6
        GO TO 10
      ENDIF
      RETURN
      END
