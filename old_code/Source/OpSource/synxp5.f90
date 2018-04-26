SUBROUTINE SYNXP5(REPORT,POINT,NGRPS,WNDKTS,T1GUST,T2GUST, &
                  VGUST,N,ICL,CLOUD,VSIF,VERTV)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP5
!
! PURPOSE       : EXPAND SECTION 5 OF SYNOP (NATIONAL SECTION)
!                 LOOKING FOR GUST & CLOUD.
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
! ARGUMENTS     : (1) REPORT
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
! $Workfile: synxp5.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 23/12/2010 10:48:08$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE ivalue_mod    !function
USE syncld_mod

! <Data Modules>

USE metdb_com_mod, only : MISSIN

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
INTEGER,          INTENT(INOUT) :: POINT      !a02
INTEGER,          INTENT(IN)    :: NGRPS      !a03
LOGICAL,          INTENT(IN)    :: WNDKTS     !a04
REAL,             INTENT(INOUT) :: T1GUST     !a05
REAL,             INTENT(INOUT) :: T2GUST     !a06
REAL,             INTENT(INOUT) :: VGUST      !a07
REAL,             INTENT(INOUT) :: N          !a08
INTEGER,          INTENT(INOUT) :: ICL        !a09
REAL,             INTENT(INOUT) :: CLOUD(3,4) !a10
REAL,             INTENT(INOUT) :: VSIF(4)    !a11
REAL,             INTENT(INOUT) :: VERTV      !a12

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

LOGICAL  :: NONGR ! SET BEFORE N VALUE FOUND, UNSET IF FOUND
LOGICAL  :: ONEGP ! SET IF A 1-GROUP IS FOUND, UNSET IF ANOTHER FOUND

INTEGER  :: IG


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!
! LOOP ROUND THE GROUPS IN SECTION 5 (APART FROM THE 555 ITSELF)
!
IG=1    ! this group number may be incremented in SYNCLD too
ONEGP=.FALSE.
NONGR=.FALSE.
!
! Gust group (accept 1//ff, 1/fff)
! If there's more than one 1-group, ignore all but the first.
!
 10 CONTINUE
IFLABEL1: &
IF (REPORT(POINT:POINT+1) == '1/') THEN
IFLABEL2: &
  IF (.NOT.ONEGP) THEN
    ONEGP=.TRUE.
    IF (REPORT(POINT+1:POINT+2) == '//') THEN
      VGUST=IVALUE(REPORT(POINT+3:POINT+4))    ! 1//ff
    ELSE
      VGUST=IVALUE(REPORT(POINT+2:POINT+4))    ! 1/fff
    END IF
!
! IF GUST SPEED FOUND, CONVERT TO M/S IF NECESSARY & SET GUST PERIOD.
!
    IF (VGUST /= MISSIN) THEN
      IF (WNDKTS) VGUST=0.5148*VGUST
      T1GUST=-60.   ! GUST IN LAST HOUR, SO 60 MINUTES BACK
      T2GUST=0.
    END IF
  END IF IFLABEL2
!
! SAMOS Q/C GROUP 7/VQN (SKIP VIS CODE & QUALITY, JUST GET CLOUD AMOUNT)
!
ELSE IF (REPORT(POINT:POINT+1) == '7/') THEN
  N=IVALUE(REPORT(POINT+4:POINT+4))
  NONGR=.FALSE.
!
! SAMOS Q/C GROUP 77VQN (SKIP VIS CODE & QUALITY, JUST GET CLOUD AMOUNT)
!
ELSE IF (REPORT(POINT:POINT+1) == '77'.AND.IG < 4.AND.NONGR) THEN
  N=IVALUE(REPORT(POINT+4:POINT+4))
  NONGR=.FALSE.
!
! CLOUD GROUPS 8....  (AS IN SECTION 3, SO CALL SAME EXPANSION ROUTINE)
!
ELSE IF (REPORT(POINT:POINT) == '8') THEN
  CALL SYNCLD(POINT,REPORT,IG,NGRPS,ICL,CLOUD,VSIF,VERTV)
END IF IFLABEL1
!
IF (IG < NGRPS) THEN
  IG=IG+1
  POINT=POINT+6
  GO TO 10
END IF
RETURN
END SUBROUTINE SYNXP5
