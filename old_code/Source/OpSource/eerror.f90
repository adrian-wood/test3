SUBROUTINE EERROR(REP,IRC)

!-----------------------------------------------------------------------
!
! PROGRAM       : EERROR
!
! PURPOSE       : TO DETECT TRANSMISSION ERRORS MARKED BY E'S AND
!                 REMOVE THE BAD GROUP IF A 5-FIGURE GROUP FOLLOWS
!
! DESCRIPTION   : TC CODE REMOVES ERRORS MARKED BY 'E E E'; THIS
!                 PROGRAM COPES WITH OTHER COMBINATIONS OF E & SPACE
!
! CALLED BY     : SYNBUL
!
! CALLS         : IVALUE
!
! ARGUMENTS     : (1) REPORT
!                 (2) RETURN CODE, 4 IF BAD GROUP REMOVED, 0 OTHERWISE
!
! REVISION INFO :
!
! $Workfile: eerror.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 11/01/2011 11:50:14$
!
! CHANGE RECORD :
! $Log:
!  3    MetDB_Refresh 1.2         11/01/2011 11:50:14    Alison Weir
!       Changes following review
!  2    MetDB_Refresh 1.1         21/12/2010 14:26:47    Alison Weir     Ported
!        to F95
!  1    MetDB_Refresh 1.0         21/12/2010 13:36:24    Alison Weir
!       Initial F77 version
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

! Use statements:
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:
CHARACTER(LEN=*), INTENT(INOUT) :: REP  !A01
INTEGER,          INTENT(OUT)   :: IRC  !A02

! Local declarations:

INTEGER          ::  IEE
INTEGER          ::  IE
INTEGER          ::  ISP
INTEGER          ::  IGROUP
INTEGER          ::  NFIGS
INTEGER          ::  IBACK
CHARACTER(LEN=1) ::  CH

IRC=0
!
IEE=INDEX(REP,'E')            ! LOOK FOR E
IF (IEE == 0) RETURN          ! IF NONE, RETURN
IE=IEE                        ! IF E FOUND, KEEP DISPLACEMENT
10 CONTINUE
IF (IEE >= LEN(REP)) RETURN   ! STOP IF END OF REPORT REACHED
ISP=INDEX(REP(IEE:),' ')      ! LOOK FOR NEXT SPACE AFTER E
IF (ISP == 0) RETURN          ! IF NONE, END OF REPORT - RETURN
!
IEE=IEE+ISP                   ! POINT PAST THIS SPACE
CH=REP(IEE-2:IEE-2)           ! LOOK AT CHARACTER BEFORE SPACE
IF (CH == 'E' .OR. CH == ' ') THEN   ! 'E' OR ANOTHER SPACE?
  GO TO 10                    ! IF SO, LOOK ON FOR NEXT SPACE
ELSE IF (CH >= '0' .AND. &    ! IF NOT, IS IT A FIGURE?
         CH <= '9') THEN
  IGROUP=IVALUE(REP(IEE-6:IEE-2))  ! IF SO, DO FIGURES FOLLOW?
  IF (IGROUP < 0) RETURN      ! IF NOT 5 FIGURES, GIVE UP.
ELSE
  RETURN
END IF
!
! WE'VE FOUND A 5-FIGURE GROUP AFTER THE E'S.  NOW LOOK BEFORE THE
! FIRST 'E' TO SEE WHAT TO REMOVE.
!
NFIGS=0                       ! COUNT FIGS TO SEE IF ANY FOUND
IBACK=1                       ! START BEFORE FIRST E
IF (IE <= 1) RETURN           ! GIVE UP
20 CONTINUE
CH=REP(IE-IBACK:IE-IBACK)     ! PREVIOUS CHARACTER
IF (CH == ' ' .AND. NFIGS == 0) THEN  ! SPACE BEFORE E'S?
  IBACK=IBACK+1               ! BACK ONE MORE TO FIND BAD GROUP
  GO TO 20
ELSE IF (CH >= '0' .AND. CH <= '9') THEN
  NFIGS=NFIGS+1               ! FIGURE IN BAD GROUP
  IBACK=IBACK+1               ! BACK ONE MORE TO FIND START
  GO TO 20
ELSE IF (CH <= ' '.AND.NFIGS > 0.AND.IE-IBACK < IEE-7) THEN
  REP(IE-IBACK:IEE-7)=' '     ! SET BAD GROUP & E'S TO SPACES
  IRC=4
  WRITE(6,*)'EERROR: CORRECTED REPORT = ',REP
END IF
RETURN                        ! OR CAN WE LOOK FOR MORE E'S?
END SUBROUTINE EERROR
