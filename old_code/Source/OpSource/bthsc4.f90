SUBROUTINE BTHSC4(REPORT,REPLEN,EXPARR,IDFLG,ID)
!
!-----------------------------------------------------------------------
!
! PROGRAM       : BTHSC4
!
! PURPOSE       : TO EXPAND SECTION 4 OF BATHY (IDENTIFIER)
!
! CALLED BY     : BTHEXP
!
! CALLS         : IVALUE CONVERTS FROM CHAR TO NUMERIC TYPE
!               : NCHTST
!
! ARGUMENTS     : REPORT   CHARACTER STRING OF REPORT (I)
!                 REPLEN   LENGTH OF REPORT           (I)
!                 EXPARR   EXPANSION ARRAY            (O)
!                 IDFLG    FLAG FOR SHIP/BUOY ID      (O)
!                 ID       SHIPS CALL SIGN            (O)
!
! REVISION INFO:
!
! $Workfile: bthsc4.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 18/04/2011 16:13:52$
!
! CHANGE RECORD:
!
! $Log:
!  9    MetDB_Refresh 1.8         18/04/2011 16:13:52    Brian Barwell   EXPARR
!        changed to INOUT to preserve whole array.
!  8    MetDB_Refresh 1.7         05/04/2011 11:16:23    Brian Barwell   EXPARR
!        declaration changed.
!  7    MetDB_Refresh 1.6         28/03/2011 17:11:22    Richard Weedon
!       redrafted after testing of MDBSTOR
!  6    MetDB_Refresh 1.5         24/03/2011 13:47:53    Richard Weedon  array
!       declaration REPORT(LEN=4096) changed to REPORT(LEN=*)
!  5    MetDB_Refresh 1.4         25/01/2011 16:07:05    Rosemary Lavery
!       replaced DATA stmt
!  4    MetDB_Refresh 1.3         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  3    MetDB_Refresh 1.2         19/01/2011 11:37:19    Richard Weedon  added
!       USE statement for call to NCHTST
!  2    MetDB_Refresh 1.1         19/01/2011 11:10:28    Richard Weedon
!       Updated var declaration comments
!  1    MetDB_Refresh 1.0         18/01/2011 18:00:13    Richard Weedon
!       Initial draft. VAR,INTENT(OUT) not yet set. Passes basic compilation
!       test. Added ivalue_mod statement and removed var declaration for
!       IVALUE.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
USE ivalue_mod
USE NCHTST_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*), INTENT(IN)     ::   REPORT
INTEGER,          INTENT(INOUT)  ::   REPLEN
REAL,             INTENT(INOUT)  ::   EXPARR(0:)
LOGICAL,          INTENT(OUT)    ::   IDFLG
CHARACTER(LEN=9), INTENT(OUT)    ::   ID

!Local Variable Declarations

REAL                              ::   MISING = -9999999.
REAL                              ::   DISP
INTEGER                           ::   LENGID !Length of id
INTEGER                           ::   BUOYID
INTEGER                           ::   INUMB ! num of first non fig
LOGICAL                           ::   ALLFIG ! True if string all fig
LOGICAL                           ::   ESPACE ! True if a char of id
                                              ! found
LOGICAL                           ::   OLFCR  ! True if next char is LF
                                              ! or carriage return
LOGICAL                           ::  OSPACE  !True if next char is sp

!************************************************************
!
!     INITIALISE VARIABLES
!     IDFLG - FLAG TO INDICATE SHIPS CALL SIGN IS REPORTED
!     ID - SHIPS CALL SIGN
!     DISP - DISPLACEMENT OF THE SHIPS CALL SIGN IN THE
!            STRING THAT WILL BE SENT TO ENCODING ROUTINE.
!            BECAUSE ID IS THE ONLY STRING IN THE REPORT,
!            IF THE CALL SIGN IS REPORTED DISP WILL ALWAYS
!            TAKE A VALUE OF 1.
!     BUOYID - BUOY IDENTIFIER (5 DIGIT GROUP)
!     ID AND BUOYID ARE MUTUALLY EXCLUSIVE
!
!************************************************************

IDFLG = .FALSE.
ID = ' '
DISP = 1
BUOYID = MISING

! STARTING AT THE END OF THE REPORT,
!  ignore any equals signs and spaces at the end of the report
!  then loop back through the report until a space is found.
! The ID of the report is taken as the characters after the space
! unless
!    The length of the callsign found is 5 characters and
! preceeded by ' 99999' and contains five figures then it is
! taken as a buoy id.
!    If it is 5 numbers it is taken as part of the report
! and the ID is set to spaces and buoy id taken as missing data.
!    The length is more than 9 characters then it is assumed to be
! corrupted and the ID is set to spaces and buoy id taken as
! missing data.

ESPACE=.TRUE.
LENGID=-1
OSPACE=.FALSE.

do_constr1 : &
DO WHILE (.NOT.OSPACE.AND.LENGID < REPLEN)
  LENGID=LENGID+1
! test for equals signs and spaces at end of report
  if_constr1 : &
  IF(REPORT(REPLEN-LENGID:REPLEN-LENGID) == '=' .OR.&
            (ESPACE.AND.&
            (REPORT(REPLEN-LENGID:REPLEN-LENGID) == ' ')))THEN
    LENGID=LENGID-1
    REPLEN=REPLEN-1
! Test for first space not at end of report this should delimit
! start of callsign.
  ELSE IF(REPORT(REPLEN-LENGID:REPLEN-LENGID) == ' ')THEN
    ID(1:LENGID)=REPORT((REPLEN-LENGID+1):REPLEN)
    IDFLG=.TRUE.
    IF(LENGID > 9)THEN
! corrupt callsign.
      ID='         '
    ELSE IF(LENGID == 5)THEN
      CALL NCHTST(1,LENGID,ALLFIG,INUMB,OSPACE,OLFCR,&
                          REPORT(REPLEN-LENGID+1:REPLEN))
      IF(ALLFIG)THEN
! part of report
        ID=' '
        IF(REPORT(REPLEN-LENGID-6:REPLEN-LENGID-1) == ' 99999')&
                THEN
          BUOYID=IVALUE(REPORT((REPLEN-LENGID+1):REPLEN))
          IDFLG=.FALSE.
        END IF
      END IF
    END IF
     OSPACE=.TRUE.
! Reset espace when callsign end found at end of report
  ELSE
    ESPACE=.FALSE.
  END IF if_constr1
END DO do_constr1

!************************************************************
!
!     ASSIGN VARIABLES TO ARRAY
!
!************************************************************

EXPARR(2) = DISP
EXPARR(4) = BUOYID

RETURN
END SUBROUTINE BTHSC4
