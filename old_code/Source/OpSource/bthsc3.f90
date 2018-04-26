SUBROUTINE BTHSC3(REPORT,REPLEN,EXPARR,TOTDEP,POS,POS1A)

!-----------------------------------------------------------------------
!
! PROGRAM       : BTHSC3
!
! PURPOSE       : TO EXPAND SECTION 3 OF BATHY
!                 (SEA DEPTH & CURRENT)
!
! CALLED BY     : BTHEXP
!
! CALLS         : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
! ARGUMENTS     : REPORT   CHARACTER STRING OF REPORT (I)
!                 REPLEN   LENGTH OF REPORT  (I)
!                 EXPARR   EXPANSION ARRAY   (I/O)
!                 TOTDEP   TOTAL WATER DEPTH (I/O)
!                 POS      SECTION LOCATING VARIABLE (I/O)
!                 POS1A    END OF REPLICATED DATA MARKER (I)
!
! REVISION INFO:
!
! $Workfile: bthsc3.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 09/05/2011 17:15:01$
!
! CHANGE RECORD:
!
! $Log:
!  9    MetDB_Refresh 1.8         09/05/2011 17:15:01    Brian Barwell   Check
!       number of characters left before looking for '66666'.
!  8    MetDB_Refresh 1.7         18/04/2011 16:43:01    Brian Barwell   EXPARR
!        changed to INOUT to preserve whole array.
!  7    MetDB_Refresh 1.6         05/04/2011 11:16:08    Brian Barwell   EXPARR
!        declaration changed.
!  6    MetDB_Refresh 1.5         28/03/2011 17:11:22    Richard Weedon
!       redrafted after testing of MDBSTOR
!  5    MetDB_Refresh 1.4         25/01/2011 16:07:05    Rosemary Lavery
!       replaced DATA stmt
!  4    MetDB_Refresh 1.3         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  3    MetDB_Refresh 1.2         19/01/2011 11:07:12    Richard Weedon
!       Updated comments on var declarations
!  2    MetDB_Refresh 1.1         18/01/2011 17:27:13    Richard Weedon
!       Initial draft. VAR,INTENT(OUT) not yet set. Passes basic compilation
!       test. Added ivalue_mod statement and removed var declaration for
!       IVALUE.
!  1    MetDB_Refresh 1.0         18/01/2011 17:24:07    Richard Weedon
!       Initial draft. VAR,INTENT(OUT) not yet set. Passes basic compilation
!       test.
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
USE metdb_com_mod, ONLY: RMISS, KTS2MPS

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*), INTENT(IN)     ::  REPORT
INTEGER,          INTENT(IN)     ::  REPLEN
REAL,             INTENT(INOUT)  ::  EXPARR(0:)
REAL,             INTENT(INOUT)  ::  TOTDEP
INTEGER,          INTENT(INOUT)  ::  POS
INTEGER,          INTENT(IN)     ::  POS1A

! Local Variable Declarations

REAL                             ::       CDIR
REAL                             ::       CSPEED
INTEGER                          ::       K5

!************************************************************
!
!     INITIALISE VARIABLES
!     K5 - INDICATOR FOR THE METHOD OF CURRENT MEASUREMENT
!     CDIR - SURFACE CURRENT DIRECTION
!     CSPEED - SURFACE CURRENT SPEED
!
!************************************************************

K5 = RMISS
CDIR = RMISS
CSPEED = RMISS

!************************************************************
!
!     CHECK IF SECTION BEGINS '66666'
!     IF NOT THIS IS NOT SECTION 3
!
!************************************************************

IF (POS+4 > REPLEN) GO TO 999
IF (REPORT(POS:POS+4) /= '66666') GO TO 999

!     NEXT GROUP - CHECK REPORT LENGTH HAS NOT BEEN EXCEEDED
POS = POS + 6
IF (POS > REPLEN) GO TO 999

!************************************************************
!
!     GROUPS WITHIN SECTION 3 ARE OPTIONAL
!     CHECK IF NEXT GROUP IS 1ZDZDZDZD  - SEA DEPTH GROUP
!     THIS GROUP AND THE (00000) GROUP OF SECTION 2
!     ARE MUTUALLY EXCLUSIVE
!     IF THIS GROUP HAS NOT BEEN REPORTED THEN THE TOTAL
!     DEPTH EXTRACTED FROM SECTION 2 WILL BE STORED
!
!************************************************************

IF (REPORT(POS:POS) == '1') THEN
  TOTDEP = IVALUE(REPORT(POS+1:POS+4))
!       NEXT GROUP
  POS = POS + 6
  IF (POS > REPLEN) GOTO 999
END IF

!************************************************************
!
!     CHECK IF CURRENT GROUP EXISTS - K5DCDCVCVC
!     K5 TAKES VALUES 2,3,4
!     CURRENT DIRECTION IS IN TENS OF DEGREES
!     CURRENT SPEED IS IN TENTHS OF KNOTS: CONVERT TO M/S.
!
!************************************************************

!     CHECK IF THIS GROUP IS NOT CALLSIGN
IF ((POS+9) >= REPLEN) GOTO 999
K5 = IVALUE(REPORT(POS:POS))
IF (K5 == 2.OR.K5 == 3.OR.K5 == 4) THEN
  CDIR = IVALUE(REPORT(POS+1:POS+2))
  IF (CDIR /= RMISS) CDIR=CDIR*10.
  CSPEED = IVALUE(REPORT(POS+3:POS+4))
  IF (CSPEED /= RMISS) CSPEED=CSPEED*0.1*KTS2MPS
!       NEXT GROUP
  POS = POS + 6
  IF (POS > REPLEN) GOTO 999
END IF

999   CONTINUE

!************************************************************
!
!     ASSIGN VARIABLES TO ARRAY
!
!************************************************************

EXPARR(POS1A+2) = TOTDEP
EXPARR(POS1A+4) = K5
EXPARR(POS1A+6) = CDIR
EXPARR(POS1A+8) = CSPEED

RETURN
END SUBROUTINE BTHSC3
