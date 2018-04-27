SUBROUTINE NCHTST(POINT,LENGTH,ALLFIG,INUMB,OSPACE,OLFCR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : NCHTST
!
! PURPOSE       : TO SEE IF A CHARACTER STRING IS ALL FIGURES.
!                 IF NOT, RETURNS NUMBER OF CHARACTER LAST CHECKED.
!
! CALLED BY     : BULHED, AMDCOR, TAFBUL, METBUL, SAMBUL, TAFBUL,
!                 CLMBUL, SYNBUL, SYNHED
!
! CALLS         : NOTHING
!
! ARGUMENTS     : (1)  POINT   FIRST CHARACTER TO BE TESTED
!                 (2)  LENGTH  LENGTH OF CHARACTER STRINGTO BE TESTED
!                 (3)  ALLFIG  LOGICAL SET IF ALL FIGURES
!                 (4)  INUMB   FIRST NON-FIGURE FOUND
!                 (5)  OSPACE  LOGICAL SET IF SPACE AFTER FIGURES
!                 (6)  OLFCR   LOGICAL SET IF LF/CR AFTER FIGURES
!                 (7)  BULL    REPORT DATA
!
! REVISION INFO :
!
! $Workfile: nchtst.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 20/12/2010 12:32:42$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 12:32:42    Stan Kellett
!       corrected declaration of CR and LF for MVS
!  2    MetDB_Refresh 1.1         16/12/2010 17:09:14    John Norton
!       Updated after rework identified by review of MDBSTOR batch 3 done.
!  1    MetDB_Refresh 1.0         07/12/2010 15:31:04    John Norton
!       MDBSTOR batch 3 code ready for review
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    :: POINT !A1
INTEGER,          INTENT(IN)    :: LENGTH !A2
LOGICAL,          INTENT(OUT)   :: ALLFIG !A3
INTEGER,          INTENT(OUT)   :: INUMB !A4
LOGICAL,          INTENT(OUT)   :: OSPACE !A5
LOGICAL,          INTENT(OUT)   :: OLFCR !A6
CHARACTER(LEN=*), INTENT(IN)    :: BULL !A7

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  INUM

CHARACTER(LEN=1) ::  CR
CHARACTER(LEN=1) ::  LF
CHARACTER(LEN=1) ::  SPACE

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

#if defined (MVS)
DATA SPACE/' '/
CR = CHAR(21)
LF = CHAR(37)
#else
DATA SPACE/' '/
CR=CHAR(13)
LF=CHAR(10)
#endif

ALLFIG=.TRUE.
OSPACE=.FALSE.
OLFCR =.FALSE.

DO INUM=POINT,POINT+LENGTH-1
IF (BULL(INUM:INUM) < '0' .OR. BULL(INUM:INUM) > '9') THEN
  ALLFIG=.FALSE.
  IF (BULL(INUM:INUM) == SPACE) OSPACE=.TRUE.
  IF (BULL(INUM:INUM) == LF) OLFCR=.TRUE.
  IF (BULL(INUM:INUM) == CR) OLFCR=.TRUE.
  INUMB=INUM-POINT+1
  RETURN
END IF
END DO

INUMB=LENGTH
RETURN
END SUBROUTINE NCHTST
