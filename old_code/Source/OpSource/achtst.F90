SUBROUTINE ACHTST(POINT,LENGTH,ALLETT,ICHRAC,OSPACE,OLFCR,BULL)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : ACHTST
!
! PURPOSE       : TO SEE IF A CHARACTER STRING IS ALL LETTERS.
!                 IF NOT, RETURNS NUMBER OF CHARACTER LAST CHECKED.
!
! CALLED BY     : BULHED, AMDCOR, TAFBUL, METBUL, SYNHED
!
! CALLS         : NOTHING
!
! ARGUMENTS     : (1)  POINT   FIRST CHARACTER TO BE TESTED
!                 (2)  LENGTH  NUMBER OF CHARACTERS TO BE TESTED
!                 (3)  ALLETT  LOGICAL SET IF ALL LETTERS
!                 (4)  ICHRAC  FIRST NON LETTER FOUND
!                 (5)  OSPACE  LOGICAL SET IF SPACE AFTER LETTERS
!                 (6)  OLFCR   LOGICAL SET IF LF/CR AFTER LETTERS
!                 (7)  BULL    REPORT DATA
!
! REVISION INFO :
!
! $Workfile: achtst.F90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/12/2010 12:14:19$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 12:14:19    Stan Kellett
!       Corrected compile of CR and LF on MVS compiler as old version fails
!       compile in build
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
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(IN)    :: POINT !a1
INTEGER,          INTENT(IN)    :: LENGTH !a2
LOGICAL,          INTENT(OUT)   :: ALLETT !a3
INTEGER,          INTENT(OUT)   :: ICHRAC !a4
LOGICAL,          INTENT(OUT)   :: OSPACE !a5
LOGICAL,          INTENT(OUT)   :: OLFCR !a6
CHARACTER(LEN=*), INTENT(IN)    :: BULL !a7

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

CHARACTER(LEN=1) ::  CH
CHARACTER(LEN=1) ::  CR
INTEGER          ::  ICHRA  ! Loop variable
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
SPACE=' '
CR = CHAR(13)
LF = CHAR(10)
#endif

ALLETT=.TRUE.
OSPACE=.FALSE.
OLFCR =.FALSE.

DO ICHRA=POINT,POINT+LENGTH-1
  CH=BULL(ICHRA:ICHRA)
  IF (CH < 'A' .OR. (CH > 'I' .AND. CH < 'J') .OR. &
     (CH > 'R' .AND. CH < 'S') .OR. CH > 'Z') THEN
    ALLETT=.FALSE.
    IF (CH == SPACE) OSPACE=.TRUE.
    IF (CH == LF) OLFCR=.TRUE.
    IF (CH == CR) OLFCR=.TRUE.
    ICHRAC=ICHRA-POINT+1
    RETURN
  END IF
END DO

ICHRAC=LENGTH

RETURN
END SUBROUTINE ACHTST
