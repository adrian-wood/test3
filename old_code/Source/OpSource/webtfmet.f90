SUBROUTINE WEBTFMET(CREP,RPRTEND,WRITEHEADER,TOR,IN_TEXT)

!-----------------------------------------------------------------------
!
! routine       : WEBTFMET
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format TAFS/METARS raw reports for web retrieval.
!
! description   : The raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! TOR           : char*(*)   (ip) : report Time Of Receipt
! IN_TEXT       : char*(*)   (i/o): dummy - string initialised here.
!
! data types    : TAFS, METARS
!
! calls         : WEBFORM  : to output the final report text
!
! REVISION INFO:
!
! $Workfile: webtfmet.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/04/2011 10:13:48$
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
!  3    MetDB_Refresh 1.2         25/04/2011 10:13:48    John Norton
!       Updated after retrieval testing.
!  2    MetDB_Refresh 1.1         01/03/2011 09:41:32    John Norton     After
!       porting.
!  1    MetDB_Refresh 1.0         23/02/2011 14:41:28    John Norton     Prior
!       to porting to f95
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE webform_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
CHARACTER(LEN=*), INTENT(IN)    :: TOR         !a04- input time of receipt
CHARACTER(LEN=*), INTENT(INOUT) :: IN_TEXT     !a05- text to preceed report on page

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  COL_END      !- end column of report on page
INTEGER          ::  COL_START    !- start column of report on page
INTEGER          ::  IPTR         !- report text pointer

LOGICAL          ::  FIRST=.TRUE. !- TRUE if first call to routine

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Move pointer past the header text.
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

IPTR       = 45
COL_START  = 15
COL_END    = 80

IF (FIRST) THEN
  FIRST = .FALSE.
  WRITE(6,'(/'' ddhhmmZ ident report'')')
END IF

WRITE(6,*)

!-----------------------------------------------------------------------
! Call routine to format text on page.
!-----------------------------------------------------------------------

CALL WEBFORM(CREP,IPTR,RPRTEND,COL_START,COL_END,IN_TEXT)

!-----------------------------------------------------------------------
! Finally output MetDB header if WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

IF (WRITEHEADER) THEN
  IN_TEXT(:) = ' '
  WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),CREP(1:30)// &
        ' CC='//CREP(31:34)//' '//TOR//' AMD='//CREP(41:41)// &
        ' COR='//CREP(43:43)
END IF

RETURN
END SUBROUTINE WEBTFMET
