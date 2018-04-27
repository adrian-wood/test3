SUBROUTINE WEBSAMOS(CREP,RPRTEND,WRITEHEADER)

!-----------------------------------------------------------------------
!
! routine       : WEBSAMOS
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format SAMOSX raw reports for web retrieval.
!
! description   : The SAMOSX raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
!
! data types    : SAMOSX
!
! calls         : WEBFORM  : to output the final report text
!
! REVISION INFO:
!
! $Workfile: websamos.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 01/03/2011 09:41:32$
!
! change record :
!
! 28-07-1998    : Made operational - S.Cox
!
!-----------------------------------------------------------------------
! $Log:
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

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER           ::  COL_END      !- end column of report on page
INTEGER           ::  COL_START    !- start column of report on page
INTEGER           ::  IPTR         !- report text pointer

LOGICAL           ::  FIRST=.TRUE. !- TRUE for 1st time in routine

CHARACTER(LEN=20) :: IN_TEXT       !- text to preceed report on page

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IN_TEXT(:) = ' '

!-----------------------------------------------------------------------
! RCS revision information.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Put the date/time and station into string IN_TEXT
!-----------------------------------------------------------------------

IN_TEXT(1:2)   = CREP(7:8)     !- day
IN_TEXT(3:6)   = CREP(1:4)     !- hour,minute
IN_TEXT(7:8)   = 'Z '
IN_TEXT(9:13)  = CREP(45:49)   !- station

!-----------------------------------------------------------------------
! Move pointer past the header text and then past the station number.
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

IPTR       = 51
COL_START  = 15
COL_END    = 80

IF (FIRST) THEN
  FIRST = .FALSE.
  WRITE(6,'('' ddhhmmZ ident report'')')
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
  WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),CREP(1:44)
END IF

RETURN
END SUBROUTINE WEBSAMOS
