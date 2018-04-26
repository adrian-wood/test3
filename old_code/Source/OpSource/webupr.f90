SUBROUTINE WEBUPR(CREP,RPRTEND,WRITEHEADER,IN_TEXT, &
                  TOR_UPR)

!-----------------------------------------------------------------------
!
! routine       : WEBUPR
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format TEMP, PILOT raw reports for web retrieval.
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
! IN_TEXT       : char*(*)   (i/o): dummy - string initialised here.
! TOR_UPR(4)    : char*(*)   (ip) : Time Of Receipt for parts in
!                                 : the order A,C,B,D
!
! data types    : TEMP, PILOT, DROPSOND
!
! calls         : WEBFORM  : to output the final report text
!
! REVISION INFO:
!
! $Workfile: webupr.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/04/2011 10:13:48$
!
! change record :
!
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
CHARACTER(LEN=*), INTENT(INOUT) :: IN_TEXT     !a04- text to preceed report on page
CHARACTER(LEN=*), INTENT(IN)    :: TOR_UPR(4)  !a05- Time Of Receipt per part

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER           ::  COL_END      !- end column of report on page
INTEGER           ::  COL_START    !- start column of report on page
INTEGER           ::  IEND         !- end of part report text
INTEGER           ::  IPOS         !- position pointer on report text
INTEGER           ::  ISTART       !- start of part report text
INTEGER           ::  IZ1          !- pos of head time in current part
INTEGER           ::  IZ2          !- pos of head time in next part
INTEGER           ::  J            !- loop counter over 4 parts
INTEGER           ::  PART_COUNT   !- part counter

LOGICAL           ::  FIRST=.TRUE. !- TRUE for 1st time in routine

CHARACTER(LEN=44) ::  HEADER       !- report text header for each part

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Assign start & end of page for formatted text.
!-----------------------------------------------------------------------

COL_START  = 18
COL_END    = 77
IPOS       = 1
PART_COUNT = 0

IF (FIRST) THEN
  FIRST = .FALSE.
  WRITE(6,'('' ddhhmmZ ident   report''/)')
END IF

!-----------------------------------------------------------------------
! Loop over 4 parts, Finding header text and start & end of message
! for each.
!-----------------------------------------------------------------------

DOLABEL1: &
DO J=1,4
IFLABEL1: &
  IF (TOR_UPR(J) /= 'TOR=**/****') THEN

    IZ1 = INDEX(CREP(IPOS:RPRTEND),'Z ')
IFLABEL2: &
    IF (IZ1 > 0) THEN
      IF (PART_COUNT /= 0) WRITE(6,*) !- write between parts
      PART_COUNT = PART_COUNT + 1
      IPOS = IPOS+IZ1
      HEADER = CREP(IPOS-5:IPOS-5+43)
      ISTART = IPOS-5+44
      IZ2 = INDEX(CREP(IPOS:),'Z ')
      IF (IZ2 > 0) THEN
        IEND = IPOS-5+IZ2
      ELSE
        IEND = RPRTEND+1
      END IF

      ISTART = ISTART - 1   !- to move to blank before the TTxx
      IEND   = IEND   - 1   !- compensate for above

!-----------------------------------------------------------------------
! Call routine to format text on page.
!-----------------------------------------------------------------------

      CALL WEBFORM(CREP,ISTART,IEND,COL_START,COL_END,IN_TEXT)
      IN_TEXT(:) = ' '

!-----------------------------------------------------------------------
! Output MetDB header if WRITEHEADER = .TRUE.
!-----------------------------------------------------------------------

      IF (WRITEHEADER) THEN
        WRITE(6,'(1X,2A)')IN_TEXT(1:COL_START-1),      &
        HEADER(1:30)//' CC='//HEADER(31:34)//' '//     &
        TOR_UPR(J)//' AMD='//HEADER(41:41)//' COR='//  &
        HEADER(43:43)
      END IF
    END IF IFLABEL2
  END IF IFLABEL1
END DO DOLABEL1

WRITE(6,*)

RETURN
END SUBROUTINE WEBUPR
