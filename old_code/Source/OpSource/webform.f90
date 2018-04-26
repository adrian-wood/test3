SUBROUTINE WEBFORM(CREP,START,RPRTEND,COL_START,COL_END, &
                   IN_TEXT)

!-----------------------------------------------------------------------
!
! routine       : WEBFORM
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To output report text to stdout
!
! description   : This routine takes a report text as input and a
!               : column start and end for the stdout. WEBFORM will
!               : write the report text to stdout between the column
!               : start and end.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : text string to output
! START         : integer    (ip) : pointer to start of text in CREP
! RPRTEND       : integer    (ip) : pointer to end of text in CREP
! COL_START     : integer    (ip) : stdout page column start
! COL_END       : integer    (ip) : stdout page column end
! IN_TEXT       : char*(*)   (ip) : text to output before col_start
!
! data types    : all
!
! calls         : none
!
! format of output text
! =====================
!
! IF (col_start > 1)
!
!               col_start                                     col_end
!                  !                                             !
!                  V                                             V
!   xxx(IN_TEXT)xxxooooooooooooooooooooooooooooooooooooooooooooooo
!                  ooooooooooooooooooo(CREP)oooooooooooooooooooooo
!                  ooooooooooooooooooooooooooooooooooooooooooooooo
!
! ELSE IF (col_start == 1)
!
!   col_start                                              col_end
!   !                                                            !
!   V                                                            V
!   oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!   oooooooooooooooooooooooooooo(CREP)oooooooooooooooooooooooooooo
!   oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
!
! END IF
!
! REVISION INFO:
!
! $Workfile: webform.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 01/03/2011 09:41:32$
!
! change record :
!
! 29-07-1998    : Made operational - S.Cox
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: CREP      !a01- report to output to stdout
INTEGER,          INTENT(IN)    :: START     !a02- start of report in string CREP
INTEGER,          INTENT(IN)    :: RPRTEND   !a03- end of report in string CREP
INTEGER,          INTENT(IN)    :: COL_START !a04- stdout page start column
INTEGER,          INTENT(IN)    :: COL_END   !a05- stdout page end column
CHARACTER(LEN=*), INTENT(IN)    :: IN_TEXT   !a06- input text to preceed report text

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER           ::  COL_SPAN !- stdout page column span
INTEGER           ::  CREP_END
INTEGER           ::  LINE     !- counter of output lines.
INTEGER           ::  IPTR     !- pointer to position in CREP

CHARACTER(LEN=20) :: LINE_TEXT !- text to preceed report text

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! initilise variables
!-----------------------------------------------------------------------

IPTR=START
LINE=1
LINE_TEXT(:)=' '
COL_SPAN=COL_END-COL_START

!-----------------------------------------------------------------------
! loop while pointer to position in report text is not greater than the
! end of the report text.
!-----------------------------------------------------------------------

DOLABEL1: &
DO WHILE (IPTR <= RPRTEND)

!-----------------------------------------------------------------------
! if we are to output the 1st line, put IN_TEXT into the string
! LINE_TEXT. This is to preceed the main report text for the 1st output
! line only, otherwise LINE_TEXT = ' '
!-----------------------------------------------------------------------

  IF (LINE == 1) THEN
    LINE_TEXT(1:LEN(IN_TEXT)) = IN_TEXT
  ELSE
    LINE_TEXT(:) = ' '
  END IF

!-----------------------------------------------------------------------
! calculate report text end for this line - it could be the end of the
! report text.
!-----------------------------------------------------------------------

  IF ((IPTR+COL_SPAN) <= RPRTEND) THEN
    CREP_END = IPTR + COL_SPAN
  ELSE
    CREP_END = RPRTEND
  END IF

!-----------------------------------------------------------------------
! if COL_START>1, output LINE_TEXT followed by CREP, otherwise output
! just CREP. Increment IPTR and LINE.
!-----------------------------------------------------------------------

  IF (COL_START > 1) THEN
    WRITE(6,'(1X,A)')LINE_TEXT(1:COL_START-1)// &
     &                     CREP(IPTR:CREP_END)
  ELSE
    WRITE(6,'(1X,A)')CREP(IPTR:CREP_END)
  END IF

  IPTR=IPTR+COL_SPAN+1
  LINE=LINE+1
END DO DOLABEL1

RETURN
END SUBROUTINE WEBFORM
