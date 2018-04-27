SUBROUTINE WEBTADV(CREP,RPRTEND,WRITEHEADER,TOR,CREP2)

!-----------------------------------------------------------------------
!
! routine       : WEBTADV
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! purpose       : To format TROPADV raw reports for web retrieval.
!
! description   : The TROPADV raw report (including header) is passed
!               : to this routine. This routines formats it for web
!               : display.
!
! arguments     :
!
! CREP          : char*(*)   (ip) : input report text from MDB
! RPRTEND       : integer    (ip) : end of CREP.
! WRITEHEADER   : logical    (ip) : TRUE if report text header wanted
! TOR           : char*(*)   (ip) : report Time Of Receipt
! CREP2         : char*(*)   (i/o): temporary string
!
! data types    : TROPADV
!
! calls         : None
!
! REVISION INFO:
!
! $Workfile: webtadv.f90$ $Folder: OpSource$
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: CREP        !a01- input report text
INTEGER,          INTENT(IN)    :: RPRTEND     !a02- length of input report text
LOGICAL,          INTENT(IN)    :: WRITEHEADER !a03- TRUE to output header
CHARACTER(LEN=*), INTENT(IN)    :: TOR         !a04- input time of receipt
CHARACTER(LEN=*), INTENT(INOUT) :: CREP2       !a05- string to hold line of text

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  COL_END   !- end column of report on page
INTEGER          ::  COL_START !- start column of report on page
INTEGER          ::  I         !- loop counter
INTEGER          ::  PTR       !- report text pointer

CHARACTER(LEN=1) ::  CR        !- carriage return
CHARACTER(LEN=1) ::  LF        !- linefeed

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Initialise variables.
!-----------------------------------------------------------------------

  LF = CHAR(37)
  CR = CHAR(21)

!-----------------------------------------------------------------------
! Output report text header if wanted and report text.
!-----------------------------------------------------------------------

WRITE(6,*)
IF (WRITEHEADER) WRITE(6,'(1X,A)')CREP(1:30)//' CC='// &
     &    CREP(31:34)//' '//TOR//' AMD='//CREP(41:41)//' COR='// &
     &    CREP(43:43)
WRITE(6,*)

!-----------------------------------------------------------------------
! Format report text. Ignore CR's, but line feed when LF found.
!-----------------------------------------------------------------------

PTR = 0
DOLABEL1: &
DO I=45,RPRTEND
  IF (CREP(I:I) == CR) THEN
    CONTINUE
  ELSE IF (CREP(I:I) == LF) THEN
    IF (PTR > 0) WRITE(6,'(1X,A)')CREP2(1:PTR)
    CREP2(:) = ' '
    PTR = 0
  ELSE
    PTR = PTR + 1
    CREP2(PTR:PTR) = CREP(I:I)
  END IF
END DO DOLABEL1

RETURN
END SUBROUTINE WEBTADV
