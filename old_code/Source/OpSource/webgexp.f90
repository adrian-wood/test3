SUBROUTINE WEBGEXP(CREP,CREP2,RPRTEND,LOOKUP)

!-----------------------------------------------------------------------
!
! routine       : WEBGEXP
!
!               : ANSI standard except for '!' used for comments,
!               : variable name lengths greater than 6 characters.
!
! descrition    : To produxe a CREP2 report text from a CREP report
!               : text by expanding 3 character groups e.g. '333' to
!               : 5 character groups e.g. ' 333 '
!
! arguments     :
!
! CREP          : char*(*)   (i/o): input text string
! CREP2         : char*(*)   (op) : output text string (expanded)
! RPRTEND       : integer    (i/o): pointer to end of text in CREP
! LOOKUP        : char*(*)   (i)  : 3 character group to expand
!
! data types    : WEBLND, WEBNCM, WEBSHP
!
! calls         : none
!
! REVISION INFO:
!
! $Workfile: webgexp.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/04/2011 10:13:48$
!
! change record :
!
! 29-07-1998    : Made operational - S.Cox
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: CREP    !a01- input report text
CHARACTER(LEN=*), INTENT(OUT)   :: CREP2   !a02- output report text (expanded)
INTEGER,          INTENT(INOUT) :: RPRTEND !a03- end of report text in CREP
CHARACTER(LEN=*), INTENT(IN)    :: LOOKUP  !a04- 3 character group to expand

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  J     !- position of 3 char group in CREP

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

CREP2(1:)    = ' '         !Initialise output argument

!-----------------------------------------------------------------------
! Find position of 3-character group in CREP. If found, expand the
! input report text - putting in output report text. Added check for
! group being at end of report. IF so, don't expand.
!-----------------------------------------------------------------------

J = INDEX(CREP,LOOKUP)
IF (J > 0 .AND. ((RPRTEND-J) > 4)) THEN
  CREP2(:) = CREP(:)
  CREP2(J+7:RPRTEND+2) = CREP(J+5:RPRTEND)
  CREP2(J+5:J+6)    = ' '
  CREP(:)  = CREP2(:)
  RPRTEND  = RPRTEND+2
END IF

RETURN
END SUBROUTINE WEBGEXP
