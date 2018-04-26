LOGICAL FUNCTION EXPREQ(IDESC,NDES)

!-----------------------------------------------------------------------
!
! FUNCTION      : EXPREQ
!
! PURPOSE       : TO DETERMINE WHETHER ELEMENTS SELECTED BY THE USER
!                 CAN BE RETURNED WITHOUT EXPANDING A REPORT
!                 BYPASSING REPORT EXPANSION.
!
! DESCRIPTION   : A LOGICAL VARIABLE IS RETURNED AS TRUE ONLY IF AT
!                 LEAST ONE OF THE ELEMENTS REQUESTED BY THE USER
!                 CAN ONLY BE OBTAINED BY EXPANDING A REPORT.
!
! DATA TYPE(S)  : TAF, METAR, NCM, SREW
!
! CALLED BY     : TFMRET
!
! CALLS         : NONE
!
! ARGUMENTS     : (1) IDESC - ARRAY OF USERS ELEMENTS SELECTED.
!                 (2) NDES  - NUMBER OF ELEMENTS IN ABOVE ARRAY.
!
!Y2K  26.06.1997  EXPREQ is Year 2000 compliant.
!
! REVISION INFO :
!
! $Workfile: expreq.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 17/11/2010 15:52:55$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  6    MetDB_Refresh 1.5         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  5    MetDB_Refresh 1.4         20/10/2010 09:03:14    Stan Kellett    Files
!       ready for review
!  4    MetDB_Refresh 1.3         15/10/2010 16:17:35    Stan Kellett    Basic
!       port almost complete just require the modules added for the interface
!       files they will need.
!  3    MetDB_Refresh 1.2         14/10/2010 17:59:43    Stan Kellett    Basic
!       port done upto changing of argument declarations. Ordering of
!       declarations and interface files still to do.
!  2    MetDB_Refresh 1.1         12/10/2010 10:10:13    Stan Kellett
!       Continuation markers changed to f90 standard.
!       Logical operators such as .LE. .GE. .GT. .LT. etc changed to <=, >=,
!       >, and < as part of porting to f90/95
!  1    MetDB_Refresh 1.0         11/10/2010 13:39:02    Stan Kellett
!       Initial f77 versions before porting to f90/95
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Argument declarations
INTEGER, INTENT(IN)       :: NDES              !
INTEGER, INTENT(IN)       :: IDESC(NDES)       ! User's element array.

! Local variable declarations
INTEGER                   :: FOUND    ! Count of matched elements.
INTEGER                   :: COUNT    ! Count of mapping values checked.
INTEGER                   :: LIST(20) ! Array of mapping values.
INTEGER                   :: USERELM  ! Loop variable of the user's
                                      ! element array IDESC.

LOGICAL                   :: EXIT     ! Exit from loops if true.
LOGICAL                   :: MATCH    ! True if user element matches
                                      ! listed mapping value.

! A list of the mapping values that are obtained from the index or
! trailer.

DATA LIST/131073,1,2,3,4,5,6,7,8,65281,65282,65283,65284,65285,           &
          131081,131082,131086,131087,131090,65250/

! Default the number of matches found to zero.

FOUND=0
USERELM=1
EXIT=.FALSE.

! Loop through the user's elements.
! Reset the LIST array count to 1 and default the match to untrue.

ELMLOOP: &
DO WHILE (USERELM  <=  NDES .AND. .NOT.EXIT)
  MATCH=.FALSE.
  COUNT=1

! Check each element for a match against a mapping value in array LIST.
! Continue through the array until a match is found or all the array
! values have been checked.

  DO WHILE (.NOT.MATCH .AND. COUNT  <=  19)
    IF (IDESC(USERELM)  ==  LIST(COUNT)) THEN
      MATCH=.TRUE.
      COUNT=COUNT+1
      FOUND=FOUND+1
    ELSE
      COUNT=COUNT+1
    END IF
  END DO

! If a user element cannot be matched against any of the mapping values
! in the LIST array then the report requires expansion.

  IF (.NOT.MATCH) THEN
    EXIT=.TRUE.
  ELSE
    USERELM=USERELM+1
  END IF
END DO ELMLOOP

! If a force exit has not been made then all the user's elements must
! have been matched to the array of mapping values so no expansion is
! necessary.

IF (EXIT) THEN
  EXPREQ=.TRUE.
ELSE
  EXPREQ=.FALSE.
END IF

RETURN
END FUNCTION EXPREQ
