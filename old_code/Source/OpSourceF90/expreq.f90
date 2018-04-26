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
! PARAMETERS    : (1) IDESC - ARRAY OF USERS ELEMENTS SELECTED.
!                 (2) NDES  - NUMBER OF ELEMENTS IN ABOVE ARRAY.
!
!Y2K  26.06.1997  EXPREQ is Year 2000 compliant.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/expreq.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.0  2001/01/08 11:58:38  usmdb
! Added copright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:09:00  13:09:00  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/17 11:50:08  uspm
! Initial revision
!
! FEB 96         RESTRUCTURE TO INCLUDE NCM AND SREW RETRIEVAL.
!                SINGLE LIST INSTEAD OF INDIVIDUAL LISTS.
!                REMOVE SUBTYPE PARAMETER AND NUMBER OF DESCRIPTORS
!                REQUESTED.
!                ROUTINE WILL ONLY BE CALLED IF THE NUMBER OF
!                ELEMENTS SELECTED BY THE USER IS LESS THAN OR EQUAL
!                TO THE MAXIMUM NUMBER OF MAPPING VALUES THAT MAY
!                BE FOUND FOR A PARTICULAR SUBTYPE FROM THE LIST.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

INTEGER       NDES              !
INTEGER       FOUND             ! Count of matched elements.
INTEGER       COUNT             ! Count of mapping values checked.
INTEGER       IDESC(NDES)       ! User's element array.
INTEGER       LIST(20)          ! Array of mapping values.
INTEGER       USERELM           ! Loop variable of the user's
                                ! element array IDESC.

LOGICAL       EXIT              ! Exit from loops if true.
LOGICAL       MATCH             ! True if user element matches
                                      ! listed mapping value.

! A list of the mapping values that are obtained from the index or
! trailer.

DATA LIST/131073,1,2,3,4,5,6,7,8,65281,65282,65283,65284,65285,&
         &131081,131082,131086,131087,131090,65250/

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/expreq.F,v $&
&'//'$ $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! Default the number of matches found to zero.

FOUND=0
USERELM=1
EXIT=.FALSE.

! Loop through the user's elements.
! Reset the LIST array count to 1 and default the match to untrue.

DO WHILE (USERELM .LE. NDES .AND. .NOT.EXIT)
  MATCH=.FALSE.
  COUNT=1

! Check each element for a match against a mapping value in array LIST.
! Continue through the array until a match is found or all the array
! values have been checked.

  DO WHILE (.NOT.MATCH .AND. COUNT .LE. 19)
    IF (IDESC(USERELM) .EQ. LIST(COUNT)) THEN
      MATCH=.TRUE.
      COUNT=COUNT+1
      FOUND=FOUND+1
    ELSE
      COUNT=COUNT+1
    ENDIF
  END DO

! If a user element cannot be matched against any of the mapping values
! in the LIST array then the report requires expansion.

  IF (.NOT.MATCH) THEN
    EXIT=.TRUE.
  ELSE
    USERELM=USERELM+1
  ENDIF
ENDDO

! If a force exit has not been made then all the user's elements must
! have been matched to the array of mapping values so no expansion is
! necessary.

IF (EXIT) THEN
  EXPREQ=.TRUE.
ELSE
  EXPREQ=.FALSE.
ENDIF

RETURN
END FUNCTION EXPREQ
