SUBROUTINE ITERAT (ELEM, IREP, TMPELM, NUMBER, IERR)

!-----------------------------------------------------------------------
!
! ROUTINE       : ITERAT
!
! PURPOSE       : to put names from request in an array & replicate
!                 them with instance numbers on end
!
! CALLED BY     : GTGRP
!
! CALLS         : none
!
! PARAMETERS    : ELEM   element name(s) in string                  (i)
!                 IREP   number of replications                     (i)
!                 TMPELM array of element names                     (o)
!                 NUMBER  (input) dimension of TMPELM             (i/o)
!                         (output) number of names after replication
!                 IERR   return code (=3 if a name is too long,     (o)
!                                     =4 if >50 names to replicate)
! REVISION INFO :
!
! $Workfile: iterat.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 16/02/2011 12:13:01$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         16/02/2011 12:13:01    John Norton     Rework
!        done
!  5    MetDB_Refresh 1.4         20/12/2010 13:00:17    Sheila Needham
!       Initialise IERR
!  4    MetDB_Refresh 1.3         24/11/2010 12:33:35    Sheila Needham  Merge
!       changes from basic test
!  3    MetDB_Refresh 1.2         18/11/2010 11:41:33    Richard Weedon
!       Updated Var Declarations
!  2    MetDB_Refresh 1.1         19/10/2010 14:34:51    Brian Barwell
!       Porting items 1-8, 12, 13 & removal of program change indicators done.
!       
!  1    MetDB_Refresh 1.0         12/10/2010 16:27:55    Brian Barwell
!       Initial f77 version before porting to f90/95.
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

! Use statements:
! <Interfaces>

! None

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*),  INTENT(IN)    :: ELEM      ! String of element names
INTEGER,       INTENT(IN)    :: IREP      ! Replication count
CHARACTER(36), INTENT(INOUT) :: TMPELM(:) ! Array of element names
INTEGER,       INTENT(INOUT) :: NUMBER    ! (IN) Dimension of TMPELM
                                          ! (OUT) Number of names
INTEGER,       INTENT(OUT)   :: IERR      ! return code

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

! LENGTH_NAME(50) may overflow (ELEM*1000 & STACK*1(1000) too) if more nesting

INTEGER    ::     LENGTH_NAME(50) ! length of each name
INTEGER    ::     I           ! pointer to string, then loop var
INTEGER    ::     J           ! loop variable
INTEGER    ::     LENGTH      ! length of name
INTEGER    ::     NELEM       ! number of names found in string
INTEGER    ::     START       ! start of name in string

LOGICAL    ::     MANY        ! set if '(', so may be >1 name
LOGICAL    ::     DONE        ! set if no more names to find

CHARACTER(LEN=4)  ::   CNUM        ! instance number in figure(s)

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

NELEM=0
DONE=.FALSE.
IERR = 0

! If the string starts (& ends) with brackets, there is probably more
! than one name

IF (ELEM(1:1) == '(') THEN
  MANY=.TRUE.
  START=2
ELSE
  MANY=.FALSE.
  START=1
END IF
I=START

! Loop round names (if MANY; outer loop) & round characters in name
! (inner loop) looking for delimiter.

DO WHILE (.NOT.DONE)
   DO WHILE ( ELEM(I:I) /= ' '.AND.ELEM(I:I) /= ')')
     I=I+1
     IF (I > LEN(ELEM) ) EXIT
   ENDDO
   IF (I >= LEN(ELEM)) DONE=.TRUE.      !2.2
   IF (I <= LEN(ELEM)) THEN
     IF ( ELEM(I:I) == ')') DONE=.TRUE.      !2.2
   END IF

! Keep name (if not too long) & length of name (if not too many)

  LENGTH=I-START
  IF (LENGTH > 32) THEN
    IERR=3
    RETURN
  ELSE IF (LENGTH > 0) THEN
    NELEM=NELEM+1
    TMPELM(NELEM)=ELEM(START:I-1)
    IF (NELEM <= 50) LENGTH_NAME(NELEM)=LENGTH
  END IF

! If there could be more than one name in brackets, look on.

  IF (.NOT.MANY) THEN
    DONE=.TRUE.                ! to drop out of loop
  ELSE IF (.NOT.DONE) THEN
    IF (ELEM(I:I) == ' ') I=I+1
    START=I
  END IF
END DO

! The names in the request have been listed: now add numbers.

IF (IREP*NELEM > NUMBER) THEN
  IERR=7
ELSE IF (IERR == 0) THEN
  NUMBER=0
  DO I=1,IREP
    WRITE (CNUM,'(I4)') I
    DO J=1,NELEM
      NUMBER=NUMBER+1

! Only 50 lengths are kept above; delimit any further names by
! looking for a space or _1 followed by a space.
! The original NELEM names are delimited by spaces on the end.
! But _1 is added, so when copying the first NELEM names (with
! different tags) look for _1 followed by space.  (Must be at
! least two spaces on end of max *32 name in *36 string.)

      IF (J <= 50) THEN
        LENGTH=LENGTH_NAME(J)
      ELSE
        IF (I == 1) THEN
          LENGTH=INDEX(TMPELM(J),' ')-1
        ELSE
          LENGTH=INDEX(TMPELM(J),'_1 ')-1
        END IF
      END IF

      IF (I <= 9) THEN
        TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(4:4)
      ELSE IF (I <= 99) THEN
        TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(3:4)
      ELSE IF (I <= 999) THEN
        TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM(2:4)
      ELSE IF (I <= 9999) THEN
        TMPELM(NUMBER)=TMPELM(J)(1:LENGTH)//'_'//CNUM
      END IF
    END DO
  END DO
END IF

IF (IREP == 0) NUMBER=NELEM
RETURN
END SUBROUTINE ITERAT
