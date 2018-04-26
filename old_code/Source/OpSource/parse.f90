SUBROUTINE PARSE(CHAR,SP,STACK,IREP,OBRKT,CBRKT,LEVEL,NEST,IERR, &
                 EXPECT,NEXT)

!-----------------------------------------------------------------------
!
! subroutine    : PARSE
!
! purpose       : Look at a character in the request (one character
!                 per call), setting a return code to tell the calling
!                 program when a name or set of names in brackets has
!                 been delimited.
!
! description   : Count open & closed brackets, noting positions of
!                 open brackets to keep track of nesting.
!                 Expect * after ) and figure(s) after *,
!                 constructing a replication count from those figures.
!
! called by     : EXPELM
!
! calls         : POP     to pull a character off stack
!               : PUSH    to put character on stack
!
! ARGUMENTS     : CHAR    character from request string            (i)
!               : SP      stack pointer (no. of chars on STACK)   (i/o)
!               : STACK   stack (character array)                 (i/o)
!               : IREP    replication count                        (o)
!               : OBRKT   number of open brackets                 (i/o)
!               : CBRKT   number of closed brackets               (i/o)
!               : LEVEL   start of nesting at each level          (i/o)
!               : NEST    nesting level                           (i/o)
!               : IERR    return code                              (o)
!               : EXPECT  set if ) or * found, so * or fig(s) expected
!               : NEXT    set to *, - for figures, / for nothing,
!                          ' ' to put space on stack next time.   (i/o)
!
!                 IERR can take the following values (positive values
!                      are errors, others call for action) here:
!                 9  first figure in replication count is zero
!                 8  too many nesting levels (>3)
!                 5  stack overflow (set by PUSH)
!                 2  * but no count follows
!                 1  brackets don't match
!                 0
!                -2  space marks end of replication count?
!                -3  space between ( and * or between * and figure?
!                -4  ) but no *
!                -6  space where no special character expected
!
!                 (See messages at end of EXPELM for other IERR>0)
!
! revision info :
!
! $Revision: 4$
! $Date: 09/02/2011 17:08:34$
! $Source: /data/us0400/mdb/op/lib/source/RCS/parse.f,v $
!
! change record :
!
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 17:08:34    Sheila Needham  Change
!        INTENT on IREP to inout
!  3    MetDB_Refresh 1.2         21/12/2010 11:34:59    Sheila Needham  Change
!        STACK and LEVEL to assumed shape to agree with interface
!  2    MetDB_Refresh 1.1         20/12/2010 15:10:31    Sheila Needham
!       Initialise IREP and IERR
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.1  2002/11/04  14:57:26  14:57:26  usmdb (Generic MetDB account)
! 18 Nov 2002    C Long
! 2.1  Describe arguments & variables, use DO WHILE instead of GO TO
!      - and comment the code!
!
!
! Revision 2.0  2001/01/08  11:59:03  11:59:03  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:18:08  13:18:08  uspm (Pat McCormack)
! First revisioned version for 1  - with Y2K change
!
! Revision 1.1  1997/02/11 16:47:14  uspm
! Initial revision
!
! 26-09-96  A : S.Cox - changes to allow an element replication of up to
!             :       - 9999
!
! 19-07-93    : the arguments 'expect' and 'next' added to the parameter
!             : list.
!             : bug fixed to handle the case where special characters
!             : are anticipated but more element names are encountered.
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

! Use statements:
! <Interfaces>

USE pop_mod
USE push_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(1), INTENT(IN)    ::  CHAR
INTEGER,      INTENT(INOUT) ::  SP
CHARACTER(1), INTENT(INOUT) ::  STACK(:)
INTEGER,      INTENT(INOUT) ::  IREP
INTEGER,      INTENT(INOUT) ::  OBRKT
INTEGER,      INTENT(INOUT) ::  CBRKT
INTEGER,      INTENT(INOUT) ::  LEVEL(:)
INTEGER,      INTENT(INOUT) ::  NEST
INTEGER,      INTENT(OUT)   ::  IERR
LOGICAL,      INTENT(INOUT) ::  EXPECT
CHARACTER(1), INTENT(INOUT) ::  NEXT

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  NUMLEN = 0 ! number of figures after *
INTEGER      ::  IFIG       ! figure of replication count
INTEGER      ::  PASTSP     ! to point back to start of group
CHARACTER(1) ::  TCHAR      ! character just POPped off stack

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IERR = 0

! NEXT shows whether a certain character or kind of character is
! expected.  But when set to space it means that a space must be put
! on the stack, because otherwise two names would be run together,
! i.e. NAMEA) NAMEB would become NAMEANAMEB.

! A name is delimited by ' ', ')' or *, a count after * by ' ' or ')'.
! Spaces between brackets & names and either side of * are optional.
! The numbers of open & closed brackets must in the end be equal, but
! 3 levels of nesting allow up to 3 more open brackets at some points.

IF (NEXT == ' ') THEN
  NEXT='/'                           ! nothing expected
  IF (SP > 0) THEN                   ! if stack's not empty...
    CALL PUSH(SP,' ',STACK,IERR)     ! put space on stack
    IF (IERR == 5) RETURN            ! stack overflow
  END IF
END IF

! If no particular character is expected...

IFLABEL1: &
IF (.NOT.EXPECT) THEN
IFLABEL2: &
  IF (CHAR == '(') THEN              ! open bracket
    OBRKT=OBRKT+1                    ! count it
    NEST=NEST+1                      ! & up nesting level

    IF (NEST > 3) THEN               ! only 3 levels allowed
      IERR=8                         !  (change string lengths
      RETURN                         !   as well as array dim-
    END IF                           !   ensions to allow more)

    CALL PUSH(SP,CHAR,STACK,IERR)
    LEVEL(NEST)=SP                   ! keep start of nesting

  ELSE IF (CHAR == ')') THEN         ! closed bracket
    IF (CBRKT < OBRKT) THEN          ! if brackets not balanced
      CBRKT=CBRKT+1                  ! count this one
      CALL PUSH(SP,CHAR,STACK,IERR)  ! put bracket on stack
      EXPECT=.TRUE.
      NEXT='*'                       ! & expect replication count
    ELSE                             ! otherwise must be error
      IERR=1                         ! unbalanced brackets
    END IF

  ELSE IF (CHAR == '*') THEN         ! * for replication count
    EXPECT=.TRUE.
    NEXT='-'                         ! so expect figure(s)

! If a replication inside brackets is just NAME*n (the asterisk was
! found when not expected, so this replication involves no brackets),
! replicate one group only - rather than from the last open bracket.
! So find previous space to set start of nesting.  (Shouldn't get
! back to start of stack?)

IFLABEL3: &
    IF (OBRKT /= CBRKT) THEN         ! if in brackets
      PASTSP=SP                      ! to go back past space
      TCHAR='*'                      ! to start DO WHILE
      DO WHILE (TCHAR /= ' ' .AND. PASTSP > 0)
        CALL POP(PASTSP,TCHAR,STACK,IERR)
      END DO

! When the space is found, the stack pointer points to the end of the
! previous group, so add 2 for the start of this group.
! (N.B. PARSE only INcreases NEST, EXPELM only DEcreases it.)

      NEST=NEST+1                    ! up nesting level

      IF (NEST > 3) THEN             ! only 3 levels allowed
        IERR=8                       !  (change string lengths
        RETURN                       !   as well as array dim-
      END IF                         !   ensions to allow more)

      LEVEL(NEST)=PASTSP+2           ! start of nested group
    END IF IFLABEL3

! If a space is found, put it on stack if inside brackets.
! (Groups inside brackets must be handled together; otherwise
! space marks end of group.)

  ELSE IF (CHAR == ' ') THEN
    IERR=-6
    IF (OBRKT /= CBRKT) CALL PUSH(SP,CHAR,STACK,IERR)

! If it's any other character, just put it on the stack.

  ELSE IF (CHAR /= ' ') THEN
    CALL PUSH(SP,CHAR,STACK,IERR)
  END IF IFLABEL2

! If * or figure is expected...

ELSE
IFLABEL4: &
  IF (CHAR /= ' ') THEN
IFLABEL5: &
    IF (NEXT == '*' .AND. CHAR == '*') THEN
      NEXT='-'                       ! expect figure next
    ELSE IF (NEXT == '-') THEN       ! if figure expected

! If figure found, build up count (unless first figure is zero - error)

IFLABEL6: &
      IF (CHAR >= '0' .AND. CHAR <= '9') THEN
IFLABEL7: &
        IF (NUMLEN == 0) THEN        ! if no figures yet
          IF (CHAR == '0') IERR=9    ! then error if zero
          IF (IERR == 0) THEN        ! otherwise start count
            READ(CHAR,'(I1)') IREP
            NUMLEN=NUMLEN+1
          END IF
        ELSE IF (NUMLEN > 0) THEN    ! if figures already
          IREP=IREP*10               ! count so far
          READ(CHAR,'(I1)') IFIG     ! next figure
          IREP=IREP+IFIG             ! combine the two
          NUMLEN=NUMLEN+1            ! one more figure
        END IF IFLABEL7

! If count expected but figure not found, must be end of count if
! figure(s) found already.  (This copes with ...)*n)...?)

      ELSE IF (NUMLEN > 0) THEN      ! if some figures already
        IERR=-2
        EXPECT=.FALSE.
        NUMLEN=0                     ! no more figures
        NEXT='/'

! Or error if count expected & no figures found.

      ELSE IF (NUMLEN == 0) THEN
        IERR=2
      END IF IFLABEL6

! If * expected but not found - just brackets with no count to follow?
! (EXPECT means * or figure next, so ELSE means above condition.)

    ELSE
      IERR=-4
      EXPECT=.FALSE.
      NEXT=' '
    END IF IFLABEL5

! If character is space...
! then warning if it's between ) and * or between * and count

  ELSE
IFLABEL8: &
    IF (NEXT == '*') THEN
      IERR=-3
    ELSE IF (NEXT == '-' .AND. NUMLEN == 0) THEN
      IERR=-3

! but if it's elsewhere or ends the count...

    ELSE
      IERR=-2
      IF (NEXT == '/') THEN
        IERR=0
        EXPECT=.FALSE.
        CALL PUSH(SP,CHAR,STACK,IERR)
      ELSE
        NEXT='/'
      END IF

! No more figures expected in count if figure(s) already found.
! (i.e. space delimits count, can't have space between figures
! - whereas space between ) and * or between * and count is OK)

      IF (NUMLEN > 0) THEN
        NUMLEN=0
        EXPECT=.FALSE.
      END IF
    END IF IFLABEL8
  END IF IFLABEL4
END IF IFLABEL1
RETURN
END SUBROUTINE PARSE
