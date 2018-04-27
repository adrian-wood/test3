      SUBROUTINE PARSE(CHAR,SP,STACK,IREP,OBRKT,CBRKT,LEVEL,NEST,IERR,
     &                 EXPECT,NEXT)

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
! calls         : PUSH    to put character on stack
!
! parameters    : CHAR    character from request string            (i)
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
! $Revision: 1$
! $Date: 30/01/2006 20:23:53$
! $Source: /data/us0400/mdb/op/lib/source/RCS/parse.f,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:53    Sheila Needham  
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

      IMPLICIT NONE

! Arguments in order (see above)

      CHARACTER*1      CHAR
      INTEGER          SP
      CHARACTER*1      STACK(*)
      INTEGER          IREP
      INTEGER          OBRKT
      INTEGER          CBRKT
      INTEGER          LEVEL(*)
      INTEGER          NEST
      INTEGER          IERR
      LOGICAL          EXPECT
      CHARACTER*1      NEXT

      INTEGER          NUMLEN    ! number of figures after *
      INTEGER          IFIG      ! figure of replication count
      INTEGER          PASTSP    ! to point back to start of group
      CHARACTER*1      TCHAR     ! character just POPped off stack

      DATA             NUMLEN/0/

      CHARACTER*132    HEAD
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/parse.f,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:23:53$ '

! NEXT shows whether a certain character or kind of character is
! expected.  But when set to space it means that a space must be put
! on the stack, because otherwise two names would be run together,
! i.e. NAMEA) NAMEB would become NAMEANAMEB.

! A name is delimited by ' ', ')' or *, a count after * by ' ' or ')'.
! Spaces between brackets & names and either side of * are optional.
! The numbers of open & closed brackets must in the end be equal, but
! 3 levels of nesting allow up to 3 more open brackets at some points.

      IF (NEXT.EQ.' ') THEN
        NEXT='/'                           ! nothing expected
        IF (SP.GT.0) THEN                  ! if stack's not empty...
          CALL PUSH(SP,' ',STACK,IERR)     ! put space on stack
          IF (IERR.EQ.5) RETURN            ! stack overflow
        ENDIF
      ENDIF

! If no particular character is expected...

      IF (.NOT.EXPECT) THEN
        IF (CHAR.EQ.'(') THEN              ! open bracket
          OBRKT=OBRKT+1                    ! count it
          NEST=NEST+1                      ! & up nesting level

          IF (NEST.GT.3) THEN              ! only 3 levels allowed
            IERR=8                         !  (change string lengths
            RETURN                         !   as well as array dim-
          ENDIF                            !   ensions to allow more)

          CALL PUSH(SP,CHAR,STACK,IERR)
          LEVEL(NEST)=SP                   ! keep start of nesting

        ELSE IF (CHAR.EQ.')') THEN         ! closed bracket
          IF (CBRKT.LT.OBRKT) THEN         ! if brackets not balanced
            CBRKT=CBRKT+1                  ! count this one
            CALL PUSH(SP,CHAR,STACK,IERR)  ! put bracket on stack
            EXPECT=.TRUE.
            NEXT='*'                       ! & expect replication count
          ELSE                             ! otherwise must be error
            IERR=1                         ! unbalanced brackets
          ENDIF

        ELSE IF (CHAR.EQ.'*') THEN         ! * for replication count
          EXPECT=.TRUE.
          NEXT='-'                         ! so expect figure(s)

! If a replication inside brackets is just NAME*n (the asterisk was
! found when not expected, so this replication involves no brackets),
! replicate one group only - rather than from the last open bracket.
! So find previous space to set start of nesting.  (Shouldn't get
! back to start of stack?)

          IF (OBRKT.NE.CBRKT) THEN         ! if in brackets
            PASTSP=SP                      ! to go back past space
            TCHAR='*'                      ! to start DO WHILE
            DO WHILE (TCHAR.NE.' ' .AND. PASTSP.GT.0)
              CALL POP(PASTSP,TCHAR,STACK,IERR)
            ENDDO

! When the space is found, the stack pointer points to the end of the
! previous group, so add 2 for the start of this group.
! (N.B. PARSE only INcreases NEST, EXPELM only DEcreases it.)

            NEST=NEST+1                    ! up nesting level

            IF (NEST.GT.3) THEN            ! only 3 levels allowed
              IERR=8                       !  (change string lengths
              RETURN                       !   as well as array dim-
            ENDIF                          !   ensions to allow more)

            LEVEL(NEST)=PASTSP+2           ! start of nested group
          ENDIF

! If a space is found, put it on stack if inside brackets.
! (Groups inside brackets must be handled together; otherwise
! space marks end of group.)

        ELSE IF (CHAR.EQ.' ') THEN
          IERR=-6
          IF (OBRKT.NE.CBRKT) CALL PUSH(SP,CHAR,STACK,IERR)

! If it's any other character, just put it on the stack.

        ELSE IF (CHAR.NE.' ') THEN
          CALL PUSH(SP,CHAR,STACK,IERR)
        ENDIF

! If * or figure is expected...

      ELSE
        IF (CHAR.NE.' ') THEN
          IF (NEXT.EQ.'*' .AND. CHAR.EQ.'*') THEN
            NEXT='-'                       ! expect figure next
          ELSE IF (NEXT.EQ.'-') THEN       ! if figure expected

! If figure found, build up count (unless first figure is zero - error)

            IF (CHAR.GE.'0' .AND. CHAR.LE.'9') THEN
              IF (NUMLEN.EQ.0) THEN        ! if no figures yet
                IF (CHAR.EQ.'0') IERR=9    ! then error if zero
                IF (IERR.EQ.0) THEN        ! otherwise start count
                  READ(CHAR,'(I1)') IREP
                  NUMLEN=NUMLEN+1
                ENDIF
              ELSE IF (NUMLEN.GT.0) THEN   ! if figures already
                IREP=IREP*10               ! count so far
                READ(CHAR,'(I1)') IFIG     ! next figure
                IREP=IREP+IFIG             ! combine the two
                NUMLEN=NUMLEN+1            ! one more figure
              ENDIF

! If count expected but figure not found, must be end of count if
! figure(s) found already.  (This copes with ...)*n)...?)

            ELSE IF (NUMLEN.GT.0) THEN     ! if some figures already
              IERR=-2
              EXPECT=.FALSE.
              NUMLEN=0                     ! no more figures
              NEXT='/'

! Or error if count expected & no figures found.

            ELSE IF (NUMLEN.EQ.0) THEN
              IERR=2
            ENDIF

! If * expected but not found - just brackets with no count to follow?
! (EXPECT means * or figure next, so ELSE means above condition.)

          ELSE
            IERR=-4
            EXPECT=.FALSE.
            NEXT=' '
          ENDIF

! If character is space...
! then warning if it's between ) and * or between * and count

        ELSE
          IF (NEXT.EQ.'*') THEN
            IERR=-3
          ELSE IF (NEXT.EQ.'-' .AND. NUMLEN.EQ.0) THEN
            IERR=-3

! but if it's elsewhere or ends the count...

          ELSE
            IERR=-2
            IF (NEXT.EQ.'/') THEN
              IERR=0
              EXPECT=.FALSE.
              CALL PUSH(SP,CHAR,STACK,IERR)
            ELSE
              NEXT='/'
            ENDIF

! No more figures expected in count if figure(s) already found.
! (i.e. space delimits count, can't have space between figures
! - whereas space between ) and * or between * and count is OK)

            IF (NUMLEN.GT.0) THEN
              NUMLEN=0
              EXPECT=.FALSE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      RETURN
      END
