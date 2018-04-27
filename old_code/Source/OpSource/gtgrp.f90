SUBROUTINE GTGRP(SP,STACK,BASE,TMPELM,IREP,NUMBER,IERR)

!-----------------------------------------------------------------------
! SUBROUTINE    : GTGRP
!
! PURPOSE       : To get element name(s) from the current innermost
!                 nesting level, replicating if necessary.
!
! DESCRIPTION   : Put characters from BASE to SP in STACK into TMPELM
!                 as one name or several names, replicated if necessary.
!                 If there are other names on the stack (BASE>0) put all
!                 those just handled back on the stack for another level
!                 of nesting to be dealt with.
!
! CALLED BY     : EXPELM
!
! CALLS         : ITERAT to replicate
!                 PUSH & POP to put characters on stack or take them off
!
! PARAMETERS    : SP      stack pointer (>0)                      (i/o)
!                 STACK   stack                                   (i/o)
!                 BASE    points to start of name or open bracket  (i)
!                 TMPELM  array of element names                   (o)
!                 IREP    replication count                        (i)
!                 NUMBER  (input) dimension of TMPELM             (i/o)
!                         (output) number of names after replication
!                 IERR    return code (=3 if name too long,        (o)
!                                      =5 if string too long)
! REVISION INFO :
!
! $Workfile: gtgrp.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 09/02/2011 16:54:47$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         09/02/2011 16:54:47    Sheila Needham
!       Initialise IERR
!  6    MetDB_Refresh 1.5         20/12/2010 12:49:55    Sheila Needham
!       Initialise IERR
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:17:26    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
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

use iterat_mod
use push_mod
use pop_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(INOUT)          ::  SP
CHARACTER(1), INTENT(INOUT)     ::  STACK(:)
INTEGER, INTENT(IN)             ::  BASE
CHARACTER(36), INTENT(OUT)      ::  TMPELM(:)
INTEGER, INTENT(IN)             ::  IREP
INTEGER, INTENT(INOUT)          ::  NUMBER
INTEGER, INTENT(OUT)            ::  IERR

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  TOP        ! last character to handle in STACK
INTEGER      ::  I          ! loop variable
INTEGER      ::  K          ! loop variable

CHARACTER(1000) :: ELEM  ! string for characters from STACK

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

IERR = 0
! POP stack down to previous level, putting characters in ELEM

TOP=SP-BASE+1
IFLABEL1: &
IF (TOP <= LEN(ELEM)) THEN
  DO I=TOP,1,-1
    CALL POP (SP,ELEM(I:I),STACK,IERR)
    IF (IERR == 6) RETURN              ! stack underflow
  END DO

! Replicate names if necessary, into TMPELM

IFLABEL2: &
  IF (ELEM(1:1) == '(' .OR. IREP > 0) THEN
    CALL ITERAT(ELEM(1:TOP),IREP,TMPELM,NUMBER,IERR)

! Or put single element in TMPELM (error if it's too long)

  ELSE
    IF (TOP > 32) THEN
      IERR=3
    ELSE
      NUMBER=1
      TMPELM(NUMBER)(1:36)=ELEM(1:TOP)
    END IF
  END IF IFLABEL2
  IF (IERR > 0) RETURN                 ! name too long, too many?

! If anything is left on STACK (after POPs above), then replications
! are nested.  So put everything back on the stack & continue.

IFLABEL3: &
  IF (SP > 0) THEN                     ! if stack pointer >0
    DO I=1,NUMBER                      ! loop round names
      CALL PUSH(SP,' ',STACK,IERR)     ! space between names

! Loop round characters in each name till space or end of string until
! all names just put in TMPELM are back on STACK, separated by spaces.

      K=1
      DO WHILE (TMPELM(I)(K:K) /= ' ' .AND. K <= 36)
        CALL PUSH(SP,TMPELM(I)(K:K),STACK,IERR)
        IF (IERR == 5) RETURN          ! stack overflow
        K=K+1
      END DO
    END DO
  END IF IFLABEL3

! Error if ELEM not big enough for all element names.

ELSE
  IERR=5
END IF IFLABEL1
RETURN
END SUBROUTINE GTGRP
