SUBROUTINE BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,          &
VALUES,IVAL,NINCREM,IVER,IRC)

! ---------------------------------------------------------------------
!
! Program       : BUFDRIP
!
! Called by     : BUFDATA, BUFDRPL
!
! Purpose       : (a) When called by BUFDRPL: to list any increments
!  (informal)     before the replication operator & include the
!                 increment descriptors in the replication.
!                 (b) When called by BUFDATA: to put the increment
!                 corresponding to an increment descriptor in the
!                 values array.
!  (formal)       (a) If the previous element is in a coordinate class
!                 and has 'increment' in its name, then list it with a
!                 pointer (IVAL) to the corresponding value.  Do this
!                 until an element that is not an increment is found.
!                 (b) Loop round the listed increments to match the
!                 element descriptor & put the corresponding value
!                 in the values array (NOBS times if data compressed).
!
! Calls         : DESFXY to split 16-bit descriptor into F, X & Y
!                 TABLEB to find standard element details
!
! Parameters    :
!  (1) CMPRES   true if data in BUFR message is compressed          (i)
!                (not changed)
!  (2) NOBS     total number of obs in message                      (i)
!                (not changed)
!  (3) DESCR    descriptor array                                    (i)
!                (not changed)
!  (4) N        number of current descriptor (operator or increment)(i)
!                (not changed)
!  (5) MAXVAL   dimension of VALUES (to check for space left)       (i)
!                (not changed)
!  (6) VALUES   value array                                        (i/o)
!                (returned with increment set - in (b) calls)
!  (7) IVAL     current subscript in VALUES                        (i/o)
!                (incremented)
!  (8) NINCREM  number of increments found (elements, not counting  (o)
!               any operators associated with them)
!  (9) IVER     TableB version number                               (i)
!                (returned for use in replication operation)
! (10) IRC      return code:                                        (o)
!                IRC=111: not enough room left in value array
!                IRC=112: increment not found in list
!
! REVISION INFO :
!
! $Workfile: bufdrip.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  7    MetDB_Refresh 1.6         09/02/2011 16:26:16    Sheila Needham
!       Initialise IRC
!  6    MetDB_Refresh 1.5         12/11/2010 09:54:17    Richard Weedon  rework
!        after peer review
!  5    MetDB_Refresh 1.4         25/10/2010 16:57:39    Richard Weedon
!       Updated to f90 standard
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:24    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
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
! ---------------------------------------------------------------------

USE desfxy_mod
USE tableb_mod

IMPLICIT  NONE

!  STEP & STACK collect increments for a whole message,
! with no reference to replication structure (nested
! or not).  This should work for any structure except
! one which increments the same element in two nested
! replications, which seems nonsensical.
!  But this means that a message with many non-nested
! replications incrementing different elements could
! fill the arrays.  If so, just empty them & carry on,
! with a warning: there's a good chance that the lost
! increments are no longer needed.  (If they are, the
! decode will fail 112.)

INTEGER,PARAMETER ::  NARRAY=20
INTEGER           ::  NSTEPS       ! number of elements in STEP/STACK
INTEGER           ::  STEP(NARRAY) !pointers to VALUES of elem in STACK
INTEGER           ::  STACK(NARRAY) ! desc for incremented elements

INTEGER,INTENT(IN)       ::  NOBS         ! argument (2)
INTEGER,INTENT(IN)       ::  DESCR(*)     ! argument (3)
INTEGER,INTENT(IN)       ::  N            ! argument (4)
INTEGER,INTENT(INOUT)    ::  IVAL         ! argument (7)
INTEGER,INTENT(OUT)      ::  IRC          ! argument (10)
INTEGER,INTENT(IN)       ::  MAXVAL       ! argument (5)
INTEGER,INTENT(IN)       ::  IVER         ! argument (9)
INTEGER                  ::  I,IS         ! loop variables
INTEGER,INTENT(OUT)      ::  NINCREM      ! NUMBER OF INCREMENTS FOUND
INTEGER                  ::  LASTL  ! subscript of previous descriptor
INTEGER                  ::  F,X,Y  ! components of descriptor FXXYYY
INTEGER                  ::  WIDTH  ! dummy argument for TABLEB call
INTEGER                  ::  SCALE  ! dummy argument for TABLEB call
INTEGER                  ::  REFVAL ! dummy argument for TABLEB call
INTEGER                  ::  FLINCR ! descriptor flag to mark increments

LOGICAL,INTENT(IN)   ::  CMPRES ! argument (1)
LOGICAL              ::  LINK  ! set if last descriptor was an increment
LOGICAL              ::  LISTED !set if stack has entry for this element
CHARACTER(LEN=60)    ::  NAME   ! element name from TABLEB
CHARACTER(LEN=24)    ::  UNITS  ! dummy argument for TABLEB call
CHARACTER(LEN=1)     ::  FORMAT ! dummy argument for TABLEB call
REAL,INTENT(INOUT)   ::  VALUES(*)
! DOUBLE PRECISION VALUES(*)

DATA FLINCR/262144/      ! 2**18 to set flag

LOGICAL ::  HEADSET=.FALSE.

SAVE

IRC = 0

IF (.NOT.HEADSET) THEN
  HEADSET=.TRUE.
  NSTEPS=0
END IF

! See if current descriptor is a replication operator or an increment.

CALL DESFXY(DESCR(N),F,X,Y)

! ---------------------------------------------------------------------
! If it's a replication operator, look for increment descriptors before
! it.  List the descriptors and the corresponding value subscripts.
! ---------------------------------------------------------------------

! If there are increments immediately before the replication, copy them
! at the end of the string to be repeated and flag them so that they're
! interpreted as having no more corresponding fields in the bit string.
! First find how many increments there are (NINCREM) by checking for the
! coordinate classes (4-7) and the word 'increment' in the element name.
! Allow for operators (F=2) among the coordinate descriptors.
! N.B. NINCREM is used to set value subscript, so ignores operators!
! (TABLEB is called only to get the name.)
! First empty the list of increments (NSTEPS=0) if it's a new message.

IF_CONSTR1 : &
IF (F == 1) THEN
  IF (NSTEPS > 0 .AND. IVAL <= STEP(1)) NSTEPS=0
  NINCREM=0              ! zero count of increment descriptors
  LASTL=N-1
  LINK=.TRUE.            ! to start loop...

! Loop while consecutive increments are being found

  DO_CONSTR1 : &
  DO WHILE (LASTL > 0 .AND. LINK)
    LINK=.FALSE.
    CALL DESFXY(DESCR(LASTL),F,X,Y)

! If it's an element descriptor from a coordinate class
! and not flagged as an increment in a previous replication,
! get the name from Table B & check for increment.

    IF_CONSTR2 : &
    IF (F == 0 .AND. X >= 4.AND.X <= 7 .AND.             &
     &        MOD(DESCR(LASTL)/FLINCR,2) /= 1) THEN
      CALL TABLEB(X,Y,IVER,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)

      IF_CONSTR3 : &
      IF (WIDTH > 0.AND.INDEX(NAME,' INCREMENT ') > 0) THEN
        NINCREM=NINCREM+1
        LINK=.TRUE.

! Loop round the stack: is there already an increment for this element?

        LISTED=.FALSE.
        I=0
        DO WHILE (.NOT.LISTED .AND. I < NSTEPS)
          I=I+1
          IF (STACK(I) == DESCR(LASTL)+FLINCR) LISTED=.TRUE.
        END DO

! If so, reset the pointer; if not, add a new entry to the stack
! (If the stack is full, empty it - there's a good chance that only
!  increments from previous replications will be lost - & continue.)
! So NSTEPS may be less than NINCREM (but NINCREM must be returned).
! (Keep a pointer rather than the value itself to allow different
! increments in data compressed together)

        IF (LISTED) THEN
          STEP(I)=IVAL-NOBS*NINCREM
        ELSE
          NSTEPS=NSTEPS+1
          IF (NSTEPS > NARRAY) THEN
            PRINT *,'BUFDRIP: too many replicated increments.'
            PRINT *,'Some values may be lost: make NARRAY bigger.'
            NSTEPS=1
          END IF
          STACK(NSTEPS)=DESCR(LASTL)+FLINCR
          STEP(NSTEPS)=IVAL-NOBS*NINCREM
        END IF
      END IF IF_CONSTR3  ! end of IF for 'increment' in name
      LASTL=LASTL-1
    ELSE IF (F == 2) THEN! If it's an operator,
      LASTL=LASTL-1      ! go back past it
      LINK=.TRUE.        ! & carry on round loop.
    END IF IF_CONSTR2    ! end of IF for coord class (& F=0 or 2)
  END DO DO_CONSTR1      ! end of loop round consecutive incremnts

! ---------------------------------------------------------------------
! If it's an increment descriptor (F=0), look it up in the value list.
! ---------------------------------------------------------------------

! Look up the increment descriptor in the list of those with values
! currently set.  Look back from end of list.

ELSE IF (F /= 1) THEN
  IS=NSTEPS
  DO WHILE (IS > 0 .AND. DESCR(N) /= STACK(IS))
    IS=IS-1
  END DO

! If the descriptor is in the list, and there's room for NOBS values
! of this element, set the increment (NOBS times if data compressed).

  IF_CONSTR4 : &
  IF (IS > 0) THEN
    IF (IVAL+NOBS > MAXVAL) THEN
      PRINT *,' not enough room left in value array'
      IRC=111
      RETURN
    END IF

! Put increment in value array NOBS times if data compressed, once only
! if not.  (N.B. if NOBS were always 1 without compression there would
! be no need for the IF, but NOBS>1 is possible without compression!)

    IF (CMPRES) THEN
      DO I=0,NOBS-1
        VALUES(IVAL+I)=VALUES(STEP(IS)+I)
      END DO
    ELSE
      VALUES(IVAL)=VALUES(STEP(IS))
    END IF
    IVAL=IVAL+NOBS
  ELSE
    PRINT *,' Replicated increment: value not listed in BUFDRIP'
    IRC=112
  END IF IF_CONSTR4
END IF IF_CONSTR1
RETURN
END SUBROUTINE BUFDRIP
