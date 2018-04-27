SUBROUTINE SCRIPT(DESCR,ND,DSPLAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : SCRIPT
!
! PURPOSE       : TO EXPAND & DISPLAY A BUFR DESCRIPTOR STRING,
!                 TO SHOW USERS THE ORDER OF VALUES IN ARRAYS.
!                 IF THERE IS DELAYED REPLICATION, THE EXPANSION
!                 CAN'T BE COMPLETED: DESCRIPTORS SUBJECT TO DELAYED
!                 REPLICATION ARE INSET IN THE DISPLAY. (N.B. DELAYED
!                 REPLICATION AND COMPRESSION ARE INCOMPATIBLE)
!                 BLANK LINES BEFORE COORDINATES GROUP ELEMENTS IN A
!                 WAY THAT SUGGESTS COORDINATE DEPENDENCES.
!
!                 ND IS THE NUMBER OF DESCRIPTORS, INCREMENTED WHENEVER
!                 A SEQUENCE IS EXPANDED OR A REPLICATION DONE. IT CAN
!                 ONLY INCREASE.
!
!                 N IS A POINTER TO THE DESCRIPTOR SEQUENCE. IT CAN ONLY
!                 ADVANCE, ALTHOUGH THE SEQUENCE MAY GROW LONGER AHEAD
!                 OF IT.
!
!                 NESTLE IS THE NESTING LEVEL, INCREMENTED WHENEVER A
!                 DELAYED REPLICATION IS FOUND. LINES IN THE DISPLAY
!                 ARE INSET BY NESTLE*2. NEST(NESTLE) IS THE NUMBER OF
!                 DESCRIPTORS FOR DELAYED REPLICATION. IT CAN GROW
!                 THROUGH SEQUENCE EXPANSION & REPLICATION, AND IT IS
!                 DECREMENTED AS ELEMENT DETAILS ARE DISPLAYED. THE
!                 NESTING LEVEL DROPS WHEN NEST(NESTLE) REACHES ZERO.
!
! CALLS         : DESFXY, TABLEB, TABLED, LOCALD
!
! ARGUMENTS     : (1) SEQUENCE OF DESCRIPTORS
!                 (2) NUMBER OF DESCRIPTORS IN SEQUENCE
!                 (3) LOGICAL VARIABLE SET TO TRUE FOR DISPLAY
!
! REVISION INFO :
!
! $Workfile: script.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 25/01/2011 21:46:44$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         25/01/2011 14:47:32    John Norton
!       Initial preporting version of f77 code.
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

USE desfxy_mod
USE locald_mod
USE tableb_mod
USE tabled_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,          INTENT(INOUT) :: DESCR(:)
INTEGER,          INTENT(INOUT) :: ND
LOGICAL,          INTENT(IN)    :: DSPLAY

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  F
INTEGER          ::  I
INTEGER          ::  IASSOC
INTEGER          ::  IN
INTEGER          ::  ISCALE
INTEGER          ::  LASTX
INTEGER          ::  N
INTEGER          ::  NEST(0:9)
INTEGER          ::  NESTLE
INTEGER          ::  NEXTRA
INTEGER          ::  NSEQ
INTEGER          ::  REFVAL
INTEGER          ::  SCALE
INTEGER          ::  SEQ(999)
INTEGER          ::  VER        !Argument used in call to tableb
INTEGER          ::  WIDTH
INTEGER          ::  X
INTEGER          ::  Y

CHARACTER(LEN=1)  ::  FORMAT
CHARACTER(LEN=60) ::  NAME
CHARACTER(LEN=20) ::  SPACES=' '
CHARACTER(LEN=24) ::  UNITS

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! EXPRESS DESCRIPTOR AS F,X,Y & CHECK FOR OPERATORS.
!
! EITHER CARRY OUT A REPLICATION OR INSET THE ELEMENTS IT COVERS.
!-----------------------------------------------------------------------

N=1
NESTLE=0
NEST(0)=ND
    1 CONTINUE
CALL DESFXY(DESCR(N),F,X,Y)

IFLABEL1: &
IF (F == 1) THEN

!-----------------------------------------------------------------------
! IF THE REPLICATION COUNT IS IN THE DESCRIPTOR, USE IT
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------

IFLABEL2: &
  IF (Y /= 0) THEN

!-----------------------------------------------------------------------
! WORK OUT HOW MANY EXTRA DESCRIPTORS, MOVE THE REST DOWN TO MAKE ROOM
! (WORKING FROM RIGHT TO LEFT TO AVOID REPETITION!), & REPEAT FROM LEFT
! TO RIGHT TO FILL THE EMPTY SLOT.
!-----------------------------------------------------------------------

    NEXTRA=X*(Y-1)

!-----------------------------------------------------------------------
! FIRST MAKE ROOM
!-----------------------------------------------------------------------

    DO I=ND,N+X+1,-1
      DESCR(I+NEXTRA)=DESCR(I)
    END DO

!-----------------------------------------------------------------------
! THEN REPEAT (BUNCH WILL RECUR AT INTERVALS OF X) & ADJUST COUNTS
!-----------------------------------------------------------------------

    DO I=1,NEXTRA
     DESCR(N+X+I)=DESCR(N+I)
    END DO

    ND=ND+NEXTRA
    NEST(NESTLE)=NEST(NESTLE)+NEXTRA
  ELSE

!-----------------------------------------------------------------------
! IF THE REPLICATION COUNT IS ZERO, INSET THE DESCRIPTORS TO BE COPIED
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------

    IF (DSPLAY) PRINT *,SPACES(1:1+2*NESTLE),'REPLICATION FACTOR'
    N=N+1
    NESTLE=NESTLE+1
    NEST(NESTLE)=X+1
    NEST(NESTLE-1)=NEST(NESTLE-1)-(X+2)
  END IF IFLABEL2

!-----------------------------------------------------------------------
! THE ONLY F=2 OPERATIONS THAT NEED TO BE CONSIDERED ARE SCALING & Q/C
!-----------------------------------------------------------------------

ELSE IF (F == 2) THEN
  IF (X == 2 .AND. Y > 0) ISCALE=Y-128
  IF (X == 2 .AND. Y == 0) ISCALE=0
  IF (X == 4) IASSOC=Y

!-----------------------------------------------------------------------
! LOOK UP A SEQUENCE  (OVERWRITE THE SEQUENCE DESCRIPTOR ITSELF)
!-----------------------------------------------------------------------

ELSE IF (F == 3) THEN
  CALL LOCALD(X,Y,SEQ,NSEQ,' ',' ')
  IF (NSEQ == 0) CALL TABLED(X,Y,SEQ,NSEQ)

!-----------------------------------------------------------------------
! INSERT SEQUENCE OF DESCRIPTORS, MOVING THE REST DOWN. ADJUST TOTAL.
!-----------------------------------------------------------------------

  DO I=ND,N+1,-1
    DESCR(I+NSEQ-1)=DESCR(I)
  END DO

  DO I=1,NSEQ
    DESCR(N+I-1)=SEQ(I)
  END DO

  ND=ND+NSEQ-1
  NEST(NESTLE)=NEST(NESTLE)+NSEQ

!-----------------------------------------------------------------------
! PRINT OUT DETAILS OF ELEMENT   (INSET IF NECESSARY)
! WITH A BLANK LINE BEFORE THE START OF ANY COORDINATE CHANGE(S)
!-----------------------------------------------------------------------

ELSE IF (DSPLAY .AND. F == 0) THEN
  CALL TABLEB(X,Y,VER,SCALE,REFVAL,WIDTH,FORMAT,NAME,UNITS)
  IF (X <= 7 .AND. LASTX > 7) PRINT *,SPACES

  IN=1+2*NESTLE
  IF (IASSOC > 0.AND.X /= 31) PRINT *,SPACES(1:1+IN),'Q/C FIELD'
  PRINT *,SPACES(1:IN),NAME(1:60-IN), UNITS(1:12), SCALE+ISCALE
END IF IFLABEL1

!-----------------------------------------------------------------------
! MOVE PAST THIS DESCRIPTOR & LOOP IF THERE ARE ANY LEFT.
! DROP THE NESTING LEVEL IF A COUNT HAS REACHED ZERO.     (N.B. NESTED
! REPLICATIONS CAN END AT THE SAME POINT, SO THE LEVEL MAY DROP BY >1)
!-----------------------------------------------------------------------

NEST(NESTLE)=NEST(NESTLE)-1
  500 CONTINUE
IF (NEST(NESTLE) <= 0) THEN
  NESTLE=NESTLE-1
  IF (NESTLE > 0) GO TO 500
END IF
IF (F /= 3) N=N+1
IF (F == 0) LASTX=X
IF (N <= ND) GO TO 1

RETURN
END SUBROUTINE SCRIPT
