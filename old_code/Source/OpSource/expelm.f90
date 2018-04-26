SUBROUTINE EXPELM(REQ,IPOS,ILEN,USRELM,NUM,QCFLAG,IFAIL,CERR)

!-----------------------------------------------------------------------
! ROUTINE       : EXPELM
!
! PURPOSE       : To extract the element names from a users request
!
! DESCRIPTION   : PARSE request a character at a time, calling GTGRP
!                 to list element names (& replicate with numbers on
!                 end if necessary) when a space or ')*n' is found,
!                 or stopping if a keyword is found among the names.
!                 Negative values of IERR from PARSE refer to e.g.
!                 unexpected spaces, positive values are errors.
!
! CALLED BY     : MDB
!
! CALLSE        : PARSE to delimit names & record any replication
!                 GTGRP to put names in array, replicating if necessary
!                 KEYS  to initialise array
!
! ARGUMENTS     : REQ     (ip)  MDB request string
!                 IPOS    (ip)  pointer to first element in request
!                 ILEN    (ip)  length of request string
!                 USRELM  (iop)  array of fully qualified names
!                 NUM     (op)  number of elements output
!                 QCFLAG  (op)  true if last element is +QC_FLAGS
!                 IFAIL   (op)  error code 8-fatal
!                 CERR    (op)  error message
!
! REVISION INFO :
!
! $Workfile: expelm.f90$ $Folder: OpSource$
! $Revision: 11$ $Date: 16/02/2011 12:13:01$
!
! CHANGE RECORD :
!
! $Log:
!  11   MetDB_Refresh 1.10        16/02/2011 12:13:01    John Norton     Rework
!        done
!  10   MetDB_Refresh 1.9         20/12/2010 12:35:59    Sheila Needham
!       Initialise IERR and CERR
!  9    MetDB_Refresh 1.8         24/11/2010 09:25:44    Brian Barwell   Added
!       omitted statement "USE KEYS_MOD".
!  8    MetDB_Refresh 1.7         17/11/2010 15:52:55    John Norton     After
!       merge batch 8 changes
!  7    MetDB_Refresh 1.6         01/11/2010 15:07:07    Stan Kellett    rework
!        done after review
!  6    MetDB_Refresh 1.5         20/10/2010 09:04:07    Stan Kellett    ready
!       for review, comments need taking out from USE statements once mod
!       files all ready
!  5    MetDB_Refresh 1.4         18/10/2010 14:53:09    Stan Kellett
!       Corrected assumed array line 71 from dimension (*) to (:)
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
USE GTGRP_MOD
USE KEYS_MOD
USE PARSE_MOD

IMPLICIT NONE

! Parameters
INTEGER, PARAMETER             :: NKEYS=33 ! number of keywords

! Arguments in order (see above)
CHARACTER(LEN=*), INTENT(IN)   :: REQ       ! MetDB Request String
INTEGER, INTENT(INOUT)         :: IPOS      ! pointer to first element in request
INTEGER, INTENT(IN)            :: ILEN      ! Length of request string
CHARACTER(LEN=36), INTENT(INOUT) :: USRELM(:) ! array of fully qualified array names
INTEGER, INTENT(OUT)           :: NUM       ! Number of elements output
LOGICAL, INTENT(OUT)           :: QCFLAG    ! true if last el +QC_FLAG
INTEGER, INTENT(OUT)           :: IFAIL     ! Error code 8 - fatal
CHARACTER(LEN=*), INTENT(OUT)  :: CERR      ! Error message

! Other variables:
! (The dimension of LEVEL is kept low, because more than 2 levels of
!  nesting is (a) unlikely to occur & (b) if it did occur, it would
!  probably need big increases in the sizes of STACK & ELEM to hold
!  all the names expanded out before the last nesting level.)

INTEGER                        :: I         ! loop counters
INTEGER                        :: J         ! Loop counter
INTEGER                        :: SP        ! stack pointer
INTEGER                        :: NUMBER    ! number of names from GTGRP
INTEGER                        :: IERR      ! return code
INTEGER                        :: IREP      ! replication count
INTEGER                        :: INDEX     ! pointer to CHAR in REQ
INTEGER                        :: NEST      ! nesting level
INTEGER                        :: COUNT     ! total number of element names
INTEGER                        :: LEVEL(3)  ! nesting start at each level
INTEGER                        :: BASE      ! start of group for GTGRP
INTEGER                        :: OBRKT     ! number of open brackets
INTEGER                        :: CBRKT     ! number of closed brackets

CHARACTER(LEN=1)               :: CHAR      ! current character from REQ
CHARACTER(LEN=1)               :: NEXT      ! set if * or figure expected
CHARACTER(LEN=1)               :: STACK(1000) ! current group(s)
CHARACTER(LEN=36)              :: TMPELM(12000) ! names from GTGRP
CHARACTER(LEN=36)              :: WORDS(NKEYS) ! request keywords

LOGICAL                        :: GETGRP     ! set if GTGRP to be called
LOGICAL                        :: LKEY        ! set if keyword found among names
LOGICAL                        :: EXPECT ! set if ) or * expected next

! Dynamic common. Compile with FPARMS='DC(*)' on IBM mainframe

COMMON/EXPELM1/STACK,TMPELM,WORDS

GETGRP=.FALSE.
LKEY=.FALSE.
NEST=0
BASE=0
SP=0
IERR=0
IREP=0
OBRKT=0
CBRKT=0
INDEX=IPOS
COUNT=0
QCFLAG=.FALSE.
EXPECT=.FALSE.
NEXT='/'
IFAIL = 0
CERR = ' '

CALL KEYS(WORDS)

! Look at each character of request in turn in PARSE, checking IERR
! on return to see if it's the end of a group.

PARSELOOP: &
DO WHILE (INDEX <= ILEN .AND. IERR == 0 .AND. .NOT.LKEY)
  CHAR=REQ(INDEX:INDEX)
  CALL PARSE(CHAR,SP,STACK,IREP,OBRKT,CBRKT,LEVEL,NEST,IERR,      &
             EXPECT,NEXT)

! If no serious error (only warnings) & not unmatched brackets at end
! of request...

CHK1:  &
  IF (IERR <= 0 .AND.                                             &
           .NOT.(ILEN == INDEX .AND. CBRKT /= OBRKT)) THEN

! IERR=0: only get group if end of string (i.e. no other delimiter)

ERRCHK: &
    IF (IERR == 0) THEN
      IF (INDEX == ILEN) GETGRP=.TRUE.
      INDEX=INDEX+1

! IERR=-2: get group, assuming no more figures in replication count
!         (but don't increment INDEX if space may be needed between
!          nested names?)

    ELSE IF (IERR == -2) THEN
      IF (INDEX == ILEN .OR. SP == 0) INDEX=INDEX+1
      GETGRP=.TRUE.

! IERR=-3: space between ) and * or between * and figure(s): only get
!          group if end of string (otherwise wait & see what's next)

    ELSE IF (IERR == -3) THEN
      IF (INDEX >= ILEN) GETGRP=.TRUE.
      INDEX=INDEX+1

! IERR=-4: get group, assuming no count after closed bracket

    ELSE IF (IERR == -4) THEN
      GETGRP=.TRUE.

! IERR=-6: get group if end of string or * and brackets match
! (and if * follows & brackets don't match (i.e. nesting),
!  then ignore the space just put on the stack by PARSE)

    ELSE IF (IERR == -6) THEN
      INDEX=INDEX+1
      IF (INDEX == ILEN) THEN
        GETGRP=.TRUE.
      ELSE
        IF (REQ(INDEX:INDEX) == '*') THEN
          IF (OBRKT /= CBRKT) SP=SP-1
        ELSE
          IF (OBRKT == CBRKT) GETGRP=.TRUE.
        END IF
      END IF
    END IF ERRCHK

! Set any negative IERR to zero now that GETGRP has been set.

    IF (IERR <  0) IERR=0

! Get group off stack, either single element or set of elements in
! brackets, replicating if necessary.

    IF (GETGRP) THEN
      IF (NEST == 0) THEN
        BASE=1
      ELSE
        BASE=LEVEL(NEST)
      END IF

      IF (SP >  0) THEN
        NUMBER=12000
        CALL GTGRP(SP,STACK,BASE,TMPELM,IREP,NUMBER,IERR)
      ELSE
        NUMBER=0
      END IF

! Check each element name against keywords, dropping out of PARSE loop
! if a keyword is found.

      IF (IERR <= 0) THEN
        DO J=1,NKEYS
          I=1
          DO WHILE (I <= NUMBER .AND. .NOT.LKEY)
            IF (TMPELM(I) == WORDS(J)) THEN
              LKEY=.TRUE.
              NUMBER=I-1 ! reset NUMBER to drop keyword etc
            ELSE
              I=I+1      ! next name
            END IF
          END DO
        END DO

! If there's any nesting at this point, reduce the nesting level now
! that the descriptors have been expanded out.
! (N.B. PARSE only INcreases NEST, EXPELM only DEcreases it.)

        IF (NEST >= 1) NEST=NEST-1

! Put elements in USRELM, incrementing COUNT.
! (SP is zero after a single name or full expansion of a replication,
!  but >0 if, say, only an inner replication has been expanded...)

        IF (SP == 0) THEN
          DO I=1,NUMBER
            USRELM(COUNT+I)(1:36)=TMPELM(I)
          END DO
          COUNT=COUNT+NUMBER
        END IF

! Reset IREP & GETGRP for next time.

        IREP=0
        GETGRP=.FALSE.
      END IF
    END IF                ! end of GETGRP block
  END IF CHK1
END DO PARSELOOP                   ! end of PARSE loop

! If IERR is positive (negative values are only warnings) or
! brackets don't match, put corresponding error message in CERR.

IF (IERR <= 0 .AND. OBRKT /= CBRKT) IERR=1
IF (IERR >  0) IFAIL=8
IF (IERR == 1) CERR='unmatched brackets in request string'
IF (IERR == 2) CERR='* not followed by count in request string'
IF (IERR == 3) CERR='an element name is too long'
IF (IERR == 4) CERR='too many names to replicate'
IF (IERR == 5) CERR='string of names to replicate is too long'
IF (IERR == 7) CERR='too many names after replication'
IF (IERR == 8) CERR='too many nesting levels (>3)'
IF (IERR == 9) CERR='first figure in replication count is zero'

! If the last element requested is QC_FLAGS, delete it & set a flag.

NUM=COUNT
IPOS=INDEX
IF (USRELM(COUNT) == '+QC_FLAGS') THEN
  NUM=NUM-1
  QCFLAG=.TRUE.
END IF
RETURN
END SUBROUTINE EXPELM
