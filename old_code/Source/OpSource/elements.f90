SUBROUTINE ELEMENTS (CREQ, NFIRST, NAME, NAMES, LREQ, LNUM, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  ELEMENTS
!
!    To process a user's MDB request string and store the details in an
!    integer array.
!
! DESCRIPTION:
!
!    ELEMENTS decodes a list of required elements as specified in a
!    user's retrieval request string (CREQ) and stores the information
!    in an integer array (LREQ). Element names in CREQ are replaced by
!    their location in a list of available elements (NAME array)
!    obtained from a new-format element index which must have been
!    previously read using a call to subroutine READMAP.  Replication
!    information is indicated in LREQ by negative numbers: -n (n>1) is
!    inserted at the start of a replication where n is the replication
!    count, and -1 marks the end of a replication.
!
!    An error message and non-zero error code is generated for certain
!    errors, mainly syntax errors in the request string: then control
!    is immediately returned to the calling program. A warning message
!    is also output for any element name not found in the list in NAME
!    but no error code is generated for this case.
!
! USAGE:  CALL ELEMENTS (CREQ, NFIRST, NAME, NAMES, LREQ, LNUM, ICODE)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    CREQ     I   C*(*)  User's MDB request string
!    NFIRST   I   I*4    First character in CREQ after 'ELEMENTS'
!    NAME     I   C*36   Array of element names from element index
!    NAMES    I   I*4    Number of element names in NAME array
!    LREQ     O   I*4    Integer array containing CREQ element info
!    LNUM     IO   I*4    Number of integers in LREQ array
!    ICODE    O   I*4    Return code - see below for details
!
! RETURN CODES:
!
!      0  CREQ string processed with no errors.
!    141  Output array 'LREQ' not big enough.
!    142  Missing open bracket in request string CREQ.
!    143  Replication indicator ('*') missing after ')' in CREQ.
!    144  Missing replication count in CREQ after '*'.
!    145  Unclosed bracket at end of request string.
!    146  Internal read failure decoding replication count.
!
! CALLS:  ELEMENTS does not call any other routines.
!
! REVISION INFO:
!
!    $Workfile: elements.f90$ $Folder: OpSource$
!    $Revision: 5$ $Date: 26/06/2012 16:52:50$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         26/06/2012 16:52:50    Brian Barwell   Code
!       to handle replication counts of 1 removed. End of replication
!       indicator changed to -9999999.
!  4    MetDB_Refresh 1.3         20/12/2010 12:32:17    Sheila Needham  Change
!        INTENT on LNUM to INOUT
!  3    MetDB_Refresh 1.2         25/10/2010 17:06:23    Brian Barwell   Made
!       arrays assumed-shape in arguments.
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*),  INTENT(IN)    ::  CREQ     ! User's request string
INTEGER,       INTENT(IN)    ::  NFIRST   ! First character of CREQ to look at
CHARACTER(36), INTENT(IN)    ::  NAME(:)  ! Element name list from BUFR index
INTEGER,       INTENT(IN)    ::  NAMES    ! Number of element names in NAME array
INTEGER,       INTENT(OUT)   ::  LREQ(:)  ! Output array describing request list
INTEGER,       INTENT(INOUT) ::  LNUM     ! Counter for elements in LREQ array
INTEGER,       INTENT(OUT)   ::  ICODE    ! Integer return code

! Local declarations:

INTEGER, PARAMETER     ::  MAXLEV = 5 ! Max. nesting levels in request string. Limit set to 5

INTEGER      ::  I      ! Integer variable for local use
INTEGER      ::  ICH    ! Pointer to character in CREQ
INTEGER      ::  ICH1, ICH2 ! Start and end of substring in CREQ
INTEGER      ::  ICOUNT ! Replication count from request string
INTEGER      ::  ILEV   ! Nesting level in request string
INTEGER      ::  INAME  ! Position of element name in list
INTEGER      ::  ISTART(MAXLEV) ! LREQ subscript for replication count
INTEGER      ::  LENREQ ! Length of user's request string
INTEGER      ::  MAXREQ ! Size of LREQ (= input value of LNUM)

LOGICAL      ::  ENDLOOP ! Flag indicating last character was ')'
LOGICAL      ::  SEARCHING ! Flag controllong element name loop

CHARACTER     :: CH      ! Character extracted from CREQ
CHARACTER(36) :: CNAME   ! Element name from request string
CHARACTER(4)  :: FORMAT  ! Format used to read replication counts
CHARACTER(50) :: TEXT(8) ! Text of error messages

!-----------------------------------------------------------------------
!                                                Text of error messages

DATA TEXT /                                              & ! ICODE
   'OUTPUT ARRAY ''LREQ'' NOT BIG ENOUGH              ', & !  141
   'MISSING OPEN BRACKET IN REQUEST STRING            ', & !  142
   'REPLICATION INDICATOR (''*'') MISSING AFTER '')'' ', & !  143
   'MISSING REPLICATION COUNT AFTER ''*''             ', & !  144
   'UNCLOSED BRACKET AT END OF REQUEST STRING         ', & !  145
   'INTERNAL READ FAILURE DECODING REPLICATION COUNT  ', & !  146
   ' ', ' ' /                                              ! spare

!-----------------------------------------------------------------------
!     FIND END OF ELEMENTS LIST (IGNORING SPACES)
!-----------------------------------------------------------------------

LENREQ = LEN(CREQ)
DO WHILE (CREQ(LENREQ:LENREQ) == ' ')
  LENREQ = LENREQ - 1
END DO

!=======================================================================
!  DECODE REQUEST STRING, LOOK UP ELEMENT NAMES AND SET UP LREQ ARRAY
!=======================================================================
!                                                   Initialise counters
MAXREQ = LNUM     ! Input value = array size
LNUM = 0          ! LREQ subscript
ICH = NFIRST      ! CREQ character
ILEV = 0          ! Nesting level
ENDLOOP = .FALSE. ! ')' not found
ICODE = 0         ! No error yet
!                                           Loop through request string
DOLABEL1: &
DO WHILE (ICH <= LENREQ)
  CH = CREQ(ICH:ICH)

!-----------------------------------------------------------------------
!  (1)  OPEN BRACKET INDICATES THE START OF A REPLICATION
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (CH == '(') THEN
    ILEV = ILEV + 1        ! Increment nesting level
    LNUM = LNUM + 1        ! Reserve space for count
    ISTART(ILEV) = LNUM    ! Keep location of loop start
    ICH = ICH + 1          ! Go to next character

!-----------------------------------------------------------------------
!  (2)  CLOSE BRACKET INDICATES THE END OF A REPLICATION
!-----------------------------------------------------------------------

  ELSE IF (CH == ')') THEN
!                                       Error if no replication is open
    IF (ILEV <= 0) ICODE = 142
!                                          Skip spaces after ')' if any
    ICH = ICH + 1
    DO WHILE (ICH < LENREQ .AND. CREQ(ICH:ICH) == ' ')
      ICH = ICH + 1
    END DO
!                                    Error if next character is not '*'

    IF (CREQ(ICH:ICH) /= '*') ICODE = 143

!-----------------------------------------------------------------------
!  (3)  ASTERISK INDICATES REPLICATION COUNT IS NEXT.
!     (This can occur without the brackets if only one item is being
!    replicated. In this case set up LREQ as if brackets were present.)
!-----------------------------------------------------------------------

  ELSE IF (CH == '*') THEN
!                                          Replication without brackets
    IF (.NOT.ENDLOOP) THEN
      ILEV = ILEV + 1           ! Increment nesting level
      ISTART(ILEV) = LNUM       ! Keep location of loop start
      LREQ(LNUM+1) = LREQ(LNUM) ! Make space for count
      LNUM = LNUM + 1           ! Increment LREQ subscript
    END IF
!                                          Skip spaces after '*' if any
    ICH = ICH + 1
    DO WHILE (ICH < LENREQ .AND. CREQ(ICH:ICH) == ' ')
      ICH = ICH + 1
    END DO

!-----------------------------------------------------------------------
!  (3a)   Read and decode replication count
!-----------------------------------------------------------------------
!                                                      Check for digits
    ICH1 = ICH         ! Start of next field
    ICH2 = 0
    DO WHILE (ICH <= LENREQ .AND. CREQ(ICH:ICH) >= '0' .AND. &
              CREQ(ICH:ICH) <= '9')
      ICH2 = ICH       ! End of next field
      ICH = ICH + 1
    END DO
!                                         Error if no digits were found
    IF (ICH2 == 0) ICODE = 144
!                                          Decode the replication count
    I = ICH2 - ICH1 + 1
    WRITE (FORMAT,'(A,I1,A)') '(I', I, ')'
    READ (CREQ(ICH1:ICH2),FORMAT,IOSTAT=I) ICOUNT

!                                         Error if internal read failed
    IF (I /= 0) ICODE = 146

!-----------------------------------------------------------------------
!  (3b)   If count is 0, reset pointer LNUM so that loop gets ignored
!         If count is >0, store replication count and mark end of loop
!         An LREQ of -9999999 indicates the end of a replication
!-----------------------------------------------------------------------
!                                                 Replication count = 0
IFLABEL2: &
    IF (ICOUNT == 0) THEN
      LNUM = ISTART(ILEV) - 1
!                                                 Replication count > 0
    ELSE
      LREQ(ISTART(ILEV)) = -ICOUNT  ! Minus replication count
      IF (LNUM <= MAXREQ) THEN
        LNUM = LNUM + 1
        LREQ(LNUM) = -9999999       ! End of loop indicator          !5
      ELSE
        ICODE = 141                 ! LREQ too small
      END IF
    END IF IFLABEL2
!                                              Revert to previous level
    ILEV = ILEV - 1

!-----------------------------------------------------------------------
!  (4)  SKIP SPACE AND ANY OTHERS AFTER IT
!-----------------------------------------------------------------------

  ELSE IF (CH == ' ') THEN
    ICH = ICH + 1
    DO WHILE (CREQ(ICH:ICH) == ' ' .AND. ICH <= LENREQ)
      ICH = ICH + 1
    END DO

!-----------------------------------------------------------------------
!  (5)  EXTRACT ELEMENT NAME - NAME ENDS BEFORE SPACE, ')' OR '*'
!-----------------------------------------------------------------------

  ELSE
    ICH1 = ICH             ! First character of name
    ICH2 = 0
    DO WHILE (ICH2 == 0 .AND. ICH < LENREQ)
      ICH = ICH + 1
      CH = CREQ(ICH:ICH)
      IF (CH == ' ' .OR. CH == ')' .OR. CH == '*') ICH2 = ICH - 1
    END DO
    IF (ICH2 == 0) ICH2 = LENREQ
!                                      Put name in CNAME (CHARACTER*36)
    CNAME = CREQ(ICH1:ICH2)

!-----------------------------------------------------------------------
!  (5a)   Look for matching name in list from BUFR index
!-----------------------------------------------------------------------
!                                              Initialisations for loop
    SEARCHING = .TRUE.  ! No match yet
    INAME = 1           ! Start at first name

!                                         Loop over names in NAME array
    DO WHILE (SEARCHING)
      IF (CNAME == NAME(INAME)) THEN  ! Match found
        SEARCHING = .FALSE.           ! so stop looking
      ELSE
        INAME = INAME + 1             ! Try the next one ...
        IF (INAME > NAMES) THEN       ! ... if there are more
          INAME = 0                   ! Zero if no match
          SEARCHING = .FALSE.         ! To stop loop
        END IF
      END IF
    END DO
!                             Warning message if element name not found

    IF (INAME == 0) WRITE (6,'(T2,A,T12,2A)') 'ELEMENTS:', &
       'ELEMENT NAME NOT FOUND IN LIST - ', CNAME

!                                         Store subscript in LREQ array
    IF (LNUM < MAXREQ) THEN
      LNUM = LNUM + 1
      LREQ(LNUM) = INAME
    ELSE
        ICODE = 141          ! LREQ too small
    END IF
    ICH = ICH2 + 1
  END IF IFLABEL1
  ENDLOOP = CH == ')'      ! Flag for end of loop with brackets
END DO DOLABEL1
!                         Error code if unclosed bracket at end of CREQ

IF (ILEV /= 0) ICODE = 145

!=======================================================================
!     CHECK RETURN CODE, PRINT MESSAGE IF NON-ZERO AND RETURN
!=======================================================================

IF (ICODE /= 0) THEN
  I = ICODE - 140
  WRITE (6,'(/T5,A,T15,A)') 'ELEMENTS:', TEXT(I)
END IF

RETURN
END SUBROUTINE ELEMENTS
