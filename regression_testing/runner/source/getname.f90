      SUBROUTINE GETNAME (CREQ, NFIRST, REQNAME)

!-----------------------------------------------------------------------
! PROGRAM     : GETNAME
!
! PURPOSE     : GETNAME extracts the name of the next data element
!               requested as specified in a users' MDB request string.
!
! DESCRIPTION :
!
!   GETNAME is used by the retrieval job MDBRTVL to generate the names
!   of retrieved data items for printing out with the tabulated data.
!   It is used by integration test jobs but not by any operational
!   storage or retrieval programs.
!
!   GETNAME scans the list of retrieved elements in an MDB request
!   string CREQ, extracting the names of the data items and reducing
!   the list to an integer array. Then subsequent calls will return
!   the names of the items one a time in order in the 36-character
!   string REQNAME.
!
!   When appropriate, names returned in REQNAME will have suffixes
!   indicating replication information, e.g. LEVL_PESR*8 in a request
!   string will generate names LEVL_PESR/1 to LEVL_PESR/8.
!
!   NFIRST controls the operation of the routine as follows:
!
!     NFIRST>0:  NFIRST contains the location of the first character
!                in CREQ after the word ELEMENTS. CREQ is analysed and
!                the name of the first element is returned in REQNAME.
!
!     NFIRST=0:  The name of the next element is returned in REQNAME.
!                CREQ is not used.
!
!     NFIRST<0:  The name of the first element is returned in REQNAME.
!                CREQ is not used.
!
!   Typically, GETNAME will be called initially with NFIRST>0, then
!   subsequent calls with NFIRST=0 will generate successive data names.
!   (If calls with NFIRST=0 continue to be made after the last data
!   item has been reached, blanks are returned in REQNAME.) A call with
!   NFIRST<0 will restart from the first name. At any time a call with
!   NFIRST>0 can be used to process a new request string.
!
! USAGE       :  CALL GETNAME (CREQ, NFIRST, REQNAME)
!
! ARGUMENTS   :  Type  I/O
!                ----  ---
!    1  CREQ     C*(*)  I   User's MDB request string (not used unless
!                                NFIRST>0)
!    2  NFIRST   I*4    I   >1:  Location of CREQ byte after 'ELEMENTS'
!                           =0:  Flag to get name of next element
!                           <0:  Flag to get name of first element
!    3  REQNAME  C*36   O   Name of next retrieval element
!
! HISTORY     : Original version written by Brian Barwell, April 2010.
!
! REVISION INFO:
!
! $Workfile: getname.f$ $Folder: UTILITY.SRCE$
! $Revision: 1$ $Date: 21/04/2010 14:18:28$
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         21/04/2010 14:18:28    Brian Barwell
!       Routine to get element names from MDB request string.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER    MAXLEV      ! Max. nesting levels in request string
      PARAMETER (MAXLEV=4)
      INTEGER    MAXREQ      ! Size of LREQ and NAME arrays
      PARAMETER (MAXREQ=300)

      INTEGER I              ! Integer variable for local use
      INTEGER ICOUNT         ! Replication count from request string
      INTEGER ILEV           ! Nesting level in request string
      INTEGER ICH            ! Pointer to character in CREQ
      INTEGER ICH1, ICH2     ! Start and end of substring in CREQ
      INTEGER INAME          ! Position of element name in list
      INTEGER IREP(MAXLEV)   ! 'Instance' values for user's element
      INTEGER IREQ           ! Counter for element of LREQ array
      INTEGER ISTART(MAXLEV) ! LREQ subscript for replication count
      INTEGER LCOUNT(MAXLEV) ! Replication counts in LREQ array
      INTEGER LENREQ         ! Length of user's request string
      INTEGER LNUM           ! Number of integers used in LREQ
      INTEGER LREQ(MAXREQ)   ! Array describing request string
      INTEGER LSTART(MAXLEV) ! Start of replications in LREQ array
      INTEGER NFIRST         ! First character of CREQ to look at

      LOGICAL ENDLOOP        ! Flag indicating last character was ')'
      LOGICAL SEARCH         ! Flag controlling loop over LREQ elements

      CHARACTER     CH           ! Character extracted from CREQ
      CHARACTER*(*) CREQ         ! User's request string
      CHARACTER*4   FORMAT       ! Format for reading replication counts
      CHARACTER*36  INSTANCE     ! Replication counts for next element
      CHARACTER*36  NAME(MAXREQ) ! Element names from request string
      CHARACTER*36  REQNAME      ! Name of next element

      DATA LNUM/0/, IREQ/1/, ILEV/0/

      COMMON /COMREQ/ NAME, LREQ   ! For dynamic allocation only

      SAVE LNUM, IREQ, ILEV, IREP, LCOUNT, LSTART

!-----------------------------------------------------------------------
!     NFIRST>0: DECODE ELEMENTS FROM MDB REQUEST STRING.
!-----------------------------------------------------------------------

      IF (NFIRST.GT.0) THEN
!                                 Find last non-blank character in CREQ
        LENREQ = LEN(CREQ)
        DO WHILE (CREQ(LENREQ:LENREQ).EQ.' ')
          LENREQ = LENREQ - 1
        END DO
!                                                       Initialisations
        LNUM = 0           ! No LREQ values used
        ICH = NFIRST       ! Start byte in CREQ
        ILEV = 0           ! No nesting yet
        INAME = 0          ! No elements decoded
        ENDLOOP = .FALSE.
!                                       Loop through characters in CREQ
        DO WHILE (ICH.LE.LENREQ)
          CH = CREQ(ICH:ICH)
!                                    '(' indicates start of replication
          IF (CH.EQ.'(') THEN
            ILEV = ILEV + 1      ! Increment nesting level
            LNUM = LNUM + 1      ! Move to next LREQ element
            ISTART(ILEV) = LNUM  ! Store loop start location
            ICH = ICH + 1
!                                      ')' indicates end of replication
          ELSE IF (CH.EQ.')') THEN
!                                                           Skip spaces
            ICH = ICH + 1
            DO WHILE (ICH.LT.LENREQ .AND. CREQ(ICH:ICH).EQ.' ')
              ICH = ICH + 1
            END DO
!                                            Start of replication count
          ELSE IF (CH.EQ.'*') THEN
!                                          Replication without brackets
            IF (.NOT.ENDLOOP) THEN
              ILEV = ILEV + 1           ! Increment nesting level
              ISTART(ILEV) = LNUM       ! Store loop start location
              LREQ(LNUM+1) = LREQ(LNUM) ! Shift element name 1 place
              LNUM = LNUM + 1
            END IF
!                                               Stop if too many levels
            IF (ILEV.GT.MAXLEV) THEN
              WRITE (6,'(T5,A,I5)') &
               'GETNAME:  TOO MANY NESTING LEVELS.  LIMIT IS', MAXLEV
              STOP
            ENDIF
!                                                    Skip spaces if any
            ICH = ICH + 1
            DO WHILE (ICH.LT.LENREQ .AND. CREQ(ICH:ICH).EQ.' ')
              ICH = ICH + 1
            END DO
!                                                      Check for digits
            ICH1 = ICH
            ICH2 = 0
            DO WHILE (ICH.LE.LENREQ .AND. CREQ(ICH:ICH).GE.'0' .AND. &
                     CREQ(ICH:ICH).LE.'9')
              ICH2 = ICH
              ICH = ICH + 1
            END DO
!                                              Decode replication count
            I = ICH2 - ICH1 + 1
            WRITE (FORMAT,'(A,I1,A)') '(I', I, ')'
            READ (CREQ(ICH1:ICH2),FORMAT,IOSTAT=I) ICOUNT
!                                                 Replication count = 0
            IF (ICOUNT.EQ.0) THEN
              LNUM = ISTART(ILEV) - 1
!                                                 Replication count = 1
            ELSE IF (ICOUNT.EQ.1) THEN
              DO I=ISTART(ILEV),LNUM-1
                LREQ(I) = LREQ(I+1)
              END DO
              LNUM = LNUM - 1
!                                                 Replication count > 1
            ELSE
              LREQ(ISTART(ILEV)) = -ICOUNT ! Replication count
              LNUM = LNUM + 1              ! Move to next LREQ element
              LREQ(LNUM) = -1              ! End of replication marker
            END IF
            ILEV = ILEV - 1
!                                                 Skip string of spaces
          ELSE IF (CH.EQ.' ') THEN
            ICH = ICH + 1
            DO WHILE (CREQ(ICH:ICH).EQ.' ' .AND. ICH.LE.LENREQ)
              ICH = ICH + 1
            END DO
!                                                 Start of element name
          ELSE
            ICH1 = ICH
            ICH2 = 0
            DO WHILE (ICH2.EQ.0 .AND. ICH.LT.LENREQ)
              ICH = ICH + 1
              CH = CREQ(ICH:ICH)
              IF (CH.EQ.' ' .OR. CH.EQ.'*' .OR. CH.EQ.')') ICH2 = ICH-1
            END DO
            IF (ICH2.EQ.0) ICH2 = LENREQ
!                                              Store name in NAME array
            INAME = INAME + 1
            NAME(INAME) = CREQ(ICH1:ICH2)
!                                                Stop if too many names
            IF (LNUM.GE.MAXREQ) THEN
              WRITE (6,'(T5,A,I5)') &
               'GETNAME:  TOO MANY ELEMENT NAMES.  LIMIT IS', MAXREQ
              STOP
            ENDIF
!                                           Put subscript in LREQ array
            LNUM = LNUM + 1
            LREQ(LNUM) = INAME
            ICH = ICH2 + 1
          END IF
          ENDLOOP = CH.EQ.')'
        END DO
      END IF
!                                                       Initialisations
      IF (NFIRST.NE.0) THEN
        IREQ = 1             ! Start at first element of LREQ
        ILEV = 0             ! No nesting yet
      END IF

!-----------------------------------------------------------------------
!     FIND THE NAME OF THE NEXT ELEMENT (INCLUDING REPLICATION SUFFIX)
!-----------------------------------------------------------------------

      SEARCH = .TRUE.
      DO WHILE (SEARCH)
!                                Return spaces if off end of LREQ array
        IF (IREQ.GT.LNUM) THEN
          REQNAME = ' '
          SEARCH = .FALSE.
!                                        Start-of-replication indicator
        ELSE IF (LREQ(IREQ).LT.-1) THEN
          ILEV = ILEV + 1              ! Increment nesting level
          LCOUNT(ILEV) = -LREQ(IREQ)   ! Get replication count
          IREP(ILEV) = 1               ! Initialise loop count
          IREQ = IREQ + 1
          LSTART(ILEV) = IREQ
!                                          End-of-replication indicator
        ELSE IF (LREQ(IREQ).LT.0) THEN
          IF (IREP(ILEV).LT.LCOUNT(ILEV)) THEN
            IREP(ILEV) = IREP(ILEV) + 1  ! Increment loop counter
            IREQ = LSTART(ILEV)          ! Return to start of loop
          ELSE
            ILEV = ILEV - 1              ! Decrement nesting level
            IREQ = IREQ + 1              ! Move to next element
          END IF
!                                                          Next element
        ELSE
          REQNAME = NAME(LREQ(IREQ))     ! Get element name
          SEARCH = .FALSE.               ! Exit DO WHILE loop
          IREQ = IREQ + 1
        END IF
      END DO

!-----------------------------------------------------------------------
!     MAKE FULLY QUALIFIED NAME INCLUDING 'INSTANCE' INFORMATION
!-----------------------------------------------------------------------
!                           Get 'instance' information from loop counts
      IF (ILEV.GT.0) THEN
        WRITE (INSTANCE,"(5('/',I6))") (IREP(I),I=1,ILEV)
        ICH = 1
!                                                         Remove spaces
        DO I=2,7*ILEV
           IF (INSTANCE(I:I).NE.' ') THEN
             ICH = ICH + 1
             INSTANCE(ICH:ICH) = INSTANCE(I:I)
           END IF
        END DO
!                                                Append to element name
        I = INDEX(REQNAME,' ') - 1
        REQNAME = REQNAME(1:I) // INSTANCE(1:ICH)
      END IF

      RETURN
      END
