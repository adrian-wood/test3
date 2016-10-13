PROGRAM MDBRTVL

!-----------------------------------------------------------------------
! PROGRAM       : MDBRTVL
!
! PURPOSE       : To retrieve data from the MetDB and print out data
!                 values in tabulated form.
!
! DESCRIPTION
! -----------
!             MDBRTVL prints a table of data retrieved from the MetDB
! with rows representing data elements and columns observations or vice
! versa. It is used for data output by retrieval jobs in integration
! tests. Data input is on unit 5 in card image form and takes the form:
!
!    - NAMELIST data controlling output table format,
!    - At least one blank line
!    - Data type and number of retrieved elements,     )
!    - List of keyword data for MetDB request string,  ) 1st retrieval
!    - List of element names for MetDB request string, )
!    - At least one blank line
!    - Data type and number of retrieved elements,     )
!    - List of keyword data for MetDB request string,  ) 2nd retrieval
!    - List of element names for MetDB request string, )
!    - At least one blank line
!                          :
!                         etc.
!                          :
!    - Data type and number of retrieved elements,     )
!    - List of keyword data for MetDB request string,  ) last retrieval
!    - List of element names for MetDB request string, )
!
!  The NAMELIST is called 'INPUT' and contains the following variables:
!
!     LAYOUT  This should be 1, 2 or 3. If 1, the rows of the printed
!             table will be data elements and the columns will be
!             observations; if 2, they will be the other way round.
!             If 3, raw report text is printed; no elements.
!     MAXOBS  Maximum number of observations to retrieve.
!     NPRINT  Maximum number of observations to print out.
!     NCOLS   Number of columns in table.
!     KOBS    NOBS dimension of retrieval array
!
! The details specified will apply to all the data printed by the job:
! it is not possible to re-pecify details between retrievals.
!
! If MAXOBS > NPRINT, the observations printed will be the first of
! those retrieved. If LAYOUT=1 the element name will be printed on each
! row after the data.
!
! A value of 6 for NCOLS will ensure that the printed table will fit
! on a 72-character screen. In any case NCOLS should not exceed 8 for
! LAYOUT=1 or 10 for LAYOUT=2.
!
! If values are not specified in the NAMELIST, the default values are:
!
!      &INPUT NPRINT=18, MAXOBS=250000, LAYOUT=1, NCOLS=6 /
!
!  The data type must be left-justified in the first 8 characters of
! the record and the number of elements must not start before column 9.
!
!  The keyword parameters ('START TIME' etc) can occupy more than one
! line but each line must start in column 1. Note that DDICT can be
! specified for the first retrieval but cannot be changed subsequently
! between retrievals.
!
!  The list of element names can occupy several lines each of which
! must be indented by at least one character. It is not necessary to
! code the keyword 'ELEMENTS': this will be automatically added by the
! program.
!
!  Current program defaults are array sizes of 100000 for returned
! data values from a call to MDB and 34 characters per observation for
! returned character data. Maximum request string size is 3034 (enough
! for 2 lines of keyword data and 40 lines of element names).
!
! REVISION INFO :
!
! $Workfile: mdbrtvl.f90$ $Folder: source$
! $Revision: 5$ $Date: 25/07/2013 14:57:52$
!
! CHANGE RECORD :
!
! $Log:
!  5    MOODS      1.4         25/07/2013 14:57:52    Darren Parfitt  Add extra
!        input forms. Add a LAYOUT for raw text reports. Allow character data
!       in LAYOUT=2
!  4    MOODS      1.3         18/03/2013 11:46:52    Alison Weir     Avoid
!       integer overflow
!  3    MOODS      1.2         20/09/2012 12:44:30    Alison Weir     Port to
!       f90
!  2    MOODS      1.1         20/09/2012 12:42:00    Alison Weir     Rename
!       f90
!  1    MOODS      1.0         20/09/2012 12:38:40    Alison Weir     Initial
!       f77 version, from MetDB project (UTILITY.SRCE) revision 5.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
IMPLICIT NONE
!                                                            Parameters

INTEGER,  PARAMETER ::   NUMOBS =10000  ! No. of Obs max
INTEGER,  PARAMETER ::   STRLEN = 900  ! raw text string lengths  
INTEGER,  PARAMETER ::   NUMVALS=150   ! No. of elements

!                                                             Variables
INTEGER ::  DVOL            ! Data volume in bytes
INTEGER ::  I               ! Dogsbody variable
INTEGER ::  ILEN            ! Length of character element
INTEGER ::  IOBS            ! Number of obs returned from MDB
INTEGER ::  IPRINT          ! Number of obs to print from group
INTEGER ::  ISTAT           ! Return status for call to MDB
INTEGER ::  ISTATA          ! Return status for argument calls
INTEGER ::  ISTAT5          ! Return status for I/O on unit 5
INTEGER ::  ITEM            ! Loop variable for retrieved elements
INTEGER ::  ITEMS           ! Number of retrieved elements requested
INTEGER ::  IVAL            ! Element of RARRAY converted to integer
INTEGER ::  J               ! General loop counter
INTEGER ::  JOB             ! Loop counter fop loop over observations
INTEGER ::  J1, J2          ! Limits of RARRAY elements for printing
INTEGER ::  KOUNTOBS        ! Count of obs in current request
INTEGER ::  LN              ! Length of argument
INTEGER ::  NEXT = 1        ! Start of element names in WANTED
INTEGER ::  NFIRST, NLAST   ! Headings for first & last columns
INTEGER ::  NOBS = 0        ! Maximum number of obs per call to MDB
INTEGER ::  NSTART          ! Second argument for call to GETNAME
INTEGER ::  OFFSET

REAL,ALLOCATABLE ::  RARRAY(:,:)    ! Array for data values from MDB

LOGICAL ::  AGAIN           ! Flag for new retrieval request
LOGICAL ::  CHARS           ! Flag for character retrieval element
LOGICAL ::  MORE            ! Flag for more data for this request
LOGICAL ::  MDBCALLED       ! True when MDB called so it can be killed 
LOGICAL ::  MORE_REQUESTS   ! True for more requests to come
LOGICAL ::  LEXISTS

CHARACTER(LEN=200)  ::  FILE_IN    ! File containing input
CHARACTER(LEN=200)  ::  FILE_OUT   ! File for output
CHARACTER(LEN=128)  ::  AROW, EOL
CHARACTER(LEN=60),    ALLOCATABLE :: CSTR(:) ! String array for MDB
CHARACTER(LEN=STRLEN),ALLOCATABLE :: CREP(:) ! Reports array for MDB
CHARACTER(LEN=8)    ::  DATYPE     ! MetDB data type
CHARACTER(LEN=30)   ::  FORMATC    ! Format for printing characters
CHARACTER(LEN=30)   ::  FORMATI    ! Format for printing integers
CHARACTER(LEN=36)   ::  NAME       ! Element name
CHARACTER(LEN=72)   ::  RECORD     ! Record read from unit 5
CHARACTER(LEN=72)   ::  BLANK=' '  ! Blank record to check for elements
CHARACTER(LEN=3034) ::  WANTED     ! Request string (3034=2*73+8+40*72)

!-----------------------------------------------------------------------
!     READ NAMELIST FOR NUMBER OF OBSERVATIONS WANTED AND PRINT LAYOUT
!-----------------------------------------------------------------------

INTEGER  ::  NPRINT=18
INTEGER  ::  MAXOBS=250000   ! to retrieve in total
INTEGER  ::  LAYOUT=1
INTEGER  ::  NCOLS=6
INTEGER  ::  KOBS=-1
NAMELIST /INPUT/ NPRINT, MAXOBS, LAYOUT, NCOLS, KOBS

MDBCALLED=.FALSE.
FILE_IN=' '
FILE_OUT=' '
CALL GET_COMMAND_ARGUMENT(1, FILE_IN, LN, ISTATA)
IF (ISTATA /= 0 .OR. FILE_IN == ' ') THEN
  PRINT *, 'INPUT FILE NAME NOT SET - LOOKING FOR DEFAULT FILENAME "INPUT"'
  INQUIRE(FILE='INPUT',EXIST=LEXISTS)
  IF (LEXISTS) THEN
    FILE_IN = 'INPUT'
  ELSE
    PRINT *, 'UNABLE TO FIND A FILE CALLED "INPUT" EITHER. EXITING.'
    STOP 8
  END IF
END IF

!                                                       Open data input
OPEN (5, FILE=FILE_IN, IOSTAT=ISTAT5)
IF (ISTAT5 /= 0) THEN
  WRITE(6,'(/T2,A,I5)') 'PROBLEM OPENING INPUT: "ISTAT5"=', ISTAT5
  STOP 8
END IF

CALL GET_COMMAND_ARGUMENT(2, FILE_OUT, LN, ISTATA)
IF (ISTATA /= 0 .OR. FILE_OUT == ' ') THEN
  PRINT *, 'OUTPUT FILE NAME NOT SET - USING STDOUT STREAM'
  FILE_OUT = ' '
ELSE
!                                                       Open data output
  OPEN (6, FILE=FILE_OUT, IOSTAT=ISTAT5)
  IF (ISTAT5 /= 0) THEN
    PRINT *, 'PROBLEM OPENING OUTPUT: "ISTAT5"=', ISTAT5
    STOP 8
  END IF
END IF

!-----------------------------------------------------------------------
!     READ DETAILS OF NEW RETRIEVAL REQUEST FROM INPUT ON UNIT 5
!-----------------------------------------------------------------------
MORE_REQUESTS=.TRUE.
WHILE_MORE_REQUESTS: &
DO WHILE (MORE_REQUESTS)
  WANTED = ' '
!                                         Skip any blank lines at start
  READ (5,'(A)',IOSTAT=ISTAT5) RECORD
  IF (ISTAT5 == 0 .AND. RECORD == BLANK) CYCLE WHILE_MORE_REQUESTS
!                                                    Read NAMELIST data
!  READ (RECORD, INPUT, IOSTAT=ISTAT5)
!         Internal namelist read is an extension, so this gets round it
IF_INPUT: &
  IF (RECORD(1:7) == ' &INPUT') THEN
    OPEN(10,STATUS='SCRATCH',FORM='FORMATTED')
    WRITE(10,'(A)')RECORD
    REWIND(10)
    READ(10,NML=INPUT,IOSTAT=ISTAT5)
    CLOSE(10)
!                                                       Stop if problem
    IF (ISTAT5 > 0) THEN
      WRITE (6,'(/T2,A,I5)') 'NAMELIST PROBLEM: "ISTAT5"=', ISTAT5
      STOP 8
    END IF
!                                              Check number  of columns
    IF (LAYOUT == 1) THEN
      NCOLS = MIN0(NCOLS,8)
    ELSE IF (LAYOUT == 2) THEN
      NCOLS = MIN0(NCOLS,10)
    END IF
    CYCLE WHILE_MORE_REQUESTS
  END IF  IF_INPUT
!                                          Set flag if more input found
  AGAIN = ISTAT5 == 0
!                                           Read next retrieval request
IF_AGAIN: &
  IF (AGAIN) THEN
!                          Read data type and number of retrieved items
    READ (RECORD,'(A8)') DATYPE
    READ (RECORD(9:),'(I4)') ITEMS
    WRITE (6,'(//T2,3A,I6/)')  &
            'DATA TYPE ', DATYPE, ',  ITEMS =', ITEMS
    IF (LAYOUT == 3 .AND. ITEMS > 1) THEN
      WRITE(6,*)'INVALID FORMAT FOR MULTIPLE ITEMS'
      STOP 8
    END IF  	  
    READ (5,'(A)') RECORD
!                                                  Look for pre-ELEMENT
!                                                part of request string
    IF_NOTBLANK: &
    IF (RECORD(1:1) /= ' ') THEN
!                                              Add to WANTED if present
      J = 1
      DO WHILE (ISTAT5 == 0 .AND. RECORD(1:1) /= ' ')
        WANTED(J:J+72) = RECORD
        WRITE (6,'(T2,A)') WANTED(J:J+72)
        J = J + 73
        READ (5,'(A)',IOSTAT=ISTAT5) RECORD
      END DO
!                                      Put 'ELEMENTS' in request string
      IF (ISTAT5 == 0) THEN
        IF (RECORD /= BLANK ) THEN
          WANTED(J:J+7) = 'ELEMENTS'
          WRITE (6,'(T2,A)') WANTED(J:J+7)
          NEXT = J + 8
          NSTART = NEXT
        ELSE
          NSTART = 1
        END IF
      ELSE
        RECORD = BLANK
        NSTART = 1
      END IF
    END IF   IF_NOTBLANK
    MORE = ISTAT5 == 0
!                                            Read list of element names
    J = NEXT
    DO WHILE (ISTAT5 == 0 .AND. RECORD /= BLANK)
      WANTED(J:J+71) = RECORD
      WRITE (6,'(T2,A)') WANTED(J:J+71)
      READ (5,'(A)',IOSTAT=ISTAT5) RECORD
      J = J + 72
    END DO
    AGAIN = ISTAT5 == 0

    IF (KOBS /= -1 ) THEN
      NOBS = MIN(KOBS, MAXOBS)
    ELSE
      NOBS = MIN0(MAXOBS,NUMOBS)
    END IF
!
    IF(ALLOCATED(RARRAY))DEALLOCATE(RARRAY)
    ALLOCATE(RARRAY(NOBS,ITEMS))
  
    IF(ALLOCATED(CSTR))DEALLOCATE(CSTR)
    ALLOCATE(CSTR(NOBS))
  
    IF(ALLOCATED(CREP))DEALLOCATE(CREP)
    ALLOCATE(CREP(NOBS))

!                                          Stop job if no more requests
  ELSE
    MORE = .FALSE.
  END IF  IF_AGAIN

!-----------------------------------------------------------------------
!     LOOP OVER CALLS TO 'MDB' UNTIL RETRIEVAL REQUEST IS COMPLETE
!-----------------------------------------------------------------------

!                                           Initialisations for do loop
  KOUNTOBS = 0  ! No obs found yet
  ISTAT = 0     ! New request
!                                              Loop over calls to 'mdb'
DO_MORE: &
  DO WHILE (MORE .AND. KOUNTOBS < MAXOBS)
    IOBS = NOBS
    MDBCALLED=.TRUE.
    CALL MDB (DATYPE, WANTED, RARRAY, IOBS, ITEMS, ISTAT, CSTR, CREP)

!                                                     Check status word
    IF (ISTAT == 16) THEN
      WRITE (6,'(/T9,A/)') 'MDB FAILURE - SKIPPING REQUEST'
      IF (AGAIN ) CYCLE WHILE_MORE_REQUESTS
    
    ELSE IF (ISTAT == 99 )THEN
      WRITE (6,'(/T9,A/)') 'RPC FAILURE - ABORT'
      EXIT WHILE_MORE_REQUESTS
!                                           Check for more data to come
    ELSE IF (ISTAT /= 4) THEN
      MORE = .FALSE.
      IF (ISTAT == 8) WRITE(6,'(/T9,A)')  &
         'NO MDB DATA AVAILABLE FOR REQUESTED DATE'
    END IF
!                                                    Print out the data
IF_PRINT: &
    IF (KOUNTOBS < NPRINT) THEN ! more to print

!-----------------------------------------------------------------------
!     LAYOUT 1:  EACH COLUMN CONTAINS DATA FOR ONE OBSERVATION
!-----------------------------------------------------------------------

IF_LAYOUT: &
      IF (LAYOUT == 1) THEN
!                                                  Get number of obs to
!                                                 print from this batch
        IPRINT = MIN0(IOBS,NPRINT-KOUNTOBS)
!                                                        Set up formats
        I = 12*MIN0(IPRINT,NCOLS) + 12
        WRITE (FORMATC,'(A,I3.3,A)')   &
             '(T', I, ',A,T2,I5,2X,10(2X,A10))'
        WRITE (FORMATI,'(A,I3.3,A)')  &
             '(T', I, ',A,T2,I5,2X,1P,10E12.4)'

!                                     Loop over obs. in groups of NCOLS
DO_JOB: &
        DO JOB=1,IPRINT,NCOLS

!                    Get range of RARRAY subscripts for first parameter

          J1 = JOB                ! 1st parameter, first ob
          J2 = J1 + NCOLS - 1     ! 1st parameter, last ob
          J2 = MIN0(J2,IPRINT)    ! 1st parameter, last ob to print

!                                                 Print column headings
          NFIRST = J1 + KOUNTOBS
          NLAST  = J2 + KOUNTOBS
          WRITE (6,'(/T5,10I12)') (J,J=NFIRST,NLAST)

!                                        Loop over elements (1 per row)
DO_ITEM1: &
          DO ITEM=1,ITEMS
!                                              Check for character data
!                                    (All OBS for this ITEM need to 'pass'
!                                     the character test before the ITEM
!                                     is deemed to be character output)
!                   
            CHARS = RARRAY(J1,ITEM) >= 262144.0
            J = J1
            DO WHILE (CHARS .AND. J <= J2)
              IVAL = NINT(RARRAY(J,ITEM))
              CHARS = (RARRAY(J,ITEM) <= 2.14784E9) .AND.  &
                      (IVAL-65535*(IVAL/65536)-1 <= LEN(CSTR(J)))
              J = J + 1
            END DO
!                                                      Get element name
            CALL GETNAME (WANTED, NSTART, NAME)
            NSTART = 0
!                                                    Print row of table
!                                                    (1) Character data
            IF (CHARS) THEN
              ILEN = NINT(RARRAY(J1,ITEM))/65536
              WRITE (6,FORMATC) NAME, ITEM, (CSTR(J)  &
                 (MOD(NINT(RARRAY(J,ITEM)),65536):                &
                 (MOD(NINT(RARRAY(J,ITEM)),65536)+ILEN-1)), J=J1,J2)
            ELSE
!                                                       (2) Real values
              WRITE (6,FORMATI) NAME, ITEM, (RARRAY(J,ITEM),J=J1,J2)
            END IF
!                                  Update subscripts for next parameter
        
          END DO  DO_ITEM1
          NSTART = -1    ! so that GETNAME gets next name
        END DO  DO_JOB

!-----------------------------------------------------------------------
!        LAYOUT 2:  EACH COLUMN CONTAINS DATA FOR ONE PARAMETER
!-----------------------------------------------------------------------

      ELSE IF (LAYOUT == 2) THEN  IF_LAYOUT  
  
        WRITE (FORMATC,'(A)') '(A,2X,A10,A)'
!                                                  Loop over parameters
!                                                    in groups of NCOLS
DO_ITEM2: &
        DO ITEM=1,ITEMS,NCOLS
!                                                 Print column headings
          NLAST = MIN0(ITEM+NCOLS-1,ITEMS)
          WRITE (6,'(/T4,10I12)') (J,J=ITEM,NLAST)

!                  Get range of RARRAY subscripts for first observation

          J1 = ITEM   ! 1ST OB, FIRST PARAMETER
          J2 = NLAST  ! 1ST OB, LAST PARAMETER

!                                           Get number of obs. to print
          NLAST = NPRINT - KOUNTOBS
          NLAST = MIN0(NLAST,IOBS)
!                                            Loop over obs. (1 per row)
          DO JOB=1,NLAST
!
!                      WRITE NUMERIC DATA TO A INITIAL DUMMY ROW STRING
            WRITE (AROW,'(T2,I5,2X,1P,10E12.4)')   &
                       JOB, (RARRAY(JOB,J),J=J1,J2)

!                      Now loop looking for any character ITEMS and
!                      replace the necessary 'fields' of the row
!                      with the character data
            DO J=J1,J2
              CHARS = RARRAY(JOB,J) >= 262144.0
              IF (CHARS) THEN
                IVAL = NINT(RARRAY(JOB,J))
                CHARS = (RARRAY(JOB,J) <= 2.14784E9) .AND. &
                        (IVAL-65535*(IVAL/65536)-1 <= LEN(CSTR(JOB)))
              
                IF (CHARS) THEN
                  ILEN = IVAL/65536
                  OFFSET = 8 + (J-1)*12
                  EOL = ''
                  IF (J < J2) THEN
                    EOL = AROW(OFFSET+13:)
                  END IF 
                  WRITE (AROW,FORMATC) AROW(1:OFFSET),   &
                    CSTR(JOB)(MOD(IVAL,65536):(MOD(IVAL,65536)+ILEN-1)), &
                    EOL
                END IF
              END IF
            END DO
!                                                    Print row of table
            WRITE (6,'(A)') AROW

!                                Update subscripts for next observation         
         
          END DO ! JOB
        END DO  DO_ITEM2
    
!-----------------------------------------------------------------------
!        LAYOUT 3:  PRINT REPORT TEXT ONLY
!-----------------------------------------------------------------------

      ELSE IF (LAYOUT == 3) THEN  IF_LAYOUT
!
        DO JOB=1,MIN0(IOBS,NPRINT-KOUNTOBS)
        
          IVAL = NINT(RARRAY(JOB,1))
          WRITE (6,'(T2,2A,I4,A,I7,A)') &
            DATYPE, '  REPORT NO.', KOUNTOBS+JOB, ':', IVAL, ' BYTES'
          WRITE (6,'(T5,A)') CREP(JOB)(1:44)
          DO J1=45,IVAL,75
            J2 = J1 + 74
            J2 = MIN0(J2,IVAL)
            WRITE (6,'(T5,A)') CREP(JOB)(J1:J2)
          END DO

        END DO

      END IF  IF_LAYOUT    
    END IF  IF_PRINT
!                                              Update observation count
    KOUNTOBS = KOUNTOBS + IOBS
  END DO  DO_MORE
!                                         Print number of obs retrieved 
  IF (KOUNTOBS > 0) THEN
    WRITE (6,'(/T2,I8,A)') KOUNTOBS, ' OBSERVATIONS RETRIEVED'
    DVOL = KOUNTOBS*ITEMS*4
    IF (LAYOUT /= 3) WRITE(6,'(/T2,A,I20)') &
                    ' Total Bytes transferred (real array) ',DVOL
  END IF

!                                       Go back to look for new request
  IF (.NOT. AGAIN) EXIT WHILE_MORE_REQUESTS

END DO WHILE_MORE_REQUESTS

IF (MDBCALLED) THEN
  WRITE(6,*)'Killing server'
  ISTAT=99
  CALL MDB (DATYPE, WANTED, RARRAY, IOBS, ITEMS,ISTAT, CSTR, CREP)
END IF

CLOSE(5)
IF ( FILE_OUT /= ' ') CLOSE(6)
      
STOP
END PROGRAM MDBRTVL
