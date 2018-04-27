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
!     LAYOUT  This should be 1 or 2. If 1, the rows of the printed
!             table will be data elements and the columns will be
!             observations; if 2, they will be the other way round.
!     MAXOBS  Maximum number of observations to retrieve.
!     NPRINT  Maximum number of observations to print out.
!     NCOLS   Number of columns in table.
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
!      &INPUT NPRINT=18, MAXOBS=250000, LAYOUT=1, NCOLS=6 &END
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
! $Workfile: mdbrtvl.f$ $Folder: UTILITY.SRCE$
! $Revision: 5$ $Date: 21/04/2010 14:17:21$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         21/04/2010 14:17:21    Brian Barwell   Get
!       element names from calls to GETNAME rather than from USRELM array.
!  4    Met_DB_Project 1.3         07/04/2009 16:00:48    Richard Weedon
!       References to SELECT on the MDBDC1 common block
!  3    Met_DB_Project 1.2         03/04/2009 13:23:31    Richard Weedon
!       Updated for SELECT element 50
!  2    Met_DB_Project 1.1         09/10/2008 12:59:59    Sheila Needham
!       Increase character dimension from 34 to 90
!  1    Met_DB_Project 1.0         08/06/2007 15:21:25    Brian Barwell   
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

      IMPLICIT NONE
!                                                            Parameters

      INTEGER    NUMOBS          ! Size of CSTR and CREP arrays
      INTEGER    NUMVALS         ! Size of RARRAY array
      PARAMETER (NUMOBS=10000)
      PARAMETER (NUMVALS=100000)
!                                                             Variables
      INTEGER I               ! Dogsbody variable
      INTEGER ILEN            ! Length of character element
      INTEGER IOBS            ! Number of obs returned from MDB
      INTEGER IPRINT          ! Number of obs to print from group
      INTEGER ISTAT           ! Return status for call to MDB
      INTEGER ISTAT5          ! Return status for I/O on unit 5
      INTEGER ITEM            ! Loop variable for retrieved elements
      INTEGER ITEMS           ! Number of retrieved elements requested
      INTEGER IVAL            ! Element of RARRAY converted to integer
      INTEGER J               ! General loop counter
      INTEGER JOB             ! Loop counter fop loop over observations
      INTEGER J1, J2          ! Limits of RARRAY elements for printing
      INTEGER KOUNTOBS        ! Count of obs in current request
      INTEGER NEXT            ! Start of element names in WANTED
      INTEGER NFIRST, NLAST   ! Headings for first & last columns
      INTEGER NOBS            ! Maximum number of obs per call to MDB
      INTEGER NSTART          ! Second argument for call to GETNAME  !5

      REAL RARRAY(NUMVALS)    ! Array for data values from MDB

      LOGICAL AGAIN           ! Flag for new retrieval request
      LOGICAL CHARS           ! Flag for character retrieval element
      LOGICAL MORE            ! Flag for more data for this request

      CHARACTER*90 CSTR(NUMOBS) ! CSTR array for MDB                 !2
      CHARACTER    CREP(NUMOBS) ! CREP array for MDB
      CHARACTER*8    DATYPE     ! MetDB data type
      CHARACTER*30   FORMATC    ! Format for printing characters
      CHARACTER*30   FORMATI    ! Format for printing integers
      CHARACTER*36   NAME       ! Element name                       !5
      CHARACTER*72   RECORD     ! Record read from unit 5
      CHARACTER*3034 WANTED     ! Request string (3034 = 2*73+8+40*72)

      DATA NEXT/1/, NOBS/0/

!-----------------------------------------------------------------------
!     COMMON BLOCK FOR DYNAMIC ALLOCATION OF LARGE ARRAYS AND STRINGS
!-----------------------------------------------------------------------

      COMMON /COMRET/ RARRAY, CSTR, CREP, WANTED

!-----------------------------------------------------------------------
!     READ NAMELIST FOR NUMBER OF OBSERVATIONS WANTED AND PRINT LAYOUT
!-----------------------------------------------------------------------

      INTEGER          NPRINT, MAXOBS, LAYOUT, NCOLS
      NAMELIST /INPUT/ NPRINT, MAXOBS, LAYOUT, NCOLS
      DATA             NPRINT, MAXOBS, LAYOUT, NCOLS
     &                /    18, 250000,      1,    6/
!                                                       Open data input
      OPEN (5, IOSTAT=ISTAT5)

!-----------------------------------------------------------------------
!     READ DETAILS OF NEW RETRIEVAL REQUEST FROM INPUT ON UNIT 5
!-----------------------------------------------------------------------

    1 WANTED = ' '
!                                         Skip any blank lines at start
      READ (5,'(A)',IOSTAT=ISTAT5) RECORD
      IF (ISTAT5.EQ.0 .AND. RECORD.EQ.' ') GO TO 1
!                                                    Read NAMELIST data
      IF (RECORD(1:7).EQ.' &INPUT') THEN
        READ (RECORD, INPUT, IOSTAT=ISTAT5)
!                                                       Stop if problem
        IF (ISTAT5.GT.0) THEN
          WRITE (6,'(/T2,A,I5)') 'NAMELIST PROBLEM: "ISTAT5"=', ISTAT5
          STOP
        END IF
!                                              Check number  of columns
        IF (LAYOUT.EQ.1) THEN
          NCOLS = MIN0(NCOLS,8)
        ELSE IF (LAYOUT.EQ.2) THEN
          NCOLS = MIN0(NCOLS,10)
        END IF
        GO TO 1
      END IF
!                                          Set flag if more input found
      AGAIN = ISTAT5.EQ.0
!                                           Read next retrieval request
      IF (AGAIN) THEN
!                          Read data type and number of retrieved items

        READ (RECORD,'(A8)') DATYPE                                  !5
        READ (RECORD(9:),*) ITEMS                                    !5
        WRITE (6,'(//T2,3A,I6/)')                                    !5
     &           'DATA TYPE ', DATYPE, ',  ITEMS =', ITEMS           !5
        READ (5,'(A)') RECORD
!                                                  Look for pre-ELEMENT
!                                                part of request string
        IF (RECORD(1:1).NE.' ') THEN
!                                              Add to WANTED if present
          J = 1
          DO WHILE (ISTAT5.EQ.0 .AND. RECORD(1:1).NE.' ')
            WANTED(J:J+72) = RECORD
            WRITE (6,'(T2,A)') WANTED(J:J+72)
            J = J + 73
            READ (5,'(A)',IOSTAT=ISTAT5) RECORD
          END DO
!                                      Put 'ELEMENTS' in request string
          IF (ISTAT5.EQ.0) THEN
            WANTED(J:J+7) = 'ELEMENTS'
            WRITE (6,'(T2,A)') WANTED(J:J+7)
            NEXT = J + 8
            NSTART = NEXT                                            !5
          ELSE
            RECORD = ' '
          END IF
        END IF
        MORE = ISTAT5.EQ.0
!                                            Read list of element names
        J = NEXT
        DO WHILE (ISTAT5.EQ.0 .AND. RECORD.NE.' ')
          WANTED(J:J+71) = RECORD
          WRITE (6,'(T2,A)') WANTED(J:J+71)
          READ (5,'(A)',IOSTAT=ISTAT5) RECORD
          J = J + 72
        END DO
        AGAIN = ISTAT5.EQ.0
!                               Find max. number of obs RARRAY can hold
        NOBS = NUMVALS/ITEMS
!                                             but not > 10000 or MAXOBS
        NOBS = MIN0(NOBS,10000,MAXOBS)
!                                          Stop job if no more requests
      ELSE
        MORE = .FALSE.
      END IF

!-----------------------------------------------------------------------
!     LOOP OVER CALLS TO 'MDB' UNTIL RETRIEVAL REQUEST IS COMPLETE
!-----------------------------------------------------------------------

!                                           Initialisations for do loop
      KOUNTOBS = 0  ! No obs found yet
      ISTAT = 0     ! New request
!                                              Loop over calls to 'mdb'
      DO WHILE (MORE .AND. KOUNTOBS.LT.MAXOBS)
        IOBS = NOBS
        CALL MDB (DATYPE, WANTED, RARRAY, IOBS, ITEMS,
     &            ISTAT, CSTR, CREP)
!                                                     Check status word
        IF (ISTAT.EQ.16) THEN
          WRITE (6,'(/T9,A/)') 'PROGRAM ABENDING DUE TO ABOVE ERROR'
          CALL SYSABN (916)
!                                           Check for more data to come
        ELSE IF (ISTAT.NE.4) THEN
          MORE = .FALSE.
          IF (ISTAT.EQ.8) WRITE(6,'(/T9,A)')
     &       'NO MDB DATA AVAILABLE FOR REQUESTED DATE'
        END IF
!                                                    Print out the data
        IF (KOUNTOBS.LT.NPRINT) THEN ! more to print

!-----------------------------------------------------------------------
!     LAYOUT 1:  EACH COLUMN CONTAINS DATA FOR ONE OBSERVATION
!-----------------------------------------------------------------------

          IF (LAYOUT.EQ.1) THEN
!                                                  Get number of obs to
!                                                 print from this batch
            IPRINT = MIN0(IOBS,NPRINT-KOUNTOBS)
!                                                        Set up formats
            I = 12*MIN0(IPRINT,NCOLS) + 12                           !5
            WRITE (FORMATC,'(A,I3.3,A)')
     &           '(T', I, ',A,T2,I5,2X,10(2X,A10))'                  !5
            WRITE (FORMATI,'(A,I3.3,A)')
     &           '(T', I, ',A,T2,I5,2X,1P,10E12.4)'                  !5

!                                     Loop over obs. in groups of NCOLS
            DO JOB=1,IPRINT,NCOLS

!                    Get range of RARRAY subscripts for first parameter

              J1 = JOB                ! 1st parameter, first ob
              J2 = J1 + NCOLS - 1     ! 1st parameter, last ob
              J2 = MIN0(J2,IPRINT)    ! 1st parameter, last ob to print

!                                                 Print column headings
              NFIRST = J1 + KOUNTOBS
              NLAST  = J2 + KOUNTOBS
              WRITE (6,'(/T5,10I12)') (J,J=NFIRST,NLAST)             !5

!                                        Loop over elements (1 per row)
              DO ITEM=1,ITEMS
!                                              Check for character data
                CHARS = RARRAY(J1).GE.262144.0
                J = J1
                DO WHILE (CHARS .AND. J.LE.J2)
                  IVAL = NINT(RARRAY(J))
                  CHARS = (RARRAY(J).LE.2.14784E9) .AND.
     &                    (IVAL-65535*(IVAL/65536)-1.LE.90)          !2
                  J = J + 1
                END DO
!                                                      Get element name
                CALL GETNAME (WANTED, NSTART, NAME)                  !5
                NSTART = 0                                           !5
!                                                    Print row of table
!                                                    (1) Character data
                IF (CHARS) THEN
                  ILEN = NINT(RARRAY(J1))/65536
                  WRITE (6,FORMATC) NAME, ITEM, (CSTR(JOB-J1+J)      !5
     &               (MOD(NINT(RARRAY(J)),65536):
     &               (MOD(NINT(RARRAY(J)),65536)+ILEN-1)), J=J1,J2)
                ELSE
!                                                       (2) Real values

                  WRITE (6,FORMATI) NAME, ITEM, (RARRAY(J),J=J1,J2)  !5
                END IF
!                                  Update subscripts for next parameter
                J1 = J1 + NOBS
                J2 = J2 + NOBS
              END DO ! ITEM
              NSTART = -1    ! so that GETNAME gets next name        !5
            END DO ! JOB

!-----------------------------------------------------------------------
!        LAYOUT 2:  EACH COLUMN CONTAINS DATA FOR ONE PARAMETER
!-----------------------------------------------------------------------

          ELSE IF (LAYOUT.EQ.2) THEN
!                                                  Loop over parameters
!                                                    in groups of NCOLS
            DO ITEM=1,ITEMS,NCOLS
!                                                 Print column headings
              NLAST = MIN0(ITEM+NCOLS-1,ITEMS)
              WRITE (6,'(/T4,10I12)') (J,J=ITEM,NLAST)

!                  Get range of RARRAY subscripts for first observation

              J1 = (ITEM-1)*NOBS + 1   ! 1ST OB, FIRST PARAMETER
              J2 = (NLAST-1)*NOBS + 1  ! 1ST OB, LAST PARAMETER

!                                           Get number of obs. to print
              NLAST = NPRINT - KOUNTOBS
              NLAST = MIN0(NLAST,IOBS)
!                                            Loop over obs. (1 per row)
              DO JOB=1,NLAST
!                                                    Print row of table
                WRITE (6,'(T2,I5,2X,1P,10E12.4)')
     &                     JOB, (RARRAY(J),J=J1,J2,NOBS)

!                                Update subscripts for next observation
                J1 = J1 + 1
                J2 = J2 + 1
              END DO ! JOB
            END DO ! ITEM
          END IF
        END IF
!                                              Update observation count
        KOUNTOBS = KOUNTOBS + IOBS
      END DO
!                                     Print number of obs retrieved and
      IF (KOUNTOBS.GT.0)
     &    WRITE (6,'(/T2,I8,A)') KOUNTOBS, ' OBSERVATIONS RETRIEVED'

!                                       Go back to look for new request
      IF (AGAIN) GO TO 1
!                                                          All finished
      STOP
      END
