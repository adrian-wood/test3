PROGRAM MDBXTNDB
!
!-----------------------------------------------------------------------
!
! PROGRAM      : MDBXTNDB
!
! PURPOSE      : TO COPY DATA FROM ONE STORAGE DATA SET IN NEW FORMAT
!                TO ANOTHER (E.G. A BIGGER ONE).
!
! DESCRIPTION  : MDBXTNDB copies all the data stored in one MDB storage
!                data set to a similar data set. The storage data sets
!                must both be in the format introduced for satellite
!                data in 2001, and details should be specified on DD
!                statements with DDNAMEs 'OLD' and 'NEW'.
!
!                If the only difference between the data sets is that
!                the new one has more data records (but not more map
!                records) data records will just be copied record by
!                record; otherwise the BUFR messages will be extracted
!                and re-stored by calling BUFREP.
!
!                The program looks at the data sets to determine
!                record lengths and the use of coarse or fine
!                resolution latitudes and longitudes.
!
!                Note that there is no check here for current data, so
!                beware of data getting lost if the new storage data
!                set has a smaller retention period.
!
! ABENDS       : Some error conditions will cause the job to abend with
!                the following user error codes.
!
!                  701  I/O error opening input or output data set.
!                  702  Input or output data set is in old format.
!                  703  No latitude descriptor in output BUFR sequence.
!                  721  VALUES array too small in GETVALS subroutine.
!                  722  Too many index records for one index period.
!                  727  Too many map records in output data set.
!                  731  Output data set is full.
!                 741-2  Could not make index entry.
!
!                Apart from 701-3, the user code is 700 + (return code
!                from BUFREP). See BUFREP listing for more details.
!
! CALLS        : BUFREP, ICHAR2, ICHAR3, IDES, LOCALD, SCRIPT.
!                Also DATIM and SYSABN.
!
! HISTORY      : Original version by Brian Barwell, February 2002.
!
! REVISION INFO:
!
! $Workfile: mdbxtndb.F90$ $Folder: OpSource$
! $Revision: 2$ $Date: 06/05/2011 16:10:03$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         06/05/2011 16:10:03    Alison Weir     Ported
!        to F95. Amended to use c i/o for ZFS datasets.
!  1    MetDB_Refresh 1.0         03/05/2011 13:46:04    Alison Weir
!       Initial F77 version
! $
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
USE bufrep_mod
USE datim_mod
USE ichar2_mod
USE ichar3_mod
USE ides_mod
USE locald_mod
USE script_mod

IMPLICIT NONE

!                                                            Parameters
INTEGER, PARAMETER  ::    MAXDES=10000   ! Size of descriptor array
INTEGER, PARAMETER  ::    MAXREC=27998   ! Maximum record length of data

!                                                             Variables
INTEGER  :: I               ! General variable for local use
INTEGER  :: ICENT           ! Last year ending in '00'
INTEGER  :: IDUM(999)       ! Dummy array for use in LOCALD
INTEGER  :: IED             ! BUFR edition number from message
INTEGER  :: INT4            ! Dummy variable for TRANSFER function
INTEGER  :: ITEMS(12)       ! Processing control data for BUFREP
INTEGER  :: IV1,    IV2     ! Old & new data set versions
INTEGER  :: IYEAR           ! Current year
INTEGER  :: J               ! General loop variable for local use
INTEGER  :: JENTRY          ! Loop variable for entries in index rec.
INTEGER  :: JREC            ! Loop variable for records in data set
INTEGER  :: KODE            ! Return code from I/O or BUFREP
INTEGER  :: LDESC(MAXDES)   ! Descriptor list for output data set
INTEGER  :: LENTRY1,LENTRY2 ! Old & new index entry lengths
INTEGER  :: NDATA           ! Record number of data record in DATREC
INTEGER  :: NDES = 0        ! No. of descriptors for output data set
INTEGER  :: NDREC           ! Record number of required data record
INTEGER  :: NFINISH         ! Byte number of finish of BUFR message
INTEGER  :: NLENGTH         ! Length of BUFR message in bytes
INTEGER  :: NMREC1, NMREC2  ! Old & new map records in data set
INTEGER  :: NOFSET1,NOFSET2 ! Old & new index period offsets from 00Z
INTEGER  :: NPT             ! Pointer to byte in data set record
INTEGER  :: NPT1            ! Max. pointers per input map record
INTEGER  :: NRECS1, NRECS2  ! Old & new total records in data set
INTEGER  :: NSTART          ! Byte number of start of BUFR message
INTEGER  :: NSTORE = 0      ! Counter for messages stored by BUFREP
INTEGER  :: NTOTAL = 0      ! Counter for all messages processed
INTEGER  :: NTRIES          ! No. of entries in current index record
INTEGER  :: NUMDUP = 0      ! Number of duplicate messages found
INTEGER  :: NUMOLD = 0      ! Number of messages too old to store
INTEGER  :: NUMREC          ! Record number
INTEGER  :: NXMIN1, NXMIN2  ! Old & new index period lengths (minutes)
INTEGER  :: NXREC           ! Next index record for current period
INTEGER  :: NXREC1, NXREC2  ! Old & new index periods in data set
INTEGER  :: NOW(8)          ! Time of receipt (in 'DATIM' format)

LOGICAL  :: FLAGS(9)        ! Processing control flags for BUFREP

CHARACTER(LEN=MAXREC)  :: DATREC  ! Data record from input data set
CHARACTER(LEN=MAXREC)  :: INDXREC ! Index record from input data set

! Namelist
INTEGER  :: LENREC1,LENREC2 ! Old & new data set record lengths
NAMELIST /INPUT/ LENREC1, LENREC2

DATA FLAGS /.TRUE.,.FALSE.,.TRUE.,2*.FALSE.,.TRUE.,3*.FALSE./
DATA ITEMS /1, 4*0, 7*-1/

!                                                 Current date and time
CALL DATIM (NOW)
IYEAR = NOW(8)           ! Current year
ICENT = 100*(IYEAR/100)  ! Last year ending in 00

!                                   Reset NOW(1-3) to 0 for rest of job
NOW(1) = 0
NOW(2) = 0
NOW(3) = 0

!=======================================================================
! READ INPUT DATA  - NEW AND OLD DATASET RECORD LENGTHS
!=======================================================================

OPEN (10,FILE='DD:INPUT',ACTION='READ',IOSTAT=KODE)
READ (10,NML=INPUT,IOSTAT=KODE)
CLOSE(10)

!=======================================================================
!  OPEN INPUT AND OUTPUT DATA SETS AND READ HEADER RECORD INFORMATION.
!  (DATREC(1:5) is used for temporary storage if an I/O error occurs.)
!=======================================================================
!
!                          Open input storage data set as direct access

CALL METDB_COPEN(1, 'DD:OLD', 0, KODE)
IF (KODE /= 0) THEN                      ! I/O error
  DATREC(1:5) = ' OLD '
ELSE
!                         Open output storage data set as direct access

  CALL METDB_COPEN(2, 'DD:NEW', 3, KODE)
  IF (KODE /= 0) DATREC(1:5) = ' NEW '   ! I/O error
END IF
!                            Fatal error message and crash if I/O error
IF (KODE /= 0) THEN
  WRITE (6,'(/T2,3A,I4/)') 'JOB TERMINATING: I/O ERROR OPENING', &
           DATREC(1:5), 'DATA SET.  STATUS CODE:', KODE
  CALL SYSABN (701)
END IF
!
!-----------------------------------------------------------------------
!                 READ AND DECODE THE HEADER RECORD
!                 =================================
! HEADER RECORD:
! +-------------------------------------------------------------------+
! !  HEX : VERSION : TOTAL : NO.OF : NO.OF : INDEX  : INDEX  : INDEX  !
! ! 0000 : NUMBERS : NO.OF :  MAP  : INDEX : PERIOD : OFFSET : ENTRY  !
! !      : D/S,NDX : RECS. : RECS. : RECS. : (MINS) : (MINS) : LENGTH !
! +------+----+----+-------+-------+-------+--------+--------+--------+
! 0      2    3    4       8      12      16       20       24       28
!                            Byte numbers
!-----------------------------------------------------------------------
!
NUMREC = 1

CALL METDB_CREAD_DIR(1,INDXREC(1:LENREC1),LENREC1,NUMREC,KODE)
IV1     = TRANSFER(INDXREC( 1: 4),INT4)
NRECS1  = TRANSFER(INDXREC( 5: 8),INT4)
NMREC1  = TRANSFER(INDXREC( 9:12),INT4)
NXREC1  = TRANSFER(INDXREC(13:16),INT4)
NXMIN1  = TRANSFER(INDXREC(17:20),INT4)
NOFSET1 = TRANSFER(INDXREC(21:24),INT4)
LENTRY1 = TRANSFER(INDXREC(25:28),INT4)

CALL METDB_CREAD_DIR(2,INDXREC(1:LENREC2),LENREC2,NUMREC,KODE)
IV2     = TRANSFER(INDXREC( 1: 4),INT4)
NRECS2  = TRANSFER(INDXREC( 5: 8),INT4)
NMREC2  = TRANSFER(INDXREC( 9:12),INT4)
NXREC2  = TRANSFER(INDXREC(13:16),INT4)
NXMIN2  = TRANSFER(INDXREC(17:20),INT4)
NOFSET2 = TRANSFER(INDXREC(21:24),INT4)
LENTRY2 = TRANSFER(INDXREC(25:28),INT4)

NPT1 = LENREC1/2      ! Maximum pointers per input map record
!
!-----------------------------------------------------------------------
!  COMPATIBILITY CHECK BETWEEN THE TWO DATA SETS
!-----------------------------------------------------------------------
!
IF (IV1 >= 65536 .OR. IV2 >= 65536) THEN
  WRITE (6,'(/T2,2A,2I8/)') 'JOB TERMINATING - DATA SET ', &
           'IN OLD FORMAT:', IV1, IV2
  CALL SYSABN (702)
END IF
!
!=======================================================================
!  IF POSSIBLE, DO A STRAIGHT COPY FROM OLD TO NEW DATA SET.
!  For the conditions making this possible, see the IF statement below.
!=======================================================================
!
IF_COPY: &
IF (LENREC2 == LENREC1 .AND. & ! Same record lengths
    IV2     == IV1     .AND. & ! Same data set & index versions
    NMREC2  == NMREC1  .AND. & ! Same number of map records
    NXREC2  == NXREC1  .AND. & ! Same number of index periods
    NXMIN2  == NXMIN1  .AND. & ! Same index period lengths
    NOFSET2 == NOFSET1 .AND. & ! Same offsets from 00Z
    NRECS2  >= NRECS1) THEN   ! No reduction of data records

  WRITE (6,'(/T2,A/)') &
      'WILL DO A STRAIGHT COPY FROM OLD TO NEW STORAGE DATA SET'
!
!-----------------------------------------------------------------------
!  Copy the map records.  If there are more records in the output data
!  set, the last map record will be a combination of map pointers from
!  the input record and zeroes from the output record representing the
!  added data records.
!  To save space INDXREC and DATREC are used to hold the input and
!  output map records respectively.
!-----------------------------------------------------------------------
!
!                                         Initialise map record counter
  I = 2
!                             Number of input map pointers to be copied
  NPT = NRECS1
!                               Loop over map records in input data set
DO_MAP: &
  DO JREC=1,NMREC1
    I = I + 1  ! Next map record number
!                                              Read input map record if
!                                           it contains wanted pointers
IFLABEL2: &
    IF (NPT > 0) THEN
      CALL METDB_CREAD_DIR(1,INDXREC(1:LENREC1),LENREC1,I,KODE)
!                                             Read output map record if
!                                             it contains wanted zeroes
      IF (NPT <= NPT1) THEN
        CALL METDB_CREAD_DIR(2,DATREC(1:LENREC1),LENREC1,I,KODE)
!
!                                 Combine to form map record for output
!
        INDXREC(2*NPT+1:LENREC1) = DATREC(2*NPT+1:LENREC1)
      END IF
!                                               Write output map record
      CALL METDB_CWRITE_DIR(2,INDXREC(1:LENREC1),LENREC1,I,KODE)
    END IF  IFLABEL2
!                             Number of input map pointers left to copy
    NPT = NPT - NPT1
  END DO    DO_MAP
!
!-----------------------------------------------------------------------
!  Copy remaining records (index overflows and data records)
!-----------------------------------------------------------------------
!
  I = 0      ! Data record counter for loop below

  DO JREC=NMREC1+3,NRECS1
    CALL METDB_CREAD_DIR(1,DATREC(1:LENREC1),LENREC1,JREC,KODE)
    CALL METDB_CWRITE_DIR(2,DATREC(1:LENREC1),LENREC1,JREC,KODE)
    I = I + 1
!                                                       Progress report
    IF (MOD(I,100) == 0) WRITE (6,'(T2,I7,A)') &
        I, ' data records copied so far'

  END DO ! JREC
!                                                               Summary
  WRITE (6,'(/T2,I7,A)') I, ' data records copied'
!
!=======================================================================
!  IF A STRAIGHT COPY IS NOT POSSIBLE, RE-STORE THE DATA USING BUFREP.
!=======================================================================
!
ELSE   IF_COPY
  WRITE (6,'(/T2,A/)') &
    'MESSAGES WILL BE EXTRACTED AND RE-STORED USING "BUFREP"'

!                                             Read BUFR sequence record
  CALL METDB_CREAD_DIR(2,DATREC(1:LENREC2),LENREC2,2,KODE)
!                                                Store sequence details
  CALL LOCALD (0, 0, IDUM, I, DATREC, 'ADD')
!                                             Fully expand 1st sequence
  READ (DATREC(1:6),'(I6)') I
  LDESC(1) = IDES(I)
  CALL SCRIPT (LDESC, NDES, .FALSE.)
!
!-----------------------------------------------------------------------
!  Look for latitude descriptor in BUFR sequence and set FLAGS(5) to
!  .TRUE. if it is high resolution. (Integers 1281 and 1282 correspond
!  to "FXXYYY" descriptors 005001 and 005002 respectively.)
!-----------------------------------------------------------------------
!
  J = 0
  DO WHILE (J < NDES)
    J = J + 1
    IF (LDESC(J) == 1281 .OR. LDESC(J) == 1282) THEN ! Latitude
      FLAGS(5) = LDESC(J) == 1281
      J = NDES + 1  ! To force exit from DO loop
    END IF
  END DO
!                            Terminate if latitude descriptor not found
  IF (J == NDES) THEN
    WRITE (6,'(/T2,2A/)') 'JOB TERMINATING - ', &
             'NO LATITUDE DESCRIPTOR FOUND IN BUFR SEQUENCE'
    CALL SYSABN (703)
  END IF
!
!-----------------------------------------------------------------------
!  Loop over index periods in input data set
!-----------------------------------------------------------------------
!
DO_IPER: &
  DO JREC=1,NXREC1
    NXREC = JREC + NMREC1 + 2               ! Base index record
    NDATA = -1                              ! No data record held
!
!-----------------------------------------------------------------------
!  Loop over index records in period
!-----------------------------------------------------------------------
!
DO_NXREC: &
    DO WHILE (NXREC > 0)
      CALL METDB_CREAD_DIR(1,INDXREC(1:LENREC1),LENREC1,NXREC,KODE)
      NTRIES = ICHAR2(INDXREC(6:7))         ! Entries in record
      NXREC = ICHAR3(INDXREC(8:10))         ! Next rec. in chain
      NPT = 10                              ! End of index header
!
!-----------------------------------------------------------------------
!  Loop over index entries in record
!-----------------------------------------------------------------------
!
DO_JENTRY: &
      DO JENTRY=1,NTRIES
        NPT = NPT + LENTRY1                    ! End of next entry
        NDREC   = ICHAR3(INDXREC(NPT-6:NPT-4)) ! Data record
        NSTART  = ICHAR2(INDXREC(NPT-3:NPT-2)) ! Start byte
        NLENGTH = ICHAR2(INDXREC(NPT-1:NPT))   ! Length in bytes
        NFINISH = NSTART + NLENGTH - 1         ! End byte
!
!-----------------------------------------------------------------------
!  Read the data record for this entry if not already held in DATREC
!-----------------------------------------------------------------------
!
        IF (NDREC /= NDATA) THEN
          CALL METDB_CREAD_DIR(1,DATREC(1:LENREC1),LENREC1,NDREC,KODE)
          NDATA = NDREC                     ! Current data record
        END IF
!
!-----------------------------------------------------------------------
!  Find BUFR edition number and extract time of receipt (ToR)
!-----------------------------------------------------------------------
!
        IED = ICHAR(DATREC(NSTART+7:NSTART+7))  ! BUFR edition
        IF (IED < 2) THEN
          I = NSTART + 16   ! ToR location (editions 0 & 1)
        ELSE IF (IED < 4) THEN
          I = NSTART + 20   ! ToR location (editions 2 & 3)
        ELSE
          I = NSTART + 23   ! ToR location (editions >3)
        END IF
!                                Get year of receipt and put in NOW(8).
!                                Convert to 4 digit year if edition <4.
        IF (IED < 4) THEN
          NOW(8) = ICHAR(DATREC(I:I)) + ICENT
          IF (NOW(8) > IYEAR) NOW(8) = NOW(8) - 100
          I = I + 1
        ELSE
          NOW(8) = ICHAR2(DATREC(I:I+1))
          I = I + 2
        END IF
!                           Put receipt month, day, hour, minute in NOW
        DO J=7,4,-1
          NOW(J) = ICHAR(DATREC(I:I))
          I = I + 1
        END DO
!                            Ensure subtype-dependent info is preserved

        ITEMS(3) = ICHAR(INDXREC(NPT-11:NPT-11))
!
!-----------------------------------------------------------------------
!  Store message in new storage data set using BUFREP
!-----------------------------------------------------------------------
!
        CALL BUFREP (2, LENREC2, NOW, FLAGS, ITEMS, -1, &
                     DATREC(NSTART:NFINISH), KODE)
        NTOTAL = NTOTAL + 1
!
!-----------------------------------------------------------------------
!  Check return code from BUFREP. Codes 11 and 12 are not fatal but
!  codes >20 will cause job termination with a failure message.
!  (DATREC(1:40) is used to hold the message.)
!-----------------------------------------------------------------------
!
!                                                Non-fatal return codes
!
        IF (KODE == 11) THEN       ! Data too old
          NUMOLD = NUMOLD + 1
        ELSE IF (KODE == 12) THEN  ! Got this already
          NUMDUP = NUMDUP + 1
!                                                    Fatal return codes
        ELSE IF (KODE == 21) THEN
          DATREC(1:40) = 'VALUES array in GETVALS is too small'
        ELSE IF (KODE == 22) THEN
          DATREC(1:40) = 'Too many index records for one period'
        ELSE IF (KODE == 27) THEN
          DATREC(1:40) = 'Too many map records in output data set'
        ELSE IF (KODE == 31) THEN
          DATREC(1:40) = 'No free records in output data set'
        ELSE IF (KODE > 40) THEN
          DATREC(1:40) = 'Unable to create index entry'

!                                 Increment counter for stored messages
        ELSE
          NSTORE = NSTORE + 1
          IF (MOD(NSTORE,100) == 0) WRITE (6,'(T2,I7,A)') &
              NSTORE, ' messages stored so far'
        END IF
!                          Print message and crash if fatal return code
!
        IF (KODE > 20) THEN
          WRITE (6,'(/T3,2A,I4,A/T3,A/)') 'JOB TERMINATING - ', &
            'ERROR CODE', KODE, ' FROM "BUFREP".', DATREC(1:40)
          CALL SYSABN (700+KODE)
        END IF
!                                                             End loops
      END DO   DO_JENTRY  ! Loop over index entries in record
    END DO     DO_NXREC   ! Loop over index records in period
  END DO       DO_IPER    ! Loop over index periods in data set

!                                       Print number of messages stored

  WRITE (6,'(/T2,I7,A)') NTOTAL, ' messages processed'
  WRITE (6,'( T2,I7,A)') NSTORE, ' messages stored'
END IF  IF_COPY
!
!-----------------------------------------------------------------------
!  Close data sets, print non-fatal messages and terminate
!-----------------------------------------------------------------------
!
CALL METDB_CCLOSE (2)
CALL METDB_CCLOSE (1)
!                                                        Print messages
IF (NUMDUP > 0) WRITE (6,'(T2,I7,A)') NUMDUP, &
    ' messages rejected as duplicates'
IF (NUMOLD > 0) WRITE (6,'(T2,I7,A)') NUMOLD, &
    ' messages too old to store in output data set'
!                                                              All done
STOP
END PROGRAM MDBXTNDB
