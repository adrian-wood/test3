      PROGRAM MDBXTRCT

!-----------------------------------------------------------------------
!
! PROGRAM      : MDBXTRCT
!
! PURPOSE      : TO EXTRACT DATA FROM A STORAGE DATA SET FOR A
!                SPECIFIED TIME WINDOW FOR ARCHIVING.
!
! DESCRIPTION  : MDBXTRCT searches through a MetDB storage data set,
!                extracts data for a specified time window and creates
!                a new storage data set containing only the selected
!                data (e.g. 24 hours of data to be archived).
!
!                The input storage data set should be specified in the
!                JCL on a DD statement with a DDname 'OLD' and must
!                refer to a data set in the new format introduced in
!                2001 for satellite data.
!
!                The output storage data should be specified in the
!                JCL on a DD statement with a DDname 'NEW' and will
!                be in the same format.
!
!                One line of input on unit 5 gives the start and end of
!                the required time window and the record length of the
!                input storage data set as comma-separated variables.
!                Format is as in this example (start in column 1):
!
!                     20010812/0000Z,20010812/2359Z,27998
!
!                For archiving purposes, the start of the time window
!                should coincide with the start of an index period and
!                the end time should be one minute before the end of an
!                index period. Adding ',ASCII' to the above input
!                causes EBCDIC identifiers in index entries in the
!                input data set to be converted into ASCII for output.
!
! ABENDS       : Some error conditions will cause the job to abend with
!                the following user error codes. In all cases, the
!                abend will occur before anything has been written to
!                the output data set.
!
!                 700  Too many records in input data set
!                 701  Input has unsupported data set version number
!                 702  Input has unsupported index entry version number
!                 703  Unexpected time found at start of data record
!                 704  Unexpected time found at start of index record
!                 705  Bad overflow data record number               !2
!                 706  Missing record number for output record
!
! CALLS        : DT2HRS, EB2ASC, CHAR3, ICHAR2, ICHAR3
!                Also DATIM, SYSABN
!
! HISTORY      : Original version by Brian Barwell, June 2001.
!
! REVISION INFO:
!
! $Workfile: mdbxtrct.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 13/05/2010 15:38:46$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         13/05/2010 15:38:46    Brian Barwell   New
!       version to work with storage data sets containing long bulletins.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:20    Sheila Needham  
! $
! Revision 2.2  2005/10/04 15:16:06  usmdb
! 17th October 2005. CHG017782. Stan Kellett. !2.2
! Increased parameter MAXRECS from 200000 to 300000 to allow
! archiving of AMSRE data.
!
! Revision 2.1  2002/10/07  14:20:23  14:20:23  usmdb (MetDB account c/o John C
! Ward)
! Increased MAXRECS to 200000 to handle MDB.ATOVSG.OPER.GAIR
! dataset - S.Cox
!
! Revision 2.0  2001/07/04  13:23:24  13:23:24  usmdb (Generic MetDB account)
! Initial version
!
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
!                                                            Parameters
      INTEGER    MAXREC      ! Maximum record length of dataset
      INTEGER    MAXRECS     ! Maximum number of records in dataset
      PARAMETER (MAXREC=27998, MAXRECS=300000)                      !2.2
!                                                             Variables

      INTEGER I               ! General variable for local use
      INTEGER IDAY1,  IDAY2   ! Start & end days of archive period
      INTEGER IHOUR1, IHOUR2  ! Start & end hours of archive period
      INTEGER ILONG           ! Long bulletin counter                !2
      INTEGER IMNTH1, IMNTH2  ! Start & end months of archive period
      INTEGER IMIN1,  IMIN2   ! Start & end minutes of archive period
      INTEGER INEW            ! Record number in output data set
      INTEGER IOLD            ! Record number in input data set
      INTEGER IPDAY           ! Day of current index period
      INTEGER IPMNTH          ! Month of current index period
      INTEGER IPT             ! Pointer to location in RECORD
      INTEGER IPTIM1, IPTIM2  ! First & last index times (century mins)
      INTEGER IPYEAR          ! Year of current index period
      INTEGER IYEAR1, IYEAR2  ! Start & end years of archive period
      INTEGER J               ! General variable for local use
      INTEGER JDAT            ! Loop variable for data records in msg.!2
      INTEGER JREC            ! Loop variable for records in data set
      INTEGER JPTIM           ! Loop variable for index periods
      INTEGER LASTMAP         ! Number of last map record in output d/s
      INTEGER LASTNDX         ! Number of last base index in output d/s
      INTEGER LBYTE(MAXRECS)  ! Locations in data records of start   !2
                              !   of 3-byte overflow record numbers  !2
      INTEGER LENREC          ! Record length of storage data set
      INTEGER LENTRY          ! Length of index entries in storage d/s
      INTEGER LREC(MAXRECS)   ! Record numbers of records containing !2
                              !   3-byte overflow record numbers     !2
      INTEGER MAPVAL(MAXRECS) ! Record numbers for output map record(s)
      INTEGER NBASE1          ! Base index record in input data set
      INTEGER NBASE2          ! Base index record in output data set
      INTEGER NRECS1, NRECS2  ! Total number of records, input & output
      INTEGER NRECORD         ! Counter for records in output data set
      INTEGER NDVER           ! Version no. for storage data set format
      INTEGER NEXTDAT         ! Record no. of next data record to read
      INTEGER NEXTNDX         ! Record no. of next index record to read
      INTEGER NMRECS1,NMRECS2 ! Number of map records, input & output
      INTEGER NTRIES          ! Number of entries in index record
      INTEGER NUMNEW(MAXRECS) ! Location of record in output data set
      INTEGER NUMOLD(MAXRECS) ! Location of record in input data set
                              !  (with negative sign if index record)
      INTEGER NUMOVFL         ! No. of overflow data records in msg. !2
      INTEGER NVER            ! 'Versions' word in data set header
      INTEGER NDXREC          ! Output record number for index record
      INTEGER NXRECS1,NXRECS2 ! Number of index records, input & output
      INTEGER NXMIN           ! Number of minutes in index period
      INTEGER NXVER           ! Version number for index entry format
      INTEGER NX00Z           ! Index period offset from 00Z (minutes)

      LOGICAL ASCII           ! .TRUE. for EBCDIC to ASCII conversion

      CHARACTER*(MAXREC) DATREC     ! Data record read from input    !2
      CHARACTER*(MAXREC) RECORD     ! Record of storage data set
      CHARACTER*8        DHEAD      ! Header from data record
      CHARACTER*80       HEAD       ! Revision information           !2
      CHARACTER*14       ITIME1     ! ) Archive period start & end
      CHARACTER*14       ITIME2     ! )  times ('yyyymmdd/hhmmZ')
      CHARACTER*30       TEXT       ! Input control text
      CHARACTER*5        TIMTAG     ! Time tag (y,m,d,h,m)

!                             Common block (for dynamic initialisation)

      COMMON /COMEXT/ MAPVAL, NUMNEW, NUMOLD, LREC, LBYTE            !2
      COMMON /COMREC/ RECORD, DATREC                                 !2
!                                                    External functions

      INTEGER DT2HRS     ! Function to return century hour
      INTEGER ICHAR2     ! Function to convert C*2 to I*4
      INTEGER ICHAR3     ! Function to convert C*3 to I*4
      CHARACTER*2 CHAR2  ! Function to convert I*4 to C*2
      CHARACTER*3 CHAR3  ! Function to convert I*4 to C*3

!                                                  Revision information
      HEAD = '$Workfile: mdbxtrct.f$ ' //
     &       '$Revision: 2$ $Date: 13/05/2010 15:38:46$'

!                                                     Initialise arrays
      DO J=1,MAXRECS
         NUMNEW(J) = 0
         NUMOLD(J) = 0
      END DO

      ILONG = 0   ! Long bulletin counter                            !2

!-----------------------------------------------------------------------
!  READ START & END DATE/TIMES, DATA SET RECORD LENGTH AND OPTIONS.
!  (Record lengths are needed to open data sets)
!-----------------------------------------------------------------------

      READ (5,'(A14,1X,A14,1X,I5,1X,A30)') ITIME1, ITIME2, LENREC, TEXT

!                                       Check for EBCDIC --> ASCII flag
      ASCII = INDEX(TEXT,'ASCII').GT.0
!                                                Decode dates and times

      READ (ITIME1,'(I4,2I2,1X,2I2)') IYEAR1,IMNTH1,IDAY1,IHOUR1,IMIN1
      READ (ITIME2,'(I4,2I2,1X,2I2)') IYEAR2,IMNTH2,IDAY2,IHOUR2,IMIN2

      WRITE (6,'(T3,4A)') 'Archive time window: ', ITIME1,' to ',ITIME2

!-----------------------------------------------------------------------
!  OPEN INPUT AND OUTPUT DATA SETS
!-----------------------------------------------------------------------

      OPEN (1, FILE='OLD', ACCESS='DIRECT', RECL=LENREC) ! Input
      OPEN (3, FILE='NEW', FORM='UNFORMATTED')           ! Output

!=======================================================================
!                 READ AND DECODE THE HEADER RECORD
!                 =================================
!
! HEADER RECORD:
! +-------------------------------------------------------------------+
! !  HEX : VERSION : TOTAL : NO.OF : NO.OF : INDEX  : INDEX  : INDEX  !
! ! 0000 : NUMBERS : NO.OF :  MAP  : INDEX : PERIOD : OFFSET : ENTRY  !
! !      : D/S,NDX : RECS. : RECS. : RECS. : (MINS) : (MINS) : LENGTH !
! +------+----+----+-------+-------+-------+--------+--------+--------+
! 0      2    3    4       8      12      16       20       24       28
!
!=======================================================================
!                                                    Read header record
      READ  (1,REC=1) NVER, NRECS1, NMRECS1, NXRECS1,
     &                NXMIN, NX00Z, LENTRY
!                                                Decode version numbers
      NDVER = NVER/256       ! Data set version
      NXVER = MOD(NVER,256)  ! Index version

!-----------------------------------------------------------------------
!     CHECKS FOR STORAGE DATA SET PROBLEMS
!-----------------------------------------------------------------------
!                                                      Too many records
      IF (NRECS1.GT.MAXRECS) THEN
        WRITE (6,'(/T5,2A,I7,A,I7/)') 'ARCHIVE ERROR: ',
     &           'Number of records =', NRECS1, ' >', MAXRECS
        CALL SYSABN (700)
!                                                  Bad data set version
      ELSE IF (NDVER.NE.1) THEN
        WRITE (6,'(/T5,2A,I6/)') 'ARCHIVE ERROR: ',
     &           'Unsupported data set version number -', NDVER
        CALL SYSABN (701)
!                                               Bad index entry version
      ELSE IF (NXVER.LT.1 .OR. NXVER.GT.2) THEN
        WRITE (6,'(/T5,2A,I6/)') 'ARCHIVE ERROR: ',
     &           'Unsupported index entry version number -', NXVER
        CALL SYSABN (702)
      END IF

!-----------------------------------------------------------------------
!  SET UP DATA FOR LOOP OVER INDEX PERIODS
!-----------------------------------------------------------------------
!                                                       Century minutes

      IPTIM1 = 60*DT2HRS(IYEAR1,IMNTH1,IDAY1,IHOUR1) + IMIN1  ! Start
      IPTIM2 = 60*DT2HRS(IYEAR2,IMNTH2,IDAY2,IHOUR2) + IMIN2  ! End

      IPTIM1 = IPTIM1 - MOD((IPTIM1-NX00Z),NXMIN)  ! First index time
      IPTIM2 = IPTIM2 - MOD((IPTIM2-NX00Z),NXMIN)  ! Last index time

!                                       Number of index periods to copy
      NXRECS2 = (IPTIM2-IPTIM1)/NXMIN + 1
      WRITE (6,'(T3,A,I4,I5,A)') 'Data selected from', NXRECS2, NXMIN,
     &         '-minute index periods.'
!                                           Last reserved output record
!                       (Count from first index record as we do not yet
!                             know how many map records there will be.)
      NRECORD = NXRECS2

!=======================================================================
!          LOOP OVER INDEX PERIODS FOR ARCHIVING TIME WINDOW
!          =================================================
!
!  INDEX RECORD:
!  +-------------------------------------------------------------- - -
!  :     DATE/TIME     : NO. OF  : NEXT INDEX : FIRST : SECOND :
!  : YR,MON,DAY,HR,MIN : ENTRIES : RECORD NO. : ENTRY : ENTRY  :
!  +---+---+---+---+---+---------+------------+-------+--------+-- - -
!  0   1   2   3   4   5         7           10     10+L     10+2L
!
!=======================================================================

      DO JPTIM=IPTIM1,IPTIM2,NXMIN
!                                         Get time tag for index period

        I = JPTIM/1440 + 1                        ! Century day
        CALL DATE13 (I, IPDAY, IPMNTH, IPYEAR)    ! Day, month, year
        I = MOD(JPTIM,1440)                       ! Minutes after 00Z
        TIMTAG = CHAR(MOD(IPYEAR,100)) //         ! Year (00-99)
     &           CHAR(IPMNTH) // CHAR(IPDAY) //   ! Month & day
     &           CHAR(I/60) // CHAR(MOD(I,60))    ! Hour & minute

!                        Get input and output base index record numbers
!                 (Output count starts from first index record since we
!                  do not yet know how many map records there will be.)

        NBASE1 = MOD((JPTIM-NX00Z)/NXMIN,NXRECS1) + NMRECS1 + 3 ! Input
        NBASE2 = MOD((JPTIM-NX00Z)/NXMIN,NXRECS2) + 1           ! Output

!=======================================================================
!        CHECK BASE INDEX RECORD AND GET FIRST DATA RECORD NUMBER
!=======================================================================
!                                       Read start of base index record

        READ (1,REC=NBASE1) RECORD(1:LENTRY+10)
!                                                        Check time tag
        IF (RECORD(1:5).NE.TIMTAG) THEN  ! Obsolete data
          NEXTNDX = 0   ! Skips loop over index records
          I = 100*(I/60) + MOD(I,60)  ! 'hhmm'
          WRITE (6,'(T5,A,I6,2I4,I6,A)') 'No data for index time',
     &         IPYEAR, IPMNTH, IPDAY, I, 'Z'

        ELSE                             ! Wanted data
          NEXTNDX = NBASE1   ! Base index record number in input
        END IF

!=======================================================================
!            LOOP OVER INDEX RECORDS FOR THIS INDEX PERIOD
!=======================================================================

        DO WHILE (NEXTNDX.GT.0)
!                                                Read next index record
          READ (1,REC=NEXTNDX) RECORD(1:LENREC)
!                                            Error message if wrong tag
          IF (RECORD(1:5).NE.TIMTAG) THEN
            WRITE (6,'(/T5,2A,2(2Z12)/)') 'ARCHIVE ERROR: ',
     &               'Wrong time in index record -', RECORD(1:5), TIMTAG
            CALL SYSABN (704)
!                                           Time tag OK so set pointers
          ELSE
            IF (NEXTNDX.EQ.NBASE1) THEN  ! Base index record
              NDXREC = NBASE2         ! Output record for this index
              MAPVAL(NDXREC) = 65535  ! No map record entry

            ELSE                         ! Overflow index record
              NRECORD = NRECORD + 1   ! Next available output record
              NDXREC = NRECORD        ! Output record for this index
              MAPVAL(NDXREC) = NBASE2 ! Output map record entry
            END IF
!                     (NUMOLD is set negative for index records so they
!                     can be distinguished from data records later on.)

            NUMOLD(NDXREC) = -NEXTNDX      ! -Old record number
            NUMNEW(NEXTNDX) = NDXREC       ! New record number       !2
          END IF

!=======================================================================
!            LOOP OVER INDEX ENRTIES FOR THIS INDEX RECORD
!=======================================================================

          NTRIES = ICHAR2(RECORD(6:7))  ! Number of entries          !2
          IPT = 10 + LENTRY             ! End of first entry         !2

          DO J=1,NTRIES                                              !2
            NEXTDAT = ICHAR3(RECORD(IPT-6:IPT-4)) ! Data record no.  !2
            NUMOVFL = ICHAR (RECORD(IPT-9:IPT-9)) ! No of overflows  !2

!                                          Set pointers if not yet done
            IF (NUMNEW(NEXTDAT).EQ.0) THEN                           !2
!                                                      Read data record
              READ (1,REC=NEXTDAT) DATREC(1:LENREC)                  !2

!                                            Error message if wrong tag
              IF (DATREC(1:5).NE.TIMTAG) THEN                        !2
                WRITE (6,'(/T5,2A,2(2Z12)/)') 'ARCHIVE ERROR: ',
     &            'Wrong time in data record -', DATREC(1:5), TIMTAG !2
                CALL SYSABN (703)
              END IF                                                 !2
!                                          If time tag OK, set pointers

              NRECORD = NRECORD + 1      ! Next available record
              MAPVAL(NRECORD) = NBASE2   ! Map record entry
              NUMOLD(NRECORD) = NEXTDAT  ! Old record number
              NUMNEW(NEXTDAT) = NRECORD  ! New record number
            END IF                                                   !2

!             If there are overflow records, store the location (record
!               and byte numbers) of the place in the first data record
!            where the number of the first overflow record will be held

            IF (NUMOVFL.GT.0) THEN                                   !2
              ILONG = ILONG + 1               ! Increment counter    !2
              LREC(ILONG) = NUMNEW(NEXTDAT)   ! New record number    !2
              I = ICHAR2(RECORD(IPT-3:IPT-2)) ! Start byte           !2
              LBYTE(ILONG) = I                ! Store start byte     !2

!=======================================================================
!         LOOP OVER OVERFLOW DATA RECORDS FOR THIS INDEX ENTRY
!=======================================================================
!
              NEXTDAT = ICHAR3(DATREC(I:I+2))  ! First overflow rec. !2

              DO JDAT=1,NUMOVFL                                      !2
!                                            Read header of data record
                READ (1,REC=NEXTDAT) DHEAD                           !2
!                                            Error message if wrong tag
                IF (DHEAD(1:5).NE.TIMTAG) THEN                       !2
                  WRITE (6,'(/T5,2A,2(2Z12)/)') 'ARCHIVE ERROR: ',   !2
     &             'Wrong time in data record -', DHEAD(1:5), TIMTAG !2
                  CALL SYSABN (703)                                  !2
                END IF                                               !2
!                                          If time tag OK, set pointers

                NRECORD = NRECORD + 1        ! Next available record !2
                MAPVAL(NRECORD) = NBASE2     ! Map record entry      !2
                NUMOLD(NRECORD) = NEXTDAT    ! Old record number     !2
                NUMNEW(NEXTDAT) = NRECORD    ! New record number     !2

                NEXTDAT = ICHAR3(DHEAD(6:8)) ! Next data record      !2
              END DO                                                 !2
            END IF                                                   !2
            IPT = IPT + LENTRY               ! Next index entry      !2
          END DO                                                     !2
          NEXTNDX = ICHAR3(RECORD(8:10))     ! Next index record     !2
        END DO
      END DO                                 ! Next index period

!                         Get number of map records for output data set
!             (Iteration is needed as more than 1 map record means more
!          records in data set which may require another map record...)

      NMRECS2 = 1                             ! Guess 1 map record
      NRECS2 = NRECORD + 3                    ! Total number of records
      DO WHILE (NRECS2.GT.NMRECS2*(LENREC/2)) ! Need another map record
        NMRECS2 = NMRECS2 + 1                 ! Add 1 to map records
        NRECS2  = NRECS2  + 1                 ! Add 1 to total records
      END DO

      LASTMAP = 2 + NMRECS2        ! Last map record in ouput data set
      LASTNDX = LASTMAP + NXRECS2  ! Last base index record in output
      ILONG = 1                    ! Reset long bulletin counter     !2

!=======================================================================
!              OUTPUT HEADER, SEQUENCE AND MAP RECORDS
!=======================================================================
!     UPDATE AND COPY HEADER RECORD
!-----------------------------------------------------------------------

      READ (1,REC=1) RECORD(1:LENREC)
      WRITE (RECORD(5:16),'(3A4)') NRECS2, NMRECS2, NXRECS2
      WRITE (3) RECORD(1:LENREC)

!-----------------------------------------------------------------------
!     COPY SEQUENCE RECORD, CONVERTING TO ASCII IF REQUIRED
!-----------------------------------------------------------------------

      READ (1,REC=2) RECORD(1:LENREC)
      IF (ASCII) CALL EB2ASC (LENREC,RECORD(1:LENREC))
      WRITE (3) RECORD(1:LENREC)

!-----------------------------------------------------------------------
!     CREATE AND OUTPUT MAP RECORDS
!-----------------------------------------------------------------------
!                                                 Loop over map records
      J = 0
      DO JREC=1,NMRECS2
!                                      Loop over pointers in map record
        DO IPT=2,LENREC,2
          J = J + 1
          IF (J.LE.LASTNDX .OR. J.GT.NRECS2) THEN
            INEW = 65535
          ELSE
            INEW = MAPVAL(J-LASTMAP) + LASTMAP
          END IF
          RECORD(IPT-1:IPT) = CHAR2(INEW)
        END DO
!                                                     Output map record
        WRITE (3) RECORD(1:LENREC)
      END DO

!=======================================================================
!         COPY INDEX AND DATA RECORDS, UPDATING RECORD NUMBERS
!=======================================================================
!                                              Loop over output records
      DO JREC=1,NRECS2-LASTMAP
!                                          Get number of record to copy
        IOLD = NUMOLD(JREC)

!-----------------------------------------------------------------------
!       MISSING RECORD NUMBER - OUTPUT A RECORD OF ZEROES
!       (This should only occur for base index records with no entries)
!-----------------------------------------------------------------------

        IF (IOLD.EQ.0) THEN
!                                                     Base index record
          IF (JREC.LE.2+NMRECS2+NXRECS2) THEN
            DO J=1,LENREC
              RECORD(J:J) = CHAR(0)
            END DO
!                                                 Not base index record
          ELSE
            WRITE (6,'(/T5,2A,I6/)') 'ARCHIVE ERROR: ',
     &               'Missing record number for output record', JREC
            CALL SYSABN (706)
          END IF

!-----------------------------------------------------------------------
!      RECORD NUMBER NOT MISSING - READ IT AND CHECK FOR DATA OR INDEX.
!      (Index records have the old record number with the sign changed.)
!-----------------------------------------------------------------------

        ELSE
!                                                       Read the record
          I = IABS(IOLD)
          READ (1,REC=I) RECORD(1:LENREC)

!-----------------------------------------------------------------------
!   IF DATA RECORD, UPDATE NUMBER OF NEXT RECORD IN SEQUENCE (IF ANY)
!-----------------------------------------------------------------------

          IF (IOLD.GT.0) THEN            ! Data record
            I = ICHAR3(RECORD(6:8))      ! Next data record (old)    !2
            IF (I.GT.0) THEN                                         !2
              I = NUMNEW(I) + LASTMAP    ! Next data record (new)    !2
              RECORD(6:8) = CHAR3(I)     ! Put number in this record
            END IF                                                   !2

!-----------------------------------------------------------------------
!  Check whether there are any overflow record numbers in this record
!   and convert them to appropriate numbers for the output data set
!-----------------------------------------------------------------------

            DO WHILE (LREC(ILONG).EQ.JREC)                           !2
              J = LBYTE(ILONG)            ! Location of record no.   !2
              I = ICHAR3(RECORD(J:J+2))   ! Record number (old)      !2
              I = NUMNEW(I)               ! Record number (relative) !2
              IF (I.LE.0) THEN                                       !2
                WRITE (6,'(/T5,2A,I6/)') 'ARCHIVE ERROR: ',          !2
     &                   'Invalid overflow data record number ', I   !2
                CALL SYSABN (705)                                    !2
              END IF                                                 !2
              I = I + LASTMAP             ! Record number (absolute) !2
              RECORD(J:J+2) = CHAR3(I)    ! Put in index entry       !2
              ILONG = ILONG + 1           ! Increment counter        !2
            END DO                                                   !2

!-----------------------------------------------------------------------
! IF INDEX RECORD, UPDATE NUMBER OF NEXT INDEX RECORD IN CHAIN (IF ANY)
! AND DATA RECORD NUMBERS. DO EBCDIC-TO-ASCII CONVERSION IF REQUIRED
!-----------------------------------------------------------------------

          ELSE IF (IOLD.LT.0) THEN       ! Index record
            I = ICHAR3(RECORD(8:10))     ! Next index record (old)   !2
            IF (I.GT.0) THEN                                         !2
              I = NUMNEW(I) + LASTMAP    ! Next index record (new)   !2
              RECORD(8:10) = CHAR3(I)    ! Put number in this record
            END IF                                                   !2
!                           Update data record numbers in index entries

            NTRIES = ICHAR2(RECORD(6:7)) ! Number of index entries
            IPT = 10                     ! Bytes before first entry

            DO J=1,NTRIES
              IPT = IPT + LENTRY                   ! End of entry
              IOLD = ICHAR3(RECORD(IPT-6:IPT-4))   ! Old record number
              INEW = NUMNEW(IOLD)                  ! New record (rel.)
              IF (INEW.GT.0) INEW = INEW + LASTMAP ! New record (abs.)
              RECORD(IPT-6:IPT-4) = CHAR3(INEW)    ! Put in index entry
            END DO
!                              Convert identifiers from EBCDIC to ASCII

            IF (ASCII .AND. NXVER.EQ.2) THEN
              IPT = 10                              ! End of header
              DO J=1,NTRIES                         ! Loop over entries
                CALL EB2ASC (8,RECORD(IPT+2:IPT+9)) ! Convert identifier
                IPT = IPT + LENTRY                  ! End of entry
              END DO
            END IF
          END IF
        END IF
!                                       Output new index or data record
        WRITE (3) RECORD(1:LENREC)
      END DO
!                                Close data set, print message and stop
      CLOSE (3)
      WRITE (6,'(T3,A,I6)')
     &         'Number of records in output data set:', NRECS2
      STOP
      END
