      PROGRAM MDBXLIST

!-----------------------------------------------------------------------
!
! PROGRAM     : MDBXLIST
!
! PURPOSE     : To print details of all index entries in an old-format
!               MDB storage data set. (Use MDBXPRNT for new format.)
!
! DESCRIPTION : MDBXLIST prints out all the information in the index
!               entries in an old-format MetDB storage data set.
!
!               The storage data set must be in the old format in use
!               before 2000 (see MetDB Technical Note 2). Details of
!               the data set should be given in a "//GO.MDBSTORE DD"
!               statement in the JCL.
!
!               For each index record (including overflow records)
!               the job prints a one-line heading giving the record
!               number, the number of entries it contains and the
!               index day and time.
!
!               Then it prints a table with the details of all the
!               index entries in the record, one line for each entry.
!               Table headings indicate the meaning of the data.
!               Times of receipt are in minutes after the index time.
!
! WARNING     : Some data sets have large numbers of index entries
!               resulting in a correspondingly large amount of output,
!               especially for data types where each report is stored
!               individually. E.g. running this job on "MDB.AMDARS"
!               produces over a million lines of output.
!
! CALLS       : ICHAR2
!
! REVISION INFO :
!
! $Workfile: mdbxlist.f$ $Folder: UTILITY.SRCE$
! $Revision: 3$ $Date: 08/03/2013 15:56:25$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         08/03/2013 15:56:25    Brian Barwell
!       Remove check for more than 12000 records.
!  2    Met_DB_Project 1.1         02/06/2008 11:40:42    Brian Barwell   Lines
!        added to treat minute correctly for UPRAIR data.
!  1    Met_DB_Project 1.0         25/04/2008 16:15:37    Brian Barwell
!       Utility to print index entries in old-format storage data set.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER ICHAR2      ! CHARACTER*2 to INTEGER*4 conversion function
      INTEGER IDAY        ! Day from index record header
      INTEGER IDLEN       ! Length of station IDs for printout
      INTEGER IHOUR       ! Hour from index record header
      INTEGER IPOS        ! Start byte of lat/long info in index entry
      INTEGER ISTART      ! Start byte number of current index entry
      INTEGER IVER        ! Index version number
      INTEGER J           ! Dogsbody variable for local use
      INTEGER JENTRY      ! Loop variable for index entries
      INTEGER LENREC      ! Record length of storage data set
      INTEGER LENTRY      ! Index entry length
      INTEGER MAXENT      ! Maximum no. of entries a record can hold
      INTEGER MAXLAT      ! Northernmost latitude of lat/long box
      INTEGER MAXLON      ! Easternmost longitude of lat/long box
      INTEGER MINLAT      ! Southernmost latitude of lat/long box
      INTEGER MINLON      ! Westernmost longitude of lat/long box
      INTEGER N           ! Number of base index record
      INTEGER NDREC       ! Data record for current index entry
      INTEGER NFLGS       ! Flags (byte 17 of 23-byte index entry)
      INTEGER NHOUR       ! Hour from index entry
      INTEGER NMIN        ! Minute from index entry
      INTEGER NOBS        ! Number of obs from index entry
      INTEGER NRECS       ! Total number of records in storage data set
      INTEGER NSAT        ! Satellite number in index entry
      INTEGER NSEQ        ! Position of bulletin in data record
      INTEGER NSKIP       ! No. of records before first index record
      INTEGER NTOR        ! Time of receipt (minutes after index time)
      INTEGER NTOTAL      ! Number of index entries left to look at
      INTEGER NTRIES      ! Number of index entries in index record
      INTEGER NUMNDX      ! Total number of index entries in data set
      INTEGER NUMOBS      ! Total number of obs in storage data set
      INTEGER NXHOUR      ! Index period in hours
      INTEGER NXREC       ! Index record number
      INTEGER NXRECS      ! Number of index periods in storage data set
      INTEGER NX00Z       ! Index period offset from 00Z in minutes

      REAL ZLAT, ZLON     ! Latitude and longitude of single observation

      LOGICAL BASE        ! Flag for base index record
      LOGICAL BOX         ! Flag for lat/lon box info in index entries
      LOGICAL MISSING     ! Flag for missing lat/long

      CHARACTER*16    HEAD1   ! First part of column headings
      CHARACTER*20    HEAD2   ! Middle part of headings (satellite)
      CHARACTER*30    HEAD3   ! Middle part of headings (non-satellite)
      CHARACTER*33    HEAD4   ! Last part of column headings
      CHARACTER*8     ID      ! Station identifier for printout
      CHARACTER*27998 RECORD  ! Record read from storage data set
      CHARACTER*5     SEA     ! 'SEA' if land/sea flag is set in entry
      CHARACTER*130   TEXT    ! Line of data for printed output
      CHARACTER*4     Z8000   ! Hex '80008000' (missing lat/long)

      DATA Z8000 /Z80008000/
!                                          Set text for column headings
      HEAD1 = ' NO.  DATA  SEQ.'
      HEAD2 = '  SAT   NOBS  HH:MM '
      HEAD3 = '  STATION   NOBS  HH:MM  FLAGS'
      HEAD4 = '  LATITUDES  LONGITUDES  TOR  SEA'
!                                                   Initialise counters
      NUMOBS = 0          ! No obs found yet
      NUMNDX = 0          ! No index records read yet

!-----------------------------------------------------------------------
!     OPEN STORAGE DATA SET AND READ HEADER RECORD INFORMATION.
!-----------------------------------------------------------------------
!                                Find record length of storage data set

      OPEN (1, FILE='MDBSTORE', FORM='UNFORMATTED', ACTION='READ')
      INQUIRE (1, RECL=LENREC)
      CLOSE (1)
!                             Re-open storage data set as direct access

      OPEN (1, FILE='MDBSTORE', ACCESS='DIRECT', RECL=LENREC,
     &      ACTION='READ')
!                                            Read and decode map record
      READ (1,REC=1) RECORD(1:LENREC)
      NRECS  = ICHAR2(RECORD(1:2))    ! Total no. of records
      NXRECS = ICHAR2(RECORD(3:4))    ! No. of index records
      NXHOUR = ICHAR2(RECORD(5:6))    ! Index period (hours)
      NX00Z  = ICHAR2(RECORD(7:8))    ! Offset from 00Z (hrs.)

!                                                  Check for old format
      IF (NRECS.EQ.0) THEN
        WRITE (6,'(/(T3,A/))') 'STORAGE DATA SET IS IN NEW FORMAT',
     &           'USE "MDBXPRNT" TO LIST INDEX ENTRY DETAILS'
        STOP
      END IF
!                                              Check index entry length
      LENTRY = ICHAR(RECORD(LENREC:LENREC))

      IF (LENTRY.EQ.12) THEN          ! 12 bytes: satellite data
        IVER = 1
        IDLEN = 6
        BOX = .TRUE.
      ELSE IF (LENTRY.EQ.23) THEN     ! 23 bytes: non-satellite data
        IVER = 2
        IDLEN = 11
        BOX = .FALSE.  ! (may be changed later)
      ELSE
        WRITE (6,'(/T3,A,I5/)') 'Invalid index entry length:', LENTRY
        STOP
      END IF
!                                 Compute max. index entries per record
      MAXENT = (LENREC-8)/LENTRY
!                                        Check for BUFR sequence record
      J = NRECS + 8
      IF (ICHAR(RECORD(J:J)).EQ.2) THEN  ! Sequence record present
        NSKIP = 2
        READ (1,REC=2) TEXT
!                                         Reset BOX for some data types
        IF (INDEX(TEXT,'PAOBS')  .GT.0 .OR.
     &      INDEX(TEXT,'309192') .GT.0 .OR.  ! (SATOBS)
     &      INDEX(TEXT,'SATOB')  .GT.0 .OR.  ! (MERGED SATOBS)
     &      INDEX(TEXT,'SALTSSH').GT.0 .OR.
     &      INDEX(TEXT,'SFERICS').GT.0 .OR.
     &      INDEX(TEXT,'SFLOC')  .GT.0) BOX = .TRUE.

      ELSE                               ! No sequence record
        NSKIP = 1
      END IF

!-----------------------------------------------------------------------
!     LOOP OVER INDEX RECORDS PRINTING OUT ENTRIES.
!-----------------------------------------------------------------------
!                                          Loop over base index records
      DO N=1,NXRECS
        NXREC = NSKIP + N     ! Base index record
        BASE =.TRUE.
!                                  Loop over all index records in chain
        DO WHILE (NXREC.GT.0)
          READ (1,REC=NXREC) RECORD(1:LENREC)  ! Read index record
          IF (BASE) THEN
            NTOTAL = ICHAR2(RECORD(3:4))  ! Entries in whole chain
          END IF
          NTRIES = MIN0(NTOTAL,MAXENT)    ! Entries in this record

          IF (NTRIES.GT.0) THEN
!                                                        Get index time
            IDAY  = ICHAR(RECORD(1:1))
            IHOUR = ICHAR(RECORD(2:2))
!                                                     Print header line

            WRITE (6,'(/T2,A,I6,A,I5,A,I4.2,"/",I2.2)')
     &               'INDEX RECORD', NXREC, '  WITH', NTRIES,
     &               '  ENTRIES.  INDEX DAY/HOUR:', IDAY, IHOUR

!                                                  Print table headings
            IF (IVER.EQ.1) THEN
              WRITE (6,'(/T5,3A)') HEAD1, HEAD2, HEAD4
            ELSE
              WRITE (6,'(/T5,3A)') HEAD1, HEAD3, HEAD4
            END IF
          END IF

!-----------------------------------------------------------------------
!         LOOP OVER INDEX ENTRIES IN CURRENT INDEX RECORD.
!-----------------------------------------------------------------------
!                                          First entry starts at byte 7
          ISTART = 7
!                                           Loop over all index entries
          DO JENTRY=1,NTRIES
!                                                         Land/sea flag
            J = ICHAR(RECORD(ISTART:ISTART))
            SEA = ' '
            IF (J.GE.128) SEA = '  SEA'
!                                         Extract data from index entry

            IF (IVER.EQ.1) THEN                         ! 12-byte entry
              J = ICHAR2(RECORD(ISTART:ISTART+1))          ! Bytes 1-2
              NSAT  = MOD(J/32,1024)                       ! Satellite
              NHOUR = MOD(J,32)                            ! Rel. hour
              J = ICHAR2(RECORD(ISTART+2:ISTART+3))        ! Bytes 3-4
              NMIN  = J/1024                               ! Minute
              NOBS  = MOD(J,1024)                          ! No. of obs.
              NTOR  = 3*ICHAR(RECORD(ISTART+8:ISTART+8))   ! Rcpt. delay
              NSEQ  = ICHAR(RECORD(ISTART+9:ISTART+9))     ! Seq. no.
              NDREC = ICHAR2(RECORD(ISTART+10:ISTART+11))  ! Data record
              IPOS = ISTART + 4

            ELSE                                        ! 23-byte entry
              NHOUR = MOD(ICHAR(RECORD(ISTART:ISTART)),64) ! Rel. hour
              NMIN  = ICHAR(RECORD(ISTART+1:ISTART+1))     ! Minute
              NOBS  = ICHAR(RECORD(ISTART+11:ISTART+11))   ! No. of obs.
              ID    = RECORD(ISTART+2:ISTART+9)            ! Station ID
              NFLGS = ICHAR(RECORD(ISTART+16:ISTART+16))   ! Flags
              NTOR  = ICHAR2(RECORD(ISTART+17:ISTART+18))  ! Rcpt. delay
              NSEQ  = ICHAR2(RECORD(ISTART+19:ISTART+20))  ! Seq. no.
              NDREC = ICHAR2(RECORD(ISTART+21:ISTART+22))  ! Data record
              IPOS = ISTART + 12
            END IF

            IF (NMIN.NE.255 .AND. NMIN.GT.60)
     &          NMIN = MOD(NMIN,64)                        ! For UPRAIR
            IF (NDREC.EQ.0 .AND. NSEQ.EQ.0) GO TO 99       ! For SALTSSH

            NHOUR = MOD(IHOUR+NHOUR,24)      ! Absolute hour
            NDREC = NDREC + NXRECS + NSKIP   ! Absolute record number

!                                                      Lat/Long details
            MISSING = RECORD(IPOS:IPOS+3) .EQ. Z8000
!
            IF (.NOT.MISSING) THEN
!                                                  ! Single lat & long
              IF (.NOT.BOX .OR. (IVER.EQ.2 .AND. NOBS.EQ.1)) THEN
                J = ICHAR2(RECORD(IPOS:IPOS+1))
                IF (J.GT.32768) J = J - 65536
                ZLAT = 0.01*FLOAT(J)               ! Latitude
                J = ICHAR2(RECORD(IPOS+2:IPOS+3))
                IF (J.GT.32768) J = J - 65536
                ZLON = 0.01*FLOAT(J)               ! Longitude

              ELSE                                 ! Lat/long box
                MINLAT = ICHAR(RECORD(IPOS  :IPOS  )) - 90
                MAXLAT = ICHAR(RECORD(IPOS+1:IPOS+1)) - 90
                MINLON = 2*ICHAR(RECORD(IPOS+2:IPOS+2)) - 180
                MAXLON = 2*ICHAR(RECORD(IPOS+3:IPOS+3)) - 180
              END IF
            END IF

!-----------------------------------------------------------------------
!           PRINT DETAILS OF INDEX ENTRY
!-----------------------------------------------------------------------

            TEXT = ' '
!                                                 12-byte index entries
            IF (IVER.EQ.1) THEN
              WRITE (TEXT,'(T3,I5,I7,I5,I6,I7, I4.2,":",I2.2, I7,I4,
     &               I7,I5,I6,A)') JENTRY, NDREC, NSEQ, NSAT, NOBS,
     &               NHOUR, NMIN, MINLAT, MAXLAT, MINLON, MAXLON,
     &               NTOR, SEA
!                                       23-byte entries with lat & long

            ELSE IF (NOBS.EQ.1 .OR. .NOT.BOX) THEN
              WRITE (TEXT,'(T3,I5,I7,I5,3X,A,I6, I4.2,":",I2.2, I6,
     &               2F11.2,I7,A)') JENTRY, NDREC, NSEQ, ID, NOBS,
     &               NHOUR, NMIN, NFLGS, ZLAT, ZLON, NTOR, SEA
              IF (NMIN.EQ.255) TEXT(42:43) = '--' ! (UPRAIR)

!                                     23-byte entries with lat/long box
            ELSE
              WRITE (TEXT,'(T3,I5,I7,I5,3X,A,I6, I4.2,":",I2.2, I6,
     &               I7,I4,I7,I5,I6,A)') JENTRY, NDREC, NSEQ, ID, NOBS,
     &               NHOUR, NMIN, NFLGS, MINLAT, MAXLAT, MINLON,
     &               MAXLON, NTOR, SEA
            END IF
!                                            Check for missing lat/long

            IF (MISSING) TEXT(53:73) = ' missing     missing '

!                                                  Output line of table
            WRITE (6,'(A)') TEXT
!                                     Update observation & index counts
            NUMOBS = NUMOBS + NOBS
   99       NUMNDX = NUMNDX + 1
            ISTART = ISTART + LENTRY
          END DO
!                                            Next index record in chain
          IF (NTRIES.LT.MAXENT) THEN ! None
            NXREC = 0
          ELSE
            NXREC = ICHAR2(RECORD(ISTART:ISTART+1)) + NSKIP - 1
          END IF
          NTOTAL = NTOTAL - NTRIES  ! Entries remaining
          BASE =.FALSE.
        END DO
      END DO
!                                       Print observation total and end

      WRITE (6,'(/T2,I12,A,I7,A)') NUMOBS, ' OBSERVATIONS AND',
     &                        NUMNDX, ' INDEX ENTRIES IN DATA SET'
      CLOSE (1)
      STOP
      END
