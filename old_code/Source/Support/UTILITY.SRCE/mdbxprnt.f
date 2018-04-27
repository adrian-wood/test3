      PROGRAM MDBXPRNT

!-----------------------------------------------------------------------
!
! PROGRAM     : MDBXPRNT
!
! PURPOSE     : To print details of all index entries in an MDB storage
!               data set.
!
! DESCRIPTION : MDBXPRNT prints out all the information contained in
!               the index entries in an MDB storage data set.
!
!               The storage data set must be in the new format devised
!               for satellite data in 2000 (see Appendix C of MetDB
!               Technical Note 14). Details of the data set should be
!               given in a "//GO.MDBSTORE DD" statement in the JCL.
!
!               For each index record (including overflow records) the
!               job prints a one-line heading giving the record number,
!               the number of entries it contains and the index date
!               and time.
!
!               Then it prints a table with the details of all the
!               index entries in the record, one line for each entry.
!               Table headings indicate the meaning of the data.
!               Times of receipt are in minutes after the index time.
!
! WARNING     : Some data sets have large numbers of index entries
!               resulting in a correspondingly large amount of output.
!               E.g. "MDB.SSMI" produces about 330,000 lines of output.
!
! CALLS       : DATIM
!
! HISTORY     : Original version written by Brian Barwell, April 2003.
!
! REVISION INFO :
!
! $Workfile: mdbxprnt.f$ $Folder: UTILITY.SRCE$
! $Revision: 2$ $Date: 14/04/2010 16:15:16$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         14/04/2010 16:15:16    Brian Barwell   Print
!        out correct lengths for large bulletins (i.e. >27990 bytes).
!  1    Met_DB_Project 1.0         28/02/2006 12:07:41    Sheila Needham  
! $
! Revision 1.2  2003/09/04 09:42:32  usmdb
! 1.2.  4 September 2003.  Brian Barwell.  Change 92/03.
! Correct bug in conversion of index date to 4-digit year.
!
! Revision 1.1  2003/04/14  13:39:56  13:39:56  usmdb (MetDB account c/o usjh)
! Initial revision
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
      INTEGER IDHEAD      ! Length of data record header             !2
      PARAMETER (IDHEAD=8)                                           !2

      INTEGER ICHAR2      ! CHARACTER*2 to INTEGER*4 conversion function
      INTEGER ICHAR3      ! CHARACTER*3 to INTEGER*4 conversion function
      INTEGER IDLEN       ! Length of station IDs for printout
      INTEGER IEND        ! End byte number of current index entry
      INTEGER ISTART      ! Start byte number of current index entry
      INTEGER ITIME(5)    ! Yr, mon, day, hr, min from index record
      INTEGER ITOR        ! Time of receipt (minutes after index time)
      INTEGER IVER        ! Index version number
      INTEGER J           ! Dogsbody variable for local use
      INTEGER JENTRY      ! Loop variable for index entries
      INTEGER LENREC      ! Record length of storage data set
      INTEGER LENTRY      ! Index entry length
      INTEGER LMESG       ! Length of message in data record
      INTEGER MAXLAT      ! Northernmost latitude of lat/long box
      INTEGER MAXLON      ! Easternmost longitude of lat/long box
      INTEGER MINLAT      ! Southernmost latitude of lat/long box
      INTEGER MINLON      ! Westernmost longitude of lat/long box
      INTEGER N           ! Number of base index record
      INTEGER NBYTE       ! Start byte of index entry in data record
      INTEGER NCENT       ! Most recent year divisible by 100
      INTEGER NDREC       ! Data record for current index entry
      INTEGER NMRECS      ! Number of map records in storage data set
      INTEGER NOBS        ! Number of obs from index entry
      INTEGER NOVFL       ! Number of overflow data records          !2
      INTEGER NOW(8)      ! Current date and time from DATIM
      INTEGER NRECS       ! Total number of records in storage data set
      INTEGER NT(6)       ! Earliest & latest times in entry (hh,mm,ss)
      INTEGER NTRIES      ! Number of index entries in index record
      INTEGER NUMNDX      ! Total number of index entries in data set
      INTEGER NUMOBS      ! Total number of obs in storage data set
      INTEGER NVER        ! Data set and index version numbers
      INTEGER NXMIN       ! Index period in minutes
      INTEGER NXREC       ! Index record number
      INTEGER NXRECS      ! Number of index periods in storage data set
      INTEGER NX00Z       ! Index period offset from 00Z in minutes

      REAL ZLAT, ZLON     ! Latitude and longitude of single observation

      CHARACTER*4     DATSEL  ! Data selection parameter for printout
      CHARACTER*26    HEAD1   ! First part of column headings        !2
      CHARACTER*65    HEAD2   ! Last part of column headings         !2
      CHARACTER*11    ID      ! Station identifier for printout
      CHARACTER*27998 RECORD  ! Record read from storage data set
      CHARACTER*5     SEA     ! 'SEA' if land/sea flag is set in entry

      HEAD1 = ' NO.  DATA  START  LENGTH '                           !2
      HEAD2 = '  NOBS  EARLIEST   LATEST  ' //                       !2
     &        '  LATITUDES  LONGITUDES  TOR  SEA SEL.'               !2

      NUMOBS = 0                ! No obs found yet
      NUMNDX = 0                ! No index records read yet
      CALL DATIM (NOW)          ! Current date and time
      NCENT = 100*(NOW(8)/100)  ! Last year divisible by 100

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
!                                                    Read header record

      READ (1,REC=1) NVER, NRECS, NMRECS, NXRECS, NXMIN, NX00Z, LENTRY
      IVER = MOD(NVER,256)
!                                 Set length of identifier for printout
      IF (IVER.EQ.1) THEN
        IDLEN = 6               ! Satellite ID
      ELSE IF (IVER.EQ.2) THEN
        IDLEN = 11              ! Character ID
      ELSE
        WRITE (6,'(/T2,A,I4)') 'Unknown index format version:', IVER
        STOP
      END IF

!-----------------------------------------------------------------------
!     LOOP OVER INDEX RECORDS PRINTING OUT ENTRIES.
!-----------------------------------------------------------------------

      NXREC = NMRECS + 2        ! Records before first index

!                                          Loop over base index records
      DO N=1,NXRECS
        NXREC = NMRECS + 2 + N  ! Base index record

!                                  Loop over all index records in chain
        DO WHILE (NXREC.GT.0)
          READ (1,REC=NXREC) RECORD(1:LENREC)  ! Read index record
          NTRIES = ICHAR2(RECORD(6:7))         ! Number of entries

          IF (NTRIES.GT.0) THEN
!                                                        Get index time
            DO J=1,5
              ITIME(J) = ICHAR(RECORD(J:J))
            END DO
!                                              Convert year to 4 digits
            ITIME(1) = ITIME(1) + NCENT                             !1.2
            IF (ITIME(1).GT.NOW(8)) ITIME(1) = ITIME(1) - 100       !1.2

!                                                     Print header line

            WRITE (6,'(/T2,A,I6,A,I5,A,I6,2("/",I2.2),I3.2,":",I2.2)')
     &               'INDEX RECORD', NXREC, '  WITH', NTRIES,
     &               '  ENTRIES.  INDEX TIME:', ITIME
!                                                  Print table headings
            IF (IVER.EQ.1) THEN
              WRITE (6,'(/T5,3A)') HEAD1, '  SAT', HEAD2
            ELSE
              WRITE (6,'(/T5,3A)') HEAD1, '  STATION ', HEAD2
            END IF
          END IF

!-----------------------------------------------------------------------
!         LOOP OVER INDEX ENTRIES IN CURRENT INDEX RECORD.
!-----------------------------------------------------------------------

          IEND = 10    ! Bytes before first entry

!                                           Loop over all index entries
          DO JENTRY=1,NTRIES
            ISTART = IEND + 1     ! Start of entry
            IEND = IEND + LENTRY  ! End of entry
!                                                         Land/sea flag
            J = ICHAR(RECORD(ISTART:ISTART))
            SEA = ' '
            IF (J.GE.128) SEA = '  SEA'
!                                                    Station identifier
            IF (IVER.EQ.1) THEN
              J = ICHAR2(RECORD(ISTART:ISTART+1))
              J = MOD(J,1024)
              WRITE (ID(1:6),'(I6)') J       ! Satellite ID
            ELSE
              ID(1:3) = ' '
              ID(4:11) = RECORD(ISTART+1:ISTART+8) ! Character ID
            END IF
!                                        Earliest and latest data times
            DO J=1,6
              NT(J) = ICHAR(RECORD(IEND-24+J:IEND-24+J))
            END DO
!                                                Number of observations
            NOBS = ICHAR2(RECORD(IEND-13:IEND-12))
!                                                        Lat/Long range
            IF (IVER.EQ.1 .OR. NOBS.GT.1) THEN  ! Box
              MINLAT = ICHAR(RECORD(IEND-17:IEND-17)) - 90
              MAXLAT = ICHAR(RECORD(IEND-16:IEND-16)) - 90
              MINLON = 2*ICHAR(RECORD(IEND-15:IEND-15)) - 180
              MAXLON = 2*ICHAR(RECORD(IEND-14:IEND-14)) - 180

            ELSE  ! Single lat & long
              J = ICHAR2(RECORD(IEND-17:IEND-16))  ! Latitude
              IF (J.GT.32768) J = J - 65536
              ZLAT = 0.01*FLOAT(J)
              J = ICHAR2(RECORD(IEND-15:IEND-14))  ! Longitude
              IF (J.GT.32768) J = J - 65536
              ZLON = 0.01*FLOAT(J)
            END IF
!                                              Data selection parameter
            J = ICHAR(RECORD(IEND-11:IEND-11))
            DATSEL = ' '
            IF (J.GT.0) WRITE (DATSEL,'(I4)') J
!                                                            Other data

            ITOR  = ICHAR2(RECORD(IEND-8:IEND-7))  ! Time of receipt
            NDREC = ICHAR3(RECORD(IEND-6:IEND-4))  ! Data record
            NBYTE = ICHAR2(RECORD(IEND-3:IEND-2))  ! Start byte
            LMESG = ICHAR2(RECORD(IEND-1:IEND))    ! Message length
            NOVFL = ICHAR(RECORD(IEND-9:IEND-9))   ! Overflow recs.  !2
            IF (NOVFL.GT.0) LMESG = LMESG + NOVFL*(LENREC-IDHEAD)    !2

!-----------------------------------------------------------------------
!           PRINT DETAILS OF INDEX ENTRY
!-----------------------------------------------------------------------

            IF (IVER.EQ.1 .OR. NOBS.GT.1) THEN
              WRITE (6,'(T3,I5,2I7,I8,A,I6,2(I4.2,2(":",I2.2)),
     &                  I6,I5,I7,I5,I6,2A)')                 !2 (above)
     &          JENTRY, NDREC, NBYTE, LMESG, ID(1:IDLEN), NOBS, NT,
     &          MINLAT, MAXLAT, MINLON, MAXLON, ITOR, SEA, DATSEL
            ELSE
              WRITE (6,'(T3,I5,2I7,I8,A,I6,2(I4.2,2(":",I2.2)),
     &                   F10.2,F12.2,I7,2A)')                !2 (above)
     &          JENTRY, NDREC, NBYTE, LMESG, ID(1:IDLEN), NOBS, NT,
     &          ZLAT, ZLON, ITOR, SEA, DATSEL
            END IF
!                                     Update observation & index counts
            NUMNDX = NUMNDX + 1
            NUMOBS = NUMOBS + NOBS
          END DO
!                                            Next index record in chain
          NXREC = ICHAR3(RECORD(8:10))
        END DO
      END DO
!                                       Print observation total and end

      WRITE (6,'(/T2,I12,A,I7,A)') NUMOBS, ' OBSERVATIONS AND',
     &                        NUMNDX, ' INDEX ENTRIES IN DATA SET'
      CLOSE (1)
      STOP
      END
