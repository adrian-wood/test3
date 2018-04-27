      PROGRAM MDBXCLR

!-----------------------------------------------------------------------
!
! PROGRAM     : MDBXCLR
!
! PURPOSE     : To clear an index record (or a range of index records)
!               in an MDB storage data set and free any corresponding
!               data records by zeroing pointers in map records.
!
! DESCRIPTION : MDBXCLR clears data from an MDB storage data set,
!               replacing the main index records for one or more index
!               periods by empty records and setting to zero any
!               pointers in the map records which correspond to the
!               same period(s). The latter frees data records and
!               overflow index records for re-use.
!
!               The storage data set must be in the new format devised
!               for satellite data in 2000 (see Appendix C of MetDB
!               Technical Note 14). Details of the data set should be
!               given in a "//GO.MDBSTORE DD" statement in the JCL.
!
!               The index period or periods to be cleared are specified
!               in a NAMELIST named 'CLEAR' on unit 5 ("//GO.SYSIN DD").
!
!               To clear a single index period specify IREC1, the
!               record number of the index record for that period and
!               ITIME1, a 5-element array giving the year, month, day,
!               hour and minute of the index time for that record, e.g.
!
!                   &CLEAR IREC1=23, ITIME1=2003,04,16,12,0  &END
!
!               To clear a range of index periods, use IREC1 and ITIME1
!               to specify the first index period and IREC2 and ITIME2
!               similarly to specify the last period, e.g.
!
!                   &CLEAR IREC1=23, ITIME1=2003,04,16,12,0,
!                          IREC2=29, ITIME2=2003,04,16,18,0  &END
!
!               ITIME2 must be later than ITIME1 but IREC2 may be less
!               than IREC1.
!
!               Specifying both record number and index time is just a
!               safeguard against accidentally clearing the wrong data
!               through typing errors.
!
!               Before each index period is cleared its index time is
!               checked to see that it matches what was expected: if
!               there is a mismatch, the period is not cleared. (This
!               means that index periods in a range will not be cleared
!               if they contain data outside the specified time range,
!               either because they still contain older data or because
!               they have already been reused for more recent data.)
!
!               Before clearing data the job does several checks and if
!               any problem is found it prints an error message and
!               abandons processing. If all is well, it prints a
!               one-line message for each index period processed saying
!               whether it was cleared or not.
!
! CALLS       : CLRMAP, DATE31, DATIM, IOBLKS
!
! HISTORY     : Original version written by Brian Barwell, April 2003.
!
! REVISION INFO :
!
! $Revision: 1$ $Date: 28/02/2006 12:07:40$
! $Source: /home/us0400/mdb/op/lib/utility/RCS/mdbxclr.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         28/02/2006 12:07:40    Sheila Needham  
! $
! Revision 1.1  2003/04/14 13:39:21  usmdb
! Initial revision
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                  Parameter statements
!
      INTEGER MAXMAP        ! Maximum number of map blocks supported
      INTEGER MAXREC        ! Maximum data set record length supported
!
      PARAMETER (MAXMAP=20, MAXREC=27998)
!                                                             Variables
!
      INTEGER ICENT1,ICENT2 ! Century minutes for first and last periods
      INTEGER ICENT         ! Century minutes for current index period
      INTEGER IREC1, IREC2  ! First and last index record to clear
      INTEGER IRECS         ! Number of index records to clear
      INTEGER ITIME1(5)     ! Index time for first period (y,m,d,h,m)
      INTEGER ITIME2(5)     ! Index time for last period (y,m,d,h,m)
      INTEGER ITIME(5)      ! Index time for current period (y,m,d,h,m)
      INTEGER J             ! General loop variable
      INTEGER JREC          ! Loop variable for index records
      INTEGER KODE          ! Internally used error code
      INTEGER LENREC        ! Record length of storage data set
      INTEGER LENTRY        ! Length of index entries in storage d/s
      INTEGER MINUTES       ! Minutes from first to last index times
      INTEGER NCENT         ! Century minutes from current index record
      INTEGER NDATA         ! Number of first data record in data set
      INTEGER NINDEX        ! Number of first index record in data set
      INTEGER NRECS         ! Number of records in storage data set
      INTEGER NRMAP(MAXMAP) ! Record from which each map block was read
      INTEGER NOW(8)        ! Current date and time (from DATIM)
      INTEGER NUMAP(MAXMAP) ! Unit from which each map record was read
      INTEGER NUMMAP        ! Number of map blocks in storage data set
      INTEGER NVER          ! Version numbers in header record
      INTEGER NXREC         ! Number of index record being processed
      INTEGER NXRECS        ! No. of index records in storage data set
      INTEGER NXMIN         ! Number of minutes in index period
      INTEGER NX00Z         ! Index period offset from 00Z (minutes)
      INTEGER N100          ! Current year modulo 100
!
      LOGICAL RESET         ! Flag used in call to CLRMAP
!
      CHARACTER*37       ERRTXT         ! Text for error messages
      CHARACTER*(MAXREC) MAPREC(MAXMAP) ! Map records from storage d/s
      CHARACTER*(MAXREC) NDXREC         ! Blank index record
      CHARACTER*5        YMDHM          ! Index time tag  (y,m,d,h,m)
!
!                                                       Data statements
      DATA NUMAP /MAXMAP*0/, NRMAP /MAXMAP*0/
      DATA IREC1, ITIME1, ICENT1 /7*0/,  IREC2, ITIME2, ICENT2 /7*0/
      DATA ERRTXT /'EXECUTION TERMINATING WITH ERROR CODE'/
!                                                              NAMELIST
      NAMELIST /CLEAR/ IREC1, ITIME1, IREC2, ITIME2
!
!                            Common block (for dynamic allocation only)
!
      COMMON /COMREC/ MAPREC, NDXREC

!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETTING UP
!-----------------------------------------------------------------------

      KODE = 0          ! No problems yet
!                                             Set up empty index record
      DO J=1,LENREC
        NDXREC(J:J) = CHAR(0)
      END DO ! J
!                                             Get current date and time
      CALL DATIM (NOW)
      N100 = 100*(NOW(8)/100)  ! Last year divisible by 100

!-----------------------------------------------------------------------
!     READ AND CHECK NAMELIST INPUT ("&CLEAR" ON UNIT 5)
!-----------------------------------------------------------------------

      OPEN  (5)
      READ  (5, CLEAR)
      CLOSE (5)
!                                 Check for start record and index time

      IF (IREC1.LE.0 .OR. ITIME1(1).LE.0) THEN  ! Not specified
        KODE = 1
      ELSE
!                     Get century minute of first index period to clear

        CALL DATE31 (ITIME1(3), ITIME1(2), ITIME1(1), ICENT1)
        ICENT1 = 1440*(ICENT1-1) + 60*ITIME1(4) + ITIME1(5)

!                                        Check for specified end period

        IF (IREC2.GT.0 .OR. ITIME2(1).GT.0) THEN

!                             Both IREC2 and ITIME2 should be specified

          IF (IREC2.LE.0 .OR. ITIME2(1).LE.0) THEN
            KODE = 2
!                      Get century minute of last index period to clear
          ELSE
            CALL DATE31 (ITIME2(3), ITIME2(2), ITIME2(1), ICENT2)
            ICENT2 = 1440*(ICENT2-1) + 60*ITIME2(4) + ITIME2(5)
          END IF
!                        Last = first if no last index period specified
        ELSE
          IREC2  = IREC1
          ICENT2 = ICENT1
        END IF
      END IF

!-----------------------------------------------------------------------
!     OPEN STORAGE DATA SET (UNIT 1) AND READ HEADER RECORD INFORMATION
!-----------------------------------------------------------------------

!                                Find record length of storage data set
      IF (KODE.EQ.0) THEN
        OPEN (1, FILE='MDBSTORE', FORM='UNFORMATTED', ACTION='READ')
        INQUIRE (1, RECL=LENREC)
        IF (LENREC.GT.MAXREC) KODE = 3  ! Records too long
        CLOSE (1)
      END IF
!                             Re-open storage data set as direct access
      IF (KODE.EQ.0) THEN
        OPEN (1, FILE='MDBSTORE', ACCESS='DIRECT', RECL=LENREC)

!                                                    Read header record

        READ (1,REC=1) NVER, NRECS, NUMMAP, NXRECS, NXMIN, NX00Z, LENTRY

!                                                    Check for problems
        IF (NVER.GE.16384) THEN
          KODE = 4                        ! Old storage data set format
        ELSE IF (NUMMAP.GT.MAXMAP) THEN
          KODE = 5                        ! Too many map records
        ELSE
          NINDEX =  NUMMAP + 3            ! First index record
          NDATA = NINDEX + NXRECS         ! First data record

          IF (IREC1.LT.NINDEX .OR. IREC1.GE.NDATA .OR.      ! Record out
     &        IREC2.LT.NINDEX .OR. IREC2.GE.NDATA) KODE = 6 !  of range
        END IF
      END IF

!-----------------------------------------------------------------------
!     COMPUTE NUMBER OF PERIODS TO BE CLEARED AND CHECK TIME RANGE
!-----------------------------------------------------------------------

!                                         No. of index periods to clear
      IF (KODE.EQ.0) THEN
        IRECS = IREC2 - IREC1 + 1
        IF (IRECS.LT.0) IRECS = IRECS + NXRECS
!                                              Check against time range
        MINUTES = NXMIN*(IRECS-1)
        IF (ICENT2-ICENT1.NE.MINUTES) KODE = 7  ! Inconsistent
      END IF

!-----------------------------------------------------------------------
!     READ MAP RECORDS AND STORE IN "MAPREC"
!-----------------------------------------------------------------------

      IF (KODE.EQ.0) THEN
        CALL IOBLKS (-2, 1, LENREC, NUMMAP, 3, NUMAP, NRMAP, MAPREC)

!-----------------------------------------------------------------------
!     LOOP OVER INDEX PERIODS, READING IN BASE INDEX RECORDS
!-----------------------------------------------------------------------

        NXREC = IREC1   ! First index record to clear
        ICENT = ICENT1  ! Century minute of first record
!                                                            Index loop
        DO JREC=1,IRECS
!                                           Read index time from record
          READ (1,REC=NXREC) YMDHM
!                                                     Decode index time
          DO J=1,5
            ITIME(J) = ICHAR(YMDHM(J:J))
          END DO ! J
!                                              Convert year to 4 digits
          ITIME(1) = ITIME(1) + N100
          IF (ITIME(1).GT.NOW(8)) ITIME(1) = ITIME(1) - 100

!                                  Convert to century minutes ("NCENT")

          CALL DATE31 (ITIME(3), ITIME(2), ITIME(1), NCENT)
          NCENT = 1440*(NCENT-1) + 60*ITIME(4) + ITIME(5)

!                                               Check for expected time
          IF (NCENT.NE.ICENT) THEN ! Wrong time
            WRITE (6,'(T3,A,I4,2A,I6, 2("/",I2.2), I3.2,":",I2.2)')
     &               'INDEX RECORD', NXREC, ' NOT CLEARED:  ',
     &               'INDEX TIME IS', ITIME
          ELSE  ! Time OK
!                                             Clear map record pointers
            CALL CLRMAP (NXREC, NDATA, NRECS,
     &                   LENREC, NUMMAP, MAPREC, NUMAP, RESET)

!                                                    Output map records
            IF (RESET) CALL IOBLKS (2, 1, LENREC,
     &                              NUMMAP, 0, NUMAP, NRMAP, MAPREC)

!                                                Blank out index record
            WRITE (1,REC=NXREC) (NDXREC(1:LENREC)
!                                                         Print message

            WRITE (6,'(T3,A,I4,2A,I6, 2("/",I2.2), I3.2,":",I2.2)')
     &               'INDEX RECORD', NXREC, ' CLEARED:      ',
     &               'INDEX TIME IS', ITIME
          END IF
!                             Update variables for next time round loop

          NXREC = NXREC + 1       ! Next base index record
          IF (NXREC.GE.NDATA) NXREC = NXREC - NXRECS
          ICENT = ICENT + NXMIN   ! Century minute of period
        END DO
      END IF

!-----------------------------------------------------------------------
!     MESSAGES FOR NON-ZERO RETURN CODES
!-----------------------------------------------------------------------

      IF (KODE.GT.0) THEN
!                                                          Heading line
        WRITE (6,'(/T3,A,I3)') ERRTXT, KODE
!                                            Some start details omitted
        IF (KODE.EQ.1) THEN
          WRITE (6,'(/T3,A,2I6, 2("/",I2.2), I3.2,":",I2.2)')
     &          'START RECORD AND/OR TIME NOT SPECIFIED:', IREC1, ITIME1

!                                      Incomplete end details specified
        ELSE IF (KODE.EQ.2) THEN
          WRITE (6,'(/T3,A,2I6, 2("/",I2.2), I3.2,":",I2.2)')
     &          'END RECORD AND/OR TIME NOT SPECIFIED:', IREC2, ITIME2

!                                                 Record length too big
        ELSE IF (KODE.EQ.3) THEN
          WRITE (6,'(/T3,A,I9)')
     &          'DATA SET RECORDS TOO LONG FOR "MAPREC" ARRAY:', LENREC

!                                           Old-format storage data set
        ELSE IF (KODE.EQ.4) THEN
          WRITE (6,'(/T3,A,2X,Z8)')
     &          'DATA SET IS IN OLD FORMAT:  VERSIONS (HEX) =', NVER

!                                                  Too many map records
        ELSE IF (KODE.EQ.5) THEN
          WRITE (6,'(/T3,A,I7)')
     &          'TOO MANY MAP RECORDS FOR "MAPREC" ARRAY:', NUMMAP

!                                           Record numbers out of range
        ELSE IF (KODE.EQ.6) THEN
          J = NDATA - 1 ! Last index record
          WRITE (6,'(/T3,A)') 'SPECIFIED INDEX RECORD OUT OF RANGE.'
          WRITE (6,'( T3,A,2I7,A,I3,A,I4)') 'START & END RECORDS:',
     &           IREC1, IREC2, '.   VALID RANGE:', NINDEX, ' TO', J

!                                    Index records & times inconsistent
        ELSE IF (KODE.EQ.7) THEN
          J = ICENT2 - ICENT1
          WRITE (6,'(/T3,A)')'SPECIFIED RECORDS AND TIMES INCONSISTENT.'
          WRITE (6,'( T3,A,2I7,A,I7,A)') 'START & END RECORDS:',
     &           IREC1, IREC2, '.   PERIOD =', MINUTES, ' MINUTES'
          WRITE (6,'( T3,A,2(I6,2("/",I2.2),I3.2,":",I2.2),A,I9,A)')
     &          'START & END TIMES:', ITIME1, ITIME2,
     &          '.   PERIOD =', J, ' MINUTES'
        END IF
      END IF
!                                                            End of job
      STOP
      END
