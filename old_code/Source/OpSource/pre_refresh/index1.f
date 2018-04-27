      SUBROUTINE INDEX1 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  INDEX1
!
! PURPOSE:     To make an index entry for a BUFR message using index
!              format 1.
!
! DESCRIPTION: 'INDEX1' creates the main part of the index entry for
!              a BUFR message in index format 1 (new format developed
!              for satellite data in late 2000).  Only details of
!              time of receipt and location in storage need to be
!              added later.
!
!              The BUFR message is partially decoded (in subroutine
!              GETVALS) to provide some details for the entry.
!
! USAGE:       CALL INDEX1 (MESSAGE, FLAGS, ITEMS, NTIME, ENTRY, KODE)
!
! PARAMETERS:  ('I'=Input, 'O'=Output)
!
!              MESSAGE (I)  (CHARACTER*(*)) BUFR message to be
!                           decoded (starting from "BUFR").
!              FLAGS   (I)  Processing flags (from headers data set).
!              ITEMS   (I)  Processing data (from headers data set).
!              NTIME   (O)  Integer array dimension (6,2) giving
!                           year, month, day, hour, minute and second
!                           of earliest and latest observations.
!              ENTRY   (O)  26-byte index entry, complete except for
!                           time of receipt and location in storage
!                           data set.
!              KODE    (O)  Integer return code as follows:
!
!                              0  Successsful completion
!                             21  VALUES array too small
!                             41  Vital bufr descriptor not found
!                             42  BUFR bulletin failed to decode
!                             44  Couldn't make index entry
!
! CALLED BY:   BUFREP
!
! CALLS:       CHAR2, GETVALS, LATBOX, ONLAND, TRANGE
!
! HISTORY:     Original version by Brian Barwell, October 2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:58$
! $Source: /data/us0400/mdb/op/lib/source/RCS/index1.f,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:58    Sheila Needham  
! $
! Revision 2.2  2002/10/07  16:00:42  16:00:42  usmdb (Generic MetDB account)
! Repositioned lat/long check to prevent very large numbers being
! generated when there was missing lat/long. These large numbers
! caused memory problems in the HPMDB - S.Cox
! 
! Revision 2.1  2001/10/03  15:08:06  15:08:06  usmdb (Generic MetDB account)
! 2.1.  15 October 2001.  Brian Barwell.  Change 127/01.
! Modify calculation of lat/long box to avoid out-of-bounds errors
! on HP.  Also increase CSTR string length from 1 to 6000.
!
! Revision 2.0  2001/06/06  10:19:38  10:19:38  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                            Parameters
      INTEGER    LCSTR        ! Size of CSTR string                 !2.1
      INTEGER    MAXVALS      ! Size of VALUES array
      PARAMETER (LCSTR=6000, MAXVALS=20000)                         !2.1
!                                                             Variables
      INTEGER KODE            ! Return code
      INTEGER ILAT, ILON      ! Pointer to lats & lons in VALUES array
      INTEGER ITEMS(12)       ! Processing data (from headers data set)
      INTEGER IVAL            ! Pointer to VALUES array
      INTEGER J               ! General loop variable
      INTEGER JOB             ! Loop variable for observations
      INTEGER LISTDES(10,2)   ! Lists of descriptors to be decoded
      INTEGER LISTITEM        ! Number of an item in parameter list
      INTEGER MAXLAT          ! Highest latitude of obs in message
      INTEGER MAXLON          ! Highest longitude of obs in message
      INTEGER MINLAT          ! Lowest latitude of obs in message
      INTEGER MINLON          ! Lowest longitude of obs in message
      INTEGER NPOS(10)        ! Location of parameters in BUFR sequence
      INTEGER NSAT            ! Satellite id. for index entry
      INTEGER NTIME(6,2)      ! Yr,mon,day,hr,min,sec of ob. time window
      INTEGER NUMLIST         ! Number of list to be used for decode
      INTEGER NUMOBS          ! Number of observations in BUFR message
!
      REAL BOX(4)             ! Boundaries of lat/long box enclosing obs
      REAL SATID              ! Satellite id. decoded from BUFR message
      REAL VALUES(MAXVALS)    ! Decoded values from BUFR message
!
      LOGICAL FIRST           ! .TRUE. if first call to this subroutine
      LOGICAL FLAGS(9)        ! Processing flags (from headers data set)
      LOGICAL FOUND           ! Flag for parameter location found
      LOGICAL MUSTHAVE(10)    ! Flags for parameters needed for index
      LOGICAL SAME            ! Flag for obs having same satellite id
      LOGICAL SEA             ! Flag for at least one ob. over the sea
!
      CHARACTER*(LCSTR) CSTR  ! Character elements from GETVALS     !2.1
      CHARACTER*26  ENTRY     ! Index entry for BUFR message
      CHARACTER*132 HEAD      ! For revision information
      CHARACTER*(*) MESSAGE   ! BUFR message
!                                                    External functions
!
      LOGICAL     ONLAND      ! Function to set land sea flag
      CHARACTER*2 CHAR2       ! I*4 to C*2 conversion routine
!
!                                 Common block (for dynamic allocation)
      COMMON /COMVAL/ VALUES, CSTR                                  !2.1
!                                                       Saved variables
      SAVE LISTDES, MUSTHAVE, FIRST
!                                                   Data initialisation
      DATA LISTDES
!          SAT  LAT  LON YEAR MNTH  DAY HOUR  MIN  SEC SURF   (LAT/LON)
     &   / 263,1282,1538,1025,1026,1027,1028,1029,1030,2060, !  COARSE
     &     263,1281,1537,1025,1026,1027,1028,1029,1030,2060/ !  HI-RES
      DATA MUSTHAVE /8*.TRUE., 2*.FALSE./
      DATA FIRST /.TRUE./
!                                                  Revision information
      IF (FIRST) THEN
         HEAD='$RCSfile: index1.f,v $ ' //
     &        '$Revision: 1$ $Date: 30/01/2006 20:22:58$'
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!     GET LIST OF WANTED PARAMETERS FROM BUFR MESSAGE
!-----------------------------------------------------------------------
!
!                          Use list with correct lat & long descriptors
      IF (FLAGS(5)) THEN
         NUMLIST = 2      ! High res. lat & long
      ELSE
         NUMLIST = 1      ! Coarse lat & long
      END IF
!                                        Partial decode of BUFR message
!
      CALL GETVALS (LISTDES(1,NUMLIST), 10, MESSAGE, MAXVALS,
     &              VALUES, CSTR, NPOS, NUMOBS, KODE)
!
      IF (KODE.EQ.0) THEN
!
!                     Check that essential parameters have been located
         LISTITEM = 0
         FOUND = .TRUE.
         DO WHILE (FOUND .AND. LISTITEM.LT.10)
            LISTITEM = LISTITEM + 1
            IF (MUSTHAVE(LISTITEM)) FOUND = NPOS(LISTITEM).GT.0
         END DO
!                                  Warning if something vital not found
         IF (.NOT.FOUND) THEN
            WRITE (6,'(T5,A,T15,A,I3,A)') 'INDEX1:', 'Vital descriptor',
     &                LISTITEM, ' not found in BUFR sequence.'
            KODE = 41
!                             Check for expected number of observations
!                                  (but store anyway even if different)
         ELSE
            IF (ITEMS(8).GT.0 .AND. NUMOBS.NE.ITEMS(8))
     &         WRITE (6,'(T5,A,T15,A,I5,A,I5,A)') 'INDEX1:',
     &               'Number of observations in message =', NUMOBS,
     &               ' instead of', ITEMS(8), ' expected.'
!
!-----------------------------------------------------------------------
!     SET LAND/SEA FLAG (SET TO 'SEA' IF AT LEAST ONE SEA POINT).
!     IF SURFACE TYPE IS NOT AVAILABLE, CALL "ONLAND".
!-----------------------------------------------------------------------
!
            ILAT = (NPOS(2)-1)*NUMOBS
            ILON = (NPOS(3)-1)*NUMOBS
            IF (NPOS(10).GT.0) THEN
               IVAL = (NPOS(10)-1)*NUMOBS
            ELSE
               IVAL = -1
            END IF
            SEA = .FALSE.
!                                    Loop over observations in bulletin
            JOB = 1
            DO WHILE (.NOT.SEA .AND. JOB.LE.NUMOBS)
!
!                             Check land/sea flag '008012' if available
!
               IF (IVAL.GE.0 .AND. VALUES(IVAL+JOB).GE.0.0) THEN
                  SEA = NINT(VALUES(IVAL+JOB)).EQ.1
!
!                                 Otherwise check 1-degree land/sea map
!
               ELSE IF (ABS(VALUES(ILAT+JOB)).LE.90.0 .AND.
     &                  ABS(VALUES(ILON+JOB)).LE.180.0) THEN
                  SEA = .NOT.ONLAND(VALUES(ILAT+JOB),VALUES(ILON+JOB))
               END IF
!                                Increment counter for next observation
               JOB = JOB + 1
            END DO
!
!-----------------------------------------------------------------------
!     CHECK THAT ALL OBS ARE FOR THE SAME SATELLITE AND GET ITS ID.
!-----------------------------------------------------------------------
!
            IVAL = (NPOS(1)-1)*NUMOBS
            SATID = VALUES(IVAL+1)   ! First satellite identifier
            SAME = .TRUE.
            JOB = 2
!                                              Check for same satellite
            DO WHILE (SAME .AND. JOB.LE.NUMOBS)
               SAME = VALUES(IVAL+JOB).EQ.SATID
               JOB = JOB + 1
            END DO ! JOB
!                              Set satellite identifier for index entry
!
            IF (SAME .AND. SATID.GT.0.0) THEN
               NSAT = NINT(SATID)
            ELSE
               NSAT = 1023  ! MISSING DATA
               WRITE (6,'(T5,A,T15,A)') 'INDEX1:',
     &              'Satellite identifiers missing or not all the same.'
            END IF
!
!-----------------------------------------------------------------------
!     GET DATE & TIME FOR OLDEST AND MOST RECENT OBSERVATIONS
!     CHECK DATE/TIME FOR MISSING DATA (BIG NEGATIVE VALUE)
!-----------------------------------------------------------------------
!
            CALL TRANGE (VALUES, NUMOBS, NPOS(4), NTIME)

            DO J=1,6
               IF (NTIME(J,1).LT.0 .OR. NTIME(J,2).LT.0) KODE = 44
            END DO ! J
!
!-----------------------------------------------------------------------
!     GET BOUNDARIES OF LAT/LONG BOX ENCLOSING OBSERVATIONS
!     (Calculation done in integers to control round-offs on HP.)   !2.1
!-----------------------------------------------------------------------
!
            CALL LATBOX (VALUES, NUMOBS, NPOS(2), BOX)

            IF (ABS(BOX(1)).GT.90.0 .OR. ABS(BOX(2)).GT.90.0 .OR.   !2.2
     &         ABS(BOX(3)).GT.180.0 .OR. ABS(BOX(4)).GT.180.0) THEN !2.2
              KODE = 44                                             !2.2
            ELSE                                                    !2.2
              MINLAT = (NINT(1.0E5*BOX(1)) +  9000000) / 100000     !2.1
              MAXLAT = (NINT(1.0E5*BOX(2)) +  9099999) / 100000     !2.1
              MINLON = (NINT(1.0E5*BOX(3)) + 18000000) / 200000     !2.1
              MAXLON = (NINT(1.0E5*BOX(4)) + 18199999) / 200000     !2.1
            ENDIF                                                   !2.2
!
!-----------------------------------------------------------------------
!     IF ALL OK, MAKE UP AS MUCH OF THE INDEX ENTRY AS POSSIBLE
!-----------------------------------------------------------------------
!
            IF (KODE.LE.10) THEN
               IVAL = NSAT/256
               IF (SEA) IVAL = IVAL + 128
!
               ENTRY(1:1)   = CHAR(IVAL)          ! Land/sea flag & ..
               ENTRY(2:2)   = CHAR(MOD(NSAT,256)) ! .. Satellite ID
               ENTRY(3:3)   = CHAR(NTIME(4,1))    ! Start hour
               ENTRY(4:4)   = CHAR(NTIME(5,1))    ! Start minute
               ENTRY(5:5)   = CHAR(NTIME(6,1))    ! Start second
               ENTRY(6:6)   = CHAR(NTIME(4,2))    ! End hour
               ENTRY(7:7)   = CHAR(NTIME(5,2))    ! End minute
               ENTRY(8:8)   = CHAR(NTIME(6,2))    ! End second
               ENTRY(9:9)   = CHAR(MINLAT)        ! Minimum latitude
               ENTRY(10:10) = CHAR(MAXLAT)        ! Maximum latitude
               ENTRY(11:11) = CHAR(MINLON)        ! Minimum longitude
               ENTRY(12:12) = CHAR(MAXLON)        ! Maximum longitude
               ENTRY(13:14) = CHAR2(NUMOBS)       ! Number of obs.
               ENTRY(15:15) = CHAR(0)             ! Spare
               ENTRY(16:16) = CHAR(0)             ! Spare
               ENTRY(17:17) = CHAR(0)             ! Spare
!              ENTRY(18:26)   Can't be set yet
            END IF
         END IF
      END IF
!                                             Return to calling program
      RETURN
      END
