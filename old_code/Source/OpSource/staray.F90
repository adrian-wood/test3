SUBROUTINE STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, &
                   NTYPE, NUMSTNS, LOOKWMO, NPOS, NUMWMO)

!-----------------------------------------------------------------------
!
! SUBROUTINE   : STARAY
!
! PURPOSE      : TO READ THE ABBREVIATED STATION LIST AND STORE
!                STATION DETAILS IN ARRAYS SUITABLE FOR LOOKING UP.
!
! DESCRIPTION : "STARAY" READS THE ABBREVIATED STATION LIST AND
!               RETURNS STATION DETAILS IN ARRAYS. AS SOME STATIONS
!               HAVE SEPARATE ENTRIES FOR SURFACE AND UPPER AIR
!               REPORTS, AN ADDITIONAL ARRAY HOLDS A LIST OF WMO
!               NUMBERS WITHOUT DUPLICATES TO BE USED FOR BINARY
!               SEARCHES.  ANOTHER ARRAY STORES INDEXES FOR
!               CORRELATING THIS WITH THE FULL LIST.
!
! USAGE       : CALL STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN,
!                            NTYPE, NUMSTNS, LOOKWMO, NPOS, NUMWMO
!
! ARGUMENTS   : (ALL ARE OUTPUT EXCEPT "NUMSTNS" WHICH IS I&O)
!
!               LISTWMO  (C*4 ARRAY)   WMO NUMBERS OF ALL STATIONS
!               DEGLAT   (REAL ARRAY)  STATION LATITUDES IN DEGREES
!               DEGLON   (REAL ARRAY)  STATION LONGITUDES IN DEGREES
!               HTP      (REAL ARRAY)  STATION PRESSURE SENSOR HEIGHTS
!               HTSTN    (REAL ARRAY)  STATION SURFACE HEIGHTS
!               NTYPE    (INT. ARRAY)  STATION TYPES (SEE NOTE BELOW)
!               NUMSTNS  (I*4) INPUT:  MAXIMUM ALLOWED NO. OF STATIONS
!                             OUTPUT:  ACTUAL NO. OF STATIONS IN LIST
!               LOOKWMO  (C*4 ARRAY)   "LISTWMO" WITHOUT DUPLICATES
!               NPOS     (I*4 ARRAY)   LOCATIONS IN "LISTWMO" OF FIRST
!                                      ENTRY FOR STATIONS IN "LOOKWMO"
!               NUMWMO   (I*4)         NUMBER OF DIFFERENT WMO NOS.
!
!               NOTE: STATION TYPE IS DEFINED BY THE FOLLOWING CODE -
!
!                      1  NOT SPECIFIED (I.E. BLANK IN LIST)
!                      2  SURFACE REPORTS ONLY
!                      3  UPPER AIR REPORTS ONLY
!                      4  SURFACE AND UPPER AIR REPORTS
!                      0  ANYTHING ELSE
!
! CALLED BY   : STAPOS
!
! CALLS TO    : NONE
!
! DATA SETS   : ABBREVIATED STATION LIST DATA SET ON UNIT 81
!
! REVISION INFO :
!
! $Workfile: staray.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 23/12/2010 14:12:00$
!
! CHANGE RECORD :
!
! AN EARLY VERSION OF THIS ROUTINE WAS BY J LEWTHWAITE AND WAS DATED
! 8 MARCH 1996. IT WAS COMPLETELY RE-WRITTEN BY BRIAN BARWELL IN MAY
! 1999 TO ALLOW BINARY SEARCHES FOR WMO STATION NUMBERS.
!
! $Log:
!  3    MetDB_Refresh 1.2         23/12/2010 14:12:00    John Norton     After
!       rework for MDBSTOR port.
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
! $
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

! Use statements:
! <Interfaces>

USE inquire_mod

! <Data Modules>

USE metdb_com_mod, only : MISSIN, RMISS

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(OUT)   :: LISTWMO(*) !a01 ARRAY OF ALL WMO STATION NUMBERS
REAL,         INTENT(OUT)   :: DEGLAT(*)  !a02 LATITUDES OF STATIONS
REAL,         INTENT(OUT)   :: DEGLON(*)  !a03 LONGITUDES OF STATIONS
REAL,         INTENT(OUT)   :: HTP(*)     !a04 PRESSURE SENSOR HEIGHTS OF STATIONS
REAL,         INTENT(OUT)   :: HTSTN(*)   !a05 SURFACE HEIGHTS OF STATIONS
INTEGER,      INTENT(OUT)   :: NTYPE(*)   !a06 STATION TYPES (SEE COMMENTS AT START)
INTEGER,      INTENT(INOUT) :: NUMSTNS    !a07 TOTAL NUMBER IF STATIONS IN LIST
INTEGER,      INTENT(OUT)   :: LOOKWMO(*) !a08 ARRAY OF DIFFERENT WMO STATION NUMBERS
INTEGER,      INTENT(OUT)   :: NPOS(*)    !a09 LOCATION IN LISTWMO OF EACH WMO NO.
INTEGER,      INTENT(OUT)   :: NUMWMO     !a10 NUMBER OF DIFFERENT STATIONS IN LIST

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!----------------------------------------------------------------------
! Declare variables
!----------------------------------------------------------------------

INTEGER          ::  I       ! FOR LOCAL INTERNAL USE
INTEGER          ::  IOS     ! STATUS CODE FROM READ STATEMENT
INTEGER          ::  KODE    ! INTERNAL CODE FOR WARNING MESSAGES
INTEGER          ::  MAXSTNS ! MAXIMUM NUMBER OF STATIONS ALLOWED
INTEGER          ::  LASTWMO ! LAST WMO STATION NUMBER LOOKED AT

CHARACTER(LEN=6)  ::  BLANKS='      ' ! SIX BLANKS
CHARACTER(LEN=57) ::  REPORT ! BUFFER TO HOLD ONE STATION'S DETAILS

#if ! defined (MVS)
INTEGER            :: LEV       !- Length of METDB_STNABRV
INTEGER            :: RC        !- Return code
CHARACTER(LEN=200) :: METDB_STNABRV !- Station list PATH
#endif

LOGICAL          :: exists

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

DO I=1,NUMSTNS
  LISTWMO(I)=MISSIN
  DEGLAT(I)=RMISS
  DEGLON(I)=RMISS
  HTP(I)=RMISS
  HTSTN(I)=RMISS
  NTYPE(I)=MISSIN
  LOOKWMO(I)=MISSIN
  NPOS(I)=MISSIN
END DO
NPOS(NUMSTNS+1)=MISSIN   !Initialise last NPOS

MAXSTNS = NUMSTNS
NUMSTNS = 0
NUMWMO  = 0
LASTWMO = 0
KODE = 0

!-----------------------------------------------------------------------
! Get environment variable METDB_STNABRV and find the length of it.
!-----------------------------------------------------------------------

#if ! defined (MVS)
CALL METDB_GETENV("METDB_STNABRV",METDB_STNABRV,RC)
IF (RC /= 0) THEN
  WRITE(6,*)'STARAY: ERROR: ENV VAR METDB_STNABRV not set'
  RETURN
END IF
LEV=LEN(METDB_STNABRV)
DO WHILE (METDB_STNABRV(LEV:LEV) == ' ')
  LEV=LEV-1
END DO
#endif

!----------------------------------------------------------------------
!     OPEN THE ABBREVIATED STATION LIST.
!----------------------------------------------------------------------

#if defined (MVS)
OPEN(81,FILE='DD:STNABRV',ACTION='READ')
#else
exists = INQUIRE('METDB_STNABRV(1:LEV)','DSN')
IF (.NOT.exists) THEN
  WRITE(6,*)'STARAY: ERROR: File ',METDB_STNABRV(1:LEV), &
            ' not found'
  RETURN
END IF
OPEN(81,FILE=METDB_STNABRV(1:LEV),IOSTAT=RC)
IF (RC /= 0) THEN
  WRITE(6,*)'STARAY: ERROR: Could not open file ', &
                  METDB_STNABRV(1:LEV)
  RETURN
END IF
#endif

!----------------------------------------------------------------------
!     LOOP OVER ENTRIES IN STATION LIST, READING AND DECODING RECORDS
!----------------------------------------------------------------------

DOLABEL1: &
DO WHILE (KODE == 0)
!                                      READ DETAILS FOR NEXT STATION
!
   READ (81, '(A)', IOSTAT=IOS, END=1) REPORT
!
!                                               CHECK FOR READ ERROR
IFLABEL1: &
   IF (IOS /= 0) THEN
      KODE = 2
!                                CHECK FOR BLANK LINE AT END OF LIST
!
   ELSE IF (REPORT(1:6) == BLANKS) THEN
      KODE = 1
!                                        CHECK FOR TOO MANY STATIONS
   ELSE IF (NUMSTNS >= MAXSTNS) THEN
      KODE = 3
!                               EXTRACT STATION DETAILS IF GOOD DATA
!
   ELSE IF (REPORT(2:2) >= '0' .AND. REPORT(2:2) <= '9') THEN
      NUMSTNS = NUMSTNS + 1
!                                                 WMO STATION NUMBER
!                 ("LOOKWMO" IS "LISTWMO" WITHOUT DUPLICATE ENTRIES)
!
      READ (REPORT(2:6),'(I5)') LISTWMO(NUMSTNS)
      IF (LISTWMO(NUMSTNS) /= LASTWMO) THEN
         NUMWMO = NUMWMO + 1
         LOOKWMO(NUMWMO) = LISTWMO(NUMSTNS)
         NPOS(NUMWMO) = NUMSTNS
         LASTWMO = LISTWMO(NUMSTNS)
      END IF
!                                             LATITUDE AND LONGITUDE
!
      READ (REPORT( 8:15),'(F8.3)') DEGLAT(NUMSTNS)
      READ (REPORT(17:24),'(F8.3)') DEGLON(NUMSTNS)
!
!                                             PRESSURE SENSOR HEIGHT
      I = -9999999
      IF (REPORT(28:33) /= BLANKS) READ (REPORT(28:33),'(I6)') I
      HTP(NUMSTNS) = FLOAT(I)
!                                                     STATION HEIGHT
      I = -9999999
      IF (REPORT(36:41) /= BLANKS) READ (REPORT(36:41),'(I6)') I
      HTSTN(NUMSTNS) = FLOAT(I)
!                                                       STATION TYPE
!
IFLABEL2: &
      IF (REPORT(43:43) == ' ') THEN      ! NOT SPECIFIED
         NTYPE(NUMSTNS) = 1
      ELSE IF (REPORT(43:43) == 'U') THEN ! UPPER AIR
         NTYPE(NUMSTNS) = 3
      ELSE IF (REPORT(43:43) == 'S') THEN ! SURFACE ...
         IF (REPORT(50:50) == 'U') THEN   ! ... & UPPER AIR
            NTYPE(NUMSTNS) = 4
         ELSE                             ! ... SURFACE ONLY
            NTYPE(NUMSTNS) = 2
         END IF
      ELSE                                ! DON'T KNOW
         NTYPE(NUMSTNS) = 0
      END IF IFLABEL2
   END IF IFLABEL1
END DO DOLABEL1
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF STATIONS
!----------------------------------------------------------------------
!                                                         CLOSE LIST
    1 CONTINUE
CLOSE (81, STATUS='KEEP')
!                                                     OUTPUT MESSAGE
IFLABEL3: &
IF (KODE <= 1) THEN
   WRITE (6,'(/T5,A,I8,A)') 'STARAY:', NUMSTNS, &
            ' RECORDS READ FROM THE ABBREVIATED STATION LIST.'
ELSE IF (KODE == 2) THEN
   WRITE (6,'(/T5,A,I6,A)') &
            'STARAY:   STATION LIST TRUNCATED AFTER', NUMSTNS, &
            ' STATIONS DUE TO I/O ERROR READING LIST.'
ELSE IF (KODE == 3) THEN
   WRITE (6,'(/T5,A,I6,A)') &
            'STARAY:   STATION LIST TRUNCATED AFTER', NUMSTNS, &
            ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'
END IF IFLABEL3
!                                         RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE STARAY
