SUBROUTINE STAPOS (REQ_WMONO, TYPE, LAT, LONG, HGPT, HGT, ISTAT)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : STAPOS
!
! PURPOSE     : TO RETURN STATION DETAILS FOR A GIVEN WMO STATION
!               NUMBER AND STATION TYPE.
!
! DESCRIPTION : A BINARY SEARCH IS MADE OF WMO STATION NUMBERS IN
!               THE ABBREVIATED STATION LIST FOR A GIVEN STATION.
!               DETAILS FOR AN ENTRY FOR WITH THE REQUIRED STATION
!               TYPE ARE LOCATED IF PRESENT AND ARE RETURNED TO THE
!               CALLING PROGRAM. A RETURN CODE IS SET > 0 IF DETAILS
!               COULD NOT BE FOUND.
!
! USAGE       : CALL STAPOS
!                      (REQ_WMONO, TYPE, LAT, LONG, HGPT, HGT, ISTAT)
!
! ARGUMENTS   :   NAME    I/O  TYPE      CONTENT
!                 ----    ---  ----      -------
!               REQ_WMONO  I  (I*4) TARGET WMO STATION NUMBER
!               TYPE       I  (C*(*)) TARGET STATION TYPE (SEE NOTE 1)
!               LAT        O  (R*4) LATITUDE OF TARGET STATION
!               LON        O  (R*4) LONGITUDE OF TARGET STATION
!               HGPT       O  (R*4) P. SENSOR HEIGHT OF TARGET STN.
!               HGT        O  (R*4) SURFACE HEIGHT OF TARGET STATION
!               ISTAT      O  (I*4) RETURN CODE (SEE NOTE 2)
!
!               NOTES: (1) STATION TYPE IS CODED AS A TEXT STRING
!                          STARTING WITH "S", "U" OR "X" WHERE
!                          S=SURFACE, U=UPPER AIR, X=UNSPECIFIED.
!
!                      (2) RETURN CODE IS 0 FOR SUCCESSFUL SEARCH, 8
!                          FOR STATION NOT FOUND, OR 16 (WITH MESSAGE)
!                          FOR BAD CODING OF "REQ_MONO" OR "TYPE".
!
! CALLED BY   : USER'S PROGRAM
!
! CALLS       : STARAY, ISRCH
!
! REVISION INFO :
! $Workfile: stapos.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/12/2010 16:11:36$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 16:11:36    Alison Weir     Update
!        'CALLS' in header comments
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
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
USE isrch_mod
USE staray_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)  :: REQ_WMONO ! TARGET WMO STATION NUMBER
CHARACTER(LEN=*), INTENT(IN)  :: TYPE      ! TARGET STATION TYPE
REAL,             INTENT(OUT) :: LAT       ! LAT OF TARGET STATION
REAL,             INTENT(OUT) :: LONG      ! LONG OF TARGET STATION
REAL,             INTENT(OUT) :: HGPT      ! SENSOR HEIGHT
REAL,             INTENT(OUT) :: HGT       ! STATION HEIGHT
INTEGER,          INTENT(OUT) :: ISTAT     ! ERROR CODE RETURNED TO USER

! Local declarations:

!----------------------------------------------------------------------
!    (IF IT IS NECESSARY TO INCREASE THE NUMBER OF STATIONS WHICH CAN
!     BE HANDLED, ALTER THE NUMBER ON THE PARAMETER STATEMENT BELOW.
!     NO OTHER CHANGES TO THIS OR ANY OTHER ROUTINE WILL BE REQUIRED.)
!---------------------------------------------------------------------
!
INTEGER, PARAMETER :: MAXSTNS =14000      ! MAX NUMBER OF STATIONS
INTEGER, PARAMETER :: MAXPLUS1=MAXSTNS+1  ! MAX NUMBER OF STATIONS + 1
!
INTEGER      :: IPOS             ! INTERNAL POINTER TO ARRAYS
INTEGER      :: ITYPE            ! CODED "TYPE" (X,S,U = 1,2,3)
INTEGER      :: JPOS             ! LOOP VARIABLE
INTEGER      :: J1               ! LIMITS FOR SEARCH
INTEGER      :: J2               ! LIMITS FOR SEARCH
INTEGER      :: NUMSTNS          ! NO. OF STATIONS FOUND IN LIST
INTEGER      :: NUMWMO           ! NO. OF DIFFERENT STATIONS IN LIST
INTEGER      :: LISTWMO(MAXSTNS) ! WMO STATION NUMBERS IN LIST
INTEGER      :: LOOKWMO(MAXSTNS) ! LIST OF DIFFERENT WMO NUMBERS
INTEGER      :: NPOS(MAXPLUS1)   ! LOOKWMO/LISTWMO COLLATING POINTERS
INTEGER      :: NTYPE(MAXSTNS)   ! STATION TYPES (SEE ABOVE)

REAL         :: DEGLAT(MAXSTNS)  ! LATITUDES OF STATIONS
REAL         :: DEGLON(MAXSTNS)  ! LONGITUDES OF STATIONS
REAL         :: HTP(MAXSTNS)     ! PRESSURE SENSOR HEIGHTS OF STNS.
REAL         :: HTSTN(MAXSTNS)   ! SURFACE HEIGHTS OF STATIONS

LOGICAL      :: FIRST=.TRUE.        ! FLAG FOR FIRST CALL TO STAPOS

!  COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
COMMON /COMSTNS/ LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, NTYPE, &
                 LOOKWMO, NPOS
!  SAVED VARIABLES
SAVE /COMSTNS/, NUMSTNS, NUMWMO, FIRST

!
!----------------------------------------------------------------------
!     IF THIS IS THE FIRST CALL TO STAPOS, READ THE ABBREVIATED
!     STATION LIST AND STORE DETAILS IN ARRAYS.
!----------------------------------------------------------------------
!
IF (FIRST) THEN
   NUMSTNS = MAXSTNS
   CALL STARAY (LISTWMO, DEGLAT, DEGLON, HTP, HTSTN, NTYPE, &
                NUMSTNS, LOOKWMO, NPOS, NUMWMO)
   NPOS(NUMWMO+1) = NUMSTNS + 1
   FIRST = .FALSE.
END IF
!
!----------------------------------------------------------------------
!     CHECK FOR VALID ICAO NUMBER AND REQUIRED STATION TYPE.
!----------------------------------------------------------------------
!
IFLABEL1: &
IF (REQ_WMONO < 01001 .OR. REQ_WMONO > 99999) THEN
   WRITE (6,'(T5,2A,I8)') 'STAPOS:   REQUESTED WMO STATION ', &
                  'NUMBER OUT OF RANGE -', REQ_WMONO
   ISTAT = 16
ELSE
   ITYPE = INDEX('XSU',TYPE(1:1))
IFLABEL2: &
   IF (ITYPE == 0) THEN
      WRITE (6,'(T5,5A)') 'STAPOS:   STATION TYPE ', &
               'DOES NOT BEGIN "S", "U" OR "X". ',   &
               'VALUE IS "', TYPE, '"'
      ISTAT = 16
!
!----------------------------------------------------------------------
!     LOOK UP REQUESTED WMO STATION NUMBER IN LOOK-UP TABLE.
!----------------------------------------------------------------------
!
   ELSE
      ISTAT = 8 ! MATCH NOT YET FOUND
      CALL ISRCH (REQ_WMONO, LOOKWMO, NUMWMO, IPOS)
!
!----------------------------------------------------------------------
!     IF FOUND, LOOK FOR REQUIRED STATION TYPE IN ORIGINAL LIST
!----------------------------------------------------------------------
!
      IF (IPOS > 0) THEN
         J1 = NPOS(IPOS)
         J2 = NPOS(IPOS+1) - 1
         DO JPOS=J1,J2
            IF (NTYPE(JPOS) == ITYPE .OR. &
               (NTYPE(JPOS) == 4 .AND. ITYPE > 1)) THEN
               ISTAT = 0 ! MATCH FOUND
               IPOS = JPOS
               GO TO 1
            END IF
         END DO ! JPOS
      END IF
    1       CONTINUE
   END IF IFLABEL2
END IF IFLABEL1
!
!----------------------------------------------------------------------
!     RETURN STATION DETAILS IF A MATCH HAS BEEN FOUND OR MISSING
!     DATA VALUES IF NOT
!----------------------------------------------------------------------
!
IF (ISTAT == 0) THEN                     ! MATCH FOUND
   LAT  = DEGLAT(IPOS)
   LONG = DEGLON(IPOS)
   HGT  = HTSTN(IPOS)
   HGPT = HTP(IPOS)
ELSE                                    ! NO MATCH FOUND
   LAT  = -9999999.0
   LONG = -9999999.0
   HGT  = -9999999.0
   HGPT = -9999999.0
END IF
!                                          RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE STAPOS
