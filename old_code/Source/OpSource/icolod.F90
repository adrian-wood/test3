SUBROUTINE ICOLOD (ICAOLIST, DEGLAT, DEGLON, HT, HP, NUMICAO)

!-----------------------------------------------------------------------
!
! PROGRAM     : ICOLOD
!
! PURPOSE     : To load the ICAO abbreviated list into memory.
!
! DESCRIPTION : The ICAO list (MDB.ICAO.LIST) is read line by line and
!               the data is put into separate arrays, a character array
!               for the station ICAO Id., real arrays for latitude and
!               longitude, and integer arrays for station height and
!               barometer heights. These arrays are then returned to
!               the calling program.
!
!               ICOLOD was largely rewritten for version 2 (including
!               the argument list) when the format of the ICAO list was
!               changed in October 2010.
!
! USAGE       : CALL ICOLOD (ICAOLIST, DEGLAT, DEGLON, HT, HP, NUMICAO)
!
! ARGUMENTS   : (All parameters are output except NUMICAO which is I&O)
!
!               ICAOLIST  (C*4 array)  ICAO Id's
!               DEGLAT    (R*4 array)  Station latitudes in degrees
!               DEGLON    (R*4 array)  Station longitudes in degrees
!               HT        (I*4 array)  Station heights in metres
!               HP        (I*4 array)  Station barometer hts in metres
!               NUMICAO   (I*4) Input: Maximum allowed no. of ICAO IDs
!                              Output: Actual no. of ICAO IDs in list
!
! CALLED BY   : ICOBRV
!
! REVISION INFO :
!
! $Workfile: icolod.F90$ $Folder: OpSource$
! $Revision: 4$ $Date: 19/01/2011 09:40:18$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         19/01/2011 09:40:18    John Norton
!       Updated after rework for MDBSTOR batch 4 done.
!  3    MetDB_Refresh 1.2         07/01/2011 09:46:52    John Norton     Post
!       MDBSTOR batch 4 porting.
!  2    MetDB_Refresh 1.1         07/01/2011 09:42:39    John Norton
!       Original f77 pre-porting version!
!  1    MetDB_Refresh 1.0         10/12/2010 16:42:02    John Norton     After
!       MDBSTOR batch 4 porting.
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

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=4), INTENT(OUT)   :: ICAOLIST(*) !a1 List of ICAO identifiers
REAL,             INTENT(OUT)   :: DEGLAT(*)   !a2 Latitudes of stations
REAL,             INTENT(OUT)   :: DEGLON(*)   !a3 Longitudes of stations
INTEGER,          INTENT(OUT)   :: HT(*)       !a4 Station heights
INTEGER,          INTENT(OUT)   :: HP(*)       !a5 Station barometer heights
INTEGER,          INTENT(INOUT) :: NUMICAO     !a6 Number of ICAO identifiers found

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

LOGICAL          ::  exists

CHARACTER(LEN=4) ::  ICAO    ! Target ICAO identifier

INTEGER          ::  IHT     ! Station height
INTEGER          ::  IHP     ! Barometer ht
INTEGER          ::  KODE=0  ! Internally used status code
INTEGER          ::  MAXICAO ! Max. no. of ICAO identifiers

REAL             ::  SLAT    ! Station latitude
REAL             ::  SLON    ! Station longitude

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

#if ! defined (MVS)
INTEGER         LEV             !- Length of METDB_ICAOLIST
INTEGER         RC              !- Return code
CHARACTER*200   METDB_ICAOLIST  !- ICAO list PATH
#endif

!-----------------------------------------------------------------------
! Initialise variables
!-----------------------------------------------------------------------

MAXICAO = NUMICAO
NUMICAO = 0

!-----------------------------------------------------------------------
! Get environment variable METDB_ICAOLIST and find the length of it.
!-----------------------------------------------------------------------

#if ! defined (MVS)
CALL METDB_GETENV("METDB_ICAOLIST",METDB_ICAOLIST,RC)
IF (RC /= 0) THEN
  WRITE(6,*)'ICOLOD: ERROR: ENV VAR METDB_ICAOLIST not set'
  RETURN
END IF
LEV=LEN(METDB_ICAOLIST)
DO WHILE (METDB_ICAOLIST(LEV:LEV) == ' ')
  LEV=LEV-1
END DO
#endif

!----------------------------------------------------------------------
!     OPEN THE ICAO LIST
!----------------------------------------------------------------------

#if defined (MVS)
OPEN (82, FILE='DD:STNICAO', ACTION='READ')
#else
exists = INQUIRE(METDB_ICAOLIST(1:LEV),'DSN')
IF (.NOT.exists) THEN
  WRITE(6,*)'ICOLOD: ERROR: File ',METDB_ICAOLIST(1:LEV), &
            ' not found'
  RETURN
END IF
OPEN(82,FILE=METDB_ICAOLIST(1:LEV),IOSTAT=RC)
IF (RC /= 0) THEN
  WRITE(6,*)'ICOLOD: ERROR: Could not open file ', &
                  METDB_ICAOLIST(1:LEV)
  RETURN
END IF
#endif

!----------------------------------------------------------------------
!     LOOP OVER STATIONS IN ICAO LIST
!----------------------------------------------------------------------

DOLABEL1: &
DO WHILE (KODE == 0)
!                                      READ DETAILS OF NEXT STATION
!
  READ (82, '(A,T11,F8.3,F9.3,2I5)', IOSTAT=KODE) &
        ICAO, SLAT, SLON, IHT, IHP
!                                              CHECK FOR READ ERROR
IFLABEL1: &
   IF (KODE /= 0) THEN
      KODE = 2
!                               CHECK FOR BLANK LINE AT END OF LIST
!
   ELSE IF (ICAO == '    ') THEN
      KODE = 1
!                                       CHECK FOR TOO MANY STATIONS
   ELSE IF (NUMICAO >= MAXICAO) THEN
      KODE = 3
!                                   GOOD DATA - GET STATION DETAILS
   ELSE
      NUMICAO = NUMICAO + 1
      ICAOLIST(NUMICAO) = ICAO  ! ICAO IDENTIFIER
      DEGLAT(NUMICAO) = SLAT    ! LATITUDE
      DEGLON(NUMICAO) = SLON    ! LONGITUDE
      HT(NUMICAO) = IHT         ! STATION HEIGHT
      HP(NUMICAO) = IHP         ! BAROMETER HEIGHT
   END IF IFLABEL1
END DO DOLABEL1
!
!----------------------------------------------------------------------
!     CLOSE THE LIST AND OUTPUT A MESSAGE GIVING NUMBER OF STATIONS
!----------------------------------------------------------------------
!
CLOSE (82)
!
IFLABEL2: &
IF (KODE <= 1) THEN
   WRITE (6,'(/T5,A,I7,A)') 'ICOLOD:', NUMICAO, &
            ' STATION RECORDS READ FROM THE ICAO LIST.'
ELSE IF (KODE == 2) THEN
   WRITE (6,'(/T5,A,I6,A)') &
            'ICOLOD:  ICAO LIST TRUNCATED AFTER', NUMICAO, &
            ' STATIONS DUE TO I/O ERROR READING LIST.'
ELSE IF (KODE == 3) THEN
   WRITE (6,'(/T5,A,I6,A)') &
            'ICOLOD:  ICAO LIST TRUNCATED AFTER', NUMICAO, &
            ' STATIONS DUE TO LIMIT ON ARRAY SIZES.'
END IF IFLABEL2

RETURN
END SUBROUTINE ICOLOD
