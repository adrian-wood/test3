SUBROUTINE ICOBRV (ICAO, POSN, HT, IRC)

!-----------------------------------------------------------------------
!
! PROGRAM     : ICOBRV
!
! PURPOSE     : To return station details for a given ICAO identifier
!
! DESCRIPTION : Details of station location and height are returned for
!               a given station whose 4-character ICAO identifier is
!               specified (first argument). The Abbreviated ICAO list
!               MDB.ICAO.LIST is read using a call to ICOLOD the first
!               time the routine is called.
!
!               If available, the barometer height is returned in HT,
!               otherwise HT will be the station height. A missing data
!               value (-999) is returned for any station latitudes,
!               longitudes or heights which are not available in the
!               abbreviated ICAO list.
!
!               ICOBRV was largely rewritten for version 2 (including
!               the argument list) when the format of the ICAO list was
!               changed in October 2010.
!
! USAGE       : CALL ICOBRV (ICAO, POSN, HT, IRC)
!
! ARGUMENTS   : Name I/O Type Size        Description
!               ---- --- ---- ----        -----------
!               ICAO  I   C*4   1  ICAO identifier (e.g. EGPD).
!               POSN  O   I*4   2  Latitude & longitude of station.
!               HT    O   I*4   1  Height of station.
!               IRC   O   I*4   1  Return Code. 0 = OK; 8 = Not found.
!
! CALLS TO    : ICOLOD, SATYPE.
!
! REVISION INFO :
!
! $Workfile: icobrv.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 21/03/2011 12:59:52$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         21/03/2011 12:59:52    Alison Weir     SAVE
!       numiaco
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

USE icolod_mod
USE satype_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=4), INTENT(IN)    :: ICAO    !a1 Target ICAO identifier
REAL,             INTENT(OUT)   :: POSN(2) !a2 Lat & Long of target station
INTEGER,          INTENT(OUT)   :: HT      !a3 Height of target station
INTEGER,          INTENT(OUT)   :: IRC     !a4 Return code (0 or 8)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,PARAMETER :: MAXICAO=10000     ! Maximum number of ICAO IDs

REAL              :: DEGLAT(MAXICAO)   ! Latitudes of ICAO stations
REAL              :: DEGLON(MAXICAO)   ! Longitudes of ICAO stations

LOGICAL           :: FIRST=.TRUE.      ! Flag for first call to ICOBRV

INTEGER           :: HTSTN(MAXICAO)    ! Heights of ICAO stations
INTEGER           :: HTBAR(MAXICAO)    ! Barometer heights of stations

CHARACTER(LEN=4)  :: ICAOLIST(MAXICAO) ! Identifiers in ICAO list

INTEGER           :: NPOS              ! Location in ICAO list
INTEGER, SAVE     :: NUMICAO           ! Number of ICAO IDs in list

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!                            COMMON block (for dynamic allocation only)

COMMON /COMLST/ ICAOLIST, DEGLAT, DEGLON, HTSTN, HTBAR
SAVE   /COMLST/

!-----------------------------------------------------------------------
!     FIRST CALL, READ THE ICAO LIST AND STORE DETAILS IN ARRAYS.
!-----------------------------------------------------------------------

IF (FIRST) THEN
!                                                        Read ICAO list
  NUMICAO = MAXICAO
  CALL ICOLOD (ICAOLIST, DEGLAT, DEGLON, HTSTN, HTBAR, NUMICAO)
  FIRST = .FALSE.
END IF

!-----------------------------------------------------------------------
!     LOOK UP ICAO IDENTIFIER IN LIST.
!-----------------------------------------------------------------------

CALL SATYPE (ICAO, ICAOLIST, ICAOLIST, NUMICAO, NPOS)

!-----------------------------------------------------------------------
!     RETURN STATION DETAILS IF ID WAS FOUND OR MISSING DATA IF NOT.
!-----------------------------------------------------------------------

IFLABEL1: &
IF (NPOS > 0) THEN                 ! Match found
  IRC = 0
  POSN(1) = DEGLAT(NPOS)             ! Latitude
  POSN(2) = DEGLON(NPOS)             ! Longitude

  HT = HTBAR(NPOS)                   ! Barometer ht. preferred
  IF (HT == -999) HT = HTSTN(NPOS)   ! otherwise station ht.

ELSE                               ! No match found
  IRC = 8
  POSN(1) = -999.0
  POSN(2) = -999.0
  HT = -999
END IF IFLABEL1

RETURN
END SUBROUTINE ICOBRV
