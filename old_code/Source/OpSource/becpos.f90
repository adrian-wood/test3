SUBROUTINE BECPOS (BEAC_REQ, BLAT, BLONG, NFTBCN, ISTAT)
!
!
!-----------------------------------------------------------------------
!
! SUBROUTINE    : BECPOS
!
! PURPOSE       : To return location details for a particular beacon.
!
! DESCRIPTION   : The latitude and longitude are returned for a beacon
!                 whose identifier is specified (first argument) using
!                 a binary search of the beacon list.
!                 The beacon list "MDB.BEACONS" is read using a call to
!                 BECRET the first time that BECRET is called.
!
! USAGE         : CALL BECPOS (BEAC_REQ, BLAT, BLONG, NFTBCN, ISTAT)
!
! PARAMETERS    :  BEAC_REQ  I  (C*8) Identifier for beacon
!                  BLAT      O  Latitude of beacon
!                  BLONG     O  Longitude of beacon
!                  NFTBCN    I  Unit number for beacon list
!                  ISTAT     O  Return code ('0'=OK; '8'= Not found in
!                               list: '16'= Name > 8 characters)
!
!                 If station details are not found, missing data values
!                 (-9999999.0) are returned for latitude and longitude
!                 and a warning message is output on unit 6.
!
! CALLED BY     : AIRPOS (or other user's program).
!
! CALLS TO      : BECRET, SATYPE.
!
! REVISION INFO:
!
! $Workfile: becpos.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 26/01/2011 15:07:59$
!
! CHANGE RECORD:
!
! $Log:
!  4    MetDB_Refresh 1.3         26/01/2011 15:07:59    Richard Weedon  Final
!       amendments
!  3    MetDB_Refresh 1.2         25/01/2011 12:39:25    Richard Weedon  added
!       revision template
!  2    MetDB_Refresh 1.1         14/01/2011 15:07:01    Richard Weedon
!       Updated for satype_mod.f90 & becret_mod.f90
!       
!  1    MetDB_Refresh 1.0         13/01/2011 16:44:37    Richard Weedon  Passes
!        intial compilation test. MOD statements not yet added, checked in at
!       end of day
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
!     DECLARE VARIABLES, INITIALISATIONS ETC.
!    (IF IT IS NECESSARY TO INCREASE THE NUMBER OF BEACONS WHICH CAN
!     BE HANDLED, ALTER THE NUMBER ON THE PARAMETER STATEMENT BELOW.
!     NO OTHER CHANGES TO THIS OR ANY OTHER ROUTINE WILL BE REQUIRED.)
!----------------------------------------------------------------------
USE becret_mod
USE satype_mod
!
!
IMPLICIT NONE
!
!
! Arguments
CHARACTER(LEN=*),INTENT(IN)   ::  BEAC_REQ ! ID. OF REQUIRED BEACON
REAL,INTENT(OUT)              ::  BLAT     ! REQ LAT & LONG
REAL,INTENT(OUT)              ::  BLONG    ! REQUIRED LAT & LONG
INTEGER,INTENT(IN)            ::  NFTBCN   ! UNIT NUMBER FOR BEACON LIST
INTEGER,INTENT(OUT)           ::  ISTAT    ! RETURN CODE
!
INTEGER,PARAMETER             ::  MAXSTNS=2500  ! MAXIMUM NUMBER OF
INTEGER                       ::  NPOS          ! POS IN BEACON LIST
INTEGER                       ::  NUMSTNS  ! NO. OF STATNS FOUND IN LIST
!                                                               REALS
REAL                          ::  DEGLAT(MAXSTNS)! LAT OF STATIONS
REAL                          ::  DEGLON(MAXSTNS)! LONG OF STATIONS
!                                                          CHARACTERS
CHARACTER(LEN=8)              ::  BEACON(MAXSTNS)! LIST BEACON ID'S.
!                                                           LOGICAL
LOGICAL                       ::  FIRST ! FLAG FOR FIRST CALL TO BECPOS
DATA FIRST /.TRUE./
!                          COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
!
COMMON /COMBCN/ BEACON, DEGLAT, DEGLON
SAVE   /COMBCN/, NUMSTNS, FIRST
!                                                REVISION INFORMATION
!----------------------------------------------------------------------
!     IF THIS IS THE FIRST CALL TO BECPOS, READ THE BEACON LIST AND
!     STORE DETAILS IN ARRAYS.
!----------------------------------------------------------------------
!
IF (FIRST) THEN
   NUMSTNS = MAXSTNS
   CALL BECRET (NFTBCN, BEACON, DEGLAT, DEGLON, NUMSTNS)
   FIRST = .FALSE.
END IF
!
!----------------------------------------------------------------------
!     CHECK IDENTIFIER AND REJECT IF TOO LONG; OTHERWISE LOOK UP IN
!     BEACON LIST.
!----------------------------------------------------------------------
!
IF (LEN(BEAC_REQ) > 8) THEN
   ISTAT = 16
ELSE
   CALL SATYPE (BEAC_REQ, BEACON, BEACON, NUMSTNS, NPOS)
   ISTAT = 0
   IF (NPOS <= 0) ISTAT = 8
END IF
!
!----------------------------------------------------------------------
!     RETURN STATION DETAILS IF A MATCH HAS BEEN FOUND OR MISSING DATA
!     VALUES AND A WARNING MESSAGE IF NOT.
!----------------------------------------------------------------------
!
IF (ISTAT == 0) THEN                 ! MATCH FOUND
   BLAT  = DEGLAT(NPOS)
   BLONG = DEGLON(NPOS)
ELSE                                 ! NO MATCH FOUND
   BLAT  = -9999999.0
   BLONG = -9999999.0
   WRITE (6,'(T5,2A)') 'BECPOS:   BEACON NAME NOT FOUND - ',&
     BEAC_REQ
END IF
!                                           RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE BECPOS
