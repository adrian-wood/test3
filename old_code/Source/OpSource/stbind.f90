SUBROUTINE STBIND(NOBS,ARRAY,ID,DATETIME,ENTRY)

!-----------------------------------------------------------------------
!
! PROGRAM       : STBIND
!
! PURPOSE       : GET TIME & PLACE DESCRIPTORS IN SATOB MESSAGE
!                 & MAKE INDEX ENTRY FROM THEM
!
! CALLED BY     : SATOB1, GOESW1
!
! CALLS         : BOXLALO, INDLALO, LATBOX
!
! ARGUMENTS     : (1) NUMBER OF REPORTS IN ARRAY
!               : (2) ARRAY OF DATA VALUES
!               : (3) SATELLITE ID
!               : (4) DATE/TIME OF FIRST REPORT
!               : (5) INDEX ENTRY SO FAR
!
! REVISION INFO :
!
!
! $Workfile: stbind.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 12/01/2011 14:13:03$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         12/01/2011 14:13:03    Rosemary Lavery
!       Initial Port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE BOXLALO_mod
USE INDLALO_mod
USE LATBOX_mod

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(IN)                :: NOBS        ! Number of observations in BUFR message
REAL, INTENT(IN)                   :: ARRAY(:)    ! Array of data values for BUFR message
CHARACTER (LEN=3), INTENT(IN)      :: ID          ! Satellite identifier
INTEGER, INTENT(OUT)               :: DATETIME(5) ! Data date & time (yr, mon, day, hr, min)
CHARACTER (LEN=23), INTENT(INOUT)  :: ENTRY       ! Index entry

! Local Variables

INTEGER  :: BEFORLAT  ! Data elements in ARRAY before 'latitude'
INTEGER  :: BEFORLON  ! Data elements in ARRAY before 'longitude'
INTEGER  :: BFORYEAR  ! Data elements in ARRAY before 'year'
INTEGER  :: NLAT      ! Position of latitude in elements of ARRAY
INTEGER  :: ITEMP     ! Used for temporary storage

REAL     :: BOX(4)    ! Observation 'box' boundary lat/longs

CHARACTER (LEN=1)  :: SECTION   ! Section number of SATOB message

! --------------------------------------------------------------------

!save variables
SAVE

!   THE BOXED 'NON-SATELLITE' INDEX.

! _______________________________________________________________
! : COR  : FINE : HOUR :MINUTE : SAT  ID :     :SECTION :NUMBER :
! :      : MESH :   6  :   1   :         :SPARE:NUMBER  :OF OBS :
! : FLAG : FLAG : BITS : BYTE  : 3 BYTES :     :1 BYTE  :2 BYTES:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 0                    1       2         5     9       10      12

! _______________________________________________________________
! : MIN  : MAX  :MIN   :MAX   :REPORT:TYPE   : TOR : REC :BLOCK :
! : LAT  : LAT  :LONG  :LONG  :FLAGS :FLAGS  :     : NUM :NUMBER:
! :1 BYTE:1 BYTE:1 BYTE:1 BYTE:4 BITS:4 BITS :     :     :      :
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!12     13     14     15     16             17    19    21     23

!  THE TYPE FLAGS ARE:
! ________________________________________________
! :TEMPERATURE:WIND:CLOUD COVER:RELATIVE HUMIDITY:
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 0           1    2           3                 4

SECTION = ENTRY(10:10)

! SET UP TIME,POSITION ENTRIES.

IF (SECTION == '4' .OR. SECTION == '7') THEN
   BFORYEAR = 1
ELSE
   BFORYEAR = 2
END IF

DATETIME(1) = ARRAY((BFORYEAR  )*NOBS + 1) ! YEAR
DATETIME(2) = ARRAY((BFORYEAR+1)*NOBS + 1) ! MONTH
DATETIME(3) = ARRAY((BFORYEAR+2)*NOBS + 1) ! DAY
DATETIME(4) = ARRAY((BFORYEAR+3)*NOBS + 1) ! HOUR
DATETIME(5) = ARRAY((BFORYEAR+4)*NOBS + 1) ! MINUTE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   INDEX ENTRY SECTIONS HANDLED BY THIS ROUTINE ARE:                !
!                                                                    !
!   MINUTES OF FIRST REPORT                                          !
!   NUMBER OF REPORTS IN SEQUENCE REFERRED TO BY INDEX ENTRY         !
!   LATITUDE AND LONGITUDE BOXES                                     !
!                                                                    !
!   INDEX ENTRY SECTIONS TO BE DONE BY NEXT ROUTINE ARE:             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ENTRY(2:2)=CHAR(DATETIME(5)) ! LOAD IN MINUTES OF REPORT
ENTRY(3:9) = '       '
ENTRY(3:5)=ID

ITEMP=NOBS/256
ENTRY(11:11)=CHAR(ITEMP)
ITEMP=MOD(NOBS,256)
ENTRY(12:12)=CHAR(ITEMP)

! LATITUDE / LONGITUDE BOX
! UNITS IN DEGREES IN ARRAY, 1/100TH DEGREES PRECISION.

IF      (SECTION == '4') THEN
   BEFORLAT = 7
ELSE IF (SECTION == '7') THEN
   BEFORLAT = 6
ELSE
   BEFORLAT = 9
END IF
BEFORLON = BEFORLAT+1

IF(NOBS > 1)THEN   !  MORE THAN 1 REPORT IN MESSAGE

  NLAT=BEFORLAT+1
  CALL LATBOX(ARRAY,NOBS,NLAT,BOX)
  CALL BOXLALO (BOX, ENTRY(13:16))

ELSE IF(NOBS == 1)THEN    !   SINGLE REPORT MESSAGE, SINGLE LAT/LONG

  CALL INDLALO(ENTRY,ARRAY(BEFORLAT+1),ARRAY(BEFORLON+1))

ENDIF

! SET TYPE FLAGS ACCORDING TO TABLE BELOW

!  ________________________________________________________________
!  SECTION  TEMPERATURE(8)  WIND(4)  CLOUD(2)  REL HUM(1)  FLAG TOT
!     2          Y            Y        N          N           12
!     3          N            Y        N          N            4
!     4          Y            N        N          N            8
!     5          Y            N        Y          N           10
!     7          N            N        N          Y            1
!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

IF      (SECTION == '2') THEN
   ENTRY(17:17) = CHAR(12)
ELSE IF (SECTION == '3') THEN
   ENTRY(17:17) = CHAR( 4)
ELSE IF (SECTION == '4') THEN
   ENTRY(17:17) = CHAR( 8)
ELSE IF (SECTION == '5') THEN
   ENTRY(17:17) = CHAR(10)
ELSE
   ENTRY(17:17) = CHAR( 1)
END IF

RETURN
END SUBROUTINE STBIND
