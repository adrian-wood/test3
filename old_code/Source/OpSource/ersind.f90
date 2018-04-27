SUBROUTINE ERSIND (A, ND, NOBS, DESCR, BULL, IFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : ERSIND
!
! PURPOSE       : FIND TIME & PLACE DESCRIPTORS IN ERS OR LASS
!                 MESSAGE & MAKE INDEX ENTRY FROM THEM
!
! CALLED BY     : BUFRBUL, LASSEP, ERSMWI, ERSMERGE, ERSWEEP
!
! CALLS         : ERSREP
!
! ARGUMENTS     : (1) ARRAY OF DECODED COORDINATE VALUES
!                      (OR ALL VALUES IF LASS; DECORD GOES AS FAR AS
!                       THE FIRST INSTANCE OF EACH COORDINATE WANTED
!                       - SO AS FAR AS LAT/LONG, SO TWO DATE/TIMES,
!                       STATE VECTOR & DATA, FOR ERS-2)
!                 (2) NUMBER OF DESCRIPTORS
!                 (3) NUMBER OF OBS IN MESSAGE
!                 (4) SEQUENCE OF DESCRIPTORS
!                 (5) BUFR MESSAGE WITH DESCRIPTORS IN
!                 (6) FT NUMBER (FOR ERSREP)
!
! REVISION INFO :
!
! $Workfile: ersind.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 18/04/2011 11:25:22$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         18/04/2011 11:25:22    Brian Barwell
!       Dimension of A changed to (NOBS,*) and RESHAPE replaced by DO loop. 
!  3    MetDB_Refresh 1.2         25/01/2011 21:46:44    Sheila Needham
!       Updated following review
!  2    MetDB_Refresh 1.1         25/01/2011 15:12:42    John Norton     After
!       initial porting of batches MDBSTOR 19 & 23
!  1    MetDB_Refresh 1.0         19/01/2011 11:28:01    John Norton
!       Pre-porting f77 version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE boxlalo_mod
USE ersrep_mod
USE latbox_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: ND         !a02
INTEGER,          INTENT(IN)    :: NOBS       !a03
REAL,             INTENT(IN)    :: A(NOBS,*)  !a01
INTEGER,          INTENT(INOUT) :: DESCR(*)   !a04
CHARACTER(LEN=*), INTENT(INOUT) :: BULL       !a05
INTEGER,          INTENT(INOUT) :: IFT        !a06

! Local declarations:

INTEGER          ::  DATIME(5)    ! Ob date/time for index entry
INTEGER          ::  DAY=1027     ! BUFR 004003
INTEGER          ::  I !?
INTEGER          ::  ID           ! Satellite identifier
INTEGER          ::  IDENT=263    ! BUFR 001007
INTEGER          ::  HH=1028      ! BUFR 004004
INTEGER          ::  LAT2=1282    ! BUFR 005002
INTEGER          ::  LAT5=1281    ! BUFR 005001
INTEGER          ::  LONG2=1538   ! BUFR 006002
INTEGER          ::  LONG5=1537   ! BUFR 006001
INTEGER          ::  MINS         ! Minute of observation
INTEGER          ::  MINUTE=1029  ! BUFR 004005
INTEGER          ::  MONTH=1026   ! BUFR 004002
INTEGER          ::  NDAY         ! 2nd subscript of A for day
INTEGER          ::  NHOUR        ! 2nd subscript of A for hour
INTEGER          ::  NLAT         ! 2nd subscript of A for latitude
INTEGER          ::  NLONG        ! 2nd subscript of A for longitude
INTEGER          ::  NMINS        ! 2nd subscript of A for minute
INTEGER          ::  NMONTH       ! 2nd subscript of A for month
INTEGER          ::  NOPER        ! Number of operators in DESCR array
INTEGER          ::  NSAT         ! 2nd subscript of A for satellite ID
INTEGER          ::  NYEAR        ! 2nd subscript of A for year
INTEGER          ::  SECOND=1030  ! BUFR 004006
INTEGER          ::  YEAR=1025    ! BUFR 004001

LOGICAL          ::  SEA          ! OPbservation land/sea flag

CHARACTER(LEN=12) :: ENTRY        ! Index entry

REAL             ::  BOX(4)       ! Min.lat., max.lat., min.long., max.long.
REAL             ::  B(NOBS*2)    ! reshaped A array for LATBOX

!-----------------------------------------------------------------------
!
! THE DESCRIPTORS CAN INCLUDE OPERATORS WITH NO CORRESPONDING VALUES:
! REMOVE THESE TO MAKE THE SEARCH FOR COORDINATES EASIER.
! (THE ARRAY ISN'T USED ONCE THE COORDINATES HAVE BEEN FOUND, DESPITE
! BEING PASSED TO ERSREP!)
!
NOPER=0          ! NUMBER OF OPERATORS FOUND
DO I=1,ND
  IF (DESCR(I) >= 16384) THEN    ! Operator
    NOPER=NOPER+1
  ELSE                           ! Data value
    DESCR(I-NOPER)=DESCR(I)
  END IF
END DO
ND=ND-NOPER
!
! GO THROUGH DESCRIPTORS NOTING SUBSCRIPTS OF SATELLITE IDENTIFIER
! AND DATA TIME/POSITION, ASSUMING THAT A DATE/TIME FOLLOWED BY A
! LAT/LONG IS THE DATA TIME, NOT A STATE VECTOR OR FORECAST TIME.
!
DOLABEL1: &
DO I=1,ND
  IF (DESCR(I) == IDENT) NSAT=I
IFLABEL1: &
  IF (DESCR(I) == YEAR .AND. DESCR(I+1) == MONTH .AND.      &
      DESCR(I+2) == DAY .AND. DESCR(I+3) == HH .AND.        &
      DESCR(I+4) == MINUTE .AND. DESCR(I+5) == SECOND .AND. &
     (DESCR(I+6) == LAT2 .OR. DESCR(I+6) == LAT5) .AND.     &
     (DESCR(I+7) == LONG2 .OR. DESCR(I+7) == LONG5)) THEN
    NYEAR=I
    NMONTH=I+1
    NDAY=I+2
    NHOUR=I+3
    NMINS=I+4
    NLAT=I+6
    NLONG=I+7
  END IF IFLABEL1
END DO DOLABEL1

! DETERMINE THE BOUNDARIES OF A LAT/LONG BOX CONTAINING ALL THE OBS.

DO I=1,NOBS
  B(I)      = A(I,NLAT)    ! Latitudes
  B(NOBS+I) = A(I,NLONG)   ! Longitudes
END DO
CALL LATBOX (B, NOBS, 1, BOX)

! IF LATITUDE AND LONGITUDE BOUNDARIES ARE OK, SET UP BYTES 1-8 OF THE
! INDEX ENTRY AND STORE THE DATA. DATA IS ASSUMED TO BE OVER THE SEA.

IFLABEL2: &
IF (ABS(BOX(1)) <=  90.0 .AND. ABS(BOX(2)) <=  90.0 .AND. &
    ABS(BOX(3)) <= 180.0 .AND. ABS(BOX(4)) <= 180.0) THEN

  SEA=.TRUE.
!
! SET UP INDEX ENTRY (IDENT, HOUR & MINUTES, NO. OF OBS, LAT/LONGS)
!  (LATITUDES: WHOLE DEGREES PLUS 90.  LONGITUDES: (DEGREES/2 + 90).)
! (TO BE COMPLETED BY TIMES & BLOCK/RECORD NUMBER IN STORAGE PROGRAM)
! ___________________________________________________________________
! : LAND :  OLD : SATELLITE  : HOUR : MINUTE : NUMBER  : LAT/ : TOR :
! : /SEA : MODEL: IDENTIFIER :  (5  :   (6   : OF OBS  : LONG : BLK :
! : FLAG : FLAG : (9 BITS)   : BITS):  BITS) :(10 BITS):  BOX : REC :
! -------------------------------------------------------------------
! 0                                 2                  4      8    12
!   SEA=1    =0
! ________________________________________________________________
! :                  : MIN : MAX : MIN : MAX : TOR : REC : BLOCK :
! :   (SEE ABOVE)    : LAT : LAT : LONG: LONG:     : NUM : NUMBER:
! :                  :     :     :     :     :     :     :       :
! ----------------------------------------------------------------
! 0                  4     5     6     7     8     9    10      12
!
  ID=A(1,NSAT)
  IF (SEA) ID=ID+1024                  ! SEA IS 1, LAND IS 0
  ENTRY(1:1)=CHAR(ID/8)
  ENTRY(2:2)=CHAR(MOD(ID,8)*32)        ! HOUR TO BE ADDED LATER
!
  MINS=A(1,NMINS)
  ENTRY(3:3)=CHAR(MINS*4+NOBS/256)     ! MINS & TOP 2 BITS OF NOBS
  ENTRY(4:4)=CHAR(MOD(NOBS,256))       ! REMAINING 8 BITS OF NOBS
!
  CALL BOXLALO (BOX, ENTRY(5:8))
!
  DATIME(1)=A(1,NYEAR)
  DATIME(2)=A(1,NMONTH)
  DATIME(3)=A(1,NDAY)
  DATIME(4)=A(1,NHOUR)
  DATIME(5)=A(1,NMINS)
!
! PASS INDEX ENTRY AND DATE/TIME OF DATA TO DECIDE WHERE TO STORE IT.
!
  CALL ERSREP(NOBS,DATIME,ENTRY,BULL,IFT)
END IF IFLABEL2
RETURN
END SUBROUTINE ERSIND
