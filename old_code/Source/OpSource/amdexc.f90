SUBROUTINE AMDEXC(OB,R,CDISP,ID,IPOINT,LENGTH,BDAY,BHOUR,DATAEX)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDEXC
!
! PURPOSE       : TO EXPAND AN AMDAR REPORT
!                 (into an array which corresponds to the BUFR
!                  descriptor sequence for encoding, currently
!                  311200 - subscripts will have to be changed
!                  if the sequence changes!)
!
! DESCRIPTION   : VALUE IS LEFT MISSING IF CODE FIGURE OUT OF RANGE,
!                 DDD>360, HOUR>24, MINUTES>59 ETC.  THE GROUPS ARE
!                 IDENTIFIED REGARDLESS OF SEQUENCE, EXCEPT THAT
!                 TEMPERATURE IS ASSUMED TO COME BEFORE DEW POINT.
!    NOTE :::::   THIS IS ESSENTIALLY THE SDB EXPANSION ROUTINE,
!                 BUT THE FIRST 6 ARRAY ELEMENTS OF EACH REPORT HAVE
!                 NOT BEEN USED IN THE MDB VERSION.
!
! DATA TYPE(S)  : AMDAR
!
! CALLED BY     : AMDAR
!
! CALLS         : OFIGTS (FUNCTION), ZPDATE, AIRDATE
!
! ARGUMENTS     : (1) OB - BULLETIN
!                 (2) R - NUMBER OF THIS REPORT IN OUTPUT ARRAY
!                 (3) CDISP - CHARACTER DISPLACEMENT FOR AIRCRAFT ID
!                 (4) ID - CHARACTER ARRAY FOR AIRCRAFT IDS
!                 (5) IPOINT - POINTER TO REPORT IN BULLETIN
!                 (6) LENGTH - LENGTH OF THIS REPORT
!                 (7) BDAY - DAY FROM BULLETIN HEADING
!                 (8) BHOUR - HOUR FROM BULLETIN HEADING
!                 (9) DATAEX - EXPANDED DATA ARRAY
!
! REVISION INFO :
!
! $Workfile: amdexc.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 21/12/2011 10:25:23$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         21/12/2011 10:25:23    Sheila Needham  Allow
!       identifiers to have (some) missing characters.
!  3    MetDB_Refresh 1.2         24/01/2011 17:06:24    Alison Weir     Remove
!        old revision comments
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
! $
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

! Use statements:
USE metdb_com_mod, only : MISSIN
USE airdate_mod
USE ofigts_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: OB      !a1
INTEGER,          INTENT(INOUT) :: R       !a2
INTEGER,          INTENT(INOUT) :: CDISP   !a3
CHARACTER(LEN=*), INTENT(INOUT) :: ID      !a4
INTEGER,          INTENT(IN)    :: IPOINT  !a5
INTEGER,          INTENT(IN)    :: LENGTH  !a6
INTEGER,          INTENT(IN)    :: BDAY    !a7
INTEGER,          INTENT(IN)    :: BHOUR   !a8
REAL,        INTENT(INOUT):: DATAEX(29,*)  !a9 Seq 311200 has 29 items

! Local declarations:

!--------------------------------------------------------------------
!Declare integer
!--------------------------------------------------------------------
INTEGER          :: DEGR
INTEGER          :: MINS
INTEGER          :: BMONTH
INTEGER          :: BYEAR
INTEGER          :: I
INTEGER          :: M
INTEGER          :: L
INTEGER          :: N
INTEGER          :: TIME
INTEGER          :: RH
INTEGER          :: DDD
INTEGER          :: FF
INTEGER          :: RFF
INTEGER          :: TEMP
INTEGER          :: FL
INTEGER          :: TB
INTEGER          :: VG
INTEGER          :: S
INTEGER          :: REPHOUR
INTEGER          :: REPDAY
INTEGER          :: REPMONTH
INTEGER          :: REPYEAR
INTEGER          :: CENDAY

!----------------------------------------------------------------------
!Declare character
!----------------------------------------------------------------------

CHARACTER(LEN=8) :: SPACES='        '
CHARACTER(LEN=8) :: PADID

!----------------------------------------------------------------------
!Declare Real
!----------------------------------------------------------------------

REAL             :: RTEMP
REAL             :: RVG
REAL             :: RLAT
REAL             :: RLONG
REAL             :: TNTHS

!----------------------------------------------------------------------
!Declare Logical
!----------------------------------------------------------------------

LOGICAL          :: NOFLT

SAVE BMONTH,BYEAR
!----------------------------------------------------------------------
!Initialise
!----------------------------------------------------------------------

DATAEX(15,R)=MISSIN                ! flight level
NOFLT=.TRUE.
ID(CDISP:CDISP+7)=SPACES           ! Clear Identifier
PADID=SPACES

!----------------------------------------------------------------------
! Delimit the groups in the report by looking for spaces.
! M & N are the subscripts of the start & end of a group.
!----------------------------------------------------------------------

M=IPOINT

100 CONTINUE

L=INDEX(OB(M+1:)//SPACES,' ')               ! LENGTH OF GROUP
N=M+L-1                                     ! POINT TO LAST CHAR

!---------------------------------------------------------------------
!If the last group had an equals sign on the end of the group then
!it wouldn't be decoded. Vertical Gust is the most common last group
!and relies on the the last character in the group being reconised
!as a figure if its not then the decode skips the group!
!----------------------------------------------------------------------

IF (OB(N:N)  ==  '=') THEN                  !Corrects detection
  N=N-1                                     !of last group
  L=L-1                                     !in Amdar report
END IF                                      !

!----------------------------------------------------------------------
! If both start and end are letters, the group must be either stage
! of flight or the identifier (assuming this ends with z, not figure).
! check for l > 1 in case there are any double spaces or spaces at end
!----------------------------------------------------------------------
IFLABEL1: &
IF (OB(M:M) >= 'A' .AND. OB(M:M) <= 'Z' .AND.   &
    OB(N:N) >= 'A' .AND. OB(N:N) <= 'Z' .AND. L > 1) THEN
IFLABEL2: &
  IF (OB(M:N) == 'ASC')THEN
    DATAEX(17,R) = 5  ! CODE TABLE 008004
  ELSE IF (OB(M:N) == 'DES')THEN
    DATAEX(17,R) = 6  ! CODE TABLE 008004
  ELSE IF (OB(M:N) == 'LVR')THEN
    DATAEX(17,R) = 3  ! CODE TABLE 008004
  ELSE IF (OB(M:N) == 'LVW')THEN
    DATAEX(17,R) = 4  ! CODE TABLE 008004
  ELSE IF (OB(M:N) == 'UNS') THEN
    DATAEX(17,R) = 2  ! CODE TABLE 008004
  ELSE IF (DATAEX(2,R) == MISSIN ) THEN  ! Assume it's an identifier
    IF (L < 8) THEN
      PADID=OB(M:N)
    ELSE
      PADID=OB(M:M+7)    ! USE FIRST 8 CHARS WHEN >8
    END IF
    ID(CDISP:CDISP+7)=PADID
    DATAEX(2,R)=CDISP                      ! & POINTER IN ARRAY
  END IF IFLABEL2

!----------------------------------------------------------------------
! If figure at start but not end, must be lat or long (degrees & tenths
! - tairepst converted minutes to tenths before storing)
!----------------------------------------------------------------------

ELSE IF (OB(M:M) >= '0' .AND. OB(M:M) <= '9' .AND.   &
         OB(N:N) >= 'A' .AND. OB(N:N) <= 'Z') THEN   IFLABEL1
IFLABEL3: &
  IF (L == 8 .AND. (OB(N-1:N)  ==  'XX')) THEN
    ID(CDISP:CDISP+7)=OB(M:M+7)
    DATAEX(2,R)=CDISP
  ELSE IF (L == 5 .AND. (OB(N:N) == 'N' .OR. OB(N:N) == 'S')) THEN
    IF (OFIGTS(OB,M,M+3)) THEN
      READ (OB(M+2:M+3),'(I2)')MINS
      READ (OB(M:M+1),'(I2)')DEGR
      TNTHS=((MINS*10.0)/6.0)
      RLAT=DEGR+(TNTHS/100.0)
      IF (OB(N:N)  ==  'S') THEN
        RLAT=-RLAT
      END IF
      DATAEX(13,R)=RLAT
    END IF
  ELSE IF (L == 6 .AND. (OB(N:N) == 'E'.OR.OB(N:N) == 'W')) THEN
    IF (OFIGTS(OB,M,M+4)) THEN
      READ (OB(M+3:M+4),'(I2)') MINS
      READ (OB(M:M+2),'(I3)') DEGR
      TNTHS=((MINS*10.0)/6.0)
      RLONG=DEGR+(TNTHS/100.0)
      IF (OB(N:N)  ==  'W') THEN
        RLONG=-RLONG
      END IF
      DATAEX(14,R)=RLONG
    END IF
  END IF IFLABEL3

!----------------------------------------------------------------------
! If figures at both start and end, must be time if 4 figs,
! date/time if 6, relative humidity if 3 (unless '333'),
! wind if 7 with slash in the middle.
! (N.B. DATAEX(11,R), seconds, used for hours*100+min
!  till seconds set to missing at the end.)
!----------------------------------------------------------------------

ELSE IF (OB(M:M) >= '0' .AND. OB(M:M) <= '9' .AND.   &
         OB(N:N) >= '0' .AND. OB(N:N) <= '9') THEN   IFLABEL1

IFLABEL4: &
  IF (L == 4 .OR. L == 6) THEN
IFLABEL5: &
    IF (OFIGTS(OB,M,N)) THEN
      IF (L == 4) THEN
        REPDAY=MISSIN
        READ (OB(M:N),'(I4)') TIME
      ELSE
        READ (OB(M:M+1),'(I2)') REPDAY
        IF (REPDAY > 31) THEN
          PRINT *,'AMDEXC: impossible date ',OB(IPOINT:N)
          RETURN
        END IF
        READ (OB(M+2:N),'(I4)') TIME
      END IF

      IF (TIME <= 2400.AND.TIME >= 0) THEN
        IF (MOD(TIME,100) < 60) DATAEX(11,R)=TIME ! HOURS*100+MINES
      ELSE
        PRINT *,'AMDEXC: impossible time ',OB(IPOINT:N)
        RETURN
      END IF
    ELSE
      PRINT *,'AMDEXC: time not all figures ',OB(IPOINT:N)
      RETURN
    END IF IFLABEL5

  ELSE IF (L == 3 .AND. OB(M:N) /= '333') THEN    IFLABEL4
    IF (OFIGTS(OB,M,N)) THEN
      READ (OB(M:N),'(I3)') RH              ! REL HUM (PERCENTAGE)
      IF (RH <= 100) DATAEX(27,R)=RH
    END IF
  ELSE IF (L == 7 .AND. OB(M+3:M+3) == '/') THEN  IFLABEL4
    IF (OFIGTS(OB,M,M+2)) THEN
      READ (OB(M:M+2),'(I3)') DDD
      IF (DDD <= 360) DATAEX(19,R)=DDD      ! WIND DIRECTION
    END IF
    IF (OFIGTS(OB,N-2,N)) THEN
      READ (OB(N-2:N),'(I3)') FF            ! WIND SPEED (KNOTS)
      RFF=FF
      DATAEX(20,R)=RFF*0.51444              ! WIND SPEED M/S
    END IF
  END IF IFLABEL4

!----------------------------------------------------------------------
! If figures at end but not start, must be temp, tb, vertical gust,
! S-group or flight level (F or A at start, A if below sea level)
! or identifier without Z at the end (letters, then figures)
!----------------------------------------------------------------------

ELSE IF (OB(M:M) >= 'A' .AND. OB(M:M) <= 'Z' .AND.   &
         OB(N:N) >= '0' .AND. OB(N:N) <= '9') THEN   IFLABEL1
IFLABEL6: &
  IF (L == 5 .AND. (OB(M:M+1) == 'MS'.OR.OB(M:M+1) == 'PS')) THEN
    IF (OFIGTS(OB,M+2,N)) THEN
      READ (OB(M+2:N),'(I3)') TEMP
      IF (OB(M:M+1) == 'MS') TEMP=-TEMP
      RTEMP=TEMP
      IF (DATAEX(25,R) == MISSIN) THEN
        DATAEX(25,R)=(RTEMP/10.)+273.1      ! TEMPERATURE K
      ELSE
        DATAEX(26,R)=(RTEMP/10.)+273.1      ! DEW POINT K
      END IF
    END IF
  ELSE IF (L == 4 .AND. OB(M:M) == 'F') THEN    IFLABEL6
    IF ((DATAEX(15,R)  ==  MISSIN) .AND. NOFLT) THEN
      IF (OB(M+1:M+1)  ==  '-') THEN
        NOFLT=.FALSE.
        DATAEX(15,R)=MISSIN
      ELSE IF (OFIGTS(OB,M+1,N)) THEN
        READ (OB(M+1:N),'(I3)') FL
        DATAEX(15,R)=FL*30.48               ! HEIGHT (M)
        NOFLT=.FALSE.
      END IF
    END IF
  ELSE IF (L == 4 .AND. OB(M:M) == 'A') THEN    IFLABEL6
    IF((DATAEX(15,R)  ==  MISSIN) .AND. NOFLT) THEN
      IF (OFIGTS(OB,M+1,N)) THEN
        READ (OB(M+1:N),'(I3)') FL
        DATAEX(15,R)=-FL*30.48              ! BELOW SEA LEVEL
        NOFLT=.FALSE.
      END IF
    END IF
  ELSE IF (L == 3 .AND. OB(M:M+1) == 'TB') THEN IFLABEL6
    IF (OFIGTS(OB,N,N)) THEN
      READ (OB(N:N),'(I1)') TB
      IF (TB <= 3) DATAEX(23,R)=TB          ! TURBULENCE 0-3
    END IF
  ELSE IF (L == 5 .AND. OB(M:M+1) == 'VG') THEN IFLABEL6
    IF (OFIGTS(OB,M+2,N)) THEN
      READ (OB(M+2:N),'(I3)') VG
      RVG=VG
      DATAEX(21,R)=RVG/10.                ! GUST M/S, TENTHS
    END IF
  ELSE IF (L == 4 .AND. OB(M:M) == 'S') THEN    IFLABEL6
    IF (OFIGTS(OB,M+1,M+1)) THEN
      READ (OB(M+1:M+1),'(I1)') S            ! S1: NAVIGATION
      IF (S <= 1) DATAEX(3,R)=S             ! (INERTIAL/OMEGA)
    END IF
    IF (OFIGTS(OB,M+2,M+2)) THEN
      READ (OB(M+2:M+2),'(I1)') S            ! S2: ACARS INTERFACE
      IF (S <= 5) DATAEX(4,R)=S             ! (ACTIVE OR NOT...)
    END IF
    IF (OFIGTS(OB,M+3,M+3)) THEN
      READ (OB(M+3:M+3),'(I1)') S            ! S3: TEMP ACCURACY

! 002005 is temperature precision in hundredths in 7 bits,
! so 2 degrees will end up missing in the BUFR message!

      IF (S <= 1) THEN
        IF (S == 1) DATAEX(24,R)=1           ! 1 degree
        IF (S == 0) DATAEX(24,R)=2           ! 2 degrees
      END IF
    END IF
  ELSE IF (DATAEX(2,R) == MISSIN) THEN                   IFLABEL6
    PADID=OB(M:N)//SPACES(1:8-L)             ! PAD WITH SPACES
    ID(CDISP:CDISP+7)=PADID                  ! PUT ID IN STRING
    DATAEX(2,R)=CDISP                       ! & POINTER IN ARRAY
  END IF IFLABEL6
!----------------------------------------------------------------------
! If characters at start an no identifier yet and has fallen through
! every other option then must be an identifier with solidii at the end.
!----------------------------------------------------------------------

ELSE IF (OB(M:M) >= 'A' .AND. OB(M:M) <= 'Z' .AND.  &
                         DATAEX(2,R) == MISSIN) THEN IFLABEL1
  PADID=OB(M:N)                            ! PAD WITH SPACES
  DO I=1,8
    IF(PADID(I:I) == '/')PADID(I:I)=' '
  END DO
  ID(CDISP:CDISP+7)=PADID                  ! PUT ID IN STRING
  DATAEX(2,R)=CDISP                       ! & POINTER IN ARRAY
END IF IFLABEL1

!----------------------------------------------------------------------
! GROUP HAS BEEN RECOGNISED IF POSSIBLE. GO ON TO NEXT IF ANY LEFT.
!----------------------------------------------------------------------

M=N+2
IF (M < IPOINT+LENGTH-1) GO TO 100

!----------------------------------------------------------------------
! Now work out the date of the report, given its time (reported) and
! the date/times in the bulletin heading and after 'amdar', treating
! these as the period covered by the reports in the bulletin.
! First decide full bulletin date from bulletin day/hour & current time
! (only need to call AIRDATE for first ob in bulletin)
!----------------------------------------------------------------------

IF (R == 1) CALL AIRDATE(BHOUR,BDAY,BMONTH,BYEAR)

!----------------------------------------------------------------------
! If there is no day in the report (though it's now mandatory!),
! assume the period is less than 24 hours, so the report day is
! either the bulletin day or the day before, depending on the hour.
! If the day is reported, then assume it's within the last month,
! so the month is either the bulletin month or the month before.
!----------------------------------------------------------------------

REPHOUR=DATAEX(11,R)/100
IFLABEL7: &
IF (REPDAY == MISSIN) THEN
  IF (REPHOUR > BHOUR) THEN
    CALL DATE31(BDAY,BMONTH,BYEAR,CENDAY)
    CENDAY=CENDAY-1
    CALL DATE13(CENDAY,REPDAY,REPMONTH,REPYEAR)
  ELSE
    REPDAY=BDAY
    REPMONTH=BMONTH
    REPYEAR=BYEAR
  END IF
ELSE
  IF (REPDAY > BDAY) THEN
    REPMONTH=BMONTH-1
    IF (REPMONTH == 0) THEN
      REPMONTH=12
      REPYEAR=BYEAR-1
    END IF
  ELSE
    REPMONTH=BMONTH
    REPYEAR=BYEAR
  END IF
END IF IFLABEL7

DATAEX(6,R)=REPYEAR
DATAEX(7,R)=REPMONTH
DATAEX(8,R)=REPDAY
DATAEX(9,R)=REPHOUR
DATAEX(10,R)=DATAEX(11,R)-REPHOUR*100        ! MINUTES
DATAEX(11,R)=MISSIN                    ! seconds
DATAEX(12,R)=MISSIN                    ! lat/long precision
DATAEX(1,R)=MISSIN

!----------------------------------------------------------------------
! INCREMENT REPORT POINTER IN OUTPUT ARRAY
!----------------------------------------------------------------------

R=R+1                                        ! NEXT REPORT
CDISP=CDISP+8                                ! NEXT ID SLOT


RETURN
END SUBROUTINE AMDEXC
