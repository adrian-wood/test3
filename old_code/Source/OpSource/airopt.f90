SUBROUTINE AIROPT(REPLEN,REPORT,POINT,TIMEDD,TIMEHH,TIMEMM,LAT,   &
      LAT2,LONG,LONG2,OPT_TEMP,OPT_WIND,OPT_WNDS,OPT_TURB,OPT_ICE,&
      MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,SIGN,MATCH,NFT,NFTBCN,DECERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIROPT
!
! PURPOSE       : SEARCH FOR OPTIONAL ELEMENT GROUPS IN AIREP
!
! DESCRIPTION   : LOOKS FOR CERTAIN COMBINATIONS OF WORDS AND LETTERS
!                 TO DETERMINE A PARTICULAR ELEMENT GROUP. ONCE FOUND
!                 THE RELEVANT DECODE ROUTINE IS CALLED. THE SEARCH
!                 WILL CONTINUE FOR A MAXIMUM 4 LOOPS OR UNTIL THE
!                 END OF THE REPORT IS REACHED
!
! ARGUMENTS     : (1) REPLEN - Report Length                         (i)
!                 (2) REPORT - Airep report                          (i)
!                 (3) POINT  - Position within report              (i/o)
!                 (4) TIMEDD - Day of current report                 (i)
!                 (5) TIMEHH - Hour of current report                (i)
!                 (6) TIMEMM - Mins of current report                (i)
!                 (7) LAT                                            (i)
!                 (8) LAT2   - Second Latitude                       (o)
!                 (9) LONG                                           (i)
!                 (10) LONG2  - Second Longitude                     (o)
!                 (11) OPT_TEMP - Decoded optional temp group      (o)
!                 (12) OPT_WIND - Decoded optional wind direction  (o)
!                 (13) OPT_WNDS - Decoded optional wind speed group(o)
!                 (14) OPT_TURB - Decoded option turb group        (o)
!                 (15) OPT_ICE  - Decoded optional ice group       (o)
!                 (16) MID_YY   - Midpt date/time                  (o)
!                 (17) MID_MTH                                     (o)
!                 (18) MID_DD                                      (o)
!                 (19) MID_HH                                      (o)
!                 (20) MID_MM                                      (o)
!                 (21) SIGN     - callsign                        (i/o)
!                 (22) MATCH    - set if second rep found          (o)
!                 (23) NFT      - MDB dataset unit number          (i)
!                 (24) NFTBCN   - Beacons dataset unit number      (i)
!                 (25) DECERR   - Decode error flag                (o)
!
! CALLED BY     : AIRARP
!
! CALLS TO      : AIRPOS,AIRELM,AIRLOC,AIRTRB,AIRICE,IARMID
!
! REVISION INFO :
!
! $Workfile: airopt.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 18/05/2011 10:12:22$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         18/05/2011 10:12:22    Stan Kellett    checks
!        added for report going beyong report length
!  5    MetDB_Refresh 1.4         20/04/2011 11:17:57    Brian Barwell   Avoid
!       exceeding report length in INDEX check. Also some tidying up.
!  4    MetDB_Refresh 1.3         27/01/2011 11:30:50    Stan Kellett
!       corrected IO to be a dummy variable rather than parameter to get
!       MDBSTOR build to work
!  3    MetDB_Refresh 1.2         14/01/2011 16:27:47    Rosemary Lavery Minor
!       update on review
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces

USE airpos_mod
USE airelm_mod
USE airloc_mod
USE airtrb_mod
USE airice_mod
USE airmid_mod

IMPLICIT NONE

! Arguments

INTEGER,INTENT(IN)    :: REPLEN          !(1) - see header comments
CHARACTER (LEN=*),INTENT(IN)  ::  REPORT !(2)
INTEGER,INTENT(INOUT) ::  POINT          !(3)
REAL,INTENT(IN)    ::     TIMEDD         !(4)
REAL,INTENT(IN)    ::     TIMEHH         !(5)
REAL,INTENT(IN)    ::     TIMEMM         !(6)
REAL,INTENT(IN)    ::     LAT            !(7)
REAL,INTENT(OUT)   ::     LAT2           !(8)
REAL,INTENT(IN)    ::     LONG           !(9)
REAL,INTENT(OUT)   ::     LONG2          !(10)
REAL,INTENT(OUT)   ::     OPT_TEMP       !(11)
REAL,INTENT(OUT)   ::     OPT_WIND       !(12)
REAL,INTENT(OUT)   ::     OPT_WNDS       !(13)
REAL,INTENT(OUT)   ::     OPT_TURB       !(14)
REAL,INTENT(OUT)   ::     OPT_ICE        !(15)
REAL,INTENT(OUT)   ::     MID_YY         !(16)
REAL,INTENT(OUT)   ::     MID_MTH        !(17)
REAL,INTENT(OUT)   ::     MID_DD         !(18)
REAL,INTENT(OUT)   ::     MID_HH         !(19)
REAL,INTENT(OUT)   ::     MID_MM         !(20)
CHARACTER(LEN=8),INTENT(INOUT) :: SIGN   !(21)
INTEGER,INTENT(OUT)   ::  MATCH          !(22)
INTEGER,INTENT(IN)    ::  NFT            !(23)
INTEGER,INTENT(IN)    ::  NFTBCN         !(24)
INTEGER,INTENT(OUT)   ::  DECERR         !(25)

! Local Parameters

! Local Variables

INTEGER :: I0 = 0          ! dummy var for call to AIRELM
INTEGER :: ATP_FLAG        !Flag for At this Point indicator
INTEGER :: ATP_OLD         !Flag to indicate if group decoded
CHARACTER(LEN=8) :: BEAC_NAME   !Beacon name used in call to AIRPOS
REAL    :: CALC_LAT
REAL    :: CALC_LONG
INTEGER :: COUNT
LOGICAL :: DEBUG           !Used for debugging
INTEGER :: END_FLAG        !Flag indicates if end of report
LOGICAL :: GEN_MID
INTEGER :: GROUPS
INTEGER :: GRPLEN          !Length of single group within report
INTEGER :: I
INTEGER :: ICE_FLAG        !Flag indicates if icing group found
INTEGER :: ICE_OLD         !Flag to indicate if group decoded
INTEGER :: IND
INTEGER :: IND2
INTEGER :: LAT_FLAG
INTEGER :: LAT_OLD
LOGICAL :: LLCORD
LOGICAL :: LREPFL          !Flag .TRUE. if end of report in AIRLOC
INTEGER :: MID_FLAG        !Flag indicates if MiD group found
INTEGER :: MID_OLD         !Flag to indicate if group decoded
INTEGER :: MIDZ
REAL    :: MISSING=-9999999.
INTEGER :: NEWZ
REAL    :: TEMP            ! Decoded temperature group
INTEGER :: TEMP_FLAG       !Flag indicates if temperature group found
INTEGER :: TEMP_OLD        !Flag to indicate if group decoded
INTEGER :: TEST
REAL    :: TLAT
REAL    :: TLONG
INTEGER :: TURB_FLAG       !Flag indicates if Turbulence group found
INTEGER :: TURB_OLD        !Flag to indicate if group decoded
REAL    :: WHL_LAT
REAL    :: WHL_LONG
INTEGER :: WIND_FLAG       !Flag indicates wind group found
INTEGER :: WIND_OLD        !Flag to indicate if group decoded

! Initialise

CALC_LAT=-9999999.
CALC_LONG=-9999999.
OPT_TEMP=-9999999.
OPT_ICE=-9999999.
OPT_TURB=-9999999.
OPT_WNDS=-9999999.
OPT_WIND=-9999999.
WHL_LAT=-9999999.
WHL_LONG=-99999999.
MID_FLAG=0
WIND_FLAG=0
TEMP_FLAG=0
ICE_FLAG=0
TURB_FLAG=0
END_FLAG=0
LAT_FLAG=0
MID_OLD=0
WIND_OLD=0
ATP_OLD=0
TEMP_OLD=0
TURB_OLD=0
ICE_OLD=0
LAT_OLD=0
GROUPS=0
LAT2=-9999999.
LONG2=-9999999.
COUNT=0
DEBUG=.FALSE.
LREPFL=.FALSE.
BEAC_NAME='AIROPT'
GEN_MID=.false.
LLCORD=.FALSE.

I=0
IND=0
IND2=0
DECERR=0

!----------------------------------------------------------------------
!Loop round and identify as many groups as possible. We only need
!one wind/temp/turb and icing group here - so a flag is set to indicate
!if one found.
!----------------------------------------------------------------------

IF (REPORT(POINT:POINT)  ==  '=') END_FLAG=1  !Check for End of report

!----------------------------------------------------------------------
!Estimate the number of optional groups to decode
!----------------------------------------------------------------------

TEST=POINT-2
DO WHILE (TEST  <=  REPLEN)
  IF (REPORT(test:test)  ==  ' ') THEN
    GROUPS=GROUPS+1
    TEST=TEST+1
  ELSE IF (REPORT(test:test) ==  '=') THEN
    TEST=REPLEN+1
  ELSE
    TEST=TEST+1
  END IF
END DO

!----------------------------------------------------------------------
!Since the optional groups are at the end of an airep report the loop
!continues until the end delimeter '=' is found.
!----------------------------------------------------------------------

dowhile1: &
DO WHILE (END_FLAG  ==  0 .AND. COUNT  <=  GROUPS)

!----------------------------------------------------------------------
!The MID flag checks to see if the string 'MID' is present in the report
!----------------------------------------------------------------------

  IF (MID_FLAG  ==  0) THEN
    IF ( (POINT+2) <= REPLEN) THEN
      IF (REPORT(POINT:POINT+2)  ==  'MID') THEN
        GEN_MID=.TRUE.
        MID_FLAG=1
      END IF
    ENDIF
  END IF

!----------------------------------------------------------------------
!----------------------------------------------------------------------

iflabel1: &
  IF (REPORT(POINT:POINT) < 'A' .OR. REPORT(POINT:POINT) > 'Z') THEN
    IND=INDEX(REPORT(POINT:REPLEN),' ')-1

iflabel1a: &
    IF (IND > 0 .AND. IND < 13) THEN
      IND=IND+POINT-1
iflabel2: &
      IF ((REPORT(IND:IND) == 'N' .OR. REPORT(IND:IND) == 'S' .OR.    &
           REPORT(IND:IND) == 'E' .OR. REPORT(IND:IND) == 'W') .AND.  &
           LAT_FLAG == 0) THEN
        CALL AIRPOS(REPLEN,REPORT,POINT,LAT2,LONG2,   &
        BEAC_NAME,NFTBCN,DECERR)
        IF ((lat2  >  -99) .and. (long2  >  -99)) then
          GEN_MID=.FALSE.
          LLCORD=.TRUE.
        ELSE
          GEN_MID=.TRUE.
        END IF
      ELSE IF (REPORT(IND:IND) == '=' .AND.   &
        IND-POINT+1 >= 3 .AND. WIND_FLAG == 0) THEN
        WIND_FLAG=1
        CALL AIRELM(REPLEN,REPORT,POINT,TEMP,OPT_WIND,OPT_WNDS, &
        WIND_FLAG,I0)
        IF ((OPT_WIND  >=  0) .AND. (OPT_WIND  <=  360)) THEN
          WIND_FLAG=1
          IF (.NOT. LLCORD) GEN_MID=.TRUE.
        END IF
      END IF iflabel2
    END IF iflabel1a
  END IF iflabel1

!----------------------------------------------------------------------
!   Look for an at this point (atp) indicator
!----------------------------------------------------------------------

  IF (ATP_FLAG  ==  0) THEN
    IF ( (POINT+2) <= REPLEN) THEN
      IF (REPORT(POINT:POINT+2)  ==  'ATP') THEN
        ATP_FLAG=1
        CALL AIRPOS(REPLEN,REPORT,POINT,LAT2,LONG2,BEAC_NAME, &
        NFTBCN,DECERR)
        IF (DECERR == 0) CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
      END IF
    END IF
  END IF


!----------------------------------------------------------------------
!  Look for temperature group
!----------------------------------------------------------------------

  IF (TEMP_FLAG  ==  0) THEN
    IF ( (POINT+2) <= REPLEN) THEN
      IF (((REPORT(POINT:POINT)  ==  'M') .OR.   &
           (REPORT(POINT:POINT)  ==  'P')).AND.       &
           (REPORT(POINT:POINT+2)  /=  'MID')) THEN
        TEMP_FLAG=1
        CALL AIRELM(REPLEN,REPORT,POINT,OPT_TEMP,OPT_WIND,OPT_WNDS, &
                   I0,TEMP_FLAG)
        TEMP_FLAG=1
        IF (.NOT. LLCORD) GEN_MID=.TRUE.
      END IF
    END IF
  END IF


!----------------------------------------------------------------------
!   Look for turbulence group
!----------------------------------------------------------------------

  IF (TURB_FLAG  ==  0) THEN
    IF ( (POINT+3) <= REPLEN) THEN
      IF ((REPORT(POINT:POINT)  ==  'T') .OR.  &
          (REPORT(POINT:POINT+2)  ==  'LTT') .OR.  &
          (REPORT(POINT:POINT+3)  ==  'LT T') .OR. &
          (REPORT(POINT:POINT+3)  ==  'MODT') .OR. &
          (REPORT(POINT:POINT+3)  ==  'CODE')) THEN
        TURB_FLAG=1
        CALL AIRTRB(REPORT,POINT,OPT_TURB)
        CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
      END IF
    END IF
  END IF

!----------------------------------------------------------------------
!   Look for icing group
!----------------------------------------------------------------------

  IF (ICE_FLAG  ==  0) THEN
    IF ( (POINT+1) <= REPLEN) THEN
      IF((REPORT(POINT:POINT)  ==  'I') .OR.  &
         (REPORT(POINT:POINT+1)  ==  'LI')) THEN
        ICE_FLAG=1
        CALL AIRICE(REPORT,POINT,OPT_ICE)
      END IF
    END IF
  END IF

!----------------------------------------------------------------------
! Check for end of report
!----------------------------------------------------------------------
  IF ( POINT <= REPLEN) THEN
    IF (REPORT(POINT:POINT) == '=') END_FLAG=1
  END IF

!----------------------------------------------------------------------
!Check to see if any group has been decoded this time round. If there
!hasnt been something decoded move the pointer onto the next group
!----------------------------------------------------------------------

  IF ((MID_FLAG == MID_OLD) .AND. (ATP_FLAG  ==  ATP_OLD) .AND. &
      (TEMP_FLAG == TEMP_OLD) .AND. (WIND_FLAG  ==  WIND_OLD) .AND. &
      (TURB_FLAG == TURB_OLD) .AND. (ICE_FLAG  ==  ICE_OLD) .AND.   &
      (LAT_FLAG == LAT_OLD)) THEN
    CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
  ELSE
    MID_OLD=MID_FLAG             !update all the flags to the
    ATP_OLD=ATP_FLAG            !current settings, since at least
    TEMP_OLD=TEMP_FLAG           !one flag will have changed.
    WIND_OLD=WIND_FLAG
    TURB_OLD=TURB_FLAG
    ICE_OLD=ICE_FLAG
  END IF

  COUNT=COUNT+1
END DO dowhile1

!----------------------------------------------------------------------
!If we have been unable to resolve a Lat/Long position for the MID
!point data we can generate a position by looking at previously stored
!reports and the current position.
! AIRMID tries to find the previous report from this aircraft
!and returns a midpoint lat/long if succesful.  If a second
!latitude or longitude was reported (usually only longitude),
!it is kept as TLONG (or TLAT) and the calculated midpoint
!checked against it.  If midpoint latitude & longitude are
!both reported, they are put with the calculated time without
!any check - dangerous?
!----------------------------------------------------------------------

iflabel3: &
IF (WIND_FLAG > 0 .OR. TEMP_FLAG > 0 .OR. MID_FLAG > 0) THEN
  TLAT=LAT2
  TLONG=LONG2
  CALL AIRMID(TIMEDD,TIMEHH,TIMEMM,MID_YY,MID_MTH,MID_DD, &
  MID_HH,MID_MM,LAT,LONG,CALC_LAT,CALC_LONG,SIGN,MATCH,NFT)
  LAT2=CALC_LAT
  LONG2=CALC_LONG

! If MID_FLAG & GEN_MID are both set, MID may have been followed
! by a longitude which can be checked against that from AIRMID:
! give up if they disagree.

  IF (GEN_MID) THEN
    IF ((LAT2  <  -999) .OR. (LONG2  < -999)) THEN
      MID_YY=MISSING
    ELSE
      IF (TLONG /= MISSING .AND.   &
          ABS(TLONG-LONG2) > 0.01) THEN
        MID_YY=MISSING
        PRINT *,'AIROPT: midpoint found but longitude reported'
        PRINT *,' and the two are different:',TLONG,LONG2
      END IF
      IF (TLAT /= MISSING .AND.    &
          ABS(TLAT-LAT2) > 0.01) THEN
        MID_YY=MISSING
        PRINT *,'AIROPT: midpoint found but latitude reported'
        PRINT *,' and the two are different:',TLAT,LAT2
      END IF
    END IF
  ELSE
    IF (TIMEDD  >  -9999) THEN
      LAT2=TLAT
      LONG2=TLONG
    ELSE
      MID_YY=MISSING
      LAT2=MISSING
      LONG2=MISSING
    END IF
  END IF

  IF (MID_YY /= MISSING) THEN
    MIDZ=MID_HH*100+MID_MM
    NEWZ=TIMEHH*100+TIMEMM
    WRITE(6,'(A10," midpoint report at ",I4.4,"Z from ",   &
            &I4.4,"Z ob. Lat/long:",2F7.2)')SIGN,MIDZ,NEWZ,LAT2,LONG2
  END IF
END IF iflabel3
DECERR=0

!----------------------------------------------------------------------
!Returns to AIRARP
!----------------------------------------------------------------------
RETURN
END SUBROUTINE AIROPT
