SUBROUTINE SYNEXP(IDENT,REPORT,REPLEN,FREXP,YYGGIW,IDATIM,LATLON, &
                  MIMJ,SYNTAX,NELM)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNEXP
!
! PURPOSE       : SYNOP EXPANSION PROGRAM TO PUT A SINGLE REPORT INTO
!                 AN ARRAY OF REAL ELEMENTS READY FOR BUFR ENCODING.
!
! DESCRIPTION   :
!  1. THE MANDATORY GROUPS IN SECTIONS 0 & 1 OF A SYNOP ARE EXPANDED
!     FIRST. IF THIS WORKS, OTHER SECTIONS ARE LOOKED FOR, INCLUDING
!     SECTION 2 AS SOME COASTAL STATIONS USE IT. SECTION GROUP COUNTS
!     & STARTS ARE KEPT & USED TO EXPAND A REPORT A SECTION AT A TIME.
!     SYNOPS USE 5-FIG GROUPS EXCEPT FOR SECTION MARKERS 333/444/555.
!     MTRLOC IS USED TO INCREMENT THE POINTER THROUGH THE REPORT.
!  2. TIME OF REPORT AND THE YYGGIW GROUP FROM THE BULLETIN HEADING
!     ARE USED IF AVAILABLE, BUT A 9-GROUP TIME WILL TAKE PRECEDENCE.
!  3. MARINE AND RADIATION SECTIONS ARE OPTIONAL FOR LAND SYNOPS: A
!     ZERO REPLICATION COUNT WILL SKIP THOSE ELEMENTS IN BUFR ENCODING
!  4. SECTIONS 0 TO 5 :  SECTION 0 STATION - MANDATORY
!                     :  SECTION 1 TWO MANDATORY GROUPS + MAIN INFO
!                     :  SECTION 2 SHIP/COASTAL SECTION
!                     :  SECTION 3 REGIONALLY DEFINED SECTION
!                     :  SECTION 4 MOUNTAIN CLOUD SECTION (NATIONAL)
!                     :  SECTION 5 NATIONAL USE ONLY
!
!  NOTE: ON ENTRY THE FIRST GROUP SHOULD BE A 5 FIGURE BLOCK/STATION
!        NUMBER (03 INSERTED FOR 3-FIG BRITISH STATIONS BY SYNBUL) FOR
!        A LAND SYNOP; SHIPS & MOBILE SYNOPS START WITH SECTION 1.
!  NOTE: THE ELEMENT VARIABLE NAMES CORRESPOND AS MUCH AS POSSIBLE
!        TO THOSE USED IN THE SYNOP CODE DEFINITION.
!  NOTE: THE ELEMENTS IN THE MAIN LIST ARE INITIALIZED BY SYNIT.
!  NOTE: THE EXPANSION ARRAY REXP HAS FIXED DISPLACEMENTS AND MISSING
!        DATA FOR ELEMENTS NOT REPORTED (MARINE/RADIATION).
!        FREXP IS EDITED FOR DELAYED REPLICATIONS & MISSING SECTIONS
!        SO THAT IT IS READY TO PASS TO THE BUFR ENCODING.
!  NOTE: The mandatory groups (iRixhVV & Nddff) are tested to see if
!        they are followed by either a space or a slash. The option of
!        a slash is included in tests as it avoids rejecting otherwise
!        good reports.
!
! DATA TYPE(S)  : SYNOPS  (WMO CODE FM 12, 13 AND 14)
!
! CALLED BY     : SYNOB
!
! CALLS         : SYNIT (INITIALISE), MTRLOC (DELIMIT GROUP),
!                 SYNREG (WORK OUT WMO REGION FROM WMO BLK AND STN.),
!                 FFUNIT (WIND UNITS), STAPOS, SYNRPR (PERIOD),
!                 SYNXP1, SYNXP2, SYNXP3, SYNXP4, SYNXP5 (SECTIONS),
!                 SYNQC (QC: INTERNAL CONSISTENCY), SYNSEQ OR SHPSEQ,
!                 ONLAND, DATE31/DATE13, OUKSTN (function)
!
! ARGUMENTS     : (1)IDENT    STATION NUMBER OR CALL SIGN      (I)
!                 (2)REPORT   SINGLE REPORT                    (I)
!                 (3)REPLEN   REPORT LENGTH                    (I)
!                 (4)FREXP    ARRAY OF VALUES                  (O)
!                 (5)YYGGIW   FROM BULLETIN HEADING            (I)
!                 (6)IDATIM   ARRAY YY,MM,DD,HH,MIN            (I/O)
!                 (7)LATLON   ARRAY LAT,LON  (INPUT FOR SHIPS) (I/O)
!                 (8)MIMJ     TO DECIDE IF LAND OR SHIP        (I)
!                 (9)SYNTAX   TRUE IF SYNTAX ERROR FOUND       (I/O)
!                (10)NELM     NUMBER OF GOOD ELEMENTS          (O)
!                              (NELM=0 TO REJECT REPORT)
!
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

USE FFUNIT_mod
USE METDB_COM_mod, ONLY : MISSIN, RMISS ! missing data value
USE MTRLOC_mod
USE OUKSTN_mod      ! function
USE SHPSEQ_mod
USE STAPOS_mod
USE SYNIT_mod
USE SYNQC_mod
USE SYNREG_mod
USE SYNRPR_mod
USE SYNSEQ_mod
USE SYNXP1_mod
USE SYNXP2_mod
USE SYNXP3_mod
USE SYNXP4_mod
USE SYNXP5_mod
USE ZPDATE_mod

IMPLICIT NONE

! Interface arguments

CHARACTER (LEN=*), INTENT(IN)  :: IDENT
CHARACTER (LEN=*), INTENT(IN)  :: REPORT
INTEGER, INTENT(IN)            :: REPLEN
REAL, INTENT(INOUT)            :: FREXP(:)  ! Copy of Array REXP, minus associated
!                                             data for replication count
CHARACTER (LEN=5), INTENT(IN)  :: YYGGIW
INTEGER, INTENT(INOUT)         :: IDATIM(5)
REAL, INTENT(INOUT)            :: LATLON(4)
CHARACTER (LEN=4), INTENT(IN)  :: MIMJ
LOGICAL, INTENT(OUT)           :: SYNTAX
INTEGER, INTENT(OUT)           :: NELM

! Local Parameters

REAL, PARAMETER  :: KTS2MPS = 0.5144444444 ! 1852/3600 IS THE STANDARD

! Local Variables

INTEGER  ::  CENDAY
INTEGER  ::  GRPCNT
INTEGER  ::  GRPLEN
INTEGER  ::  I
INTEGER  ::  ICL
INTEGER  ::  IHOUR
INTEGER  ::  IMT
INTEGER  ::  INSHIP ! Ship identifier
INTEGER  ::  IR
INTEGER  ::  IRC
INTEGER  ::  ISWELL
INTEGER  ::  IVALUE
INTEGER  ::  IWMOB
INTEGER  ::  IWMOR
INTEGER  ::  IWMOS
INTEGER  ::  IX
INTEGER  ::  J
INTEGER  ::  L
INTEGER  ::  LGROUP
INTEGER  ::  NELEM
INTEGER  ::  NGRP
INTEGER  ::  NGRPS
INTEGER  ::  NSECT
INTEGER  ::  POINT
INTEGER  ::  POINT1
INTEGER  ::  POINT2
INTEGER  ::  SECGPS(5)   ! SECTION STARTS & GROUP COUNTS
INTEGER  ::  SECNO       ! SECTION STARTS & GROUP COUNTS
INTEGER  ::  SECPTR(5)   ! SECTION STARTS & GROUP COUNTS

REAL  :: ATEND       !a0x Pressure tendency amount (a)
REAL  :: A3          !a0x Standard isobaric surface (a3)
REAL  :: BI          !a0x Amount & type of sea ice(bi)
REAL  :: CH          !a0x Type High Cloud (CH)
REAL  :: CI          !a0x Sea ice concentration(ci)
REAL  :: CL          !a0x Type Low Cloud (CL)
REAL  :: CM          !a0x Type Medium Cloud (CM)
REAL  :: DD          !a0x Wind direction (dd)
REAL  :: DI          !a0x Bearing of nearest sea ice(Di)
REAL  :: DISO        !a0x 1 hour diffuse radiation
REAL  :: DISO24      !a0x 24 hour diffuse radiation
REAL  :: DS          !a0x Direction of movement (Ds)
REAL  :: E           !a0x State of Ground (E/E') '
REAL  :: EEE         !a0x Evaporation or evapotranspiration (EEE)
REAL  :: ESES        !a0x Ice thickness on ship(EsEs)
REAL  :: FF          !a0x Wind speed (ff/fff)
REAL  :: GLSO        !a0x 1 hour global radiation
REAL  :: GLSO24      !a0x 24 hour global radiation
REAL  :: GROUP8(12)  !a0x "8" Cloud group elements(4*NsChshs)
REAL  :: HHH         !a0x Geopotential height (hhh)
REAL  :: HL          !a0x Height of base of (lowest) cloud (h)
REAL  :: HLCONV(0:9)
REAL  :: HOUR        !a0x Hour (GG)
REAL  :: IE          !a0x Type of instrumentation for evaporation (iE)
REAL  :: ISICE       !a0x Cause of ice accretion on ship (Is)
REAL  :: IW          !a0x Wind Speed Units indicator (iw)
REAL  :: LAT         !a0x Latitude (LaLaLa or Station Master)
REAL  :: LLFLAG
REAL  :: LON         !a0x Longitude (LoLoLoLo or Station Master)
REAL  :: LW          !a0x 1 hour Longwave radiation
REAL  :: LW24        !a0x 24 hour Longwave radiation
REAL  :: MAXTTT      !a0x Max Air Temperature (TxTxTx)
REAL  :: MINS        !a0x Minutes (gg)
REAL  :: MINTTT      !a0x Min Air Temperature (TnTnTn)
REAL  :: MTCL(4,2)   !a0x Mountain Clouds (2*NXCXHXHXCt)
REAL  :: NH          !a0x Amount of low cloud (Nh)
REAL  :: NT          !a0x Total cloud amount (N)
REAL  :: PCHANG      !a0x 3 hour Pressure change amount(ppp)
REAL  :: PCHG24      !a0x 24 hour Pressure change amount (P24P24P24)
REAL  :: PPPP        !a0x MSL pressure (PPPP)
REAL  :: PSEQ
REAL  :: P0P0P0      !a0x Station level Pressure (P0P0P0P0)
REAL  :: REXP(2,150) ! ARRAY USED TO LOAD DATA INTO.
REAL  :: RNET        !a0x 1 hour Net radiation
REAL  :: RNET24      !a0x 24 hour Net radiation
REAL  :: RRR         !a0x Section 1 Precipitation amount (RRR)
REAL  :: RRR3        !a0x Section 3 Precipitation amount (RRR)
REAL  :: RS          !a0x Rate of ice accretion on ship(Rs)
REAL  :: R24         !a0x 24 hour Precipitation amount (R24R24R24R24)
REAL  :: STNHTP
REAL  :: SDIR        !a0x 1 hour direct solar radiation
REAL  :: SW          !a0x 1 hour shortwave radiation
REAL  :: SWNET       !a0x 1 hour net shortwave radiation
REAL  :: SDIR24      !a0x 24 hour direct solar radiation
REAL  :: SEASTE      !a0x State of sea 924 group (S)
REAL  :: SEAVIS      !a0x Horiz vis towards sea 924 group (Vs)
REAL  :: SI          !a0x Sea ice development(Si)
REAL  :: SSS         !a0x Snow depth (sss)
REAL  :: STNHT       !a0x Station height
REAL  :: STNTYP      !a0x Station type (ix)
REAL  :: SUN1        !a0x 1 hour Sunshine amount (SS)
REAL  :: SUN2        !a0x 24 hour Sunshine amount (SSS)
REAL  :: SWELWV(6)   !a0x Swell Waves (2*dwdwPwPwHwHw)
REAL  :: SWNET24     !a0x 24 hour net shortwave radiation
REAL  :: SW24        !a0x 24 hour shortwave radiation
REAL  :: TBTBTB      !a0x Wet-bulb temperature(TbTbTb)
REAL  :: TBTYPE      !a0x Wet-bulb temperature measurement method (sw)
REAL  :: TDTDTD      !a0x Dew point temperature (TdTdTd)
REAL  :: TGTG        !a0x Grass minimum temperature (TgTg)
REAL  :: TPER1       !a0x Max temp time period
REAL  :: TPER2       !a0x Min temp time period
REAL  :: TR          !a0x Section 1 type of precipitation (tR)
REAL  :: TR3         !a0x Section 3 type of precipitation (tR)
REAL  :: TTT         !a0x Air temperature (TTT)
REAL  :: TWTWTW      !a0x Sea temperature(TwTwTw)
REAL  :: TWTYPE      !a0x SST measurement method (ss)
REAL  :: T1GUS       !a0x Start gust period of gust 910 group
REAL  :: T2GUS       !a0x End gust period of gust 910 group
REAL  :: T1GUST      !a0x Start gust period of gust 911 group
REAL  :: T2GUST      !a0x End gust period of gust 911 group
REAL  :: T1MEAN      !a0x Start mean period of gust 912 group
REAL  :: T2MEAN      !a0x End mean period of gust 912 group
REAL  :: UUU         !a0x Relative humidity (UUU)
REAL  :: VERTV       !a0x Vertical visibilty
REAL  :: VGUS        !a0x 910 group gust value (ff/fff)
REAL  :: VGUST       !a0x 911 group gust value (ff/fff)
REAL  :: VISCONV(90:99)
REAL  :: VMEAN       !a0x 912 group mean value (ff/fff)
REAL  :: VS          !a0x Speed of movement(vs)
REAL  :: VSIF(4)     !a0x Significance of cloud layers
REAL  :: VV          !a0x Visibility (VV)
REAL  :: WMOBLK      !a0x WMO Block Number (II)
REAL  :: WMOREG      !a0x WMO Region
REAL  :: WMOSTA      !a0x WMO Station Number (iii)
REAL  :: WPER        !a0x Past weather period
REAL  :: WVHT        !a0x Wave height (HwaHwa)
REAL  :: WVMS        !a0x Wave measurement method
REAL  :: WVPER       !a0x Wave period (PwaPwa)
REAL  :: WW          !a0x Present Weather (ww)
REAL  :: W1          !a0x Past Weather 1 (W1)
REAL  :: W2          !a0x Past Weather 2 (W2)
REAL  :: ZI          !a0x Sea ice situation (zi)

LOGICAL  ::  BADGRP
LOGICAL  ::  DATVAL  ! TRUE IF NO DATA VALUES FOUND
LOGICAL  ::  ERR     ! TRUE IF DATA VALUE ERROR
LOGICAL  ::  IWREP
LOGICAL  ::  LAUTO
LOGICAL  ::  LRAD
LOGICAL  ::  LREPFL
LOGICAL  ::  LSEA
LOGICAL  ::  MOBILE  ! Flag set to .TRUE. if mobile SYNOP
LOGICAL  ::  MSG     ! 1.26 Flag passed to SYNQC for diagnostics
LOGICAL  ::  ONLAND
LOGICAL  ::  SHIP
LOGICAL  ::  TEST1   ! Flag for outcome of NDDFF test
LOGICAL  ::  UKSHIP  ! Flag set to .TRUE. if a UK ship
LOGICAL  ::  WNDKTS
LOGICAL  ::  WXMAN


CHARACTER (LEN=109)  :: METELM

! METELM - THIS STRING IS USED TO COUNT METEOROLOGICALLY USEFUL DATA.
! IT REPRESENTS THE FIRST 109 ELEMENTS OF THE ARRAY REXP (50 ON FIRST
! LINE OF DATA STMT, 59 ON SECOND).   SET=1 IF ELEMENT IS TO BE COUNTED,
! SET=0 IF IT IS FIXED, INSTRUMENTAL OR COORDINATE INFORMATION.

DATA  METELM &
      /'00000000000000111111110111010110101100100111011111&
       &11101011111111111111101100010100011011111111111111010101011'/

DATA  VISCONV /30.,60.,200.,500.,1000.,2000.,4000.,10000.,  &
               20000.,50000./
DATA  HLCONV /25.,50.,100.,200.,300.,600.,1000.,1500.,      &
              2000.,2500./

! --------------------------------------------------------------------

! INITIALISE ALL THE TEMPORARY VARIABLES HOLDING ELEMENT VALUES

CALL SYNIT(                                                &
        WMOBLK,WMOSTA,WMOREG,LAT,LON,STNHT,STNTYP,         &
        DD,FF,TTT,TDTDTD,UUU,VV,VERTV,WW,WPER,             &
        W1,W2,NT,TPER1,                                    &
        MAXTTT,TPER2,MINTTT,E,TR,RRR,TR3,RRR3,R24,         &
        T1GUST,T2GUST,VGUST,T1MEAN,T2MEAN,VMEAN,PPPP,      &
        P0P0P0,A3,HHH,SSS,TGTG,ATEND,PCHANG,PCHG24,        &
        IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR,  &
        RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,     &
        SEAVIS,SEASTE,TWTYPE,TWTWTW,                       &
        DS,VS,TBTYPE,TBTBTB, WVMS,WVHT,WVPER,SWELWV,       &
        ISICE,ESES,RS,CI,SI,BI,DI,ZI,                      &
        CL,CM,CH,NH,HL,VSIF,GROUP8,MTCL,IW,HOUR,MINS,      &
        T1GUS,T2GUS,VGUS)

STNHTP=RMISS

DO I=1,150     ! ARRAY WITH FIXED SLOTS FOR Q/C
  DO J=1,2
    REXP(J,I)=RMISS
  END DO
END DO
!                    ! SECTION POINTERS & GROUP COUNTS
DO J=1,5
  SECPTR(J)=0
  SECGPS(J)=0
END DO

SHIP=MIMJ == 'BBXX'
MOBILE=MIMJ == 'OOXX'
LREPFL=.FALSE.  ! END OF REPORT FOUND FLAG
LRAD=.FALSE.    ! RADIATION SECTION USED FLAG.
LSEA=.FALSE.    ! COASTAL DATA FLAG.
LAUTO=.FALSE.   ! AUTO OR SEMIAUTO STATION FLAG
WXMAN=.TRUE.    ! DEFAULT WEATHER CODES USED ARE 'MANNED'
SYNTAX=.FALSE.

UKSHIP=.FALSE.  ! UK ship flag
INSHIP=-9999999 ! Numeric ship identifier set to missing

NELM=0         ! RETURN WITH NELM=0 TO REJECT REPORT

NGRP=0         ! NUMBER OF GROUPS FOUND IN REPORT.
NSECT=0        ! NUMBER OF SYNOP SECTIONS FOUND
NELEM=4        ! NUMBER OF ELEMENTS REQUESTED FROM STATIONMASTER
GRPCNT=0       ! NO. OF CURRENT GROUP
ISWELL=0       ! SWELL WAVE GROUP COUNT, SECTION 2.
ICL=0          ! COUNT OF CLOUD GROUPS
IMT=0          ! COUNT OF MOUNTAIN CLOUD GROUPS

POINT2=1       ! POINT TO FIRST GROUP
!*********************************************************************
! GET WIND REPORTING UNITS FROM YYGGIW.
! IF IW IS MISSING, UNITS WILL BE FOUND BY WMO BLOCK NUMBER LATER.
! CONVERT SYNOP CODE TO BUFR CODE:
! IW SYNOP CODE TBL 1855, BUFR FLAG TBL 2002
! SET WNDKTS IF WIND SPEEDS NEED TO BE CONVERTED TO M/S
!*********************************************************************
IWREP=.FALSE.

IFBLOCK1: &
IF(YYGGIW /= ' ')THEN
  I=IVALUE(YYGGIW(5:5))
  IF(I /= MISSIN)THEN
    IF(I >= 3)THEN
      WNDKTS=.TRUE.
      IF(I == 3)IW=4.
      IF(I == 4)IW=12.
    ELSE
      WNDKTS=.FALSE.
      IF(I == 0)IW=0.
      IF(I == 1)IW=8.
    END IF
    IWREP=.TRUE.
  END IF
END IF IFBLOCK1
!********************************************************************
!
! EXPAND SECTION 0 (JUST THE STATION NUMBER) IF IT IS A LAND SYNOP
!
!********************************************************************

! KEEP POINT1 POINTING TO THE FIRST GROUP & MOVE POINT2 TO THE NEXT.
! MTRLOC RETURNS THE LENGTH OF THE FIRST GROUP, SETTING LREPFL IF
! THERE ARE NO MORE GROUPS. (WHEN 5-FIGURE GROUPS HAVE BEEN COUNTED
! THERE IS NO NEED FOR MTRLOC; USE POINT INSTEAD OF POINT1 & POINT2.)

IFBLOCK2: &
IF (.NOT.SHIP .AND. .NOT.MOBILE) THEN
  POINT1=POINT2
  CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
  IF (GRPLEN /= 5 .OR. LREPFL) RETURN

! ASSUME STATION ID IS THE FIRST GROUP

  IWMOB=IVALUE(REPORT(POINT1:POINT1+1))
  IWMOS=IVALUE(REPORT(POINT1+2:POINT1+4))
  CALL SYNREG(IWMOB,IWMOS,IWMOR)
  WMOBLK=IWMOB
  WMOSTA=IWMOS
  WMOREG=IWMOR

! SET DEFAULT WIND REPORTING UNITS ACCORDING TO BLOCK AND STN NUMBER

  IF(.NOT.IWREP)THEN
    CALL FFUNIT(IWMOB,IWMOS,I)
    IF(I == 3.OR.I == 4)THEN
      WNDKTS=.TRUE.
    ELSE
      WNDKTS=.FALSE.
    END IF
  END IF

!-----------------------------------------------------------------------
! Get station latitude, longitude, height from the abbreviated station
! list. Preference is to get the surface station details 'S'.
!-----------------------------------------------------------------------

  CALL STAPOS(IWMOB*1000+IWMOS,'S',LAT,LON,STNHTP,STNHT,IRC)

  IF (LAT == RMISS .AND. LON == RMISS) THEN
    IRC=0
    CALL STAPOS(IWMOB*1000+IWMOS,'X',LAT,LON,STNHTP,STNHT,IRC)
  END IF

  IF (LAT == RMISS .AND. LON == RMISS) THEN
    IRC=0
    CALL STAPOS(IWMOB*1000+IWMOS,'U',LAT,LON,STNHTP,STNHT,IRC)
  END IF

! If still no station details output error message

  IF (LAT == RMISS .AND. LON == RMISS) THEN
    WRITE(6,*) 'NO STATION DETAILS- ',IWMOB*1000+IWMOS
  END IF
!***********************************************************************
! Note. The next line of code affects how station heights are set in
! station height (STNHT). Users requested that old coding practice
! for station height be retained when pressure sensor height (STNHTP)
! was additionally made available on retrieval. When they are ready
! to use separate values remove or comment out the line.
!***********************************************************************
  IF (STNHTP /= RMISS.AND.STNHT == RMISS) STNHT=STNHTP
  LATLON(1)=LAT
  LATLON(2)=LON           ! PUT LAND LAT/LONG IN ARRAY FOR INDEX
ELSE                      ! IF SHIP, LAT/LONG ALREADY FOUND

! For mobile SYNOPs, skip Marsden square group and get station height.

  IF (MOBILE) THEN
    CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
    POINT1 = POINT2
    CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)
    I = IVALUE(REPORT(POINT1+4:POINT1+4))
    IF (I >= 1 .AND. I <= 8) THEN   ! Good unit indicator
      STNHT = IVALUE(REPORT(POINT1:POINT1+3))
      IF (I > 4) THEN   ! Height in feet
        STNHT = STNHT*0.3048
      END IF
    END IF
  END IF

  LAT=LATLON(1)
  LON=LATLON(2)

!-----------------------------------------------------------------------
! Recognise UK ships to enable wind gust data obtained from
! section 5 from lightships and moored buoys, to be decoded and stored.
! First check if it is a ship whose identifier starts with '6' (UK)
!-----------------------------------------------------------------------

  IF(IDENT(1:1)  ==  '6' ) THEN

!-----------------------------------------------------------------------
! Attempt to convert the first 5 characters of the ship identifier      )
! (IDENT) to an integer using function IVALUE.
! IVALUE will return -9999999 if non-digits are found.
!-----------------------------------------------------------------------

    INSHIP = IVALUE(IDENT(1:5))

!Check if the ship is a UK ship if valid value for INSHIP

    IF (INSHIP > 0) UKSHIP = OUKSTN(INSHIP)

  END IF
END IF IFBLOCK2

!***********************************************************************
! FIND STARTS OF OTHER SECTIONS AND COUNT GROUPS IN EACH SECTION.
! SECPTR & SECGPS ARE START & GROUP COUNT FOR THE SECTIONS FOUND.
! SECGPS=0 FOR MISSING SECTIONS.  COUNT DOES NOT INCLUDE NNN-GROUP
! EXCEPT FOR 222..; AND COUNT FOR 222 SECTION MAY INCLUDE 'ICE'.
!  CHECK THAT THE SECTION HAS NOT BEEN FOUND BEFORE - ESPECIALLY
! IMPORTANT FOR SHIPS'' SECTION 2, BECAUSE 222.. MAY OCCUR LATER!
!********************************************************************

SECPTR(1)=POINT2           ! START OF SECTION 1
SECNO=1                    ! HANDLING SECTION 1
NGRP=0
BADGRP=.FALSE.             ! WILL BE SET IF GROUP NOT 5 FIGURES

20    CONTINUE             ! BACK TO HERE IN LOOP ROUND GROUPS

POINT1=POINT2              ! START OF NEXT GROUP

CALL MTRLOC(REPLEN,REPORT,POINT2,GRPLEN,LREPFL)

! IF THE GROUP LENGTH IS 3

IFBLOCK3: &
IF (GRPLEN == 3) THEN

! CHECK FOR GLOBAL EXCHANGE SECTION, SECTION 3

IFBLOCK4: &
  IF (REPORT(POINT1:POINT1+2) == '333'  &
     .AND. SECGPS(3) == 0) THEN
    SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION
    SECNO=3                ! HANDLING SECTION 3
    NGRP=0                 ! RESET GROUP COUNT TO ZERO
    SECPTR(3)=POINT2       ! START OF FIRST 5-FIGURE GROUP
    BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS

!   CHECK FOR REGIONAL EXCHANGE SECTION, SECTION 4, MOUNTAIN CLOUD DATA.

  ELSE IF (REPORT(POINT1:POINT1+2)     &
   == '444'.AND. SECGPS(4) == 0) THEN
    SECGPS(SECNO)=NGRP   ! GROUP COUNT FOR PREVIOUS SECTION
    SECNO=4              ! HANDLING SECTION 4
    NGRP=0               ! RESET GROUP COUNT TO ZERO
    SECPTR(4)=POINT2     ! START OF FIRST 5-FIGURE GROUP
    BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS

!   CHECK FOR NATIONAL EXCHANGE SECTION, SECTION 5.

  ELSE IF (REPORT(POINT1:POINT1+2)     &
          == '555'.AND. SECGPS(5) == 0) THEN
    SECGPS(SECNO)=NGRP   ! GROUP COUNT FOR PREVIOUS SECTION
    SECNO=5              ! HANDLING SECTION 5
    NGRP=0               ! RESET GROUP COUNT TO ZERO
    SECPTR(5)=POINT2     ! START OF FIRST 5-FIGURE GROUP
    BADGRP=.FALSE.       ! RESET FLAG FOR NON-5-FIGURE GROUPS
  ELSE IF (REPORT(POINT1:POINT1+2) == 'ICE' &
         .AND.SECNO == 2) THEN
    NGRP=NGRP+1          ! INCLUDE 'ICE' IN SECTION 2 COUNT.
  ELSE
! IF THE GROUP LENGTH IS 3 AND NOT ONE OF THE ABOVE TYPES THEN IT IS BAD
    BADGRP=.TRUE.
  END IF IFBLOCK4

!****** GROUP LENGTH IS 5

ELSE IF (GRPLEN == 5) THEN

!  CHECK FOR COASTAL SECTION, SECTION 2.  N.B. IN THIS CASE (ONLY)
!  PASS THE 222.. GROUP ITSELF TO THE SECTION-HANDLING PROGRAM: 222
!  IS FOLLOWED BY INFORMATION IN THE CASE OF SHIPS.

  IFBLOCK5: &
  IF (SECGPS(2) == 0 .AND.                     &
     (REPORT(POINT1:POINT1+4) == '222//' .OR.  &
      REPORT(POINT1:POINT1+4) == '22200' .OR.  &
  (SHIP .AND. REPORT(POINT1:POINT1+2) == '222'))) THEN
    SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION
    SECNO=2                ! HANDLING SECTION 2
    NGRP=1                 ! RESET GROUP COUNT TO ONE
    SECPTR(2)=POINT1       ! KEEP START OF SECTION
    BADGRP=.FALSE.         ! RESET FLAG FOR NON-5-FIGURE GROUPS
  ELSE IF ((REPORT(POINT1:POINT1+4) == '33333' &
     .OR. REPORT(POINT1:POINT1+4) == '333//')  &
     .AND.SECGPS(3) == 0                       &
     .AND.(.NOT.SHIP)) THEN
    SECGPS(SECNO)=NGRP     ! GROUP COUNT FOR PREVIOUS SECTION
    SECNO=3                ! HANDLING SECTION 3
    NGRP=0                 ! RESET GROUP COUNT TO ZERO
    SECPTR(3)=POINT2       ! START OF FIRST 5-FIGURE GROUP
    BADGRP=.FALSE.         ! RESET FLAG FOR NON-5-FIGURE GROUPS
! if grplen is 5 then increment the number of groups in that section
  ELSE
    NGRP=NGRP+1            ! COUNT 5 FIG GROUPS IF not above types
  END IF IFBLOCK5
END IF IFBLOCK3

IF (.NOT.LREPFL) GOTO 20

SECGPS(SECNO)=NGRP
LREPFL=.FALSE.
!**********************************************************************
!
! WE NOW HAVE COUNTS OF 5-FIGURE GROUPS IN THE DIFFERENT SECTIONS,
! STOPPING IF A NON-5-FIGURE GROUP WAS FOUND.
!
! FIRST PROCESS MANDATORY GROUPS IN SECTION 1: IRIXHVV & NDDFF
!  N.B. WHEN FF=99, A GROUP 00FFF FOLLOWS
!
!**********************************************************************
!
! SOME STATIONS DON'T REPORT NDDFF; TO BE SURE IT'S THERE WE WANT EITHER
! ..... ..... 1S... (WHERE S IS 0 0R 1) OR ..... ...99 00... 1S...
! - BUT THIS WOULD BE TOO RESTRICTIVE, 1- & 2-GROUPS MAY BE LEFT OUT!
! SO JUST SKIP FIRST GROUP IF THE PATTERN IS ..... 1S... 2S...
!
!**********************************************************************
POINT=SECPTR(1)
NGRPS=SECGPS(1)  ! NO. OF GROUPS IN THIS SECTION

IFBLOCK6: &
IF (NGRPS <= 1) THEN
!       PRINT*,' SYNEXP NGRPS <= 1 '
!       PRINT*,' >',REPORT(1:REPLEN),'<'
  RETURN
ELSE IF (NGRPS == 2) THEN

! TEST FIRST TWO GROUPS TO FIND IF ALL CHARACTERS ARE VALID AND THERE IS
! AT LEAST ONE VALID DATA VALUE (IRIXHVV NDDFF).

  ERR=.FALSE.
  DATVAL=.FALSE.
! IR
  IF (REPORT(POINT:POINT) < '1' .OR.        &
      REPORT(POINT:POINT) > '4')ERR=.TRUE.
! IX
  IF (REPORT(POINT+1:POINT+1) < '1' .OR.    &
      REPORT(POINT+1:POINT+1) > '7')ERR=.TRUE.
! h
  IF (REPORT(POINT+2:POINT+2) >= '0' .AND.  &
      REPORT(POINT+2:POINT+2) <= '9')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+2:POINT+2) == '/')THEN
  ELSE
     ERR=.TRUE.
  END IF
! VV
  IF (REPORT(POINT+3:POINT+4) >= '00' .AND. &
      REPORT(POINT+3:POINT+4) <= '99')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+3:POINT+4) == '//')THEN
  ELSE
     ERR=.TRUE.
  END IF
! Test if group is followed by either a space or a slash. The slash is
! included as a group seperator as it avoids rejecting otherwise good
! reports.
  IF (REPORT(POINT+5:POINT+5) /= ' ' .and. &
      REPORT(POINT+5:POINT+5) /= '/') THEN
     ERR=.TRUE.
     PRINT*,' SYNEXP space missing after iRiXhVV group ',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'<'
  END IF
! N
  IF (REPORT(POINT+6:POINT+6) >= '0' .AND.  &
      REPORT(POINT+6:POINT+6) <= '9')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+6:POINT+6) == '/')THEN
  ELSE
     ERR=.TRUE.
  END IF
! dd
  IF (REPORT(POINT+7:POINT+8) >= '00' .AND. &
      REPORT(POINT+7:POINT+8) <= '36')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+7:POINT+8) == '99')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+7:POINT+8) == '//')THEN
  ELSE
     ERR=.TRUE.
  END IF
! ff
  IF (REPORT(POINT+9:POINT+10) >= '00' .AND. &
      REPORT(POINT+9:POINT+10) <= '99')THEN
     DATVAL=.TRUE.
  ELSE IF(REPORT(POINT+9:POINT+10) == '//')THEN
  ELSE
     ERR=.TRUE.
     PRINT*,' SYNEXP ff >',REPORT(POINT+9:POINT+10),'< Datim =',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN)
  END IF
! Test if group is followed by either a space or a slash. The slash is
! included as a group seperator as it avoids rejecting otherwise good
! reports.
  IF (REPLEN > (POINT+11))THEN
    if(REPORT(POINT+11:POINT+11) /= ' ' .and. &
       REPORT(POINT+11:POINT+11) /= '/') THEN
      ERR=.TRUE.
      PRINT*,' SYNEXP space missing after Nddff group id>',IDENT,'< Datim =',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'<'
    END IF
  ELSE
!   PRINT*,' SYNEXP short report ',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'< replen =',replen
  END IF

  IF(ERR.OR..NOT.DATVAL)THEN
          PRINT*,' SYNEXP NGRPS == 2  FAILED!!!!!!! ID>',IDENT,'< DATIM =',YYGGIW(1:4),':00Z >',REPORT(POINT:REPLEN),'<'
!   RETURN       >'
  ELSE
  END IF
END IF IFBLOCK6

! UNLESS SECOND & THIRD GROUPS LOOK LIKE 1-GROUP & 2-GROUP (TEMPERATURE)
! ASSUME FIRST TWO GROUPS ARE CLOUD/VIS/WIND (IRIXHVV NDDFF).
! BUT FIRST CHECK THAT THERE ARE AT LEAST 3 GROUPS.

TEST1 = NGRPS >= 3
IF (TEST1) THEN
  TEST1 = REPORT(POINT+6 :POINT+6)  == '1' .AND.  &
          REPORT(POINT+12:POINT+12) == '2'
END IF

IFBLOCK7: &
IF (TEST1) THEN
  POINT=POINT+6                            ! SKIP FIRST GROUP
  NGRPS=NGRPS-1
  PRINT *,'SYNEXP: NO NDDFF?. id>',IDENT,'< Datim =',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN)
ELSE

! ASSUME IR GROUP FIRST

  IR=IVALUE(REPORT(POINT:POINT))

! IF NO RAINFALL GROUP SET PERIOD WITHOUT RAIN FROM HOUR & WMO REGION

  IF (IR == 3) THEN
    RRR=0.
    CALL SYNRPR(IDATIM(4),IWMOR,IWMOB,IWMOS,TR)
  END IF

! IX: SET FLAG TO SAY WHICH PRESENT/PAST WEATHER CODE TABLES ARE USED

  IX=IVALUE(REPORT(POINT+1:POINT+1))
  IF (IX /= MISSIN) THEN
    IF (IX <= 4) THEN
      WXMAN=.TRUE.
    ELSE
      WXMAN=.FALSE.
    END IF
    IF (IX >= 4) LAUTO=.TRUE.
  END IF

! HT OF LOWEST CLOUD

  I=IVALUE(REPORT(POINT+2:POINT+2))
  IF (I /= MISSIN) HL=HLCONV(I)

! VIS: set <100m to 90m (agreed with NWP, Adam Maycock, Jan 2000)
! (This value may be reset later by a 961.. group)

  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) THEN
    IF (I == 0) THEN
      VV=90.
    ELSE IF (I <= 50) THEN
      VV=I*100
    ELSE IF (I >= 56 .AND. I <= 80) THEN
      VV=(I-50)*1000
    ELSE IF (I > 80 .AND. I <= 89) THEN
      VV=(I-80)*5000+30000
    ELSE IF (I > 89) THEN
      VV=VISCONV(I)
    END IF
  END IF
! Test if group is followed by either a space or a slash. The slash is
! included as a group seperator as it avoids rejecting otherwise good
! reports.
  IF (REPORT(POINT+5:POINT+5) /= ' ' .and. &
      REPORT(POINT+5:POINT+5) /= '/') THEN
     PRINT*,' SYNEXP:Rejected after bad iRiXhVV group. id>',IDENT,'< Datim =',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'<'
     NELM=0
     RETURN
  END IF
!
  POINT=POINT+6
  NGRPS=NGRPS-1
!**********************************************************************
!
! NOW NDDFF GROUP(S).  FIRST TOTAL CLOUD AMOUNT
!
!**********************************************************************
  NT=IVALUE(REPORT(POINT:POINT))

! WIND DIRECTION (MAY BE 99; IF SO, PASS 990 TO Q/C & RESET IT AFTER)

  I=IVALUE(REPORT(POINT+1:POINT+2))
  IF (I /= MISSIN) THEN
    DD=I*10.
  END IF

! WIND SPEED (IN FOLLOWING 00FFF GROUP IF 99)

  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= 99) THEN
    FF=I
! Fix to problem 577 in which some Ships use a template
! which includes a '00' group after the wind group even
! if the FF value is less than 99.
! To get round this problem, a check is made if the following
! group starts with '00', and if so then the pointer needs to be
! moved to the start of the '00' group.
    IF (POINT+7 <= LEN(REPORT)) THEN
      IF (REPORT(POINT+6:POINT+7) == '00')  THEN
         POINT = POINT + 6
         NGRPS = NGRPS -1
      END IF
    END IF
  ELSE
    POINT=POINT+6
    NGRPS=NGRPS-1
    IF (REPORT(POINT:POINT+1) == '00') THEN
      FF=IVALUE(REPORT(POINT+2:POINT+4))
    ELSE
      FF=99.
    END IF
  END IF
  IF (WNDKTS .AND. FF > RMISS) FF=FF*KTS2MPS
! Test if group is followed by either a space or a slash. The slash is
! included as a group seperator as it avoids rejecting otherwise good
! reports.
  IF (REPLEN > (POINT+5))THEN
    IF(REPORT(POINT+5:POINT+5) /= ' ' .and. &
      REPORT(POINT+5:POINT+5) /= '/') THEN
      PRINT*,' SYNEXP:Rejected after bad Nddff group. id>',IDENT,'< Datim =',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'<'
      NELM=0
      RETURN
    END IF
  ELSE
!   PRINT*,' SYNEXP short report ',YYGGIW(1:4),':00Z >',REPORT(1:REPLEN),'< replen =',replen
  END IF

  POINT=POINT+6
  NGRPS=NGRPS-1
END IF IFBLOCK7
!***************************************************************
!
! NOW CALL SYNXP1 TO DECODE SECTION 1 FROM TEMPERATURE GROUP ON,
! THEN SUBROUTINES TO HANDLE SECTIONS 2,3,4,5 IF THEY EXIST.
!
!***************************************************************
CALL SYNXP1(REPORT,POINT,NGRPS,SYNTAX,IWMOB,                &
 WXMAN,TTT,TDTDTD,UUU,P0P0P0,PPPP,A3,HHH,ATEND,PCHANG,RRR,  &
  TR,WW,W1,W2,NH,CL,CM,CH,HOUR,MINS,IX,IDATIM)

! SHIP OR COASTAL SECTION (222); NOT ALL ELEMENTS REPORTED IF COASTAL

IF (SECGPS(2) > 0) THEN
  POINT=SECPTR(2)        ! START OF SECTION
  NGRPS=SECGPS(2)        ! NUMBER OF GROUPS IN SECTION
  CALL SYNXP2(REPORT,POINT,NGRPS,SYNTAX,                    &
              LSEA,ISWELL,TWTYPE,TWTWTW,                    &
              DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV,   &
              ISICE,ESES,RS,CI,SI,BI,DI,ZI,REPLEN)
END IF

! 333 SECTION.  FIRST SET PERIOD OF PAST WEATHER TO 3 HOURS FOR OBS AT
! NOMINAL HOUR 3,9,15,21Z, 6 HOURS FOR A NOMINAL HOUR OF 0,6,12,18Z.
! (SAME PERIOD MAY BE USED FOR WINDS IN 9-GROUPS)

! IF HOUR & MINS SET BY TIME GROUP IN SECTION 1, ADJUST HOUR FOR PERIOD
! checks.

IF (MINS > 30 .AND. HOUR /= RMISS)THEN
  IF (HOUR == 23) THEN
    IHOUR=0
  ELSE
    IHOUR=HOUR+1
  END IF
ELSE
  IHOUR=IDATIM(4)
END IF

IF (MOD(IHOUR,3) == 0) THEN
  WPER=-6+MOD(IHOUR,6)    ! -6 PLUS 0 OR 3 IF MAIN/INTERMED HOUR
ELSE
  WPER=-1                 ! OTHERWISE ASSUME HOURLY OBS
END IF

IF (SECGPS(3) > 0) THEN
  POINT=SECPTR(3)        ! START OF SECTION
  NGRPS=SECGPS(3)        ! NUMBER OF GROUPS IN SECTION
  CALL SYNXP3(REPORT,POINT,NGRPS,SYNTAX,                      &
   IWMOB,IWMOR,WNDKTS,WPER,IR,IDATIM,TPER1,TPER2,TGTG,VERTV,  &
   A3,HHH,MAXTTT,MINTTT,E,SSS,                                &
   T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS,                     &
   T1MEAN,T2MEAN,VMEAN,ICL,GROUP8,VSIF,RRR3,TR3,R24,IE,EEE,   &
   SUN2,SUN1,RNET,GLSO,DISO,LW,SW,RNET24,GLSO24,              &
   DISO24,LW24,SW24,PCHG24,SEASTE,SEAVIS,VV,LRAD,LSEA)
END IF

! MOUNTAIN CLOUD SECTION (444)

IF (SECGPS(4) > 0) THEN
  POINT=SECPTR(4)        ! START OF SECTION
  NGRPS=SECGPS(4)        ! NUMBER OF GROUPS IN SECTION
! IMT IS ALWAYS 0 OR 2 ON EXIT
  CALL SYNXP4(REPORT,POINT,NGRPS,IMT,MTCL)
END IF

! NATIONAL SECTION (555) ONLY EXPANDED FOR BLOCKS 3 AND OVERSEAS
!                             UK OWNED STATIONS

IF ((WMOBLK == 3.OR.OUKSTN(INT(WMOSTA+WMOBLK*1000.0)).OR.     &
    UKSHIP).AND. SECGPS(5) > 0) THEN
  POINT=SECPTR(5)        ! START OF SECTION (FALKLANDS ADDED F
  NGRPS=SECGPS(5)        ! NUMBER OF GROUPS IN SECTION
  CALL SYNXP5(REPORT,POINT,NGRPS,WNDKTS,T1GUST,T2GUST,VGUST,  &
              NT,ICL,GROUP8,VSIF,VERTV)
END IF
!**********************************************************************
! RULES USED FOR SETTING DATES AND TIMES (Revised May 2008).
! ----------------------------------------------------------
!
! REPORT TIME is taken from a 9-group (9GGgg) in section 1 of message
!  and contained in variables HOUR & MINS (HOUR='GG', MINS='gg').
!  (See WMO SYNOP regulation 12.2.8 about coding the 9-group.)
! BULLETIN TIME (day and hour) is taken from group YYGGiw in section
!  0 of message with year and month taken from the system clock (see
!  routine OBHOUR). It is held in the 5-element array IDATIM, the
!  minute (IDATIM(5)) being zero at this point.
!
! If report time is not available use the bulletin time.
!
! Otherwise use the report time but with the following modifications:
!
!   - For manual reports, if the report time is less than 10 minutes
!     after the bulletin hour, use the bulletin time with minutes=0.
!
!   - For reports other than mobile SYNOPs, if the report time is not
!     more than 10 minutes before the bulletin hour, use the bulletin
!     time with minutes=0.
!
! If using the report time changes the hour from 00 to 23, a day will
! need to be subtracted from the date.
!
! The bulletin hour is expected to be consistent with the report
! time rounded either forwards or backwards to a whole hour. If this
! is not the case, print a warning and use the bulletin hour. This
! is the best policy as this case can be generated by a missing
! indicator ('333') for section 3 of the report leading to a 9-group
! in section 3 being erroneously interpreted as 9GGgg in section 1.
!
!**********************************************************************

IFBLOCK8: &
IF (MINS /= RMISS .AND. HOUR /= RMISS) THEN
  I = NINT(HOUR)

IFBLOCK9: &
  IF (IDATIM(4) == I) THEN

! Bulletin hours and report hours agree.
! Reset time to whole hour if manual report and minutes < 10.

    IF (MINS < 10.0 .AND. .NOT.LAUTO) THEN
      IDATIM(5) = 0
    ELSE
      IDATIM(5) = INT(MINS)
    END IF

  ELSE IF (IDATIM(4) == (I+1) .OR.              &
          (IDATIM(4) == 0 .AND. I == 23)) THEN

! Bulletin hour is report hour rounded forwards to next whole hour.
! Reset time to whole hour if not mobile SYNOP and minutes > 49.

    IF (MINS >= 50.0 .AND. .NOT.MOBILE) THEN
      IDATIM(5) = 0
    ELSE
      IDATIM(4) = I
      IDATIM(5) = INT(MINS)

! Subtract a day if IDATIM(4) was changed from 0 to 23.

      IF (I == 23) THEN
        CALL DATE31 (IDATIM(3),IDATIM(2),IDATIM(1),CENDAY)
        CENDAY = CENDAY - 1
        CALL DATE13 (CENDAY,IDATIM(3),IDATIM(2),IDATIM(1))
      END IF
    END IF

  ELSE

! Ignore report time if not same hour or previous hour.

    WRITE (6,'(T4,A,A,A,A5)')                                    &
        'SYNEXP: TIMES DIFFER  ID>',IDENT(1:9),'<YYGGIW= ',YYGGIW
    WRITE (6,'(T4,2(A,I2.2,A,I2.2))')             &
        ' BULLETIN DD/HH IS ', IDATIM(3), '/', IDATIM(4),  &
        ',  REPORT HH:MM IS ', I, ':', INT(MINS)
    I = MIN0(REPLEN,102)
    WRITE (6,'(T5,2A)') 'REPORT STARTS: ', REPORT(1:I)
  END IF IFBLOCK9
END IF IFBLOCK8

!**********************************************************************
!
! END OF EXPANSION. PUT VALUES IN ARRAY FOR INTERNAL CONSISTENCY CHECKS
!
!**********************************************************************
REXP(2,1)=WMOBLK
REXP(2,2)=WMOSTA
REXP(2,3)=WMOREG
REXP(2,4)=LAT
REXP(2,5)=LON
REXP(2,6)=STNHT                     ! station height
REXP(2,7)=STNHTP                    ! pressure sensor height
IF (IX < 4) REXP(2,8)=1.            ! use IX (not WXMAN!) to
IF (IX >= 4) REXP(2,8)=0.           ! set manual/automatic flag
REXP(2,9)=IDATIM(1)                 ! YEAR
REXP(2,10)=IDATIM(2)                ! MONTH
REXP(2,11)=IDATIM(3)                ! DAY
REXP(2,12)=IDATIM(4)                ! HOUR
REXP(2,13)=IDATIM(5)                ! MINUTE
IF (IWREP) REXP(2,14)=IW
REXP(2,15)=DD
REXP(2,16)=FF
REXP(2,17)=TTT
REXP(2,18)=TDTDTD
REXP(2,19)=UUU
REXP(2,20)=VV
REXP(2,21)=VERTV
REXP(2,22)=WW
REXP(2,23)=WPER
REXP(2,24)=W1
REXP(2,25)=W2
REXP(2,26)=NT                       !  TOTAL CLOUD COVER
REXP(2,27)=TPER1
REXP(2,28)=MAXTTT
REXP(2,29)=TPER2
REXP(2,30)=MINTTT
REXP(2,31)=E
REXP(2,32)=TR
REXP(2,33)=RRR
REXP(2,34)=TR3
REXP(2,35)=RRR3
REXP(2,36)=R24
IF(VGUS == RMISS)THEN
   REXP(2,37)=T1GUST
   REXP(2,38)=T2GUST
   REXP(2,39)=VGUST
ELSE
   REXP(2,37)=T1GUS
   REXP(2,38)=T2GUS
   REXP(2,39)=VGUS
END IF
REXP(2,40)=T1MEAN
REXP(2,41)=T2MEAN
REXP(2,42)=VMEAN
REXP(2,43)=PPPP
REXP(2,44)=P0P0P0
REXP(2,45)=A3
REXP(2,46)=HHH
REXP(2,47)=SSS
REXP(2,48)=TGTG
REXP(2,49)=ATEND
REXP(2,50)=PCHANG
REXP(2,51)=PCHG24

! RADIATION ELEMENTS (IF ANY)

IFBLOCK10: &
IF(LRAD)THEN
  REXP(2,52)=IE
  REXP(2,53)=EEE
  REXP(2,54)=-24
  REXP(2,55)=SUN2
  REXP(2,56)=-1.
  REXP(2,57)=SUN1
  REXP(2,58)=RNET
  REXP(2,59)=GLSO
  REXP(2,60)=DISO
  REXP(2,61)=LW
  REXP(2,62)=SW
  REXP(2,63)=SWNET
  REXP(2,64)=SDIR
  REXP(2,65)=RNET24
  REXP(2,66)=GLSO24
  REXP(2,67)=DISO24
  REXP(2,68)=LW24
  REXP(2,69)=SW24
  REXP(2,70)=SWNET24
  REXP(2,71)=SDIR24
END IF IFBLOCK10

! SHIP ELEMENTS OR COASTAL (IF ANY)

IFBLOCK11: &
IF(LSEA)THEN           ! MARINE DATA FOUND.
  REXP(2,72)=1.        ! SEA QUALIFIER
  REXP(2,73)=SEAVIS    ! VISIBILITY TOWARDS SEA
  REXP(2,74)=SEASTE    ! STATE OF SEA
  REXP(2,75)=1.        ! (DIRECTION FROM CODE FIGURE, VERY ROUGH)
  REXP(2,76)=DS        ! DIRECTION OF MOVEMENT
  REXP(2,77)=1.        ! (SPEED FROM CODE FIGURE, VERY ROUGH)
  REXP(2,78)=VS        ! SPEED OF MOVEMENT
  REXP(2,79)=TWTYPE    ! SEA-SURFACE TEMP. MEASUREMENT METHOD
  REXP(2,80)=TWTWTW    ! SEA-SURFACE TEMPERATURE
  REXP(2,81)=TBTYPE    ! WET-BULB TEMPERATURE MEASUREMENT METHOD
  REXP(2,82)=TBTBTB    ! WET-BULB TEMPERATURE
  REXP(2,83)=WVMS      ! WAVE MEASUREMENT METHOD
  REXP(2,84)=WVPER     ! WAVE PERIOD
  REXP(2,85)=WVHT      ! WAVE HEIGHT
  REXP(2,86)=RMISS     ! (CANCEL WAVE MEASUREMENT METHOD)
  REXP(2,87)=SWELWV(1) ! SWELL WAVE DIRECTION
  REXP(2,88)=SWELWV(2) ! SWELL WAVE PERIOD
  REXP(2,89)=SWELWV(3) ! SWELL WAVE HEIGHT
  REXP(2,90)=SWELWV(4) ! SWELL WAVE DIRECTION
  REXP(2,91)=SWELWV(5) ! SWELL WAVE PERIOD
  REXP(2,92)=SWELWV(6) ! SWELL WAVE HEIGHT
  REXP(2,93)=ISICE     !
  REXP(2,94)=ESES      !
  REXP(2,95)=RS        !
  REXP(2,96)=CI        !
  REXP(2,97)=SI        !
  REXP(2,98)=BI        !
  REXP(2,99)=DI        !
  REXP(2,100)=ZI       !
END IF IFBLOCK11

! END WITH CLOUD.

REXP(2,101)=7.
REXP(2,102)=CL
REXP(2,103)=8.
REXP(2,104)=CM
REXP(2,105)=9.
REXP(2,106)=CH
REXP(2,107)=0.
REXP(2,108)=NH
REXP(2,109)=HL

! SECTION 3 CLOUD GROUPS (LAYER NUMBER, AMOUNT, TYPE, BASE)

IF(ICL > 0)THEN
  IF(ICL > 4) ICL=4
  DO I=1,ICL
   REXP(2,110+(I-1)*4)=VSIF(I)
   REXP(2,111+(I-1)*4)=GROUP8(1+(I-1)*3)
   REXP(2,112+(I-1)*4)=GROUP8(2+(I-1)*3)
   REXP(2,113+(I-1)*4)=GROUP8(I*3)
  END DO
END IF

! MOUNTAIN CLOUD GROUPS (008002, AMOUNT, TYPE, HEIGHT OF TOP, SHAPE)
! (THE SECOND GROUP MAY BE MISSING )
IF(IMT > 0)THEN
  DO I=1,2
   REXP(2,126+(I-1)*5)=RMISS    ! 008002 (CODE FIGURE NOT DECIDED)
   REXP(2,127+(I-1)*5)=MTCL(1,I)
   REXP(2,128+(I-1)*5)=MTCL(2,I)
   REXP(2,129+(I-1)*5)=MTCL(3,I)
   REXP(2,130+(I-1)*5)=MTCL(4,I)
  END DO
END IF
!*********************************************************************
!
! DO INTERNAL CONSISTENCY, LAND/SEA & SEQUENCE CHECKS
! & COUNT METEOROLOGICAL ELEMENTS
!
!*********************************************************************

! CHECK INTERNAL CONSISTENCY (CLOUD, WEATHER, VIS ETC), SETTING FLAGS
! IN REXP(1,N) - OR THE SYNTAX FLAG.

MSG=.FALSE.                   ! diagnostics not required
CALL SYNQC(REXP,WXMAN,IR,IX,ICL,WPER,MSG,SYNTAX,SHIP)

! SYNQC HAS CHECKED FOR DD=990 (VARIABLE): NOW SET THAT TO ZERO

IF (REXP(2,15) == 990.) REXP(2,15)=0.

! PRESSURE SEQUENCE CHECK: PSEQ IS SET LIKE THE Q/C FLAGS (CONSISTENT,
! SUSPECT, NO TEST DONE) - FLAG ALL ELEMENTS INVOLVED IN THE SAME WAY.
! SAME FOR SHIPS'' LAT/LONG FLAG (LAT/LONG CHECKED AGAINST MOVEMENT).
! SKIP LAND CHECK IF PRESSURE OR CHANGE MISSING.
! SKIP SHIP CHECK IF CALL SIGN IS ONLY 'SHIP'!.
! NO SEQUENCE CHECKS ARE DONE FOR MOBILE SYNOPS.

PSEQ=RMISS  ! No check yet

IFBLOCK12: &
IF (SHIP) THEN
  IF (LAT == RMISS.OR.LON == RMISS)THEN
!         DO NOT DO SEQUENCE CHECK
  ELSE IF (IDENT(1:6) /= 'SHIP  ') THEN
    CALL SHPSEQ(IDENT,IDATIM,PPPP,PCHANG,PSEQ,      &
                LAT,LON,DS,VS,LLFLAG)
    REXP(1,4)=LLFLAG            ! LAT
    REXP(1,5)=LLFLAG            ! LONG
    REXP(1,76)=LLFLAG           ! DS
    REXP(1,78)=LLFLAG           ! VS
  END IF
ELSE IF (.NOT.MOBILE .AND.                          &
         PPPP /= RMISS .AND. PCHANG /= RMISS) THEN
  CALL SYNSEQ(IDENT(1:5),IDATIM,PPPP,PCHANG,PSEQ)
END IF IFBLOCK12

REXP(1,43)=PSEQ                 ! MSL PRESSURE
REXP(1,49)=PSEQ                 ! PRESSURE TENDENCY
REXP(1,50)=PSEQ                 ! PRESSURE CHANGE

! IF SHIP IS ONLAND (ACCORDING TO OUR MAP OF ONE-DEGREE SQUARES WITH
! SEA IN) FLAG LAT & LONG.

IF (SHIP) THEN
  IF (LAT == RMISS.OR.LON == RMISS)THEN
    REXP(1,4)=1.                ! LATITUDE SUSPECT
    REXP(1,5)=1.                ! LONGITUDE SUSPECT
  ELSE IF (ONLAND(LAT,LON)) THEN
    REXP(1,4)=1.                ! LATITUDE SUSPECT
    REXP(1,5)=1.                ! LONGITUDE SUSPECT
  END IF
END IF

! COUNT ELEMENTS WHICH ARE METEOROLOGICAL, NOT MISSING & NOT FLAGGED,
! THEN ADD 3 FOR EACH CLOUD GROUP.


DO I=15,109      ! (DD-HL)
  IF (METELM(I:I) == '1' .AND. REXP(2,I) /= RMISS  &
                         .AND. REXP(1,I) /= 1) NELM=NELM+1
END DO
NELM=NELM+3*(ICL+IMT)
!*********************************************************************
!
! CONVERT 2-DIMENSIONAL ARRAY USED FOR QUALITY CONTROL INTO
! 1-DIMENSIONAL ARRAY FOR BUFR ENCODING.
!
!*********************************************************************
J=1          ! OUTPUT SUBSCRIPT
FREXP(J)=2.  !  SIGNIFICANCE OF ASSOCIATED DATA, INDICATING QC.

! ID DETAILS: CALL SIGN (SHIPS OR MOBILE SYNOPS); WMO NO. (LAND SYNOPS)

IFBLOCK13: &
IF (SHIP) THEN
  FREXP(2)=RMISS    ! BUOY ID Q/C missing
  FREXP(3)=RMISS    ! BUOY ID missing
  FREXP(4)=RMISS    ! Call sign Q/C missing
  FREXP(5)=1.       ! Call sign already in character string
  J=5
ELSE IF (MOBILE) THEN
  FREXP(2)=RMISS    ! Call sign Q/C missing
  FREXP(3)=1.       ! Call sign already in character string
  J=3
ELSE
  DO I=1,3          ! WMO block, station, region
    J=J+2
    FREXP(J-1)=REXP(1,I)   ! Flag
    FREXP(J)=REXP(2,I)     ! Element
  END DO
END IF IFBLOCK13

! SEPARATE CODE FOR LAND STATIONS & SHIPS UNTIL CLOUD DATA. LAND FIRST:

IFBLOCK14: &
IF (.NOT.SHIP) THEN
  DO I=4,51 !   MANDATORY ELEMENTS (LAT-PCHG24)
    J=J+2
    FREXP(J-1)=REXP(1,I)       ! FLAG
    FREXP(J)=REXP(2,I)         ! ELEMENT
  END DO

! OPTIONAL RADIATION SECTION - INSERT DELAYED REPLICATION COUNT

  IF(.NOT.LRAD)THEN
    J=J+1
    FREXP(J)=0.
  ELSE
    J=J+1
    FREXP(J)=1.

    DO I=52,71     ! (IE-SDIR24)
      J=J+2
      FREXP(J-1)=REXP(1,I)
      FREXP(J)=REXP(2,I)
    END DO
  END IF

! OPTIONAL COASTAL SECTION

IFBLOCK15: &
  IF(.NOT.LSEA)THEN
    J=J+1
    FREXP(J)=0.
  ELSE
    J=J+1
    FREXP(J)=1.
!                          LAND/SEA, VIS, STATE OF SEA
    DO I=72,74     ! (1.-SEASTE)
      J=J+2
      FREXP(J-1)=REXP(1,I)
      FREXP(J)=REXP(2,I)
    END DO
!                          SEA WATER TEMPERATURE
    DO I=79,80     ! (TWTYPE,TWTWTW)
      J=J+2
      FREXP(J-1)=REXP(1,I)
      FREXP(J)=REXP(2,I)
    END DO
!                          WAVES & SWELL (INCLUDING MISSING VALUE OF
!                           ELEMENT 85 TO CANCEL MEASUREMENT METHOD)
    DO I=83,92     ! (WVMS-SWELWV(6))
      J=J+2
      FREXP(J-1)=REXP(1,I)
      FREXP(J)=REXP(2,I)
    END DO
  END IF IFBLOCK15
ELSE

! SHIPS HAVE NO SNOW, STATE OF GROUND ETC, NO RADIATION SECTION,
! NO SPECIFICALLY COASTAL ELEMENTS LIKE VISIBILITY OUT TO SEA.

  DO I=4,30              ! LAT, LONG ETC  (LAT-MINTTT)
    J=J+2
    FREXP(J-1)=REXP(1,I)
    FREXP(J)=REXP(2,I)
  END DO

! SKIP STATE OF GROUND FOR SHIPS

  DO I=32,43             ! (TR-PPPP)
    J=J+2
    FREXP(J-1)=REXP(1,I)
    FREXP(J)=REXP(2,I)
  END DO

! SKIP HIGH-LEVEL PRESSURE & SNOW FOR SHIPS

  DO I=49,51             ! (ATEND-PCHG24)
    J=J+2
    FREXP(J-1)=REXP(1,I)
    FREXP(J)=REXP(2,I)
  END DO

! SKIP COASTAL ELEMENTS BUT THEN DO ALL MARINE SECTION, INCLUDING ICE.

  DO I=75,100            ! (1.-ZI)
    J=J+2
    FREXP(J-1)=REXP(1,I)
    FREXP(J)=REXP(2,I)
  END DO
END IF IFBLOCK14

! FINALLY CLOUD: MAIN CLOUD GROUP, THEN ANY 8-GROUPS & MOUNTAIN CLOUD

DO I=101,109             ! (7.-HL)
  J=J+2
  FREXP(J-1)=REXP(1,I)
  FREXP(J)=REXP(2,I)
END DO

! 8-GROUPS

J=J+1
FREXP(J)=ICL                     ! COUNT OF 8-GROUPS

DO LGROUP=1,ICL
  DO L=1,4
    J=J+2
    FREXP(J-1)=REXP(1,109+(LGROUP-1)*4+L)
    FREXP(J)=REXP(2,109+(LGROUP-1)*4+L)
  END DO
END DO

! MOUNTAIN CLOUD GROUPS (SHIPS WON''T HAVE ANY!)

J=J+1
FREXP(J)=IMT                     ! COUNT OF MOUNTAIN GROUPS

DO LGROUP=1,IMT
  DO L=1,5
    J=J+2
    FREXP(J-1)=REXP(1,125+(LGROUP-1)*5+L)
    FREXP(J)=REXP(2,125+(LGROUP-1)*5+L)
  END DO
END DO


RETURN
END SUBROUTINE SYNEXP
