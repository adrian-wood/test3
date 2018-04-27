SUBROUTINE SYNXP3(REPORT,POINT,NGRPS,SYNTAX, &
   IWMOB,IWMOR,WNDKTS,WPER,IR,IDATIM,TPER1,TPER2,TGTG,VERTV, &
   A3,HHH,MAXTTT,MINTTT,E,SSS, &
   T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS, &
   T1MEAN,T2MEAN,VMEAN,ICL,GROUP8,VSIF,RRR3,TR3,R24,IE,EEE, &
   SUN2,SUN1,RNET,GLSO,DISO,LW,SW,RNET24,GLSO24, &
   DISO24,LW24,SW24,PCHG24,SEASTE,SEAVIS,VV, &
   LRAD,LSEA)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNXP3
!
! PURPOSE       : EXPANDS GROUPS FROM SECTION 3 OF A SYNOP REPORT.
!
! DESCRIPTION   : PROCESSES ALL GROUPS AFTER THE SECTION IDENTIFIER,
!                 CALLS SUBROUTINES FOR 5-GROUPS, 8-GROUPS & 9-GROUPS.
!
! CALLED BY     : SYNEXP
!
! CALLS         : SYNTPR, SYNFIV, SYNCLD, SYNNIN
!
! ARGUMENTS     : (1)REPORT  CHAR    COMPLETE REPORT               (I)
!                 (2)POINT   INTEGER START OF GROUP AFTER 333      (I/O)
!                 (3)NGRPS   INTEGER TOTAL NUMBER OF GRPS IN S3    (I)
!                 (4)SYNTAX  LOGICAL TRUE IF SYNTAX ERROR FOUND    (I/O)
!                 (5)IWMOB   INTEGER WMO BLOCK                     (I)
!                 (6)IWMOR   INTEGER WMO REGION                    (I)
!                 (7)WNDKTS  LOGICAL TRUE IF WND REPORTED IN KTS   (I)
!                 (8)WPER    REAL    PAST WEATHER PERIOD (HOURS)   (I/O)
!                 (9)IR      REAL    RAINFALL REPORTING FLAG       (I)
!                (10)IDATIM  INTEGER ARRAY OF YMDHM                (I)
!                (11)-(50)   REAL    ELEMENTS. SEE SYNINI          (I/O)
!                (51)LRAD    LOGICAL TRUE IF RADIATION GRPS FOUND  (I/O)
!                (52)LSEA    LOGICAL TRUE IF COASTAL GRPS FOUND    (I/O)
!
! REVISION INFO :
!
! $Workfile: synxp3.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 06/07/2011 09:05:00$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         06/07/2011 09:05:00    John Norton
!       Updated argument intents for LSEA and LRAD and removed the setting of
!       those arguments.
!  4    MetDB_Refresh 1.3         22/03/2011 14:48:49    Brian Barwell   Change
!        REPORT to LEN=*.
!  3    MetDB_Refresh 1.2         23/12/2010 14:11:12    John Norton
!       Initialised LSEA and LRAD.
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

USE ivalue_mod   !function
USE syncld_mod
USE synfiv_mod
USE synnin_mod
USE syntpr_mod

! <Data Modules>

USE metdb_com_mod, only : MISSIN, RMISS, TCONV

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
INTEGER,          INTENT(INOUT) :: POINT      !a02
INTEGER,          INTENT(IN)    :: NGRPS      !a03
LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
INTEGER,          INTENT(IN)    :: IWMOB      !a05
INTEGER,          INTENT(IN)    :: IWMOR      !a06
LOGICAL,          INTENT(IN)    :: WNDKTS     !a07
REAL,             INTENT(INOUT) :: WPER       !a08 Past weather period
INTEGER,          INTENT(IN)    :: IR         !a09
INTEGER,          INTENT(IN)    :: IDATIM(*)  !a10
REAL,             INTENT(INOUT) :: TPER1      !a11 Max temp time period
REAL,             INTENT(INOUT) :: TPER2      !a12 Min temp time period
REAL,             INTENT(INOUT) :: TGTG       !a13 Grass minimum temperature (TgTg)
REAL,             INTENT(INOUT) :: VERTV      !a14 Vertical visibilty
REAL,             INTENT(INOUT) :: A3         !a15 Standard isobaric surface (a3)
REAL,             INTENT(INOUT) :: HHH        !a16 Geopotential height (hhh)
REAL,             INTENT(INOUT) :: MAXTTT     !a17 Max Air Temperature (TxTxTx)
REAL,             INTENT(INOUT) :: MINTTT     !a18 Min Air Temperature (TnTnTn)
REAL,             INTENT(INOUT) :: E          !a19 State of Ground (E/E') '
REAL,             INTENT(INOUT) :: SSS        !a20 Snow depth (sss)
REAL,             INTENT(INOUT) :: T1GUST     !a21 Start gust period of gust 911 group
REAL,             INTENT(INOUT) :: T2GUST     !a22 End gust period of gust 911 group
REAL,             INTENT(INOUT) :: VGUST      !a23 911 group gust value (ff/fff)
REAL,             INTENT(INOUT) :: T1GUS      !a24 Start gust period of gust 910 group
REAL,             INTENT(INOUT) :: T2GUS      !a25 End gust period of gust 910 group
REAL,             INTENT(INOUT) :: VGUS       !a26 910 group gust value (ff/fff)
REAL,             INTENT(INOUT) :: T1MEAN     !a27 Start mean period of 912 group
REAL,             INTENT(INOUT) :: T2MEAN     !a28 End mean period of 912 group
REAL,             INTENT(INOUT) :: VMEAN      !a29 912 group mean value (ff/fff)
INTEGER,          INTENT(INOUT) :: ICL        !a30
REAL,             INTENT(INOUT) :: GROUP8(12) !a31 "8" Cloud group elements(4*NsChshs)
REAL,             INTENT(INOUT) :: VSIF(4)    !a32 Significance of cloud layers
REAL,             INTENT(INOUT) :: RRR3       !a33 Section 3 Precipitation amount (RRR)
REAL,             INTENT(INOUT) :: TR3        !a34 Section 3 type of precipitation (tR)
REAL,             INTENT(INOUT) :: R24        !a35 24 hour Precipitation amount (R24R24R24R24)
REAL,             INTENT(INOUT) :: IE         !a36 Type of instrumentation for evaporation (iE)
REAL,             INTENT(INOUT) :: EEE        !a37 Evaporation or evapotranspiration (EEE)
REAL,             INTENT(INOUT) :: SUN2       !a38 24 hour Sunshine amount (SSS)
REAL,             INTENT(INOUT) :: SUN1       !a39 1 hour Sunshine amount (SS)
REAL,             INTENT(INOUT) :: RNET       !a40 1 hour Net radiation
REAL,             INTENT(INOUT) :: GLSO       !a41 1 hour global radiation
REAL,             INTENT(INOUT) :: DISO       !a42 1 hour diffuse radiation
REAL,             INTENT(INOUT) :: LW         !a43 1 hour Longwave radiation
REAL,             INTENT(INOUT) :: SW         !a44 1 hour shortwave radiation
REAL,             INTENT(INOUT) :: RNET24     !a45 24 hour Net radiation
REAL,             INTENT(INOUT) :: GLSO24     !a46 24 hour global radiation
REAL,             INTENT(INOUT) :: DISO24     !a47 24 hour diffuse radiation
REAL,             INTENT(INOUT) :: LW24       !a48 24 hour Longwave radiation
REAL,             INTENT(INOUT) :: SW24       !a49 24 hour shortwave radiation
REAL,             INTENT(INOUT) :: PCHG24     !a50 24 hour Pressure change amount (P24P24P24)
REAL,             INTENT(INOUT) :: SEASTE     !a51 State of sea 924 group (S)
REAL,             INTENT(INOUT) :: SEAVIS     !a52 Horiz vis towards sea 924 group (Vs)
REAL,             INTENT(INOUT) :: VV         !a53 Visibility (VV)
LOGICAL,          INTENT(INOUT) :: LRAD       !a54
LOGICAL,          INTENT(INOUT) :: LSEA       !a55

! Local declarations:

REAL :: SDIR       ! 1 hour direct solar radiation
REAL :: SWNET      ! 1 hour net shortwave radiation
REAL :: SDIR24     ! 24 hour direct solar radiation
REAL :: SWNET24    ! 24 hour net shortwave radiation

INTEGER          ::  HR
INTEGER          ::  I
INTEGER          ::  IG
INTEGER          ::  IH
INTEGER          ::  ISIG
INTEGER          ::  LASTID

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

ISIG=0 !  COUNT OF SIGNIFICANT LAYERS IN 8 GROUPS
ICL=0  !  CLOUD 8 GROUP COUNTER
HR=IDATIM(4)
!**********************************************************************
!
! CHECK SEQUENCE OF GROUPS. SET SYNTAX FLAG & STOP IF DESCENDING
!
!**********************************************************************
IG=1
LASTID=0
 10    CONTINUE
I=IVALUE(REPORT(POINT:POINT))
IF(I < LASTID)THEN
  SYNTAX=.TRUE.
  RETURN
END IF
LASTID=I
!**********************************************************************
!
! MAX DRY BULB TEMPERATURE (DEGREES AND TENTHS TO K)
!
!**********************************************************************
IFLABEL1: &
IF (REPORT(POINT:POINT) == '1') THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
  IF (I /= MISSIN) THEN
    IF (REPORT(POINT+1:POINT+1) == '1') THEN
      MAXTTT=-I*0.1 + TCONV
    ELSE IF (REPORT(POINT+1:POINT+1) == '0') THEN
      MAXTTT=I*0.1 + TCONV
    END IF
    IF (MAXTTT > RMISS) CALL SYNTPR(IWMOR,HR,1,TPER1)
  END IF
!**********************************************************************
!
! MIN DRY BULB TEMPERATURE (DEGREES AND TENTHS TO K)
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '2') THEN
  I=IVALUE(REPORT(POINT+2:POINT+4))
  IF (I /= MISSIN) THEN
    IF (REPORT(POINT+1:POINT+1) == '1') THEN
      MINTTT=-I*0.1+TCONV
    ELSE IF (REPORT(POINT+1:POINT+1) == '0') THEN
      MINTTT=I*0.1+TCONV
    END IF
    IF (MINTTT > RMISS) CALL SYNTPR(IWMOR,HR,2,TPER2)
  END IF
!**********************************************************************
!
! STATE OF GROUND WITHOUT SNOW AND GRASS MINIMUM
! E SYNOP CODE 0901 = BUFR CODE 020062
! GRASS MINIMUM (WHOLE DEGREES TO K)
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '3') THEN
  E=IVALUE(REPORT(POINT+1:POINT+1))
  I=IVALUE(REPORT(POINT+3:POINT+4))
  IF (I /= MISSIN) THEN
    IF (REPORT(POINT+2:POINT+2) == '1') THEN
      TGTG=-I+TCONV
    ELSE IF (REPORT(POINT+2:POINT+2) == '0') THEN
      TGTG= I+TCONV
    END IF
  END IF
!**********************************************************************
!
! STATE OF GROUND WITH SNOW AND SNOW DEPTH.
! E' SYNOP CODE 0975 = BUFR CODE 020062 +10
! SSS SYNOP CODE 3889 = BUFR CODE 013013 (M AND -1,-2)
!
! #### NOTE THAT SOME IRANIAN STATIONS (BLOCK 40) REPORT THE
! #### 850MB LEVEL HEIGHT IN THIS GROUP, INSTEAD OF SNOW DATA.
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '4') THEN
  I=IVALUE(REPORT(POINT+1:POINT+1))
  IH=IVALUE(REPORT(POINT+2:POINT+4))
IFLABEL2: &
  IF (I == 8 .AND. IWMOB == 40 .AND. IH >= 300) THEN
    A3=85000.     ! ASSUME 850MB HEIGHT
    HHH=IH+1000.
  ELSE            ! IT MUST BE STATE OF GROUND AND SNOW DEPTH
    IF (I /= MISSIN) E=I+10.
    IF (IH > 0 .AND. IH <= 996) THEN
      SSS=IH*0.01
    ELSE IF (IH == 997) THEN
      SSS=-1.
    ELSE IF (IH == 998) THEN
      SSS=-2.
    END IF
  END IF IFLABEL2
!**********************************************************************
!
! HANDLE 5-GROUPS IN A SEPARATE ROUTINE (BECAUSE THE RADIATION GROUPS
! START WITH 0-6).  WHEN SYNFIV FINDS A GROUP UNCONNECTED WITH 5-GROUPS
! THE MAIN LOOP GOES ON WITH THAT GROUP RATHER THAN MOVING TO THE NEXT.
!
!**********************************************************************
ELSE IF(REPORT(POINT:POINT) == '5')THEN
  LRAD=.TRUE.
  CALL SYNFIV(REPORT,POINT,NGRPS,IG, &
    IR,IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR, &
    RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,PCHG24)
  IF (IG <= NGRPS) GO TO 10
!**********************************************************************
!
! RAINFALL AMOUNT AND PERIOD.
! CONVERT AMOUNT FROM SYNOP CODE TABLE 3590 TO KG/M**2 (NUMERICALLY
! SAME AS MM) & PERIOD FROM CODE TABLE 4019 TO HOURS
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '6') THEN
  I=IVALUE(REPORT(POINT+1:POINT+3))
  IF (I > 0 .AND. I <= 989) THEN
    RRR3=I
  ELSE IF (I > 990 .AND. I <= 999) THEN
    RRR3=(I-990.)*0.1
  ELSE IF (I == 990.) THEN
    RRR3=-1.
  END IF

!1.7  For India and Sri-Lanka a tR value of / means that the rain
!1.6  reported is that since 0300Z.

IFLABEL3: &
  IF((REPORT(POINT+4:POINT+4) == '/').AND.(IWMOB >= 42) &
    .AND.(IWMOB <= 43)) THEN
    TR3=-(MOD(IDATIM(4)+20,24)+1)
  ELSE
    I = IVALUE(REPORT(POINT+4:POINT+4))
    IF (I >= 1 .AND. I <= 4) THEN
      TR3=-I*6
    ELSE IF (I >= 5 .AND. I <= 7) THEN
      TR3=-(I-4.)
    ELSE IF (I == 8) THEN
      TR3=-9.
    ELSE IF (I == 9) THEN
      TR3=-15.
    ELSE
      TR3=I
    END IF
  END IF IFLABEL3

! SOME BLOCKS IN THE TWENTIES & THIRTIES REPORT 12-HOUR RAINFALL WITH
! TR=0 BECAUSE THE 12-HOUR PERIOD DEPENDS ON LOCAL TIME RATHER THAN
! ENDING AT THE REPORT TIME.

  IF (I == 0 .AND.IWMOB >= 20.AND.IWMOB <= 39) TR3=-12.
!**********************************************************************
!
! 24 HOUR RAINFALL AMOUNT (TENTHS OF MM TO KG/M**2, I.E. MM)
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '7') THEN
  I=IVALUE(REPORT(POINT+1:POINT+4))
  IF (IWMOB == 89) THEN             !1.8 Antartic 7 groups hold
    R24=-9999999.0                  !1.8 prevailing wind not rain
                                    !1.8 so set R24 to missing
  ELSE                              !1.8 It is 24 hour rainfall
    IF (I >= 0 .AND. I <= 9998) THEN
      R24=I*0.1
    ELSE IF (I == 9999) THEN
      R24=-1.
    END IF
  END IF                            !1.8
!**********************************************************************
!
! HANDLE CLOUD 8-GROUPS IN SUBROUTINE
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '8') THEN    !  CLOUD GROUPS
  IF (REPORT(POINT:POINT+4) /= '80000') THEN
    CALL SYNCLD(POINT,REPORT,IG,NGRPS,ICL,GROUP8,VSIF,VERTV)
  END IF
!**********************************************************************
!
! HANDLE 9-GROUPS IN SUBROUTINE, RETURNING AT END OF 9-GROUPS
!
!**********************************************************************
ELSE IF (REPORT(POINT:POINT) == '9') THEN
  CALL SYNNIN(REPORT(POINT:),NGRPS-IG+1,WNDKTS,WPER, &
              T1GUST,T2GUST,VGUST, T1GUS,T2GUS,VGUS, &
              T1MEAN,T2MEAN,VMEAN, &
              LSEA,SEASTE,SEAVIS,VV)
  RETURN
END IF IFLABEL1
!**********************************************************************
!
! MOVE ON TO NEXT GROUP
!
!**********************************************************************
POINT=POINT+6   !  NEXT GROUP
IG=IG+1
IF (IG <= NGRPS) GOTO 10
 999   CONTINUE
RETURN
END SUBROUTINE SYNXP3
