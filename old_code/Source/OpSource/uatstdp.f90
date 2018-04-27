SUBROUTINE UATSTDP(STRING,PTR,ARRAY,NUM_LEV,PART,ID,KNOTS, &
                   BASE,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UATSTDP
!
! PURPOSE       : EXPAND STANDARD LEVEL DATA IN TEMP PART A OR C
!
! NOTE          : This routine must be the first routine after
!                 UAHEAD when expanding Temp, Pilot or Dropsonde
!                 Parts A & C
!
! DATA TYPE(S)  : UPPER AIR TEMP PART A/C (AND B)
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION), UATSTX
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) input pointer to '99' at start of surface data
!                     if part A or '70' if part C...
!                      (points past end of section on return)
!                 (3) output array, with header data already in it
!                 (4) number of levels already in output array
!                     (only >0 for part B)
!                 (5) part (A or C - or B, for 51515 section)
!                 (6) hundreds figure of highest level with wind (Id)
!                 (7) flag set on if winds in knots
!                 (8)
!                 (9) QC flags output (all 0 except where dd was bad
!                     (& is now missing)
!
! REVISION INFO :
!
!
! $Workfile: uatstdp.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 19/04/2011 16:00:46$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         19/04/2011 16:00:46    Sheila Needham
!       Initialise variables and test array subscripts before using
!       out-of-range values
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE IVALUE_mod
USE METDB_COM_mod, ONLY: MISSIN, RMISS, TCONV, KTS2MPS
USE UATSTX_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT)  :: STRING   ! (a1) part of report being expanded
INTEGER, INTENT(INOUT)            :: PTR      ! (a2) pointer within report
REAL, INTENT(INOUT)               :: ARRAY(:) ! (a3) array of decoded values
INTEGER, INTENT(INOUT)            :: NUM_LEV  ! (a4) number of standard levels
CHARACTER (LEN=1), INTENT(IN)     :: PART     ! (a5) message part (A,C or B)
INTEGER, INTENT(INOUT)            :: ID       ! (a6) wind top level indicator
LOGICAL, INTENT(IN)               :: KNOTS    ! (a7) indicates speed in knots
INTEGER, INTENT(INOUT)            :: BASE     ! (a8) base displacement in array
REAL, INTENT(OUT)                 :: QCBIT_ARRAY(999)  ! (a9) array of 1bit QC flags


! Local Parameters

INTEGER, PARAMETER  :: LEV_TYPE = 32    ! standard levels

! Local Variables

INTEGER  :: P                 !pressure
INTEGER  :: HT                !height
INTEGER  :: T                 !temperature
INTEGER  :: TD                !dew point depression
INTEGER  :: DD                !wind direction
INTEGER  :: FF                !wind speed
INTEGER  :: PHT
INTEGER  :: L
INTEGER  :: MAXL
INTEGER  :: IND92             !start of 925mb data
INTEGER  :: IND85             !start of 850mb data
INTEGER  :: IND77             !start of 775mb data
INTEGER  :: IND60             !start of 600mb data
INTEGER  :: NG9900            !groups for surface & 1000mb
INTEGER  :: STNHT
INTEGER  :: IND1
INTEGER  :: IRC               ! return code from UATSTX
INTEGER  :: PTR99             !pointer to 99... for print
INTEGER  :: PSTD(20)
INTEGER  :: H850 = 0             ! subscript of 850mb height in ARRAY
INTEGER  :: H700 = 0             ! subscript of 700mb height in ARRAY
INTEGER  :: H500 = 0             ! subscript of 500mb height in ARRAY
LOGICAL  :: WINDGP            ! set if wind group expected
CHARACTER (LEN=2)  :: IND
CHARACTER (LEN=2)  :: LEVEL(20)

SAVE

! --------------------------------------------------------------------
! the data consists of heights, temperatures and perhaps winds for
! standard levels (3 groups per level if wind, 2 if not), the level
! being indicated by 2 figures of the pressure, the presence of a
! wind by an indicator in the date/time group.  the height has to be
! reconstructed from the 3 figures given.
! indicators for standard levels & corresponding pressures (pascals)
! - 12 for part a, 5 for part c, 3 for 51515 section in part b.
! --------------------------------------------------------------------

DATA PSTD/-9999999, 100000,92500,85000,70000,50000, &
             40000,30000,25000,20000,15000,10000,   &
              7000, 5000, 3000, 2000, 1000,         &
             92500,77500,60000/

DATA LEVEL/'99', '00', '92', '85', '70', '50',      &
           '40', '30', '25', '20', '15', '10',      &
           '70', '50', '30', '20', '10',            &
           '92', '77', '60'/

! --------------------------------------------------------------------

IF (PART == 'A' .OR. PART == 'C') THEN
  NUM_LEV=0                          !first level to start with
  BASE=8                             !base displacement
END IF

PTR99=PTR   ! keep start to print message if in error

! --------------------------------------------------------------------
! decide (from part, a or c) starting point in above arrays
! --------------------------------------------------------------------

IF (PART == 'A') THEN
  L=1
  MAXL=12
ELSE IF (PART == 'C') THEN
  L=13
  MAXL=17
ELSE IF (PART == 'B') THEN
  L=18
  MAXL=20
  IF (STRING(PTR-6:PTR-2) == '52525') THEN
    L=3
    MAXL=3
  END IF
END IF
! --------------------------------------------------------------------
! Find starts of levels below 850mb if it's a Part A.
! The 850mb and 925mb identifiers can be recognised unambiguously;
! the 1000mb indicator could occur in a T or wind group, so deduce
! its position from the number of groups for the lowest two levels,
! assuming that the surface wind is only missing if there are no
! winds at all, i.e. only if only 4 groups for the 2 levels.
!
! This approach was adopted (Dec 97) because too much data was lost
! by expecting TTAAs to adhere strictly to the Manual on codes.
! It was found that
! - not all obs have a surface wind, although it's now mandatory
! - not all obs have 925mb data, although it's now standard
! - some obs omit a ///// wind group when 1000mb is below the
!    surface even if Id says all levels have winds
! - some obs set Id to the top level with a wind rather than with
!    a wind group, i.e. ///// is coded where not expected
! - 06476 omits 1000mb because its surface is nearer 925mb
! These errors can be treated as differences of interpretation,
! easier to handle here than to get changed at source.
!
! --------------------------------------------------------------------

IF (PART == 'A') THEN
  IND85=INDEX(STRING(PTR:),' 85')
  IND92=INDEX(STRING(PTR:),' 92')

  IF (IND92 > 0 .AND. IND92 < IND85) THEN
    NG9900=IND92/6
  ELSE IF (IND92 == 0 .AND. IND85 > 0) THEN
    NG9900=IND85/6
  ELSE
    GO TO 100             ! if no 92 or 85 give up!
  END IF

! Give up if no 00 in right place.
! Carry on if only 3 groups for bottom 2 levels: may be no 1000mb

  IF (NG9900 < 3 .OR. NG9900 > 6) GO TO 100
  IF (NG9900 == 4 .AND. STRING(PTR+12:PTR+13) /= '00') GO TO 100
  IF (NG9900 > 4 .AND. STRING(PTR+18:PTR+19) /= '00') GO TO 100
END IF

! --------------------------------------------------------------------
! If it's a part B, only look for 775 & 600mb data, assuming both
! levels have winds if they start 3 groups apart, no wind if 2...
! --------------------------------------------------------------------

IF (PART == 'B') THEN
  IND92=INDEX(STRING(PTR-1:),' 92')
  IND77=INDEX(STRING(PTR-1:),' 77')
  IND60=INDEX(STRING(PTR-1:),' 60')
  IF (IND60-IND77 == 12 .OR. IND60-IND77 == 18) THEN
    IF (IND92 == 0) THEN ! if no 925mb data,
      L=19               ! point to '77' as first indicator
      PTR=PTR-1+IND77    ! move pointer to start of 775mb data
      IF (PTR >= LEN(STRING)) GO TO 100
    END IF
  ELSE
    GO TO 100            ! give up if 51515 section not right
  END IF
END IF

! --------------------------------------------------------------------
! Check indicator & get height (pressure if surface data)
! --------------------------------------------------------------------

 1 CONTINUE

P=MISSIN
HT=MISSIN
T=MISSIN
TD=MISSIN
DD=MISSIN
FF=MISSIN

IND=STRING(PTR:PTR+1)

IFBlock1: &
IF (IND == LEVEL(L)) THEN
  NUM_LEV=NUM_LEV+1
  P=PSTD(L)
  PHT=IVALUE(STRING(PTR+2:PTR+4))               ! PRESS? HEIGHT?

IFBlock2: &
  IF (PHT /= MISSIN) THEN

IFBlock3: &
    IF (PART == 'A') THEN
      IF (IND == '99') THEN                     ! SURFACE PRESSURE
        P=PHT*100                               ! MB TO PASCALS
        IF (P < 10000) THEN                     ! MAY BE >1000MB
          P=P+100000
        END IF

        IF (ARRAY(7) > RMISS) THEN
          HT=ARRAY(7)                           ! STATION HEIGHT
        ELSE
          HT=ARRAY(8)
        END IF
        STNHT=HT
      END IF

! --------------------------------------------------------------------
! Put height in usable form, adding thousands figure etc.
! The thousands figures of the 850mb & 700mb heights are decided
! using an approach described by K H Hinkelmann, WMO Regional
! Training Seminar on Numerical Weather Prediction, Moscow, 1965:
! ("<600 reported" etc refer to the 3 figures hhh in 85hhh/70hhh)
!  H850>1000m if <600 reported,
!  H850<1000m if >800 reported,
!   otherwise H850>1000m if H700>2700m.
!  H700>3000m if <200 reported,
!  H850<3000m if >400 reported,
!   otherwise H700>3000m if H500>5100m.
! Set H850>1000m if <800 reported & H700>3000m if <400 reported,
! then deal with the 600-800 & 200-400 ranges at the end when
! heights can be cross-checked.
! --------------------------------------------------------------------

      IF (IND == '00') THEN                     ! 1000MB HEIGHT
        IF (PHT >= 500) THEN                    ! MAY BE NEGATIVE
          PHT=500-PHT
        END IF
      END IF
      IF (IND == '00' .OR. IND == '92') THEN    ! BOTH <1000M
        HT=PHT
      END IF
      IF (IND == '85') THEN                     ! 850MB (C.1500M)
        IF (PHT < 800) THEN
          HT=PHT+1000
        ELSE
          HT=PHT
        END IF
      END IF
      IF (IND == '70') THEN                     ! 700MB (C.3000M)
        HT=PHT+2000
        IF (HT <= 2400) THEN                    !   >3000M?
          HT=HT+1000
        END IF
      END IF
      IF (IND <= '50' .AND. IND >= '10') THEN   ! 500MB & ABOVE
        HT=PHT*10                               !   DEKAMETRES
        IF (IND == '30' .OR. IND == '25') THEN  ! 300MB & 250MB
          IF (HT < 5000) THEN                   !   >10000M?
            HT=HT+10000
          END IF
        END IF
        IF (IND <= '20') THEN                   ! 200MB & ABOVE
          HT=HT+10000
        END IF
      END IF                                    !   >10000M
    ELSE IF (PART == 'C') THEN
      HT=PHT*10
      IF (IND == '70' .OR. IND == '50') THEN    ! 70MB & 50MB
        HT=HT+10000
        IF (HT < 15000) THEN                    !   >20000M?
          HT=HT+10000
        END IF
      END IF
      IF (IND == '30' .OR. IND == '20') THEN
        HT=HT+20000
      END IF
      IF (IND == '10') THEN                     ! 10MB
        HT=HT+20000
        IF (HT < 25000) THEN                    !   >30000M?
          HT=HT+10000
        END IF
      END IF
    ELSE IF (PART == 'B') THEN
      HT=PHT
      IF (IND == '77') THEN
        HT=HT+2000
      END IF
      IF (IND == '60') THEN
        HT=HT+4000
      END IF
    END IF IFBlock3
  END IF IFBlock2

! --------------------------------------------------------------------
! Set pressure & height in output array, then advance pointer
! --------------------------------------------------------------------

  IF (IND == '99') THEN
    ARRAY(BASE+(7*NUM_LEV))=64
  ELSE
    ARRAY(BASE+(7*NUM_LEV))=LEV_TYPE
  END IF

  ARRAY(BASE+(7*NUM_LEV)+1)=P            !pressure
  ARRAY(BASE+(7*NUM_LEV)+2)=HT           !height

! Keep subscripts of 850, 700 & 500mb heights for final check on
! 850 & 700mb thousands figures.

  IF (PART == 'A') THEN
    IF (IND == '85') H850=BASE+7*NUM_LEV+2
    IF (IND == '70') H700=BASE+7*NUM_LEV+2
    IF (IND == '50') H500=BASE+7*NUM_LEV+2
  END IF

  QCBIT_ARRAY(BASE+(7*NUM_LEV))=0        !level type okay
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0      !pressure okay
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0      !height okay

  PTR=PTR+6
  IF (PTR >= LEN(STRING)) GO TO 100

! --------------------------------------------------------------------
! Temperature & dew point
! --------------------------------------------------------------------

  T=IVALUE(STRING(PTR:PTR+2))         ! TEMPERATURE IN TENTHS

  IF (T > -9999) THEN
    IF (MOD(T,2) == 1) THEN           ! <0 IF TENTHS FIGURE ODD
      T=-T
    END IF
    TD=IVALUE(STRING(PTR+3:PTR+4))    ! DEW POINT DEPRESSION

    IF (TD /= MISSIN) THEN
      IF (TD > 50) THEN               ! IN WHOLE DEGREES IF >50
        TD=(TD-50)*10
      END IF
      TD=T-TD
    ELSE
      TD=MISSIN
    END IF
  ELSE
    T=MISSIN
    TD=MISSIN
  END IF

! --------------------------------------------------------------------
! Put temperature and dew point in output array, then advance pointer
! --------------------------------------------------------------------

  IF (T /= MISSIN) THEN
    ARRAY(BASE+(7*NUM_LEV)+3)=T*0.1 + TCONV         !TEMPERATURE
    IF (TD /= MISSIN) THEN
      ARRAY(BASE+(7*NUM_LEV)+4)=TD*0.1 + TCONV      !DEW POINT
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+4)=RMISS               !NO DEWPOINT
    END IF
  ELSE
    ARRAY(BASE+(7*NUM_LEV)+3)=RMISS                 !NO TEMP
    ARRAY(BASE+(7*NUM_LEV)+4)=RMISS
  END IF

  QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0                 !TEMP OKAY
  QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0                 !DEWPOINT OK

  PTR=PTR+6
  IF (PTR >= LEN(STRING)) GO TO 100

! --------------------------------------------------------------------
! Wind: the levels below 850mb have already been delimited; at 850mb
! and above expect a wind group if Id indicates one (but allow below
! for a ///// group where Id says there is no wind!)
! N.B. Id=/ and Id=0 both mean no winds at 850mb and above.
! --------------------------------------------------------------------

  IND1=IVALUE(IND(1:1))               ! first figure of ident
  WINDGP=.TRUE.
  IF (PART == 'A') THEN
    IF (L == 1 .AND. NG9900 == 4) WINDGP=.FALSE.
    IF (L == 2 .AND. NG9900 < 6) WINDGP=.FALSE.
    IF (L == 3 .AND. IND85-IND92 < 18) WINDGP=.FALSE.
    IF (L >= 4 .AND. (ID <= 0 .OR.IND1 < ID)) WINDGP=.FALSE.
  ELSE IF (PART == 'B') THEN
    IF (IND60-IND77 == 12) WINDGP=.FALSE.
  ELSE IF (PART == 'C') THEN
    IF (ID < 0 .OR. IND1 < ID) WINDGP=.FALSE.
  END IF

  IF (WINDGP) THEN
    DD=IVALUE(STRING(PTR:PTR+1))
    FF=IVALUE(STRING(PTR+2:PTR+4))
    IF (DD /= MISSIN) THEN
      DD=DD*10
      IF (FF >= 500) THEN             ! check wind direction
        FF=FF-500
        DD=DD+5
      END IF
!           IF (DD > 360) DD=MISSIN         ! do this below!
    END IF

! --------------------------------------------------------------------
! Put wind in output array, then advance pointer
! --------------------------------------------------------------------

    IF (DD > 360) THEN
      DD=MISSIN                              !invalid direction
      ARRAY(BASE+(7*NUM_LEV)+5)=DD           !set missing
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=1      !direction suspect
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+5)=DD           !direction okay
      QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
    END IF

    IF (FF /= MISSIN) THEN
      IF (KNOTS) THEN
        ARRAY(BASE+(7*NUM_LEV)+6)=FF*KTS2MPS
      ELSE
        ARRAY(BASE+(7*NUM_LEV)+6)=FF
      END IF
    ELSE
      ARRAY(BASE+(7*NUM_LEV)+6)=RMISS
    END IF

    QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0        !speed okay

    PTR=PTR+6
    IF (PTR >= LEN(STRING)) GO TO 100
  END IF

! --------------------------------------------------------------------
! If the indicator doesn't match, return if it's a new section.
! If there's no 925mb indicator, look for 850mb in the same group.
! If there's a slashed (wind) group look again in the next group.
! If there's no 1000mb data (NG9900=3), look on for 925 or 850mb.
! Otherwise accept that we're out of step and give up!
! --------------------------------------------------------------------

ELSE IF (IND == '88' .OR. IND == '77' .OR. IND == '66') THEN
  GOTO 100       ! finished!
ELSE IF ((L <= 3 .AND. NG9900 == 3) .OR.  &
               (L == 3 .AND. IND92 == 0)) THEN
  L=L+1          ! look for next ident in same group
  IF (L <= MAXL) GOTO 1
ELSE IF (STRING(PTR:PTR+4) == '/////') THEN
  PTR=PTR+6      ! look for same ident in next group
  IF (PTR >= LEN(STRING)) GO TO 100
  GO TO 1
ELSE
  print *,'UATSTDP: ',LEVEL(L),' not found in ',  &
                STRING(PTR-18:PTR+4),'     Id=',ID

! try to recover: if so, try again with either both L and pointer
! changed or Id or level indicator corrected

  IRC=0
  CALL UATSTX(STRING,PTR,LEVEL,L,MAXL,ID,IRC)
  IF (IRC > 1) GO TO 1
  GO TO 100      ! give up
END IF IFBlock1

L=L+1
IF (L <= MAXL) GO TO 1

  100 CONTINUE
! --------------------------------------------------------------------
! Before returning do some cross-checks which may reset the
! thousands figures of the 850 & 700mb heights:
!  if 3200<H700<3400m & H500<5100m, set H700 thousands figure to 2
!  if 1600<H850<1800m & H700<2700m, set H850 thousands figure to 0
! --------------------------------------------------------------------
IF (PART == 'A') THEN
  IF (H500 > 0 .AND. H700 > 0) THEN
    IF (ARRAY(H500) > RMISS.AND.ARRAY(H500) < 5100.) THEN
      IF (ARRAY(H700) > 3200..AND. ARRAY(H700) < 3400.) THEN
        ARRAY(H700)=ARRAY(H700)-1000.
      END IF
    END IF
  END IF
  IF (H700 > 0 .AND. H850 > 0) THEN
    IF (ARRAY(H700) > RMISS.AND.ARRAY(H700) < 2700.) THEN
      IF (ARRAY(H850) > 1600..AND. ARRAY(H850) < 1800.) THEN
        ARRAY(H850)=ARRAY(H850)-1000.
      END IF
   END IF
  END IF
END IF

RETURN
END SUBROUTINE UATSTDP
