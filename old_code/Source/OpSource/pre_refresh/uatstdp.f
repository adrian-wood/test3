      SUBROUTINE UATSTDP(STRING,PTR,ARRAY,NUM_LEV,PART,ID,KNOTS,
     &                   BASE,QCBIT_ARRAY)

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
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
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
! $Revision: 1$
! $Date: 30/01/2006 20:25:44$
! $Source: /data/us0400/mdb/op/lib/source/RCS/uatstdp.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:44    Sheila Needham  
! $
! Revision 2.1  2001/11/06  10:08:59  10:08:59  usmdb (MetDB account c/o usjh)
! Added check that L doesn't exceed MAXL. S.Cox
! 
! Revision 2.0  2001/07/03  10:44:37  10:44:37  usmdb (Generic MetDB account)
! Moved DATA statements before executable section. Removed unused
! variables. Removed unwanted print statement. Added copyright
! and modified header - S.Cox
!
! Revision 1.4  2000/11/07  12:02:34  12:02:34  usmdb (Generic MetDB account)
! 20 Nov 2000
! Decide dubious 850mb & 700mb heights by checking against height above.
!
! Revision 1.3  98/09/16  16:10:34  16:10:34  usmdb (Generic MDB account)
! 21/09/98 Correct temperature conversion from 273.15 to 273.1 so as
! to be consistent with our documentation and UASIGPR. Jon Lewthwaite.
!
! Revision 1.2  98/08/12  08:44:45  08:44:45  usmdb (Generic MDB account)
! put values in array before incrementing pointer to avoid losing
! last group because of check marked !c. (no lines marked !e because
! lines only rearranged!)                                             !e
!
! Revision 1.1  98/02/19  11:56:05  11:56:05  usmdb (Generic MDB account)
! Initial revision
!
! 23/01/98 - accept 925mb data in 52525 section (block 94)            !d
!
! 23/01/98 - call UATSTX to try to recover from bad figure            !c
!            & check against length after incrementing PTR!
!
! 08/12/97 - undo the above change                                    !b
!          - decide whether wind group expected at lower levels in    !b
!            Temp A by searching for 85 & 92 rather than trusting Id
!          - don't give up if ///// wind where no group expected      !b
!          - only look in same group for next ident if no 92 or 00    !b
!          - only look for 775 & 600mb data in 51515 section          !b
!          - don't change the sign if T>0 above 400mb (let Q/C do it)
!
! 01/09/97 - Change position of pointer increment to after the wind
!            group decode so that if the wind group is duff (/////)
!            then the pointer will still be incremented properly.     !A
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!declare character
      CHARACTER STRING*(*)              !part of report being expanded
      CHARACTER PART*1
      CHARACTER IND*2
      CHARACTER*2 LEVEL(20)                                          !b
      CHARACTER HEAD*132                 !revision information

!declare real
      REAL ARRAY(*)                      !array of decoded values
      REAL QCBIT_ARRAY(999)              !array of 1bit QC flags
      REAL K2MS                          !knot to m/s conversion
      REAL C2KELVIN                      !degree c to kelvin

!declare integer
      INTEGER PTR                        !pointer within report
      INTEGER P                          !pressure
      INTEGER HT                         !height
      INTEGER T                          !temperature
      INTEGER TD                         !dew point depression
      INTEGER DD                         !wind direction
      INTEGER FF                         !wind speed
      INTEGER PHT
      INTEGER L
      INTEGER MAXL
      INTEGER IVALUE                     !function subprog
      INTEGER IND92                      !start of 925mb data        !b
      INTEGER IND85                      !start of 850mb data        !b
      INTEGER IND77                      !start of 775mb data        !b
      INTEGER IND60                      !start of 600mb data        !b
      INTEGER NG9900                     !groups for surface & 1000mb !b
      INTEGER STNHT
      INTEGER IND1
      INTEGER ID
      INTEGER IRC                        ! return code from UATSTX   !c
      INTEGER MISSING                    !indicates missing data
      INTEGER PTR99                      !pointer to 99... for print !b
      INTEGER PSTD(20)                                               !b
      INTEGER BASE                       !base displacement in array
      INTEGER NUM_LEV                    !number of standard levels
      INTEGER LEV_TYPE                   !indicates type of level
      INTEGER H850          ! subscript of 850mb height in ARRAY   !1.4
      INTEGER H700          ! subscript of 700mb height in ARRAY   !1.4
      INTEGER H500          ! subscript of 500mb height in ARRAY   !1.4

!declare logical
      LOGICAL HEADSET                                               !2.1
      LOGICAL KNOTS                      !indicates speed in knots
      LOGICAL WINDGP                     ! set if wind group expected !b

      SAVE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the data consists of heights, temperatures and perhaps winds for   !
! standard levels (3 groups per level if wind, 2 if not), the level  !
! being indicated by 2 figures of the pressure, the presence of a    !
! wind by an indicator in the date/time group.  the height has to be !
! reconstructed from the 3 figures given.                            !
! indicators for standard levels & corresponding pressures (pascals) !
! - 12 for part a, 5 for part c, 3 for 51515 section in part b.      !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      DATA PSTD/-9999999, 100000,92500,85000,70000,50000,
     &             40000,30000,25000,20000,15000,10000,
     &              7000, 5000, 3000, 2000, 1000,
     &             92500,77500,60000/                             !b

      DATA LEVEL/'99', '00', '92', '85', '70', '50',
     &           '40', '30', '25', '20', '15', '10',
     &           '70', '50', '30', '20', '10',
     &           '92', '77', '60'/                                !b

      DATA HEADSET/.FALSE./                                         !2.1

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:44$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

      IF (PART .EQ. 'A' .OR. PART .EQ. 'C') THEN
        NUM_LEV=0                          !first level to start with
        BASE=8                             !base displacement
      ENDIF
      LEV_TYPE=32                          ! standard levels

      K2MS=(1852./3600.)                   ! knots to m/s
      C2KELVIN=273.1                       ! C to Kelvin            1.3
      MISSING=-9999999

      PTR99=PTR   ! keep start to print message if in error          !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! decide (from part, a or c) starting point in above arrays          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (PART.EQ.'A') THEN
        L=1
        MAXL=12
      ELSE IF (PART.EQ.'C') THEN
        L=13
        MAXL=17
      ELSE IF (PART.EQ.'B') THEN
        L=18
        MAXL=20                                                      !b
        IF (STRING(PTR-6:PTR-2).EQ.'52525') THEN                     !d
          L=3                                                        !d
          MAXL=3                                                     !d
        ENDIF                                                        !d
      ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Find starts of levels below 850mb if it's a Part A.                !b
! The 850mb and 925mb identifiers can be recognised unambiguously;   !b
! the 1000mb indicator could occur in a T or wind group, so deduce   !b
! its position from the number of groups for the lowest two levels,  !b
! assuming that the surface wind is only missing if there are no     !b
! winds at all, i.e. only if only 4 groups for the 2 levels.         !b
!                                                                    !b
! This approach was adopted (Dec 97) because too much data was lost  !b
! by expecting TTAAs to adhere strictly to the Manual on codes.      !b
! It was found that                                                  !b
! - not all obs have a surface wind, although it's now mandatory     !b
! - not all obs have 925mb data, although it's now standard          !b
! - some obs omit a ///// wind group when 1000mb is below the        !b
!    surface even if Id says all levels have winds                   !b
! - some obs set Id to the top level with a wind rather than with    !b
!    a wind group, i.e. ///// is coded where not expected            !b
! - 06476 omits 1000mb because its surface is nearer 925mb           !b
! These errors can be treated as differences of interpretation,      !b
! easier to handle here than to get changed at source.               !b
!                                                                    !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (PART.EQ.'A') THEN                                          !b
        IND85=INDEX(STRING(PTR:),' 85')                              !b
        IND92=INDEX(STRING(PTR:),' 92')                              !b

        IF (IND92.GT.0 .AND. IND92.LT.IND85) THEN                    !b
          NG9900=IND92/6                                             !b
        ELSE IF (IND92.EQ.0 .AND. IND85.GT.0) THEN                   !b
          NG9900=IND85/6                                             !b
        ELSE
          GO TO 100             ! if no 92 or 85 give up!            !b
        ENDIF                                                        !b

! Give up if no 00 in right place.                                   !b
! Carry on if only 3 groups for bottom 2 levels: may be no 1000mb    !b

        IF (NG9900.LT.3. OR. NG9900.GT.6) GO TO 100                  !b
        IF (NG9900.EQ.4.AND.STRING(PTR+12:PTR+13).NE.'00') GO TO 100 !B
        IF (NG9900.GT.4.AND.STRING(PTR+18:PTR+19).NE.'00') GO TO 100 !B
      ENDIF                                                          !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! If it's a part B, only look for 775 & 600mb data, assuming both
! levels have winds if they start 3 groups apart, no wind if 2...
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF (PART.EQ.'B') THEN                                          !b
        IND92=INDEX(STRING(PTR-1:),' 92')                            !d
        IND77=INDEX(STRING(PTR-1:),' 77')                            !b
        IND60=INDEX(STRING(PTR-1:),' 60')                            !b
        IF (IND60-IND77.EQ.12 .OR. IND60-IND77.EQ.18) THEN           !B
          IF (IND92.EQ.0) THEN ! if no 925mb data,                   !d
            L=19               ! point to '77' as first indicator    !b
            PTR=PTR-1+IND77    ! move pointer to start of 775mb data !B
            IF (PTR.GE.LEN(STRING)) GO TO 100                      !1.4
          ENDIF                                                      !b
        ELSE                                                         !b
          GO TO 100            ! give up if 51515 section not right  !b
        ENDIF                                                        !b
      ENDIF                                                          !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Check indicator & get height (pressure if surface data)             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    1 P=MISSING
      HT=MISSING
      T=MISSING
      TD=MISSING
      DD=MISSING
      FF=MISSING

      IND=STRING(PTR:PTR+1)
      IF (IND.EQ.LEVEL(L)) THEN
        NUM_LEV=NUM_LEV+1
        P=PSTD(L)
        PHT=IVALUE(STRING(PTR+2:PTR+4))               ! PRESS? HEIGHT?
        IF (PHT.NE.MISSING) THEN
          IF (PART.EQ.'A') THEN
            IF (IND.EQ.'99') THEN                     ! SURFACE PRESSURE
              P=PHT*100                               ! MB TO PASCALS
              IF (P.LT.10000) THEN                    ! MAY BE >1000MB
                P=P+100000
              ENDIF

              IF (ARRAY(7) .NE. MISSING) THEN
                HT=ARRAY(7)                           ! STATION HEIGHT
              ELSE
                HT=ARRAY(8)
              ENDIF
              STNHT=HT
            ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Put height in usable form, adding thousands figure etc.             !
! The thousands figures of the 850mb & 700mb heights are decided   !1.4
! using an approach described by K H Hinkelmann, WMO Regional      !1.4
! Training Seminar on Numerical Weather Prediction, Moscow, 1965:  !1.4
! ("<600 reported" etc refer to the 3 figures hhh in 85hhh/70hhh)  !1.4
!  H850>1000m if <600 reported,                                    !1.4
!  H850<1000m if >800 reported,                                    !1.4
!   otherwise H850>1000m if H700>2700m.                            !1.4
!  H700>3000m if <200 reported,                                    !1.4
!  H850<3000m if >400 reported,                                    !1.4
!   otherwise H700>3000m if H500>5100m.                            !1.4
! Set H850>1000m if <800 reported & H700>3000m if <400 reported,   !1.4
! then deal with the 600-800 & 200-400 ranges at the end when      !1.4
! heights can be cross-checked.                                    !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            IF (IND.EQ.'00') THEN                     ! 1000MB HEIGHT
              IF (PHT.GE.500) THEN                    ! MAY BE NEGATIVE
                PHT=500-PHT
              ENDIF
            ENDIF
            IF (IND.EQ.'00' .OR. IND.EQ.'92') THEN    ! BOTH <1000M
              HT=PHT
            ENDIF
            IF (IND.EQ.'85') THEN                     ! 850MB (C.1500M)
              IF (PHT.LT.800) THEN                                 !1.4
                HT=PHT+1000                                        !1.4
              ELSE                                                 !1.4
                HT=PHT                                             !1.4
              ENDIF                                                !1.4
            ENDIF
            IF (IND.EQ.'70') THEN                     ! 700MB (C.3000M)
              HT=PHT+2000
              IF (HT.LE.2400) THEN                    !   >3000M?
                HT=HT+1000
              ENDIF
            ENDIF
            IF (IND.LE.'50' .AND. IND.GE.'10') THEN   ! 500MB & ABOVE
              HT=PHT*10                               !   DEKAMETRES
              IF (IND.EQ.'30' .OR. IND.EQ.'25') THEN  ! 300MB & 250MB
                IF (HT.LT.5000) THEN                  !   >10000M?
                  HT=HT+10000
                ENDIF
              ENDIF
              IF (IND.LE.'20') THEN                   ! 200MB & ABOVE
                HT=HT+10000
              ENDIF
            ENDIF                                     !   >10000M
          ELSE IF (PART.EQ.'C') THEN
            HT=PHT*10
            IF (IND.EQ.'70' .OR. IND.EQ.'50') THEN    ! 70MB & 50MB
              HT=HT+10000
              IF (HT.LT.15000) THEN                   !   >20000M?
                HT=HT+10000
              ENDIF
            ENDIF
            IF (IND.EQ.'30' .OR. IND.EQ.'20') THEN
              HT=HT+20000
            ENDIF
            IF (IND.EQ.'10') THEN                     ! 10MB
              HT=HT+20000
              IF (HT.LT.25000) THEN                   !   >30000M?
                HT=HT+10000
              ENDIF
            ENDIF
          ELSE IF (PART.EQ.'B') THEN
            HT=PHT
            IF (IND.EQ.'77') THEN                                    !b
              HT=HT+2000
            ENDIF
            IF (IND.EQ.'60') THEN
              HT=HT+4000
            ENDIF
          ENDIF
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Set pressure & height in output array, then advance pointer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (IND.EQ.'99') THEN
          ARRAY(BASE+(7*NUM_LEV))=64
        ELSE
          ARRAY(BASE+(7*NUM_LEV))=LEV_TYPE
        ENDIF

        ARRAY(BASE+(7*NUM_LEV)+1)=P            !pressure
        ARRAY(BASE+(7*NUM_LEV)+2)=HT           !height

! Keep subscripts of 850, 700 & 500mb heights for final check on   !1.4
! 850 & 700mb thousands figures.                                   !1.4

        IF (PART.EQ.'A') THEN                                      !1.4
          IF (IND.EQ.'85') H850=BASE+7*NUM_LEV+2                   !1.4
          IF (IND.EQ.'70') H700=BASE+7*NUM_LEV+2                   !1.4
          IF (IND.EQ.'50') H500=BASE+7*NUM_LEV+2                   !1.4
        ENDIF                                                      !1.4

        QCBIT_ARRAY(BASE+(7*NUM_LEV))=0        !level type okay
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0      !pressure okay
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0      !height okay

        PTR=PTR+6
        IF (PTR.GE.LEN(STRING)) GO TO 100                          !1.4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Temperature & dew point                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        T=IVALUE(STRING(PTR:PTR+2))         ! TEMPERATURE IN TENTHS

        IF (T.GT.-9999) THEN
          IF (MOD(T,2).EQ.1) THEN           ! <0 IF TENTHS FIGURE ODD
            T=-T
          ENDIF
          TD=IVALUE(STRING(PTR+3:PTR+4))    ! DEW POINT DEPRESSION

          IF (TD .NE. MISSING) THEN
            IF (TD.GT.50) THEN              ! IN WHOLE DEGREES IF >50
              TD=(TD-50)*10
            ENDIF
            TD=T-TD
          ELSE
            TD=MISSING
          endif
        ELSE
          T=MISSING
          TD=MISSING
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Put temperature and dew point in output array, then advance pointer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (T.NE.MISSING) THEN
          ARRAY(BASE+(7*NUM_LEV)+3)=T*(1./10.)+C2KELVIN   !TEMPERATURE
          IF (TD .NE. MISSING) THEN
            ARRAY(BASE+(7*NUM_LEV)+4)=TD*(1./10.)+C2KELVIN !DEW POINT
          ELSE
            ARRAY(BASE+(7*NUM_LEV)+4)=MISSING             !NO DEWPOINT
          ENDIF
        ELSE
          ARRAY(BASE+(7*NUM_LEV)+3)=MISSING               !NO TEMP
          ARRAY(BASE+(7*NUM_LEV)+4)=MISSING
        ENDIF

        QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0                 !TEMP OKAY
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0                 !DEWPOINT OK

        PTR=PTR+6
        IF (PTR.GE.LEN(STRING)) GO TO 100                          !1.4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Wind: the levels below 850mb have already been delimited; at 850mb !b
! and above expect a wind group if Id indicates one (but allow below !b
! for a ///// group where Id says there is no wind!)                 !b
! N.B. Id=/ and Id=0 both mean no winds at 850mb and above.          !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IND1=IVALUE(IND(1:1))               ! first figure of ident
        WINDGP=.TRUE.                                                !b
        IF (PART.EQ.'A') THEN                                        !b
          IF (L.EQ.1 .AND. NG9900.EQ.4) WINDGP=.FALSE.               !b
          IF (L.EQ.2 .AND. NG9900.LT.6) WINDGP=.FALSE.               !b
          IF (L.EQ.3 .AND. IND85-IND92.LT.18) WINDGP=.FALSE.         !b
          IF (L.GE.4 .AND. (ID.LE.0 .OR.IND1.LT.ID)) WINDGP=.FALSE.  !b
        ELSE IF (PART.EQ.'B') THEN                                   !b
          IF (IND60-IND77.EQ.12) WINDGP=.FALSE.                      !b
        ELSE IF (PART.EQ.'C') THEN                                   !b
          IF (ID.LT.0 .OR. IND1.LT.ID) WINDGP=.FALSE.                !b
        ENDIF                                                        !b

        IF (WINDGP) THEN                                             !b
          DD=IVALUE(STRING(PTR:PTR+1))
          FF=IVALUE(STRING(PTR+2:PTR+4))
          IF (DD.NE.MISSING) THEN
            DD=DD*10
            IF (FF.GE.500) THEN             ! check wind direction
              FF=FF-500
              DD=DD+5
            ENDIF
!           IF (DD.GT.360) DD=MISSING       ! do this below!         !b
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Put wind in output array, then advance pointer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (DD .GT. 360) THEN
            DD=MISSING                             !invalid direction
            ARRAY(BASE+(7*NUM_LEV)+5)=DD           !set missing
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=1      !direction suspect
          ELSE
            ARRAY(BASE+(7*NUM_LEV)+5)=DD           !direction okay
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
          ENDIF

          IF (FF .NE. MISSING) THEN
            IF (KNOTS) THEN
              ARRAY(BASE+(7*NUM_LEV)+6)=FF*K2MS
            ELSE
              ARRAY(BASE+(7*NUM_LEV)+6)=FF
            ENDIF
          ELSE
            ARRAY(BASE+(7*NUM_LEV)+6)=MISSING
          ENDIF

          QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0        !speed okay

          PTR=PTR+6                                                  !A
          IF (PTR.GE.LEN(STRING)) GO TO 100                        !1.4
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! If the indicator doesn't match, return if it's a new section.      !
! If there's no 925mb indicator, look for 850mb in the same group.  !b
! If there's a slashed (wind) group look again in the next group.   !b
! If there's no 1000mb data (NG9900=3), look on for 925 or 850mb.   !b
! Otherwise accept that we're out of step and give up!              !b
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      ELSE IF (IND.EQ.'88' .OR. IND.EQ.'77' .OR. IND.EQ.'66') THEN
        GOTO 100       ! finished!
      ELSE IF ((L.LE.3 .AND. NG9900.EQ.3) .OR.                      !b
     &         (L.EQ.3 .AND. IND92.EQ.0)) THEN                      !b
        L=L+1          ! look for next ident in same group          !b
        IF (L.LE.MAXL) GOTO 1                                       !2.1
      ELSE IF (STRING(PTR:PTR+4).EQ.'/////') THEN                   !b
        PTR=PTR+6      ! look for same ident in next group          !b
        IF (PTR.GE.LEN(STRING)) GO TO 100                           !1.4
        GO TO 1                                                     !b
      ELSE                                                          !b
        print *,'UATSTDP: ',LEVEL(L),' not found in ',
     &          STRING(PTR-18:PTR+4),'     Id=',ID

! try to recover: if so, try again with either both L and pointer   !c
! changed or Id or level indicator corrected                        !c

        IRC=0                                                       !c
        CALL UATSTX(STRING,PTR,LEVEL,L,MAXL,ID,IRC)                 !c
        IF (IRC.GT.1) GO TO 1                                       !c
        GO TO 100      ! give up                                    !b
      ENDIF

      L=L+1
      IF (L.LE.MAXL) GO TO 1

  100 CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Before returning do some cross-checks which may reset the        !1.4
! thousands figures of the 850 & 700mb heights:                    !1.4
!  if 3200<H700<3400m & H500<5100m, set H700 thousands figure to 2 !1.4
!  if 1600<H850<1800m & H700<2700m, set H850 thousands figure to 0 !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF (PART.EQ.'A') THEN                                        !1.4
        IF (ARRAY(H500).NE.MISSING.AND.ARRAY(H500).LT.5100.) THEN  !1.4
          IF (ARRAY(H700).GT.3200..AND. ARRAY(H700).LT.3400.) THEN !1.4
            ARRAY(H700)=ARRAY(H700)-1000.                          !1.4
          ENDIF                                                    !1.4
        ENDIF                                                      !1.4
        IF (ARRAY(H700).NE.MISSING.AND.ARRAY(H700).LT.2700.) THEN  !1.4
          IF (ARRAY(H850).GT.1600..AND. ARRAY(H850).LT.1800.) THEN !1.4
            ARRAY(H850)=ARRAY(H850)-1000.                          !1.4
          ENDIF                                                    !1.4
        ENDIF                                                      !1.4
      ENDIF                                                        !1.4
      RETURN
      END
