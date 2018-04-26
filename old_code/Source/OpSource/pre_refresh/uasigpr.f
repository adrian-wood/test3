      SUBROUTINE UASIGPR(STRING,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS,
     &                   BADSEQ,SIG_BASE,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UASIGPR (Upper Air SIGnificant levels with Pressure)
!
! PURPOSE       : EXPAND SIGNIFICANT LEVELS WITH PRESSURE AS VERTICAL
!                 COORDINATE IN TEMP/PILOT PART B OR D
!
! DATA TYPE(S)  : UPPER AIR TEMP B/D & PILOT B/D WITH PRESSURE SENSOR
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION)
!
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO FIRST SIGNIFICANT LEVEL
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (4) OUTPUT SUBSCRIPT (NOT INCREMENTED HERE)
!                 (5) PART (B OR D OR MISSING IF 51515 SECTION)
!                 (6) TEMPERATURES OR WINDS (TRUE IF WINDS)
!                 (7) FLAG SET ON IF WINDS IN KNOTS  (--> *.5148)
!                 (8) FLAG SET ON IF PRESSURES NOT DECREASING
!                 (9) NUMBERS OF TEMPERATURES & WINDS IN REPORT
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:41$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uasigpr.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:41    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:08:19  usmdb
! Checks added to prevent string out of bounds - S.Cox
!
! Revision 2.0  2001/07/03  10:44:35  10:44:35  usmdb (Generic MetDB account)
! Moved DATA statement before executable section. Removed unused
! variable. Added copyright and modified header - S.Cox
!
! Revision 1.5  98/09/16  16:10:58  16:10:58  usmdb (Generic MetDB account)
! 21/09/1998 Additional checking to trap more 'out-of-bounds'
! conditions. v(G)=33 ev(G)=3 Jon Lewthwaite
!
! Revision 1.4  98/08/12  08:45:47  08:45:47  usmdb (Generic MDB account)
! Remove some old code from when calls for 51515 section were
! different (code to reset NUM_LEV lost any 51515 data from first
! ob in run)
!
! Revision 1.3  98/01/27  10:15:39  10:15:39  usmdb (Generic MDB account)
! Add 900m height                                                     !B
!
! Revision 1.2  1997/07/31 11:45:47  uspm
! First revision for 1
!
! Revision 1.1  1997/07/04 14:43:48  uspm
! Initial revision
!
! 20/12/96 CORRECT NO. OF LEVELS IF LEVEL SET MISSING                 !A
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
      CHARACTER STRING*(*)        !report
      CHARACTER PART*1            !report part
      CHARACTER*2 IND(10)         !group indicator
      CHARACTER HEAD*132          !revision information

!declare integer
      INTEGER PTR                 !pointer within report
      INTEGER P                   !pressure
      INTEGER T                   !air temperature
      INTEGER TD                  !dew point temperature
      INTEGER DD                  !wind direction
      INTEGER FF                  !wind speed
      INTEGER NUM_LEV             !number of levels
      INTEGER L
      INTEGER LASTP               !last pressure
      INTEGER MISSING             !indicates missing
      INTEGER IVALUE              !function subprog
      INTEGER SIG_BASE            !array displacement base sig report
      INTEGER HEIGHT
      INTEGER LENG
!declare real
      REAL QCBIT_ARRAY(999)       !qcbit array
      REAL ARRAY(*)               !expanded elements array
      REAL KT2MS                  !knots to ms
-1 conversion
      REAL C2KELVIN               !degree c to kelvin

!declare logical
      LOGICAL KNOTS               !indicates speed in knots
      LOGICAL BADSEQ
      LOGICAL WINDS               !indicates wind or temp decode
      LOGICAL DEBUG
      LOGICAL HEADSET                                               !2.1

      SAVE

      DATA HEADSET/.FALSE./                                         !2.1
      DATA IND/
     & '00','11','22','33','44','55','66','77','88','99'/           !2.0

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: uasigpr.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:41$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

      KT2MS=(1852./3600.)         !KNOTS TO M/S
      C2KELVIN=273.1
      MISSING=-9999999.
      P=-9999999
      T=-9999999
      TD=-9999999
      DD=-9999999
      FF=-9999999
      DEBUG=.FALSE.
      LASTP=MISSING
      BADSEQ=.FALSE.
      LENG=LEN(STRING)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the data consists of 2 groups per level, the first having a repeated!
! figure as a sequence number, then a 3-figure pressure (mb if part b,!
! tenths if part d) with any thousands figure omitted; the second is  !
! either a temperature or wind group as for standard levels.          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (PART.EQ.'B') THEN                   ! '00' FOR SURFACE
        L=1
      ENDIF
      IF (PART.EQ.' ') THEN                   ! 11, 22, 33 IF 51515
        L=2
      ENDIF
      IF (PART.EQ.'D') THEN                   ! NO SURFACE IF PART D
        L=2
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! convert the pressure (mb in part b, tenths in part d), separate dd  !
! & ff, construct td from t and the dewpoint depression.              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    1 IF (STRING(PTR:PTR+1).EQ.IND(L)) THEN   ! IF NOT OUT OF STEP,
        IF (PART .EQ. ' ' .AND. L .EQ. 2) THEN                       !B
          HEIGHT=900.0                                               !B
        ELSE                                                         !B
          HEIGHT=MISSING                                             !B
        ENDIF                                                        !B

        IF ((PTR+4).LE.LENG) THEN                                   !2.1
          P=IVALUE(STRING(PTR+2:PTR+4))       ! CONVERT PRESSURE    !2.1
        ELSE                                                        !2.1
          P=MISSING                                                 !2.1
        ENDIF                                                       !2.1
        IF (P.NE.MISSING) THEN
          IF (PART.EQ.'B' .AND. P.LT.100) P=P+1000  ! P CAN BE >1000
          IF (PART.EQ.'B') P=P*100            ! MB TO PASCALS
          IF (PART.EQ.' ') P=P*100            ! MB TO PASCALS (51515)
          IF (PART.EQ.'D') P=P*10             ! MB IN B, TENTHS IN D
          IF (WINDS) THEN
            IF ((PTR+7).LE.LENG) THEN                               !2.1
              DD=IVALUE(STRING(PTR+6:PTR+7))                        !2.1
            ELSE                                                    !2.1
              DD=MISSING                                            !2.1
            ENDIF                                                   !2.1
            IF ((PTR+10).LE.LENG) THEN                              !2.1
              FF=IVALUE(STRING(PTR+8:PTR+10))                       !2.1
            ELSE                                                    !2.1
              FF=MISSING                                            !2.1
            ENDIF                                                   !2.1
            IF (DD.NE.MISSING) THEN
              DD=DD*10
              IF (FF.GE.500) THEN
                FF=FF-500
                DD=DD+5
              ENDIF
            ENDIF
          ELSE
            IF ((PTR+8).LE.LENG) THEN                               !2.1
              T=IVALUE(STRING(PTR+6:PTR+8))   ! TEMP IN 10THS       !2.1
            ELSE                                                    !2.1
              T=MISSING                                             !2.1
            ENDIF                                                   !2.1
            IF (T.NE.MISSING) THEN
              IF (MOD(T,2).EQ.1) T=-T         ! <0 IF TENTHS FIGURE ODD
              IF ((PTR+10).LE.LENG) THEN                            !2.1
                TD=IVALUE(STRING(PTR+9:PTR+10)) ! DEW PT DEPRESSION !2.1
              ELSE                                                  !2.1
                TD=MISSING                                          !2.1
              ENDIF                                                 !2.1
              IF (TD.GT.50) TD=(TD-50)*10     ! IN WHOLE DEGREES IF >50
              IF (TD.NE.MISSING) TD=T-TD      ! DEPRESSION TO DEW POINT
            ENDIF
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! a slashed level (missing data) means the wind or temperature was   !
! unknown between the pressures above & below.  set the pressure to  !
! just above the previous level & leave the temperature or wind      !
! missing to stop interpolation in that layer.                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ELSEIF (LASTP.NE.MISSING) THEN
          P=LASTP-1
          IF (WINDS) THEN
            DD=MISSING
            FF=MISSING
          ELSE
            T=MISSING
            TD=MISSING
          ENDIF
        ENDIF
*
        PTR=PTR+12                            ! POINT PAST TWO GROUPS
        L=L+1                                 ! NEXT INDICATOR
        IF (L.GT.10) L=2                       ! '11' FOLLOWS '99'

        IF (LASTP.NE.MISSING .AND. P.GT.LASTP) BADSEQ=.TRUE.
        LASTP=P                               ! IN CASE NEXT P MISSING
*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with pressure wind direction and wind speed                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ARRAY(SIG_BASE+(7*NUM_LEV)+1)=P       !set pressure
        QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+1)=0.

        IF (WINDS) THEN                       !sig level winds
          IF (DD.NE.MISSING) THEN
            IF (DD .GT. 360) THEN             !check valid direction
              DD=MISSING
              ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
              QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=1. !suspect direction
            ELSE
              ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
              QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0. !OKAY direction
            ENDIF
          ELSE
            ARRAY(SIG_BASE+(7*NUM_LEV)+5)=MISSING    !direction missing
            QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.   !direction okay
          ENDIF

          IF (FF .NE. MISSING) THEN
            IF (DD .NE. MISSING) THEN
              IF (KNOTS) THEN                          !speed in knots
                ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF*KT2MS !and convert
              ELSE
                ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF       !speed in m/s
              ENDIF
            ENDIF
          ELSE
            ARRAY(SIG_BASE+(7*NUM_LEV)+6)=MISSING    !speed missing
          ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set qcbit for wind speed                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.     !speed okay

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!sig level winds,so height,temperature and dew point must be set to   !
!missing.                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ARRAY(SIG_BASE+(7*NUM_LEV)+2)=HEIGHT       !height missing  !B
          ARRAY(SIG_BASE+(7*NUM_LEV)+3)=MISSING      !temp. missing
          ARRAY(SIG_BASE+(7*NUM_LEV)+4)=MISSING      !dew point missing
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.     !height okay
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.     !temp okay
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.     !dew point okay

        ELSE                                         !sig temps
          IF (T.NE.MISSING) THEN                     !air temperature
            ARRAY(SIG_BASE+(7*NUM_LEV)+3)=T*(1./10.)+C2KELVIN
          ELSE                                       !temp. missing
            ARRAY(SIG_BASE+(7*NUM_LEV)+3)=MISSING
          ENDIF

          IF (TD.NE.MISSING) THEN                    !dew point
            ARRAY(SIG_BASE+(7*NUM_LEV)+4)=TD*(1./10.)+C2KELVIN
          ELSE
            ARRAY(SIG_BASE+(7*NUM_LEV)+4)=MISSING    !dew point missing
          ENDIF

          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.     !temp. okay
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.     !dew point okay

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!sig level temps so no height wind direction or wind speed. These must!
!be set to missing                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ARRAY(SIG_BASE+(7*NUM_LEV)+2)=MISSING      !height missing
          ARRAY(SIG_BASE+(7*NUM_LEV)+5)=MISSING      !direction missing
          ARRAY(SIG_BASE+(7*NUM_LEV)+6)=MISSING      !speed missing
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.     !height okay
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.     !direction okay
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.     !speed okay
        ENDIF

        IF (WINDS) THEN
          ARRAY(SIG_BASE+(7*NUM_LEV))=2              !sig level WINDS
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.
        ELSE
          ARRAY(SIG_BASE+(7*NUM_LEV))=4              !sig level temps
          QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.
        ENDIF

        IF (P .NE. MISSING) THEN                                     !A
          NUM_LEV=NUM_LEV+1                                          !A
        ENDIF                                                        !A
        IF (PTR .LT. LENG) THEN                                     !1.5
          GO TO 1
        ENDIF                                                       !1.5
      ENDIF
      RETURN
      END
