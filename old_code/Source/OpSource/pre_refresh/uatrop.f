      SUBROUTINE UATROP(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : UATROP                                              
!                                                                     
! PURPOSE       : EXPAND TROPOPAUSE DATA IN TEMP PART A/C             
!                                                                     
! DATA TYPE(S)  : UPPER AIR TEMP PART A/C                             
!                                                                     
! CALLED BY     : UAXPAND                                             
!                                                                     
! CALLS         : IVALUE (FUNCTION)                                 !1.4
!                                                                     
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES  
!                 (2) INPUT POINTER TO START OF FIRST TROPOPAUSE      
!                      (POINTS PAST END OF SECTION ON RETURN)         
!                 (3) OUTPUT ARRAY, WITH STANDARD LEVEL DATA IN IT    
!                 (4) number of levels already in array    (i/o)    !1.4
!                 (5) PART (A OR C)                                   
!                 (6) FLAG SET ON IF WINDS IN KNOTS                   
!                 (7) output 1 bit QC flags                           
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uatrop.F,v $
!                                                                     
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:43    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:36  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.5  2000/03/10  09:59:37  09:59:37  usmdb (Generic MetDB account)
! 20 March 2000    C Long
! Don't leave dd set as 88, 77, 66 when no wind reported!
! 
! Revision 1.4  99/09/09  10:00:15  10:00:15  usmdb (Generic MDB account)
! 20 Sept 99     C Long
! 1.4  Increment NUM_LEV at start rather than end
!      (This goes with a change to UAXPAND)
! 1.4a Change C/K conversion from 273.15 to 273.1 (temperatures in tenths)
!
! Revision 1.3  98/09/16  16:10:56  16:10:56  usmdb (Generic MDB account)
! 21/09/1998 Additional checks to stop 'out-of-bounds' conditions.
! v(G)= 19 ev(G)=4 Jon Lewthwaite
!
! Revision 1.2  97/07/31  11:46:16  11:46:16  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:46:53  uspm
! Initial revision
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

!declare character
      CHARACTER OB*(*)                      !report being expanded
      CHARACTER PART*1
      CHARACTER HEAD*132                    !revision information

!declare real
      REAL QCBIT_ARRAY(999)                 !array of 1 bit QC flags
      REAL ARRAY(*)                         !array of decoded values
      REAL C2KELVIN                         !degree c to kelvin
      REAL KT2MS                            !knots to ms
-1 conversion

!declare integer
      INTEGER PTR                           !pointer within report
      INTEGER P
      INTEGER T                             !temperature
      INTEGER TD                            !dew point depression
      INTEGER DD                            !wind direction
      INTEGER FF                            !wind speed
      INTEGER NTROP                         !number of trops. found
      INTEGER IVALUE                        !function subprog
      INTEGER L
      INTEGER NUM_LEV                       !NUMBER OF LEVELS
      INTEGER MISSING                       !indicates missing
      INTEGER BASE

!declare logical
      LOGICAL KNOTS                         !indicates wind speed knots
      logical debug
      SAVE

!initialize variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uatrop.F,v $
     &'//'$ $Date: 30/01/2006 20:25:43$ $Revision: 1$'

      BASE=8
      C2KELVIN=273.1      ! temperature in tenths                  !1.4a
      MISSING=-9999999
      KT2MS=(1852./3600.)
      debug=.false.
      NTROP=0
    1 P=MISSING
      T=MISSING
      TD=MISSING
      DD=MISSING
      FF=MISSING

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! 3 groups per tropopause (can be >1 trop): 88 with 3-fig press, temp!
! group, wind.                                                       !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      P=IVALUE(OB(PTR+2:PTR+4))
      IF (P.NE.MISSING) THEN
        NUM_LEV=NUM_LEV+1                   ! one more level       !1.4
        IF (PART.EQ.'A') P=P*100            ! PRESSURE TO PASCALS
        IF (PART.EQ.'C') P=P*10             ! (PART C TENTHS OF HPA)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! temperature & dew point                                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        T=IVALUE(OB(PTR+6:PTR+8))           ! TEMPERATURE IN TENTHS
        IF (T.NE.MISSING) THEN
          IF (MOD(T,2).EQ.1) T=-T           ! <0 IF TENTHS FIGURE ODD
          TD=IVALUE(OB(PTR+9:PTR+10))       ! DEW POINT DEPRESSION
          IF (TD.GT.50) TD=(TD-50)*10       ! IN WHOLE DEGREES IF >50
          IF (TD.NE.MISSING) TD=T-TD        ! DEPRESSION TO DEW POINT
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! wind             (there should always be a wind, at least slashes   !
!                    - but check for next indicator just in case)     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        L=LEN(OB(PTR:))
        IF (L.GE.14) THEN                                          !1.3
          DD=IVALUE(OB(PTR+12:PTR+13))
        ENDIF                                                      !1.3
        IF (L.GE.17 .AND. DD.NE.88 .AND. DD.NE.77 .AND. DD.NE.66) THEN
          FF=IVALUE(OB(PTR+14:PTR+16))
          IF (DD.NE.MISSING) THEN
            DD=DD*10
            IF (FF.GE.500) THEN            !check for accuracy in dir.
              FF=FF-500                    !500 added to speed if 5 deg.
              DD=DD+5
            ENDIF
            IF (DD.GT.360) THEN            !check for valid direction
              DD=MISSING
              QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=1
            ELSE
              QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
            ENDIF
          ENDIF
          PTR=PTR+18                       !move pointer on past group
        ELSE
          PTR=PTR+12
          DD=MISSING                       !don't leave dd=77 etc  !1.5
        ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! set values in output array for encoding                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with level type, pressure and height                            !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ARRAY(BASE+(7*NUM_LEV))=16                     !level type=trop
        ARRAY(BASE+(7*NUM_LEV)+1)=P                    !set pressure
        ARRAY(BASE+(7*NUM_LEV)+2)=MISSING              !no height
        QCBIT_ARRAY(BASE+(7*NUM_LEV))=0
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with air temperature and dew point                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (T .NE. MISSING) THEN
          ARRAY(BASE+(7*NUM_LEV)+3)=T*(1./10.)+C2KELVIN  !temperature
        ELSE
          ARRAY(BASE+(7*NUM_LEV)+3)=MISSING
        ENDIF

        IF (TD .NE. MISSING) THEN
          ARRAY(BASE+(7*NUM_LEV)+4)=TD*(1./10.)+C2KELVIN  !temperature
        ELSE
          ARRAY(BASE+(7*NUM_LEV)+4)=MISSING
        ENDIF

        QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with wind direction and speed.                                  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ARRAY(BASE+(7*NUM_LEV)+5)=DD                      !wind direct
        IF (FF .NE. MISSING) THEN
          IF (KNOTS) THEN
            ARRAY(BASE+(7*NUM_LEV)+6)=FF*KT2MS            !wind speed
          ELSE
            ARRAY(BASE+(7*NUM_LEV)+6)=FF                  !wind speed
          ENDIF
        ELSE
          ARRAY(BASE+(7*NUM_LEV)+6)=MISSING
        ENDIF
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0
        QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0

!1.4 increment NUM_LEV at start of IF block, not end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check to see if another tropopause has been reported. If there is then
!go back to the beginning and decode.                                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF (PTR .LT. LEN(OB)) THEN                                  !1.3
          IF (OB(PTR:PTR+1).EQ.'88') THEN
            GOTO 1
          ENDIF
        ENDIF                                                       !1.3
      ENDIF
      RETURN
      END
