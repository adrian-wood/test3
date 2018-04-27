      SUBROUTINE UAPSTD(OB,PTR,ARRAY,NUM_LEV,PART,BLOCK,STN,KNOTS,
     &                  QCBIT_ARRAY)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : UAPSTD  (Upper Air Pilot STanDard levels)           
!                                                                     
! PURPOSE       : EXPAND STANDARD LEVEL DATA IN PILOT PART A OR C     
!                                                                     
! DATA TYPE(S)  : UPPER AIR PILOT PART A/C                            
!                                                                     
! CALLED BY     : UAXPAND                                             
!                                                                     
! CALLS         : IVALUE (FUNCTION), STDPRHT (FUNCTION)               
!                                                                     
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES  
!                 (2) INPUT POINTER TO START OF FIRST STANDARD LEVEL  
!                      (POINTS PAST END OF SECTION ON RETURN)         
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT    
!                 (4) OUTPUT SUBSCRIPT (NOT INCREMENTED HERE)         
!                 (5) PART (A OR C)                                   
!                 (6) BLOCK NUMBER                                    
!                 (7) STATION NUMBER                                  
!                 (8) FLAG SET ON IF WINDS IN KNOTS  (--> *.5148)     
!                 (9) NUMBER OF WINDS IN REPORT                       
!                 (10) 1bit qc bit array output                       
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uapstd.F,v $
!                                                                     
! CHANGE RECORD :                                                     
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:40    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:34  usmdb
! Moved DATA statements before exectuable section. Added copyright
! and modified header - S.Cox
!
! Revision 1.4  2001/03/07  11:58:13  11:58:13  usmdb (Generic MetDB account)
! 19 March 2001    C Long
! Let PPAA start at any level accepted by UAXPAND
! 
! Revision 1.3  98/09/16  16:10:54  16:10:54  usmdb (Generic MetDB account)
! 21/09/1998 Additional checks to ensure that we do not go 'out-of-
! bounds' Jon Lewthwaite
!
! Revision 1.2  97/07/31  11:45:27  11:45:27  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:41:24  uspm
! Initial revision
!
! 17/03/97 : changed to pass all parts of a pilot to the function!
!            STDPRHT to calculate heights. Also new code to
!            set a bit flag in the trailer entry.                    !A
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
      CHARACTER OB*(*)            !report being decoded
      CHARACTER PART*1            !report part
      CHARACTER IND*2
      CHARACTER LEV*2
      CHARACTER*2 LEVEL(14)
      CHARACTER HEAD*132          !revision information

!declare integer
      INTEGER PTR                 !pointer within report
      INTEGER BLOCK               !wmo block no.
      INTEGER STN                 !wmo station number
      INTEGER P                   !pressure
      INTEGER HT                  !height
      INTEGER DD                  !wind direction
      INTEGER FF                  !wind speed
      INTEGER MISSING             !indicates missing
      INTEGER STDPRHT
      INTEGER L                   ! level subscript               !1.4
      INTEGER N
      INTEGER IVALUE
      INTEGER NUM_LEV
      INTEGER I
      INTEGER PRESS(14)
      INTEGER BASE                !BASE displacement of array
      INTEGER LENG                                                !1.3

!declare real
      REAL KT2MS                  !knots to ms
-1
      REAL ARRAY(*)               !array of decoded elements
      REAL QCBIT_ARRAY(999)       !array of 1 bit qc flags

!declare logical
      LOGICAL KNOTS               !indicates wind speed in knots if true

      SAVE

      DATA PRESS/
     & 85000, 70000, 50000, 40000, 30000, 25000, 20000, 15000, 10000,
     &  7000,  5000,  3000,  2000,  1000/

      DATA LEVEL/
     & '85',  '70',  '50',  '40',  '30',  '25',  '20',  '15',  '10',
     & '70',  '50',  '30',  '20',  '10'/

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uapstd.F,v $
     &'//'$ $Date: 30/01/2006 20:25:40$ $Revision: 1$'

      BASE=8                                                        !2.0
      MISSING=-9999999
      KT2MS=(1852./3600.)
      LENG=LEN(OB)                                                  !1.3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the above list gives the standard levels in part a and then part c  !
! (pilots have no 1000mb or 925mb level)                              !
! The first group must end 85, 70 or 50 to get through UAXPAND.    !1.4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (PART.EQ.'A') THEN
        DO I=1,3                                                   !1.4
          IF (OB(PTR+3:PTR+4).EQ.LEVEL(I)) L=I                     !1.4
        ENDDO                                                      !1.4
      ELSE IF (PART.EQ.'C') THEN                                   !1.4
        L=10
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the data consists of a pressure group followed by up to 3 winds,    !
! this sequence being repeated as often as necessary.  the pressure   !
! group has an indicator (44 or 55), a count (up to 3) and a 2-figure !
! level (as in the above array) for the first of the winds that follow!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    1 IND=OB(PTR:PTR+1)
      N=IVALUE(OB(PTR+2:PTR+2))
      LEV=OB(PTR+3:PTR+4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! an indicator of 44 means that the pilot has a pressure sensor, so the
! pressures are genuine pressures and the heights unknown; 55 indicates
! that heights have been measured and the pressures are only nominal -!
! heights are generated for all reports regardless of instumentation  !
! trailer byte 17 bit 5 is set to indicate if the part being decoded  !
! had a pressure sensor - this is used later in retrieval to inform   !
! users.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (IND.EQ.'44' .OR. IND.EQ.'55') THEN
        PTR=PTR+6                         ! PAST 44/55 GROUP
        IF (N.NE.MISSING .AND. N.LE.3 .AND. LEV.EQ.LEVEL(L)) THEN
          DO 10 I=1,N
            P=PRESS(L)
            IF ((BLOCK .NE. MISSING) .AND. (STN .NE. MISSING)) THEN
              HT=STDPRHT(BLOCK,STN,L)
            ELSE
              HT=MISSING
            ENDIF

            IF ((PTR+4) .LE. LENG) THEN                             !1.3
              DD=IVALUE(OB(PTR:PTR+1))     !get direction
              FF=IVALUE(OB(PTR+2:PTR+4))   !get wind speed
            ELSE                                                    !1.3
              DD=MISSING                                            !1.3
              FF=MISSING                                            !1.3
            ENDIF                                                   !1.3

            IF (DD.NE.MISSING) THEN
              DD=DD*10
              IF (FF.GE.500) THEN
                FF=FF-500
                DD=DD+5
              ENDIF
              IF (DD.GT.360) THEN           !check direction valid
                DD=MISSING
              ELSE
              ENDIF
            ENDIF
            PTR=PTR+6                       ! PAST WIND GROUP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!set level type indicator                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ARRAY(BASE+(7*NUM_LEV))=32      !indicate sig level winds
            QCBIT_ARRAY(BASE+(7*NUM_LEV))=0.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with pressure and height                                        !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ARRAY(BASE+(7*NUM_LEV)+1)=P     !set pressure
            ARRAY(BASE+(7*NUM_LEV)+2)=HT !set height
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0.
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with temperature and dew point. NOTE in PILOT reports there is  !
!no temperature or dew point                                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ARRAY(BASE+(7*NUM_LEV)+3)=missing
            ARRAY(BASE+(7*NUM_LEV)+4)=missing
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0.
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with wind direction and wind speed                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            ARRAY(BASE+(7*NUM_LEV)+5)=DD
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.

            IF (FF .NE. MISSING) THEN
              IF (KNOTS) THEN
                ARRAY(BASE+(7*NUM_LEV)+6)=FF*KT2MS
              ELSE
                ARRAY(BASE+(7*NUM_LEV)+6)=FF
              ENDIF
            ELSE
              ARRAY(BASE+(7*NUM_LEV)+6)=MISSING
            ENDIF
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.
            QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!increment number of levels                                          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            IF ((PART.EQ.'A' .AND. L.GT.9) .OR.
     &         (PART .EQ. 'C' .AND. L.GT.14)) THEN
              GOTO 100
            ENDIF
            NUM_LEV=NUM_LEV+1
            L=L+1                         ! NEXT STANDARD LEVEL
   10     CONTINUE
          IF (PTR .LE. LENG) THEN                                  !1.3
            IF (LEN(OB(PTR:)).GT.10) GO TO 1
          ENDIF                                                    !1.3
        ENDIF
      ENDIF
*
  100 CONTINUE
      NUM_LEV=NUM_LEV-1
      RETURN
      END
