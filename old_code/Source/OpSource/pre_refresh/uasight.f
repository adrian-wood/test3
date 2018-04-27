      SUBROUTINE UASIGHT(STRING,PTR,ARRAY,NUM_LEV,PART,STNHT,KNOTS,
     &                   SIG_BASE,QCBIT_ARRAY,LENG)                   !A

!-----------------------------------------------------------------------
!
! PROGRAM       : UASIGHT  (Upper Air SIGnificant winds with HeighT)
!
! PURPOSE       : EXPAND SIGNIFICANT WINDS WITH HEIGHT AS COORDINATE
!                 IN PILOT B/D
!
! DATA TYPE(S)  : UPPER AIR PILOT PART B/D
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION), UASHEAR
!
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST STANDARD LEVEL
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (4) OUTPUT SUBSCRIPT (NOT INCREMENTED HERE)
!                 (5) PART (B OR D)
!                 (6) STATION HEIGHT
!                 (7) FLAG SET ON IF WINDS IN KNOTS
!                 (8) NUMBER OF WINDS IN REPORT
!                 (9) qc 1bit flag output
!                (10) length of report
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uasight.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:40    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:07:59  usmdb
! Checks added to prevent string out of bounds - S.Cox
!
! Revision 2.0  2001/07/03  10:44:34  10:44:34  usmdb (Generic MetDB account)
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  98/05/15  10:31:50  10:31:50  usmdb (Generic MetDB account)
! Length of report passed in for checking for out-of-bounds
! errors.                                                             !A
!
! Revision 1.2  97/07/31  11:45:36  11:45:36  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:42:12  uspm
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

      IMPLICIT NONE

!declare character
      CHARACTER STRING*(*)           !report string being expanded
      CHARACTER PART*1               !report type
      CHARACTER IND*1                !indicator for height from report
      CHARACTER HEAD*132             !revision information

!declare real
      REAL ARRAY(*)                  !expanded elements array
      REAL QCBIT_ARRAY(999)          !qc 1 bit flag array
      REAL KT2MS

!declare integer
      INTEGER PTR                    !pointer within report
      INTEGER STNHT                  !height of station
      INTEGER HT                     !height from report
      INTEGER DD                     !wind direction
      INTEGER FF                     !wind speed
      INTEGER STEP                   !indicates which multiple
      INTEGER NWINDS                 !displacement of current wind
      INTEGER IVALUE                 !function subprogram
      INTEGER I                      !used as loop counter
      INTEGER TENS                   !from report
      INTEGER UNITS(3)               !from report
      INTEGER MISSING                !indicates value is missing
      INTEGER NUM_LEV
      INTEGER SIG_BASE
      INTEGER LENG                   !Length of report               !A

!declare logical
      LOGICAL KNOTS                  !indicates wind speed in knots
      LOGICAL HEADSET                                               !2.1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the data consists of a height group followed by up to 3 winds, this!
! sequence being repeated as often as necessary.  a height group has !
! an indicator followed by a tens figure and up to 3 units figures.  !
! heights are multiples of 300m (indicator 9) or 500m (indicator 8). !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      SAVE

      DATA HEADSET/.FALSE./                                         !2.1

!initialise variables
      MISSING=-9999999
      NWINDS=0
      KT2MS=(1852./3600.)

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: uasight.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:40$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

    1 IND=STRING(PTR:PTR)
      IF (IND.EQ.'8' .OR. IND.EQ.'9') THEN     !check for indicator
        IF (IND.EQ.'8') THEN
          STEP=500
        ELSE
         STEP=300
        ENDIF
        TENS=IVALUE(STRING(PTR+1:PTR+1))

        DO I=1,3
          UNITS(I)=IVALUE(STRING(PTR+1+I:PTR+1+I))
        ENDDO

        IF ((UNITS(2).NE.MISSING .AND. UNITS(1).GT.UNITS(2)) .OR.
     &  (UNITS(3).NE.MISSING .AND. UNITS(2).GT.UNITS(3))) RETURN
        PTR=PTR+6

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! combine the tens & units figures to give multiples of 300/500m.     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        DO 20 I=1,3
         IF (PART.EQ.'B' .AND. I.EQ.1 .AND. NWINDS.EQ.0
     &   .AND. UNITS(1).EQ.MISSING) THEN
           HT=STNHT
         ELSE IF (TENS.NE.MISSING .AND. UNITS(I).NE.MISSING) THEN
           HT=(TENS*10+UNITS(I))*STEP
         ELSE
           GO TO 20
         ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with wind direction
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         IF ((PTR+1).LE.LENG) THEN                                  !2.1
           DD=IVALUE(STRING(PTR:PTR+1))  !get direction from report !2.1
         ELSE                                                       !2.1
           DD=MISSING                                               !2.1
         ENDIF                                                      !2.1
         IF ((PTR+4).LE.LENG) THEN                                  !2.1
           FF=IVALUE(STRING(PTR+2:PTR+4))  !get speed from report   !2.1
         ELSE                                                       !2.1
           FF=MISSING                                               !2.1
         ENDIF                                                      !2.1
         IF (DD.NE.MISSING) THEN
           DD=DD*10
           IF (FF.GE.500) THEN             !check to see if direction
             FF=FF-500                     !end in a 5. Correct speed
             DD=DD+5                       !correct direction
           ENDIF
           IF (DD.GT.360) THEN             !check for valid direction
             DD=MISSING                    !set missing if invalid
             ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
             QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=1.
           ELSE
             ARRAY(SIG_BASE+(7*NUM_LEV)+5)=DD
             QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+5)=0.
           ENDIF
         ENDIF

         PTR=PTR+6                         !move pointer past group

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!deal with wind speed
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

         IF (FF .NE. MISSING) THEN
           IF (KNOTS) THEN                 !wind speed in knots
             ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF*KT2MS
           ELSE                            !wind speed in ms
-1
             ARRAY(SIG_BASE+(7*NUM_LEV)+6)=FF
           ENDIF
         ELSE                              !wind speed missing
           ARRAY(SIG_BASE+(7*NUM_LEV)+6)=MISSING
         ENDIF

         ARRAY(SIG_BASE+(7*NUM_LEV))=2               !SET LEVEL TYPE
         ARRAY(SIG_BASE+(7*NUM_LEV)+2)=HT            !set height
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV))=0.        !LEVEL ID OKAY
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+1)=0.      !PRESS OKAY
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+2)=0.      !HT OKAY
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+3)=0.      !TEMP OKAY
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+4)=0.      !DEW POINT OKAY
         QCBIT_ARRAY(SIG_BASE+(7*NUM_LEV)+6)=0.      !WIND SPEED OKAY

         NUM_LEV=NUM_LEV+1
   20    CONTINUE
         IF (PTR .LT. LENG) THEN                                      !A
           GO TO 1
         ENDIF                                                        !A
       ENDIF

      RETURN
      END
