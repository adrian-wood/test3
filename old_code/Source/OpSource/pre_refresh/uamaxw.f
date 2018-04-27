      SUBROUTINE UAMAXW(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,
     &                  QCBIT_ARRAY,LENG)                             !B

!-----------------------------------------------------------------------
!
! PROGRAM       : UAMAXW
!
! PURPOSE       : EXPAND MAX WIND DATA IN TEMP OR PILOT PART A/C
!                 This version only stores one wind shear but all
!                 max winds
!
! DATA TYPE(S)  : UPPER AIR TEMP PART A & PILOT PART A/C
!
! CALLED BY     : UAXPAND (separate calls for TEMP & PILOT)         !1.8
!
! CALLS         : IVALUE (FUNCTION)
!
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                 (2) INPUT POINTER TO START OF FIRST MAX WIND
!                      (POINTS PAST END OF SECTION ON RETURN)
!                 (3) OUTPUT ARRAY, WITH STANDARD LEVEL DATA IN IT
!                 (4) number of levels already in array    (i/o)    !1.8
!                 (5) PART (A OR C)
!                 (6) FLAG SET ON IF WINDS IN KNOTS
!                 (7) qc 1 bit flags output
!                 (8) length of report                              !1.8
!
! REVISION INFO :
!
! $Workfile: uamaxw.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 18/05/2010 14:00:14$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         18/05/2010 14:00:14    Brian Barwell
!       Convert wind shears to m/s if reported in knots.
!  1    Met_DB_Project 1.0         30/01/2006 20:25:39    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:07:39  usmdb
! Checks added to prevent string out of bounds - S.Cox
!
! Revision 2.0  2001/07/03  10:44:33  10:44:33  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.8  99/09/09  10:13:29  10:13:29  usmdb (Generic MetDB account)
! 20 Sept 99     C Long
! Increment NUM_LEV here, not in UAXPAND. This requires rearrangement
! of code, so the program will be tidied up. (This goes with a change
! to UAXPAND)
!
! Revision 1.7  99/04/12  10:47:44  10:47:44  usmdb (Generic MDB account)
! 19-04-1999 S.Cox - ref MetDB problems 438,440
! To prevent out of bounds error
!
! Revision 1.6  99/03/11  14:44:10  14:44:10  usmdb (Generic MDB account)
! dummy revision due to problem with checkin
!
! Revision 1.5  99/03/11  13:27:04  13:27:04  usmdb (Generic MDB account)
!
! Revision 1.4  98/09/16  16:11:10  16:11:10  usmdb (Generic MDB account)
! 21/09/1998 Additional 'out-of-bounds' checking. Jon Lewthwaite
!
! Revision 1.3  98/05/15  10:31:15  10:31:15  usmdb (Generic MDB account)
! Length of report passed in to be used for checking for out-of-bounds
! errors.                                                             !B
!
! Revision 1.2  97/07/31  11:45:17  11:45:17  uspm (Pat McCormack)
! First revision for 1
!
! Revision 1.1  1997/07/04 14:39:46  uspm
! Initial revision
!
! 17/03/97 CORRECT MAX WIND HEIGHT VALUE CALCULATION                  !A
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

      IMPLICIT NONE

!declare character
      CHARACTER OB*(*)                 !report being decoded
      CHARACTER PART*1                 !report type
      CHARACTER HEAD*80                !revision information         !2

!declare real
      REAL ARRAY(*)                    !array of decoded values
      REAL QCBIT_ARRAY(*)              !array of 1-bit qc flags
      REAL KT2MS                       !knots-to-m/s conversion factor

!declare integer
      INTEGER PTR                      !pointer within report
      INTEGER P                        !pressure
      INTEGER HT                       !height
      INTEGER DD                       !wind direction
      INTEGER FF                       !wind speed
      INTEGER VB                       !wind shear above and
      INTEGER VA                       !below max wind
      INTEGER MISSING                  !indicates missing
      INTEGER NUM_LEV                  !number of levels
      INTEGER BASE                     ! magic number for subscripts!
      INTEGER IVALUE
      INTEGER LENG                     !Length of report             !B

!declare logical
      LOGICAL KNOTS                    ! set if speed in knots
      LOGICAL GOT_SHEAR                ! set if wind shear found
      LOGICAL HEADSET                                               !2.1

      SAVE

      DATA HEADSET/.FALSE./                                         !2.1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! More than one max wind can be handled, including 77... followed by  !
! 66... (but UAXPAND looks for 77... first, so they must be in order).!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!Initialize variables

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD = '$Workfile: uamaxw.f$ ' //
     &         '$Revision: 2$ $Date: 18/05/2010 14:00:14$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

! Why is base set to 8?  There are 14 elements before the replication
! over levels.  NUM_LEV is incremented at the start of the level.  So
! the first element for the first level, the 15th in the array, has
! subscript 14+7*(NUM_LEV-1)+1 = 8+7*NUM_LEV.

      BASE=8
      GOT_SHEAR=.FALSE.                !reset while looking for more
      KT2MS=(1852./3600.)
      MISSING=-9999999

      VB=MISSING
      VA=MISSING

  1   CONTINUE                                                      !2.1

      P=MISSING
      HT=MISSING
      DD=MISSING
      FF=MISSING

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! TEMPs & some PILOTs have 77 or 66 followed by a pressure in millibars
! (tenths if part C),                                                 !
! other pilots a single 6 or 7 followed by a height in tens of metres.!
! There are 2 or 3 groups per max wind, the shear group being optional!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (OB(PTR:PTR+1).EQ.'77' .OR. OB(PTR:PTR+1).EQ.'66') THEN
        IF ((PTR+4).LE.LENG) P=IVALUE(OB(PTR+2:PTR+4))              !2.1
        IF (P.NE.MISSING .AND. PART.EQ.'A') P=P*100
        IF (P.NE.MISSING .AND. PART.EQ.'C') P=P*10
      ELSE IF (OB(PTR:PTR).EQ.'7' .OR. OB(PTR:PTR).EQ.'6') THEN
        IF ((PTR+4).LE.LENG) HT=IVALUE(OB(PTR+1:PTR+4))             !2.1
        IF (HT.NE.MISSING) HT=HT*10
      ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Wind  (a max wind must be above 500mb)                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ((P.NE.MISSING .AND. P.LE.50000) .OR.
     &    (P.EQ.MISSING .AND. HT.NE.MISSING)) THEN                  !1.8
        IF ((PTR+7).LE.LENG) DD=IVALUE(OB(PTR+6:PTR+7))             !2.1
        IF ((PTR+10).LE.LENG) FF=IVALUE(OB(PTR+8:PTR+10))           !2.1
        IF (DD.NE.MISSING .AND.DD.LE.36 .AND. FF.GT.30) THEN        !1.8
          NUM_LEV=NUM_LEV+1            ! one more level
          DD=DD*10
          IF (FF.GE.500) THEN          !see if direction ends in 5
            FF=FF-500                  !correct the speed
            DD=DD+5                    !correct wind direction
          ENDIF
          PTR=PTR+12                   !move past groups

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Wind shears in kilometre above & below  (optional group 4VbVa)      !
! Only the first wind shear group is used. GOT_SHEAR is set to        !
! TRUE when the first wind shear is decoded. After that UAMAXW will   !
! check for a wind shear group and then skip over it.                 !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (PTR.LT.LENG) THEN                                    !1.8
            IF (OB(PTR:PTR).EQ.'4') THEN
              IF (.NOT.GOT_SHEAR) THEN
                IF ((PTR+2).LE.LENG) VB=IVALUE(OB(PTR+1:PTR+2))     !2.1
                IF ((PTR+4).LE.LENG) VA=IVALUE(OB(PTR+3:PTR+4))     !2.1
                GOT_SHEAR=.TRUE.
              ENDIF
              PTR=PTR+6
            ENDIF
          ENDIF                                                      !B

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Set level type, pressure and height in output array.             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ARRAY(BASE+(7*NUM_LEV))=8
          ARRAY(BASE+(7*NUM_LEV+1))=P                                !A
          ARRAY(BASE+(7*NUM_LEV+2))=HT                               !A

          QCBIT_ARRAY(BASE+(7*NUM_LEV))=0.
          QCBIT_ARRAY(BASE+(7*NUM_LEV)+1)=0.
          QCBIT_ARRAY(BASE+(7*NUM_LEV)+2)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Air temperature and dewpoint are not reported in this section,
! so missing.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ARRAY(BASE+(7*NUM_LEV)+3)=MISSING
          ARRAY(BASE+(7*NUM_LEV)+4)=MISSING

          QCBIT_ARRAY(BASE+(7*NUM_LEV)+3)=0.
          QCBIT_ARRAY(BASE+(7*NUM_LEV)+4)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Wind direction and speed. Wind shear is kept for the first max      !
!wind group found, but only put in the encoding array (outside       !
!the replication) when all the max winds have been decoded.          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          ARRAY(BASE+(7*NUM_LEV)+5)=DD

          IF (KNOTS) THEN
            ARRAY(BASE+(7*NUM_LEV)+6)=FF*KT2MS
          ELSE
            ARRAY(BASE+(7*NUM_LEV)+6)=FF
          ENDIF

          QCBIT_ARRAY(BASE+(7*NUM_LEV)+5)=0.
          QCBIT_ARRAY(BASE+(7*NUM_LEV)+6)=0.

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Check if more max winds. If so go back and get next level. If not then!
!set the wind shear group found with the first max wind.               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

          IF (PTR.LT.LENG) THEN                                    !1.8
            IF (OB(PTR:PTR).EQ.'7' .OR. OB(PTR:PTR).EQ.'6') THEN
              IF (OB(PTR+2:PTR+3).NE.'999') GO TO 1
            ENDIF
          ENDIF                                                      !B

          IF (KNOTS) THEN                                            !2
            IF (VB.GT.0) ARRAY(BASE+(7*NUM_LEV)+7) = KT2MS*FLOAT(VB) !2
            IF (VA.GT.0) ARRAY(BASE+(7*NUM_LEV)+8) = KT2MS*FLOAT(VA) !2
          ELSE                                                       !2
            ARRAY(BASE+(7*NUM_LEV)+7)=FLOAT(VB)
            ARRAY(BASE+(7*NUM_LEV)+8)=FLOAT(VA)
          END IF                                                     !2
        ENDIF
      ENDIF

      RETURN
      END
