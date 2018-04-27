      SUBROUTINE UASONDE(OB,ARRAY,QCBIT_ARRAY)                      !1.7

!-----------------------------------------------------------------------
!
! PROGRAM       : UASONDE
!
! PURPOSE       : TO HANDLE 31313 SECTION IN TEMP (SONDE            !1.6
!                 DETAILS & LAUNCH TIME; WATER TEMPERATURE IF SHIP)
!
! DATA TYPE(S)  : UPPER AIR TEMP                                    !1.6
!
! CALLED BY     : UAXPAND
!
! CALLS         : IVALUE (FUNCTION), DATE31,DATE13                  !1.5
!
! PARAMETERS    : (1) INPUT REPORT (5-FIG GROUPS) STARTING AT 31313
!                 (2) OUTPUT ARRAY, WITH HEADER DATA ALREADY IN IT
!                 (3) Q/C FLAG ARRAY                       (output) !1.7
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uasonde.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:42    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:08:40  usmdb
! Checks added to prevent string out of bounds - S.Cox
!
! Revision 2.0  2001/07/03  10:44:35  10:44:35  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.7  2000/09/06  10:55:51  10:55:51  usmdb (Generic MetDB account)
! 18 Sept 2000   C Long
! Check against substring length, not length of whole report.
!
! Revision 1.6  2000/08/09  14:55:24  14:55:24  usmdb (Generic MDB account)
! 17 July 2000     C Long
! Accept launch time for any part, before or after nominal hour
!
! Revision 1.5  99/10/06  11:46:56  11:46:56  usmdb (Generic MDB account)
! 18 Oct 99       C Long
! For dropsondes reset time to launch time if within an hour of
! nominal hour (before or after), resetting date if necessary.
!
! Revision 1.4  99/09/09  09:59:50  09:59:50  usmdb (Generic MDB account)
! 20 Sept 99  C Long
! Change C/K conversion for water temperature from ship from 273.15
! to 273.1 (tenths)
!
! Revision 1.3  98/09/16  16:11:08  16:11:08  usmdb (Generic MDB account)
! 21/09/1998 Additional checks for 'out-of-bound' conditions.
! v(G)=8 ev(G)=1 Jon Lewthwaite                                       !A
!
! Revision 1.2  97/07/31  11:45:56  11:45:56  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:44:43  uspm
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
      CHARACTER OB*(*)                    !report being expanded
      CHARACTER HEAD*132                  !revision information

!declare integer
      INTEGER MISSING                     !indicates missing data
      INTEGER RADCOR                      !radiation correction
      INTEGER SONDE                       !type of sonde
      INTEGER TRACK                       !type of tracking
      INTEGER HOUR                        !hour of report
      INTEGER MINUTE                      !minutes of report
      INTEGER TW                          !water temperature
      INTEGER IVALUE
      INTEGER IHM
      INTEGER LENG                        !length of OB             !2.1
      INTEGER NOMHOUR
      INTEGER DAY                                                   !1.5
      INTEGER MONTH                                                 !1.5
      INTEGER YEAR                                                  !1.5
      INTEGER CENDAY                                                !1.5

!declare logical
      LOGICAL HEADSET                                               !2.1

!declare real
      REAL ARRAY(*)
      REAL QCBIT_ARRAY(999)               !array of 1bit qc flags
      REAL C2KELVIN                       !degree c to kelvin

      SAVE

      DATA HEADSET/.FALSE./                                         !2.1

!initialize variables
      C2KELVIN=273.1      ! water temp from ship is in tenths       !1.4
      MISSING=-9999999
      LENG=LEN(OB)                                                  !2.1

      IF (.NOT.HEADSET) THEN                                        !2.1
        HEAD='$RCSfile: uasonde.F,v $ ' //
     &       '$Revision: 1$ $Date: 30/01/2006 20:25:42$'
        HEADSET=.TRUE.                                              !2.1
      ENDIF                                                         !2.1

!----------------------------------------------------------------------
! The data consists of an 8-group (time of launch) perhaps preceded by
! a group starting with a radiation correction from 0 to 7, and
! perhaps followed by a 9-group (water temperature).
!----------------------------------------------------------------------

      ARRAY(14)=MISSING                                             !2.1
      ARRAY(15)=MISSING                                             !2.1
      ARRAY(16)=MISSING                                             !2.1
      IHM=0                                                         !2.1

      IF (LENG.GE.13) THEN                                          !2.1
        IF (OB(7:7).LE.'7' .AND. OB(13:13).EQ.'8') THEN
          RADCOR=IVALUE(OB(7:7))             ! RADIATION CORRECTION
          SONDE=IVALUE(OB(8:9))              ! TYPE OF RADIOSONDE
          TRACK=IVALUE(OB(10:11))            ! TRACKING SYSTEM
          ARRAY(14)=SONDE
          ARRAY(15)=TRACK
          ARRAY(16)=RADCOR
          IHM=13                             ! POINTER TO LAUNCH TIME
        ENDIF                                                       !2.1
      ENDIF                                                         !2.1

      IF (LENG.GE.7) THEN                                           !2.1
        IF (OB(7:7).EQ.'8') THEN
          IHM=7                            ! LAUNCH TIME IN 1ST GROUP
        ENDIF
      ENDIF

      QCBIT_ARRAY(14)=0                    !set all qc bits to
      QCBIT_ARRAY(15)=0                    !okay
      QCBIT_ARRAY(16)=0

      IF (IHM.GT.0) THEN
        IF ((IHM+2).LE.LENG) THEN                                   !2.1
          HOUR=IVALUE(OB(IHM+1:IHM+2))     ! HOUR OF LAUNCH         !2.1
        ELSE                                                        !2.1
          HOUR=MISSING                                              !2.1
        ENDIF                                                       !2.1
        IF ((IHM+4).LE.LENG) THEN                                   !2.1
          MINUTE=IVALUE(OB(IHM+3:IHM+4))   ! MINUTE OF LAUNCH       !2.1
        ELSE                                                        !2.1
          MINUTE=MISSING                                            !2.1
        ENDIF                                                       !2.1

!-----------------------------------------------------------------------
! If the launch hour is the same as the nominal hour or the next    !1.6
! hour, set the minutes.  Launch time is mandatory in all parts     !1.6
! from May 2000.                                                    !1.6
!-----------------------------------------------------------------------

        NOMHOUR=ARRAY(12)                  ! HOUR FROM START OF REPORT
        IF (MINUTE.GE.0 .AND. MINUTE.LT.60) THEN                    !1.6
          IF (HOUR.EQ.NOMHOUR .OR. HOUR.EQ.NOMHOUR-1                !1.5
     &        .OR. (HOUR.EQ.23.AND.NOMHOUR.EQ.0)) THEN              !1.5
            ARRAY(12)=HOUR                 ! reset hour             !1.5
            ARRAY(13)=MINUTE               ! set minute in array    !1.5
            IF (HOUR.EQ.23.AND.NOMHOUR.EQ.0) THEN                   !1.5
              YEAR=ARRAY(9)                                         !1.5
              MONTH=ARRAY(10)                                       !1.5
              DAY=ARRAY(11)                                         !1.5
              CALL DATE31(DAY,MONTH,YEAR,CENDAY)                    !1.5
              CALL DATE13(CENDAY-1,DAY,MONTH,YEAR)                  !1.5
              ARRAY(9)=YEAR                                         !1.5
              ARRAY(10)=MONTH                                       !1.5
              ARRAY(11)=DAY                                         !1.5
            ENDIF                                                   !1.5
          ENDIF                                                     !1.5
        ENDIF                                                       !1.5
        QCBIT_ARRAY(12)=0                  !set qc bit for hour = okay
        QCBIT_ARRAY(13)=0                  !set qc bit for minute =okay

!----------------------------------------------------------------------
! Only ships have water temperature group, so see if call sign set.
!----------------------------------------------------------------------

        IF (IHM+10.LT.LENG) THEN                                !2.1!1.7
          IF (ARRAY(3).NE.MISSING .AND. OB(IHM+6:IHM+6).EQ.'9') THEN
            TW=IVALUE(OB(IHM+8:IHM+10))        ! WATER TEMPERATURE
            IF (OB(IHM+7:IHM+7).EQ.'1') TW=-TW ! PRECEDED BY SIGN
            ARRAY(17)=C2KELVIN+TW*(1./10.)     !convert to kelvin
          ELSE
            ARRAY(17)=MISSING
          ENDIF
        ELSE                                                          !A
          ARRAY(17)=MISSING                                           !A
        ENDIF
        QCBIT_ARRAY(17)=0                    !set qc bit for water temp.
      ENDIF

      RETURN                                 !return to uaxpand
      END
