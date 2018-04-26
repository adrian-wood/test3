      SUBROUTINE BTHSC1(REPORT,REPLEN,EXPARR,POS,DATIME,MIMJ,IERR)

!-----------------------------------------------------------------------
!
! PROGRAM      : BTHSC1
!
! PURPOSE      : TO EXPAND SECTION 1 OF BATHY (TIME,
!                POSITION, WIND & SURFACE TEMPERATURE)
!
! CALLED BY    : BTHEXP
!
! CALLS        : IVALUE   CONVERTS FROM CHAR TO NUMERIC TYPE
!
! PARAMETERS   : REPORT   CHARACTER STRING OF REPORT (I)
!                REPLEN   LENGTH OF REPORT  (I)
!                EXPARR   EXPANSION ARRAY   (O)
!                POS      SECTION LOCATING VARIABLE (I/O)
!                DATIME   DATE/TIME ARRAY (O)
!                MIMJ     REPORT IDENTIFIER  (I)
!                IERR     REPORT STATUS (DUFF REPORT = 16)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:07$
! $Source: /data/us0400/mdb/op/lib/source/RCS/bthsc1.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:07    Sheila Needham  
! $
! Revision 2.1  2003/08/05  11:09:15  11:09:15  usmdb (MetDB account c/o usjh)
! 18 Aug 2003     C Long
! 2.1  Deal with sign of temperature before adding 273.1!
! 
! Revision 2.0  2001/05/31  13:27:34  13:27:34  usmdb (MetDB account c/o usjh)
! Separated variable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.7  2000/05/05  14:55:18  14:55:18  usmdb (Generic MetDB account)
! 8 May 2000     C Long
! 1.7 Check for slash(es) at end of lat/long groups
!
! Revision 1.6  99/09/09  09:57:14  09:57:14  usmdb (Generic MDB account)
! 20 Sept 99  C Long
! 1.6 Change C/K conversion from 273.2 to 273.1 for air temperature
!
! Revision 1.5  99/04/12  10:59:00  10:59:00  usmdb (Generic MDB account)
! 19 April 1999      C Long
! 1.5 Allow for 6-figure lat/long groups (thousandths of degrees)
!
! Revision 1.4  98/09/16  16:11:31  16:11:31  usmdb (Generic MDB account
!
! Revision 1.3  1997/09/10 16:06:20  uspm
! Correct expansion of single-figure year to 4 figs.                  !B
!
! Revision 1.2  1997/07/31 09:13:39  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/04 11:08:12  uspm
! Initial revision
!
! JAN 96 - WIND DIRECTION IS IN TENS, NOT WHOLE DEGREES!              !A
!
! INTRODUCED : 11/07/94
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

      CHARACTER*4096 REPORT
      CHARACTER*4    MIMJ
      CHARACTER*132  HEAD
      REAL           EXPARR(0:300)
      REAL           MISING                                         !2.0
      REAL           KT2MPS
      REAL           YEAR,MONTH,DAY
      REAL           HOUR,MIN
      REAL           LAT,LONG
      REAL           TEMP
      REAL           WDIR,WSPEED
      INTEGER        IDEG   ! copy of degrees for range checks     c
      INTEGER        IMIN   ! copy of minutes for range checks     c
      INTEGER        ITHOU  ! thousandths of lat or long            !1.5
      INTEGER        IYEAR,IMONTH,IDAY                              !1.4
      INTEGER        REPLEN
      INTEGER        POS
      INTEGER        IERR
      INTEGER        IVALUE
      INTEGER        SIGN
      INTEGER        IU     ! INDICATOR FOR UNITS IN WHICH
                            ! WINDSPEED HAS BEEN REPORTED
      INTEGER        NOW(8) ! FOR STORING SYSTEM DATE AND TIME
      INTEGER        SYSYR  ! SYSTEM YEAR = ELEMENT NOW(8)
      INTEGER        QUAD   ! QUADRANT OF GLOBE
      INTEGER        DATIME(5)
      LOGICAL        KNOTS  ! FLAG FOR WINDSPEED UNITS
      LOGICAL        VALDDY ! TRUE IF DATE VALUES CORRECT           !1.4

      DATA           MISING/-9999999./                              !2.0

*************************************************************
*
*     ON ENTRY, POS IS INDICATING THE FIRST CHARACTER
*     AFTER MIMJ I.E. BEGINNING OF SECTION 1
*
*     SET INITIAL VALUES
*     KT2MPS - KNOTS TO M/S CONVERSION FACTOR
*     WDIR - WIND DIRECTION
*     WSPEED - WIND SPEED
*
*************************************************************

      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/bthsc1.f,v $
     &'//'$ $Date: 30/01/2006 20:21:07$ $Revision: 1$'

      KT2MPS = 0.5148
      YEAR = MISING
      MONTH = MISING
      DAY = MISING
      HOUR = MISING
      MIN = MISING
      LAT = MISING
      LONG = MISING
      WDIR = MISING
      WSPEED = MISING
      TEMP = MISING
      IERR=0
      KNOTS = .FALSE.

*************************************************************
*
*     RESET POSITION BACK TO THE BEGINNING OF REPORT
*     THEN MOVE PAST MIMJ
*
*************************************************************

      POS = 1
      IF (REPORT(POS:POS+3).EQ.MIMJ) THEN
        POS = POS + 5
        IF (REPORT(POS:POS).EQ.' ') POS=POS+1
      ENDIF

*************************************************************
*
*     START STORING ELEMENTS INTO ARRAY
*     ALSO CHECKING VALIDITY OF THE DATE AND LOCATION DATA
*
*************************************************************

      DAY = IVALUE(REPORT(POS:POS+1))


      MONTH = IVALUE(REPORT(POS+2:POS+3))


*     ONLY UNIT OF YEAR IS REPORTED
*     USE SYSTEM YEAR TO EXPAND AND VALIDATE YEAR IN REPORT
*     CONSIDERING ONLY DATA A YEAR OLD. ANY OTHER YEAR WILL
*     RESULT IN THE REPORT BEING REJECTED.

      YEAR = IVALUE(REPORT(POS+4:POS+4))

      CALL DATIM(NOW)
      SYSYR = MOD(NOW(8),10)
      IF (SYSYR.EQ.INT(YEAR)) THEN
        YEAR = NOW(8)

!     Convert single-figure year to 4-figure
      ELSE IF ((SYSYR-1).EQ.INT(YEAR)             .OR.
     &         (SYSYR.EQ.0 .AND. INT(YEAR).EQ.9)) THEN
        YEAR = NOW(8) - 1

      ENDIF

c     Vailidate year, month and day using VALDDY                    !1.4
c       function VALDDY (day,month,year)                            !1.4

        IYEAR=INT(YEAR)                                             !1.4
        IMONTH=INT(MONTH)                                           !1.4
        IDAY=INT(DAY)                                               !1.4

      IF(.NOT.(VALDDY(IDAY,IMONTH,IYEAR)))THEN                      !1.4
        PRINT*, ' BTHSC1 INVALID DATE >',REPORT(1:REPLEN),'<'       !1.4
     &  ,'DAY = ',DAY,' MONTH = ',MONTH,' YEAR = ',YEAR             !1.4
        IERR = 16                                                   !1.4
        GOTO 999                                                    !1.4
      ENDIF                                                         !1.4

*************************************************************
*
*     TIME GROUP GGGG/ - STORE AND VALIDATE
*
*************************************************************

      POS = POS + 6

      HOUR = IVALUE(REPORT(POS:POS+1))
      IF (HOUR.LT.0 .OR. HOUR.GT.23) THEN
        PRINT*,' BTHSC1 INVALID HOUR >',REPORT(1:REPLEN),'<'
        IERR = 16
        GOTO 999
      ENDIF

      MIN = IVALUE(REPORT(POS+2:POS+3))
      IF (MIN.LT.0 .OR. MIN.GT.59) THEN
        PRINT*,' BTHSC1 INVALID MINUTE >',REPORT(1:REPLEN),'<'
        IERR = 16
        GOTO 999
      ENDIF

      POS = POS + 6

*************************************************************
*
*     LATITUDE GROUP - QCLALALALA(La)                              !1.5
*
*     QUAD= QUADRANT OF GLOBE - USE TO MAKE NECESSARY MODS
*           TO DATA
*
*     CONVERT FROM DEGREES AND MINUTES TO DEGREES with fraction    !1.5
*      (thousandths if MiMiMjMj is JJVV)                           !1.5
* (But 6-figure KKYY groups can end with one or two slashes,       !1.7
*  meaning hundredths (not minutes!) or tenths.)                   !1.7
*
*************************************************************

      QUAD = IVALUE(REPORT(POS:POS))
      IF(.NOT.(QUAD.EQ.1.OR.QUAD.EQ.3.OR.QUAD.EQ.5.OR.QUAD.EQ.7))THEN
        PRINT*, ' BTHSC1 INVALID QUAD >',REPORT(1:REPLEN),'<'      !1.4
        IERR=16                                                    !1.4
        GOTO 999                                                   !1.4
      ENDIF                                                        !1.4

      IDEG = IVALUE(REPORT(POS+1:POS+2))                           !1.4
      IF (MIMJ.NE.'JJVV') THEN
        IMIN = IVALUE(REPORT(POS+3:POS+4))                         !1.4
        IF(IDEG.GE.0.AND.IDEG.LE.90.AND.IMIN.GE.0.AND.IMIN.LT.60)THEN
          LAT=REAL(IDEG)+REAL(IMIN)*0.016667                       !1.5
          IF (QUAD.EQ.3.OR.QUAD.EQ.5) LAT = -LAT
        ELSE                                                       !1.4
          PRINT*, ' BTHSC1: INVALID LAT >',REPORT(1:REPLEN),'<'    !1.4
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+6                                                  !1.5
      ELSE
        IF (REPORT(POS+4:POS+5).EQ.'//') THEN                      !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+3))*100                    !1.7
        ELSE IF (REPORT(POS+5:POS+5).EQ.'/') THEN                  !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+4))*10                     !1.7
        ELSE                                                       !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+5))                        !1.5
        ENDIF                                                      !1.7
        IF (IDEG.GE.0 .AND. IDEG.LE.90 .AND. ITHOU.GE.0) THEN      !1.5
          LAT=REAL(IDEG)+REAL(ITHOU)*0.001                         !1.5
          IF (QUAD.EQ.3.OR.QUAD.EQ.5) LAT=-LAT                     !1.5
        ELSE                                                       !1.5
          PRINT*, ' BTHSC1: INVALID LAT >',REPORT(1:REPLEN),'<'    !1.5
          IERR=16                                                  !1.5
          GOTO 999                                                 !1.5
        ENDIF                                                      !1.5
        POS=POS+7                                                  !1.5
      ENDIF                                                        !1.5

*************************************************************
*
*     NEXT GROUP = LONGITUDE GROUP  LOLOLOLOLO(Lo)                 !1.5
*
*************************************************************

      IDEG = IVALUE(REPORT(POS:POS+2))                             !1.4
      IF (MIMJ.NE.'JJVV') THEN
        IMIN = IVALUE(REPORT(POS+3:POS+4))                         !1.4
        IF(IDEG.GE.0.AND.IDEG.LE.180.AND.IMIN.GE.0.AND.IMIN.LT.60)THEN
          LONG=REAL(IDEG)+REAL(IMIN)*0.016667                      !1.5
          IF (QUAD.EQ.5.OR.QUAD.EQ.7) LONG=-LONG
        ELSE                                                       !1.4
          PRINT*, ' BTHSC1: INVALID LONG >',REPORT(1:REPLEN),'<'   !1.4
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+6                                                  !1.5
      ELSE
        IF (REPORT(POS+4:POS+5).EQ.'//') THEN                      !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+3))*100                    !1.7
        ELSE IF (REPORT(POS+5:POS+5).EQ.'/') THEN                  !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+4))*10                     !1.7
        ELSE                                                       !1.7
          ITHOU=IVALUE(REPORT(POS+3:POS+5))                        !1.5
        ENDIF                                                      !1.7
        IF (IDEG.GE.0 .AND. IDEG.LE.180 .AND. ITHOU.GE.0) THEN     !1.5
          LONG=REAL(IDEG)+REAL(ITHOU)*0.001                        !1.5
          IF (QUAD.EQ.5.OR.QUAD.EQ.7) LONG=-LONG                   !1.5
        ELSE                                                       !1.5
          PRINT*, ' BTHSC1: INVALID LONG >',REPORT(1:REPLEN),'<'   !1.5
          IERR=16                                                  !1.5
          GOTO 999                                                 !1.5
        ENDIF                                                      !1.5
        POS=POS+7                                                  !1.5
      ENDIF                                                        !1.5
*************************************************************
*
*     NEXT GROUPS ARE OPTIONAL = WIND GROUP AND TEMP GROUP
*
*     TEST FOR WIND GROUP -(IUDDFF) - IU TAKES VALUES 0 TO 3.
*     DIRECTION IS IN TENS OF DEGREES, SPEED IN WHOLE M/S OR KNOTS.
*     CONVERT FROM KNOTS TO M/S IF REQUIRED.
*
*************************************************************

      IU = IVALUE(REPORT(POS:POS))
      IF (IU.GE.0.AND.IU.LE.3) THEN
        IF (IU.EQ.1.OR.IU.EQ.3) KNOTS = .TRUE.
*       WIND DIRECTION
        WDIR = IVALUE(REPORT(POS+1:POS+2))
        IF (WDIR.NE.MISING) WDIR=WDIR*10.                            !A
*       WIND SPEED
        WSPEED = IVALUE(REPORT(POS+3:POS+4))
        IF ((KNOTS).AND.(WSPEED.NE.MISING)) WSPEED = WSPEED * KT2MPS
*       MOVE TO NEXT GROUP
        POS = POS + 6
      ENDIF

*************************************************************
*
*     TEST FOR TEMPERATURE GROUP - 4SNTTT
*
*************************************************************

      IF (REPORT(POS:POS).EQ.'4') THEN
        SIGN = IVALUE(REPORT(POS+1:POS+1))
        TEMP = IVALUE(REPORT(POS+2:POS+4))
        IF(TEMP.NE.MISING) THEN
          IF (SIGN.EQ.1) TEMP = -TEMP                              !2.1
          TEMP = (TEMP * 0.1) + 273.1  ! air temp in tenths        !2.1
        ENDIF
*       MOVE TO NEXT GROUP - THIS POSITION WILL INDICATE THE
*       START OF SECTION 2 WITHIN THE REPORT
        POS = POS + 6
      ENDIF

*************************************************************
*
*     ASSIGN VARIABLES TO VALUES AND DATIME ARRAY
*
*************************************************************

999   EXPARR(6) = YEAR
      EXPARR(8) = MONTH
      EXPARR(10) = DAY
      EXPARR(12) = HOUR
      EXPARR(14) = MIN
      EXPARR(16) = LAT
      EXPARR(18) = LONG
      EXPARR(20) = WDIR
      EXPARR(22) = WSPEED
      EXPARR(24) = TEMP

      DATIME(1) = YEAR
      DATIME(2) = MONTH
      DATIME(3) = DAY
      DATIME(4) = HOUR
c     DATIME(5) = MINUTE      ! old line
      DATIME(5) = MIN         ! hh+/-10mins=0 ?????

      RETURN
      END
