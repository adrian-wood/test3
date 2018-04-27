      SUBROUTINE TESSC1(REPORT,POS,EXPARR,DATIME,IERR)             !2.1

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC1
!
! PURPOSE       : To expand section 1 of TESAC (time,
!                 position, wind and surface temperature)
!
! CALLED BY     : TESEXP
!
! CALLS         : IVALUE   converts figures to numbers
!                 VALDDY   function to check date                   !1.3
!
! PARAMETERS    : REPORT   character string starting with MiMiMjMj  (I)
!                 POS      pointer to report                        (O)
!                 EXPARR   expansion array                          (O)
!                 DATIME   data/time array                          (O)
!                 IERR     =16 if duff report                       (O)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:19$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tessc1.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:19    Sheila Needham  
! $
! Revision 2.1  2001/11/06 10:16:00  usmdb
! 19 Nov 2001     C Long
! 2.1  Strip down argument list.  Change code at start
!      & prints for consistency with reduced argument list.
!      Tidy up comments.
!
! Revision 2.0  2001/07/03  10:44:16  10:44:16  usmdb (Generic MetDB account)
! Separated varaiable declaration and initialisation. Added
! copyright and modified header - S.Cox
!
! Revision 1.6  2000/05/05  14:55:41  14:55:41  usmdb (Generic MetDB acc
! 8 May 2000    C Long
! 1.6  Check for slash(es) at end of lat/long groups
!
! Revision 1.5  99/09/09  09:59:10  09:59:10  usmdb (Generic MDB account
! 20 Sept 99  C Long
! Change C/K conversion for air temperature (in tenths) from 273.2 to
! 273.1 (leaving 273.15 in TESSC2 for sea temperature in hundredths)
!
! Revision 1.4  99/04/12  10:59:21  10:59:21  usmdb (Generic MDB account
! 19 April 1999    C Long
! Allow for 6-figure lat/long groups (thousandths of degrees)
!
! Revision 1.3  98/09/16  16:11:33  16:11:33  usmdb (Generic MDB account
! no entry
!
! Revision 1.2  1997/07/31 11:42:59  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 14:28:30  uspm
! Initial revision
!
! Aug 97 - Amend for Year 2000 - adjust single-figure
!          year test for decade end - Jim Arnott                      !C
!
! JAN 97 - DON'T IGNORE HUNDREDS FIGURE OF LONGITUDE!                 !B
!
! JAN 96 - WIND DIRECTION IS IN TENS, NOT WHOLE DEGREES               !A
!
! INTRODUCED  :  22/08/94
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

      CHARACTER*(*)  REPORT
      CHARACTER*4    MIMJ
      REAL           EXPARR(0:*)
      REAL           MISING           ! MISSING DATA                !2.0
      REAL           KT2MPS           ! CONVERSION FACTOR - KNOTS TO M/S
      REAL           YEAR,MONTH,DAY
      REAL           HOUR,MIN
      REAL           LAT,LONG
      REAL           WDIR,WSPEED
      REAL           TEMP
      INTEGER        IYEAR,IMONTH,IDAY                              !1.3
      INTEGER        POS
      INTEGER        IERR
      INTEGER        IVALUE
      INTEGER        SIGN
      INTEGER        IDEG   ! copy of Degrees                        D
      INTEGER        IMIN   ! copy of Degrees                        D
      INTEGER        ITHOU  ! thousandths of lat or long            !1.4
      INTEGER        IU     ! wind speed units indicator
      INTEGER        NOW(8) ! system date/time
      INTEGER        SYSYR  ! system year from NOW(8)
      INTEGER        QUAD   ! quadrant of globe
      INTEGER        DATIME(5)
      LOGICAL        KNOTS  ! FLAG FOR WINDSPEED UNITS
      LOGICAL        VALDDY ! TRUE IF DATE VALUES CORRECT           !1.3

      CHARACTER*132  HEAD

      DATA           MISING/-9999999./                              !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tessc1.F,v $
     &'//'$ $Date: 30/01/2006 20:25:19$ $Revision: 1$'

      KT2MPS=0.5148
      YEAR=MISING
      MONTH=MISING
      DAY=MISING
      HOUR=MISING
      MIN=MISING
      LAT=MISING
      LONG=MISING
      WDIR=MISING
      WSPEED=MISING
      TEMP=MISING
      IU=MISING
      QUAD=MISING
      IERR=0

! Skip MiMiMjMj at start of report and one or two spaces after it

      MIMJ=REPORT(1:4)
      POS=6
      IF (REPORT(POS:POS).EQ.' ') POS=POS+1

! Next group is date (ddmmy)

      DAY=IVALUE(REPORT(POS:POS+1))
      MONTH=IVALUE(REPORT(POS+2:POS+3))

! Convert single-figure year to either this year or last

      YEAR=IVALUE(REPORT(POS+4:POS+4))
      CALL DATIM(NOW)
      SYSYR=MOD(NOW(8),10)
      IF (SYSYR.EQ.INT(YEAR)) THEN
        YEAR=NOW(8)
      ELSE IF (SYSYR-1.EQ.INT(YEAR) .OR.
     &        (SYSYR.EQ.0 .AND. INT(YEAR).EQ.9)) THEN
        YEAR=NOW(8)-1
      ENDIF

! Check date and give up if invalid

      IYEAR=INT(YEAR)                                               !1.3
      IMONTH=INT(MONTH)                                             !1.3
      IDAY=INT(DAY)                                                 !1.3

      IF(.NOT.(VALDDY(IDAY,IMONTH,IYEAR)))THEN                      !1.3
        PRINT*, ' TESSC1 INVALID DATE >',REPORT,'<'
     &  ,'DAY=',DAY,' MONTH=',MONTH,' YEAR=',YEAR                   !1.3
        IERR=16                                                     !1.3
        GOTO 999                                                    !1.3
      ENDIF                                                         !1.3

      POS=POS+6

! Next group is time (hhmm/).  Give up if hour or minute invalid.

      HOUR=IVALUE(REPORT(POS:POS+1))
      IF (HOUR.LT.0 .OR. HOUR.GT.23) THEN                           !1.3
        PRINT*,'TESSC1: invalid hour >',REPORT,'<'
        IERR=16
        GOTO 999
      ENDIF

      MIN=IVALUE(REPORT(POS+2:POS+3))
      IF (MIN.LT.0 .OR. MIN.GT.59) THEN                             !1.3
        PRINT*,'TESSC1: invalid minute >',REPORT,'<'
        IERR=16
        GOTO 999
      ENDIF

      POS=POS+6

! Latitude group can have 5 figures or 6, starting with quadrant.
! Minutes if 5 figures, thousandths if 6 (if MiMiMjMj is KKYY);
! but 6-figure KKYY groups can end with one or two slashes,
! meaning hundredths (not minutes!) or tenths.

      QUAD=IVALUE(REPORT(POS:POS))
      IF (.NOT.(MOD(QUAD,2).EQ.1)) THEN
        PRINT*,' TESSC1: invalid quadrant >',REPORT,'<'
        IERR=16                                                    !1.4
        GOTO 999                                                   !1.4
      ENDIF                                                        !1.4

      IDEG=IVALUE(REPORT(POS+1:POS+2))                             !1.4

! Minutes

      IF (MIMJ.NE.'KKYY') THEN
        IMIN=IVALUE(REPORT(POS+3:POS+4))                           !1.4
        IF (IDEG.GE.0.AND.IDEG.LE.90.AND.IMIN.GE.0.AND.IMIN.LT.60) THEN
          LAT=REAL(IDEG)+REAL(IMIN)*0.016667                       !1.4
          IF (QUAD.EQ.3 .OR. QUAD.EQ.5) LAT=-LAT
        ELSE                                                       !1.4
          PRINT*,' TESSC1: invalid latitude >',REPORT,'<'
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+6                                                  !1.4

! Thousandths

      ELSE
        IF (REPORT(POS+4:POS+5).EQ.'//') THEN                      !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+3))*100                    !1.6
        ELSE IF (REPORT(POS+5:POS+5).EQ.'/') THEN                  !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+4))*10                     !1.6
        ELSE                                                       !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+5))                        !1.4
        ENDIF                                                      !1.6
        IF (IDEG.GE.0 .AND. IDEG.LE.90 .AND. ITHOU.GE.0) THEN      !1.4
          LAT=REAL(IDEG)+REAL(ITHOU)*0.001                         !1.4
          IF (QUAD.EQ.3 .OR. QUAD.EQ.5) LAT=-LAT
        ELSE                                                       !1.4
          PRINT*,' TESSC1: INVALID LAT >',REPORT,'<'
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+7                                                  !1.4
      ENDIF                                                        !1.4

! Longitude group can have 5 or 6 figures like latitude

      IDEG=IVALUE(REPORT(POS:POS+2))                               !1.4

! Minutes

      IF (MIMJ.NE.'KKYY') THEN
        IMIN=IVALUE(REPORT(POS+3:POS+4))                           !1.4
        IF (IDEG.GE.0.AND.IDEG.LE.180.AND.IMIN.GE.0.AND.IMIN.LT.60) THEN
          LONG=REAL(IDEG)+REAL(IMIN)*0.016667                      !1.4
          IF (QUAD.EQ.5 .OR. QUAD.EQ.7) LONG=-LONG
        ELSE                                                       !1.4
          PRINT*, ' TESSC1: INVALID LONG >',REPORT,'<'
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+6                                                  !1.4

! Thousandths

      ELSE
        IF (REPORT(POS+4:POS+5).EQ.'//') THEN                      !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+3))*100                    !1.6
        ELSE IF (REPORT(POS+5:POS+5).EQ.'/') THEN                  !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+4))*10                     !1.6
        ELSE                                                       !1.6
          ITHOU=IVALUE(REPORT(POS+3:POS+5))                        !1.4
        ENDIF                                                      !1.6
        IF (IDEG.GE.0 .AND. IDEG.LE.180 .AND. ITHOU.GE.0) THEN     !1.4
          LONG=REAL(IDEG)+REAL(ITHOU)*0.001                        !1.4
          IF (QUAD.EQ.5 .OR. QUAD.EQ.7) LONG=-LONG
        ELSE                                                       !1.4
          PRINT*, ' TESSC1: INVALID LONG >',REPORT,'<'
          IERR=16                                                  !1.4
          GOTO 999                                                 !1.4
        ENDIF                                                      !1.4
        POS=POS+7                                                  !1.4
      ENDIF                                                        !1.4

! Wind group is ddff preceded by an indicator from 0 to 3 (odd if knots)

      IU=IVALUE(REPORT(POS:POS))
      IF (IU.GE.0 .AND. IU.LE.3) THEN
        IF (IU.EQ.1 .OR. IU.EQ.3) KNOTS=.TRUE.
        WDIR=IVALUE(REPORT(POS+1:POS+2))
        WSPEED=IVALUE(REPORT(POS+3:POS+4))
        IF (WDIR.NE.MISING) WDIR=WDIR*10.
        IF (KNOTS .AND. WSPEED.NE.MISING) WSPEED=WSPEED*KT2MPS
        POS=POS+6
      ENDIF

! 4-group is temperature (second figure sign) in tenths Celsius

      IF (REPORT(POS:POS).EQ.'4') THEN
        SIGN=IVALUE(REPORT(POS+1:POS+1))
        TEMP=IVALUE(REPORT(POS+2:POS+4))
        IF (TEMP.NE.MISING) THEN
          TEMP=(TEMP*0.1)+273.1
          IF (SIGN.EQ.1) TEMP=-TEMP
        ENDIF
        POS=POS+6
      ENDIF

999   CONTINUE

! Put values in output arrays

      EXPARR(6) =YEAR
      EXPARR(8) =MONTH
      EXPARR(10)=DAY
      EXPARR(12)=HOUR
      EXPARR(14)=MIN
      EXPARR(16)=LAT
      EXPARR(18)=LONG
      EXPARR(20)=WDIR
      EXPARR(22)=WSPEED
      EXPARR(24)=TEMP

      DATIME(1)=YEAR
      DATIME(2)=MONTH
      DATIME(3)=DAY
      DATIME(4)=HOUR
      DATIME(5)=MIN

      RETURN
      END
