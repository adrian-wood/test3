      SUBROUTINE ONEHOUR(REQ,RC)
      IMPLICIT NONE

!----------------------------------------------------------------------
!
! PROGRAM       : ONEHOUR
!
! PURPOSE       : To split an MDB request into hourly requests
!
! DATA TYPES    : any retrievable using MDB requests
!
! CALLED BY     : MDBX
!
! CALLS         : IVALUE, DATE31,DATE13
!
! PARAMETERS    : (1) request
!                 (2) return code (4 if more hours to return, 0 if not)
!
!----------------------------------------------------------------------
      CHARACTER*(*) REQ
      INTEGER RC

      INTEGER IST            ! pointer to START TIME
      INTEGER LST            ! pointer to END TIME
      INTEGER IYMD           ! pointer to year in start time
      INTEGER LYMD           ! pointer to year in end time

      INTEGER IYEAR          ! year in start time
      INTEGER IMONTH
      INTEGER IDAY
      INTEGER IHOUR
      INTEGER IMIN

      INTEGER LYEAR          ! year in end time
      INTEGER LMONTH
      INTEGER LDAY
      INTEGER LHOUR
      INTEGER LMIN

      INTEGER NYEAR          ! year to return in hourly request
      INTEGER NMONTH
      INTEGER NDAY
      INTEGER NHOUR

      INTEGER ICENDAY        ! start time century-day
      INTEGER LCENDAY        ! end time century-day
      INTEGER ICENTHR        ! start time century-hour
      INTEGER LCENTHR        ! end time century-hour
      INTEGER LOOPHR         ! loop century-hour
      INTEGER IVALUE

      DATA IST/0/

! First time find start & end times & convert to century-hours

      IF (IST.EQ.0) THEN     ! (Would it be better to check for RC=4?)
        IST=INDEX(REQ,'START TIME ')
        LST=INDEX(REQ,' END TIME ')
        IYMD=IST+INDEX(REQ(IST:),'/')-9
        LYMD=LST+INDEX(REQ(LST:),'/')-9

! Start time

        IYEAR=IVALUE(REQ(IYMD:IYMD+3))
        IMONTH=IVALUE(REQ(IYMD+4:IYMD+5))
        IDAY=IVALUE(REQ(IYMD+6:IYMD+7))
        IHOUR=IVALUE(REQ(IYMD+9:IYMD+10))
        IMIN=IVALUE(REQ(IYMD+11:IYMD+12))

! End time

        LYEAR=IVALUE(REQ(LYMD:LYMD+3))
        LMONTH=IVALUE(REQ(LYMD+4:LYMD+5))
        LDAY=IVALUE(REQ(LYMD+6:LYMD+7))
        LHOUR=IVALUE(REQ(LYMD+9:LYMD+10))
        LMIN=IVALUE(REQ(LYMD+11:LYMD+12))

! Convert start & end times to century days & then hours.

        CALL DATE31(IDAY,IMONTH,IYEAR,ICENDAY)
        CALL DATE31(LDAY,LMONTH,LYEAR,LCENDAY)
        ICENTHR=(ICENDAY-1)*24+IHOUR
        LCENTHR=(LCENDAY-1)*24+LHOUR
        LOOPHR=ICENTHR
      ENDIF

! Loop over hours between start & end, resetting date/time in request
! to start at hh00 & end at hh59 (but preserving minutes in original
! request for first & last hours).

      IF (ICENTHR.LT.LCENTHR .AND. LOOPHR.LE.LCENTHR) THEN
        NHOUR=MOD(LOOPHR,24)
        CALL DATE13(LOOPHR/24+1,NDAY,NMONTH,NYEAR)

        WRITE (REQ(IYMD:IYMD+3),'(I4.4)') NYEAR
        WRITE (REQ(IYMD+4:IYMD+5),'(I2.2)') NMONTH
        WRITE (REQ(IYMD+6:IYMD+7),'(I2.2)') NDAY
        WRITE (REQ(IYMD+9:IYMD+10),'(I2.2)') NHOUR

! Copy start date & hour (just set) to end time, then set minutes.

        REQ(LYMD:LYMD+10)=REQ(IYMD:IYMD+10)

! First hour?  If so, keep minutes from original start time

        IF (LOOPHR.EQ.ICENTHR) THEN
          WRITE (REQ(IYMD+11:IYMD+12),'(I2.2)') IMIN
        ELSE
          REQ(IYMD+11:IYMD+12)='00'
        ENDIF

! Last hour?  If so, keep minutes from original end time

        IF (LOOPHR.EQ.LCENTHR) THEN
          WRITE (REQ(LYMD+11:LYMD+12),'(I2.2)') LMIN
        ELSE
          REQ(LYMD+11:LYMD+12)='59'
        ENDIF

! Next hour: have we reached the end time?

        LOOPHR=LOOPHR+1
        IF (LOOPHR.LE.LCENTHR) THEN
          RC=4
        ELSE
          RC=0
        ENDIF
      ELSE
        RC=0
      ENDIF

! If we've finished with this request, reset IST to allow another one.

      IF (RC.EQ.0) IST=0
      RETURN
      END
