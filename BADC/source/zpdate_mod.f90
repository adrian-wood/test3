!-----------------------------------------------------------------------
! ZPDATE - MetDB version
! ------
!
! REVISION INFO  :
!
! $Revision: 1$
! $Date: 25/09/2012 14:45:56$
!
! CHANGE HISTORY :
!
! $Log:
! MB-1457  Feb 2017  Add ISO date validation functions  Andy Moorhouse
!
!  1    MOODS      1.0         25/09/2012 14:45:56    Paul Barnham    Copied
!       from MetDB project, revision 1.
! $
! Revision 1.2  2003/01/30 15:32:35  usmdb
! Now MetDB version as a bug found in the original. DATE31 and
! DATE32 problems with the MONTHS array when the month is
! January - S.Cox
!
! 7 Updated 17/4/98 by Stephen Turner
!                       Converted to F90 and added Zeller Method
! 6 Updated 23/3/98 by Edward Jones
!                       Added JDAY  Routine
! 5 Updated 17/2/98 by Edward Jones
!                       Added DATCHK and MNTHDS Routines
! 4 Updated 30/1/98 by Edward Jones
!                       Update ZPDATE subroutine
!                       Added ISALEAP subroutine
!                       Ported to HP, Cray and PC from MET.SRCELIB
! 3 RE-COMPILED 29/10/91 TO PRODUCE 31 BIT ADDRESSING MODE VERSION
!       BY M. COLLIER - COPIED TO MET.PROGLIB
! 2 RE-WRITE   BY PAUL WHITE  13/12/89
!                       TO MAKE RE-ENTRANT WITH INLINE EXPANSION
!                       TRANSLATED BY FPP 2.26B16 13/12/89  11:41:08
!                       SWITCHES: LSTOFF=T,OPTON=78,TDYON=FX
! 1 TRANSLATED BY FPP 2.26B16 11/12/89  11:44:56   TDYON=X
! 0 ORIGINAL VERSION BY JOHN PRINCE, LONG AGO IN THE MISTS OF TIME,
!                                     FOR THE IBM WITH STATIC MEMORY
!
!-----------------------------------------------------------------------

MODULE zpdate_mod
SAVE

TYPE DATETIME
  INTEGER :: YEAR
  INTEGER :: MONTH
  INTEGER :: DAY
  INTEGER :: HOUR
  INTEGER :: MINUTE
  INTEGER :: SECOND
END TYPE DATETIME

CONTAINS
LOGICAL FUNCTION ISALEAP(IY)
!
!     Returns .TRUE. if IY is a Leap year
!     Returns .FALSE. if IY is not a Leap year
!
  IMPLICIT NONE
!					INPUT ARGUMENT
  INTEGER, INTENT(IN) :: IY


  IF (IY/4*4 .NE. IY) THEN			 ! Divide by 4
     ISALEAP=.FALSE.
  ELSE  		
    IF (IY/400*400 .EQ. IY) THEN	 ! Century check
       ISALEAP=.TRUE.
    ELSE
	  IF (IY/100*100 .EQ. IY) THEN   ! Century qualifier
         ISALEAP=.FALSE.
      ELSE
        ISALEAP=.TRUE.
      ENDIF
    ENDIF
  ENDIF
END FUNCTION ISALEAP

!-------------------------------------------------------------------------

SUBROUTINE  ZPDATE
!
!     Prints version information
!
  PRINT *, '  ZPDATE - f90 MetDB version'
  PRINT *, '  LAST MODIFIED $Date: 25/09/2012 14:45:56$'
  PRINT *, '  by Simon Cox (DD)'
  RETURN
END SUBROUTINE ZPDATE

!-----------------------------------------------------------------------
SUBROUTINE DATE21 (IDY, IY, ICD)
!
!     DAYS SINCE 1.1.1900, FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IYN

  IYN = IY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
     ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF

END SUBROUTINE DATE21
!-----------------------------------------------------------------------
SUBROUTINE DATE23 (IDY, IY, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAY OF YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) ::  IDY, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: I, K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS


  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF


  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
  K = IDY

  DO I=1,12
    K = K - MONTHS(I)
    IF (K .GT. 0) THEN
       CYCLE
    ELSE
      ID = K + MONTHS(I)
      IM = I
      INY = IY
    ENDIF
    EXIT
  END DO

END SUBROUTINE DATE23
!-----------------------------------------------------------------------
SUBROUTINE DATE13 (ICD, ID, IM, INY)
!
!     DAY, MONTH, YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ID, IM, INY
!                       LOCAL VARIABLES
  INTEGER :: IDY, IY
  INTEGER :: K,KD,KE,KY,I,K1X, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN				!these allow for the non-leap years 1900,
     K = K + 1
     IF (K .GE. 73416) THEN         !2100,
        K = K + 1
        IF (K .GE. 109941) THEN     !2200,
           K = K + 1
           IF (K .GE. 146466) THEN  !2300 ...
       		  K = K + 1
           ENDIF
        ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN			! and 1800 respectively
     K = K - 1
  ENDIF 	

  KY = K/1461*4
  KD = K - K/1461*1461
  IF (KD .LT. 0) THEN
     KD = KD + 1461
     KY = KY - 4
  ENDIF
  KY = KY + 1900
  IF (KD .GT. 366) THEN
     KD = KD - 1
     KE = KD/365
     KD = KD - KD/365*365
  ENDIF
  IF (KD .EQ. 0) THEN
     KE = KE - 1
     KD = 365
  ENDIF
  INY = KY + KE
  IDY = KD
  IY = INY

  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

  MONTHS = (/31,days_in_feb,31,30,31,30,31,31,30,31,30,31/)
	
  K1X = IDY

  DO I=1,12
    K1X = K1X - MONTHS(I)
    IF (K1X .GT. 0) THEN
	   CYCLE
	ELSE
      ID = K1X + MONTHS(I)
      IM = I
      INY = IY

    ENDIF
    EXIT
  END DO	

END SUBROUTINE DATE13
!-----------------------------------------------------------------------
SUBROUTINE DATE31 (ID, IM, IY, ICD)
!
!     DAYS SINCE 1.1.1900 FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN) :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: ICD
!                       LOCAL VARIABLES
  INTEGER :: IDY, INY
  INTEGER :: K,IYN, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS

  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

! array ranges from 0 to November (not from Jan to Dec as in
! previous version)                                                 !1.2

  MONTHS = (/0,31,days_in_feb,31,30,31,30,31,31,30,31,30/)          !1.2

  K = SUM(MONTHS(1:IM))                                             !1.2

  IDY = K + ID
  INY = IY
  IYN = INY - 1900
  IF (IYN .GT. 0) THEN
     ICD = IDY + IYN*365 + (IYN-1)/4 - (IYN-1)/100 + (IYN+299)/400
  ELSE
    ICD = IDY + IYN*365 + IYN/4 - IYN/100
  ENDIF


 END SUBROUTINE DATE31
!-----------------------------------------------------------------------
SUBROUTINE  DATE12(ICD,IDY,IY)
!
!     DAY OF YEAR FROM DAYS SINCE 1.1.1900
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ICD
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, IY
!                       LOCAL VARIABLES
  INTEGER :: K, KD, KE, KY

  K = ICD
  KE = 0
  IF (K .GE. 366) THEN
     K = K + 1
     IF (K .GE. 73416) THEN
        K = K + 1
        IF (K .GE. 109941) THEN
           K = K + 1
           IF (K .GE. 146466) THEN
		      K = K + 1
		   ENDIF
         ENDIF
     ENDIF
  ENDIF
  IF (K .LE. -36159) THEN
     K = K - 1
  ENDIF
     KY = K/1461*4
     KD = K - K/1461*1461
     IF (KD .LT. 0) THEN
        KD = KD + 1461
        KY = KY - 4
     ENDIF
     KY = KY + 1900
     IF (KD .GT. 366) THEN
        KD = KD - 1
        KE = KD/365
        KD = KD - KD/365*365
     ENDIF
     IF (KD .EQ. 0) THEN
        KE = KE - 1
        KD = 365
     ENDIF
     IY = KY + KE
     IDY = KD

END SUBROUTINE DATE12
!-----------------------------------------------------------------------
SUBROUTINE  DATE32(ID,IM,IY,IDY,INY)
!
!     DAY OF YEAR FROM DAY, MONTH, YEAR
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID, IM, IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: IDY, INY
!                       LOCAL VARIABLES
  INTEGER :: K, days_in_feb
  INTEGER, DIMENSION(12) :: MONTHS



  IF (ISALEAP(IY)) THEN
     days_in_feb = 29
  ELSE
     days_in_feb = 28
  ENDIF

! array ranges from 0 to November (not from Jan to Dec as in
! previous version)                                                 !1.2

  MONTHS = (/0,31,days_in_feb,31,30,31,30,31,31,30,31,30/)          !1.2

  K = SUM(MONTHS(1:IM))                                             !1.2

  IDY = K + ID
  INY = IY

END SUBROUTINE DATE32


!-----------------------------------------------------------------------
INTEGER FUNCTION MNTHDS(MONTH,YEAR)
!
!	Returns Days in Month from Month and Year.
!
  IMPLICIT NONE
!					INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: MONTH,YEAR
!					LOCAL VARIABLES
!  LOGICAL :: ISALEAP


SELECT CASE (MONTH)
  CASE (1,3,5,7,8,10,12)
    MNTHDS = 31
  CASE (4,6,9,11)
    MNTHDS = 30
  CASE (2)
    IF (ISALEAP(YEAR)) THEN
	  MNTHDS = 29
    ELSE
	  MNTHDS = 28
    ENDIF
  CASE DEFAULT
	PRINT *, "Error in function MNTHDS"
	RETURN
END SELECT

END FUNCTION MNTHDS
!-----------------------------------------------------------------------
SUBROUTINE ISO2UTC (ISOSTR,VALID,ODATETIME)
!
! Converts a supplied string containing an ISO date (with offset if
! present) to UTC, returning INT values for year, month, day etc.
! Firstly checks validity of the syntax of the string, i.e. colon separators,
! offset format, date and time portions must be separated with "T".
! Then checks actual date & time values. Checks for offsets.
! Converts values to century minutes, adjusts if necessary and
! converts back. Finally, returns UTC values

  IMPLICIT NONE
  CHARACTER(LEN=*), INTENT(IN)  :: ISOSTR
  LOGICAL, INTENT(OUT) :: VALID
  TYPE(DATETIME), OPTIONAL, INTENT(OUT) :: ODATETIME

! Locals
  INTEGER :: POSITION, IOSTAT, OFFSETPOS, VRESULT = 0
  CHARACTER(LEN=10) :: CDATE ! 10 chars max, if yyyy-mm-dd
  CHARACTER(LEN=20) :: CTIME
  CHARACTER(LEN=4) :: CYEAR
  CHARACTER(LEN=2) :: CMONTH, CDAY, CHOUR, CMINUTE, CSECOND
  INTEGER :: YEAR, MONTH, DAY, HOUR, MINUTE, SECOND = 0
  INTEGER :: CENTDAY, CENTHRS, CENTMINS = 0
  CHARACTER(LEN=2) :: CADJ_HRS, CADJ_MINS
  INTEGER :: ADJ_HRS, ADJ_MINS = 0
  CHARACTER(LEN=1) :: CADJ_SGN
  CHARACTER(LEN=20) :: COFFSET

  VALID = .TRUE.
  IF (PRESENT(ODATETIME)) THEN
    ODATETIME%YEAR = 0
    ODATETIME%MONTH = 0
    ODATETIME%DAY = 0
    ODATETIME%HOUR = 0
    ODATETIME%MINUTE = 0
    ODATETIME%SECOND = 0
  END IF

! Must have a separator between date & time of "T"
  POSITION = INDEX(ISOSTR,"T")
  IF (POSITION == 0) THEN     ! No "T" separator, so invalid - bail out
    VALID = .FALSE.
    RETURN
  ELSE                        ! Split into date and time
    CDATE = TRIM(ISOSTR(1:POSITION-1))
    CTIME = TRIM(ISOSTR(POSITION+1:))
  END IF

! Check date: must be either 8 chars yyyymmdd all numeric, or 10 chars
! with "-" separators yyyy-mm-dd.
! Populate TEMPDATE with yyyymmdd characters.
  IF (LEN(TRIM(CDATE)) == 8) THEN
    CYEAR = CDATE(1:4)
    CMONTH = CDATE(5:6)
    CDAY = CDATE(7:8)
  ELSE IF (LEN(TRIM(CDATE)) == 10) THEN
    IF (CDATE(5:5) /= "-" .OR. CDATE(8:8) /= "-") THEN
      VALID=.FALSE.
    ELSE
      CYEAR = CDATE(1:4)
      CMONTH = CDATE(6:7)
      CDAY = CDATE(9:10)
    END IF
  ELSE  ! neither 8 or 10 chars
    VALID = .FALSE.
  END IF

! If date is invalid format, no point carrying on, so return.
  IF (.NOT. VALID) RETURN

! Now check the validity of the date field
  READ(CYEAR,*,IOSTAT=IOSTAT) YEAR
  IF (IOSTAT /= 0) VALID = .FALSE.

  READ(CMONTH,*,IOSTAT=IOSTAT) MONTH
  IF (IOSTAT /= 0) VALID = .FALSE.

  READ(CDAY,*,IOSTAT=IOSTAT) DAY
  IF (IOSTAT /= 0) VALID = .FALSE.

! If date yr/mth/day values invalid, no point carrying on - bail.
  IF (.NOT. VALID) RETURN

  CALL DATCHK(DAY, MONTH, YEAR, VALID)

  IF ( .NOT. VALID) THEN
    RETURN
  END IF

! Check time: may be colon separated. Must have a suffix of either "Z"
! or an offset of +-hh(mm), which may be colon separated.
  OFFSETPOS = SCAN(CTIME,"Z+-")
  IF (OFFSETPOS == 0) THEN  ! No Offset character, so invalid
    VALID = .FALSE.
    RETURN
  ELSE
    COFFSET = CTIME(OFFSETPOS+1:)
    CADJ_SGN = CTIME(OFFSETPOS:OFFSETPOS)
    CTIME = CTIME(1:OFFSETPOS-1)
  END IF

! Time must contain only numbers and colon
  VRESULT = VERIFY(TRIM(CTIME),"0123456789:")
  IF (VRESULT /= 0) THEN
    VALID= .FALSE.
    RETURN
  END IF

  CHOUR = CTIME(1:2) ! Hours is always 1st two chars
  CTIME = CTIME(3:)  ! Remainder

  IF (CTIME(1:1) == ":") THEN ! Got a colon separator
    CMINUTE = CTIME(2:3) ! Minutes
    CTIME = CTIME(4:)    ! Remainder
  ELSE
    CMINUTE = CTIME(1:2) ! Minutes
    CTIME = CTIME(3:)    ! Remainder
  END IF

  IF (LEN(TRIM(CTIME)) > 0) THEN ! We have seconds too
    IF (CTIME(1:1) == ":") THEN
      CSECOND = CTIME(2:3) ! Seconds
    ELSE
      CSECOND = CTIME(1:2) ! Seconds
    END IF
  ELSE
    CSECOND = "00"
  END IF

! Now check the validity of the time fields
  READ(CHOUR,*,IOSTAT=IOSTAT) HOUR
  IF (IOSTAT /= 0) VALID = .FALSE.

  READ(CMINUTE,*,IOSTAT=IOSTAT) MINUTE
  IF (IOSTAT /= 0) VALID = .FALSE.

  READ(CSECOND,*,IOSTAT=IOSTAT) SECOND
  IF (IOSTAT /= 0) VALID = .FALSE.

! If time hr/min/sec values invalid, no point carrying on - bail.
  IF (.NOT. VALID) RETURN

  CALL TIMCHK(HOUR, MINUTE, SECOND, VALID)

! Leap second allowed only on last day of month...
  IF (SECOND == 60) THEN
    IF (DAY /= MNTHDS(MONTH, YEAR)) THEN
      VALID = .FALSE.
    END IF
  END IF

! If time is invalid, no point carrying on, so return.
  IF (.NOT. VALID) RETURN

! Now deal with offset adjustment if supplied
  IF (CADJ_SGN /= "Z") THEN
    CADJ_HRS = COFFSET(1:2) ! Hours is always 1st two chars
    COFFSET = COFFSET(3:)   ! Remainder
    IF (LEN(TRIM(COFFSET)) > 0) THEN ! Minute offset too
      IF (COFFSET(1:1) == ":") THEN
        CADJ_MINS = COFFSET(2:3)
      ELSE
        CADJ_MINS = COFFSET(1:2)
      END IF
    ELSE
      CADJ_MINS = "00"
    END IF

    READ(CADJ_HRS,*,IOSTAT=IOSTAT) ADJ_HRS
    IF (IOSTAT /= 0) VALID = .FALSE.

    READ(CADJ_MINS,*,IOSTAT=IOSTAT) ADJ_MINS
    IF (IOSTAT /= 0) VALID = .FALSE.

    IF (VALID) THEN
      ADJ_MINS = (ADJ_HRS * 60) + ADJ_MINS
      ! For a Positive offset, minutes should be subtracted, and vv
      IF (CADJ_SGN == "+") ADJ_MINS = 0 - ADJ_MINS

      ! Convert to century minutes
      CALL DATE31(DAY, MONTH, YEAR, CENTDAY)        ! Century days
      CENTHRS = ((CENTDAY-1)*24) + HOUR             ! Century hours
      CENTMINS = (CENTHRS*60) + MINUTE + ADJ_MINS   ! Century mins

      ! Now convert back from century mins
      MINUTE = MOD(CENTMINS,60)
      CENTHRS = CENTMINS/60
      HOUR = MOD(CENTHRS,24)
      CENTDAY = ((CENTHRS - HOUR)/24) + 1
      CALL DATE13(CENTDAY, DAY, MONTH, YEAR)
    END IF

  END IF    !  Offset adjustments

  IF (VALID) THEN
    IF (PRESENT(ODATETIME)) THEN
      ODATETIME%YEAR = YEAR
      ODATETIME%MONTH = MONTH
      ODATETIME%DAY = DAY
      ODATETIME%HOUR = HOUR
      ODATETIME%MINUTE = MINUTE
      ODATETIME%SECOND = SECOND
    END IF
  END IF

END SUBROUTINE ISO2UTC

!-----------------------------------------------------------------------
SUBROUTINE TIMCHK (HOUR,MINUTE,SECOND,VALID)
!
! Checks time for validity.
! NB1 leap seconds: this routine allows seconds to be 60 if both HOUR is
! 23 and MINUTE is 59 - user should also check that the associated date
! is the last day of a month, as that is the only date when leap seconds
! can be added (use MNTHDS).
! NB2 HOUR allowed to be 24, if both MINUTE and SECOND are 0.
!
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: HOUR,MINUTE,SECOND
  LOGICAL, INTENT(OUT) :: VALID

  VALID = .TRUE.
  IF (HOUR < 0 .OR. HOUR > 24) VALID = .FALSE.
  IF (MINUTE < 0 .OR. MINUTE > 59) VALID = .FALSE.
  IF (SECOND < 0 .OR. SECOND > 60) VALID = .FALSE.

  IF (SECOND == 60 .AND. (HOUR /= 23 .OR. MINUTE /= 59)) VALID = .FALSE.

  IF (HOUR == 24 .AND. (MINUTE /= 0 .OR. SECOND /= 0)) VALID = .FALSE.

  RETURN
END SUBROUTINE TIMCHK

!-----------------------------------------------------------------------
SUBROUTINE DATCHK (DAY,MONTH,YEAR,VALID)
!
!	Checks the date and returns to a given statement for invalid
!     values
!
  IMPLICIT NONE
  INTEGER, INTENT(IN)  :: DAY,MONTH,YEAR
  LOGICAL, INTENT(OUT) :: VALID

  INTEGER :: LDAY
  INTEGER :: LMNTH
  !INTEGER :: MNTHDS1


  IF (YEAR .EQ. 1752) THEN
     LDAY=14
     LMNTH=9
  ELSE
    LDAY=1
    LMNTH=1
  ENDIF

!	Check that within valid year range
!	Check that within valid month range
!	and check that within valid day range

  IF ((YEAR .GE. 1752) .AND.			&
              (YEAR .LE. 2399) .AND.    &
              (MONTH .GE. LMNTH) .AND.  &
              (MONTH .LE. 12)   .AND.   &
              (DAY .GE. LDAY) .AND.     &
           (DAY .LE. MNTHDS(MONTH,YEAR))) THEN
!	Valid date, so return from here	
              VALID=.TRUE.
  ELSE
              VALID=.FALSE.
  ENDIF
  RETURN
END SUBROUTINE DATCHK
!-----------------------------------------------------------------------
! The JDATE Conversion algorithms are based on the algorithm published
! in a letter to the editor of Communications of the ACM (CACM, volume 11,
! number 10, October 1968, p.657) by Henry F. Fliegel and
! Thomas Van Flandern
! This algorithm is valid only for dates from
! 1/3/-4900 G onward when converting from a Julian day number to a date,
! or from 1/3/-4800 when converting from a date to a Julian day number.
! It should be noted that these algorithms are valid only in the
! Gregorian Calendar and the Proleptic Gregorian Calendar (after the
! dates given above). They do not handle dates in the Julian Calendar.
!-----------------------------------------------------------------------
SUBROUTINE JDATE31(ID,IM,IY,OD)
!
!     Returns the Julian Day Number for a Day, Month, Year
!
  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID,IM,IY
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD

  OD = ID - 32075											     &
           + 1461 * ( IY + 4800 - ( 14 - IM ) / 12 )/4		     &
           + 367 * ( IM - 2 + (( 14 - IM ) / 12 ) * 12 ) / 12    &
           - 3 * ( ( IY + 4900 - ( 14 - IM ) / 12 ) / 100 ) / 4


END SUBROUTINE JDATE31

!-----------------------------------------------------------------------
SUBROUTINE JDATE13(ID,OD,OM,OY)
!
!     Returns the Day, Month, Year from a Julian Day Number
!

  IMPLICIT NONE
!                        INPUT ARGUMENTS
  INTEGER, INTENT(IN)  :: ID
!                       OUTPUT ARGUMENTS
  INTEGER, INTENT(OUT) :: OD,OM,OY
!                       LOCAL VARIABLES
  INTEGER :: J,I,L,N

  L = ID + 68569
  N = ( 4 * L ) / 146097
  L = L - ( 146097 * N + 3 ) / 4
  I = ( 4000 * ( L + 1 ) ) / 1461001
  L = L - ( 1461 * I ) / 4 + 31
  J = ( 80 * L ) / 2447
  OD = L - ( 2447 * J ) / 80
  L = J / 11
  OM = J + 2 - ( 12 * L )
  OY = 100 * ( N - 49 ) + I + L


END SUBROUTINE JDATE13

END MODULE zpdate_mod
