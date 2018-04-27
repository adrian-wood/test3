SUBROUTINE OBHOUR(DATE,DAY,HOUR,MIMJ,TOL,RC)

!----------------------------------------------------------------------
!
! PROGRAM       : OBHOUR
!
! PURPOSE       : REJECT REPORTS TOO FAR AHEAD OF CURRENT TIME, THEN
!                 CHECK REPORT & BULLETIN DATE/TIMES, ASSUMING THAT
!                 MISTAKES IN THE BULLETIN DATE/TIME ARE LESS LIKELY.
!                 ERROR IF DIFFERENCE MORE THAN TOLERANCE, EXCEPT IF
!                 OLD DATA, IN WHICH CASE IT MAY BE LAST MONTH'S.
!
! DATA TYPE(S)  : ANY OBS WITH DATE/TIME GIVEN SOMEWHERE ELSE AS WELL
!                 IN BULLETIN HEADING
!
! CALLED BY     : UAHEAD, SYNBUL, ENHBUL, NCMBUL, SATEM1, TRKEXC,
!                 SRWBUL, SYNOB
!
! CALLS         : DATE31, VALDDY, DATIM
!
! ARGUMENTS     : (1) ARRAY WITH DAY & HOUR SET FROM BULLETIN HEADER
!                 (2) DAY FROM WITHIN BULLETIN
!                 (3) HOUR FROM WITHIN BULLETIN
!                 (4) MIMJ (IN CASE IT'S OLD DATA)
!                 (5) TOLERANCE (IN HOURS)
!                 (6) RETURN CODE (0 IF OK, 4 IF DIFFERENCE TOO BIG)
!
! REVISION INFO :
!
! $Workfile: obhour.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 20/12/2010 16:10:47$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         20/12/2010 16:10:47    Alison Weir     Update
!        'CALLS' in header comments
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
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

! Use statements:
USE datim_mod
USE valddy_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) :: DATE(4)  !A01
INTEGER,          INTENT(IN)    :: DAY      !A02
INTEGER,          INTENT(IN)    :: HOUR     !A03
CHARACTER(LEN=4), INTENT(IN)    :: MIMJ     !A04
INTEGER,          INTENT(IN)    :: TOL      !A05
INTEGER,          INTENT(OUT)   :: RC       !A06

! Local declarations:

INTEGER          ::  YEAR
INTEGER          ::  MONTH
INTEGER          ::  NOW(9)
INTEGER          ::  BCENDY  ! CENTURY DAYS AND HOURS
INTEGER          ::  RCENDY  ! CENTURY DAYS AND HOURS
INTEGER          ::  BCENHR  ! CENTURY DAYS AND HOURS
INTEGER          ::  RCENHR  ! CENTURY DAYS AND HOURS
INTEGER          ::  NCENDY
INTEGER          ::  NCENHR
INTEGER          ::  MAXHRS ! Max allowed report-system time difference

CHARACTER(LEN=100) ::  CFMT1
CHARACTER(LEN=100) ::  CFMT2
CHARACTER(LEN=100) ::  CFMT3

RCENHR=0

! SET YEAR & MONTH PROVISIONALLY FROM CURRENT DATE/TIME, BEFORE
! ADJUSTING THEM IF NECESSARY TO CALCULATE CENTURY-DAY.

CALL DATIM(NOW)
YEAR=NOW(8)
MONTH=NOW(7)

CALL DATE31(NOW(6),MONTH,YEAR,NCENDY)
NCENHR=(NCENDY-1)*24+NOW(5)            ! CURRENT CENTURY-HOUR

! IF IT IS OLD DATA THEN IF THE DAY IS GREATER THAN THE SYSTEM DATE
! THEN ASSUME IT IS FOR THE PREVIOUS MONTH.

IFLABEL1: &
IF (MIMJ(1:3) == 'OLD') THEN
  IF (DAY > NOW(6)) THEN
    MONTH=MONTH-1
    IF (MONTH == 0) THEN
      MONTH=12
      YEAR=YEAR-1
    END IF
  END IF
ELSE

! IF THE CURRENT DAY OF THE MONTH AND THE DAY IN THE BULLETIN HEADER
! SUGGEST THE DATA IS FOR LAST MONTH OR NEXT MONTH, CHANGE THE MONTH.
! AND GO BACK A MONTH IF THE BULLETIN DAY WOULD OTHERWISE BE FOR THE
! DAY AFTER TOMORROW OR LATER.

IFLABEL2: &
  IF (DATE(3) == 1 .AND. NOW(6) >= 28) THEN
    MONTH=MONTH+1
    IF (MONTH == 13) THEN
      MONTH=1
      YEAR=YEAR+1
    END IF
  ELSE IF ((DATE(3) >= 28 .AND. NOW(6) == 1) &
     .OR. DATE(3) > NOW(6)+1) THEN
    MONTH=MONTH-1
    IF (MONTH == 0) THEN
      MONTH=12
      YEAR=YEAR-1
    END IF
  END IF IFLABEL2

! WITH THE ADJUSTED MONTH WORK OUT THE CENTURY-HOUR

  CALL DATE31(DATE(3),MONTH,YEAR,BCENDY)
  BCENHR=(BCENDY-1)*24+DATE(4)

! NOW DO THE SAME FOR THE SECOND TIME (CALL IT THE REPORT TIME)

  YEAR=NOW(8)
  MONTH=NOW(7)

IFLABEL3: &
  IF (DAY == 1 .AND. NOW(6) >= 28) THEN
    MONTH=MONTH+1
    IF (MONTH == 13) THEN
      MONTH=1
      YEAR=YEAR+1
    END IF
  ELSE IF ((DAY >= 28 .AND. NOW(6) == 1) &
     .OR. DAY > NOW(6)+1) THEN
    MONTH=MONTH-1
    IF (MONTH == 0) THEN
      MONTH=12
      YEAR=YEAR-1
    END IF
  END IF IFLABEL3

  CALL DATE31(DAY,MONTH,YEAR,RCENDY)
  RCENHR=(RCENDY-1)*24+HOUR
END IF IFLABEL1

! IF THE REPORT IS MORE THAN AN HOUR EARLY (3 HOURS FOR TROPADV
! REPORTS), DON'T TRUST ITS TIME.  IF THE CENTURY-HOURS ARE NO
! MORE THAN THE TOLERANCE APART, ACCEPT THE DAY/HOUR FROM THE
! REPORT.  OTHERWISE SET ERROR RETURN CODE.

MAXHRS = 1
IF (MIMJ == 'TROP') MAXHRS = 3    ! TROPADV reports

IFLABEL4: &
IF (RCENHR > NCENHR+MAXHRS) THEN
  CFMT1='(I5.2,''/'',I2.2,''Z REPORT AT'',I3.2,''/'',I2.2,'// &
        '''Z (IN '',A4,'' BULLETIN)'')'
  WRITE (*,CFMT1) DAY,HOUR, NOW(6),NOW(5), MIMJ
  RC=4
ELSE IF (MIMJ(1:3) == 'OLD' .OR. ABS(RCENHR-BCENHR) <= TOL) THEN
  DATE(1)=YEAR
  DATE(2)=MONTH
  DATE(3)=DAY
  DATE(4)=HOUR
  RC=0
  IF (.NOT.VALDDY(DAY,MONTH,YEAR)) THEN
    RC=4
    CFMT3='(I5.2,''/'',I2.2,''/'',I4.4,'' :  INVALID DATE'')'
    WRITE (*,CFMT3) DAY,MONTH,YEAR
  END IF

ELSE
  CFMT2='(I5.2,''/'',I2.2,''Z REPORT IN'',I3.2,''/'',I2.2,''Z '','// &
         'A4,'' BULLETIN'')'
  WRITE (*,CFMT2) DAY,HOUR, DATE(3),DATE(4), MIMJ
  RC=4
END IF IFLABEL4
RETURN
END SUBROUTINE OBHOUR
