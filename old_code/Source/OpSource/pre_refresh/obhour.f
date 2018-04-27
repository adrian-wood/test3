      SUBROUTINE OBHOUR(DATE,DAY,HOUR,MIMJ,TOL,RC)

      IMPLICIT NONE

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
! CALLS         : DATE31, VALDDY
!
! PARAMETERS    : (1) ARRAY WITH DAY & HOUR SET FROM BULLETIN HEADER
!                 (2) DAY FROM WITHIN BULLETIN
!                 (3) HOUR FROM WITHIN BULLETIN
!                 (4) MIMJ (IN CASE IT'S OLD DATA)
!                 (5) TOLERANCE (IN HOURS)
!                 (6) RETURN CODE (0 IF OK, 4 IF DIFFERENCE TOO BIG)
!
! REVISION INFO :
!
! $Workfile: obhour.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 31/08/2010 15:06:45$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         31/08/2010 15:06:45    Brian Barwell
!       Accept TROPADV bulletins with report times up to 3 hours in future.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:47    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:42  usmdb
! Removed unused variable MISSING. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  98/11/12  08:50:00  08:50:00  usmdb (Generic MetDB accou
! 16/11/1998  fixes possible end of month problem. v=15 ev=1
!
! Revision 1.3  98/09/16  16:11:29  16:11:29  usmdb (Generic MDB account
! 21/09/1998 usjh Date checking routine added (prob 131, 56172)
!                 + IMPLICIT NONE v(G)=15, ev(G)=1
!
! Revision 1.2  97/07/31  09:31:55  09:31:55  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 13:05:02  uspm
! Initial revision
!
! DEC 94: MAKE TOLERANCE AN ARG; SET RC IF ERROR, NOT MISSING DAY.
!         REJECT REPORTS TOO FAR AHEAD OF CURRENT TIME.
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

      INTEGER DATE(4), YEAR,MONTH,DAY,HOUR, NOW(9), TOL,RC
      INTEGER BCENDY,RCENDY, BCENHR,RCENHR  ! CENTURY DAYS AND HOURS
      INTEGER NCENDY                                               !1.3
      INTEGER NCENHR                                               !1.3
      INTEGER MAXHRS   ! Max allowed report-system time difference !2

      CHARACTER MIMJ*4
      CHARACTER*80 HEAD                                            !2

      LOGICAL VALDDY      ! logical date check function            !1.3

      HEAD = '$Workfile: obhour.f$ ' //
     &       '$Revision: 2$ $Date: 31/08/2010 15:06:45$'

      RCENHR=0                                                     !1.4

! SET YEAR & MONTH PROVISIONALLY FROM CURRENT DATE/TIME, BEFORE
! ADJUSTING THEM IF NECESSARY TO CALCULATE CENTURY-DAY.

      CALL DATIM(NOW)
      YEAR=NOW(8)
      MONTH=NOW(7)

      CALL DATE31(NOW(6),MONTH,YEAR,NCENDY)                        !1.3
      NCENHR=(NCENDY-1)*24+NOW(5)            ! CURRENT CENTURY-HOUR

! IF IT IS OLD DATA THEN IF THE DAY IS GREATER THAN THE SYSTEM DATE
! THEN ASSUME IT IS FOR THE PREVIOUS MONTH.

      IF (MIMJ(1:3).EQ.'OLD') THEN
        IF (DAY.GT.NOW(6)) THEN                                    !1.4
          MONTH=MONTH-1
          IF (MONTH.EQ.0) THEN
            MONTH=12
            YEAR=YEAR-1
          ENDIF
        ENDIF
      ELSE

! IF THE CURRENT DAY OF THE MONTH AND THE DAY IN THE BULLETIN HEADER
! SUGGEST THE DATA IS FOR LAST MONTH OR NEXT MONTH, CHANGE THE MONTH.
! AND GO BACK A MONTH IF THE BULLETIN DAY WOULD OTHERWISE BE FOR THE
! DAY AFTER TOMORROW OR LATER.

        IF (DATE(3).EQ.1 .AND. NOW(6).GE.28) THEN
          MONTH=MONTH+1
          IF (MONTH.EQ.13) THEN
            MONTH=1
            YEAR=YEAR+1
          ENDIF
        ELSE IF ((DATE(3).GE.28 .AND. NOW(6).EQ.1)
     &     .OR. DATE(3).GT.NOW(6)+1) THEN
          MONTH=MONTH-1
          IF (MONTH.EQ.0) THEN
            MONTH=12
            YEAR=YEAR-1
          ENDIF
        ENDIF

! WITH THE ADJUSTED MONTH WORK OUT THE CENTURY-HOUR

        CALL DATE31(DATE(3),MONTH,YEAR,BCENDY)                     !1.3
        BCENHR=(BCENDY-1)*24+DATE(4)

! NOW DO THE SAME FOR THE SECOND TIME (CALL IT THE REPORT TIME)

        YEAR=NOW(8)
        MONTH=NOW(7)

        IF (DAY.EQ.1 .AND. NOW(6).GE.28) THEN
          MONTH=MONTH+1
          IF (MONTH.EQ.13) THEN
            MONTH=1
            YEAR=YEAR+1
          ENDIF
        ELSE IF ((DAY.GE.28 .AND. NOW(6).EQ.1)
     &     .OR. DAY.GT.NOW(6)+1) THEN
          MONTH=MONTH-1
          IF (MONTH.EQ.0) THEN
            MONTH=12
            YEAR=YEAR-1
          ENDIF
        ENDIF

        CALL DATE31(DAY,MONTH,YEAR,RCENDY)                         !1.3
        RCENHR=(RCENDY-1)*24+HOUR
      ENDIF

! IF THE REPORT IS MORE THAN AN HOUR EARLY (3 HOURS FOR TROPADV    !2
! REPORTS), DON'T TRUST ITS TIME.  IF THE CENTURY-HOURS ARE NO     !2
! MORE THAN THE TOLERANCE APART, ACCEPT THE DAY/HOUR FROM THE      !2
! REPORT.  OTHERWISE SET ERROR RETURN CODE.                        !2

      MAXHRS = 1                                                   !2
      IF (MIMJ.EQ.'TROP') MAXHRS = 3    ! TROPADV reports          !2

      IF (RCENHR.GT.NCENHR+MAXHRS) THEN                            !2
        WRITE (*,1) DAY,HOUR, NOW(6),NOW(5), MIMJ
    1   FORMAT (I5.2,'/',I2.2,'Z REPORT AT',I3.2,'/',I2.2,'Z (IN ',
     &          A4,' BULLETIN)')
        RC=4
      ELSE IF (MIMJ(1:3).EQ.'OLD'. OR. ABS(RCENHR-BCENHR).LE.TOL) THEN
        DATE(1)=YEAR
        DATE(2)=MONTH
        DATE(3)=DAY
        DATE(4)=HOUR
        RC=0
        IF (.NOT.VALDDY(DAY,MONTH,YEAR)) THEN                      !1.3
          RC=4                                                     !1.3
          WRITE (*,3) DAY,MONTH,YEAR                               !1.3
    3     FORMAT (I5.2,'/',I2.2,'/',I4.4,' :  INVALID DATE')       !1.3
        ENDIF                                                      !1.3

      ELSE
        WRITE (*,2) DAY,HOUR, DATE(3),DATE(4), MIMJ
    2   FORMAT (I5.2,'/',I2.2,'Z REPORT IN',I3.2,'/',I2.2,'Z ',
     &          A4,' BULLETIN')
        RC=4
      ENDIF
      RETURN
      END
