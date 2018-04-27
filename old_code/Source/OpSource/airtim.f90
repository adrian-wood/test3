SUBROUTINE AIRTIM(REPLEN,REPORT,POINT,TIMEDD,TIMEYY,TIMEMNTH,&
TIMEHH,TIMEMM,YYGGGG,DECERR)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRTIM
!
! PURPOSE       : TO DECODE TIME GROUP IN AIREP REPORT
!
! DESCRIPTION   : THE RELEVANT PART OF THE AIREP REPORT IS CUT INTO
!                 DAY HOUR AND MINUTE. THESE ARE OBTAINED FROM THE
!                 REPORT AND PASSED BACK TO AIRARP
!
! CALLED BY     : AIRARP
!
! CALLS TO      : AIRLOC
!                 AIRGRP
!                 DATIM
!                 ZPDATE
!
!
!
! PARAMETERS    : 1. REPLEN - LENGTH OF REPORT - I
!                 2. REPORT - REPORT BEING EXPANDED - I
!                 3. POINT - POINTER WITHIN REPORT -I/O
!                 4. TIMEYY -  YEAR - O
!                 5. TIMEMTH - MONTH- O
!                 6. TIMEDD  - DAY - O
!                 7. TIMEHH - TIME IN HOURS- O
!                 8. TIMEMM - TIME IN MINUTES - O
!                 9. YYGGGG - BULLETIN DATE/TIME - I
!                10. DECERR - DECODE ERROR FLAG -O
!
!
! REVISION INFO:
!
! $Workfile: airtim.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 25/01/2011 16:49:45$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         25/01/2011 16:49:45    Richard Weedon
!       identing tarted up
!  4    MetDB_Refresh 1.3         25/01/2011 16:47:57    Richard Weedon  intent
!        for DECERR changed to INOUT
!  3    MetDB_Refresh 1.2         25/01/2011 12:34:46    Richard Weedon  intent
!        added
!  2    MetDB_Refresh 1.1         17/01/2011 11:16:57    Richard Weedon
!       updated for calls to airloc, airgrp, datim, date31 & date13.
!  1    MetDB_Refresh 1.0         13/01/2011 15:25:25    Richard Weedon
!       Initial version, passes basic compilation test
! $
!
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
!Interface statements
USE airloc_mod
USE airgrp_mod
USE datim_mod
USE zpdate_mod
!
IMPLICIT NONE
!
! Paramaters
!
INTEGER,INTENT(IN)             ::  REPLEN   !report length
CHARACTER(LEN=*),INTENT(IN)    ::  REPORT   !size passed on from ai
INTEGER,INTENT(INOUT)          ::  POINT    !position within group
REAL,INTENT(OUT)               ::  TIMEDD   !extract day-time
REAL,INTENT(OUT)               ::  TIMEHH   !extract hour-time
REAL,INTENT(OUT)               ::  TIMEMM   !extract minute-time
REAL,INTENT(OUT)               ::  TIMEYY   !Year
REAL,INTENT(OUT)               ::  TIMEMNTH !Month
CHARACTER(LEN=6),INTENT(IN)    ::  YYGGGG   !day/time from bulletin
INTEGER,INTENT(INOUT)          ::  DECERR   !Decode error flag

!declare characters
CHARACTER(LEN=20)       ::  CHARR    !dummy array initialise as blank
!
!declare integers
INTEGER                 ::  GRPLEN   !length of group within report
INTEGER                 ::  NCHAR    !no. of non-numerics in group
INTEGER                 ::  HED_YEAR !calulated year for header
INTEGER                 ::  HED_MNTH !calulated month for header
INTEGER                 ::  HED_DAY  !day in bulletin heading
INTEGER                 ::  HED_HOUR !hour in bulletin heading
INTEGER                 ::  HED_MIN  !minute in bulletin heading
INTEGER                 ::  CENDAY   !century day
INTEGER                 ::  ID
INTEGER                 ::  IM
INTEGER                 ::  IY
INTEGER                 ::  NOW(8) !system date/time array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!now(8)=year,now(7)=month,now(6)=day,now(5)=hour,now(4)=mins !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!declare logical
LOGICAL LREPFL         !indicates end of report if = .true.

SAVE
!initialise variables
GRPLEN=0                 !set initial grouplength =0
CHARR=' '
LREPFL=.FALSE.           !set end of report to false

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!This section is for the extraction of the time groups from the      !
!airep report. A time group in an airep may consist of the following !
!format DDHHMM or HHMM, where DD is Day,HH is Hour and MM is minute. !
!The porgram searches to see what group type it has. We are then able!
!to  remove  the component parts of the group for storage. Note:Each !
!component (DD,HH or MM) does have its own BUFR descriptor.          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

CALL AIRLOC(REPLEN,REPORT,POINT,GRPLEN,LREPFL)
CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-1),&
     GRPLEN,NCHAR,CHARR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check the return of LREPFL from AIRLOC to see if the end of the report!
!has been reached.                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

IF (LREPFL) THEN
  DECERR=1
END IF

if_constr1 : &
IF (DECERR == 0) THEN

  CALL DATIM(NOW)              !get system date/time

! Get header date/time

  READ(YYGGGG(1:2),'(I2)') HED_DAY
  READ(YYGGGG(3:4),'(I2)') HED_HOUR
  READ(YYGGGG(5:6),'(I2)') HED_MIN

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calcuate the month/year of header day                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! it is assumed that the header time is quite reliable and reports will
! not be received more than a fews days late (note they may then be
! rejected by storage).  Also assumes system clock is correct.

! initially set year as per system

  HED_YEAR=NOW(8)

! if date is less than or equal to system then assume same month

  IF (HED_DAY <= NOW(6)) THEN
    HED_MNTH=NOW(7)
  ELSE

!  otherwise assume it is in the previous month - then do a check
!   on the year in case we have crossed the New Year.

    HED_MNTH=NOW(7)-1
    IF (HED_MNTH == 0) THEN
      HED_MNTH=12
      HED_YEAR=NOW(8)-1
    END IF
  END IF

!  carry out a logic check on header date/time

  IF ((HED_MNTH < 1).OR.(HED_MNTH > 12)&
     .OR.(HED_DAY < 1).OR.(HED_DAY > 31)&
     .OR.(HED_HOUR < 0).OR.(HED_HOUR > 23)&
     .OR.(HED_MIN < 0).OR.(HED_MIN > 59))&
     THEN
    DECERR=1       !corrupt header
  END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this section gets the HHMM time group information                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if_constr2 : &
  IF ((GRPLEN  ==  4) .AND. (NCHAR  ==  0)) THEN
    READ (REPORT(POINT-5:POINT-4),'(F2.0)')TIMEHH
    READ (REPORT(POINT-3:POINT-2),'(F2.0)')TIMEMM

! logic check on time

    IF ((TIMEHH  >  23) .OR. (TIMEMM  >  59) .OR. (TIMEHH < &
     00) .OR. (TIMEMM  <  00)) THEN

      DECERR=1                      !corrupt HHMM group
    END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!if time less than header assume headers year,month,day else          !
!if hours greater than current assume previous days year/month/day    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    IF(DECERR == 0) THEN

      IF (TIMEHH  <=  HED_HOUR .OR. (TIMEHH == HED_HOUR .AND.&
       (TIMEMM  <=  HED_MIN))) THEN   !assume header day

        TIMEYY=HED_YEAR
        TIMEMNTH=HED_MNTH
        TIMEDD=HED_DAY               !take header date

      ELSE

!  otherwise assume previous day

        CALL DATE31(HED_DAY,HED_MNTH,HED_YEAR,CENDAY)

        CENDAY=CENDAY-1
        CALL DATE13(CENDAY,ID,IM,IY)
        TIMEYY=IY                   !day before header year

        TIMEMNTH=IM                 !----"----- month
        TIMEDD=ID                   !----"----- day
      END IF
    END IF            !for HHMM group



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this section gets the DDHHMM time group information                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ELSE IF ((GRPLEN ==  6) .AND. (NCHAR  ==  0)) THEN
    READ(REPORT(POINT-7:POINT-6),'(F2.0)')TIMEDD
    READ(REPORT(POINT-5:POINT-4),'(F2.0)')TIMEHH
    READ(REPORT(POINT-3:POINT-2),'(F2.0)')TIMEMM

! logic check on date/time

    IF ((TIMEHH  >  23) .OR. (TIMEMM  >  59) .OR. (TIMEHH < &
     00) .OR. (TIMEMM  <  00) .OR. (TIMEDD  >  31) .OR. (TIMEDD&
     <  01)) THEN
      DECERR=1                   !corrupt DDHHMM group
    END IF


! need to assign month and year to bulletin date.  Assume report must
! be older than bulletin header.  Except when day is the same - when
! can be newer -> forecast so ignore.

    if_constr3 : &
    IF(DECERR == 0) THEN

      TIMEYY=HED_YEAR          !set year
      IF (TIMEDD <= HED_DAY) THEN
! set as present month is date is less
        TIMEMNTH=HED_MNTH
     ELSE
     TIMEMNTH=HED_MNTH-1
      IF (TIMEMM == 0) THEN
        TIMEMNTH=12
        TIMEYY=TIMEYY-1
     END IF
 END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  check if time id greater than header - if so reject              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF ((TIMEDD == HED_DAY).AND.((TIMEHH > HED_HOUR).OR.&
       ((TIMEHH == HED_HOUR).AND.(TIMEMM > HED_MIN)))) THEN
        DECERR=1
 WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM,&
     'MIN REPORT- HEADER ',YYGGGG
      END IF

! if neither HHMM or DDMMNN conditions met then assume corrupt message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  check if time id greater than system - if so reject              !
!  extra check added to help correct end month prob.                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (TIMEMNTH == NOW(7).AND.TIMEDD > NOW(6)) THEN
           DECERR=1
           WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM,&
           'MIN REPORT- HEADER ',YYGGGG
      END IF

      IF((TIMEDD == NOW(6)).AND.((TIMEHH > NOW(5)).OR.&
         ((TIMEHH == NOW(5)).AND.(TIMEMM > NOW(4))))) THEN
            DECERR=1
            WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM,&
            'MIN REPORT- HEADER ',YYGGGG
      END IF

    END IF if_constr3

  ELSE
    DECERR=1
  END IF if_constr2 !end of checking for HHMM or DDHHMM

END IF if_constr1  !decerr for end of report


RETURN               !return to calling routine
END SUBROUTINE AIRTIM
