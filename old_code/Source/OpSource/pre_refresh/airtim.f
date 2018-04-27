      SUBROUTINE AIRTIM(REPLEN,REPORT,POINT,TIMEDD,TIMEYY,TIMEMNTH,
     &                  TIMEHH,TIMEMM,YYGGGG,DECERR)

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
! CALLS TO      : NONE                                                  
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
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:47$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airtim.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:47    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:25  usmdb
! Changed LOGICAL*4 tto LOGICAL for declaration of LREPFL. Added
! copyright and modified header - S.Cox
!
! Revision 1.4  98/10/15  11:07:05  11:07:05  usmdb (Generic MetDB account)
! 19/10/1998 emergency change for TOMS to attempt to fix end month problem. 
! 
! Revision 1.3  98/04/20  06:56:35  06:56:35  usmdb (Generic MDB account)
! Improve Date/Time handling.
!
! 09/03/1998  Make changes to time allocation so time/date is
!             obtained from the header and not the system (usjh)      !a
!
! Revision 1.2  97/07/31  09:08:40  09:08:40  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 10:36:41  uspm
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

!declare characters
      CHARACTER REPORT*(*)     !size passed on from airarp
      CHARACTER CHARR*20       !dummy array initialise as blank
      CHARACTER*6 YYGGGG       !day/time from bulletin
      CHARACTER*132 HEAD       !revision information

!declare integers
      INTEGER REPLEN           !report length
      INTEGER GRPLEN           !length of group within report
      INTEGER POINT            !position within group in report
      INTEGER NCHAR            !no. of non-numerics in group
      INTEGER DECERR           !decode error flag
      INTEGER HED_YEAR         !calulated year for header              !a
      INTEGER HED_MNTH         !calulated month for header             !a
      INTEGER HED_DAY          !day in bulletin heading                !a
      INTEGER HED_HOUR         !hour in bulletin heading               !a
      INTEGER HED_MIN          !minute in bulletin heading             !a
      INTEGER CENDAY           !century day                            !a

      INTEGER ID
      INTEGER IM
      INTEGER IY
      INTEGER NOW(8)           !system date/time array
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!now(8)=year,now(7)=month,now(6)=day,now(5)=hour,now(4)=mins !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!declare real
      REAL    TIMEDD           !extract day-time
      REAL    TIMEHH           !extract hour-time
      REAL    TIMEMM           !extract minute-time
      REAL    TIMEYY           !Year
      REAL    TIMEMNTH         !Month

!declare logical
      LOGICAL LREPFL         !indicates end of report if = .true.   !2.0

      SAVE
!initialise variables
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airtim.F,v $
     &'//'$ $Date: 30/01/2006 20:20:47$ $Revision: 1$'

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
      CALL AIRGRP(REPORT(POINT-GRPLEN-1:POINT-1),
     &GRPLEN,NCHAR,CHARR)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!check the return of LREPFL from AIRLOC to see if the end of the report!
!has been reached.                                                     !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF (LREPFL) THEN
        DECERR=1
      ENDIF

      IF (DECERR.EQ.0) THEN

        CALL DATIM(NOW)              !get system date/time

! Get header date/time

        READ(YYGGGG(1:2),'(I2)') HED_DAY                      !a
        READ(YYGGGG(3:4),'(I2)') HED_HOUR                     !a
        READ(YYGGGG(5:6),'(I2)') HED_MIN                      !a

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Calcuate the month/year of header day                               !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! it is assumed that the header time is quite reliable and reports will
! not be received more than a fews days late (note they may then be
! rejected by storage).  Also assumes system clock is correct.

! initially set year as per system

        HED_YEAR=NOW(8)                                     !a

! if date is less than or equal to system then assume same month

        IF (HED_DAY.LE.NOW(6)) THEN                         !a
          HED_MNTH=NOW(7)                                   !a
        ELSE                                                !a

!  otherwise assume it is in the previous month - then do a check
!   on the year in case we have crossed the New Year.

          HED_MNTH=NOW(7)-1                                 !a
          IF (HED_MNTH.EQ.0) THEN                           !a
            HED_MNTH=12                                     !a
            HED_YEAR=NOW(8)-1                               !a
          ENDIF                                             !a
        ENDIF                                               !a

!  carry out a logic check on header date/time

        IF ((HED_MNTH.LT.1).OR.(HED_MNTH.GT.12)             !a
     &    .OR.(HED_DAY.LT.1).OR.(HED_DAY.GT.31)             !a
     &    .OR.(HED_HOUR.LT.0).OR.(HED_HOUR.GT.23)           !a
     &    .OR.(HED_MIN.LT.0).OR.(HED_MIN.GT.59))            !a
     &  THEN
          DECERR=1       !corrupt header                   !a
        ENDIF                                              !a


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this section gets the HHMM time group information                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        IF ((GRPLEN .EQ. 4) .AND. (NCHAR .EQ. 0)) THEN
          READ (REPORT(POINT-5:POINT-4),'(F2.0)')TIMEHH
          READ (REPORT(POINT-3:POINT-2),'(F2.0)')TIMEMM

! logic check on time

          IF ((TIMEHH .GT. 23) .OR. (TIMEMM .GT. 59) .OR. (TIMEHH .LT.
     &     00) .OR. (TIMEMM .LT. 00)) THEN

            DECERR=1                      !corrupt HHMM group
          ENDIF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!if time less than header assume headers year,month,day else          !
!if hours greater than current assume previous days year/month/day    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          IF(DECERR.EQ.0) THEN

            IF (TIMEHH .LE. HED_HOUR .OR. (TIMEHH.EQ.HED_HOUR .AND. !a
     &     (TIMEMM .LE. HED_MIN))) THEN   !assume header day        !a

              TIMEYY=HED_YEAR                                       !a
              TIMEMNTH=HED_MNTH                                     !a
              TIMEDD=HED_DAY               !take header date        !a

            ELSE

!  otherwise assume previous day

              CALL DATE31(HED_DAY,HED_MNTH,HED_YEAR,CENDAY)         !a

              CENDAY=CENDAY-1
              CALL DATE13(CENDAY,ID,IM,IY)
              TIMEYY=IY                   !day before header year

              TIMEMNTH=IM                 !----"----- month
              TIMEDD=ID                   !----"----- day
            ENDIF
          ENDIF       ! decerr for HHMM group                      !a



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!this section gets the DDHHMM time group information                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        ELSEIF ((GRPLEN .EQ. 6) .AND. (NCHAR .EQ. 0)) THEN
          READ(REPORT(POINT-7:POINT-6),'(F2.0)')TIMEDD
          READ(REPORT(POINT-5:POINT-4),'(F2.0)')TIMEHH
          READ(REPORT(POINT-3:POINT-2),'(F2.0)')TIMEMM

! logic check on date/time

          IF ((TIMEHH .GT. 23) .OR. (TIMEMM .GT. 59) .OR. (TIMEHH .LT.
     &    00) .OR. (TIMEMM .LT. 00) .OR. (TIMEDD .GT. 31) .OR. (TIMEDD
     &    .LT. 01)) THEN
            DECERR=1                   !corrupt DDHHMM group
          ENDIF


! need to assign month and year to bulletin date.  Assume report must
! be older than bulletin header.  Except when day is the same - when
! can be newer -> forecast so ignore.

          IF(DECERR.EQ.0) THEN

            TIMEYY=HED_YEAR          !set year                      !a
            IF (TIMEDD.LE.HED_DAY) THEN                             !a
! set as present month is date is less
              TIMEMNTH=HED_MNTH
      ELSE                                                    !a
       TIMEMNTH=HED_MNTH-1                                   !a
       IF (TIMEMM.EQ.0) THEN                                 !a
         TIMEMNTH=12                                         !a
         TIMEYY=TIMEYY-1                                     !a
              ENDIF                                                 !a
       ENDIF                                                   !a


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  check if time id greater than header - if so reject              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            IF ((TIMEDD.EQ.HED_DAY).AND.((TIMEHH.GT.HED_HOUR).OR.
     &     ((TIMEHH.EQ.HED_HOUR).AND.(TIMEMM.GT.HED_MIN)))) THEN  !1.4
              DECERR=1
       WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM,
     &       'MIN REPORT- HEADER ',YYGGGG
            ENDIF

! if neither HHMM or DDMMNN conditions met then assume corrupt message

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  check if time id greater than system - if so reject              !
!  extra check added to help correct end month prob.                !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            IF (TIMEMNTH.EQ.NOW(7).AND.TIMEDD.GT.NOW(6)) THEN     !1.4
              DECERR=1                                            !1.4
              WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM, !1.4
     &        'MIN REPORT- HEADER ',YYGGGG                        !1.4
            ENDIF                                                 !1.4

            IF((TIMEDD.EQ.NOW(6)).AND.((TIMEHH.GT.NOW(5)).OR.     !1.4
     &      ((TIMEHH.EQ.NOW(5)).AND.(TIMEMM.GT.NOW(4))))) THEN    !1.4
              DECERR=1                                            !1.4
              WRITE(6,*) 'AIRTIM:FORECAST- ',TIMEHH,'HRS',TIMEMM, !1.4
     &        'MIN REPORT- HEADER ',YYGGGG                        !1.4
            ENDIF                                                 !1.4

          ENDIF

        ELSE
          DECERR=1                                              !a
        ENDIF          !end of checking for HHMM or DDHHMM  !a

      ENDIF                !decerr for end of report


      RETURN               !return to calling routine           !a
      END
