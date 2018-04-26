      SUBROUTINE SRWBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR,
     &                  CORNUM,MIMJ,NFTSRW,BULL)                    !1.6

!-----------------------------------------------------------------------
!
! PROGRAM     : SRWBUL
!
! PURPOSE     : TO IDENTIFY INDIVIDUAL SREW REPORTS WITHIN
!               A BULLETIN AND EXTRACT RELEVANT HEADER
!               INFORMATION TO FORM AN INDEX. REPORT IS STORED
!               IN CHARACTER FORMAT AFTER SOME QUALITY CONTROL CHECKS
!
! CALLED BY   : MDBSTOR
!
! CALLS
!        SUBROUTINES  : BULLED,STAPOS,DATIM,OBHOUR,NCMIND,TAFREP
!        FUNCTIONS    : OFIGTS
!
! PARAMETERS  : POINT   - CURRENT POSITION IN BULLETIN
!                        (BEFORE STN ID IN FIRST REPORT
!                         UNLESS AN OLD REPORT)
!               BULEND  - END OF BULLETIN (BEFORE NNNN)
!               TTAAII  - WILL BE SRUK95
!               CCCC    - COLLECTING CENTRE
!               YYGGGG  - DAY AND HOUR OF REPORT
!               OCOR    - CORRECTED REPORT FLAG
!               CORNUM  - CORRECTION NUMBER
!               MIMJ    - 'OLDR' IF AN OLD REPORT                   !1.6
!               NFTSRW  - FT NUMBER FOR SREW DATASET (15)
!               BULL    - BULLETIN
!
! REVISION INFO :
!
! $Workfile: srwbul.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 19/07/2007 15:10:10$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         19/07/2007 15:10:10    Brian Barwell   Some
!       code removed to allow locations to be stored for CDL data.
!       Commented-out diagnostics also removed & revision information tidied
!       up.
!  1    Met_DB_Project 1.0         30/01/2006 20:24:24    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:55  usmdb
! Removed argument STNID from call to NCMIND as not used in NCMIND.
! Added copyright and modified header - S.Cox
!
! Revision 1.6  2001/01/15  10:35:38  10:35:38  usmdb (Generic MetDB account)
! Change date: 22JAN01	R Hirst
! Enable handling of 'OLD' SREW reports.
! Addition of argument MIMJ.
!
! Revision 1.5  2000/04/07  09:24:44  09:24:44  usmdb (Generic MDB account)
! 17 April 2000      C Long
! 1.5  Accept 8-character SREWs from Gibraltar etc (not block 03)
!
! Revision 1.4  99/02/11  11:52:06  11:52:06  usmdb (Generic MDB account)
! 15th February 1999 John Norton
! Add handling of CDL reports with 8 characters and remove
! call to STAPOS when CDL report processed.
!
! Revision 1.3  98/08/12  08:33:25  08:33:25  usmdb (Generic MDB account)
! Check all three STAPOS types and only output message if station
! not found in STAPOS. J Norton.                                      !c
!
! Revision 1.2  97/07/31  11:34:30  11:34:30  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:27:36  uspm
! Initial revision
!
! 10/08/96    only print one line if bulletin is bad                  !b
!
! 05/06/96    REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY A NEW
!             PARAMETER 'BULL'.
!
! MAR 96: GET POSITION FROM STAPOS RATHER THAN MDB                    !A
!
! INTRODUCED  : MAY 95
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2007 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

!-------------------------------------------------------------
!
!     VARIABLE EXPLANATIONS
!    ***********************
!
!     BLKSIZ    - blocksize required to store data (passed to tafrep)
!     ENTRY     - 23 byte index
!     I         - general loop variable for time of receipt array
!     LAT,LON   - lat and lon of station, extracted by calling
!                 station master
!     OBDATE(5) - date and time of observation
!                        (1)=year,(2)=month,(3)=day,(4)=hour,(5)=min
!     OBDAY,OBHR- day and hour of observation contained in
!                 bulletin heading
!     QC        - quality control flag to discard bulletins
!     RC        - return code from obhour call
!     REPORT    - report to be stored
!     STNID     - station identifier - this will be reported in
!                 3 figures and padded out to 5 with block number
!     STORE     - flag to store reports if true
!                 neither completely numeric or iiiotr
!     TOL       - tolerance variable from obhour call
!     TOR(5)    - time of receipt (1)=year,(2)=month,(3)=day
!                               (4)=hour,(5)=minute
!
!
!------------------------------------------------------------

!----- variable declarations

      CHARACTER*(*)      BULL
      CHARACTER*(*)      CCCC
      CHARACTER*(*)      CORNUM
      CHARACTER*23       ENTRY
      CHARACTER*9        REPORT
      CHARACTER*5        STNID
      CHARACTER*(*)      TTAAII
      CHARACTER*(*)      YYGGGG
      CHARACTER*80       HEAD                                        !2
      CHARACTER*7        MIMJ                                       !1.6

      REAL               LAT
      REAL               LON
      REAL               RMISS             ! real missing data value !c
      REAL               STNHT                                       !a
      REAL               STNHTP                                      !a

      INTEGER            BLKSIZ
      INTEGER            POINT
      INTEGER            BULEND
      INTEGER            I
      INTEGER            IRC
      INTEGER            NFTSRW
      INTEGER            NOW(9)
      INTEGER            OBDATE(5)
      INTEGER            OBDAY
      INTEGER            OBHR
      INTEGER            OBMIN
      INTEGER            RC
      INTEGER            TOL
      INTEGER            TOR(5)
      INTEGER            IVALUE

      LOGICAL            FIRST             ! Flag for first call     !2
      LOGICAL            OCOR
      LOGICAL            OFIGTS            ! function
      LOGICAL            QC
      LOGICAL            STORE

      DATA               FIRST/.TRUE./                               !2
      DATA               RMISS/-9999999./                            !c

!----- initialisation

      IF (FIRST) THEN                                                !2
        HEAD = '$Workfile: srwbul.f$ ' //
     &         '$Revision: 2$ $Date: 19/07/2007 15:10:10$'
        FIRST = .FALSE.                                              !2
      END IF                                                         !2

      BLKSIZ=27998

      DO I=1,5                   ! INITIALISES TIME OF
        TOR(I)=0                 ! RECEIPT ARRAY ELEMENTS
      END DO                     ! 1 TO 5.

!----- 'Bulled' will edit bulletin of unnecessary characters and return
!      two pointers. the first to the start of the report and the second
!      to the last character before 'NNNN' end of message.

      CALL BULLED(POINT,BULEND,BULL)

!diagnostics
!     WRITE (6,'(/)')
!     WRITE (6,*) 'start point/bulend - ',POINT,BULEND
!     WRITE (6,*) 'bulletin data - ',BULL(POINT:BULEND)

!----- loop round reports in bulletin until end of bulletin reached
!      set 'store' to false before each report

      DO WHILE (POINT.LT.BULEND-5)                                  !1.4
        STORE=.FALSE.
        REPORT(1:)=' '                                              !1.4

!diagnostics
!     WRITE (6,*) 'current point in report - ',POINT

!----- advance pointer over any spaces before start of report if any

        DO WHILE (BULL(POINT:POINT).EQ.' ')
          POINT=POINT+1
        ENDDO

!----- If an 'OLD' report, get day/hour from group after 'OLDR',    !1.6
!----- call OBHOUR and move on to start of report ie. the station   !1.6
!----- number. Old reports are expected to arrive one per bulletin. !1.6
!----- 'Point' is at the start of the day/hour group.               !1.6
!----- This routine can only cope with one 'OLDR' and day/time      !1.6
!----- per bulletin.                                                !1.6

        IF (MIMJ(1:4).EQ.'OLDR') THEN                               !1.6
           OBDAY=IVALUE(BULL(POINT  :POINT+1))                      !1.6
           OBHR =IVALUE(BULL(POINT+2:POINT+3))                      !1.6
           OBMIN=0                                                  !1.6

           OBDATE(3)=OBDAY                                          !1.6
           OBDATE(4)=OBHR                                           !1.6
           OBDATE(5)=OBMIN                                          !1.6

           CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)               !1.6
           IF (RC.EQ.4) RETURN                                      !1.6

           POINT=POINT+5                                            !1.6
        ENDIF                                                       !1.6

!----- check position of point to see if there is enough length left
!      for another report.
!----- UK SREW reports are 6 characters long and CDL SREW reports   !1.4
!      are 8 characters long, but 8-character reports come from     !1.5
!      stations outside block 03 (Gibraltar etc) too, so accept     !1.5
!      either 8 characters or 6.                                    !1.5
                                                                    !1.4
        IF (BULL(POINT+6:POINT+6).EQ.' '.OR.                        !1.5
     &                        BULL(POINT+6:POINT+6).EQ.'=') THEN    !1.5
          REPORT(1:2)='03'                                          !1.5
          REPORT(3:8)=BULL(POINT:POINT+5)                           !1.5
          REPORT(9:)=' '                                            !1.5
          STORE=.TRUE.                                              !1.5
          POINT=POINT+7                                             !1.5
        ELSE IF (BULL(POINT+8:POINT+8).EQ.' '.OR.                   !1.5
     &                        BULL(POINT+8:POINT+8).EQ.'=') THEN    !1.5
          REPORT=BULL(POINT:POINT+7)                                !1.5
          STORE=.TRUE.                                              !1.5
          POINT=POINT+9                                             !1.5
        ELSE                                                        !1.4
          IF(BULEND.LT.112)THEN                                     !1.4
            WRITE (6,*)' SRWBUL BAD SREW BULLETIN: ',               !1.4
     &                   BULL(1:BULEND-1)                           !1.4
          ELSE                                                      !1.4
            WRITE (6,*)' SRWBUL BAD SREW BULLETIN: ',BULL(1:112)    !1.4
          ENDIF                                                     !1.4
          DO WHILE (BULL(POINT:POINT).NE.' ')                       !1.4
            POINT=POINT+1                                           !1.4
          ENDDO                                                     !1.4
        ENDIF                                                       !1.4


!diagnostics
!     WRITE (6,*) 'pre check report - ',REPORT

!----- if report cleared length check then check report is either
!      numeric or a trace or nil report.

        IF (STORE) THEN
          QC=OFIGTS(REPORT,1,8)                                     !1.4
          IF (.NOT.QC) THEN
            IF ((REPORT(7:8).EQ.'TR').OR.(REPORT(6:8).EQ.'NIL').OR. !1.4
     &         (REPORT(6:8).EQ.'OTR')) THEN                         !1.4
              STORE=.TRUE.
            ELSE
              STORE=.FALSE.
              WRITE (6,*) 'SRWBUL invalid report - not stored - ',  !1.4
     &                   REPORT                                     !1.4
            ENDIF
          ENDIF
        ENDIF

!----- If report passed content check then check for valid
!      station identifier (formerly non cdl stations only).          !2
!      Check extended to all stations for version 2, August 2007.    !2

        IF (STORE) THEN
          STNID(1:5)=REPORT(1:5)                                    !1.4

!----- call STAPOS to get lat/lon for this station

          CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,STNHT,IRC)
                                                                     !c
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !c
            IRC=0                                                    !c
            CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,STNHT,IRC)  !c
          ENDIF                                                      !c
                                                                     !c
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !c
            IRC=0                                                    !c
            CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,STNHT,IRC)  !c
          ENDIF                                                      !c
                                                                     !c
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !c
          WRITE(6,*)' SREW (SRWBUL)--> STATION NOT FOUND IN STAPOS ',
     &                 STNID                                         !c
          ENDIF                                                      !c c
        ENDIF

!----- now start storage process if all checks passed

        IF (STORE) THEN

!----- call system clock to get time of receipt

          CALL DATIM(NOW)
          TOR(1)=NOW(8)     ! YEAR
          TOR(2)=NOW(7)     ! MONTH
          TOR(3)=NOW(6)     ! DAY
          TOR(4)=NOW(5)     ! HOUR
          TOR(5)=NOW(4)     ! MINUTE

!----- day and time of observation is YYGGgg
!      need to identify month and year of observation
!      call to routine obhour to return date
!      - allows for last months/years data

!       convert character form of day and hour to integer form
!       also extract mintes of observation for report header.
!       If an 'OLD' report, all this has already been done.         !1.6

           IF (MIMJ(1:4).NE.'OLDR') THEN                            !1.6
             READ (YYGGGG(1:2),'(I2)') OBDAY
             READ (YYGGGG(3:4),'(I2)') OBHR
             READ (YYGGGG(5:6),'(I2)') OBMIN

             OBDATE(3)=OBDAY
             OBDATE(4)=OBHR
             OBDATE(5)=OBMIN

             CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)
             IF (RC.EQ.4) RETURN
           ENDIF                                                    !1.6

!----- index type - 23byte.
!      call to ncmind to compile index entry

          CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,OBHR,OBMIN,     !2.0
     &                LAT,LON)

!----- finally store report
!      call to tafrep

          CALL TAFREP(OBDATE,ENTRY,REPORT(1:8),NFTSRW,BLKSIZ,STNID)

        ENDIF               ! end of store report
      ENDDO                 ! end of conditional 'point' < 'bulend'

      RETURN
      END
