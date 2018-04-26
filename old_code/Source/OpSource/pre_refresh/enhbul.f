      SUBROUTINE ENHBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR,
     &                  CORNUM,MIMJ,NFTENH,BULL)                    !1.4

!-----------------------------------------------------------------------
!
! PROGRAM       : ENHBUL
!
! PURPOSE       : TO IDENTIFY INDIVIDUAL ENHANCED SAWS REPORTS WITHIN
!                 A BULLETIN AND EXTRACT RELEVANT HEADER
!                 INFORMATION TO FORM AN INDEX. REPORT IS STORED WITH
!                 CHARACTER REPORT AND BUFR STRING OF ELEMENTS.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED,OBHOUR,MTRLOC,STAPOS,ENHEXP,DATIM,ENBUFR
!                 CCCODE,NCMIND,TAFREP
!
! PARAMETERS    : POINT   - CURRENT POSITION IN BULLETIN
!                          (BEFORE STN ID IN FIRST REPORT
!                           UNLESS AN 'OLD' REPORT)
!                 BULEND  - END OF BULLETIN (BEFORE NNNN)
!                 TTAAII  - WILL BE SCUK83
!                 CCCC    - COLLECTING CENTRE
!                 YYGGGG  - DAY AND HOUR OF REPORT
!                 OCOR    - CORRECTED REPORT FLAG
!                 CORNUM  - CORRECTION NUMBER
!                 MIMJ    - 'OLDE' IF AN OLD REPORT                 !1.4
!                 NFTENH  - FT NUMBER FOR ENHANCED SAWS DATASET (23)
!                 BULL    - BULLETIN
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:22$
! $Source: /home/us0400/mdb/op/lib/source/RCS/enhbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:22    Sheila Needham  
! $
! Revision 2.1  2001/07/03 13:44:58  usmdb
! Removed argument IDENT from call to NCMIND as not used in
! NCMIND. Variable IDENT deleted as not used - S.Cox
!
! Revision 2.0  2001/05/31  13:27:44  13:27:44  usmdb (Generic MetDB account)
! Removed unused variable. Added copyright and modified
! header - S.Cox
! 
! Revision 1.5  2001/03/07  11:59:02  11:59:02  usmdb (Generic MetDB account)
! 19 March 2001    C Long
! 1.5  Encode with new sequence to preserve radiation precision
! 
! Revision 1.4  2001/01/15  10:34:57  10:34:57  usmdb (Generic MetDB account)
! Change date: 22JAN01  R Hirst
! Enable handling of 'OLD' ESAWS reports. Addition of argument MIMJ.
!
! Revision 1.3  99/02/11  11:54:30  11:54:30  usmdb (Generic MDB account
! 15th February 1999 John Norton
! Stop program calling STAPOS when WMO block number is 99.
! Set Lat,Long and Stn Height variables in this program so
! they do not have to be passed to ENHEXP.
!
! Revision 1.2  97/10/23  12:45:26  12:45:26  usjl (Jon Lewthwaite)
! TYPE OF EXPARR CHANGED FROM INTEGER TO REAL
!
! Revision 1.1  1997/09/25 15:47:18  usjl
! Initial revision
!
! 01/10/97    TYPE OF EXPARR CHANGED FROM INTEGER TO REAL.         JN !B
!
! 29/09/97    INTRODUCED OPERATIONALLY
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

!----- variable declarations

!-----------------------------------------------------------------------        
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
!     TOL       - tolerance variable from obhour call
!     TOR(5)    - time of receipt (1)=year,(2)=month,(3)=day
!                               (4)=hour,(5)=minute
!-----------------------------------------------------------------------        

      CHARACTER*(*)      BULL
      CHARACTER*(*)      CCCC
      CHARACTER*(*)      CORNUM
      CHARACTER*23       ENTRY
      CHARACTER*4096     REPORT
      CHARACTER*5        STNID
      CHARACTER*(*)      TTAAII
      CHARACTER*(*)      YYGGGG
      CHARACTER*10000    MESSAG
      CHARACTER*132      HEAD
      CHARACTER*7        MIMJ                                       !1.4

      REAL               LAT
      REAL               LON
      REAL               STNHT
      REAL               STNHTP

      INTEGER            ICCCC             ! collecting centre code
      INTEGER            IDES              ! ?
      INTEGER            DESCR(500)        ! descriptor array
      INTEGER            NDES              ! number of descriptors
      INTEGER            NELEM             ! number of input elements
      INTEGER            NOBS              ! number of obs
      INTEGER            LEN               ! length of bufr string
      LOGICAL            CMPRES            ! BUFR compress flag

      INTEGER            BLKSIZ
      INTEGER            POINT
      INTEGER            POS               ! FIRST REPORT DATA GROUP
      INTEGER            BULEND
      INTEGER            I
      INTEGER            IRC
      INTEGER            NFTENH
      INTEGER            NOW(9)
      INTEGER            NUM_VALS         !number of data values in rpt
      INTEGER            OBDATE(5)
      INTEGER            OBDAY
      INTEGER            OBHR
      INTEGER            OBMIN
      INTEGER            RC
      INTEGER            TOL
      INTEGER            TOR(5)
      INTEGER            IVALUE
      INTEGER            REPLEN
      INTEGER            GRPLEN
      INTEGER            REPEND
      REAL               EXPARR(30)                                 !b

      LOGICAL            OCOR
      LOGICAL            LREPFL
      LOGICAL            SYNTAX

      REAL               RMISS              ! real missing data value
      DATA               RMISS/-9999999./
      DATA               CMPRES/.FALSE./
      DATA               TOL/0/


!----- initialisation
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/enhbul.F,v $
     &'//'$ $Date: 30/01/2006 20:22:22$ $Revision: 1$'
      BLKSIZ=27998

      DO I=1,5                   ! INITIALISES TIME OF
        TOR(I)=0                 ! RECEIPT ARRAY ELEMENTS
      END DO                     ! 1 TO 5.

!     PRINT*,' ENHBUL 1 >',BULL(1:BULEND),'<'                !TEST LINE


!----- 'Bulled' will edit bulletin of unnecessary characters and return
!      two POINTers. the first to the start of the report and the second
!      to the last character before 'NNNN' end of message.

      CALL BULLED(POINT,BULEND,BULL)

!diagnostics
c     WRITE (6,'(/)')                                        !test line
c     WRITE (6,*) 'start POINT/bulend - ',POINT,BULEND       !test line
c     WRITE (6,*)
c    & 'bulletin data ',BULEND,' ->',BULL(POINT:BULEND),'<'  !test line

!----- day and time of observation is YYGGgg
!      need to identify month and year of observation
!      call to routine obhour to return date
!      - allows for last months/years data

!       convert character form of day and hour to integer form
!       also extract minutes of observation for report header
!       (if not an 'OLD' report)                                    !1.4

        IF (MIMJ(1:4) .NE. 'OLDE') THEN                             !1.4
           READ (YYGGGG(1:2),'(I2)') OBDAY
           READ (YYGGGG(3:4),'(I2)') OBHR
           READ (YYGGGG(5:6),'(I2)') OBMIN

!----- If an 'OLD' report, get day/hour from group after 'OLDE'     !1.4
!----- and move on to start of report ie. the station number.       !1.4
!----- Old reports are expected to arrive one per bulletin.         !1.4
!----- 'Point' is at the space after 'OLDE'.                        !1.4
!----- This routine can only cope with one 'OLDE' & date/time       !1.4
!----- per bulletin.                                                !1.4

        ELSE                                                        !1.4
           OBDAY=IVALUE(BULL(POINT+1:POINT+2))                      !1.4
           OBHR =IVALUE(BULL(POINT+3:POINT+4))                      !1.4
           OBMIN=0                                                  !1.4
           POINT=POINT+6                                            !1.4
        ENDIF                                                       !1.4

        OBDATE(3)=OBDAY
        OBDATE(4)=OBHR
        OBDATE(5)=OBMIN

        CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)                  !1.4
        IF (RC.EQ.4) RETURN


!----- call system clock to get time of receipt

        CALL DATIM(NOW)
        TOR(1)=NOW(8)     ! YEAR
        TOR(2)=NOW(7)     ! MONTH
        TOR(3)=NOW(6)     ! DAY
        TOR(4)=NOW(5)     ! HOUR
        TOR(5)=NOW(4)     ! MINUTE


!----- loop round reports in bulletin until end of bulletin reached
!

      DO WHILE (POINT.LT.BULEND)                       ! reports loop


!diagnostics
!     WRITE (6,*) 'current POINT in report - ',POINT

!----- advance POINTer over any spaces before start of report if any

        DO WHILE (BULL(POINT:POINT).EQ.' ')
          POINT=POINT+1
        ENDDO

!----- check position of POINT to see if there is enough length left
!      for another report.

        IF ((BULEND-POINT).GE.7) THEN                ! long enough?
           REPLEN=INDEX(BULL(POINT:BULEND),'=')-1
           IF (REPLEN.LE.0) RETURN
           REPEND=POINT+REPLEN-1
           REPORT=BULL(POINT:REPEND)

!----- set syntax flag to false for report

           SYNTAX=.FALSE.
           LAT= RMISS
           LON= RMISS
           STNHT= RMISS
           STNHTP= RMISS

*************************************************************
*
*      OBTAIN THE IDENTIFIER FOR THE REPORT BY CHECKING THE LENGTH OF
*      THE FIRST GROUP IF 3 CHAR THEN BLOCK 03 IS ASSUMED TO BE MISSING.
*
*************************************************************

          POS=1
          CALL MTRLOC(REPLEN,REPORT,POS,GRPLEN,LREPFL)
          IF (GRPLEN.EQ.5 ) THEN
             STNID(1:5)=REPORT(1:5)
             POS=7
          ELSEIF (GRPLEN.EQ.3 ) THEN
             STNID(3:5)=REPORT(1:3)
             STNID(1:2)='03'
             POS=5
          ENDIF

c     PRINT*,' ENHBUL REPORT ',OBDATE,STNID,'>',BULL(POINT:REPEND),'<'


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!there are two calls to stapos. The first looks up the requested      !
!station in the surface list. If the requested station is not found   !
!then a second call to stapos looks up the 'x' list. This list contains
!stations that have not been identified as being upperair/surface.    !
!If after the second call the requested station details are still not !
!available then a message is output to warn of the situation          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          IF(STNID(1:2).EQ.'99')THEN                               !1.3
            LAT=RMISS                                              !1.3
            LON=RMISS                                              !1.3
            STNHTP=RMISS                                           !1.3
            STNHT=RMISS                                            !1.3
          ELSE                                                     !1.3
            CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,STNHT,IRC)

            IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN
              IRC=0
              CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,STNHT,IRC)
            ENDIF

            IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN
              IRC=0
              CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,STNHT,IRC)
            ENDIF

            IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN
            WRITE(6,*)' ESAWS (ENHBUL)--> STATION NOT FOUND IN STAPOS ',
     &                 STNID
            ENDIF
          ENDIF                                                    !1.3


          CALL ENHEXP(REPORT,REPLEN,POS,EXPARR,STNID,OBDATE,NUM_VALS,
     &                SYNTAX)

*                                                                  !1.3
* LATITUDE                                                         !1.3
*                                                                  !1.3
      EXPARR(8)=LAT                                                !1.3
*                                                                  !1.3
* LONGITUDE                                                        !1.3
*                                                                  !1.3
      EXPARR(9)=LON                                                !1.3
*                                                                  !1.3
* HEIGHT                                                           !1.3
*                                                                  !1.3
      IF (STNHTP.NE.RMISS.AND.STNHT.EQ.RMISS) STNHT=STNHTP         !1.3
      EXPARR(10)=STNHT                                             !1.3


***********************************************************************
*
* CALL BUFR ENCODING ROUTINE, WITH CURRENT TIME AS TIME OF RECEIPT TO
* GO IN SECTION 1.  PUT REPORT IN OUTPUT STRING BEFORE BUFR MESSAGE.
*
***********************************************************************
          MESSAG(1:REPLEN)=REPORT

          DESCR(1)=IDES(307225)                                    !1.5
          NDES=1
          NELEM=30
          NOBS=1

          CALL ENBUFR(DESCR,EXPARR,NDES,NELEM,NOBS,STNID,TOR,
     &                    MESSAG(REPLEN+1:),CMPRES,LEN)
*
* SET CCCC & LAND/SEA BUFR DATA TYPE IN SECTION 1 OF THE BUFR MESSAGE
* (DISPLACEMENT AS FOR BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)
*
          IF (CCCC.NE.'    ') CALL CCCODE(287,ICCCC,CCCC)
          MESSAG(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
          MESSAG(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))

!----- index type - 23byte.
!      call to ncmind to compile index entry

          CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,OBHR,OBMIN,     !2.1
     &                LAT,LON)

!      Set a value which can be used to determine preferred report. In
!      this report type the numbers of data values in report is used.

          ENTRY(12:12)=CHAR(NUM_VALS)

!      Set bit 5 of byte 17 of index entry to indicate there is a
!      syntactical error found when report expanded if syntax=.true..

      IF (SYNTAX) THEN
        ENTRY(17:17)=CHAR(64)                     ! FLAG IF SYNTAX ERROR
      ELSE
        ENTRY(17:17)=CHAR(0)                      ! CLEAR BYTE IF NOT
      ENDIF

c     PRINT*,' ENHBUL ENTRY1 >',ENTRY,'<'
c     PRINT*,' ENHBUL MESSAG >',MESSAG(1:REPLEN+LEN),'<'

!----- finally store report
!      call to tafrep

!diagnostics

C#    PRINT*,' ENHBUL ',STNID,' REPORT LENGTHS = ',REPLEN,LEN,
C#   &       ' TOTAL LENGTH = ',(REPLEN+LEN)  !TESTLINE

          CALL TAFREP(OBDATE,ENTRY,MESSAG(1:REPLEN+LEN),
     &                  NFTENH,BLKSIZ,STNID)


! move pointer on to next report

          POINT=REPEND+2

!diagnostics
!     WRITE (6,*) 'pre check report - ',REPORT

        ELSE                                         ! long enough?
          POINT=BULEND
        ENDIF                                        ! long enough?

      ENDDO                                            ! reports loop


      RETURN
      END
