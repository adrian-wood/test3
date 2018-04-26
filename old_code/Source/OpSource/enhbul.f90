SUBROUTINE ENHBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR, &
                  CORNUM,MIMJ,NFTENH,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : ENHBUL
!
! PURPOSE       : TO IDENTIFY INDIVIDUAL ENHANCED SAWS REPORTS WITHIN
!                 A BULLETIN AND EXTRACT RELEVANT HEADER
!                 INFORMATION TO FORM AN INDEX. REPORT IS STORED WITH
!                 CHARACTER REPORT AND BUFR STRING OF ELEMENTS.         ING OF ELEMENTS.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : BULLED,OBHOUR,MTRLOC,STAPOS,ENHEXP,DATIM,ENBUFR
!                 CCCODE,NCMIND,TAFREP
!
! ARGUMENTS     : POINT   - CURRENT POSITION IN BULLETIN
!                          (BEFORE STN ID IN FIRST REPORT
!                           UNLESS AN 'OLD' REPORT)
!                 BULEND  - END OF BULLETIN (BEFORE NNNN)
!                 TTAAII  - WILL BE SCUK83
!                 CCCC    - COLLECTING CENTRE
!                 YYGGGG  - DAY AND HOUR OF REPORT
!                 OCOR    - CORRECTED REPORT FLAG
!                 CORNUM  - CORRECTION NUMBER
!                 MIMJ    - 'OLDE' IF AN OLD REPORT
!                 NFTENH  - FT NUMBER FOR ENHANCED SAWS DATASET (23)
!                 BULL    - BULLETIN
!
! REVISION INFO :
!
! $Workfile: enhbul.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 20/12/2010 16:12:01$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         20/12/2010 16:12:01    Alison Weir
!       Correct indentation
!  2    MetDB_Refresh 1.1         15/12/2010 12:10:55    Alison Weir
!       Include extra ENBUFR argument IVER
!  1    MetDB_Refresh 1.0         14/12/2010 10:51:40    Alison Weir
!       Initial version MDBSTOR batch 5
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE bulled_mod
USE cccode_mod
USE datim_mod
USE enbufr_mod
USE enhexp_mod
USE mtrloc_mod
USE ncmind_mod
USE obhour_mod
USE stapos_mod
USE tafrep_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(INOUT) ::    POINT     !A01
INTEGER,          INTENT(INOUT) ::    BULEND    !A02
CHARACTER(LEN=*), INTENT(IN)    ::    TTAAII    !A03
CHARACTER(LEN=*), INTENT(IN)    ::    CCCC      !A04
CHARACTER(LEN=*), INTENT(IN)    ::    YYGGGG    !A05
LOGICAL,          INTENT(IN)    ::    OCOR      !A06
CHARACTER(LEN=*), INTENT(IN)    ::    CORNUM    !A07
CHARACTER(LEN=7), INTENT(IN)    ::    MIMJ      !A08
INTEGER,          INTENT(IN)    ::    NFTENH    !A09
CHARACTER(LEN=*), INTENT(INOUT) ::    BULL      !A10

! Local declarations:

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

CHARACTER(LEN=23)    ::  ENTRY
CHARACTER(LEN=4096)  ::  REPORT
CHARACTER(LEN=5 )    ::  STNID
CHARACTER(LEN=10000) ::  MESSAG

INTEGER              ::  ICCCC          ! collecting centre code
INTEGER              ::  IDES           ! ?
INTEGER              ::  DESCR(500)     ! descriptor array
INTEGER              ::  NDES           ! number of descriptors
INTEGER              ::  NELEM          ! number of input elements
INTEGER              ::  NOBS           ! number of obs
INTEGER              ::  LEN            ! length of bufr string
INTEGER              ::  BLKSIZ
INTEGER              ::  POS            ! FIRST REPORT DATA GROUP
INTEGER              ::  I
INTEGER              ::  IRC
INTEGER              ::  NOW(9)
INTEGER              ::  NUM_VALS       ! number of data values in rpt
INTEGER              ::  OBDATE(5)
INTEGER              ::  OBDAY
INTEGER              ::  OBHR
INTEGER              ::  OBMIN
INTEGER              ::  RC
INTEGER              ::  TOL=0
INTEGER              ::  TOR(5)
INTEGER              ::  IVALUE
INTEGER              ::  REPLEN
INTEGER              ::  GRPLEN
INTEGER              ::  REPEND
INTEGER, PARAMETER   ::  IVER=13        ! Table B version number

LOGICAL              ::  CMPRES=.FALSE. ! BUFR compress flag
LOGICAL              ::  LREPFL
LOGICAL              ::  SYNTAX

REAL                 ::  EXPARR(30)
REAL                 ::  LAT
REAL                 ::  LON
REAL                 ::  STNHT
REAL                 ::  STNHTP
REAL, PARAMETER      ::  RMISS=-9999999.  ! real missing data value

!----- initialisation
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
!     WRITE (6,'(/)')                                        !test line
!     WRITE (6,*) 'start POINT/bulend - ',POINT,BULEND       !test line
!     WRITE (6,*)
!    & 'bulletin data ',BULEND,' ->',BULL(POINT:BULEND),'<'  !test line

!----- day and time of observation is YYGGgg
!      need to identify month and year of observation
!      call to routine obhour to return date
!      - allows for last months/years data

!       convert character form of day and hour to integer form
!       also extract minutes of observation for report header
!       (if not an 'OLD' report)

IFLABEL1: &
  IF (MIMJ(1:4)  /=  'OLDE') THEN
     READ (YYGGGG(1:2),'(I2)') OBDAY
     READ (YYGGGG(3:4),'(I2)') OBHR
     READ (YYGGGG(5:6),'(I2)') OBMIN

!----- If an 'OLD' report, get day/hour from group after 'OLDE'
!----- and move on to start of report ie. the station number.
!----- Old reports are expected to arrive one per bulletin.
!----- 'Point' is at the space after 'OLDE'.
!----- This routine can only cope with one 'OLDE' & date/time
!----- per bulletin.

  ELSE
     OBDAY=IVALUE(BULL(POINT+1:POINT+2))
     OBHR =IVALUE(BULL(POINT+3:POINT+4))
     OBMIN=0
     POINT=POINT+6
  END IF IFLABEL1

  OBDATE(3)=OBDAY
  OBDATE(4)=OBHR
  OBDATE(5)=OBMIN

  CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)
  IF (RC == 4) RETURN

!----- call system clock to get time of receipt

  CALL DATIM(NOW)
  TOR(1)=NOW(8)     ! YEAR
  TOR(2)=NOW(7)     ! MONTH
  TOR(3)=NOW(6)     ! DAY
  TOR(4)=NOW(5)     ! HOUR
  TOR(5)=NOW(4)     ! MINUTE

!----- loop round reports in bulletin until end of bulletin reached
!

DOLABEL1: &
DO WHILE (POINT < BULEND)                        ! reports loop

!diagnostics
!     WRITE (6,*) 'current POINT in report - ',POINT

!----- advance POINTer over any spaces before start of report if any

  DO WHILE (BULL(POINT:POINT) == ' ')
    POINT=POINT+1
  END DO

!----- check position of POINT to see if there is enough length left
!      for another report.

IFLABEL2: &
  IF ((BULEND-POINT) >= 7) THEN                ! long enough?
     REPLEN=INDEX(BULL(POINT:BULEND),'=')-1
     IF (REPLEN <= 0) RETURN
     REPEND=POINT+REPLEN-1
     REPORT=BULL(POINT:REPEND)

!----- set syntax flag to false for report

     SYNTAX=.FALSE.
     LAT= RMISS
     LON= RMISS
     STNHT= RMISS
     STNHTP= RMISS

! ***********************************************************
!
!      OBTAIN THE IDENTIFIER FOR THE REPORT BY CHECKING THE LENGTH OF
!      THE FIRST GROUP IF 3 CHAR THEN BLOCK 03 IS ASSUMED TO BE MISSING.
!
! ***********************************************************

    POS=1
    CALL MTRLOC(REPLEN,REPORT,POS,GRPLEN,LREPFL)
    IF (GRPLEN == 5 ) THEN
       STNID(1:5)=REPORT(1:5)
       POS=7
    ELSE IF (GRPLEN == 3 ) THEN
       STNID(3:5)=REPORT(1:3)
       STNID(1:2)='03'
       POS=5
    END IF

!     PRINT*,' ENHBUL REPORT ',OBDATE,STNID,'>',BULL(POINT:REPEND),'<'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!there are two calls to stapos. The first looks up the requested      !
!station in the surface list. If the requested station is not found   !
!then a second call to stapos looks up the 'x' list. This list contains
!stations that have not been identified as being upperair/surface.    !
!If after the second call the requested station details are still not !
!available then a message is output to warn of the situation          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IFLABEL3: &
    IF(STNID(1:2) == '99')THEN
      LAT=RMISS
      LON=RMISS
      STNHTP=RMISS
      STNHT=RMISS
    ELSE
      CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,STNHT,IRC)

      IF ((LAT  ==  RMISS) .AND. (LON  ==  RMISS)) THEN
        IRC=0
        CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,STNHT,IRC)
      END IF

      IF ((LAT  ==  RMISS) .AND. (LON  ==  RMISS)) THEN
        IRC=0
        CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,STNHT,IRC)
      END IF

      IF ((LAT  ==  RMISS) .AND. (LON  ==  RMISS)) THEN
        WRITE(6,*)' ESAWS (ENHBUL)--> STATION NOT FOUND IN STAPOS ',&
                 STNID
      END IF
    END IF IFLABEL3

    CALL ENHEXP(REPORT,REPLEN,POS,EXPARR,STNID,OBDATE,NUM_VALS,&
                SYNTAX)

!
! LATITUDE
!
    EXPARR(8)=LAT
!
! LONGITUDE
!
    EXPARR(9)=LON
!
! HEIGHT
!
    IF (STNHTP /= RMISS.AND.STNHT == RMISS) STNHT=STNHTP
    EXPARR(10)=STNHT

! *********************************************************************
!
! CALL BUFR ENCODING ROUTINE, WITH CURRENT TIME AS TIME OF RECEIPT TO
! GO IN SECTION 1.  PUT REPORT IN OUTPUT STRING BEFORE BUFR MESSAGE.
!
! *********************************************************************
    MESSAG(1:REPLEN)=REPORT

    DESCR(1)=IDES(307225)
    NDES=1
    NELEM=30
    NOBS=1

    CALL ENBUFR(DESCR,EXPARR,NDES,NELEM,NOBS,STNID,TOR,&
                    MESSAG(REPLEN+1:),CMPRES,LEN,IVER)
!
! SET CCCC & LAND/SEA BUFR DATA TYPE IN SECTION 1 OF THE BUFR MESSAGE
! (DISPLACEMENT AS FOR BUFR VERSION 1; CHANGE IF TOTAL LENGTH AT START)
!
    IF (CCCC /= '    ') CALL CCCODE(287,ICCCC,CCCC)
    MESSAG(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
    MESSAG(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))

!----- index type - 23byte.
!      call to ncmind to compile index entry

    CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,OBHR,OBMIN,&
                LAT,LON)

!      Set a value which can be used to determine preferred report. In
!      this report type the numbers of data values in report is used.

    ENTRY(12:12)=CHAR(NUM_VALS)

!      Set bit 5 of byte 17 of index entry to indicate there is a
!      syntactical error found when report expanded if syntax=.true..

    IF (SYNTAX) THEN
      ENTRY(17:17)=CHAR(64)                     ! FLAG IF SYNTAX ERROR
    ELSE
      ENTRY(17:17)=CHAR(0)                      ! CLEAR BYTE IF NOT
    END IF

!     PRINT*,' ENHBUL ENTRY1 >',ENTRY,'<'
!     PRINT*,' ENHBUL MESSAG >',MESSAG(1:REPLEN+LEN),'<'

!----- finally store report
!      call to tafrep

!diagnostics

!#    PRINT*,' ENHBUL ',STNID,' REPORT LENGTHS = ',REPLEN,LEN,
!#   &       ' TOTAL LENGTH = ',(REPLEN+LEN)  !TESTLINE

    CALL TAFREP(OBDATE,ENTRY,MESSAG(1:REPLEN+LEN),&
                  NFTENH,BLKSIZ,STNID)

! move pointer on to next report

    POINT=REPEND+2

!diagnostics
!     WRITE (6,*) 'pre check report - ',REPORT

  ELSE                                         ! long enough?
    POINT=BULEND
  END IF IFLABEL2                              ! long enough?

END DO DOLABEL1                                  ! reports loop

RETURN
END SUBROUTINE ENHBUL
