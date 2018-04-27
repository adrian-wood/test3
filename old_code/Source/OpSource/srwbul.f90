SUBROUTINE SRWBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,OCOR,  &
                  CORNUM,MIMJ,NFTSRW,BULL)

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
!        FUNCTIONS    : OFIGTS,IVALUE
!
! ARGUMENTS   : POINT   - CURRENT POSITION IN BULLETIN
!                        (BEFORE STN ID IN FIRST REPORT
!                         UNLESS AN OLD REPORT)
!               BULEND  - END OF BULLETIN (BEFORE NNNN)
!               TTAAII  - WILL BE SRUK95
!               CCCC    - COLLECTING CENTRE
!               YYGGGG  - DAY AND HOUR OF REPORT
!               OCOR    - CORRECTED REPORT FLAG
!               CORNUM  - CORRECTION NUMBER
!               MIMJ    - 'OLDR' IF AN OLD REPORT
!               NFTSRW  - FT NUMBER FOR SREW DATASET (15)
!               BULL    - BULLETIN
!
! REVISION INFO :
!
! $Workfile: srwbul.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 14/01/2011 11:21:03$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
! $
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

! Use statements:
USE metdb_com_mod, only : RMISS
USE bulled_mod
USE stapos_mod
USE datim_mod
USE obhour_mod
USE ncmind_mod
USE tafrep_mod
USE ofigts_mod
USE ivalue_mod

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

! Subroutine arguments:

INTEGER,          INTENT(INOUT) ::   POINT
INTEGER,          INTENT(INOUT) ::   BULEND
CHARACTER(LEN=*), INTENT(IN)    ::   TTAAII
CHARACTER(LEN=*), INTENT(IN)    ::   CCCC
CHARACTER(LEN=*), INTENT(IN)    ::   YYGGGG
LOGICAL,          INTENT(IN)    ::   OCOR
CHARACTER(LEN=*), INTENT(IN)    ::   CORNUM
CHARACTER(LEN=7), INTENT(IN)    ::   MIMJ
INTEGER,          INTENT(IN)    ::   NFTSRW
CHARACTER(LEN=*), INTENT(INOUT) ::   BULL

! Local declarations:

CHARACTER(LEN=23) ::     ENTRY
CHARACTER(LEN=9)  ::     REPORT
CHARACTER(LEN=5)  ::     STNID

REAL              ::     LAT
REAL              ::     LON
REAL              ::     STNHT
REAL              ::     STNHTP

INTEGER           ::     BLKSIZ
INTEGER           ::     I
INTEGER           ::     IRC
INTEGER           ::     NOW(9)
INTEGER           ::     OBDATE(5)
INTEGER           ::     OBDAY
INTEGER           ::     OBHR
INTEGER           ::     OBMIN
INTEGER           ::     RC
INTEGER           ::     TOL
INTEGER           ::     TOR(5)

LOGICAL           ::     QC
LOGICAL           ::     STORE

!----- initialisation

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

DOLABEL1: &
DO WHILE (POINT < BULEND-5)
  STORE=.FALSE.
  REPORT(1:)=' '

!diagnostics
!     WRITE (6,*) 'current point in report - ',POINT

!----- advance pointer over any spaces before start of report if any

  DO WHILE (BULL(POINT:POINT) == ' ')
    POINT=POINT+1
  END DO

!----- If an 'OLD' report, get day/hour from group after 'OLDR',
!----- call OBHOUR and move on to start of report ie. the station
!----- number. Old reports are expected to arrive one per bulletin.
!----- 'Point' is at the start of the day/hour group.
!----- This routine can only cope with one 'OLDR' and day/time
!----- per bulletin.

IFLABEL1: &
  IF (MIMJ(1:4) == 'OLDR') THEN
     OBDAY=IVALUE(BULL(POINT  :POINT+1))
     OBHR =IVALUE(BULL(POINT+2:POINT+3))
     OBMIN=0

     OBDATE(3)=OBDAY
     OBDATE(4)=OBHR
     OBDATE(5)=OBMIN

     CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)
     IF (RC == 4) RETURN

     POINT=POINT+5
  END IF IFLABEL1

!----- check position of point to see if there is enough length left
!      for another report.
!----- UK SREW reports are 6 characters long and CDL SREW reports
!      are 8 characters long, but 8-character reports come from
!      stations outside block 03 (Gibraltar etc) too, so accept
!      either 8 characters or 6.

IFLABEL2: &
  IF (BULL(POINT+6:POINT+6) == ' '.OR.  &
                        BULL(POINT+6:POINT+6) == '=') THEN
    REPORT(1:2)='03'
    REPORT(3:8)=BULL(POINT:POINT+5)
    REPORT(9:)=' '
    STORE=.TRUE.
    POINT=POINT+7
  ELSE IF (BULL(POINT+8:POINT+8) == ' '.OR.  &
                        BULL(POINT+8:POINT+8) == '=') THEN
    REPORT=BULL(POINT:POINT+7)
    STORE=.TRUE.
    POINT=POINT+9
  ELSE
    IF(BULEND < 112)THEN
      WRITE (6,*)' SRWBUL BAD SREW BULLETIN: ',  &
                   BULL(1:BULEND-1)
    ELSE
      WRITE (6,*)' SRWBUL BAD SREW BULLETIN: ',BULL(1:112)
    END IF
    DO WHILE (BULL(POINT:POINT) /= ' ')
      POINT=POINT+1
    END DO
  END IF IFLABEL2

!diagnostics
!     WRITE (6,*) 'pre check report - ',REPORT

!----- if report cleared length check then check report is either
!      numeric or a trace or nil report.

IFLABEL3: &
  IF (STORE) THEN
    QC=OFIGTS(REPORT,1,8)
    IF (.NOT.QC) THEN
      IF ((REPORT(7:8) == 'TR').OR.(REPORT(6:8) == 'NIL').OR.  &
         (REPORT(6:8) == 'OTR')) THEN
        STORE=.TRUE.
      ELSE
        STORE=.FALSE.
        WRITE (6,*) 'SRWBUL invalid report - not stored - ',  &
                   REPORT
      END IF
    END IF
  END IF IFLABEL3

!----- If report passed content check then check for valid
!      station identifier (formerly non cdl stations only).
!      Check extended to all stations for version 2, August 2007.

IFLABEL4: &
  IF (STORE) THEN
    STNID(1:5)=REPORT(1:5)

!----- call STAPOS to get lat/lon for this station

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
    WRITE(6,*)' SREW (SRWBUL)--> STATION NOT FOUND IN STAPOS ', &
                 STNID
    END IF
  END IF IFLABEL4

!----- now start storage process if all checks passed

IFLABEL5: &
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
!       If an 'OLD' report, all this has already been done.

     IF (MIMJ(1:4) /= 'OLDR') THEN
       READ (YYGGGG(1:2),'(I2)') OBDAY
       READ (YYGGGG(3:4),'(I2)') OBHR
       READ (YYGGGG(5:6),'(I2)') OBMIN

       OBDATE(3)=OBDAY
       OBDATE(4)=OBHR
       OBDATE(5)=OBMIN

       CALL OBHOUR(OBDATE,OBDAY,OBHR,MIMJ,TOL,RC)
       IF (RC == 4) RETURN
     END IF

!----- index type - 23byte.
!      call to ncmind to compile index entry

    CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,OBHR,OBMIN,  &
                LAT,LON)

!----- finally store report
!      call to tafrep

    CALL TAFREP(OBDATE,ENTRY,REPORT(1:8),NFTSRW,BLKSIZ,STNID)

  END IF IFLABEL5    ! end of store report
END DO DOLABEL1       ! end of conditional 'point' < 'bulend'

RETURN
END SUBROUTINE SRWBUL
