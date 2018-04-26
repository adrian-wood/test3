SUBROUTINE ENHEXP(REPORT,REPLEN,POS,EXPARR,STNID,OBDATE,NUM_VALS,&
                  SYNTAX)

!-----------------------------------------------------------------------
!
! PROGRAM       : ENHEXP
!
! PURPOSE       : TO EXPAND REPORT INTO AN ARRAY READY FOR
!                 ENCODING INTO BUFR.
!
! CALLED BY     : ENHBUL
!
! CALLS         : OFIGTS, TEMPEXP, DATE13, DATE31
!
! ARGUMENTS     : REPORT   CHARACTER STRING OF REPORT (I)
!                 REPLEN   LENGTH OF REPORT  (I)
!                 POS      FIRST GROUP POSITION (I)
!                 EXPARR   EXPANSION ARRAY   (O)
!                 STNID    STATION ID (STRING) (I)
!                 NUM_VALS NUMBER OF DATA VALUES (O)
!                 SYNTAX   SET TO TRUE IF SYNTAX ERROR FOUND
!
! REVISION INFO :
!
! $Workfile: enhexp.f90$ $Folder: OpSource$
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
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
USE ofigts_mod
USE tempexp_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=4096), INTENT(IN)        ::  REPORT      !A01
INTEGER,             INTENT(IN)        ::  REPLEN      !A02
INTEGER,             INTENT(INOUT)     ::  POS         !A03
REAL,                INTENT(OUT)       ::  EXPARR(30)  !A04
CHARACTER(LEN=5),    INTENT(IN)        ::  STNID       !A05
INTEGER,             INTENT(IN)        ::  OBDATE(5)   !A06
INTEGER,             INTENT(OUT)       ::  NUM_VALS    !A07
LOGICAL,             INTENT(INOUT)     ::  SYNTAX      !A08

! Local declarations:

CHARACTER(LEN=4 )  ::   TTVAL
LOGICAL            ::   STORE

INTEGER            ::   CENTDY
INTEGER            ::   DAY_M1
INTEGER            ::   MON_M1
INTEGER            ::   YR_M1
INTEGER            ::   GROUP
INTEGER            ::   I
INTEGER            ::   FLAG(7)
INTEGER            ::   IARR(30)
INTEGER            ::   SPCPOS       ! POSITION OF NEXT SPACE IN REPORT
INTEGER, PARAMETER ::   MISSIN=-9999999   ! integer missing data value

REAL, PARAMETER    ::  RMISS=-9999999.      ! real missing data value
REAL, PARAMETER    ::  KTS2MPS=0.5144444444 ! knots to m/s conversion
                                            ! 1852/3600 IS THE STANDARD

! ***********************************************************
!
!     INITIALISE ARRAY TO MISSING DATA
!
! ***********************************************************

DO I= 1,30
  IARR(I) = MISSIN
END DO

NUM_VALS=0
TTVAL(1:)=' '

!
! WMO BLK AND STN NO
!
READ (STNID(01:02),'(I2)') IARR(1)
READ (STNID(03:05),'(I3)') IARR(2)

!
! year
!
IARR(3)=OBDATE(1)

!
! month
!
IARR(4)=OBDATE(2)

!
! day
!
IARR(5)=OBDATE(3)

!
! hour
!
IARR(6)=OBDATE(4)

!
! minute
!
IARR(7)=OBDATE(5)

! Initialise group done indicators

DO GROUP=1,7
  FLAG(GROUP)=0
END DO

! Loop through groups in report

DOLABEL1: &
DO WHILE (POS < REPLEN)

! Find position of next space in report

  SPCPOS=INDEX(REPORT(POS:REPLEN),' ')

!     PRINT*,' ENHEXP >',REPORT(POS-2:POS+2),'<',POS,REPLEN,SPCPOS,
!    &                   (REPLEN-POS)                ! test lines

! Test that first digit of group is a number

IFLABEL1: &
  IF (OFIGTS(REPORT,POS,POS).AND. &
       (SPCPOS == 6.OR.(SPCPOS == 0.AND.(REPLEN-POS) == 4)))THEN
!
! 1 Group (1dadafafa - Modal wind speed and direction)
!
IFLABEL2: &
    IF (REPORT(POS:POS) == '1')THEN
IFLABEL3: &
      IF (FLAG(1)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+1,POS+2)) THEN
! DADA
          READ (REPORT(POS+1:POS+2),'(I2)') IARR(17)
          IARR(15)=10
          IARR(16)=6
          FLAG(1)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF

        IF (OFIGTS(REPORT,POS+3,POS+4)) THEN
!fafa
          IARR(18)=4
          READ (REPORT(POS+3:POS+4),'(I2)') IARR(19)
          FLAG(1)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF IFLABEL3
    END IF IFLABEL2
!
! 2 Group (20fafafa - Modal wind speed > 99)
!
    IF ((REPORT(POS:POS+1) == '20').AND.(IARR(18) >= 0))THEN
      IF (FLAG(2)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+2,POS+4)) THEN
!fafafa
          READ (REPORT(POS+2:POS+4),'(I3)') IARR(19)
          FLAG(1)=1
          FLAG(2)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF
    END IF
!
! 3 Group (3dgdgfgfg - Direction and speed of max gust)
!
IFLABEL4: &
    IF (REPORT(POS:POS) == '3')THEN
IFLABEL5: &
      IF (FLAG(3)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+1,POS+2)) THEN
!dgdg
          READ (REPORT(POS+1:POS+2),'(I2)') IARR(25)
          IARR(25)=IARR(25)
          FLAG(3)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF

        IF (OFIGTS(REPORT,POS+3,POS+4)) THEN
!fgfg
          READ (REPORT(POS+3:POS+4),'(I2)') IARR(26)
          FLAG(3)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF IFLABEL5
    END IF IFLABEL4
!
! 4 Group (40fgfgfg - Speed of max gust > 99)
!
    IF ((REPORT(POS:POS+1) == '40').AND.(IARR(15) >= 0))THEN
      IF (FLAG(4)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+2,POS+4)) THEN
!fgfg
          READ (REPORT(POS+2:POS+4),'(I3)') IARR(26)
          FLAG(4)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF
    END IF
!
! 5 Group (5HHmm - Time of max gust)
!
IFLABEL6: &
    IF (REPORT(POS:POS) == '5')THEN
IFLABEL7: &
      IF (FLAG(5)  ==  0) THEN
IFLABEL8: &
        IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
!HHmm
          READ (REPORT(POS+1:POS+2),'(I2)') IARR(23)   !HH
          READ (REPORT(POS+3:POS+4),'(I2)') IARR(24)   !mm
          FLAG(5)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1

! year, month and day for time of max gust

IFLABEL9: &
          IF(IARR(23) <= OBDATE(4))THEN
! Take bulletin date for year, month and day values
            IARR(20)=OBDATE(1)                         !YEAR
            IARR(21)=OBDATE(2)                         !MONTH
            IARR(22)=OBDATE(3)                         !DAY
          ELSE
! Take yesterday for year, month and day values
            CALL DATE31(OBDATE(3),OBDATE(2),OBDATE(1),CENTDY)
            CALL DATE13((CENTDY-1),DAY_M1,MON_M1,YR_M1)
            IARR(20)=YR_M1                             !YEAR
            IARR(21)=MON_M1                            !MONTH
            IARR(22)=DAY_M1                            !DAY
          END IF IFLABEL9
        END IF IFLABEL8
      END IF IFLABEL7
    END IF IFLABEL6
!
! 6 Group (6snT0T0T0 - 10cm soil temperature)
!
    IF (REPORT(POS:POS) == '6')THEN
      IF (FLAG(6)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
          TTVAL=REPORT(POS+1:POS+4)
          IARR(12)=NINT(TEMPEXP(TTVAL)*10)
          FLAG(6)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF
    END IF
!
! 7 Group (7kkkk - Global irradiation)
!
    IF (REPORT(POS:POS) == '7')THEN
      IF (FLAG(7)  ==  0) THEN
        IF (OFIGTS(REPORT,POS+1,POS+4)) THEN
          IARR(13)=1
          READ (REPORT(POS+1:POS+4),'(I4)') IARR(14)
          FLAG(7)=1
          STORE=.TRUE.
          NUM_VALS=NUM_VALS+1
        END IF
      END IF
    END IF

    POS=POS+6
  ELSE
    SYNTAX=.TRUE.
!     PRINT*,' ENHEXP - GROUP LENGTH PROBLEM ',STNID,(OBDATE(I),I=3,5)
!     PRINT*,' >',REPORT(1:REPLEN),'<',POS,REPLEN,SPCPOS
    IF (SPCPOS == 0) THEN
      POS=REPLEN
    ELSE
      POS=POS+SPCPOS
    END IF
  END IF IFLABEL1
!
END DO DOLABEL1

! ***********************************************************
!
!     TRANSFER ELEMENTS FROM INTEGER ARRAY TO REAL ARRAY
!
! ***********************************************************

DO I= 1,30
  IF(IARR(I) /= MISSIN)THEN
    EXPARR(I)= REAL(IARR(I))
  ELSE
    EXPARR(I) = RMISS
  END IF
END DO

! ***********************************************************
!
!     CONVERSIONS
!
! ***********************************************************

! set 10cm soil temperature depth coordinate value and temperature value
! to degrees and tenths.

IF(IARR(12) /= MISSIN)THEN
  EXPARR(11)= 0.1
  EXPARR(12)= EXPARR(12)/10.0
END IF

! convert wind speeds from knots to metres per second

IF(IARR(19) /= MISSIN)THEN
  EXPARR(19)= EXPARR(19)*KTS2MPS
END IF

IF(IARR(26) /= MISSIN)THEN
  EXPARR(26)= EXPARR(26)*KTS2MPS
END IF

! convert wind directions from 10's of degrees to degrees

IF(IARR(17) /= MISSIN)THEN
  EXPARR(17)= EXPARR(17)*10
END IF

IF(IARR(25) /= MISSIN)THEN
  EXPARR(25)= EXPARR(25)*10
END IF

! convert global radiation from kJ/m2 to J/m2

IF(IARR(14) /= MISSIN)THEN
  EXPARR(14)= EXPARR(14)*1000
END IF

!     PRINT*,' ENHEXP flags ',FLAG               ! test line
                                           ! test line
!
!     IF(STORE)THEN                                ! test line
!       DO I= 1,30                                 ! test line
!         IF(expARR(I) /= rmiss)THEN               ! test line
!           PRINT*,' ENHEXP ',I,EXPARR(I),iarr(i)  ! test line
!         END IF                                   ! test line
!       END DO                                     ! test line
!     ELSE                                         ! test line
!        PRINT*,' ENHEXP - NO DATA ELEMENTS REPORTED! '
!     END IF                                       ! test line

RETURN
END SUBROUTINE ENHEXP
