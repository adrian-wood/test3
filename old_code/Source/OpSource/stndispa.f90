SUBROUTINE STNDISPA

!**********************************************************************
!                                                                     *
! PROGRAM       : STNDISPA (SUBROUTINE)                               *
!                                                                     *
! PURPOSE       : TO IDENTIFY THE TYPE OF STATION IDENTIFIER          *
!                 ENTERED AND TO VALIDATE THE REQUIRED ACTION         *
!                                                                     *
! DESCRIPTION   : IDENTIFIES TYPE OF STATION ASKED FOR AND CHECKS     *
!                 THE REQUIRED ACTION IS ALLOWD BY LOOKING RETRIEVING *
!                 RELEVENT PIECES OF INFORMATION FRON THE INDEX       *
!                 AND MAIN DATA STORE                                 *
!                                                                     *
! CALLED BY     : STNDISP                                             *
!                                                                     *
! CALLS         : STNINDX                                             *
!                 STNFIN                                              *
!                                                                     *
! PARAMETERS    : NONE (COMMON BLOCK USED)                            *
!                                                                     *
! Y2K  16.06.1997  STNDISPA IS YEAR 2000 COMPLIANT.                   *
!                                                                     *
! CHANGE RECORD :                                                     *
!   DATE : JUL 92 FIRST VERSION OF SOFTWARE                           *
!          SEP 95 ENHANCEMENT TO DISPLAY                              *
!                                                                     *
!**********************************************************************

! *** USE STNINDX_MOD
! *** USE STNFIN_MOD

! Include variables required

! *** USE STNINIT_MOD

IMPLICIT NONE

! Local Variables

CHARACTER (LEN=4)      :: ICAOLST(50)
INTEGER                :: INDREC2
INTEGER                :: IJK
INTEGER                :: WMOLST(50)
INTEGER                :: DCNNLST(50)
INTEGER                :: RAINLST(50)
INTEGER                :: INDWMO(1000)
INTEGER                :: III              ! loop counter

! INITIALISE IDENTIFIERS FIRST

WMOID=-32768
ICAOID='    '
DCNNID=-32768
RAINID=-32768
BLKSTN=0
DO III=1,50
  WMOLST(III)=0
  ICAOLST(III)='    '
  DCNNLST(III)=0
  RAINLST(III)=0
END DO
WMOCHA  = ' '  ! A
ICAOCHA = ' '  ! A
DCNNCHA = ' '  ! A
RAINCHA = ' '  ! A

! SET INITIAL VALUES FOR VARIABLES

BLK=0
STN=0
ICAO='    '
DCNN=0
RAIN=0
STATION='                                         '
COUNTRY='                                    '

! Variables not used in this routine and not found in Included COMMON
! *** LAT=0
! *** LONG=0
! *** IREGION=0

LAT1=0
LAT2=0
LATP='N'
LONG1=0
LONG2=0
LONGP='E'
HT1=0
HT2=0
CHASER='XXXXXX'
OYEAR=0
OMONTH=0
ODAY=0
OHOUR=0
CYEAR=0
CMONTH=0
CDAY=0
CHOUR=0
FLAGS='000000001110000000000000'
CLASS=0
HAW00=99999
HAW12=99999
MAP=0
RUN1=0
RUN2=0
OREC=0
NREC=0
UA='----'
SYN='-------------'
NCM='-----------------------'
NUM=0
OHOUR=IHOUR
ODAY=IDAY
OMONTH=IMONTH
OYEAR=IYEAR

DO III=1,10
  DAY(III)='---------'
  SYNTIM(III)='------------------------'
  NCMTIM1(III)=0
  NCMTIM2(III)=0
END DO


IF_ID: &
IF(IDTYPE == 1)THEN
!        IT IS A WMO NUMBER (CONVERT TO INTEGER)
   DO III=1,5
     IF(CDENT(III:III) <= 'Z')THEN
       PRINT*,'INVALID ID FOR WMO IDENTIFIER'
       ISTAT=16  ! A (WAS SET TO 2 OF ALL NUMBERS!)
       GOTO 999
     END IF
   END DO
   READ(CDENT,'(I5)')WMOID
   READ(CDENT(1:2),'(I2)')BLK
   READ(CDENT(3:5),'(I3)')STN
   READ(CDENT(1:5),'(I5)')BLKSTN
ELSE IF(IDTYPE == 2)THEN
!        IT IS AN ICAO ID
   DO III=1,4
     IF(CDENT(III:III) > 'Z')THEN
       PRINT*,'INVALID ID FOR ICAO IDENTIFIER'
       ISTAT=16  ! A
       GOTO 999
     END IF
   END DO
   ICAO=CDENT(1:4)
   ICAOID=ICAO
ELSE IF(IDTYPE == 3)THEN
!        IT IS A DCNN NUMBER (CONVERT TO INTEGER)
   DO III=1,4
     IF(CDENT(III:III) <= 'Z')THEN
       PRINT*,'INVALID ID FOR DCNN IDENTIFIER'
       ISTAT=16  ! A
       GOTO 999
     END IF
   END DO
   READ(CDENT,'(I4)')DCNN
   DCNNID=DCNN
ELSE IF(IDTYPE == 4)THEN
!        IT IS A RAINFALL NUMBER (CONVERT TO INTEGER)
   DO III=1,6
     IF(CDENT(III:III) <= 'Z')THEN
       PRINT*,'INVALID ID FOR RAIN IDENTIFIER'
       ISTAT=16  ! A
       GOTO 999
     END IF
   END DO
   READ(CDENT,'(I6)')RAIN
   RAINID=RAIN
END IF IF_ID

ISTAT=0

! OPEN INDEX AND MAIN DETAILS DATASTORE

OPEN(89,ACCESS='DIRECT',FORM='FORMATTED',RECL=692) !A 89 WAS 85
OPEN(88,ACCESS='DIRECT',FORM='FORMATTED',RECL=692) !A 88 WAS 84

! CALL INDEX PGM

WMOLST(1)=WMOID
ICAOLST(1)(1:4)=ICAOID(1:4)
DCNNLST(1)=DCNNID
RAINLST(1)=RAINID

! SELECT CURRENT OR HISTORICAL STATION REQUIRED

IF (CURR  ==  'H') THEN  ! A
   POS = 'A'             ! A
ELSE                     ! A
   POS = 'L'             ! A
END IF                   ! A

  100 CALL STNINDX(WMOLST,ICAOLST,DCNNLST,RAINLST, &
      IDTYPE,POS,ISTAT,INDREC,INDWMO)                 !A (INDWMO ADDED)

IF (CURR == 'H' .AND. INDREC(2) == 0) VIEWCURR = '?'  ! A

! SELECT APPROPRIATE PROCESSING DEPENDING ON ACTION REQUIRE

IF_ACTION: &
IF(ACTION == 'O')THEN
  IF(ISTAT <= 0)THEN
    GOTO 999
  END IF

ELSE IF(ACTION /= 'O')THEN

IF_RETRIEVE: &
  IF((ISTAT > 4).OR.(INDREC(1) <= 0))THEN
     GOTO 999
  ELSE

     IJK=1
     INDREC2=INDREC(IJK)

! RETRIEVE A RECORD

     CALL STNFIN(INDREC2,BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1, &
      LONG2,LONGP,HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN, &
      OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,      &
      FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,      &
      UA,SYN,NCM,NUM,                                       &
      DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,CHASER,RECNO)

     BLKSTN=BLK*1000+STN
     F15=FLAGS(15:15)  ! A (ALL ADDED LINES TILL NEXT ! A)
     IF (BLKSTN  <  99999) THEN
       WRITE (WMOCHA, '(I5.5)') BLKSTN
     ELSE
       WMOCHA='-----'
     END IF
     IF (ICAO == '    ' .OR. F15 == '2') THEN
       ICAOCHA='-----'
     ELSE
       ICAOCHA=ICAO
     END IF
     IF (DCNN  >  0) THEN
       WRITE (DCNNCHA, '(I4.4)') DCNN
     ELSE
       DCNNCHA='----'
     END IF
     IF (RAIN  >  0) THEN
       WRITE (RAINCHA, '(I6.6)') RAIN
     ELSE
       RAINCHA='------'
     END IF
     IF (HT1  /=  -32768) THEN
       WRITE (HT1CHA, '(I4)') HT1
     ELSE
       HT1CHA='--  '
     END IF
     IF (HT2  /=  -32768) THEN
       WRITE (HT2CHA, '(I4)') HT2
     ELSE
       HT2CHA='--  '
     END IF
     IF (SUA == 'S' .AND. F15 == '2' .AND. POS == 'L') THEN
        POS = 'A'
        GOTO 100
     END IF       ! A (END OF ADDITIONAL CODE)
  END IF IF_RETRIEVE
END IF IF_ACTION

999 RETURN
END SUBROUTINE STNDISPA
