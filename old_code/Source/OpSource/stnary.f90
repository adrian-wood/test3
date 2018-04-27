SUBROUTINE STNARY(BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,LONG2,LONGP,    &
                  HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,         &
                  OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,  &
                  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,  &
                  UA,SYN,NCM,DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,     &
                  CHASER,RARRAY,NOBS,NELEM,ARRAY1,CSTR,NOBCON)

!-----------------------------------------------------------------------
!
! PROGRAM       : STNARY
!
! PURPOSE       : TO PUT VALUES FROM RETRIEVED VARIABLES INTO USER
!                 ARRAY FROM RETRIEVAL THROUGH THE MDB
!
! CALLED BY     : STNRT
!
! CALLS         : NONE
!
! REVISION INFO :
!
!
! $Workfile: stnary.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 09/02/2011 17:18:30$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         09/02/2011 17:18:30    Sheila Needham  Change
!        INENTS on users arrays
!  3    MetDB_Refresh 1.2         23/11/2010 08:41:39    Stan Kellett
!       changed dimensions of CSTR from (:) to (*)
!  2    MetDB_Refresh 1.1         12/11/2010 17:10:31    Rosemary Lavery remove
!        old revision info
!  1    MetDB_Refresh 1.0         04/11/2010 13:28:15    Rosemary Lavery
!       Initial port
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

IMPLICIT NONE

! Interface Arguments

INTEGER,            INTENT(IN)       :: BLK
INTEGER,            INTENT(IN)       :: STN
CHARACTER (LEN=4),  INTENT(IN)       :: ICAO
INTEGER,            INTENT(IN)       :: LAT1
INTEGER,            INTENT(IN)       :: LAT2
CHARACTER (LEN=1),  INTENT(IN)       :: LATP
INTEGER,            INTENT(IN)       :: LONG1
INTEGER,            INTENT(IN)       :: LONG2
CHARACTER (LEN=1),  INTENT(IN)       :: LONGP
INTEGER,            INTENT(IN)       :: HT1
INTEGER,            INTENT(IN)       :: HT2
CHARACTER (LEN=42), INTENT(IN)       :: STATION
CHARACTER (LEN=32), INTENT(IN)       :: COUNTRY
INTEGER,            INTENT(IN)       :: REGION
INTEGER,            INTENT(IN)       :: DCNN
INTEGER,            INTENT(IN)       :: RAIN
INTEGER,            INTENT(IN)       :: OYEAR
INTEGER,            INTENT(IN)       :: OMONTH
INTEGER,            INTENT(IN)       :: ODAY
INTEGER,            INTENT(IN)       :: OHOUR
INTEGER,            INTENT(IN)       :: CYEAR
INTEGER,            INTENT(IN)       :: CMONTH
INTEGER,            INTENT(IN)       :: CDAY
INTEGER,            INTENT(IN)       :: CHOUR
CHARACTER (LEN=24), INTENT(IN)       :: FLAGS
INTEGER,            INTENT(IN)       :: CLASS
INTEGER,            INTENT(IN)       :: HAW00
INTEGER,            INTENT(IN)       :: HAW12
INTEGER,            INTENT(IN)       :: MAP
INTEGER,            INTENT(IN)       :: RUN1
INTEGER,            INTENT(IN)       :: RUN2
INTEGER,            INTENT(IN)       :: OREC
INTEGER,            INTENT(IN)       :: NREC
CHARACTER (LEN=4),  INTENT(IN)       :: UA
CHARACTER (LEN=13), INTENT(IN)       :: SYN
CHARACTER (LEN=23), INTENT(INOUT)    :: NCM
CHARACTER (LEN=9),  INTENT(INOUT)    :: DAY(10)
CHARACTER (LEN=24), INTENT(IN)       :: SYNTIM(10)
INTEGER,            INTENT(IN)       :: NCMTIM1(10)
INTEGER,            INTENT(IN)       :: NCMTIM2(10)
INTEGER,            INTENT(IN)       :: SQUARE
CHARACTER (LEN=6),  INTENT(IN)       :: CHASER
INTEGER,            INTENT(IN)       :: NOBS
INTEGER,            INTENT(IN)       :: NELEM
REAL,               INTENT(INOUT)    :: RARRAY(NOBS,NELEM)
INTEGER,            INTENT(IN)       :: ARRAY1(:)
CHARACTER (LEN=*),  INTENT(INOUT)    :: CSTR(:)
INTEGER,            INTENT(INOUT)    :: NOBCON

! Local Variables

INTEGER               :: I,J,K
INTEGER               :: CHARPOS
INTEGER               :: IBIN(9)
INTEGER               :: IBIN2(4)
INTEGER               :: IBIN3(13)
INTEGER               :: IBIN4(12)
INTEGER               :: IBIN5(10)
INTEGER               :: IBIN6(2)
INTEGER               :: IBIN7(6)
INTEGER               :: THEPOS(9)
INTEGER               :: IELEM        ! Current element id
INTEGER               :: IJK          ! Loop counter

DATA IBIN/256,128,64,32,16,8,4,2,1/
DATA IBIN2/8,4,2,1/
DATA IBIN3/4096,2048,1024,512,256,128,64,32,16,8,4,2,1/
DATA IBIN4/2048,1024,512,256,128,64,32,16,8,4,2,1/
DATA IBIN5/512,256,128,64,32,16,8,4,2,1/
DATA IBIN6/2,1/
DATA IBIN7/32,16,8,4,2,1/
DATA THEPOS/13,12,7,6,5,4,3,2,1/


! SET K TO NUMBER COUNT OF OBS RETRIEVED (NOBCON)

K=NOBCON
J=1
CHARPOS=1

DO_ELEMT: &
DO I =1,NELEM
  IELEM=ARRAY1(I)

IF_ELEMNO: &
  IF(IELEM == 1)THEN
    IF((BLK /= -32768).AND.(STN /= -32768))THEN
      RARRAY(K,J)=BLK*1000+STN
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 2)THEN
    IF(ICAO /= '    ')THEN
      RARRAY(K,J)=4*65536+CHARPOS
      CSTR(K)(CHARPOS:CHARPOS+3)=ICAO
      CHARPOS=CHARPOS+4
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 3)THEN
    RARRAY(K,J)=LAT1*100+REAL(LAT2)/60.0*100.0
    RARRAY(K,J)=RARRAY(K,J)/100
    IF(LATP == 'S')THEN
      RARRAY(K,J)=RARRAY(K,J)*(-1)
    END IF
    J=J+1
  ELSE IF(IELEM == 4)THEN
    RARRAY(K,J)=LONG1*100+REAL(LONG2)/60.0*100.0
    RARRAY(K,J)=RARRAY(K,J)/100
    IF(LONGP == 'W')THEN
      RARRAY(K,J)=RARRAY(K,J)*(-1)
    END IF
    J=J+1
  ELSE IF(IELEM == 5)THEN
    IF(HT1 /= -32768)THEN
      RARRAY(K,J)=HT1
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 6)THEN
    IF(HT2 /= -32768)THEN
      RARRAY(K,J)=HT2
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 7)THEN
    RARRAY(K,J)=42*65536+CHARPOS
    CSTR(K)(CHARPOS:CHARPOS+41)=STATION
    CHARPOS=CHARPOS+42
    J=J+1
  ELSE IF(IELEM == 8)THEN
    RARRAY(K,J)=32*65536+CHARPOS
    CSTR(K)(CHARPOS:CHARPOS+31)=COUNTRY
    CHARPOS=CHARPOS+32
    J=J+1
  ELSE IF(IELEM == 9)THEN
    IF((REGION > -1).AND.(REGION <= 7))THEN
      RARRAY(K,J)=REGION
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 10)THEN
    IF(DCNN /= 0)THEN
      RARRAY(K,J)=DCNN
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 11)THEN
    IF(RAIN /= 0)THEN
      RARRAY(K,J)=RAIN
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 12)THEN
    RARRAY(K,J)=OYEAR
    J=J+1
  ELSE IF(IELEM == 13)THEN
    RARRAY(K,J)=OMONTH
    J=J+1
  ELSE IF(IELEM == 14)THEN
    RARRAY(K,J)=ODAY
    J=J+1
  ELSE IF(IELEM == 15)THEN
    RARRAY(K,J)=OHOUR
    J=J+1
  ELSE IF(IELEM == 16)THEN
    RARRAY(K,J)=CYEAR
    J=J+1
  ELSE IF(IELEM == 17)THEN
    RARRAY(K,J)=CMONTH
    J=J+1
  ELSE IF(IELEM == 18)THEN
    RARRAY(K,J)=CDAY
    J=J+1
  ELSE IF(IELEM == 19)THEN
    RARRAY(K,J)=CHOUR
    J=J+1
  ELSE IF(IELEM == 20)THEN
    RARRAY(K,J)=0.            ! CLEAR ELEMENT ! A
    DO IJK=1,9
      IF(FLAGS(THEPOS(IJK):THEPOS(IJK)) == '1')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN(IJK)
      END IF
    END DO
    J=J+1
  ELSE IF(IELEM == 21)THEN
    IF(FLAGS(8:8) == '1')THEN
      RARRAY(K,J)=1
    ELSE IF(FLAGS(8:8) == '2')THEN
      RARRAY(K,J)=2
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 22)THEN
    IF(FLAGS(11:11) == '1')THEN
      RARRAY(K,J)=1
    ELSE IF(FLAGS(11:11) == '2')THEN
      RARRAY(K,J)=2
    ELSE IF(FLAGS(11:11) == '3')THEN
      RARRAY(K,J)=3
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 23)THEN
    IF(FLAGS(10:10) == '1')THEN
      RARRAY(K,J)=1
    ELSE IF(FLAGS(10:10) == '2')THEN
      RARRAY(K,J)=2
    ELSE IF(FLAGS(10:10) == '3')THEN
      RARRAY(K,J)=3
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 24)THEN
    IF(CLASS /= 0)THEN
      RARRAY(K,J)=CLASS
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 25)THEN
    IF(FLAGS(9:9) == '1')THEN
      RARRAY(K,J)=1
    ELSE IF(FLAGS(9:9) == '2')THEN
      RARRAY(K,J)=2
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 26)THEN
    IF(HAW00 /= 99999)THEN
      RARRAY(K,J)=HAW00
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 27)THEN
    IF(HAW12 /= 99999)THEN
      RARRAY(K,J)=HAW12
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 28)THEN
    IF(MAP /= 0)THEN
      RARRAY(K,J)=MAP
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 29)THEN
    IF(RUN1 /= 0)THEN
      RARRAY(K,J)=RUN1
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 30)THEN
    IF(RUN2 /= 0)THEN
      RARRAY(K,J)=RUN2
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 31)THEN
    IF(OREC /= 0)THEN
      RARRAY(K,J)=OREC
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 32)THEN
    IF(NREC /= 0)THEN
      RARRAY(K,J)=NREC
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 33)THEN
    RARRAY(K,J)=0
    DO IJK=1,4
      IF((UA(IJK:IJK) /= '-').AND.(UA(IJK:IJK) /= 'X'))THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN2(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 34)THEN
    RARRAY(K,J)=0
    DO IJK=1,13
      IF(SYN(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN3(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 35)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(NCM(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 36)THEN
    RARRAY(K,J)=0
    DO IJK=13,23
      IF(NCM(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN3(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 37)THEN
    RARRAY(K,J)=0

! PROCESS REPORTING PRACTICE FOR SUMMER/WINTER (1), PUBLIC HOLIDAY (2)
! AND REPORTING DAYS (3-9)

DO_REPPRAC1: &
    DO IJK=1,9
IF_REPPRAC1: &
      IF(IJK == 1)THEN
        IF(DAY(1)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(1)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(1)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(1)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF IF_REPPRAC1
    END DO DO_REPPRAC1

    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 38)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(1)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 39)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(1)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 40)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(1) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(1) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 41)THEN
    RARRAY(K,J)=0

DO_REPPRAC2: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(2)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(2)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(2)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(2)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC2
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 42)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(2)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 43)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(2)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 44)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(2) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(2) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 45)THEN
    RARRAY(K,J)=0

    DO_REPPRAC3: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(3)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(3)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(3)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(3)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC3

    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 46)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(3)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 47)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(3)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 48)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(3) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(3) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 49)THEN
    RARRAY(K,J)=0

DO_REPPRAC4: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(4)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(4)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(4)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(4)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC4
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 50)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(4)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 51)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(4)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 52)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(4) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(4) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 53)THEN
    RARRAY(K,J)=0

DO_REPPRAC5: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(5)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(5)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(5)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(5)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC5
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 54)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(5)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 55)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(5)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 56)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(5) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(5) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 57)THEN
    RARRAY(K,J)=0

DO_REPPRAC6: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(6)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(6)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(6)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(6)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC6
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 58)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(6)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 59)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(6)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 60)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(6) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(6) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 61)THEN
    RARRAY(K,J)=0

DO_REPPRAC7: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(7)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(7)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(7)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(7)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC7
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 62)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(7)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 63)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(7)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 64)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(7) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(7) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 65)THEN
    RARRAY(K,J)=0

DO_REPPRAC8: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(8)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(8)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(8)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(8)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC8
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 66)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(8)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 67)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(8)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 68)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(8) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(8) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 69)THEN
    RARRAY(K,J)=0

DO_REPPRAC9: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(9)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(9)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(9)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(9)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      END IF
    END DO DO_REPPRAC9

    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 70)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(9)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 71)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(9)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)  ! A
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 72)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(9) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(9) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 73)THEN
    RARRAY(K,J)=0

DO_REPPRAC10: &
    DO IJK=1,9
      IF(IJK == 1)THEN
        IF(DAY(10)(IJK:IJK) == 'S')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
        ELSE IF(DAY(10)(IJK:IJK) == 'W')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        ELSE IF(DAY(10)(IJK:IJK) == '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK)
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
      ELSE
        IF(DAY(10)(IJK:IJK) /= '-')THEN
          RARRAY(K,J)=RARRAY(K,J)+IBIN5(IJK+1)
        END IF
          END IF
    END DO DO_REPPRAC10
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 74)THEN
    RARRAY(K,J)=0
    DO IJK=1,12
      IF(SYNTIM(10)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 75)THEN
    RARRAY(K,J)=0
    DO IJK=13,24
      IF(SYNTIM(10)(IJK:IJK) /= '-')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN4(IJK-12)
      END IF
    END DO
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 76)THEN
    RARRAY(K,J)=0
    IF(NCMTIM1(10) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(1)
    END IF
    IF(NCMTIM2(10) /= 0)THEN
      RARRAY(K,J)=RARRAY(K,J)+IBIN6(2)
    END IF
    IF(RARRAY(K,J) == 0)THEN
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 77)THEN
    IF(SQUARE /= 0)THEN
      RARRAY(K,J)=SQUARE
    ELSE
      RARRAY(K,J)=-9999999
    END IF
    J=J+1
  ELSE IF(IELEM == 78)THEN
    RARRAY(K,J)=0
    DO IJK=1,6
      IF(CHASER(IJK:IJK) == '1')THEN
        RARRAY(K,J)=RARRAY(K,J)+IBIN7(IJK)
      END IF
    END DO
    J=J+1
  END IF IF_ELEMNO

END DO DO_ELEMT

NOBCON=NOBCON+1

RETURN
END SUBROUTINE STNARY
