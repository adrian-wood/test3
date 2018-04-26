MODULE STNINIT_MOD
!
! VARIABLES FOR STATION MASTER
!
! VARIABLES FOLLOWED BY '! A' ARE ADDITIONAL ONES FOR 29/95 CHANGE
!
!Y2K  16.06.1997  STNINIT IS YEAR 2000 COMPLIANT.
!
!   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

IMPLICIT NONE

CHARACTER (LEN=4)       ::  ICAO
CHARACTER (LEN=4)       ::  UA
CHARACTER (LEN=6)       ::  CHASER
CHARACTER (LEN=24)      ::  FLAGS
CHARACTER (LEN=24)      ::  SYNTIM(10)
CHARACTER (LEN=13)      ::  SYN
CHARACTER (LEN=23)      ::  NCM
CHARACTER (LEN=9)       ::  DAY(10)
CHARACTER (LEN=42)      ::  STATION
CHARACTER (LEN=32)      ::  COUNTRY
CHARACTER (LEN=1)       ::  LATP
CHARACTER (LEN=1)       ::  LONGP
CHARACTER (LEN=1)       ::  CURR
CHARACTER (LEN=1)       ::  UP
CHARACTER (LEN=20)      ::  SOURCE
CHARACTER (LEN=20)      ::  AUSER
CHARACTER (LEN=2)       ::  COYEAR
CHARACTER (LEN=2)       ::  COMONTH
CHARACTER (LEN=2)       ::  CODAY
CHARACTER (LEN=2)       ::  COHOUR
CHARACTER (LEN=2)       ::  CCYEAR
CHARACTER (LEN=2)       ::  CCMONTH
CHARACTER (LEN=2)       ::  CCDAY
CHARACTER (LEN=2)       ::  CCHOUR
CHARACTER (LEN=1)       ::  V,W,X,Y
CHARACTER (LEN=100)     ::  CMDSTR
CHARACTER (LEN=1)       ::  ACTION
CHARACTER (LEN=1)       ::  A
CHARACTER (LEN=1)       ::  POS
CHARACTER (LEN=6)       ::  CDENT
CHARACTER (LEN=4)       ::  ICAOID
CHARACTER (LEN=8)       ::  CPANEL

CHARACTER (LEN=3)       ::  NCMTIM1A
CHARACTER (LEN=3)       ::  NCMTIM1B
CHARACTER (LEN=3)       ::  NCMTIM1C
CHARACTER (LEN=3)       ::  NCMTIM1D
CHARACTER (LEN=3)       ::  NCMTIM1E
CHARACTER (LEN=3)       ::  NCMTIM1F
CHARACTER (LEN=3)       ::  NCMTIM1G
CHARACTER (LEN=3)       ::  NCMTIM1H
CHARACTER (LEN=3)       ::  NCMTIM1I
CHARACTER (LEN=3)       ::  NCMTIM1J

CHARACTER (LEN=3)       ::  NCMTIM2A
CHARACTER (LEN=3)       ::  NCMTIM2B
CHARACTER (LEN=3)       ::  NCMTIM2C
CHARACTER (LEN=3)       ::  NCMTIM2D
CHARACTER (LEN=3)       ::  NCMTIM2E
CHARACTER (LEN=3)       ::  NCMTIM2F
CHARACTER (LEN=3)       ::  NCMTIM2G
CHARACTER (LEN=3)       ::  NCMTIM2H
CHARACTER (LEN=3)       ::  NCMTIM2I
CHARACTER (LEN=3)       ::  NCMTIM2J

CHARACTER (LEN=9)       ::  DAY1
CHARACTER (LEN=9)       ::  DAY2
CHARACTER (LEN=9)       ::  DAY3
CHARACTER (LEN=9)       ::  DAY4
CHARACTER (LEN=9)       ::  DAY5
CHARACTER (LEN=9)       ::  DAY6
CHARACTER (LEN=9)       ::  DAY7
CHARACTER (LEN=9)       ::  DAY8
CHARACTER (LEN=9)       ::  DAY9
CHARACTER (LEN=9)       ::  DAY10

CHARACTER (LEN=24)      ::  SYNTIM1
CHARACTER (LEN=24)      ::  SYNTIM2
CHARACTER (LEN=24)      ::  SYNTIM3
CHARACTER (LEN=24)      ::  SYNTIM4
CHARACTER (LEN=24)      ::  SYNTIM5
CHARACTER (LEN=24)      ::  SYNTIM6
CHARACTER (LEN=24)      ::  SYNTIM7
CHARACTER (LEN=24)      ::  SYNTIM8
CHARACTER (LEN=24)      ::  SYNTIM9
CHARACTER (LEN=24)      ::  SYNTIM10

CHARACTER (LEN=1)       ::  F1, F2, F3, F4, F5, F6
CHARACTER (LEN=1)       ::  F7, F8, F9,F10,F11,F12
CHARACTER (LEN=1)       ::  F13,F14,F15,F16,F17,F18
CHARACTER (LEN=1)       ::  F19,F20,F21,F22,F23,F24

CHARACTER (LEN=1)       ::  SUA                            ! A
CHARACTER (LEN=1)       ::  VIEWCURR                       ! A
CHARACTER (LEN=21)      ::  STNTYPE                        ! A
CHARACTER (LEN=5)       ::  WMOCHA                         ! A
CHARACTER (LEN=4)       ::  ICAOCHA                        ! A
CHARACTER (LEN=4)       ::  DCNNCHA                        ! A
CHARACTER (LEN=6)       ::  RAINCHA                        ! A
CHARACTER (LEN=4)       ::  HT1CHA                         ! A
CHARACTER (LEN=4)       ::  HT2CHA                         ! A

INTEGER                 ::  BLK
INTEGER                 ::  STN
INTEGER                 ::  DCNN
INTEGER                 ::  RAIN
INTEGER                 ::  REGION
INTEGER                 ::  LAT1
INTEGER                 ::  LAT2
INTEGER                 ::  LONG1
INTEGER                 ::  LONG2
INTEGER                 ::  HT1
INTEGER                 ::  HT2
INTEGER                 ::  CLASS
INTEGER                 ::  HAW00
INTEGER                 ::  HAW12
INTEGER                 ::  OYEAR
INTEGER                 ::  OMONTH
INTEGER                 ::  ODAY
INTEGER                 ::  OHOUR
INTEGER                 ::  CYEAR
INTEGER                 ::  CMONTH
INTEGER                 ::  CDAY
INTEGER                 ::  CHOUR
INTEGER                 ::  MAP
INTEGER                 ::  RUN1
INTEGER                 ::  RUN2
INTEGER                 ::  OREC
INTEGER                 ::  NREC
INTEGER                 ::  NUM
INTEGER                 ::  NCMTIM1(10)
INTEGER                 ::  NCMTIM2(10)
INTEGER                 ::  RECNO
INTEGER                 ::  SQUARE

INTEGER                 ::  BLKSTN
INTEGER                 ::  IDTYPE
INTEGER                 ::  IBLKSTN
INTEGER                 ::  DCNNID
INTEGER                 ::  RAINID
INTEGER                 ::  ISTAT
INTEGER                 ::  INDREC(1000)

INTEGER                 ::  IHOUR
INTEGER                 ::  IDAY
INTEGER                 ::  IMONTH
INTEGER                 ::  IYEAR
INTEGER                 ::  LOGREC
INTEGER                 ::  WMOID
INTEGER                 ::  END

COMMON /CHARS/ ACTION,A,CDENT,ICAO,UA,CHASER,FLAGS,SYNTIM,SYN,        &
     &   NCM,DAY,STATION,COUNTRY,LATP,LONGP,POS,ICAOID,CURR,          &
     &   DAY1,DAY2,DAY3,DAY4,DAY5,DAY6,DAY7,DAY8,DAY9,DAY10,          &
     &   SYNTIM1,SYNTIM2,SYNTIM3,SYNTIM4,SYNTIM5,                     &
     &   SYNTIM6,SYNTIM7,SYNTIM8,SYNTIM9,SYNTIM10,                    &
     &   F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,                      &
     &   F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,V,W,X,Y,UP,  &
     &   NCMTIM1A,NCMTIM1B,NCMTIM1C,NCMTIM1D,NCMTIM1E,                &
     &   NCMTIM1F,NCMTIM1G,NCMTIM1H,NCMTIM1I,NCMTIM1J,                &
     &   NCMTIM2A,NCMTIM2B,NCMTIM2C,NCMTIM2D,NCMTIM2E,                &
     &   NCMTIM2F,NCMTIM2G,NCMTIM2H,NCMTIM2I,NCMTIM2J,SOURCE,AUSER,   &
     &   COYEAR,COMONTH,CODAY,COHOUR,CCYEAR,CCMONTH,CCDAY,CCHOUR,     &
     &   CPANEL,SUA,VIEWCURR,STNTYPE,WMOCHA,ICAOCHA,DCNNCHA,RAINCHA,  &
     &   HT1CHA,HT2CHA

COMMON /INTS/ BLKSTN,BLK,STN,DCNN,RAIN,REGION,LAT1,LAT2,              &
     &   LONG1,LONG2,HT1,HT2,CLASS,HAW00,HAW12,                       &
     &   OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,             &
     &   MAP,RUN1,RUN2,OREC,NREC,NUM,                                 &
     &   NCMTIM1,NCMTIM2,RECNO,SQUARE,IDTYPE,IBLKSTN,DCNNID,RAINID,   &
     &   ISTAT,IHOUR,IDAY,IMONTH,IYEAR,INDREC,LOGREC,                 &
     &   WMOID,END

END MODULE STNINIT_MOD
