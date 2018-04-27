MODULE stnmas_mod
  IMPLICIT NONE
  TYPE :: stnmas_details
    INTEGER                           :: BLK
    INTEGER                           :: STN
    CHARACTER (LEN=4)                 :: ICAO
    INTEGER                           :: LAT1
    INTEGER                           :: LAT2
    CHARACTER (LEN=1)                 :: LATP
    INTEGER                           :: LONG1
    INTEGER                           :: LONG2
    CHARACTER (LEN=1)                 :: LONGP
    INTEGER                           :: HT1
    INTEGER                           :: HT2
    CHARACTER (LEN=42)                :: STATION
    CHARACTER (LEN=32)                :: COUNTRY
    INTEGER                           :: REGION
    INTEGER                           :: DCNN
    INTEGER                           :: RAIN
    INTEGER                           :: OYEAR
    INTEGER                           :: OMONTH
    INTEGER                           :: ODAY
    INTEGER                           :: OHOUR
    INTEGER                           :: CYEAR
    INTEGER                           :: CMONTH
    INTEGER                           :: CDAY
    INTEGER                           :: CHOUR
    CHARACTER (LEN=24)                :: FLAGS
    INTEGER                           :: CLASS
    INTEGER                           :: HAW00
    INTEGER                           :: HAW12
    INTEGER                           :: MAP
    INTEGER                           :: RUN1
    INTEGER                           :: RUN2
    INTEGER                           :: OREC
    INTEGER                           :: NREC
    CHARACTER (LEN=4)                 :: UA
    CHARACTER (LEN=13)                :: SYN
    CHARACTER (LEN=23)                :: NCM
    INTEGER                           :: NUM
    CHARACTER (LEN=9)                 :: DAY(10)
    CHARACTER (LEN=24)                :: SYNTIM(10)
    INTEGER                           :: NCMTIM1(10)
    INTEGER                           :: NCMTIM2(10)
    INTEGER                           :: SQUARE
    CHARACTER (LEN=6)                 :: CHASER
    INTEGER                           :: RECNO
  END TYPE stnmas_details
END MODULE stnmas_mod
