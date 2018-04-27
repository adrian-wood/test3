MODULE STNARY_MOD
INTERFACE
SUBROUTINE STNARY(BLK,STN,ICAO,LAT1,LAT2,LATP,LONG1,LONG2,LONGP,    &
                  HT1,HT2,STATION,COUNTRY,REGION,DCNN,RAIN,         &
                  OYEAR,OMONTH,ODAY,OHOUR,CYEAR,CMONTH,CDAY,CHOUR,  &
                  FLAGS,CLASS,HAW00,HAW12,MAP,RUN1,RUN2,OREC,NREC,  &
                  UA,SYN,NCM,DAY,SYNTIM,NCMTIM1,NCMTIM2,SQUARE,     &
                  CHASER,RARRAY,NOBS,NELEM,ARRAY1,CSTR,NOBCON)

IMPLICIT NONE

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

END SUBROUTINE STNARY
END INTERFACE
END MODULE STNARY_MOD
