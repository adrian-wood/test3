MODULE SRWINT_MOD
INTERFACE
SUBROUTINE SRWINT(VALUES,CNAM,LAT,LON,YEAR,MONTH,DAY,HOUR,BULCOD,  &
                  CCCC,STNID)

IMPLICIT NONE

REAL,               INTENT(INOUT)  :: VALUES(11) ! Expanded data values
CHARACTER (LEN=*),  INTENT(OUT)    :: CNAM       ! String to hold text element data
REAL,               INTENT(IN)     :: LAT        ! Station latitude
REAL,               INTENT(IN)     :: LON        ! Station longitude
INTEGER,            INTENT(IN)     :: YEAR       ! Year of observation
INTEGER,            INTENT(IN)     :: MONTH      ! Month of observation
INTEGER,            INTENT(IN)     :: DAY        ! Day of observation
INTEGER,            INTENT(IN)     :: HOUR       ! Hour of observation
CHARACTER (LEN=*),  INTENT(IN)     :: BULCOD     ! Bulletin identifier
CHARACTER (LEN=*),  INTENT(IN)     :: CCCC       ! Originating centre
CHARACTER (LEN=*),  INTENT(IN)     :: STNID      ! Station identifier

END SUBROUTINE SRWINT
END INTERFACE
END MODULE SRWINT_MOD
