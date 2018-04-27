MODULE NCMINT_MOD
INTERFACE
SUBROUTINE NCMINT(VALUES,CNAM,RLAT,RLON,YEAR,MONTH,DAY,HOUR,MNT, &
                  BULCOD,CCCC,STNID)

IMPLICIT NONE

REAL,              INTENT(INOUT) ::  VALUES(79)    ! Expansion array
CHARACTER (LEN=*), INTENT(OUT)   ::  CNAM          ! Character element data
REAL,              INTENT(IN)    ::  RLAT          ! Station latitude
REAL,              INTENT(IN)    ::  RLON          ! Station longitude
INTEGER,           INTENT(IN)    ::  YEAR          ! Year of report validity
INTEGER,           INTENT(IN)    ::  MONTH         ! Month of report validity
INTEGER,           INTENT(IN)    ::  DAY           ! Day of report validity
INTEGER,           INTENT(IN)    ::  HOUR          ! Hour of report validity
INTEGER,           INTENT(IN)    ::  MNT           ! Minute of report validity
CHARACTER (LEN=*), INTENT(IN)    ::  BULCOD        ! Bulletin identifier
CHARACTER (LEN=*), INTENT(IN)    ::  CCCC          ! Originating centre
CHARACTER (LEN=*), INTENT(IN)    ::  STNID         ! Station identifier

END SUBROUTINE NCMINT
END INTERFACE
END MODULE NCMINT_MOD
