MODULE MTRINT_MOD
INTERFACE
SUBROUTINE MTRINT(VALUES,CNAM,RLAT,RLON,IY,IM,ID,IH,IMIN, &
                  BULCOD,CCCC,THISID)

IMPLICIT NONE

REAL,              INTENT(OUT) ::  VALUES(:)  ! Expansion array
CHARACTER (LEN=*), INTENT(OUT) ::  CNAM       ! Character element data
REAL,              INTENT(IN)  ::  RLAT       ! Station latitude
REAL,              INTENT(IN)  ::  RLON       ! Station longitude
INTEGER,           INTENT(IN)  ::  IY         ! Year of report validity
INTEGER,           INTENT(IN)  ::  IM         ! Month of report validity
INTEGER,           INTENT(IN)  ::  ID         ! Day of report validity
INTEGER,           INTENT(IN)  ::  IH         ! Hour of report validity
INTEGER,           INTENT(IN)  ::  IMIN       ! Minute of report validity
CHARACTER (LEN=*), INTENT(IN)  ::  BULCOD     ! Bulletin identifier
CHARACTER (LEN=*), INTENT(IN)  ::  CCCC       ! Originating centre
CHARACTER (LEN=*), INTENT(IN)  ::  THISID     ! Station identifier

END SUBROUTINE MTRINT
END INTERFACE
END MODULE MTRINT_MOD
