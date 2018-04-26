MODULE decord_mod
  INTERFACE
    SUBROUTINE DECORD(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY, &
                        LIST,INLIST)
      IMPLICIT NONE
      INTEGER, INTENT(INOUT)                     :: DESCR(:)     ! SEQUENCE OF DESCRIPTORS,                a1
      REAL, INTENT(OUT)                          :: VALUES(:)    ! ARRAY FOR VALUES,                       a2
      CHARACTER(LEN=*), INTENT(OUT)              :: NAMES*(*)    ! FOR ANY CHARACTER VALUES ,              a3
      INTEGER, INTENT(INOUT)                     :: ND           ! Number of descriptors in BUFR sequence, a4
      INTEGER, INTENT(IN)                        :: NOBS         ! NUMBER OF REPORTS,                      a5
      CHARACTER(LEN=*), INTENT(IN)               :: STRING       ! BUFR MESSAGE,                           a6
      LOGICAL, INTENT(IN)                        :: DSPLAY       ! FLAG SET IF DISPLAY OF VALUES REQUIRED, a7
      INTEGER, INTENT(IN)                        :: INLIST       ! NUMBER OF DESCRIPTORS IN LIST,          a9
      INTEGER, INTENT(IN)                        :: LIST(INLIST) ! List of descriptors to look for,        a8
    END SUBROUTINE DECORD
  END INTERFACE
END MODULE decord_mod
