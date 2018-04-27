MODULE BUFRHED_mod
  INTERFACE
    SUBROUTINE BUFRHED(MESSAGE,EDITION,TABLES,CENTRE, &
                       DATYPE,DATIME,LOCAL1,LOCAL2,   &
                       CMPRES,NOBS,ND,DESCR,DSPLAY)

    IMPLICIT NONE

    ! Interface Arguments

    CHARACTER(LEN=*),INTENT(IN)  ::  MESSAGE    ! INPUT BUFR MESSAGE
    INTEGER,INTENT(INOUT)        ::  EDITION    ! BUFR edition number
    INTEGER,INTENT(INOUT)        ::  TABLES(:)  !
    INTEGER,INTENT(INOUT)        ::  CENTRE(:)  !
    INTEGER,INTENT(INOUT)        ::  DATYPE(:)  !
    INTEGER,INTENT(INOUT)        ::  DATIME(:)  !
    CHARACTER(LEN=*)             ::  LOCAL1     ! SECTION 1 EXTRA DATA
    CHARACTER(LEN=*)             ::  LOCAL2     ! SECTION 2 DATA
    LOGICAL, INTENT(OUT)         ::  CMPRES     ! true if data compressed
    INTEGER, INTENT(INOUT)       ::  NOBS       ! number of obs
    INTEGER, INTENT(INOUT)       ::  ND         ! number of descriptors
    INTEGER, INTENT(INOUT)       ::  DESCR(:)   !
    LOGICAL, INTENT(IN)          ::  DSPLAY     ! true for print of values

    END SUBROUTINE BUFRHED
  END INTERFACE
END MODULE BUFRHED_mod
