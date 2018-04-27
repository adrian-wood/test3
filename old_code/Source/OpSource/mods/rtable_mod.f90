MODULE rtable_mod
  INTERFACE
    SUBROUTINE RTABLE (CTYPE, IRM, IAREA, RNAME, LTEST, IETIME, &
                       CDAT, IDATA, LIST, IERR, CERR, MSTREAM,  & !2.4b
                       ELIST)                                    !2.6b

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(8), INTENT(IN)    ::  CTYPE ! Data type code to look up in table
    !INTEGER,      INTENT(IN)    ::  IRM ! Preferred data (1=raw, 2=merged)
    INTEGER,      INTENT(INOUT) ::  IRM ! Preferred data (1=raw, 2=merged). JN to correct INTENT value
    INTEGER,      INTENT(IN)    ::  IAREA ! Indicator for model area
    CHARACTER(*), INTENT(IN)    ::  RNAME ! User's retrieval table name ('DDICT')
    LOGICAL,      INTENT(IN)    ::  LTEST ! .TRUE. if diagnostic printout required
    INTEGER,      INTENT(INOUT) ::  IETIME(9) ! Amended user's start & end times
    CHARACTER(*), INTENT(OUT)   ::  CDAT ! Data set name from retrieval table
    INTEGER,      INTENT(OUT)   ::  IDATA(5) ! Data set information (see above)
    CHARACTER(8), INTENT(OUT)   ::  LIST ! Member name of list of element names
    INTEGER,      INTENT(OUT)   ::  IERR ! Error code (see above)
    CHARACTER(*), INTENT(OUT)   ::  CERR ! Text of error or warning message
    CHARACTER(3), INTENT(OUT)   ::  MSTREAM ! MASS stream (minus MDB prefix) !2.4b
    CHARACTER(8), INTENT(OUT)   ::  ELIST ! Member name of element index !  2.6b

    END SUBROUTINE RTABLE
  END INTERFACE
END MODULE rtable_mod
