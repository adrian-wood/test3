MODULE BUFDRIP_mod
  INTERFACE
    SUBROUTINE BUFDRIP(CMPRES,NOBS,DESCR,N,MAXVAL,VALUES,IVAL,  &
     &NINCREM,IVER,IRC)
      IMPLICIT NONE
      LOGICAL :: CMPRES
      INTEGER :: NOBS
      INTEGER :: DESCR(*)
      INTEGER :: N
      INTEGER :: MAXVAL
      REAL :: VALUES(*)
      INTEGER :: IVAL
      INTEGER :: NINCREM
      INTEGER :: IVER
      INTEGER :: IRC
    END SUBROUTINE BUFDRIP
  END INTERFACE
END MODULE BUFDRIP_mod
