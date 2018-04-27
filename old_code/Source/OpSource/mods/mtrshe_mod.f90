MODULE MTRSHE_MOD
INTERFACE
SUBROUTINE MTRSHE(REPLEN,REPORT,POINT,GRPLEN,LREPFL,BADGRP,RWYUSE, &
                  RWYDIR,RWYPRL)

IMPLICIT NONE

INTEGER,           INTENT(IN)    ::  REPLEN   ! Overall Metar report length
CHARACTER (LEN=*), INTENT(IN)    ::  REPORT   ! Complete Metar report
INTEGER,           INTENT(INOUT) ::  POINT    ! Current 'point' within report
INTEGER,           INTENT(OUT)   ::  GRPLEN   ! Exact length of wind shear group
LOGICAL,           INTENT(OUT)   ::  LREPFL   ! Set if POINT is beyond the report length
LOGICAL,           INTENT(OUT)   ::  BADGRP   ! Flag to bypass sections of code if
                                              ! wind shear group contains syntax errors
INTEGER,           INTENT(OUT)   ::  RWYUSE   ! Expansion value to define runway(s) affected
INTEGER,           INTENT(OUT)   ::  RWYDIR   ! Expansion value to define runway direction
INTEGER,           INTENT(OUT)   ::  RWYPRL   ! Expansion value to define parallel runways

END SUBROUTINE MTRSHE
END INTERFACE
END MODULE MTRSHE_MOD
