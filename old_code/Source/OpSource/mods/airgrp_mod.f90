MODULE airgrp_mod
 INTERFACE
  SUBROUTINE AIRGRP(GROUP,LENGTH,NCHAR,CHARR)
   INTEGER,INTENT(IN)            ::  LENGTH
   INTEGER,INTENT(OUT)           ::  NCHAR
   CHARACTER(LEN=*),INTENT(IN)   ::  GROUP
   CHARACTER(LEN=*),INTENT(OUT)  ::  CHARR
  END SUBROUTINE AIRGRP
 END INTERFACE
END MODULE airgrp_mod