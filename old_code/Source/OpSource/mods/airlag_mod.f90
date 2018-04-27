MODULE airlag_mod
 INTERFACE
  SUBROUTINE AIRLAG(BULL,YYGGGG,SPAN,RC)
   INTEGER,INTENT(OUT)       ::  RC     ! return code
   INTEGER,INTENT(OUT)          ::  SPAN   ! max hr diff in bulletin
   CHARACTER(LEN=*),INTENT(IN) ::  BULL   ! bull(start with 1st report)
   CHARACTER(LEN=6),INTENT(IN) ::  YYGGGG ! date/time from bull header
  END SUBROUTINE AIRLAG
 END INTERFACE
END MODULE airlag_mod
