MODULE bogexp_mod
  INTERFACE
    SUBROUTINE BOGEXP(REPORT,VALUES,bogusType,IDENT_PRESSURE)
    CHARACTER(LEN=80)           :: REPORT          !Character report being read-in
    REAL                        :: VALUES(*)       !Decoded elements array *(13)
    CHARACTER(LEN=3)            :: bogusType       !Bogus report type
    CHARACTER(LEN=4)            :: IDENT_PRESSURE  !Pressure kept to be placed in INDEX  
    END SUBROUTINE BOGEXP
  END INTERFACE
END MODULE bogexp_mod