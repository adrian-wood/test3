MODULE bogind_mod
  INTERFACE
    SUBROUTINE bogind (REPORT, VALUES, bogusType, OFT, YMD, TOR, IDENT_PRESSURE)
    CHARACTER(LEN=*)               :: REPORT         !Observation     
    REAL                           :: VALUES(13)     !Decoded elements
    CHARACTER(LEN=3)               :: bogusType      !Bogus report type
    INTEGER                        :: OFT            !Storage dataset unit number
    INTEGER                        :: YMD(3)         !Year Month Day in numeric
    INTEGER                        :: TOR(5)         !Time of receipt
    CHARACTER(LEN=4)               :: IDENT_PRESSURE !Pressure for identifier  
    END SUBROUTINE bogind
  END INTERFACE
END MODULE bogind_mod