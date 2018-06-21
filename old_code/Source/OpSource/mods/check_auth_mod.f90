MODULE check_auth_mod
 INTERFACE
  SUBROUTINE CHECK_AUTH(UID,SUBTYPE,DATE,NOBS,NELEM,CREQ,RC)
    CHARACTER(*),INTENT(IN)  :: UID
    CHARACTER(*),INTENT(IN)  :: SUBTYPE
    INTEGER,INTENT(IN)       :: DATE(8)
    INTEGER,INTENT(IN)       :: NOBS
    INTEGER,INTENT(IN)       :: NELEM
    CHARACTER(*),INTENT(IN)  :: CREQ
    INTEGER,INTENT(INOUT)    :: RC
  END SUBROUTINE CHECK_AUTH
 END INTERFACE
END MODULE CHECK_AUTH_MOD