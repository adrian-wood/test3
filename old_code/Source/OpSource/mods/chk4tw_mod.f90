MODULE chk4tw_mod
  INTERFACE
    SUBROUTINE CHK4TW(IDESC,IREPL,NDES,WANTEMP,WANTWND,USRLEVT, &
         USRLEVW,LFLAG)

    IMPLICIT NONE

INTEGER,INTENT(IN)   ::  NDES

INTEGER,INTENT(IN)   ::  IDESC(NDES)
INTEGER,INTENT(IN)   ::  IREPL(NDES)
INTEGER,INTENT(OUT)  ::  USRLEVT
INTEGER,INTENT(OUT)  ::  USRLEVW
LOGICAL,INTENT(IN)   ::  LFLAG
LOGICAL,INTENT(OUT)  ::  WANTEMP
LOGICAL,INTENT(OUT)  ::  WANTWND

    END SUBROUTINE CHK4TW
  END INTERFACE
END MODULE chk4tw_mod
