MODULE synxp2_mod
  INTERFACE
    SUBROUTINE SYNXP2(REPORT,POINT,NGRPS,SYNTAX, &
            LSEA,ISWELL,TWTYPE,TWTWTW, &
            DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV, &
            ISICE,ESES,RS,CI,SI,BI,DI,ZI,REPLEN)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
    INTEGER,          INTENT(INOUT) :: POINT      !a02
    INTEGER,          INTENT(IN)    :: NGRPS      !a03
    LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
    LOGICAL,          INTENT(OUT)   :: LSEA       !a05
    INTEGER,          INTENT(OUT)   :: ISWELL     !a06
    REAL,             INTENT(INOUT) :: TWTWTW     !a07 Sea temperature(TwTwTw)
    REAL,             INTENT(INOUT) :: TWTYPE     !a08 SST measurement method (ss)
    REAL,             INTENT(INOUT) :: DS         !a09 Direction of movement (Ds)
    REAL,             INTENT(INOUT) :: VS         !a10 Speed of movement(vs)
    REAL,             INTENT(INOUT) :: TBTYPE     !a11 Wet-bulb temperature measurement method (sw)
    REAL,             INTENT(INOUT) :: TBTBTB     !a12 Wet-bulb temperature(TbTbTb)
    REAL,             INTENT(INOUT) :: WVMS       !a13 Wave measurement method
    REAL,             INTENT(INOUT) :: WVHT       !a14 Wave height (HwaHwa)
    REAL,             INTENT(INOUT) :: WVPER      !a15 Wave period (PwaPwa)
    REAL,             INTENT(INOUT) :: SWELWV(6)  !a16 Swell Waves (2*dwdwPwPwHwHw)
    REAL,             INTENT(INOUT) :: ISICE      !a17 Cause of ice accretion on ship (Is)
    REAL,             INTENT(INOUT) :: ESES       !a18 Ice thickness on ship(EsEs)
    REAL,             INTENT(INOUT) :: RS         !a19 Rate of ice accretion on ship(Rs)
    REAL,             INTENT(INOUT) :: CI         !a20 Sea ice concentration(ci)
    REAL,             INTENT(INOUT) :: SI         !a21 Sea ice development(Si)
    REAL,             INTENT(INOUT) :: BI         !a22 Amount & type of sea ice(bi)
    REAL,             INTENT(INOUT) :: DI         !a23 Bearing of nearest sea ice(Di)
    REAL,             INTENT(INOUT) :: ZI         !a24 Sea ice situation (zi)
    INTEGER,          INTENT(IN)    :: REPLEN     !a25

    END SUBROUTINE SYNXP2
  END INTERFACE
END MODULE synxp2_mod
