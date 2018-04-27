MODULE synxp1_mod
  INTERFACE
    SUBROUTINE SYNXP1(REPORT,POINT,NGRPS,SYNTAX,IWMOB, &
      WXMAN,TTT,TDTDTD,UUU,P0P0P0,PPPP,A3,HHH,ATEND, &
      PCHANG,RRR,TR,WW,W1,W2,NH,CL,CM,CH,HOUR,MINS,IX,IDATIM)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT     !a01
    INTEGER,          INTENT(INOUT) :: POINT      !a02
    INTEGER,          INTENT(IN)    :: NGRPS      !a03
    LOGICAL,          INTENT(INOUT) :: SYNTAX     !a04
    INTEGER,          INTENT(IN)    :: IWMOB      !a05
    LOGICAL,          INTENT(IN)    :: WXMAN      !a06
    REAL,             INTENT(INOUT) :: TTT        !a07 Air temperature (TTT)
    REAL,             INTENT(INOUT) :: TDTDTD     !a08 Dew point temperature (TdTdTd)
    REAL,             INTENT(INOUT) :: UUU        !a09 Relative humidity (UUU)
    REAL,             INTENT(INOUT) :: P0P0P0     !a10 Station level Pressure (P0P0P0P0)
    REAL,             INTENT(INOUT) :: PPPP       !a11 MSL pressure (PPPP)
    REAL,             INTENT(INOUT) :: A3         !a12 Standard isobaric surface (a3)
    REAL,             INTENT(INOUT) :: HHH        !a13 Geopotential height (hhh)
    REAL,             INTENT(INOUT) :: ATEND      !a14 Pressure tendency amount (a)
    REAL,             INTENT(INOUT) :: PCHANG     !a15 3 hour Pressure change amount(ppp)
    REAL,             INTENT(INOUT) :: RRR        !a16 Section 1 Precipitation amount (RRR)
    REAL,             INTENT(INOUT) :: TR         !a17 Section 1 type of precipitation (tR)
    REAL,             INTENT(INOUT) :: WW         !a18 Present Weather (ww)
    REAL,             INTENT(INOUT) :: W1         !a19 Past Weather 1 (W1)
    REAL,             INTENT(INOUT) :: W2         !a20 Past Weather 2 (W2)
    REAL,             INTENT(INOUT) :: NH         !a21 Amount of low cloud (Nh)
    REAL,             INTENT(INOUT) :: CL         !a22 Type Low Cloud (CL)
    REAL,             INTENT(INOUT) :: CM         !a23 Type Medium Cloud (CM)
    REAL,             INTENT(INOUT) :: CH         !a24 Type High Cloud (CH)
    REAL,             INTENT(INOUT) :: HOUR       !a25 Hour (GG)
    REAL,             INTENT(INOUT) :: MINS       !a26 Minutes (gg)
    INTEGER,          INTENT(IN)    :: IX         !a27
    INTEGER,          INTENT(IN)    :: IDATIM(5)  !a28

    END SUBROUTINE SYNXP1
  END INTERFACE
END MODULE synxp1_mod
