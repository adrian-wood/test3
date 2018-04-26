MODULE uprparts_mod
  INTERFACE
    SUBROUTINE UPRPARTS(KeptPart,PosnPart,NumParts,Trailor,Message, &
                        Diagnostics,CharIndex,Istat,WantTemps, &
                        WantWinds,NumTempLevels,NumWindLevels, &
                        Latitude,Longitude,Hour,Minute,Day,Month, &
                        Year,ReportText,FinalProfile,MaxFinalElms, &
                        ArrElmNum,ArrSegNum,ArrSubNum,ArrNRows, &
                        ArrStyp,ArrSegst,ArrSegLen,ArrNsegs, &
                        Isect1,CallSign,                      &
                        NewMdbCall,CCCC,TTAA,Displ,Width,     &
                        Scale,RefVal,Found)

    IMPLICIT NONE

! Subroutine arguments:

    LOGICAL,      INTENT(IN)    ::  KeptPart(4) ! which parts are in the chain
    INTEGER,      INTENT(IN)    ::  PosnPart(4) ! position of part in chain
    INTEGER,      INTENT(IN)    ::  NumParts ! number of parts in the chain
    CHARACTER(*), INTENT(IN)    ::  Trailor(1:NumParts)
    CHARACTER(*), INTENT(IN)    ::  Message(1:NumParts)
    LOGICAL,      INTENT(IN)    ::  Diagnostics ! TRUE if output wanted
    CHARACTER(*), INTENT(IN)    ::  CharIndex(:)             !1.10
    INTEGER,      INTENT(INOUT) ::  Istat ! MetDB retrieval status condition
    LOGICAL,      INTENT(IN)    ::  WantTemps ! TRUE if user wants temperature levels
    LOGICAL,      INTENT(IN)    ::  WantWinds ! TRUE if user wants wind levels
    INTEGER,      INTENT(IN)    ::  NumTempLevels ! number of temp levels wanted
    INTEGER,      INTENT(IN)    ::  NumWindLevels ! number of wind levels wanted
    REAL,         INTENT(IN)    ::  Latitude ! latitude
    REAL,         INTENT(IN)    ::  Longitude ! longitude
    INTEGER,      INTENT(IN)    ::  Hour ! hour
    INTEGER,      INTENT(IN)    ::  Minute ! minute
    INTEGER,      INTENT(IN)    ::  Day ! day
    INTEGER,      INTENT(IN)    ::  Month ! month
    INTEGER,      INTENT(IN)    ::  Year ! year
    CHARACTER(*), INTENT(INOUT) ::  ReportText
    REAL,         INTENT(OUT)   ::  FinalProfile(:) ! final profile
    INTEGER,      INTENT(IN)    ::  MaxFinalElms ! max number of elems in FinalProfile
    INTEGER,      INTENT(OUT)   ::  ArrElmNum(:) ! ARRINDX elements array
    INTEGER,      INTENT(OUT)   ::  ArrSegNum(:) ! ARRINDX segment no. array
    INTEGER,      INTENT(OUT)   ::  ArrSubNum(:) ! ARRINDX segment subscript array
    INTEGER,      INTENT(OUT)   ::  ArrNrows ! ARRINDX no. of elements
    INTEGER,      INTENT(OUT)   ::  ArrStyp(:) ! ARRINDX segment repl count array
    INTEGER,      INTENT(OUT)   ::  ArrSegst(:) ! ARRINDX segment start array
    INTEGER,      INTENT(OUT)   ::  ArrSegLen(:) ! ARRINDX segment length array
    INTEGER,      INTENT(OUT)   ::  ArrNsegs ! ARRINDX no. of segments
    INTEGER,      INTENT(OUT)   ::  Isect1(:) ! array of TOR e.t.c.
    CHARACTER(*), INTENT(OUT)   ::  CallSign(1) ! Ship Call Sign
    LOGICAL,      INTENT(INOUT) ::  NewMdbCall ! TRUE if new MetDB call !C
    CHARACTER(4), INTENT(OUT)   ::  CCCC ! collecting centre
    CHARACTER(*), INTENT(OUT)   ::  TTAA                        !F
    INTEGER,      INTENT(IN)    ::  Displ(:)                        !G
    INTEGER,      INTENT(IN)    ::  Width(:)                        !G
    INTEGER,      INTENT(IN)    ::  Scale(:)                        !G
    INTEGER,      INTENT(IN)    ::  RefVal(:)                       !G
    LOGICAL,      INTENT(IN)    ::  Found(:) ! Array of MetDB keywords selected !I

    END SUBROUTINE UPRPARTS
  END INTERFACE
END MODULE uprparts_mod
