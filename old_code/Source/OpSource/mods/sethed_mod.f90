MODULE sethed_mod
  INTERFACE
    SUBROUTINE SETHED(HEADER,RLAT,RLON,IH,IM,ID,IMON,IY, &
        ITH,ITM,NAMD,NCOR,CCCC)

IMPLICIT NONE

CHARACTER(LEN=*),INTENT(INOUT)    ::   HEADER
CHARACTER(LEN=4),INTENT(IN)       ::   CCCC
REAL,INTENT(IN)                   ::   RLAT
REAL,INTENT(IN)                   ::   RLON
REAL                              ::   RMDI
INTEGER,INTENT(IN)                ::   IH
INTEGER,INTENT(IN)                ::   IM
INTEGER,INTENT(IN)                ::   ID
INTEGER,INTENT(IN)                ::   IMON
INTEGER,INTENT(IN)                ::   IY
INTEGER,INTENT(IN)                ::   ITH
INTEGER,INTENT(IN)                ::   ITM
INTEGER,INTENT(IN)                ::   NAMD
INTEGER,INTENT(IN)                ::   NCOR

    END SUBROUTINE SETHED
  END INTERFACE
END MODULE sethed_mod
