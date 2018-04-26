MODULE synnin_mod
  INTERFACE
    SUBROUTINE SYNNIN(REPORT,NINGPS,WNDKTS,WPER, &
                      T1GUST,T2GUST,VGUST,T1GUS,T2GUS,VGUS, &
                      T1MEAN,T2MEAN,VMEAN, &
                      LSEA,SEASTE,SEAVIS,VV)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)    :: REPORT         !a01 Rest of report string
    INTEGER,          INTENT(IN)    :: NINGPS         !a02
    LOGICAL,          INTENT(IN)    :: WNDKTS         !a03
    REAL,             INTENT(IN)    :: WPER           !a04 Past weather period
    REAL,             INTENT(INOUT) :: T1GUST         !a05 Start gust period of gust 911 group
    REAL,             INTENT(INOUT) :: T2GUST         !a06 End gust period of gust 911 group
    REAL,             INTENT(INOUT) :: VGUST          !a07 911 group gust value (ff/fff)
    REAL,             INTENT(INOUT) :: T1GUS          !a08 Start gust period of gust 910 group
    REAL,             INTENT(INOUT) :: T2GUS          !a09 End gust period of gust 910 group
    REAL,             INTENT(INOUT) :: VGUS           !a10 910 group gust value (ff/fff)
    REAL,             INTENT(INOUT) :: T1MEAN         !a11 Start mean period of 912 group
    REAL,             INTENT(INOUT) :: T2MEAN         !a12 End mean period of 912 group
    REAL,             INTENT(INOUT) :: VMEAN          !a13 912 group mean value (ff/fff)
    LOGICAL,          INTENT(INOUT) :: LSEA           !a14
    REAL,             INTENT(INOUT) :: SEASTE         !a15 State of sea 924 group (S)
    REAL,             INTENT(INOUT) :: SEAVIS         !a16 Horiz vis towards sea 924 group (Vs)
    REAL,             INTENT(INOUT) :: VV             !a17 Visibility (VV)

    END SUBROUTINE SYNNIN
  END INTERFACE
END MODULE synnin_mod
