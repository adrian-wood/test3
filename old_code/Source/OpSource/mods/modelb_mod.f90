MODULE modelb_mod
  INTERFACE
      SUBROUTINE MODELB(IFT,BUFMES,L,IRC)
      IMPLICIT NONE                                                 !1.5
      INTEGER IFT                                                   !1.5
      CHARACTER           BUFMES*(*)                                !1.5
      INTEGER L                                                     !1.5
      INTEGER IRC                                                   !1.5
      END SUBROUTINE MODELB
    END INTERFACE
END MODULE modelb_mod
