MODULE modlvb_mod
  INTERFACE
      SUBROUTINE MODLVB(IFT,BUFMES,L,IRC)
      IMPLICIT NONE                                                 !1.5
      INTEGER IFT                                                   !1.5
      CHARACTER           BUFMES*(*)                                !1.5
      INTEGER L                                                     !1.5
      INTEGER IRC                                                   !1.5
      END SUBROUTINE MODLVB
    END INTERFACE
END MODULE modlvb_mod
