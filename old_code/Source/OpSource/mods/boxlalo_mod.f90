MODULE BOXLALO_mod
  INTERFACE
    SUBROUTINE BOXLALO (BOX, CBOX)

    IMPLICIT NONE

    REAL, INTENT(IN)                :: BOX(4)  ! (a1) Area lat/long limits (as output by LATBOX)
    CHARACTER (LEN=4), INTENT(OUT)  :: CBOX    ! (a2) String for insertion in index entry

    END SUBROUTINE BOXLALO
  END INTERFACE
END MODULE BOXLALO_mod
