MODULE WANTID_MOD
  INTERFACE
    SUBROUTINE WANTID (ENTRY, NVER, NSAT, CHID, NPLATS, KEEP)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(LEN=*), INTENT(IN)  :: ENTRY   ! Index entry
    INTEGER,          INTENT(IN)  :: NVER    ! Index entry version number (1 or 2)
    INTEGER,          INTENT(IN)  :: NSAT(:) ! User's requested satellite IDs
    CHARACTER(LEN=9), INTENT(IN)  :: CHID(:) ! User's requested platform IDs
    INTEGER,          INTENT(IN)  :: NPLATS  ! Number of platforms specified by user
    INTEGER,          INTENT(OUT) :: KEEP    ! 'Keep' flag from identifier checking

    END SUBROUTINE WANTID
  END INTERFACE
END MODULE WANTID_MOD
