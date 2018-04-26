MODULE isrch_mod
  INTERFACE
    SUBROUTINE ISRCH (NUMBER, LIST, NSIZE, NPOS)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,  INTENT(IN)  :: NUMBER     ! A01 Target integer
    INTEGER,  INTENT(IN)  :: NSIZE      ! A03 Number of integers in list
    INTEGER,  INTENT(IN)  :: LIST(NSIZE) ! A02 Array of ints to be searched
    INTEGER,  INTENT(OUT) :: NPOS       ! A04 Location in list of target int

    END SUBROUTINE ISRCH
  END INTERFACE
END MODULE isrch_mod
