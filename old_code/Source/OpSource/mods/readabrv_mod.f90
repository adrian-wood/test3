MODULE readabrv_mod
  INTERFACE
    SUBROUTINE READABRV(Stn,Name)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*), INTENT(IN)  ::  Stn !- Input station number
    CHARACTER(*), INTENT(OUT) ::  Name !- Input station name

    END SUBROUTINE READABRV
  END INTERFACE
END MODULE readabrv_mod
