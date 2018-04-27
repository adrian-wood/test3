MODULE metdb_com_mod

  INTEGER, PARAMETER  :: KR8 = SELECTED_REAL_KIND(8) ! 8 decimal places

! Constants

  INTEGER, PARAMETER  :: MISSIN = -9999999 ! Integer missing data value

  REAL, PARAMETER             :: KTS2MPS = 0.5144444444   ! 1852/3600 IS THE STANDARD
  REAL (KIND=KR8), PARAMETER  :: PI = 3.14159265          ! PI
  REAL, PARAMETER             :: REARTH = 6371229.        ! radius of e arth in metres
  REAL, PARAMETER             :: RMISS = -9999999.        ! REAL missing data value
  REAL, PARAMETER             :: TCONV = 273.1      ! SYNOP TEMPERATURES ARE IN TENTHS
  REAL, PARAMETER             :: FT2METR = 0.3048         ! feet to metres conversion

END MODULE metdb_com_mod
