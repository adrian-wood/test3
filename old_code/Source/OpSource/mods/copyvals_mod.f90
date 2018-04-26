MODULE copyvals_mod
  INTERFACE
    SUBROUTINE COPYVALS (IDISP, WANTOB, NDISP, INTELM, &
                         VALUES1, CSTR1, NOBS1, NOB1, &
                         VALUES2, CSTR2, NOBS2, NOB2, ICODE)
    IMPLICIT NONE

! Subroutine arguments:

    INTEGER, INTENT(IN)             ::  NDISP ! Number of data elements per ob.
    INTEGER, INTENT(IN)             ::  IDISP(NDISP) ! Array of displacements for VALUES1
    LOGICAL(kind=1), INTENT(IN)     ::  WANTOB(*)
    INTEGER, INTENT(IN)             ::  INTELM(*) ! Additional data for transfer (TOR etc.)
    INTEGER, INTENT(INOUT)          ::  NOBS1 ! Max. number of obs in VALUES1 array
    REAL, INTENT(IN)                ::  VALUES1(NOBS1,*) ! Data values from BUFR decode
    CHARACTER(*), INTENT(IN)        ::  CSTR1
    INTEGER, INTENT(INOUT)          ::  NOB1 ! Actual number of obs in VALUES1 array
    INTEGER, INTENT(IN)             ::  NOBS2 ! Max. number of obs in VALUES2 array
    REAL, INTENT(OUT)               ::  VALUES2(NOBS2,*) ! User's output array of data values
    CHARACTER(*), INTENT(OUT)       ::  CSTR2(NOBS2)
    INTEGER, INTENT(INOUT)          ::  NOB2 ! Actual number of obs in VALUES2 array
    INTEGER, INTENT(OUT)            ::  ICODE ! Return code

    END SUBROUTINE COPYVALS
  END INTERFACE
END MODULE copyvals_mod
