MODULE elements_mod
  INTERFACE
    SUBROUTINE ELEMENTS (CREQ, NFIRST, NAME, NAMES, LREQ, LNUM, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    CHARACTER(*),  INTENT(IN)       ::  CREQ    ! User's request string
    INTEGER,       INTENT(IN)       ::  NFIRST  ! First character of CREQ to look at
    CHARACTER(36), INTENT(IN)       ::  NAME(:) ! Element name list from BUFR index
    INTEGER,       INTENT(IN)       ::  NAMES   ! Number of element names in NAME array
    INTEGER,       INTENT(OUT)      ::  LREQ(:) ! Output array describing request list
    INTEGER,       INTENT(INOUT)    ::  LNUM    ! Counter for elements in LREQ array
    INTEGER,       INTENT(OUT)      ::  ICODE   ! Integer return code

    END SUBROUTINE ELEMENTS
  END INTERFACE
END MODULE elements_mod
