MODULE dataindx_mod
  INTERFACE
    SUBROUTINE DATAINDX (NUNIT, BULSEQ, NUMSEQ, NDSC, NUMDSC, &
                         CREQ, NFIRST, IDISP, NDISP, ICODE)

    IMPLICIT NONE

! Subroutine arguments:

    INTEGER,      INTENT(IN)     ::  NUNIT        ! Unit number for index data set
    CHARACTER(*), INTENT(IN)     ::  BULSEQ       ! Sequence from BUFR Section 3
    INTEGER,      INTENT(IN)     ::  NUMSEQ       ! Number of descriptors in BULSEQ
    INTEGER,      INTENT(IN)     ::  NUMDSC       ! Number of descriptors in NDSC array
    INTEGER,      INTENT(IN)     ::  NDSC(NUMDSC) ! Descriptor array from BUFR decode
    CHARACTER(*), INTENT(IN)     ::  CREQ         ! User's request string
    INTEGER,      INTENT(IN)     ::  NFIRST       ! First character of CREQ to look at
    INTEGER,      INTENT(OUT)    ::  IDISP(:)     ! Displacements from "RETPOS"
    INTEGER,      INTENT(INOUT)  ::  NDISP        ! No. of values in IDISP array
    INTEGER,      INTENT(OUT)    ::  ICODE        ! Return code

    END SUBROUTINE DATAINDX
  END INTERFACE
END MODULE dataindx_mod
 