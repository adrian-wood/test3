! $Header: prtbk1.f, 1, 27/02/2007 16:11:08, Stan Kellett$
!
      SUBROUTINE PRTBK1 (IB0PAR,IB1PAR)
! 
!**************************************************************
!*
!*    NAME      : PRTBK1
!*
!*    FUNCTION  : PRINT THE INFORMATION IN THE PRODUCT DEFINITION
!*                BLOCK (BLOCK 1) OF DECODED GRIB DATA.
!*
!*    INPUT     : IB1PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 1.
!*
!*    OUTPUT    : FLAG FIELDS ARE PRINTED IN BINARY REPRESENTATION.
!*                OTHER FIELDS AS INTEGERS.
!*
!*    JOHN HENNESSY  ECMWF OCTOBER 1985
!*    EMMA HIBLING   MOD JULY 91 TO PRINT EDITION 1 BLOCK 1
!*
!**************************************************************
! 
!*
!************************************************************************
!** COPYRIGHT (C) 1988,   BY  EUROPEAN  CENTRE FOR MEDIUM RANGE WEATHER *
!** FORECASTS (ECMWF) , SHINFIELD PARK,  READING,  BERKSHIRE,  RG6 9AX, *
!** ENGLAND. THIS SOFTWARE OR ANY COPIES THEREOF MAY NOT BE PROVIDED OR *
!** OTHERWISE  MADE  AVAILABLE  TO  ANY  OTHER PERSON.  NO TITLE TO AND *
!** OWNERSHIP  OF THE SOFTWARE IS HEREBY TRANSFERRED.  ECMWF ASSUMES NO *
!** RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS SOFTWARE.          *
!**                                                                     *
!************************************************************************
!* The most recent version of the GRIBEX software is available from the 
!* following location: 
!*   http://www.ecmwf.int/products/data/software/download/gribex.html 
!***********************************************************************
!*
!
      IMPLICIT INTEGER (A-Z)
 !
      DIMENSION IB1PAR(*),IB0PAR(*)
 
!     If IB0PAR(1)=1, print edition 1 information.
 
      IF (IB0PAR(1).EQ.1) THEN
        WRITE (*,*) 'GRIB - EDITION 1'
        WRITE (*,*)
      ENDIF
 
      WRITE (*,9000)
 9000 FORMAT (' BLOCK 1 - PRODUCT DEFINITION BLOCK',/)
 
      WRITE (*,9001) IB1PAR(1)
 9001 FORMAT (' ORIGINATING CENTRE IDENTIFIER.       ',I9)
      WRITE (*,9002) IB1PAR(2)
 9002 FORMAT (' MODEL IDENTIFICATION.                ',I9)
      WRITE (*,9003) IB1PAR(3)
 9003 FORMAT (' GRID DEFINITION.                     ',I9)
      NBIT = 8
      CALL PRTBIN (IB1PAR(4),NBIT,OUT,ERR)
      WRITE (*,9004) OUT
 9004 FORMAT (' FLAG (CODE TABLE 1)                   ',I8.8)
      WRITE (*,9005) IB1PAR(5)
 9005 FORMAT (' PARAMETER IDENTIFIER (CODE TABLE 2). ',I9)
      WRITE (*,9006) IB1PAR(6)
 9006 FORMAT (' TYPE OF LEVEL (CODE TABLE 3).        ',I9)
      WRITE (*,9007) IB1PAR(7)
 9007 FORMAT (' VALUE 1 OF LEVEL (CODE TABLE 3).     ',I9)
      WRITE (*,9008) IB1PAR(8)
 9008 FORMAT (' VALUE 2 OF LEVEL (CODE TABLE 3).     ',I9)
      WRITE (*,9009) IB1PAR(9)
 9009 FORMAT (' YEAR OF DATA.                        ',I9)
      WRITE (*,9010) IB1PAR(10)
 9010 FORMAT (' MONTH OF DATA.                       ',I9)
      WRITE (*,9011) IB1PAR(11)
 9011 FORMAT (' DAY OF DATA.                         ',I9)
      WRITE (*,9012) IB1PAR(12)
 9012 FORMAT (' HOUR OF DATA.                        ',I9)
      WRITE (*,9013) IB1PAR(13)
 9013 FORMAT (' MINUTE OF DATA.                      ',I9)
      WRITE (*,9014) IB1PAR(14)
 9014 FORMAT (' TIME UNIT (CODE TABLE 4).            ',I9)
      WRITE (*,9015) IB1PAR(15)
 9015 FORMAT (' TIME RANGE ONE.                      ',I9)
      WRITE (*,9016) IB1PAR(16)
 9016 FORMAT (' TIME RANGE TWO.                      ',I9)
      WRITE (*,9017) IB1PAR(17)
 9017 FORMAT (' TIME RANGE INDICATOR (CODE TABLE 5). ',I9)
      WRITE (*,9018) IB1PAR(18)
 9018 FORMAT (' NUMBER AVERAGED.                     ',I9)
!
!     GRIB 1 HAS EXTRA INFORMATION
!
      IF (IB0PAR(1).EQ.1) THEN
        WRITE (*,9100) IB1PAR(19)
 9100   FORMAT (' CENTURY OF REFERENCE TIME.           ',I9)
        WRITE (*,9101) IB1PAR(20)
 9101   FORMAT (' DECIMAL SCALE FACTOR.                ',I9)
        WRITE (*,9102) IB1PAR(21)
 9102   FORMAT (' LENGTH OF BLOCK 1.                   ',I9)

!       If block 1 has centre-defined extensions, write them.

        IF (IB1PAR(21).GT.28) THEN
          DO I = 22 , IB1PAR(21) - 6 
            WRITE (*,9103) I,IB1PAR(I)
 9103       FORMAT (' EXTENSION BYTE NUMBER ',I4,' .VALUE    ',I9)
          END DO
        ENDIF

      ENDIF
 
      WRITE(*,*)
 
      RETURN
      END
