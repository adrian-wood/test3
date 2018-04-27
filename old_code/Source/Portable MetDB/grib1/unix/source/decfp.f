! $Header: decfp.f, 1, 27/02/2007 16:11:05, Stan Kellett$

      SUBROUTINE DECFP (FVAL,IEXP,IMANT)
 
!********************************************************************
!*
!*    NAME      : DECFP
!*
!*    FUNCTION  : DECODE GRIB CODE REPRESENTATION TO
!*                FLOATING POINT NUMBER.
!*
!*    INPUT     : IEXP  - 8 BIT SIGNED EXPONENT
!*                IMANT - 24 BIT MANTISSA
!*
!*    OUTPUT    : FVAL  - FLOATING POINT NUMBER
!*                IEXP  - UNCHANGED
!*                IMANT - UNCHANGED
!*
!*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
!*
!*    MODIFIED BY T12EH , 25TH FEB 92 - IF IMANT = 0 , FVAL = 0.
!*    (PUT IN TO AVOID FLOATING POINT OVERFLOW WHEN IEXP = 128)
!*
!********************************************************************
!***********************************************************************
!* COPYRIGHT (C) 1988,   BY  EUROPEAN  CENTRE FOR MEDIUM RANGE WEATHER *
!* FORECASTS (ECMWF) , SHINFIELD PARK,  READING,  BERKSHIRE,  RG6 9AX, *
!* ENGLAND. THIS SOFTWARE OR ANY COPIES THEREOF MAY NOT BE PROVIDED OR *
!* OTHERWISE  MADE  AVAILABLE  TO  ANY  OTHER PERSON.  NO TITLE TO AND *
!* OWNERSHIP  OF THE SOFTWARE IS HEREBY TRANSFERRED.  ECMWF ASSUMES NO *
!* RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS SOFTWARE.          *
!*                                                                     *
!***********************************************************************
!* The most recent version of the GRIBEX software is available from the 
!* following location: 
!*   http://www.ecmwf.int/products/data/software/download/gribex.html 
!***********************************************************************
!

!     (T12EH mod - test for all bits set to 1 - this causes
!     an underflow error)
!     255 = FF
!     16777215 = FFFFFF

      IF (IEXP.EQ.255.AND.IMANT.EQ.16777215) THEN
        IEXP  = 0
        IMANT = 0
      ENDIF

!    Sign of value
 
      JEXP  = IEXP
      ISIGN = 1
 
      IF (JEXP.GE.128) THEN
        JEXP  = JEXP - 128
        ISIGN = -1
      END IF
 
!     Decode value.
 
      IF (IMANT.EQ.0) THEN
        FVAL = 0.
      ELSE
        FVAL = ISIGN*2.**(-24)*IMANT*16.**(JEXP-64)
      END IF
 
      RETURN
      END
