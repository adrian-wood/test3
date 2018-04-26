
      SUBROUTINE CONFP (FVAL,IEXP,IMANT)
C
C
C
C
C
C---->
C********************************************************************
C*
C*    NAME      : CONFP
C*
C*    FUNCTION  : CONVERT FLOATING POINT NUMBER FROM MACHINE
C*                REPRESENTATION TO GRIB REPRESENTATION.
C*
C*    INPUT     : FVAL  - FLOATING POINT NUMBER TO BE CONVERTED.
C*
C*    OUTPUT    : IEXP  - 8 BIT SIGNED EXPONENT
C*                IMANT - 24 BIT MANTISSA
C*                FVAL  - UNCHANGED.
C*
C*    JOHN HENNESSY , ECMWF , 15TH APRIL 1985
!***********************************************************************
!* COPYRIGHT (C) 1988,   BY  EUROPEAN  CENTRE FOR MEDIUM RANGE WEATHER *
!* FORECASTS (ECMWF) , SHINFIELD PARK,  READING,  BERKSHIRE,  RG6 9AX, *
!* ENGLAND. THIS SOFTWARE OR ANY COPIES THEREOF MAY NOT BE PROVIDED OR *
!* OTHERWISE  MADE  AVAILABLE  TO  ANY  OTHER PERSON.  NO TITLE TO AND *
!* OWNERSHIP  OF THE SOFTWARE IS HEREBY TRANSFERRED.  ECMWF ASSUMES NO *
!* RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS SOFTWARE.          *
!***********************************************************************
!* The most recent version of the GRIBEX software is available from the 
!* following location: 
!*   http://www.ecmwf.int/products/data/software/download/gribex.html 
!***********************************************************************
C*
C********************************************************************
C----<
C
C
      IF (FVAL.EQ.0.0)
     C   THEN
             IEXP = 128
             IMANT = 0
             RETURN
         ENDIF
C
C
      EPS = 1.0E-12
      REF = FVAL
C
C     SIGN OF VALUE
C
      ISIGN = 0
      IF (REF.LE.0.)
     C   THEN
             ISIGN = 128
             REF = - REF
         ENDIF
C
C
C
C
C
C     EXPONENT
C
      IF (REF.EQ.0.0)
     C   THEN
            IEXP = 0
         ELSE
            IEXP = INT(ALOG(REF)*(1.0/ALOG(16.0))+64.0+1.0+EPS)
         ENDIF
C
      IF (IEXP.LT.0  ) IEXP = 0
      IF (IEXP.GT.127) IEXP = 127
C
C
C
C
C
C
C
C
C
C
C     MANTISSA
C
      IMANT = NINT (REF/16.0**(IEXP-70))
C
C     ADD SIGN BIT TO EXPONENT.
C
      IEXP = IEXP + ISIGN
C
C
C
C
      RETURN
C
      END
