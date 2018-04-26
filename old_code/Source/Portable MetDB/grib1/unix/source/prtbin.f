C $Header: prtbin.f, 1, 27/02/2007 16:11:08, Stan Kellett$

      SUBROUTINE PRTBIN (IN,NBIT,OUT,ERR)
 
C**********************************************************************
C*
C*  NAME      : PRTBIN
C*
C*  FUNCTION  : PRODUCES A DECIMAL NUMBER WITH ONES AND ZEROES
C*              CORRESPONDING TO THE ONES AND ZEROES OF THE INPUT
C*              BINARY NUMBER.
C*              EG INPUT NUMBER 1011 BINARY, OUTPUT NUMBER 1011 DECIMAL.
C*
C*  INPUT     : IN   - INTEGER VARIABLE CONTAINING BINARY NUMBER.
C*              NBIT - NUMBER OF BITS TO BE USED.
C*
C*  OUTPUT    : OUT  - INTEGER VARIABLE CONTAINING DECIMAL VALUE
C*                     WITH ONES AND ZEROES CORRESPONDING TO THOSE OF
C*                     THE INPUT BINARY NUMBER.
C*
C*              ERR  - 0 IF NO ERROR , 1 IF INVALID INTEGER LENGTH.
C*
C*    JOHN HENNESSY     ECMWF, OCTOBER 1985
C*
C**********************************************************************
 
*
************************************************************************
** COPYRIGHT (C) 1988,   BY  EUROPEAN  CENTRE FOR MEDIUM RANGE WEATHER *
** FORECASTS (ECMWF) , SHINFIELD PARK,  READING,  BERKSHIRE,  RG6 9AX, *
** ENGLAND. THIS SOFTWARE OR ANY COPIES THEREOF MAY NOT BE PROVIDED OR *
** OTHERWISE  MADE  AVAILABLE  TO  ANY  OTHER PERSON.  NO TITLE TO AND *
** OWNERSHIP  OF THE SOFTWARE IS HEREBY TRANSFERRED.  ECMWF ASSUMES NO *
** RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS SOFTWARE.          *
**                                                                     *
************************************************************************
!* The most recent version of the GRIBEX software is available from the 
!* following location: 
!*   http://www.ecmwf.int/products/data/software/download/gribex.html 
!***********************************************************************


      IMPLICIT INTEGER (A-Z)
 
      OUT = 0
 
C     CHECK SIZE OF INTEGER
 
      IF (NBIT.LT.0.OR.NBIT.GT.15) THEN
        ERR = 1
        RETURN
      ELSE
        ERR = 0
      ENDIF
 
      DO 100 I=1,NBIT
        K    = I - 1
        MASC = 2**K
        TEMP = IAND(IN,MASC)
        IF (TEMP.NE.0) OUT = OUT + 10**K
  100 CONTINUE
 
      RETURN
      END
