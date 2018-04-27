C $Header: offset.f, 1, 27/02/2007 16:11:07, Stan Kellett$

      SUBROUTINE OFFSET  (IOFF,NVAL,IWORD,IBYTE,NBIT,ILEN,IERR)
 
C********************************************************************
C*
C*    NAME      : OFFSET
C
C*    FUNCTION  : CALCULATES THE WORD AND BIT OFFSET OF THE START
C*                OF THE NEXT BIT-FIELD IN AN ARRAY OF BINARY
C*                DATA, FOLLOWING THE INSERTION/EXTRACTION OF ONE
C*                OR MORE CONSECUTIVE BIT-FIELDS.
C*
C*    INPUT     : IOFF  - BIT OFFSET AT WHICH LAST INSERTION OR
C*                        EXTRACTION STARTED.
C*                NVAL  - NUMBER OF FIELDS INSERTED OR EXTRACTED IN
C*                        LAST OPERATION.
C*                IWORD - WORD NUMBER OF ARRAY AT WHICH LAST OPERATION
C*                        STARTED.
C*                IBYTE - LENGTH, IN BITS, OF LAST FIELD(S) INSERTED
C*                        OR EXTRACTED.
C*                NBIT  - NUMBER OF BITS IN COMPUTER WORD.
C*                ILEN  - NUMBER OF WORDS IN ARRAY.
C*
C*    OUTPUT    : IOFF  - BIT OFFSET AT WHICH NEXT INSERTION OR
C*                        EXTRACTION STARTS.
C*                NVAL  - UNCHANGED.
C*                IWORD - WORD NUMBER OF ARRAY AT WHICH NEXT OPERATION
C*                        STARTS.
C*                IBYTE - UNCHANGED.
C*                NBIT  - UNCHANGED.
C*                ILEN  - UNCHANGED.
C*
C*                IERR  - UNCHANGED IF NO ERROR.
C*                        EQUALS 3 IF NEXT OPERATION IS OUTSIDE ARRAY
C*                        BOUNDS.
C*
C*    GENERAL   : OFFSET CALLS -----
C*
C*    AUTHOR    : J.HENNESSY  15.4.85
C*
C*    MODIFIED  : J.HENNESSY  28.11.85
C*                A. Edmunds  10.6.97
C*
C********************************************************************
 
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


C     CALCULATE NEXT WORD AND BIT POSITIONS.
 
      IBITL = NVAL  * IBYTE
      INTER = IBITL / NBIT
      IOFFS = IBITL - INTER * NBIT
      IWORD = IWORD + INTER
      IOFF  = IOFF  + IOFFS
 
      IF (IOFF.GE.NBIT) THEN
        IWORD = IWORD + 1
        IOFF  = IOFF  - NBIT
      END IF
 
C     CHECK THAT NEXT WORD TO BE ACCESSED LIES WITHIN THE ARRAY BOUNDS.
 
      IF (IWORD.GT.ILEN) THEN
        IERR = 3
        WRITE (*,9001) IWORD,ILEN
 9001   FORMAT (' Word ', I8, ' is outside array bounds ',I8)
      ENDIF
 
      RETURN
      END
