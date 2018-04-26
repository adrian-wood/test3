C $Header: prbkn2.f, 1, 27/02/2007 16:11:08, Stan Kellett$

      SUBROUTINE PRBKN2 (IB2PAR,RPAR)
 
C**************************************************************
C*
C*    NAME      : PRBKN2
C*
C*    FUNCTION  : PRINT THE INFORMATION IN THE GRID DEFINITION
C*                BLOCK (BLOCK 2) OF DECODED EDITION 1 GRIB DATA.
C*
C*    INPUT     : IB2PAR - ARRAY OF DECODED PARAMETERS FROM BLOCK 2.
C*
C*    OUTPUT    : FLAG FIELDS ARE PRINTED IN BINARY REPRESENTATION.
C*                OTHER FIELDS AS INTEGERS.
C*
C*    EMMA HIBLING   JULY 1991  EDITION 1.
C*    EMMA HIBLING   JULY 1991  ROTATED LAT/LONG,RPAR.
C*
C**************************************************************
!$Log:
! 1    Met_DB_Project 1.0         27/02/2007 16:11:08    Stan Kellett    
!$
!Revision 1.10  2004/05/20 11:35:44  usmdb
!19 May 2004. Add ECMWF copyright notice.
!
!Revision 1.9  1999/04/16 09:30:39  usmdb
!Add Lambert conformal projection
!
!$Revision: 1$
!$Date: 27/02/2007 16:11:08$
! 
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


      IMPLICIT INTEGER(A-Z)

      REAL      RPAR(*)

      DIMENSION IB2PAR(*)
 
      WRITE (*,*) 'GRIB - EDITION 1'
      WRITE (*,*)
      WRITE (*,9000)
 9000 FORMAT (' BLOCK 2 - GRID DEFINITION BLOCK',/)
      WRITE (*,9001) IB2PAR(3)
 9001 FORMAT (' DATA REPRESENTATION TYPE (TABLE 6)   ',I9)
      WRITE (*,9500) IB2PAR(1)
 9500 FORMAT (' NUMBER OF VERTICAL COORDINATES.      ',I9)
      WRITE (*,9501) IB2PAR(2)
 9501 FORMAT (' LOCATION OF VERTICAL COORDINATES.    ',I9)
 
C     Spherical harmonics.
 
      IF (IB2PAR(3).EQ.50) THEN
 
        WRITE (*,9002) IB2PAR(4)
 9002   FORMAT (' J - PENTAGONAL RESOLUTION PARAMETER. ',I9)
        WRITE (*,9003) IB2PAR(5)
 9003   FORMAT (' K - PENTAGONAL RESOLUTION PARAMETER. ',I9)
        WRITE (*,9004) IB2PAR(6)
 9004   FORMAT (' M - PENTAGONAL RESOLUTION PARAMETER. ',I9)
        WRITE (*,9005) IB2PAR(7)
 9005   FORMAT (' REPRESENTATION TYPE (TABLE 9).       ',I9)
        WRITE (*,9006) IB2PAR(8)
 9006   FORMAT (' REPRESENTATION MODE (TABLE 10).      ',I9)
        WRITE (*,9007) (IB2PAR(I),I=9,13)
 9007   FORMAT (' NOT USED.                            ',I9)
        WRITE(*,*)
 
        RETURN
 
      ENDIF
 
C     Gaussian, latitude / longitude and mercator.
 
      IF (IB2PAR(3).EQ.0.OR.IB2PAR(3).EQ.4.OR.IB2PAR(3).EQ.1.
     +    OR.IB2PAR(3).EQ.10) THEN
 
        WRITE (*,9102) IB2PAR(4)
 9102   FORMAT (' NO. OF POINTS ALONG A LATITUDE.      ',I9)
        WRITE (*,9103) IB2PAR(5)
 9103   FORMAT (' NO. OF POINTS ALONG A MERIDIAN.      ',I9)
        WRITE (*,9104) IB2PAR(6)
 9104   FORMAT (' LATITUDE OF FIRST GRID POINT         ',I9)
        WRITE (*,9105) IB2PAR(7)
 9105   FORMAT (' LONGITUDE OF FIRST GRID POINT        ',I9)
        NBIT = 8
        CALL PRTBIN (IB2PAR(8),NBIT,OUT,ERR)
        WRITE (*,9106) OUT
 9106   FORMAT (' RESOLUTION FLAG.                      ',I8.8)
        WRITE (*,9107) IB2PAR(9)
 9107   FORMAT (' LATITUDE OF EXTREME POINT.           ',I9)
        WRITE (*,9108) IB2PAR(10)
 9108   FORMAT (' LONGITUDE OF EXTREME POINT.          ',I9)
        
        IF (IB2PAR(3).EQ.1) WRITE (*,9109) IB2PAR(12)
        IF (IB2PAR(3).NE.1) WRITE (*,9109) IB2PAR(11)
 9109   FORMAT (' INCREMENT ALONG A PARALLEL.          ',I9)
        
        IF (IB2PAR(3).EQ.0.OR.IB2PAR(3).EQ.10) THEN
          WRITE (*,9110) IB2PAR(12)
 9110     FORMAT(' INCREMENT ALONG A MERIDIAN.          ',I9)
        ENDIF
        
        IF (IB2PAR(3).EQ.1) THEN
          WRITE (*,9110) IB2PAR(11)
        ENDIF
        
        IF (IB2PAR(3).EQ.4) THEN
          WRITE (*,8110) IB2PAR(12)
 8110     FORMAT(' NO. OF LATITUDE LINES POLE / EQUATOR.',I9)
        ENDIF
        
        NBIT = 8
        CALL PRTBIN (IB2PAR(13),NBIT,OUT,ERR)
        WRITE (*,9111) OUT
 9111   FORMAT (' SCANNING MODE FLAGS (CODE TABLE 8).   ',I8.8)
        
        IF (IB2PAR(3).EQ.1) THEN
          WRITE (*,9112) IB2PAR(14)
 9112     FORMAT(' LATIN.                               ',I9)
        ENDIF
       
C       Rotated lat/long grid.

        IF (IB2PAR(3).EQ.10) THEN
          WRITE (*,9113) IB2PAR(14)
 9113     FORMAT(' LATITUDE OF SOUTHERN POLE.           ',I9)
          WRITE (*,9114) IB2PAR(15)
 9114     FORMAT(' LONGITUDE OF SOUTHERN POLE.          ',I9)
          WRITE (*,9115) RPAR(1)
 9115     FORMAT(' ANGLE OF ROTATION.                  ',F10.3)

        ENDIF

        RETURN
      ENDIF
C
C     Polar stereographic.
C
      IF (IB2PAR(3).EQ.5) THEN
 
        WRITE (*,9202) IB2PAR(4)
 9202   FORMAT (' NO. OF POINTS ALONG X AXIS.          ',I9)
        WRITE (*,9203) IB2PAR(5)
 9203   FORMAT (' NO. OF POINTS ALONG Y AXIS.          ',I9)
        WRITE (*,9204) IB2PAR(6)
 9204   FORMAT (' LATITUDE OF ORIGIN (SOUTH -IVE).     ',I9)
        WRITE (*,9205) IB2PAR(7)
 9205   FORMAT (' LONGITUDE OF ORIGIN (WEST -IVE).     ',I9)
        NBIT = 8
        CALL PRTBIN (IB2PAR(8),NBIT,OUT,ERR)
        WRITE (*,9206) OUT
 9206   FORMAT (' RESOLUTION FLAG.                      ',I8.8)
        WRITE (*,9207) IB2PAR(9)
 9207   FORMAT (' LONGITUDE OF VERTICAL AXIS.          ',I9)
        WRITE (*,9208) IB2PAR(10)
 9208   FORMAT (' X-DIRECTION INCREMEMT.               ',I9)
        WRITE (*,9209) IB2PAR(11)
 9209   FORMAT(' Y-DIRECTION INCREMENT.               ',I9)
        WRITE (*,9210) IB2PAR(12)
 9210   FORMAT (' PROJECTION CENTRE FLAG.              ',I9)
        NBIT = 8
        CALL PRTBIN (IB2PAR(13),NBIT,OUT,ERR)
        WRITE (*,9111) OUT
 9211   FORMAT (' SCANNING MODE FLAGS (CODE TABLE 8).   ',I8.8)
        WRITE(6,*)
        
        RETURN
 
      ENDIF
c
c Lambert Conformal
c 

      IF (IB2PAR(3).EQ.3) THEN                    !1.9
        WRITE (*,9202) IB2PAR(4)                  !1.9 No. points x
        WRITE (*,9203) IB2PAR(5)                  !1.9 No. points y
        WRITE (*,9104) IB2PAR(6)                  !1.9 Lat 1st grid
        WRITE (*,9105) IB2PAR(7)                  !1.9 Long 1st grid    
        NBIT = 8                                  !1.9  
        CALL PRTBIN (IB2PAR(8),NBIT,OUT,ERR)      !1.9  
        WRITE (*,9106) OUT                        !1.9 Res. flag
        WRITE (*,9207) IB2PAR(9)                  !1.9 Lov
        WRITE (*,9208) IB2PAR(10)                 !1.9 Dx
        WRITE (*,9209) IB2PAR(11)                 !1.9 Dy
        WRITE (*,9210) IB2PAR(12)                 !1.9 Prj centre
        NBIT = 8                                  !1.9
        CALL PRTBIN (IB2PAR(13),NBIT,OUT,ERR)     !1.9
        WRITE (*,9111) OUT                        !1.9 scanning mode
        WRITE (*,9301) IB2PAR(14)                 !1.9
 9301   FORMAT (' LATIN 1                              ',I9)  !1.9
        WRITE (*,9302) IB2PAR(15)                 !1.9
 9302   FORMAT (' LATIN 2                              ',I9)  !1.9
        WRITE (*,9113) IB2PAR(16)                 !1.9
        WRITE (*,9114) IB2PAR(17)                 !1.9
        WRITE(6,*)         
        
      ENDIF                                       !1.9
      
      END                                         
