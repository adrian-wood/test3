C $Header: sbyte.F, 2, 12/01/2009 16:48:31, Alison Weir$

      SUBROUTINE SBYTE( PACKIN, UNPKED, OFFSET, LENGTH )
*
************************************************************************
** COPYRIGHT (C) 1988,   BY  EUROPEAN  CENTRE FOR MEDIUM RANGE WEATHER *
** FORECASTS (ECMWF) , SHINFIELD PARK,  READING,  BERKSHIRE,  RG6 9AX, *
** ENGLAND. THIS SOFTWARE OR ANY COPIES THEREOF MAY NOT BE PROVIDED OR *
** OTHERWISE  MADE  AVAILABLE  TO  ANY  OTHER PERSON.  NO TITLE TO AND *
** OWNERSHIP  OF THE SOFTWARE IS HEREBY TRANSFERRED.  ECMWF ASSUMES NO *
** RESPONSIBILITY FOR THE USE OR RELIABILITY OF ITS SOFTWARE.          *
**                                                                     *
** HP Version Coded under project GAGS 17th March 1992                 *
**                                                                     *
************************************************************************
!* The most recent version of the GRIBEX software is available from the 
!* following location: 
!*   http://www.ecmwf.int/products/data/software/download/gribex.html 
!***********************************************************************
!
! Revision Information:
!
! $Revision: 2$
! $Date: 12/01/2009 16:48:31$
! $Folder: source$
! $Workfile: sbyte.F$ 
!
! $Log:
!  2    Met_DB_Project 1.1         12/01/2009 16:48:31    Alison Weir     SX6
!       directives changed to L64 to allow compilation on IBM supercomputer.
!  1    Met_DB_Project 1.0         27/02/2007 16:05:03    Stan Kellett    
! $
! Revision 6.2  2004/05/20 11:39:51  usmdb
! 19 May 2004. Add ECMWF copyright notice.
!
! Revision 6.1  2004/05/18 11:32:51  usmdb
! Add SX6 directives.
!
! Revision 6.0  2001/08/20 13:58:01  usmdb
! version replaces individual versions for hp and cray by using preprocessor statements
!
! 
*

      INTEGER   OFFSET, LENGTH

      INTEGER   SHFT, START, END, NBITS, WORDLENGTH
# if defined (CRAY) || defined (L64)      /*2*/
      PARAMETER ( WORDLENGTH = 64 )
# else
      PARAMETER ( WORDLENGTH = 32 )
# endif

      INTEGER     UNPKED,PACKIN(2),WORD(2)
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/sbyte.F,v $
     &'//'$Date: 12/01/2009 16:48:31$ $Revision: 2$'
        HEADSET = .TRUE.
      ENDIF                
! End of Revision Code


      START    = 2*WORDLENGTH - OFFSET - LENGTH
      END      = 2*WORDLENGTH - OFFSET
      SHFT     = START - WORDLENGTH
      NBITS    = END - WORDLENGTH

      WORD(1)=UNPKED


      IF (SHFT.LT.0) THEN     ! if true, bits will straddle word

C       Move bits that overhang the edge of WORD(1) into the top of
C       WORD(2)

        CALL MVBITS(WORD(1),0,-SHFT,PACKIN(2),START)

        WORD(1)= ISHFT( WORD(1),SHFT)  ! shift bits in WORD(1) to RHS.

        CALL MVBITS(WORD(1),0,NBITS,PACKIN(1),0) ! pack bits

      ELSE

        WORD(1)= ISHFT( WORD(1),SHFT)  ! shift bits in WORD(1) to RHS.

        CALL MVBITS(WORD(1),SHFT,LENGTH,PACKIN(1),SHFT) ! pack bits

      ENDIF

      RETURN
      END
