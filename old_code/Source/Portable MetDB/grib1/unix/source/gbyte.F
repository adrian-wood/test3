C $Header: gbyte.F, 2, 12/01/2009 16:48:31, Alison Weir$

      SUBROUTINE GBYTE( INPCK, OUTPCK, OFFSET, LENGTH )
!
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
!*                                                                     *
!* HP Version Coded under project GAGS 10th March 1992                 *
!* Modified A. Edmunds, U.K. Met. Office 4/8/97 - Bug fix.             *
!* Modified S.Kellett, U.K. Met office 12/2/01 - meging versions using *
!*                                     pre-processor statements        *
!*                                                                     *
!***********************************************************************
!
! Revision Information:
!
! $Revision: 2$
! $Date: 12/01/2009 16:48:31$
! $Folder: source$
! $Workfile: gbyte.F$ 
!
! $Log:
!  2    Met_DB_Project 1.1         12/01/2009 16:48:31    Alison Weir     SX6
!       directives changed to L64 to allow compilation on IBM supercomputer.
!  1    Met_DB_Project 1.0         27/02/2007 16:04:59    Stan Kellett    
! $
! Revision 6.2  2004/05/20 11:32:21  usmdb
! 19 May 2004. Add ECMWF copyright notice.
!
! Revision 6.1  2004/05/18 11:28:17  usmdb
! Add SX6 directives.
!
! Revision 6.0  2001/08/09 13:49:55  usmdb
! Pre-processor statements for different word size between CRAY T3E and other
! platforms. old different modules for each platform replaced.
! Stan Kellett.
!
!

      INTEGER   OFFSET, LENGTH

      INTEGER   SHFT, START, END, NBLANKS, WORDLENGTH
# if defined (CRAY) || defined (L64)           /*2*/
      PARAMETER ( WORDLENGTH = 64 )
# else
      PARAMETER ( WORDLENGTH = 32 )
# endif

      INTEGER     OUTPCK,INPCK(2),WORD(2)
      
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/gbyte.F,v $
     &'//'$Date: 12/01/2009 16:48:31$ $Revision: 2$'
        HEADSET = .TRUE.
      ENDIF                
! End of Revision Code

      START    = 2*WORDLENGTH - OFFSET - LENGTH ! for reading bits
      END      = 2*WORDLENGTH - OFFSET ! Used in finding end of integer
      NBLANKS  = END - WORDLENGTH ! Number of bits to initialise

      WORD(1)=INPCK(1)
      WORD(2)=INPCK(2)

      SHFT = START - WORDLENGTH

C     BLANK OUT DIGITS MORE SIGNIFICANT THAN END IN WORD(1)

      IF ( OFFSET .GT. 0 ) THEN
        CALL MVBITS(0,0,OFFSET,WORD(1),NBLANKS)
      ENDIF

      WORD(1)= ISHFT( WORD(1),-SHFT)

      IF (SHFT.LT.0) THEN

! MVBITS copies from bit START towards little endian side of 
!        length SHFT from Word(2) into WORD(1) starting at bit 0
        CALL MVBITS(WORD(2),START,-SHFT,WORD(1),0)

      ENDIF

      OUTPCK=WORD(1)

      RETURN
      END
