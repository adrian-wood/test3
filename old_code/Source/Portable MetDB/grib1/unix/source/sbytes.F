! $Header: sbytes.F, 2, 12/01/2009 16:48:31, Alison Weir$

      SUBROUTINE SBYTES( INPCK, OUTPCK, SBIT, NBITS, SKBITS, REPEAT )
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
!
!
!     GENERAL PURPOSE SUBROUTINE TO UNPACK AND RE-PACK BITS.
!
!     THIS ROUTINE SHOULD WORK WITH ANY FORTRAN COMPILER, ALL THAT
!     IS REQUIRED IS TO CHANGE THE VALUE OF THE PARAMETER NWORD,
!     WHICH INDICATES THE NUMBER OF BITS IN AN INTEGER ON THE HOST
!     COMPUTER, AND THE PARAMETER DIRECT, WHICH INDICATES THE DIRECTION
!     OF THE BIT STREAM, I.E. IF THE FIRST BIT IS THE LEAST SIGNIFICANT
!     OR MOST SIGNIFICANT IN THE WORD.
!
!
!     INPUT:
!
!       OUTPCK:   AN UNPACKED INTEGER ARRAY CONTAINING THE INPUT
!                 BINARY STREAM
!
!        SBIT:    THE BIT AT WHICH TO START THE ENCODING
!
!        NBITS:   THE NUMBER OF BITS TO PACK
!
!        SKBITS:  THE NUMBER OF BITS TO SKIP BEFORE PACKING THE NEXT
!                 SEQUENCE OF BITS.
!
!        REPEAT:  THE NUMBER OF PACKINGS TO BE PERFORMED
!
!     OUTPUT:
!
!        INPCK:  A PACKED BINARY STREAM
!
!
! Revision Information:
!
! $Revision: 2$
! $Date: 12/01/2009 16:48:31$
! $Folder: source$
! $Workfile: sbytes.F$ 
!
! $Log:
!  2    Met_DB_Project 1.1         12/01/2009 16:48:31    Alison Weir     SX6
!       directives changed to L64 to allow compilation on IBM supercomputer.
!  1    Met_DB_Project 1.0         27/02/2007 16:05:03    Stan Kellett    
! $
! Revision 6.2  2004/05/20 11:40:52  usmdb
! 19 May 2004. Add ECMWF copyright notice.
!
! Revision 6.1  2004/05/18 11:35:40  usmdb
! Add SX6 directives.
!
! Revision 6.0  2001/08/20 14:02:48  usmdb
! replaces different versions for hp and cray using preprocessor statements.
!
!
!     SUBROUTINE ARGUMENTS
!
      INTEGER   SBIT, NBITS, SKBITS, REPEAT, INPCK(*), OUTPCK(*)
!
!     MACHINE DEPENDENT PARAMETERS
!
      INTEGER   NWORD, DIRECT

# if defined (CRAY) || defined (L64)      /*2*/
      PARAMETER (NWORD=64)
!                                      64 BITS IN A INTEGER
# else
      PARAMETER (NWORD=32)
!                                      32 BITS IN A INTEGER
# endif

      PARAMETER (DIRECT=1)
!                                      1 ==> FIRST BIT IS LEAST SIGNIFICANT
!                                      0 ==> FIRST BIT IS MOST SIGNIFICANT
!
!     GENERAL VARIABLES
!
      INTEGER   I, OFFSET, BASE
      
! Revision Code:
      LOGICAL HEADSET
      CHARACTER*132 HEAD
      DATA HEADSET /.FALSE./
      
      IF (.NOT.HEADSET) THEN
        HEAD='
     &$Source: /home/us0400/grib/source/RCS/sbytes.F,v $
     &'//'$Date: 12/01/2009 16:48:31$ $Revision: 2$'
        HEADSET = .TRUE.
      ENDIF                
! End of Revision Code
!
!     LOOP "REPEAT" TIMES, TO EXTRACT THE FIELDS
!
      DO 10 I = 1, REPEAT
         OFFSET = SBIT + (NBITS+SKBITS)*(I-1)
         BASE = OFFSET / NWORD + 1
         OFFSET = MOD(OFFSET,NWORD)
         CALL SBYTE( INPCK(BASE), OUTPCK(I), OFFSET, NBITS )
10    CONTINUE

      RETURN
      END
