***********************************************************************         
*                                                                     *         
* PROGRAM       : STRIP                                               *         
*                                                                     *         
* PURPOSE       : FUNCTION FOR USE BY FORTRAN TO FIND LENGTH OF       *         
*                 CHARACTER STRING NOT COUNTING TRAILING BLANKS       *         
*                                                                     *         
* PARAMETERS    : (1) CHARACTER STRING                                *         
*                 (2) LENGTH INCLUDING TRAILING BLANKS                *         
*                 (REDUCED LENGTH RETURNED IN R0 AS FOR FUNCTION)     *         
*                                                                     *         
*Y2K  16.06.1997  STRIP IS YEAR 2000 COMPLIANT.                                 
*                                                                     *         
* CHANGE RECORD :                                                     *         
*   DATE :-        PURPOSE:-                                          *         
*                                                                     *         
***********************************************************************
* $Log:
*  1    Met_DB_Project 1.0         30/01/2006 20:24:42    Sheila Needham  
* $
* Revision 1.1  1997/08/05 15:46:27  uspm
* Initial revision
*
*
***********************************************************************         
STRIP    CSECT                                                                  
         USING *,15            ONLY REGISTERS 0,1,14 & 15 ARE USED.             
         LM    0,1,0(1)        ADDRESSES OF STRING & LENGTH.                    
         L     1,0(1)          LENGTH ITSELF.                                   
         AR    1,0             R1 NOW POINTS TO END OF STRING (PAST END         
         SR    0,1             TO GET LENGTH INTO R0, WHERE IT WILL BE          
         LPR   0,0              DECREMENTED IN LOOP & RETURNED.                 
         BCTR  1,0             POINT TO LAST CHAR RATHER THAN PAST END          
*                                                                               
TRYSPACE CLI   0(1),C' '       SPACE ?                                          
         BNER  14              IF NOT, RETURN.                                  
         BCTR  1,0             IF SO, POINT BACK ONE CHARACTER                  
         BCT   0,TRYSPACE      AND LOOP ON, ADJUSTING COUNT IN R0.              
         BR    14              IF NO NON-SPACE FOUND, RETURN LENGTH=0.
         DC    C'$Date: 30/01/2006 20:24:42$ $Revision: 1$'
         END                                                                    
