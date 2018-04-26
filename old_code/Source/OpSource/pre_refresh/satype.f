      SUBROUTINE SATYPE (TEXT, LIST1, LIST2, NTYPES, NUMTYP)                    

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : SATYPE                                                       
!                                                                              
! PURPOSE       : TO SEARCH A SEQUENCE OF RANGES OF CHARACTER STRINGS          
!                 (E.G. BULLETIN HEADERS OR DATA TYPE NAMES) TO FIND           
!                 THE ONE CORRESPONDING TO A GIVEN STRING.                     
!                                                                              
! DESCRIPTION   : A CHARACTER STRING IS COMPARED WITH A BATCH OF NON-          
!                 OVERLAPPING RANGES AND THE VALUE IS RETURNED OF THE          
!                 RANGE CONTAINING THE SPECIFIED STRING. RANGES ARE            
!                 DEFINED BY THEIR FIRST AND LAST MEMBERS AND MUST BE          
!                 IN ALPHANUMERICAL ORDER. SEARCHING IS DONE USING             
!                 SUCCESSIVE HALVING OF THE SEARCH WINDOW.                     
!                 IF THE SPECIFIED STRING DOES NOT OCCUR IN ANY OF             
!                 THE RANGES, A ZERO OR NEGATIVE VALUE IR RETURNED.            
!                                                                              
! CALLED BY     : SATIND                                                       
!                                                                              
! PARAMETERS    : (1) CHARACTER STRING TO BE SEARCHED FOR.                     
!                 (2) ARRAY OF FIRST CHARACTER STRINGS IN EACH RANGE.          
!                 (3) ARRAY OF LAST CHARACTER STRINGS IN EACH RANGE.           
!                 (4) NUMBER OF RANGES (= DIMENSION OF ABOVE ARRAYS).          
!                 (5) NUMBER OF RANGE CONTAINING SPECIFIED STRING              
!                     (VALUE RETURNED BY THIS ROUTINE).                        
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/satype.F,v $
!
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:09    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:51  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/10/02  08:21:35  08:21:35  uspm (Pat McCormack)
! Initial revision
! 
! 8 APR 1997  ORIGINAL VERSION.                       (B.R.B.)               
!                                                                              
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE                                                             
*                                             DECLARE INTEGER VARIABLES         
*                                                                               
      INTEGER IFIRST      ! ) First and last ranges during binary               
      INTEGER ILAST       ! )   search (they home in on NUMTYP)                 
      INTEGER NTYPES      ! (Input) Number of search ranges                     
      INTEGER NUMTYP      ! (Output) Number of range containing TEXT            
*                                                                               
*                                           DECLARE CHARACTER VARIABLES         
*                                                                               
      CHARACTER*132 HEAD           ! Revision information text                  
      CHARACTER*(*) LIST1(NTYPES)  ! Array of beginnings of text ranges         
      CHARACTER*(*) LIST2(NTYPES)  ! Array of ends of text ranges               
      CHARACTER*(*) TEXT           ! (Input) Text to be searched for            
*-----------------------------------------------------------------------        
*                                                  REVISION INFORMATION         
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/satype.F,v $
     &'//'$ $Date: 30/01/2006 20:24:09$ $Revision: 1$'
*                                INITIALISE TO FULL RANGE OF DATA TYPES         
      IFIRST = 0                                                                
      ILAST  = NTYPES                                                           
*                                            CHOOSE TYPE NEAR MID-RANGE         
    1 CONTINUE
      NUMTYP = (IFIRST + ILAST + 1) / 2                                         
*                                        COMPARE WITH CURRENT DATA TYPE         
*                                          LIMITS AND RESET ONE OF THEM         
      IF (TEXT.LT.LIST1(NUMTYP)) THEN                                           
         ILAST = NUMTYP - 1                                                     
      ELSE                                                                      
         IFIRST = NUMTYP                                                        
      END IF                                                                    
*                                           SEARCH ENDS IF FIRST = LAST         
      IF (IFIRST.NE.ILAST) GO TO 1                                              
*                                                                               
*             CHECK UPPER LIMIT OF VALID BULLETIN HEADERS FOR DATA TYPE         
*           FOUND (SIGN OF "NUMTYP" IS CHANGED IF HEADER OUTSIDE RANGE)         
*                                                                               
      NUMTYP = IFIRST                                                           
      IF (NUMTYP.EQ.0 .OR. TEXT.GT.LIST2(NUMTYP)) NUMTYP = -NUMTYP              
      RETURN                                                                    
      END                                                                       
