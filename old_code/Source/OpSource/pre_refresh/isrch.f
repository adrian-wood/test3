      SUBROUTINE ISRCH (NUMBER, LIST, NSIZE, NPOS)

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM     : ISRCH                                                 
!                                                                     
! PURPOSE     : TO FIND A GIVEN INTEGER IN AN ARRAY OF INTEGERS       
!               ALREADY SORTED INTO INCREASING NUMERICAL ORDER.       
!                                                                     
! DESCRIPTION : USING A BINARY SEARCH PROCEDURE, THE LOCATION OF THE  
!               GIVEN INTEGER IN THE LIST IS FOUND AND RETURNED TO    
!               THE CALLING PROGRAM.  IF THE INTEGER IS NOT IN THE    
!               LIST, A ZERO OR NEGATIVE VALUE IS RETURNED.           
!                                                                     
! USAGE       : CALL ISRCH (NUMBER, LIST, NSIZE, NPOS)                
!                                                                     
! PARAMETERS  : NUMBER  (I)  INTEGER TO BE LOCATED IN LIST            
!               LIST    (I)  ARRAY OF INTEGERS TO BE SEARCHED         
!               NSIZE   (I)  NUMBER OF INTEGERS IN LIST               
!               NPOS    (O)  LOCATION OF NUMBER IN LIST               
!                                                                     
!               IF THE INTEGER IS NOT IN THE LIST AND LIES BETWEEN    
!               THE NTH AND (N+1)TH ELEMENTS, THE VALUE RETURNED IN   
!               "NPOS" IS -N ("N" IN RANGE 0 TO NSIZE).               
!                                                                     
! CALLED BY   : STAPOS (OR OTHER USER'S PROGRAM)                      
!                                                                     
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:02$
! $Source: /home/us0400/mdb/op/lib/source/RCS/isrch.F,v $
!
! CHANGE RECORD :
!                                                                     
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:02    Sheila Needham  
! $
! Revision 2.0  2001/07/03 11:08:36  usmdb
! Moved declaration of NSIZE before declaration of array of size
! NSIZE. Added copyright and modified header - S.Cox
!
! Revision 1.1  99/06/10  14:43:34  14:43:34  usmdb (Generic MetDB account)
! Initial revision
! 
! ORIGINAL VERSION BASED ON "SATYPE" (WHICH DOES A SIMILAR BINARY
! SEARCH IN CHARACTER ARRAYS).           
!               
! WRITTEN BY BRIAN BARWELL, 5 MAY 1999.
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
!
      IMPLICIT NONE
!                                                     DECLARE VARIABLES
!
      INTEGER IFIRST      ! ) First and last ranges during binary
      INTEGER ILAST       ! )   search (they home in on NPOS)
      INTEGER NSIZE       !  Number of integers in list             !2.0
      INTEGER LIST(NSIZE) !  Array of integers to be searched       !2.0
      INTEGER NPOS        !  Location in list of target integer
      INTEGER NUMBER      !  Target integer
!
      CHARACTER*132 HEAD  !  For revision information
!-----------------------------------------------------------------------
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/isrch.F,v $
     &'//'$Date: 30/01/2006 20:23:02$ $Revision: 1$'
!                                       INITIALISE TO COVER WHOLE ARRAY
      IFIRST = 0
      ILAST  = NSIZE
!                                  LOOP UNTIL RANGE REDUCED TO 1 NUMBER
      DO WHILE (IFIRST.LT.ILAST)
!                                         CHOOSE ELEMENT NEAR MID-RANGE
         NPOS = (IFIRST + ILAST + 1) / 2
!                                           COMPARE TARGET WITH CURRENT
!                                          LIMITS AND RESET ONE OF THEM
         IF (NUMBER.LT.LIST(NPOS)) THEN
            ILAST = NPOS - 1
         ELSE
            IFIRST = NPOS
         END IF
      END DO
!                 CHECK THAT THE TARGET NUMBER AGREES WITH THE ENTRY IN
!                    THE LIST (SIGN OF "NPOS" IS CHANGED IF IT DOESN'T)
      NPOS = IFIRST
      IF (NPOS.EQ.0 .OR. NUMBER.NE.LIST(NPOS)) NPOS = -NPOS
      RETURN
      END
