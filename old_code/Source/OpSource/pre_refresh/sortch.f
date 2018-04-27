      SUBROUTINE SORTCH(C,L,N,MASK)                                             

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : SORTCH                                                       
!                                                                              
! PURPOSE       : TO SORT A CHARACTER ARRAY ON FIELDS DEFINED IN THE           
!                 MASK INTO ASCENDING ORDER                                    
!                                                                              
! DESCRIPTION   : THIS IS A FORTRAN VERSION OF VARIOUS SORTS BASED             
!                 ON THE KDF9 PROGRAM L82.  IT USES A "DIMINISHING             
!                 INCREMENT" METHOD BASED ON FRANK & LAZARUS (COMM             
!                 A.C.M.,JAN 1960).  FOR A GENERAL DISCUSSION SEE              
!                 KNUTH: "ART OF COMPUTER PROGRAMMING: SORTING AND             
!                 SEARCHING" (1973) -  SHELF MARK EC5B(43).                    
!                                                                              
! CALLED BY     : VARIOUS
!                                                                              
! PARAMETERS    : (1) CHAR*L(N): N STRINGS OF LENGTH L                         
!                 (2) LENGTH OF EACH STRING                                    
!                 (3) NUMBER OF ITEMS TO BE SORTED                             
!                 (4) MASK WITH BLANKS FOR CHARACTERS TO BE IGNORED            
!                                                                              
!Y2K  16.06.1997  SORTCH IS YEAR 2000 COMPLIANT.                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:22$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sortch.F,v $
!                                                                               
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:22    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:10  usmdb
! Move INTEGER declarations above other delclarations.
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/06/20  09:01:31  09:01:31  uspm (Pat McCormack)
! Add IMPLICT NONE
! 
! Revision 1.1  1997/06/19 13:42:26  uspm
! Initial revision
!
! OCT 96 - CHANGE TO CHARACTER VARIABLE DECLARATION FROM NON-STANDRD         
!          TO STANDARD (PORTABLE) DECLARATION.                     !A         
!
! DEC 95 - SWAPPING ONE CHARACETR AT A TIME IS TOO EXPENSIVE: SWAP           
!          WHOLE STRINGS, WHICH MEANS LENGTH OF STRING USED IN SWAP          
!          RESTRICTS GENERALITY OF SORT - ERROR IF STRINGS TOO LONG.         
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE                                             
                                                                                
      INTEGER       X
      INTEGER       L
      INTEGER       M
      INTEGER       N
      INTEGER       K
      INTEGER       I
      INTEGER       J
      INTEGER       MINVAL
      INTEGER       MAXVAL

      CHARACTER*(*) C(N), MASK                                       !A         
      CHARACTER     CSWAP*500 
      CHARACTER     HEAD*132                                                  
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sortch.F,v $
     &'//'$ $Date: 30/01/2006 20:24:22$ $Revision: 1$'                                                           
                                                                               
! GIVE UP IF INPUT STRINGS TOO LONG FOR FIXED-LENGTH SWAP STRING                
                                                                               
      IF (L.GT.500) THEN                                                        
        PRINT *,' STRINGS TOO LONG FOR SORT'                                    
        RETURN                                                                  
      ENDIF                                                                     
                                                                               
! THE MASK MAY START AND/OR END WITH BLANKS.  IN COMPARING STRINGS IT'S         
! ENOUGH TO LOOP FROM FIRST NON-BLANK TO LAST RATHER THAN FROM 1 TO L.          
                                                                               
      MINVAL=1                                                                     
      DO WHILE (MASK(MINVAL:MINVAL).EQ.' ')                                           
        MINVAL=MINVAL+1                             
      ENDDO                                                                     
                                                                               
      MAXVAL=L                                                                     
      DO WHILE (MASK(MAXVAL:MAXVAL).EQ.' ')                                           
        MAXVAL=MAXVAL-1                                                               
      ENDDO                                                                     
                                                                               
! FIRST SET THE INCREMENT X.  THIS WILL BE QUARTERED OR HALVED AS THE           
! SORT GOES ON, BEING ROUNDED UP TO AN ODD NUMBER TO GIVE A DECREASING          
! SEQUENCE ENDING WITH 1.  STRINGS WITH SUBSCRIPTS X APART WILL BE              
! COMPARED AND SWAPPED IF OUT OF ORDER.                                         
                                                                               
      X=N                                                                       
   10 X=X/2                                                                     
      IF (X.GE.8) X=X/2                                                         
      IF (MOD(X,2).EQ.0) X=X+1                                                  
                                                                               
! DO N MINUS X COMPARISONS FOR EACH INCREMENT.  IF A SWAP IS DONE,              
! REPEAT THE COMPARISON INVOLVING THE ITEM JUST MOVED UP.                       
                                                                               
      DO 50 K=1,N-X                                                             
       I=K                                                                      
   20  J=I+X                                                                    
                                                                               
! WHERE THE MASK IS SET, COMPARE CHARACTERS IN THE TWO ITEMS IN TURN,           
! WORKING FROM LEFT TO RIGHT (I.E. THE ORDER OF IMPORTANCE OF FIELDS            
! IN THE MASK IS SIMPLY THEIR ORDER IN THE ITEMS TO BE SORTED), AND IF          
! NECESSARY SWAP THE STRINGS.                                                   
                                                                               
       DO 30 M=MINVAL,MAXVAL                                                          
        IF (MASK(M:M).NE.' ') THEN                                              
          IF (C(I)(M:M).GT.C(J)(M:M)) THEN                                      
            CSWAP=C(I)                                                          
            C(I)=C(J)                                                           
            C(J)=CSWAP                                                          
                                                                               
! IF A COMPARISON X BACK CAN BE REPEATED, DO SO.  IN THAT WAY                   
! EACH SUBSET OF ITEMS X APART WILL BE PUT INTO ORDER.                          
                                                                               
            IF (I.GT.X) THEN                                                    
              I=I-X                                                             
              GO TO 20                                                          
            ELSE                                                                
              GO TO 50                                                          
            ENDIF                                                               
          ELSE IF (C(I)(M:M).LT.C(J)(M:M)) THEN                                 
            GO TO 50                                                            
          ENDIF                                                                 
        ENDIF                                                                   
   30  CONTINUE                                                                 
   50 CONTINUE                                                                  
                                                                               
! REPEAT THE PROCESS WITH A SMALLER INCREMENT UNLESS THE INCREMENT              
! HAS REACHED 1, IN WHICH CASE THE SORT HAS BEEN COMPLETED.                     
                                                                               
      IF (X.GT.1) GO TO 10                                                      

      RETURN                                                                    
      END                                                                       
