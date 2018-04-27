      SUBROUTINE REPEDT(BULL,REPORT,REPLEN)                             
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : REPEDT                                               
!                                                                      
! PURPOSE       : TO TIDY UP A SINGLE REPORT  (NOT EDITING IN PLACE)   
!                                                                      
! DESCRIPTION   : FIGURE, LETTER, SPACE, SLASH, EQUALS, PLUS           
!                 AND MINUS ARE ACCEPTABLE CHARACTERS.                 
!                 IF TWO SUCCESSIVE CHARACTERS ARE SPACE OR LESS,      
!                 REDUCE THEM TO A SINGLE SPACE.  REPLACE ANY OTHER    
!                 UNEXPECTED CHARACTER BY A SLASH.                     
!                                                                      
! CALLED BY     : SYNBUL                                               
!                                                                      
! PARAMETERS    : (1) BULL     REPORT IN BULLETIN           (I)        
!                 (2) REPORT   EDITED REPORT                (O)        
!                 (3) REPLEN   REPORT LENGTH (MAY BE RESET) (I/O)      
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:00$
! $Source: /home/us0400/mdb/op/lib/source/RCS/repedt.F,v $
!                                                                      
! CHANGE RECORD :
!             
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:00    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:46  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:32:44  09:32:44  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:09:22  uspm
! Initial revision
!
! MADE FROM BULLED, BUT DOESN'T PASS BULLETIN IN COMMON & DOESN'T
! EDIT IN PLACE.
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

      CHARACTER*(*) BULL,REPORT                                         
      CHARACTER*1 CH
      CHARACTER*132 HEAD
      
      INTEGER REPLEN                                                    

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/repedt.F,v $
     &'//'$ $Date: 30/01/2006 20:24:00$ $Revision: 1$'
      
*                                                                       
      N=0                    ! POINTER TO OUTPUT REPORT                 
*                                                                       
      DO 100 IN=1,REPLEN                                                
      IF (IN.LT.REPLEN .AND.                                            
     -    BULL(IN:IN).LE.' ' .AND. BULL(IN+1:IN+1).LE.' ') THEN         
        BULL(IN+1:IN+1)=' '                                             
      ELSE IF (IN.LT.REPLEN .AND.                                       
     -    BULL(IN:IN).LE.' ' .AND. BULL(IN+1:IN+1).EQ.'=') THEN         
        CONTINUE                                                        
      ELSE                                                              
        CH=BULL(IN:IN)                                                  
        N=N+1                                                           
        IF (CH.EQ.' ' .OR.                                              
     -      CH.EQ.'/' .OR.                                              
     -      CH.EQ.'=' .OR.                                              
     -      CH.EQ.'+' .OR.                                              
     -      CH.EQ.'-' .OR.                                              
     -     (CH.GE.'A'.AND.CH.LE.'I') .OR.                               
     -     (CH.GE.'J'.AND.CH.LE.'R') .OR.                               
     -     (CH.GE.'S'.AND.CH.LE.'Z') .OR.                               
     -     (CH.GE.'0'.AND.CH.LE.'9')) THEN                              
          REPORT(N:N)=CH                                                
        ELSE                                                            
          REPORT(N:N)='/'                                               
        ENDIF                                                           
      ENDIF                                                             
 100  CONTINUE                                                          
*                                                                       
      REPLEN=N                                                          
      RETURN                                                            
      END                                                               
