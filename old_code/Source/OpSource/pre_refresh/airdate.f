      SUBROUTINE AIRDATE(BHOUR,BDAY,BMONTH,BYEAR)                               

!----------------------------------------------------------------------         
!                                                                               
! PROGRAM       : AIRDATE
!                                                                               
! PURPOSE       : To decide the bulletin month & year for aircraft data         
!                 (so that report day can then be found by going back           
!                 up to 24 hours from bulletin day/hour)                        
!                                                                               
! DESCRIPTION   : The month from the system clock is the default, but           
!                 if the current time is after 23Z and the bulletin is          
!                 for 0Z on the 1st, the date is next month if the 1st          
!                 is tomorrow; or if the bulletin date/hour is later            
!                 than the next hour set the date to last month.                
!                                                                               
! DATA TYPE(S)  : AMDAR (& AIREP?)                                              
!                                                                               
! CALLED BY     : AMDEXC (& AIR...?) once for each bulletin                     
!                                                                               
! CALLS         : ZPDATE                                                        
!                                                                               
! PARAMETERS    : (1) bulletin hour                                 (i)         
!                 (2) bulletin day                                  (i)         
!                 (3) bulletin month                                (o)         
!                 (4) bulletin year                                 (o)         
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:37$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airdate.F,v $
!                                                                               
! CHANGE RECORD :                                                               
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:37    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:19  usmdb
! Added copyright and modified header and comments - S.Cox
!
! Revision 1.2  97/11/20  14:57:39  14:57:39  usjl (Jon Lewthwaite)
! no entry
! 
! Nov 97 - made to replace bad code in AMDEXC                                 
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

      INTEGER BHOUR                                                             
      INTEGER BDAY                                                              
      INTEGER BMONTH                                                            
      INTEGER BYEAR                                                             
                                                                                
      INTEGER NOW(8)                                                            
      INTEGER CENDAY                                                            
      INTEGER NEXT_DAY                                                          
      INTEGER NEXT_MONTH                                                        
      INTEGER NEXT_YEAR                                                         
      LOGICAL TOMORROW                                                          
      CHARACTER*132 HEAD                                                                          

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airdate.F,v $
     &'//'$ $Date: 30/01/2006 20:20:37$ $Revision: 1$'

!-----------------------------------------------------------------------
! Set the month and year from the bulletin day and the current time.            
! A bulletin may be for tomorrow if it comes in just before midnight,           
! and tomorrow may be next month; otherwise it's for last month if              
! day/hour greater than current.                                                
!-----------------------------------------------------------------------
                                                                                
!-----------------------------------------------------------------------
! First set current month as default                                            
!-----------------------------------------------------------------------
                                                                                
      CALL DATIM(NOW)                                                           
      BYEAR=NOW(8)                                                              
      BMONTH=NOW(7)                                                             
                                                                                
!-----------------------------------------------------------------------
! If bulletin is for 0Z on the 1st and it's after 23Z now, see if               
! the 1st is tomorrow: if so, the bulletin is for next month.                   
!-----------------------------------------------------------------------
                                                                                
      IF (BDAY.EQ.1 .AND. BHOUR.EQ.0 .AND.                                      
     &  NOW(6).GE.28 .AND. NOW(5).EQ.23) THEN                                   
        CALL DATE31(NOW(6),BMONTH,BYEAR,CENDAY)                                 
        CENDAY=CENDAY+1                                                         
        CALL DATE13(CENDAY,NEXT_DAY,NEXT_MONTH,NEXT_YEAR)                       
        IF (NEXT_DAY.EQ.1) THEN                                                 
          BYEAR=NEXT_YEAR                                                       
          BMONTH=NEXT_MONTH                                                     
          TOMORROW=.TRUE.                                                       
        ELSE                                                                    
          TOMORROW=.FALSE.                                                      
        ENDIF                                                                   
      ELSE                                                                      
        TOMORROW=.FALSE.                                                        
      ENDIF                                                                     
                                                                                
!-----------------------------------------------------------------------
! If bulletin day greater than current day but not tomorrow,                    
! or same day but hour too far ahead for data to be a few minutes               
! early (though that shouldn't happen with aircraft data!),                     
! then must be last month.                                                      
!-----------------------------------------------------------------------
                                                                                
      IF (.NOT.TOMORROW .AND. (BDAY.GT.NOW(6) .OR.                              
     &    (BDAY.EQ.NOW(6).AND.BHOUR.GT.NOW(5)+1))) THEN                         
        BMONTH=BMONTH-1                                                         
        IF (BMONTH.EQ.0) THEN                 ! back to December?               
          BMONTH=12                                                             
          BYEAR=BYEAR-1                       ! if so, last year                
        ENDIF                                                                   
      ENDIF                                                                     

      RETURN                                                                    
      END                                                                       
