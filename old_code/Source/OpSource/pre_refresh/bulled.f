      SUBROUTINE BULLED(ISTART,IEND,BULL)                               

!-----------------------------------------------------------------------
!                                                                       
! PROGRAM       : BULLED                                                
!                                                                       
! PURPOSE       : TO TIDY UP BULLETIN AFTER HEADER INFORMATION FOUND.   
!                                                                       
! DESCRIPTION   : FIGURE, LETTER, SPACE, SLASH, PLUS, EQUALS, PLUS      
!                 AND MINUS ARE ACCEPTABLE CHARACTERS.                  
!                 IF TWO SUCCESSIVE CHARACTERS ARE SPACE OR LESS,       
!                 REDUCE THEM TO A SINGLE SPACE.  REPLACE ANY OTHER     
!                 UNEXPECTED CHARACTER BY A SLASH.                      
!                                                                       
! CALLED BY     : BULHED,BTHBUL,CLMBUL,BUOY,NCMBUL,SRWBUL,STMBUL,       
!                 TESBUL                                                
!                                                                       
! CALLS         : NOTHING                                               
!                                                                       
! PARAMETERS    : (1) ISTART   STARTING POINT IN BULLETIN               
!                 (2) IEND     END POINT (RESET IF SPACES MERGED)       
!                 (3) BULL     REPORT DATA                              
!                                                                       
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:33$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bulled.F,v $
!
! CHANGE RECORD :                                                       
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:33    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:39  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  99/10/06  11:47:38  11:47:38  usmdb (Generic MetDB accou
! Change day 18-10-99 - S.Cox
! If BULLED finds a single LF (not preceeded by a CR), it is replaced
! by a single space. This is needed for the Thailand MetDB, where
! lines in a bulletin file are ended with a LF rather than CRCRLF
! (as is the case in the UKMO)
! 
! Revision 1.2  97/07/31  09:14:40  09:14:40  uspm (Pat McCormack)
! First revision for MVS
!
! Revision 1.1  1997/07/04 11:17:23  uspm
! Initial revision
!
! 03/06/96 (A.M)  REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY NEW     
!                 PARAMETER 'BULL'.                                    
!                                                                       
! JULY 94: REMOVE SPACE BEFORE EQUAL SIGN                             
!
! JULY 93: ACCEPT + AND - AS WELL                                     
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                        
      CHARACTER*(*)  BULL                                               
      CHARACTER*1    CH                                                 
      CHARACTER*132  HEAD

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bulled.F,v $
     &'//'$ $Date: 30/01/2006 20:21:33$ $Revision: 1$'

!-----------------------------------------------------------------------
! TWO POINTERS TO THE SAME STRING ARE USED, IN & N (INPUT & OUTPUT).    
! N IS NOT ADVANCED WHEN A CHARACTER IS SKIPPED, SO CAN BE LESS THAN IN.
!-----------------------------------------------------------------------

      N=ISTART                                                          
                                                                        
      DO 100 IN=ISTART,IEND                                             
      IF (BULL(IN:IN).LE.' ' .AND. BULL(IN+1:IN+1).LE.' ') THEN         
        BULL(IN+1:IN+1)=' '                                             
      ELSE IF (BULL(IN:IN).LE.' ' .AND. BULL(IN+1:IN+1).EQ.'=') THEN    
        CONTINUE                                                        
      ELSE                                                              
        CH=BULL(IN:IN)                                                  
        IF (CH.EQ.' ' .OR. CH.EQ.'/' .OR. CH.EQ.'='                     
     &            .OR. CH.EQ.'+' .OR. CH.EQ.'-'                         
     &            .OR. (CH.GE.'A'.AND.CH.LE.'I')                        
     &            .OR. (CH.GE.'J'.AND.CH.LE.'R')                        
     &            .OR. (CH.GE.'S'.AND.CH.LE.'Z')                        
     &            .OR. (CH.GE.'0'.AND.CH.LE.'9')) THEN                  
          BULL(N:N)=CH                                                  

!-----------------------------------------------------------------------
! If the character < ' ', e.g. single LF, substitute it with a ' '  !1.3
!-----------------------------------------------------------------------

        ELSEIF (CH.LT.' ') THEN                                     !1.3
          BULL(N:N)=' '                                             !1.3

        ELSE                                                            
          BULL(N:N)='/'                                                 
        ENDIF                                                           
        N=N+1                                                           
      ENDIF                                                             
 100  CONTINUE                                                          
                                                                        
      IEND=N                                                            

      RETURN                                                            
      END                                                               
