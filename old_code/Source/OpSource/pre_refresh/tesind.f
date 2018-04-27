      SUBROUTINE TESIND(EXPARR,IDFLG,ID,OCOR,ENTRY)                     
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! ROUTINE       : TESIND                                                  
!                                                                      
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE                         
!                 OF TESAC MESSAGES IN THE MDB                            
!                                                                      
! CALLED BY     : TESENC                                                  
!                                                                      
! CALLS         : INDLALO                                          !1.3a          
!                                                                      
! PARAMETERS    : EXPARR   EXPANDED ARRAY  (I)                            
!                 IDFLG    CALL SIGN FLAG (I)                             
!                 ID       SHIPS CALL SIGN  (I)                           
!                 OCOR     CORRECTION REPORT FLAG(I)                      
!                 ENTRY    INDEX ENTRY  (O)                               
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:19$
! $Source: /home/us0400/mdb/op/lib/source/RCS/tesind.F,v $
! 
! CHANGE RECORD :                                                     
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:19    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:14  usmdb
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  2000/03/10  09:37:45  09:37:45  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! 1.3  Put spaces after 5-figure identifier in index entry
!      (to blank out end of any previous longer identifier)
! 1.3a Call INDLALO to put lat/long in index entry 
! 
! Revision 1.2  97/07/31  11:42:50  11:42:50  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 14:27:38  uspm
! Initial revision
!
! INTRODUCED : 22/08/94                                              
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
                                                                        
      CHARACTER*9    ID                                                 
      CHARACTER*23   ENTRY         ! INDEX ENTRY                        
      CHARACTER*132  HEAD
      REAL           EXPARR(0:1000)                                     
      INTEGER        HOUR,MINUTE                                        
      INTEGER        BUOYID                                         !2.0
      LOGICAL        OCOR,IDFLG                                         
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/tesind.F,v $
     &'//'$ $Date: 30/01/2006 20:25:19$ $Revision: 1$'
                                                                        
*                                                                       
*   THE SINGLE REPORT NON-SATELLITE INDEX.                              
*                                                                       
* ____________________________________________________                  
* : COR  : FINE : HOUR : MINUTE : BUOY ID/  : NUMBER :                  
* :      : MESH :   6  :    1   : CALL SIGN : OF OBS :                  
* : FLAG : FLAG : BITS :  BYTE  : 9 BYTES   : 1 BYTE :                  
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~                  
* 0                    1       2           11       12                  
*                                                                       
* _____________________________________________________________         
* : LATITUDE : LONGITUDE : REPORT. TYPE  : TOR : REC : BLOCK  :         
* :          :           : FLAGS . FLAGS :     : NUM : NUMBER :         
* : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :        :         
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~         
* 12        14          16              17    19    21       23         
*                                                                       
                                                                        
**************************************************************          
*                                                                       
*     AMDSTO WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM                    
*                                                                       
**************************************************************          
                                                                        
      HOUR = EXPARR(12)                                                 
      MINUTE = EXPARR(14)                                               
      ENTRY(2:2) = CHAR(MINUTE)                                         
                                                                        
**************************************************************          
*                                                                       
*     IF COR FLAG HAS BEEN SET THEN SET APPRORIATE BIT IN INDEX         
*     AS WELL AS HOUR PARAMETER                                         
*                                                                       
**************************************************************          
                                                                        
      IF (OCOR) THEN                                                    
        ENTRY(1:1) = CHAR(HOUR+128)                                     
      ELSE                                                              
        ENTRY(1:1) = CHAR(HOUR)                                         
      ENDIF                                                             
                                                                        
**************************************************************          
*                                                                       
*     STORE SHIP/BUOY ID                                                
*                                                                       
**************************************************************          
                                                                        
      IF (IDFLG) THEN                                                   
        ENTRY(3:11) = ID                                                
      ELSE                                                              
        BUOYID = EXPARR(4)                                              
        WRITE(ENTRY(3:7),'(I5)') BUOYID                                 
        ENTRY(8:11)=' '                                           !1.3  
      ENDIF                                                             
                                                                        
**************************************************************          
*                                                                       
*     NUMBER OF OBS                                                     
*                                                                       
**************************************************************          
                                                                        
      ENTRY(12:12) = CHAR(1)                                            
                                                                        
**************************************************************          
*                                                                       
*     LAT AND LONG STORED IN INDEX IN HUNDRETHS OF DEGREE               
*                                                                       
**************************************************************          
                                                                        
      CALL INDLALO(ENTRY,EXPARR(16),EXPARR(18))                   !1.3a 
                                                                        
**************************************************************          
*                                                                       
*     SET FLAGS WITHIN BYTE 17 TO INDICATE WHETHER DATA TYPE            
*     IS BATHY OR TESAC (STORED IN SAME DATASET)                        
*     IN BIT 1 PUT 1 IF TESAC (0 IF BATHY)                              
*                                                                       
**************************************************************          
                                                                        
      ENTRY(17:17) = CHAR(1)                                            
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
