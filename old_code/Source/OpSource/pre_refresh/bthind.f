      SUBROUTINE BTHIND(EXPARR,IDFLAG,ID,OCOR,ENTRY)                    
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : BTHIND                                                  
!                                                                      
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE OF BATHY MESSAGES IN
!                 THE MDB                            
!                                                                      
! CALLED BY     : BTHENC                                                  
!                                                                      
! CALLS         : INDLALO                                           !1.3
!                                                                      
! PARAMETERS    : EXPARR   EXPANDED ARRAY  (I)                            
!                 IDFLAG   FLAG FOR SHIP ID (I)                           
!                 ID       SHIPS ID  (I)                                  
!                 OCOR     CORRECTION REPORT FLAG  (I)                    
!                 ENTRY    INDEX ENTRY  (O)                               
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:06$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bthind.F,v $
!
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:06    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:36  usmdb
! Removed unused variables. Added copyright and modified
! header - S.Cox
!
! Revision 1.3  2000/03/10  09:36:08  09:36:08  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! 1.3  Call INDLALO to put lat/long in index entry
! 
! Revision 1.2  97/07/31  09:13:27  09:13:27  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 11:07:20  uspm
! Initial revision
!
! MAY 95 - BLANK OUT REST OF IDENT IF ONLY 5 FIGURES                  
!                                                                      
! INTRODUCED : 11/07/94                                                
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
                                                                        
      IMPLICIT NONE                                                     
                                                                        
      CHARACTER*9    ID                                                 
      CHARACTER*23   ENTRY                                              
      CHARACTER*132  HEAD
      REAL           EXPARR(0:1000)                                     
      INTEGER        HOUR,MINUTE                                        
      INTEGER        BUOYID                                         !2.0
      LOGICAL        OCOR,IDFLAG                                        

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bthind.F,v $
     &'//'$ $Date: 30/01/2006 20:21:06$ $Revision: 1$'

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
* : LATITUDE : LONGITUDE : REPORT TYPE   : TOR : REC : BLOCK  :         
* :          :           : FLAGS . FLAGS :     : NUM : NUMBER :         
* : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :        :         
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~         
* 12        14          16              17    19    21       23         
*                                                                       
                                                                        
*************************************************************           
*                                                                       
*     AMDSTO WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM                    
*                                                                       
*************************************************************           
                                                                        
*************************************************************           
*                                                                       
*     INSERT TIME ELEMENTS                                              
*                                                                       
*************************************************************           
                                                                        
      HOUR = EXPARR(12)                                                 
      MINUTE = EXPARR(14)                                               
      ENTRY(2:2) = CHAR(MINUTE)                                         
                                                                        
*************************************************************           
*                                                                       
*     IF COR FLAG HAS BEEN SET THEN SET APPRORIATE BIT IN               
*     INDEX AS WELL AS HOUR PARAMETER                                   
*                                                                       
*************************************************************           
                                                                        
      IF (OCOR) THEN                                                    
        ENTRY(1:1) = CHAR(HOUR+128)                                     
      ELSE                                                              
        ENTRY(1:1) = CHAR(HOUR)                                         
      ENDIF                                                             
                                                                        
*************************************************************           
*                                                                       
*     STORE SHIP/BUOY ID (PAD WITH BLANKS IF 5-FIGURE NUMBER)           
*                                                                       
*************************************************************           
                                                                        
      IF (IDFLAG) THEN                                                  
        ENTRY(3:11) = ID                                                
      ELSE                                                              
        BUOYID = EXPARR(4)                                              
        WRITE(ENTRY(3:7),'(I5)') BUOYID                                 
        ENTRY(8:11) = ' '                                               
      ENDIF                                                             
                                                                        
*************************************************************           
*                                                                       
*     NUMBER OF OBS/REPORTS                                             
*                                                                       
*************************************************************           
                                                                        
      ENTRY(12:12) = CHAR(1)                                            
                                                                        
*************************************************************           
*                                                                       
*     LAT AND LONG STORED IN INDEX IN HUNDRETHS OF DEGREE               
*                                                                       
*************************************************************           
                                                                        
      CALL INDLALO(ENTRY,EXPARR(16),EXPARR(18))                   !1.3  
                                                                        
      RETURN                                                            
      END                                                               
