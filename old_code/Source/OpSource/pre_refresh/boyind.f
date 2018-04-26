      SUBROUTINE BOYIND(ARRAY,OCOR,DRGFLG,ENTRY)                        

!-----------------------------------------------------------------------        
!                                                                      
! PROGRAM       : BOYIND
!                                                                      
! PURPOSE       : TO MAKE INDEX ENTRY FOR STORAGE OF BUOY MESSAGES IN
!                 THE MDB                             
!                                                                      
! CALLED BY     : BOYENC                                                  
!                                                                      
! CALLS         : INDLALO                                           !1.7          
!                                                                      
! PARAMETERS    : ARRAY    EXPANDED ARRAY  (I)                            
!                 OCOR     CORRECTION REPORT FLAG(I)                      
!                 DRGFLG   DROGUE FLAG (I)                                
!                 ENTRY    INDEX ENTRY  (O)                               
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:06$
! $Source: /home/us0400/mdb/op/lib/source/RCS/boyind.F,v $
!                                                                      
! CHANGE RECORD :      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:06    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:32  usmdb
! Removed unused variable. Added copyright and modified
! header - S.Cox
!
! Revision 1.7  2000/03/10  09:35:50  09:35:50  usmdb (Generic MetDB acc
! 20 March 2000     C Long
! 1.7  Call INDLALO to put lat/long in index entry
! 
! Revision 1.6  99/02/11  12:22:16  12:22:16  usmdb (Generic MDB account
! 15-02-1999. Increase decoded elements array from 400 to 600 to cope
! with large BUOY reports. Jon Lewthwaite
!
! Revision 1.5  99/01/14  14:00:24  14:00:24  usmdb (Generic MDB account
! boyind//cl    18 Jan 1999     C Long
! 1.5 Set missing lat/long correctly in index.
!
! Revision 1.4  98/09/16  16:10:49  16:10:49  usmdb (Generic MDB account
! 21/09/1998 Increase the size of the elements array to allow for
! large messages with many levels. Jon Lewthwaite
! v(G)=3 ev(G)=1
!
! Revision 1.3  97/11/04  09:10:50  09:10:50  usjl (Jon Lewthwaite)
! Change the array subscripts for Lat and Long after WMO code change. !A
!
! Revision 1.2  1997/07/31 09:12:47  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 11:04:02  uspm
! Initial revision
!
! INTRODUCED  : 02/11/94                                               
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
                                                                        
      CHARACTER*23   ENTRY                                              
      CHARACTER*132  HEAD
      REAL           ARRAY(0:600)                                 !1.6  
      INTEGER        HOUR,MINUTE                                    !2.0
      INTEGER        BUOYID                                             
      LOGICAL        OCOR,DRGFLG                                        
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/boyind.F,v $
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
                                                                        
      HOUR = ARRAY(10)                                                  
      MINUTE = ARRAY(12)                                                
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
*     BUOY ID                                                           
*                                                                       
**************************************************************          
                                                                        
        ENTRY(3:11) = '         '                                       
        BUOYID = ARRAY(2)                                               
        WRITE(ENTRY(3:7),'(I5)') BUOYID                                 
                                                                        
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
                                                                        
      CALL INDLALO(ENTRY,ARRAY(18),ARRAY(20))                      !1.7 
                                                                        
**************************************************************          
*                                                                       
*     SET FLAGS WITHIN BYTE 17 TO INDICATE DATA TYPE                    
*     IS BUOY AND IF WITH/WITHOUT DROGUE                                
*      BIT 1 SET IF BUOY HAS DROGUE                                     
*      BIT 2 SET TO INDICATE BUOY                                       
*                                                                       
**************************************************************          
                                                                        
      IF (DRGFLG) THEN                                                  
        ENTRY(17:17) = CHAR(3)                                          
      ELSE                                                              
        ENTRY(17:17) = CHAR(2)                                          
      ENDIF                                                             
                                                                        
                                                                        
      RETURN                                                            
      END                                                               
