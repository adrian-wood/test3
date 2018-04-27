      SUBROUTINE NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,                  
     &                  HOUR,MIN,LAT,LON)                           !2.0
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM    : NCMIND                                                  
!                                                                      
! PURPOSE    : TO MAKE INDEX ENTRY FOR STORAGE                         
!              OF NCM MESSAGES IN THE MDB                              
!              INDEX TYPE - 23 BYTE CHAINED                            
!                                                                      
! CALLED BY  : NCMBUL                                                  
!                                                                      
! CALLS      : INDLALO                                              !1.4
!                                                                      
! PARAMETERS : ENTRY    INDEX ENTRY           (OUTPUT)                 
!              OCOR     COR FLAG              (INPUT)                  
!              OBHOUR   HOUR OF OBSERVATION   (INPUT)                  
!              OBMIN    MINUTE OF OBSERVATION (INPUT)                  
!              LAT      LATITUDE              (INPUT)                  
!              LON      LONGITUDE             (INPUT)                  
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:44$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ncmind.F,v $
!                                                                      
! CHANGE RECORD :                                                     
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:44    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:40  usmdb
! Removed unused dummy argument STNID. Added copyright and modified
! header - S.Cox
!
! Revision 1.4  2000/03/10  09:36:48  09:36:48  usmdb (Generic MetDB acc
! 20 March 2000      C Long
! 1.4  Call INDLALO to put lat/long in index entry
! 
! Revision 1.3  99/01/14  14:00:52  14:00:52  usmdb (Generic MDB account
! 18 Jan 1999     C Long
! 1.3 Set missing lat/long correctly in index.
!
! Revision 1.2  97/07/31  09:31:45  09:31:45  uspm (Pat McCormack)
! First revision for  1
!
! Revision 1.1  1997/07/04 13:04:00  uspm
! Initial revision
!
! INTRODUCED  : 06/03/95                                              
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
                                                                        
      CHARACTER*23   ENTRY                                              
      CHARACTER*2    CORNUM                                             
      CHARACTER*4    CCCC                                               
      CHARACTER*6    TTAAII                                             
      CHARACTER*132  HEAD
      INTEGER        HOUR,MIN                                           
      REAL           LAT,LON                                            
      LOGICAL        OCOR                                               
                                                                        
*                                                                       
*   THE 23 BYTE CHAINED REPORT NON-SATELLITE TRAILER (REQ'D BY TAFREP)  
*                                                                       
* ______________________________________________________________        
* : COR  : FINE : HOUR : MINUTE :TTAAII(3:6):CORNUM : CCCC  : NUMBER :  
* :      : MESH :   6  :    1   :           :       :       : OF OBS :  
* : FLAG : FLAG : BITS :  BYTE  :4 BYTES    :1 BYTE :4 BYTE : 1 BYTE :  
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~      
* ---------1-----------|---2----|--3 4 5 6--|---7---|-8--11-|---12---|  
*                                                                       
* _____________________________________________________________         
* : LATITUDE : LONGITUDE : REPORT. TYPE  : TOR : REC : BLOCK   :        
* :          :           : FLAGS . FLAGS :     : NUM : NUMBER  :        
* : 2 BYTES  : 2 BYTES   : 4 BITS.4 BITS :     :     :         :        
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~         
* |--13 14---|---15 16---|------17-------|18-19|20-21|--22-23--|        
*                                                                       
                                                                        
**************************************************************          
*                                                                       
*     TAFREP WILL TAKE CARE OF TOR, RECNUM, BLOCKNUM & OVERFLOWS        
*                                                                       
**************************************************************          

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ncmind.F,v $
     &'//'$ $Date: 30/01/2006 20:23:44$ $Revision: 1$'
                                                                        
                                                                        
**************************************************************          
*                                                                       
*     INITIALISE INDEX TO BLANKS                                        
*                                                                       
*     IF COR FLAG HAS BEEN SET THEN SET APPROPRIATE BIT IN INDEX         
*     AS WELL AS HOUR PARAMETER                                         
*                                                                       
*     SET MINUTE                                                        
*                                                                       
**************************************************************          
                                                                        
                                                                        
      ENTRY(1:23) = '                       '                           
                                                                        
      IF (OCOR) THEN                                                    
        ENTRY(1:1) = CHAR(HOUR+128)                                     
      ELSE                                                              
        ENTRY(1:1) = CHAR(HOUR)                                         
      ENDIF                                                             
                                                                        
      ENTRY(2:2) = CHAR(MIN)                                            
                                                                        
**************************************************************          
*                                                                       
*     STORE TTAAII,CORNUM,CCCC                                          
*                                                                       
**************************************************************          
                                                                        
      ENTRY(3:6) = TTAAII(3:6)                                          
      ENTRY(7:7) = CORNUM(2:2)                                          
      ENTRY(8:11) = CCCC                                                
                                                                        
**************************************************************          
*                                                                       
*     NUMBER OF OBS                                                     
*                                                                       
**************************************************************          
                                                                        
      ENTRY(12:12) = CHAR(1)                                            
                                                                        
**************************************************************          
*                                                                       
*     LAT AND LONG STORED IN INDEX IN HUNDREDTHS OF DEGREE               
*                                                                       
**************************************************************          
                                                                        
      CALL INDLALO(ENTRY,LAT,LON)                                  !1.4
                                                                        
      RETURN                                                            
      END                                                               
