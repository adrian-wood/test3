      INTEGER FUNCTION STDPRHT(BLOCK,STN,LEVEL)                         

!-----------------------------------------------------------------------
!                                                                       
! PROGRAM       : STDPRHT                                               
!                                                                       
! PURPOSE       : FIND HEIGHT EQUIVALENT TO STANDARD PRESSURE           
!                                                                       
! DESCRIPTION   : IF A PILOT BALLOON HAS A PRESSURE SENSOR, THEN ITS    
!                 PRESSURES ARE GENUINE. IF NOT, THEY ARE EQUIVALENT    
!                 TO HEIGHTS LISTED IN THE WMO MANUAL ON CODES.  BUT    
!                 THESE LISTS, REGIONAL & NATIONAL, DON'T COVER ALL     
!                 AREAS.  THE LISTS HERE, AGREED WITH B INGLEBY (MAY    
!                 1989) COMBINE THE 2 LISTS FOR EUROPE & ANTARCTICA,    
!                 DISTINGUISH BETWEEN NORTH & SOUTH IN AUSTRALASIA &    
!                 S AMERICA, AND ASSUME NO PILOTS DONE IN N AMERICA.    
!                                                                       
! DATA TYPE(S)  : PILOT A/C                                             
!                                                                       
! CALLED BY     : UASTDHT                                               
!                                                                       
! PARAMETERS    : (1) BLOCK NUMBER                                      
!                 (2) STATION NUMBER                                    
!                 (3) STANDARD PRESSURE                                 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:32$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stdprht.F,v $
!                                                                       
! CHANGE RECORD : 
!                                                                       
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:32    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:00  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:32:34  09:32:34  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:08:32  uspm
! Initial revision
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
      INTEGER BLOCK                                                     
      INTEGER STN                                                       
      INTEGER LIST(0:99)                                                
      INTEGER HT(14,8)                                                  
      INTEGER L,LEVEL                                                   
      CHARACTER*132 HEAD
!     WRITE(6,*)'IN PSTDHT BLOCK,STATION,LEVEL = ',BLOCK,STN,LEVEL      
*                                                                       
* ASSIGN ONE OF THE LISTS BELOW TO EACH WMO BLOCK.  A ZERO IN THE LIST  
* ARRAY MEANS EITHER THE BLOCK DOESN'T EXIST OR NO HEIGHT INFORMATION.  
* (NONE IS NEEDED IF NO PILOTS DONE OR PRESSURE SENSORS ALWAYS USED)    
* SOME BLOCKS NEED A FURTHER CHECK ON STATION NUMBER.                   
*                                                                       
      DATA LIST/                                                        
     - 0,6,6,6,6, 0,6,6,6,0, 6,6,6,6,6, 6,6,6,0,0,                      
     - 2,2,6,2,2, 2,6,6,2,2, 2,2,2,6,6, 2,2,6,2,2,                      
     - 2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2, 2,2,2,2,2,                      
     - 1,1,1,1,1, 1,1,1,1,0, 0,0,0,0,0, 0,0,0,0,0,                      
     - 3,3,3,3,3, 3,3,3,4,7, 5,5,5,8,5, 8,5,5,5,0/                      
*                                                                       
      DATA HT/                                                          
* LIST 1:   AFRICA (EXCEPT ALGERIA)                                     
     - 1500,3000,5700,7500,9600,10800,12300,14100,16500,                
     - 18600,20700,23400,25800,29700,                                   
* LIST 2:   ASIA                                                        
     - 1500,3100,5800,7600,9500,10600,12300,14100,16600,                
     - 18500,20500,24000,26500,31000,                                   
* LIST 3:   SOUTH AMERICA (NORTH OF 40S; ARGENTINA'S FIGURES)           
     - 1500,3000,5700,7500,9600,10500,12300,14100,16200,                
     - 18300,20700,23700,26400,30900,                                   
* LIST 4:   SOUTH AMERICA (SOUTH OF 40S; ARGENTINA'S FIGURES)           
     - 1500,3000,5400,7200,9000,10200,12000,13500,15900,                
     - 18300,20700,23700,26400,30900,                                   
* LIST 5:   SW PACIFIC (INCLUDING MALAYSIA, BUT NOT SOUTH OF 33S)       
     - 1500,3050,5750,7500,9550,10800,12250,14050,16450,                
     - 18700,20650,23900,26550,31200,                                   
* LIST 6:   EUROPE (INCLUDING MIDDLE EAST & ALGERIA)                    
     - 1500,3000,5450,7100,9000,10500,12000,13500,15950,                
     - 18400,20600,23600,26450,30950,                                   
* LIST 7:   ANTARCTICA                                                  
     - 1350,2850,5050,6550,8450,9675,10900,12550,14850,                 
     - 18400,20600,23600,26450,30950,                                   
* LIST 8:   SW PACIFIC (SOUTH OF 33S)                                   
     - 1500,3050,5650,7250,9250,10450,11900,13700,16250,                
     - 18550,20650,23950,26600,31300/                                   
*                                                                       
!     WRITE(6,*)'IN PSTDHT BLOCK,STATION,LEVEL = ',BLOCK,STN,LEVEL      

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stdprht.F,v $
     &'//'$ $Date: 30/01/2006 20:24:32$ $Revision: 1$'
      
      L=LIST(BLOCK)                                                     
      IF (BLOCK.EQ.20 .AND. STN.GE.100 .AND. STN.LT.200) L=6            
      IF (BLOCK.EQ.40 .AND. STN.LT.350) L=6                             
      IF (BLOCK.EQ.48 .AND. STN.GE.600 .AND. STN.LT.800) L=5            
      IF (BLOCK.EQ.60 .AND. STN.GE.350 .AND. STN.LT.700) L=6            
      IF (BLOCK.EQ.85 .AND. STN.GE.782) L=4                             
      IF (BLOCK.EQ.87 .AND. STN.GE.763) L=4                             
      IF (BLOCK.EQ.88 .AND. STN.GT.903) L=7                             
      IF (BLOCK.EQ.94 .AND. STN.GE.600) L=8                             
*                                                                       
      IF (L.NE.0) STDPRHT=HT(LEVEL,L)                                   
      IF (L.EQ.0) STDPRHT=-9999999                                      
      RETURN                                                            
      END                                                               
