      SUBROUTINE AIRSET(LAT,LONG,LAT2,LONG2,MATCH,TIMEYY,TIMEMNTH,      
     &TIMEDD,TIMEHH,TIMEMM,LEVEL,TEMP,WINDD,WINDS,OPT_TEMP,OPT_WIND,    
     &OPT_WNDS,OPT_TURB,OPT_ICE,MID_YY,MID_MTH,MID_DD,MID_HH,MID_MM,    
     &ARYFLG,AIR_ARAY1,AIR_ARAY2,INDX_ARAY1,INDX_ARAY2)                 

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SUBROUTINE AIRSET                                     
!                                                                      
! PURPOSE       : TO SET UP ARRAYS FOR BUFR ENCODE ROUTINES              
!                                                                      
! DESCRIPTION   : THE AIREP ELEMENTS ARE PASSED TO THIS ROUTINE WHERE    
!                 THEY ARE THEN STORED IN AN ARRAY IN THE ORDER  OF THE  
!                 BUFR DESCRIPTORS. THE ARRAY IS THEN PASSED TO          
!                 AIRCOD                                                 
!                                                                      
! CALLED BY     : AIRARP                                                 
!                                                                      
! CALLS TO      : NONE                                                   
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/airset.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:45    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:24  usmdb
! Removed unused variable, added copyright and modified
! header - S.Cox
!
! Revision 1.2  97/07/31  09:08:11  09:08:11  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 10:29:31  uspm
! Initial revision
!
! Sept 96 - set count of good values to go in index entry             !a
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
                                                                        
!declare variables                                                      
      INTEGER  MATCH                                                    
      INTEGER  ARYFLG                                                   
      INTEGER  MID_FLAG                                                 
      INTEGER  NELM         ! count of good values                    !a
                                                                        
!declare real                                                           
      REAL     LAT                                                      
      REAL     LONG                                                     
      REAL     LAT2                                                     
      REAL     LONG2                                                    
      REAL     TIMEDD                                                   
      REAL     TIMEHH                                                   
      REAL     TIMEMM                                                   
      REAL     TIMEMNTH                                                 
      REAL     TIMEYY                                                   
      REAL     LEVEL                                                    
      REAL     TEMP                                                     
      REAL     WINDD                                                    
      REAL     WINDS                                                    
      REAL     OPT_TEMP                                                 
      REAL     OPT_WIND                                                 
      REAL     OPT_WNDS                                                 
      REAL     OPT_TURB                                                 
      REAL     OPT_ICE                                                  
      REAL     AIR_ARAY1(18)                                            
      REAL     AIR_ARAY2(18)                                            
      REAL     INDX_ARAY1(8)                                          !a
      REAL     INDX_ARAY2(8)                                          !a
      REAL     MID_YY                                                   
      REAL     MID_MTH                                                  
      REAL     MID_DD                                                   
      REAL     MID_HH                                                   
      REAL     MID_MM                                                   
                                                                        
!declare character                                                      
      CHARACTER*132   HEAD     !Revision information

!declare logical                                                        
      LOGICAL DEBUG                                                     
                                                                        
      SAVE                                                              
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/airset.F,v $
     &'//'$ $Date: 30/01/2006 20:20:45$ $Revision: 1$'
      
      DEBUG=.FALSE.                                                     
                                                                        
      IF (MATCH .EQ. 1) THEN                                            
        MID_FLAG=21                                                     
      ENDIF                                                             
                                                                        
!store the elements in the air_aray                                     
      IF (MID_YY .LE. 0) THEN                                           
        NELM=0                     ! initialise count of good values  !a
        ARYFLG=1                                                        
                                                                        
        AIR_ARAY1(1)=1               !Aircraft id displacement          
        AIR_ARAY1(2)=9             !BEACON REPORTING POINT              
        AIR_ARAY1(3)=1             !Flag - always obs. for first report 
        AIR_ARAY1(4)=TIMEYY          !Year                              
        AIR_ARAY1(5)=TIMEMNTH        !Month                             
        AIR_ARAY1(6)=TIMEDD        !Day                                 
        AIR_ARAY1(7)=TIMEHH        !Hour                                
        AIR_ARAY1(8)=TIMEMM        !Minute                              
        AIR_ARAY1(9)=1             !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY1(10)=LAT          !Latitude                            
        AIR_ARAY1(11)=LONG         !Longitude                           
        AIR_ARAY1(12)=1            !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY1(13)=LEVEL         !Flight level                       
        AIR_ARAY1(14)=WINDD        !Wind direction                      
        AIR_ARAY1(15)=WINDS        !Wind speed                          
        AIR_ARAY1(16)=OPT_TURB     !Turbulence                          
        AIR_ARAY1(17)=TEMP         !Air temperature                     
        AIR_ARAY1(18)=OPT_ICE      !Icing                               
                                                                        
        IF (WINDD.NE.-9999999.) NELM=NELM+1                           !A
        IF (WINDS.NE.-9999999.) NELM=NELM+1                           !A
        IF (OPT_TURB.NE.-9999999.) NELM=NELM+1                        !A
        IF (TEMP.NE.-9999999.) NELM=NELM+1                            !A
        IF (OPT_ICE.NE.-9999999.) NELM=NELM+1                         !A
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!set up array for index entry                                        !  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
        INDX_ARAY1(1)=TIMEYY        !CALCULATED IN AIRTIM               
        INDX_ARAY1(2)=TIMEMNTH                                          
        INDX_ARAY1(3)=TIMEDD                                            
        INDX_ARAY1(4)=TIMEHH                                            
        INDX_ARAY1(5)=TIMEMM                                            
        INDX_ARAY1(6)=LAT                                               
        INDX_ARAY1(7)=LONG                                              
        INDX_ARAY1(8)=NELM                                            !a
      ELSE                                                              
        NELM=0                     ! initialise count of good values  !a
        ARYFLG=2                                                        
                                                                        
        AIR_ARAY1(1)=1               !Aircraft id displacement          
        AIR_ARAY1(2)=9             !BEACON REPORTING POINT              
        AIR_ARAY1(3)=1             !Flag - always obs. for first report 
        AIR_ARAY1(4)=TIMEYY          !Year                              
        AIR_ARAY1(5)=TIMEMNTH        !Month                             
        AIR_ARAY1(6)=TIMEDD        !Day                                 
        AIR_ARAY1(7)=TIMEHH        !Hour                                
        AIR_ARAY1(8)=TIMEMM        !Minute                              
        AIR_ARAY1(9)=1             !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY1(10)=LAT          !Latitude                            
        AIR_ARAY1(11)=LONG         !Longitude                           
        AIR_ARAY1(12)=1            !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY1(13)=LEVEL         !Flight level                       
        AIR_ARAY1(14)=WINDD        !Wind direction                      
        AIR_ARAY1(15)=WINDS        !Wind speed                          
        AIR_ARAY1(16)=OPT_TURB     !Turbulence                          
        AIR_ARAY1(17)=TEMP         !Air temperature                     
        AIR_ARAY1(18)=OPT_ICE      !Icing                               
                                                                        
        IF (WINDD.NE.-9999999.) NELM=NELM+1                           !A
        IF (WINDS.NE.-9999999.) NELM=NELM+1                           !A
        IF (OPT_TURB.NE.-9999999.) NELM=NELM+1                        !A
        IF (TEMP.NE.-9999999.) NELM=NELM+1                            !A
        IF (OPT_ICE.NE.-9999999.) NELM=NELM+1                         !A
        INDX_ARAY1(8)=NELM                                            !a
                                                                        
        NELM=0                     ! initialise count of good values  !a
        AIR_ARAY2(1)=1               !Aircraft id displacement          
        AIR_ARAY2(2)=9             !BECAON ID                           
        AIR_ARAY2(3)=21            !TIME SIGINIFICANCE FLAG             
        AIR_ARAY2(4)=MID_YY          !Year                              
        AIR_ARAY2(5)=MID_MTH         !Month                             
        AIR_ARAY2(6)=MID_DD        !Day                                 
        AIR_ARAY2(7)=MID_HH        !Hour                                
        AIR_ARAY2(8)=MID_MM        !Minute                              
        AIR_ARAY2(9)=MID_FLAG      !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY2(10)=LAT2          !Latitude                           
        AIR_ARAY2(11)=LONG2         !Longitude                          
        AIR_ARAY2(12)=1            !FLAG INDICATES OBSERVED LAT/LONG    
        AIR_ARAY2(13)=LEVEL         !Flight level                       
        AIR_ARAY2(14)=OPT_WIND     !Wind direction                      
        AIR_ARAY2(15)=OPT_WNDS     !Wind speed                          
        AIR_ARAY2(16)=-9999999.    !Turbulence                          
        AIR_ARAY2(17)=OPT_TEMP     !Air temperature                     
        AIR_ARAY2(18)=-9999999.    !Icing                               
                                                                        
        IF (OPT_WIND.NE.-9999999.) NELM=NELM+1                        !A
        IF (OPT_WNDS.NE.-9999999.) NELM=NELM+1                        !A
        IF (OPT_TEMP.NE.-9999999.) NELM=NELM+1                        !A
        INDX_ARAY2(8)=NELM                                            !a
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!set up array for index entry (NELM already in INDX_ARAY(8)          !  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
        INDX_ARAY1(1)=TIMEYY                                            
        INDX_ARAY1(2)=TIMEMNTH                                          
        INDX_ARAY1(3)=TIMEDD                                            
        INDX_ARAY1(4)=TIMEHH                                            
        INDX_ARAY1(5)=TIMEMM                                            
        INDX_ARAY1(6)=LAT                                               
        INDX_ARAY1(7)=LONG                                              
                                                                        
        INDX_ARAY2(1)=MID_YY                                            
        INDX_ARAY2(2)=MID_MTH                                           
        INDX_ARAY2(3)=MID_DD                                            
        INDX_ARAY2(4)=MID_HH                                            
        INDX_ARAY2(5)=MID_MM                                            
        INDX_ARAY2(6)=LAT2                                              
        INDX_ARAY2(7)=LONG2                                             
      ENDIF                                                             
                                                                        
      RETURN                                                            
      END                                                               
