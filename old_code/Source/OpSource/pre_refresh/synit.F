      SUBROUTINE SYNIT(                                                 
     +        WMOBLK,WMOSTA,WMOREG, LAT,LON, STNHT,STNTYP,              
     +        DD,FF, TTT,TDTDTD,UUU, VV,VERTV, WW,WPER,W1,W2,           
     +        NT, TPER1,MAXTTT,TPER2,MINTTT, E, TR,RRR,TR3,RRR3,R24,    
     +        T1GUST,T2GUST,VGUST, T1MEAN,T2MEAN,VMEAN,                 
     +        PPPP,P0P0P0, A3,HHH, SSS,TGTG, ATEND,PCHANG,PCHG24,       
     +        IE,EEE, SUN2,SUN1, RNET,GLSO,DISO,LW,SW,SWNET,SDIR,       
     +        RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,            
     +        SEAVIS,SEASTE,TWTYPE,TWTWTW,                              
     +        DS,VS,TBTYPE,TBTBTB,WVMS,WVHT,WVPER,SWELWV,               
     +        ISICE,ESES,RS,CI,SI,BI,DI,ZI,                             
     +        CL,CM,CH,NH,HL, VSIF,GROUP8, MTCL, IW,HOUR,MINS,          
     +        T1GUS,T2GUS,VGUS)                                    !A   

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : SYNIT                                                
!                                                                      
! PURPOSE       : TO INITIALISE ALL ELEMENT VARIABLES TO MISSING       
!                                                                      
! DATA TYPE(S)  : LNDSYN                                               
!                                                                      
! CALLED BY     : SYNEXP                                               
!                                                                      
! PARAMETERS    : ALL ELEMENT VARIABLES                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:52$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synit.F,v $
!                                                                      
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:52    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:05  usmdb
! Changed pre-processor statement to if (MVS), else....
! Added copyright and modified header - S.Cox
!
! Revision 1.6  98/01/29  11:34:09  11:34:09  usmdb (Generic MetDB accou
! Addition of IBM preprocess directive
! 
! Revision 1.5  97/09/22  13:49:55  13:49:55  uspm (Pat McCormack)
! Change ifdef to if defined
! 
! Revision 1.4  1997/07/31 11:38:25  uspm
! First revision for MVS
!
! Revision 1.3  1997/07/04 13:44:53  uspm
! Latest version from MVS - Y2K check
!
! Revision 1.2  1997/05/15 15:09:49  uspm
! Add revision information and preprocessor statements about INCLUDE 
! statements
!
! 19/11/1996     ADD INITIALISATION OF T1GUS,T2GUS & VGUS             !A   
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

      INTEGER I,J                                                       

#if defined (MVS)
      INCLUDE (SYNELM)                                                  
      INCLUDE (SHPELM)                                                  
      INCLUDE (SYNCOM)                                                  
#else
      INCLUDE 'SYNELM'
      INCLUDE 'SHPELM'
      INCLUDE 'SYNCOM'
#endif

      CHARACTER*132 CHEAD
      
      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synit.F,v $
     &'//'$ $Date: 30/01/2006 20:24:52$ $Revision: 1$'

      WMOBLK=RMISS    ! BLOCK                                           
      WMOSTA=RMISS    ! STATION NUMBER                                  
      WMOREG=RMISS    ! WMO REGION                                      
      STNTYP=RMISS    ! STATION TYPE CODE TBL 002002                    
      LAT   =RMISS    ! LATITUDE                                        
      LON   =RMISS    ! LONGITUDE                                       
      STNHT =RMISS    ! STATION HEIGHT                                  
      STNTYP=RMISS    ! STATION TYPE                                    
      DD    =RMISS    ! WIND DIRECTION                                  
      FF    =RMISS    ! WIND SPEED                                      
      TTT   =RMISS    ! TEMPERATURE                                     
      TDTDTD=RMISS    ! DEWPOINT                                        
      UUU   =RMISS    ! RELATIVE HUMIDITY                               
      VV    =RMISS    ! HORIZONTAL VIS                                  
      VERTV =RMISS    ! VERTICAL VISIBILITY                             
      WW    =RMISS    ! PRESENT WEATHER                                 
      WPER  =RMISS    ! PAST WEATHER PERIOD                             
      W1    =RMISS    ! PAST WEATHER 1                                  
      W2    =RMISS    ! PAST WEATHER 2                                  
      NT    =RMISS    ! TOTAL CLOUD AMOUNT                              
      TPER1 =RMISS    ! MAX TEMP TIME PERIOD                            
      MAXTTT=RMISS    ! MAX TEMPERATURE                                 
      TPER2 =RMISS    ! MIN TEMP TIME PERIOD                            
      MINTTT=RMISS    ! MIN TEMPERATURE                                 
      E     =RMISS    ! STATE OF GROUND                                 
      TR    =RMISS    ! RAINFALL PERIOD                                 
      RRR   =RMISS    ! RAINFALL AMOUNT                                 
      TR3   =RMISS    ! RAINFALL TIME PERIOD ( FROM SECTION 3)          
      RRR3  =RMISS    ! RAINFALL AMOUNT (SECTION 3)                     
      R24   =RMISS    ! 24 HOUR RAINFALL                                
      T1GUST=RMISS    ! GUST PERIOD                                     
      T2GUST=RMISS    ! PERIOD OF GUST                                  
      VGUST =RMISS    ! MAX WIND SPEED (GUST)                           
      T1GUS =RMISS    ! GUST PERIOD                               !A    
      T2GUS =RMISS    ! PERIOD OF GUST                            !A    
      VGUS  =RMISS    ! MAX WIND SPEED (GUST)                     !A    
      T1MEAN=RMISS    ! MAX WIND SPEED (GUST)                           
      T2MEAN=RMISS    ! PERIOD OF MEAN WIND SPEED                       
      VMEAN =RMISS    ! MAX MEAN WIND SPEED                             
      PPPP  =RMISS    ! MEAN SEA LEVEL PRESSURE                         
      P0P0P0=RMISS    ! STATION LEVEL PRESSURE                          
      A3    =RMISS    ! PRESSURE LEVEL                                  
      HHH   =RMISS    ! GEOP. HT OF GIVEN PRESSURE LEVEL                
      SSS   =RMISS    ! SNOW DEPTH                                      
      TGTG  =RMISS    ! GRASS MINIMUM                                   
      ATEND =RMISS    ! PRESSURE TENDENCY                               
      PCHANG=RMISS    ! LAST 3 HR PRESSURE CHANGE                       
      PCHG24=RMISS    ! 24 HOUR PRESSURE CHANGE                         
*                                                                       
* RADIATION ELEMENTS                                                    
*                                                                       
      IE    =RMISS    ! TYPE OF INSTR FOR EVAP.TRANS.                   
      EEE   =RMISS    ! 24 HOUR EVAPOTRANSPIRATION                      
      SUN2  =RMISS    ! DAILY HOURS OF SUNSHINE                         
      SUN1  =RMISS    ! SUNSHINE AMOUNT IN LAST HOUR                    
      RNET  =RMISS    ! LAST HOUR NET RADIATION                         
      GLSO  =RMISS    ! LAST HOUR GLOBAL RADIATION                      
      DISO  =RMISS    ! LAST HOUR DIFFUSE RADIATION                     
      LW    =RMISS    ! LAST HOUR LONGWAVE RADIATION                    
      SW    =RMISS    ! LAST HOUR SHORTWAVE RADIATION                   
      SWNET =RMISS    ! LAST HOUR NET SHORTWAVE RADIATION               
      SDIR  =RMISS    ! LAST HOUR DIRECT SOLAR RADIATION                
      RNET24=RMISS    ! 24 HOUR NET RADIATION                           
      GLSO24=RMISS    ! 24  HOUR GLOBAL RADIATION                       
      DISO24=RMISS    ! 24 HOUR DIFFUSE RADIATION                       
      LW24  =RMISS    ! 24 HOUR LONGWAVE RADIATION                      
      SW24  =RMISS    ! 24 HOUR SHORTWAVE RADIATION                     
      SWNET24=RMISS   ! 24 HOUR NET SHORTWAVE RADIATION                 
      SDIR24=RMISS    ! 24 HOUR DIRECT SOLAR RADIATION                  
*                                                                       
* SEA (OR COASTAL) ELEMENTS                                             
*                                                                       
      SEAVIS=RMISS    ! HORIZ VIS TOWARDS SEA                           
      SEASTE=RMISS    ! STATE OF SEA                                    
      DS    =RMISS    ! DIRECTION OF MOVEMENT                           
      VS    =RMISS    ! SPEED OF MOVEMENT                               
      TBTYPE=RMISS    ! WET-BULB TEMPERATURE MEASUREMENT METHOD         
      TBTBTB=RMISS    ! WET-BULB TEMPERATURE                            
      TWTYPE=RMISS    ! SST MEASUREMENT METHOD                          
      TWTWTW=RMISS    ! SEA TEMPERATURE                                 
      WVMS  =RMISS    ! WAVE MEASUREMENT METHOD                         
      WVHT  =RMISS    ! WAVE HEIGHT                                     
      WVPER =RMISS    ! WAVE PERIOD                                     
*                                                                       
      DO I=1,6                                                          
      SWELWV(I)=RMISS                                                   
      ENDDO                                                             
*                                                                       
      ISICE =RMISS    ! CAUSE OF ICE ACCRETION ON SHIP                  
      ESES  =RMISS    ! ICE THICKNESS ON SHIP                           
      RS    =RMISS    ! RATE OF ICE ACCRETION ON SHIP                   
      CI    =RMISS    ! SEA ICE CONCENTRATION                           
      SI    =RMISS    ! SEA ICE DEVELOPMENT                             
      BI    =RMISS    ! AMOUNT & TYPE OF SEA ICE                        
      DI    =RMISS    ! BEARING OF NEAREST SEA ICE                      
      ZI    =RMISS    ! SEA ICE SITUATION                               
*                                                                       
* CLOUD                                                                 
*                                                                       
      CL    =RMISS    ! LOW CLOUD TYPE                                  
      CM    =RMISS    ! MEDIUM CLOUD TYPE                               
      CH    =RMISS    ! HIGH CLOUD TYPE                                 
      NH    =RMISS    ! AMOUNT OF LOW (LOWEST) CLOUD                    
      HL    =RMISS    ! HT OF BASE OF (LOWEST) CLOUD                    
      IW    =RMISS    ! ORIGINAL WIND SPEED UNITS                       
      HOUR  =RMISS    ! ACTUAL HOUR OF OBSERVATION                      
      MINS  =RMISS    ! ACTUAL MINUTE OF OBSERVATION                    
*                                                                       
* CLOUD 8-GROUPS                                                        
*                                                                       
      DO I=1,4                                                          
      VSIF(I)=RMISS                                                     
      ENDDO                                                             
*                                                                       
      DO I=1,12                                                         
      GROUP8(I)=RMISS                                                   
      ENDDO                                                             
*                                                                       
* MOUNTAIN CLOUD                                                        
*                                                                       
      DO I=1,2                                                          
      DO J=1,4                                                          
      MTCL(J,I)=RMISS                                                   
      ENDDO                                                             
      ENDDO                                                             
*                                                                       
      RETURN                                                            
      END
