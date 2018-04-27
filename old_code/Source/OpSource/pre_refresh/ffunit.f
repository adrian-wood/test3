      SUBROUTINE FFUNIT(WMOBLK,WMOSTN,IW)                               

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : FFUNIT                                               
!                                                                      
! PURPOSE       : SET WIND REPORTING UNITS FOR REPORTS WITH NO YYGGIW  
!                                                                      
! DESCRIPTION   : ASSUME BLOCKS 11 (STATIONS>400), 12, 15 AND 20-39    
!                 REPORT IN M/S, THE REST IN KNOTS.                    
!                                                                      
! CALLED BY     : SYNEXP                                               
!                                                                      
! PARAMETERS    : (1) WMO BLOCK NUMBER                            (I)  
!                 (2) WMO STATION NUMBER                          (I)  
!                 (3) IW SET TO 0 FOR M/S, 3 FOR KNOTS            (O)  
!                                                                      
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ffunit.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:27    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:46  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  09:27:11  09:27:11  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 12:42:17  uspm
! Initial revision
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

      INTEGER WMOBLK,WMOSTN,IW
      CHARACTER*132 HEAD
*                                                                       
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ffunit.F,v $
     &'//'$ $Date: 30/01/2006 20:22:27$ $Revision: 1$'
      
      IF(WMOBLK.EQ.11)THEN                                              
        IF(WMOSTN.LT.400)THEN    ! AUSTRIA: KNOTS                       
          IW=3                                                          
        ELSE                     ! CZECH REPUBLIC & SLOVAKIA: M/S       
          IW=0                                                          
        ENDIF                                                           
      ELSEIF(WMOBLK.EQ.12)THEN   ! POLAND: M/S                          
        IW=0                                                            
      ELSEIF(WMOBLK.EQ.15)THEN   ! ROMANIA: M/S                         
        IW=0                                                            
      ELSEIF(WMOBLK.LT.20)THEN                                          
        IW=3                                                            
      ELSEIF(WMOBLK.GT.39)THEN                                          
        IW=3                                                            
      ELSE                                                              
        IW=0                     ! 'FORMER USSR': M/S                   
      ENDIF                                                             
*                                                                       
      RETURN                                                            
      END                                                               
