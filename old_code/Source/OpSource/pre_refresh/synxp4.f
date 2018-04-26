      SUBROUTINE SYNXP4(REPORT,POINT,NGRPS,IMT,MTCL)                    

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SYNXP4                                               
!                                                                      
! PURPOSE       : EXPAND MOUNTAIN CLOUD GROUPS IN SYNOP (SECTION 4)    
!                                                                      
! DESCRIPTION   :                                                      
!                                                                      
! CALLED BY     : SYNEXP                                               
!                                                                      
! PARAMETERS    : (1) REPORT                                           
!                 (2) STARTING POINT                                   
!                 (3) NUMBER OF GROUPS IN SECTION (ALL NCHHCT)         
!                 (4) NUMBER OF MOUNTAIN CLOUD GROUPS (MAX 2)          
!                 (5) MOUNTAIN CLOUD DATA                              
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:06$
! $Source: /home/us0400/mdb/op/lib/source/RCS/synxp4.F,v $
!                                                      
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:06    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:11  usmdb
! Separated variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:40:34  11:40:34  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:53:15  uspm
! Initial revision
!
! 20-08-96  S.M.N  THERE MUST BE 2 CLOUD GROUPS, THE FIRST IS BELOW    
!           STATION CLOUD AMOUNT, THE SECOND IS ABOVE STN CLOUD AMT.   
!           IF THERE IS ONLY ONE GROUP ASSUME IT IS BELOW STN        !A 
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
                                                                        
      INTEGER POINT,NGRPS,IMT,I,J                                       
      CHARACTER HEAD*132
      CHARACTER REPORT*(*)                                              
      REAL MTCL(4,2)                                                    
      INTEGER MISSIN                                                !2.0

      DATA MISSIN/-9999999/                                         !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/synxp4.F,v $
     &'//'$ $Date: 30/01/2006 20:25:06$ $Revision: 1$'
      
      DO J=1,MIN(NGRPS,2)                                               
*                                                                       
* EACH GROUP GIVES AMOUNT, TYPE, HEIGHT & CLOUD TOP DESCRIPTION (CT)    
* - BUT HEIGHT OF TOPS IS ABOVE MSL, NOT RELATIVE TO STATION!           
*                                                                       
       MTCL(1,J)=IVALUE(REPORT(POINT:POINT))       ! N (AMOUNT)         
       MTCL(2,J)=IVALUE(REPORT(POINT+1:POINT+1))   ! C (TYPE)           
       I=IVALUE(REPORT(POINT+2:POINT+3))             ! HH               
       IF (I.NE.MISSIN) MTCL(3,J)=I*100.           ! TO METRES          
       MTCL(4,J)=IVALUE(REPORT(POINT+4:POINT+4))   ! CT                 
       POINT=POINT+6                                                    
      ENDDO                                                             
      IMT=2                                                             
      RETURN                                                            
      END                                                               
