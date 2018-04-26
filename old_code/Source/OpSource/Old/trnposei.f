      SUBROUTINE TRNPOSEI(MATRIX, WORKSPACE, NOBS, NELS)                        
                                                                                
!---------------------------------------------------------------------!         
!                                                                     !         
! PROGRAM       : TRNPOSEI                                            !         
!                                                                     !         
! PURPOSE       : FORTRAN replacement for NAG routine F01CRE          !         
!                 Transposes the elements of a 2 dimensional array    !         
!                 INTEGER VERSION - TRNPOSER IS FOR REAL ARRAYS       !         
!                                                                     !         
! CALLS         : NONE                                                !         
!                                                                     !         
!Y2K  16.06.1997  TRNSPOSE is Year 2000 compliant.                    !         
!     13.08.1997  MADE FROM TRNPOSER                                  !         
!                                                                     !         
!---------------------------------------------------------------------!
! $Log:
!  2    Met_DB_Project 1.1         26/11/2007 11:11:30    Brian Barwell
!       Obsolete routine used for SAT120 storage (removed from MDBSTOR load
!       module on 19 November 2007).
!  1    Met_DB_Project 1.0         30/01/2006 20:25:31    Sheila Needham  
! $
! Revision 1.1  1997/08/14 08:09:02  uspm
! Initial revision
!
!---------------------------------------------------------------------!
                                                                                
      IMPLICIT NONE                                                             
                                                                                
!-----------------------------------------------------------------------        
! declare variables                                                             
!-----------------------------------------------------------------------        
                                                                                
      INTEGER   NOBS                   !- dimension 1 of input array            
      INTEGER   NELS                   !- dimension 2 of input array            
      INTEGER   IEL,IOB,K              !- loop counters                         
                                                                                
      INTEGER   MATRIX(NOBS*NELS)      !- matrix to transpose                   
      INTEGER   WORKSPACE(NELS,NOBS)   !- workspace
      
      CHARACTER HEAD*132               !- Revision information
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/trnposei.F,v $
     &'//'$ $Date: 26/11/2007 11:11:30$ $Revision: 2$'
                                                                                
!-----------------------------------------------------------------------        
! transpose matrix                                                              
!-----------------------------------------------------------------------        
                                                                                
      K=0                                                                       
      DO IEL=1,NELS                                                             
        DO IOB=1,NOBS                                                           
          K=K+1                                                                 
          WORKSPACE(IEL,IOB)=MATRIX(K)                                          
         ENDDO                                                                  
      ENDDO                                                                     
                                                                                
      K=0                                                                       
      DO IOB=1,NOBS                                                             
        DO IEL=1,NELS                                                           
          K=K+1                                                                 
          MATRIX(K)=WORKSPACE(IEL,IOB)                                          
        ENDDO                                                                   
      ENDDO                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
