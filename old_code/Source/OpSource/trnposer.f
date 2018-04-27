      SUBROUTINE TRNPOSER(MATRIX, WORKSPACE, NOBS, NELS)              !A        
                                                                                
!---------------------------------------------------------------------!         
!                                                                     !         
! PROGRAM       : TRNSPOSE                                            !         
!                                                                     !         
! PURPOSE       : FORTRAN replacement for NAG routine F01CRE          !         
!                 Transposes the elements of a 2 dimensional array    !         
!                                                                     !         
! CALLS         : NONE                                                !         
!                                                                     !         
!Y2K  16.06.1997  TRNPOSER is Year 2000 compliant.                    !         
!                                                                     !         
!CHANGE RECORD:                                                       !         
!                                                                     !         
! 13/08/1997 !A - NAME CHANGE TO FROM TRNSPOSE TO TRNPOSER TO         !         
!                 DIFFERENTIATE BETWEEN THE 'REAL' VERSION AND THE    !         
!                 'INTEGER' VERSION' TRNPOSEI                         !         
!---------------------------------------------------------------------!         
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:31    Sheila Needham  
! $
! Revision 1.1  1997/08/14 08:09:07  uspm
! Initial revision
!                                                          
!---------------------------------------------------------------------!         
                                                                                
      IMPLICIT NONE                                                             
                                                                                
!-----------------------------------------------------------------------        
! declare variables                                                             
!-----------------------------------------------------------------------        
                                                                                
      INTEGER   NOBS                    !- dimension 1 of input array           
      INTEGER   NELS                    !- dimension 2 of input array           
      INTEGER   IEL,IOB,K               !- loop counters                        
                                                                                
      REAL      MATRIX(NOBS*NELS)       !- matrix to transpose                  
      REAL      WORKSPACE(NELS,NOBS)    !- workspace                            
                                                                                
      CHARACTER HEAD*132                                                        
                                                                                
!Initialise variables                                                           
      HEAD='                                                                    
     &$Source: /home/us0400/mdb/op/lib/source/RCS/trnposer.F,v $                 
     &'//'$ $Date: 30/01/2006 20:25:31$ $Revision: 1$'                      
                                                                                
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
