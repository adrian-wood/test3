      SUBROUTINE SUBMIT(CJCL,NL)                                                
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! ROUTINE       : SUBMIT
!                                                                               
! PURPOSE       : Submits a number of lines of jcl by writing                   
!                 directly to intout.                                           
!                                                                               
! CALLED BY     : CARTSUB
!                                                                               
! CALLS         : Nothing                                                       
!                                                                               
! I/O           : Output on unit 200 INTOUT                                     
!                                                                               
! ARGUMENTS     : (1) CJCL  : char*80 array of JCL lines to submit              
!               : (2) NL    : integer number of lines to submit                 
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/submit.F,v $
!                                                                               
! CHANGE RECORD :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:42    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:01  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/07/31  11:36:21  11:36:21  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:39:14  uspm
! Initial revision
!
! 13-05-97  !A  : Add CLOSE(200) for MDB offline retrieval - S.Cox              
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

      CHARACTER*80  CJCL(NL)                                                     
      CHARACTER*132 HEAD
                                                                                
      INTEGER       NL                                                           
                                                                                
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/submit.F,v $
     &'//'$ $Date: 30/01/2006 20:24:42$ $Revision: 1$'
                                                                                
!-----------------------------------------------------------------------        
! connect unit 200 to DDNAME INTOUT                                             
!-----------------------------------------------------------------------        
                                                                                
      OPEN(200,FILE='INTOUT')                                                   
                                                                                
      WRITE(200,'(A80)')(CJCL(K),K=1,NL)                                        
                                                                                
      CLOSE(200)                                                      !A        
                                                                                
      RETURN                                                                    
      END
