      CHARACTER*(*) FUNCTION GETCHR(RDISP,CD)

!-----------------------------------------------------------------------
!                                                                              
! FUNCTION      : GETCHR IN MDB                                               
!                                                                              
! PURPOSE       : TO EXTRACT CHARACTER ELEMENTS FROM A STRING RETURNED        
!                 BY MDB                                                       
! DESCRIPTION   :                                                              
!                                                                              
! DATA TYPE(S)  : ALL MDB TYPES                                                
!                                                                              
! CALLED BY     : USER                                                         
!                                                                              
! CALLS         : NOTHING                                                      
!                                                                              
! PARAMETERS    : (1) POINTER VALUE FROM MDB REAL ARRAY                        
!                 (2) CHARACTER STRING                                         
!                                                                              
! INTRODUCED : 17/02/92                                                        
!                                                                              
!Y2K  26.06.1997  GETCHR IS YEAR 2000 COMPLIANT.                               
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:32$
! $Source: /home/us0400/mdb/op/lib/source/RCS/getchr.F,v $
!                                                                              
! CHANGE RECORD :                                                              
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:32    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:58:39  usmdb
! Assumed-length character function is an obsolete feature
! of f95. I cannot find any guidance on how to re-code it, so
! it stays. Added copyright and modified header - S.Cox
!
! Revision 1.1  97/08/04  13:47:21  13:47:21  uspm (Pat McCormack)
! Initial revision
! 
!----------------------------------------------------------------------         
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      CHARACTER*(*) CD
      CHARACTER*132 HEAD                                                          
      REAL RDISP
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/getchr.F,v $
     &'//'$ $Date: 30/01/2006 20:22:32$ $Revision: 1$'                                                                
      ID=RDISP                                                                  
      ILEN=ID/65536                                                             
      IS=MOD(ID,65536)                                                          
      GETCHR=CD(IS:IS+ILEN-1)                                                   

      RETURN                                                                    
      END
