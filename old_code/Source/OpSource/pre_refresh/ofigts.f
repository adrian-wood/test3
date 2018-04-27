      LOGICAL FUNCTION OFIGTS(CSTR,ISTART,IEND)                         

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : OFIGTS                                               
!                                                                      
! PURPOSE       : TO SEE IF A CHARACTER STRING CONSISTS OF FIGURES     
!                                                                      
! CALLED BY     : BATHY, TESAC, AMDARX & ANY OTHER FORTRAN EXPANSION   
!                                                                      
! PARAMETERS    : (1) CSTR   - STRING TO BE CHECKED                    
!                 (2) ISTART - LOCATION OF START OF STRING REQUIRED    
!                 (3) IEND   - END OF STRING                           
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:49$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ofigts.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:49    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:42  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  97/08/01  08:49:45  08:49:45  uspm (Pat McCormack)
! Initial revision
! 
! OCT 91 - LENGTH OF CHARACTER STRING SET TO
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

      CHARACTER*(*) CSTR                                                
      CHARACTER*132 HEAD
      
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ofigts.F,v $
     &'//'$ $Date: 30/01/2006 20:23:49$ $Revision: 1$'
     
      OFIGTS=.TRUE.                                                     
      DO 10,ILOOP=ISTART,IEND                                           
           IF ((CSTR(ILOOP:ILOOP).LT.'0').OR.                           
     &              (CSTR(ILOOP:ILOOP).GT.'9')) OFIGTS=.FALSE.          
   10 CONTINUE                                                          
      RETURN                                                            
      END                                                               
