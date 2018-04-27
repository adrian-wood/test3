      SUBROUTINE UPREPID(Byte17,RprtId)                                         
                                                                                
!-----------------------------------------------------------------------        
!                                                                               
! subroutine    : UPRREPID                                                      
!                                                                               
!               : ANSI standard except for IMPLICIT NONE and '!' used           
!               : for comments                                                  
!                                                                               
! purpose       : MetDB Upper-Air retrieval. Used to set the RPRT_IDNY          
!               : to put in the user's array.                                   
!                                                                               
! data type(s)  : TEMP, PILOT, DROPSOND                                         
!                                                                               
! called by     : UPRRET                                                        
!                                                                               
! sub calls     : None                                                          
!                                                                               
! arguments     :                                                               
!                                                                               
! Byte17        : character (ip) : index entry byte 17                          
! RprtId        : integer   (op) : report id                                    
!                                                                               
!Y2K  26.06.1997  UPREPID is Year 2000 compliant.                               
!
! revision info :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:48$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uprepid.F,v $
!                                                                               
! change record :                                                               
!                                                                               
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:48    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:22  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:37:01  13:37:01  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/17 11:59:35  uspm
! Initial revision
!
! 27-09-96      : Written S.Cox                                                 
!                                                                               
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                                
      IMPLICIT NONE                                                             
                                                                                
      INTEGER        IByte17                  !- byte 17 of index entry         
      INTEGER        Mdi                      !- missing data indicator         
      INTEGER        RprtId                   !- report identity                
                                                                                
      LOGICAL        DropSonde                !- TRUE if it's a DROPSOND        
      LOGICAL        Fixed                    !- TRUE if fixed report           
      LOGICAL        OnLand                   !- TRUE if on land                
                                                                                
      CHARACTER*1    Byte17                   !- current index entry            
                                                                                
      DATA           Mdi/-9999999/            !- missing data indicator         
                                                                                                                                                               
      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uprepid.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:25:48$ '
                                                                                
      IByte17   = ICHAR(Byte17)                                                 
      DropSonde = (MOD(IByte17/8,2).EQ.1 .AND. MOD(IByte17/4,2).EQ.0)           
      Fixed     = (MOD(IByte17/2,2).EQ.0)                                       
      OnLand    = (MOD(IByte17/1,2).EQ.0)                                       
                                                                                
      IF (DropSonde) THEN                                                       
        RprtId = 0                                                              
      ELSE                                                                      
        IF (Fixed .AND. OnLand) THEN                                            
          RprtId = 1                                                            
        ELSEIF ((.NOT.Fixed) .AND. OnLand) THEN                                 
          RprtId = 2                                                            
        ELSEIF (.NOT.OnLand) THEN                                               
          RprtId = 3                                                            
        ELSE                                                                    
          RprtId = Mdi                                                          
        ENDIF                                                                   
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
