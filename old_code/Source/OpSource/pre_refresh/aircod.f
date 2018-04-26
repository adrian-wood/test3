      SUBROUTINE AIRCOD(AIR_ARAY1,AIR_ARAY2,ARYFLG,SIGN,BEAC_NAME,        
     &                  DESCR,NDESC,REPLEN,MESAGE,LENGTH,DATIME)    !2.0                                  
                                                                                
!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : AIRCOD                                                           
!                                                                              
! PURPOSE       : TO PASS ELEMENTS AND DESCIPTOR ARRAYS TO THE BUFR                
!                 ENCODING ROUTINES                                                
!                                                                              
! CALLED BY     : AIRENC                                            !2.0
!                                                                              
! CALLS         : ENBUFR                                                           
!                                                                              
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:36$
! $Source: /home/us0400/mdb/op/lib/source/RCS/aircod.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:36    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:19  usmdb
! Removed unused argument NREPS. Added copyright and modified
! header - S.Cox
!
! Revision 1.2  97/07/31  09:06:04  09:06:04  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/03 13:55:17  uspm
! Initial revision
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
                                                                                
!-----------------------------------------------------------------------
! declare variables.                                                        
!-----------------------------------------------------------------------
                                                                                
      INTEGER   MAXDESC                !- max no. of descriptors.               
      INTEGER   MAXOBS                 !- max no. of obs.                       
      INTEGER   NOBS                   !- no. of obs.                           
      INTEGER   NELEM                  !- no. of elements.                      
      PARAMETER (MAXDESC=20)        !- set max descs.                           
      PARAMETER (MAXOBS=2)           !- set max no. of obs.                     
      INTEGER   ARYFLG                                                          
      INTEGER   DESCR(22)    !- BUFR descriptors array.                         
      INTEGER   DATIME(5)              !- date/time array.                      
      INTEGER   REPLEN                                                          
      INTEGER   NDESC                  !- no. of descriptors.                   
      INTEGER   LENGTH                 !- length of BUFR message.               
                                                                                
      REAL      AIR_ARAY1(18)          !Elements array                          
      REAL      AIR_ARAY2(18)        !Elements array with Mid point             
                                                                                
      CHARACTER*8    SIGN                                                       
      CHARACTER      MESAGE*10240           !- final BUFR message.              
      CHARACTER      NAMES*25               !- BUFR names string.               
      CHARACTER*(*)  BEAC_NAME                                                  
      CHARACTER*132  HEAD              !- revision information
                                                                                
      LOGICAL   CMP                    !- BUFR compression flag.                
                                                                                
!-----------------------------------------------------------------------
! initialise variables.                                                     
!-----------------------------------------------------------------------

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/aircod.F,v $
     &'//'$Date: 30/01/2006 20:20:36$ $Revision: 1$'
     
!-----------------------------------------------------------------------        
! ARYFLG determines whether we have just the one report or if the report        
! had a mid point and thus there are two index entries etc
!-----------------------------------------------------------------------        

      LENGTH=0              !- eventual output from enbufr, octets.             
      NAMES(1:)='                    '                                          
      NELEM=18                                                                  
      NOBS=1                                                                    
      CMP=.FALSE.                                                               
      NAMES(1:8)=SIGN                                                           
      NAMES(9:17)=BEAC_NAME                                                     
      IF (ARYFLG .EQ. 1) THEN                                                   
        CALL ENBUFR(DESCR,AIR_ARAY1,NDESC,NELEM,NOBS,NAMES,DATIME,              
     &              MESAGE(REPLEN+1:),CMP,LENGTH)                               
      ELSEIF (ARYFLG .EQ. 2) THEN                                               
        CALL ENBUFR(DESCR,AIR_ARAY2,NDESC,NELEM,NOBS,NAMES,DATIME,              
     &              MESAGE(REPLEN+1:),CMP,LENGTH)                               
      ENDIF                                                                     
                                                                                
      RETURN                                                                    
      END                                                                       
