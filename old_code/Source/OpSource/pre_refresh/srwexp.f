      SUBROUTINE SRWEXP(REPORT,REALEXP,MSGLEN)                      !1.3 

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : SRWEXP   in  TFMRET                                  
!                                                                      
! PURPOSE       : TO EXPAND A SREW REPORT INTO ELEMENTS HELD IN A      
!                 REAL ARRAY FOR USE WITH THE MDB SYSTEM               
!                                                                      
! DESCRIPTION   : THE REPORTED RAINFALL AMOUNT IS CHECKED TO ESTABLISH 
!                 WHETHER THE QUANTITY REPORTED IS A TRACE OR GREATER  
!                 OR WHETHER THERE HAS BEEN NO MEASUREMENT MADE AND    
!                 A NIL VALUE REPORTED.                                
!                                                                      
! DATA TYPE(S)  : SREW                                                 
!                                                                      
! CALLED BY     : TFMRET                                               
!                                                                      
! CALLS         : NONE                                                 
!                                                                      
! PARAMETERS    : (1) REPORT  - SREW REPORT IN CHARACTER FORMAT        
!                 (2) REALEXP - ARRAY OF EXPANDED ELEMENTS             
!                 (3) MSGLEN  - length of message                   !1.3 
!                                                                      
!Y2K  26.06.1997  SRWEXP is Year 2000 compliant.                        
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:25$
! $Source: /home/us0400/mdb/op/lib/source/RCS/srwexp.F,v $
! 
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:25    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:11  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  99/02/11  11:56:26  11:56:26  usmdb (Generic MDB account
! 15th February 1999 John Norton
! Added expansion of reports when WMO Block Number
! added to start of raw report to cope with CDL
! reports.
! v(G) = 7 ev(G) = 1
! 
! Revision 1.2  97/08/04  13:33:37  13:33:37  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
!
! Revision 1.1  1997/02/17 11:56:23  uspm
! Initial revision
!
! FEB 96 INTRODUCED TO ALLOW SREW RETRIEVAL                   
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
                                                                        
                                                                        
! Variable declarations                                                 
                                                                        
      CHARACTER*(*)  REPORT       ! The SREW report                     
                                                                        
      REAL           REALEXP(*)   ! Array to hold expanded elements     
      REAL           RRR          ! Rainfall amount                     
      INTEGER        MSGLEN       ! Message length                      
                                                                        
      CHARACTER*132 HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/srwexp.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:25$ '

! Initialise variables                                                  
                                                                        
      RRR=-9999999                                                      
                                                                        
! Decode rainfall value for old style reports (6 characters iiiRRR)     
                                                                        
      IF(MSGLEN.EQ.6)THEN                                          !1.3 

! Expand a report when the quantity of rainfall cannot be measured or   
! is not reported.                                                      
                                                                        
      IF (REPORT(4:6) .EQ. 'NIL' .OR. REPORT(4:6) .EQ. '999') THEN      
        RRR=-9999999                                                    
                                                                        
! Expand a report when the quanity of rainfall is a trace.              
                                                                        
      ELSEIF (REPORT(4:6) .EQ. 'OTR' .OR. REPORT(4:6) .EQ. '0TR') THEN  
        RRR=-1                                                          
                                                                        
! Expand a report when the quantity of rainfall is a measured amount.   
                                                                        
      ELSE                                                              
        READ (REPORT(4:6),'(F3.1)') RRR                                 
      ENDIF                                                             
                                                                        
! Decode rainfall value for new style reports (8 characters IIiiiRRR)   
                                                                        
      ELSEIF(MSGLEN.EQ.8)THEN                                      !1.3 
                                                                   !1.3 
! Expand a report when the quantity of rainfall cannot be measured or   
! is not reported.                                                 !1.3 
                                                                   !1.3 
      IF (REPORT(6:8) .EQ. 'NIL' .OR. REPORT(6:8) .EQ. '999') THEN !1.3 
        RRR=-9999999                                               !1.3 
                                                                   !1.3 
! Expand a report when the quanity of rainfall is a trace.         !1.3 
                                                                   !1.3 
      ELSEIF (REPORT(6:8) .EQ. 'OTR' .OR. REPORT(6:8) .EQ. '0TR') THEN  
        RRR=-1                                                     !1.3 
                                                                   !1.3 
! Expand a report when the quantity of rainfall is a measured amount.   
                                                                   !1.3 
      ELSE                                                         !1.3 
        READ (REPORT(6:8),'(F3.1)') RRR                            !1.3 
      ENDIF                                                        !1.3 
      ENDIF                                                        !1.3 
                                                                        
! Copy the expanded rainfall amount into the expansion array.           
                                                                        
      REALEXP(11)=RRR                                                   
                                                                                                                                                
      RETURN                                                            
      END                                                               
