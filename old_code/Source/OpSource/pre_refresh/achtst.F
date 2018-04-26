      SUBROUTINE ACHTST(POINT,LENGTH,ALLETT,ICHRAC,OSPACE,OLFCR,BULL)   
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : ACHTST
!                                                                      
! PURPOSE       : TO SEE IF A CHARACTER STRING IS ALL LETTERS.         
!                 IF NOT, RETURNS NUMBER OF CHARACTER LAST CHECKED.    
!                                                                      
! CALLED BY     : BULHED, AMDCOR, TAFBUL, METBUL, SYNHED               
!                                                                      
! CALLS         : NOTHING                                              
!                                                                      
! PARAMETERS    : (1)  POINT   FIRST CHARACTER TO BE TESTED            
!                 (2)  LENGTH  NUMBER OF CHARACTERS TO BE TESTED       
!                 (3)  ALLETT  LOGICAL SET IF ALL LETTERS              
!                 (4)  ICHRAC  FIRST NON LETTER FOUND                  
!                 (5)  OSPACE  LOGICAL SET IF SPACE AFTER LETTERS      
!                 (6)  OLFCR   LOGICAL SET IF LF/CR AFTER LETTERS      
!                 (7)  BULL    REPORT DATA                             
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:20:33$
! $Source: /home/us0400/mdb/op/lib/source/RCS/achtst.F,v $
!
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:20:33    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:17  usmdb
! Changed preprocessor directive to if defined (MVS), else...
! Replaced labelled DO loop with normal DO loop. Added copyright
! and modified header - S.Cox
!
! Revision 1.3  98/01/29  11:17:29  11:17:29  usmdb (Generic MetDB accou
! Addition of IBM preprocess directive
! 
! Revision 1.2  98/01/28  16:45:51  16:45:51  uspm (Pat McCormack)
! Add HPUX versions of code to initialise LF, CR, SPACE
! 
! Revision 1.1  1997/07/03 13:23:55  uspm
! Initial revision
!
! 03/06/96 (A.M)  REMOVAL OF COMMON BLOCK TBULL, REPLACED BY          
!                 PARAMETER 'BULL'.                                   
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

      LOGICAL        ALLETT                                             
      LOGICAL        OSPACE                                             
      LOGICAL        OLFCR                                              
                                                                        
      INTEGER        LENGTH                                             
      INTEGER        POINT                                              
                                                                        
      CHARACTER*(*)  BULL                                               
      CHARACTER*1    CH                                                 
      CHARACTER*1    SPACE                                              
      CHARACTER*1    LF                                                 
      CHARACTER*1    CR                                                 
      CHARACTER*132  HEAD         
#if defined (MVS)      
      DATA SPACE/' '/,CR/Z15/,LF/Z25/                                   
#else
      SPACE=' '
      CR=CHAR(13)
      LF=CHAR(10)
#endif

!-----------------------------------------------------------------------
! revision information
!-----------------------------------------------------------------------
 
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/achtst.F,v $
     &'//'$ $Date: 30/01/2006 20:20:33$ $Revision: 1$'
     
      ALLETT=.TRUE.                                                     
      OSPACE=.FALSE.                                                    
      OLFCR =.FALSE.                                                    
                                                                       
      DO ICHRA=POINT,POINT+LENGTH-1                                 !2.0
        CH=BULL(ICHRA:ICHRA)                                              
        IF (CH.LT.'A' .OR. (CH.GT.'I' .AND. CH.LT.'J') .OR.               
     &     (CH.GT.'R' .AND. CH.LT.'S') .OR. CH.GT.'Z') THEN               
          ALLETT=.FALSE.                                                  
          IF (CH.EQ.SPACE) OSPACE=.TRUE.                                  
          IF (CH.EQ.LF) OLFCR=.TRUE.                                      
          IF (CH.EQ.CR) OLFCR=.TRUE.                                      
          ICHRAC=ICHRA-POINT+1                                            
          RETURN                                                          
        ENDIF                                                             
      ENDDO                                                         !2.0
                                                                       
      ICHRAC=LENGTH                                                     

      RETURN                                                            
      END                                                               
