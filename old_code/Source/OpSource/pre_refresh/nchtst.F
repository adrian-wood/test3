      SUBROUTINE NCHTST(POINT,LENGTH,ALLFIG,INUMB,OSPACE,OLFCR,BULL)    

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : NCHTST
!                                                                      
! PURPOSE       : TO SEE IF A CHARACTER STRING IS ALL FIGURES.         
!                 IF NOT, RETURNS NUMBER OF CHARACTER LAST CHECKED.    
!                                                                      
! CALLED BY     : BULHED, AMDCOR, TAFBUL, METBUL, SAMBUL, TAFBUL,      
!                 CLMBUL, SYNBUL, SYNHED                               
!                                                                      
! CALLS         : NOTHING                                              
!                                                                      
! PARAMETERS    : (1)  POINT   FIRST CHARACTER TO BE TESTED            
!                 (2)  LENGTH  LENGTH OF CHARACTER STRINGTO BE TESTED  
!                 (3)  ALLFIG  LOGICAL SET IF ALL FIGURES              
!                 (4)  INUMB   FIRST NON-FIGURE FOUND                  
!                 (5)  OSPACE  LOGICAL SET IF SPACE AFTER FIGURES      
!                 (6)  OLFCR   LOGICAL SET IF LF/CR AFTER FIGURES      
!                 (7)  BULL    REPORT DATA                             
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:40$
! $Source: /home/us0400/mdb/op/lib/source/RCS/nchtst.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:40    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:39  usmdb
! Changed preprocessor statement to if (MVS), else ...
! Added copyright and modified header - S.Cox
!
! Revision 1.4  98/01/29  11:24:15  11:24:15  usmdb (Generic MetDB accou
! Addition of IBM preprocess directive
! 
! Revision 1.3  97/07/31  09:31:23  09:31:23  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.2  1997/07/03 15:27:56  uspm
! Latest version from MVS - Y2K check
!
! Revision 1.1  1997/05/22 09:06:41  uspm
! Initial revision
!
! 03/06/96 (A.M)  REMOVAL OF COMMOON BLOCK 'TBULL', REPLACED BY NEW   
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
                                                                        
      LOGICAL        ALLFIG                                             
      LOGICAL        OSPACE                                             
      LOGICAL        OLFCR                                              
                                                                        
      INTEGER        LENGTH                                             
      INTEGER        POINT                                              
                                                                        
      CHARACTER*(*)  BULL                                               
      CHARACTER*1    SPACE                                              
      CHARACTER*1    LF                                                 
      CHARACTER*1    CR                                                 
      CHARACTER*132  HEAD
                                                                        
#if defined (MVS)
      DATA SPACE/' '/,CR/Z15/,LF/Z25/                                   
#else
      DATA SPACE/' '/
      CR=CHAR(13)
      LF=CHAR(10)
#endif                                                                  
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/nchtst.F,v $
     &'//'$ $Date: 30/01/2006 20:23:40$ $Revision: 1$'

      ALLFIG=.TRUE.                                                     
      OSPACE=.FALSE.                                                    
      OLFCR =.FALSE.                                                    

      DO 10 INUM=POINT,POINT+LENGTH-1                                   
      IF (BULL(INUM:INUM).LT.'0' .OR. BULL(INUM:INUM).GT.'9') THEN      
        ALLFIG=.FALSE.                                                  
        IF (BULL(INUM:INUM).EQ.SPACE) OSPACE=.TRUE.                     
        IF (BULL(INUM:INUM).EQ.LF) OLFCR=.TRUE.                         
        IF (BULL(INUM:INUM).EQ.CR) OLFCR=.TRUE.                         
        INUMB=INUM-POINT+1                                              
        RETURN                                                          
      ENDIF                                                             
 10   CONTINUE                                                          

      INUMB=LENGTH                                                      
      RETURN                                                            
      END                                                               
