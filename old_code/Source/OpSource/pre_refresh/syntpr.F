      SUBROUTINE SYNTPR(REGION,HOUR,TFLAG,TPER)                                 

!-----------------------------------------------------------------------
!                                                                              
! SUBROUTINE    : SYNTPR                                                       
!                                                                              
! PURPOSE       : TO DECIDE PERIOD COVERED BY MAX OR MIN TEMPERATURE           
!                 FROM REGIONAL REPORTING PRACTICES (PERIOD MISSING            
!                 IF NON-STANDARD HOUR)                                        
!                                                                              
! CALLED BY     : SYNXP3                                                       
!                                                                              
! PARAMETERS    : (1) REGION INTEGER WMO REGION NUMBER            (I)          
!                 (2) HOUR   INTEGER REPORTING HOUR (GMT)         (I)          
!                 (3) TFLAG  INTEGER 1 FOR MAX OR 2 FOR MIN TEMP  (I)          
!                 (4) TPER   REAL    TIME PERIOD IN HOURS (-VE)   (O)          
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:03$
! $Source: /home/us0400/mdb/op/lib/source/RCS/syntpr.F,v $
!                                                                              
! CHANGE RECORD :
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:03    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:09  usmdb
! Changed preprocessor statement to if (MVS), else....
! Added copyright and modified header - S.Cox
!
! Revision 1.3  98/01/29  11:39:14  11:39:14  usmdb (Generic MetDB account)
! Addition of IBM preprocess directive
! 
! Revision 1.2  97/07/31  11:39:53  11:39:53  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.1  1997/07/04 13:50:47  uspm
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

      INTEGER REGION,HOUR,TFLAG                                                 
      CHARACTER*132 HEAD
      REAL    TPER                                                              
      LOGICAL MAX,MIN                                                           

#if defined (MVS)
      INCLUDE (SYNCOM)                                                          
#else
      INCLUDE 'SYNCOM'
#endif                                                          

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/syntpr.F,v $
     &'//'$ $Date: 30/01/2006 20:25:03$ $Revision: 1$'
      
      MAX=TFLAG.EQ.1                                                            
      MIN=TFLAG.EQ.2                                                            
*                                                                               
      TPER=RMISS                                                                
*                                                                               
      IF (REGION.EQ.1) THEN                                                     
        IF (HOUR.EQ.18.AND.MAX) TPER=-12.                                       
        IF (HOUR.EQ.06.AND.MIN) TPER=-12.                                       
*                                                                               
      ELSE IF (REGION.EQ.2) THEN                                                
        TPER=-12.                                                               
*                                                                               
      ELSE IF (REGION.EQ.3) THEN                                                
        IF (HOUR.EQ.00.AND.MAX) TPER=-24.                                       
        IF (HOUR.EQ.12.AND.MIN) TPER=-24.                                       
*                                                                               
      ELSE IF (REGION.EQ.4) THEN                                                
        IF (MAX) THEN                                                           
          IF (HOUR.EQ.00) TPER=-12.                                             
          IF (HOUR.EQ.06) TPER=-24.                                             
          IF (HOUR.EQ.12) TPER=-36.                                             
          IF (HOUR.EQ.18) TPER=-12.                                             
        ELSE IF (MIN) THEN                                                      
          IF (HOUR.EQ.00) TPER=-18.                                             
          IF (HOUR.EQ.06) TPER=-24.                                             
          IF (HOUR.EQ.12) TPER=-12.                                             
          IF (HOUR.EQ.18) TPER=-24.                                             
        ENDIF                                                                   
*                                                                               
      ELSE IF (REGION.EQ.5) THEN                                                
        IF (HOUR.EQ.12.AND.MAX) TPER=-24.                                       
        IF (HOUR.EQ.00.AND.MIN) TPER=-24.                                       
*                                                                               
      ELSE IF (REGION.EQ.6) THEN                                                
        IF (HOUR.EQ.18.OR.HOUR.EQ.06) TPER=-12.                                 
      ENDIF                                                                     
      RETURN                                                                    
      END                                                                       
