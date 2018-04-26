      SUBROUTINE STBENC(DCVALS, NELEM, NOBS, DESCR,                     
     &                  MESSAGE, MESSLEN, DATETIME)                     

!-----------------------------------------------------------------------
!                                                                     
! PROGRAM       : STBENC                                              
!                                                                     
! PURPOSE       : INITIALISES VALUES BEFORE A CALL TO BUFR ENCODING     
!                 SOFTWARE                                              
!                                                                      
! CALLED BY     : SATOB1                                               
!                                                                      
! CALLS         : ENBUFR                                                
!                                                                      
! PARAMETERS    : (1) DCVALS - DECODED VALUES OCCUPYING NOBS*NELEM      
!                              LENGTH IN REAL ARRAY                     
!                 (2) NELEM - NUMBER OF ELEMENTS                        
!                 (3) NOBS - NUMBER OF 'OBS'                            
!                 (4) DESCR - INTEGER LIST OF BUFR DESCRIPTORS          
!                 (5) MESSAGE - CHARACTER STRING FOR BUFR'ED MESSAGE    
!                 (6) MESSLEN - LENGTH OF BUFR'ED MESSAGE IN OCTETS     
!                 (7) DATETIME - YEAR, MONTH, DAY, HOUR, MINUTE ARRAY   
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:31$
! $Source: /home/us0400/mdb/op/lib/source/RCS/stbenc.F,v $
!
! CHANGE RECORD :                                                      
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:31    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:59  usmdb
! Removed unused variables. Separated variable declaration and
! initialisation. Changed pre-processor statement to be if (MVS),
! else... Added copyright and modified header - S.Cox
!
! Revision 1.5  98/01/29  11:27:54  11:27:54  usmdb (Generic MetDB accou
! Addition of IBM preprocess directive
! 
! Revision 1.4  97/07/31  11:35:13  11:35:13  uspm (Pat McCormack)
! First revision for MVS
! 
! Revision 1.3  1997/07/04 07:56:10  uspm
! Latest version from MVS - Y2K check
!
! Revision 1.2  1997/05/15 14:52:12  uspm
! Add revision info and preprocessor checks around initialisation of
! variable BUFR
!
! MAY0794 : Change noted by "ENTRY" but not documented.               
!                                                                     
! NOV 93  : FIRST WRITTEN                                             
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
                                                                       
      INTEGER NDESCR, NELEM, NOBS, MESSLEN                              
      INTEGER DESCR(*)                                                  
      INTEGER DATETIME(*)                                               
                                                                       
      REAL DCVALS(*)                                                    
                                                                       
      LOGICAL CMPRES, DISPLAY                                           
                                                                       
      CHARACTER*4 BUFR                                              !2.0
      CHARACTER*132 CHEAD
      CHARACTER NAMES                                                   
      CHARACTER*(*) MESSAGE                                             

#if defined (MVS)
      DATA BUFR/Z42554652/                                          !2.0
#else
      DATA BUFR/'BUFR'/                                             !2.0
#endif
      
      CHEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/stbenc.F,v $
     &'//'$ $Date: 30/01/2006 20:24:31$ $Revision: 1$'

      NDESCR  = 1                                                       
      CMPRES  = .TRUE.                                                  
      DISPLAY = .FALSE.                                                 
      MESSAGE = ' '                                                     
                                                                       
      CALL ENBUFR(DESCR, DCVALS, NDESCR, NELEM, NOBS, NAMES, DATETIME,  
     &            MESSAGE, CMPRES, MESSLEN)                             
      IF (MESSAGE(1:4) .NE. BUFR) THEN                                  
         PRINT*, 'A SATOB BULLETIN NOT SENT TO MDB'                     
         RETURN                                                         
      END IF                                                            
                                                                       
      NOBS = 10000                                                      
  100 FORMAT (1X, 7F12.2)                                               
                                                                       
      RETURN                                                            
      END                                                               
