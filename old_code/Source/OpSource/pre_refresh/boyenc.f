      SUBROUTINE BOYENC(ARRAY,DATIME,TTAAII,CCCC,OCOR,DRGFLG,NFT)   !2.0

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : BOYENC
!                                                                      
! PURPOSE       : TO ENCODE BUOY MESSAGE INTO BUFR AND STORE                                                 
!                                                                      
! CALLED BY     : BUOY MDBSTOR
!                                                                      
! CALLS         : BOYIND, CCCODE, AIRSTO                            !1.6          
!                                                                      
! PARAMETERS    : (ALL INPUT)                                          
!                 ARRAY  - EXPANDED ARRAY OF ELEMENTS                   
!                 DATIME - TIME OF OB ARRAY                             
!                 TTAAII -                                              
!                 CCCC   - COLLECTING CENTRE                        !1.6          
!                 OCOR   - CORRECTED REPORT FLAG                        
!                 DRGFLG - DROGUE FLAG                                  
!                 NFT    - FT NUMBER FOR MDB DATASET                    
!                                                                      
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:04$
! $Source: /home/us0400/mdb/op/lib/source/RCS/boyenc.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:04    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:32  usmdb
! Removed unused dummy argument REPLEN. Removed unused variables,
! added copyright and modified header - S.Cox
!
! Revision 1.6  2000/03/10  10:25:22  10:25:22  usmdb
! 20 March 2000     C Long
! 1.6  Call AIRSTO rather than AMDSTO. Call CCCODE here rather than
!      in BUOY, passing CCCC from BUOY to go in trailer. Remove code
!      to reset year after BUFR encode - no longer needed. 
! 
! Revision 1.5  99/02/11  12:23:27  12:23:27  usmdb
! 15-02-1999. Increase decoded elements array from 400 to 600 to cope
! with large reports. Jon Lewthwaite
!
! Revision 1.4  98/09/16  16:10:46  16:10:46  usmdb
! 21/09/1998 Increase the size of the elements array to allow for
! large messages with many levels. Jon Lewthwaite
!
! Revision 1.3  97/10/28  08:43:56  08:43:56  usjl (Jon Lewthwaite)
! WMO CODE CHANGES - NEW LOCALSEQ GENERATED                           !A
!
! Revision 1.2  1997/07/31 09:12:24  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 11:01:23  uspm
! Initial revision
!
! INTRODUCED  : 02/10/94                                              
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        
                                                                        
      IMPLICIT NONE                                                     
                                                                        
      CHARACTER*4       CCCC                                       !1.6 
      CHARACTER*6       TTAAII                                          
      CHARACTER*10      NAMES                                           
      CHARACTER*23      ENTRY                                           
      CHARACTER*3000    MESAGE                                          
      CHARACTER*132     HEAD
      CHARACTER*9       IDENT                                      !1.6 

      REAL              ARRAY(0:600)                          !1.5 !1.4 
                                                                        
      INTEGER           DATIME(5),NOW(8),TOR(5)                         
      INTEGER           ICCCC,I                                         
      INTEGER           NFT,BLKSIZ                                      
      INTEGER           IDES,NDES                                       
      INTEGER           DESCR(300)                                      
      INTEGER           NELEM,NOBS,LEN                                  
                                                                        
      LOGICAL           CMPRES                                      !2.0
      LOGICAL           OCOR,DRGFLG                                     

      DATA CMPRES/.FALSE./                                          !2.0

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/boyenc.F,v $
     &'//'$ $Date: 30/01/2006 20:21:04$ $Revision: 1$'

*************************************************************           
*                                                                       
*     SET CLASS 3 DESCRIPTOR ACCORDING TO WHETHER DROGUE                
*     IS PRESENT - DIFFERENT DESCRIPTOR LISTS                           
*                                                                       
*************************************************************           
                                                                        
      IF (DRGFLG) THEN                                                  
        DESCR(1) = IDES(331201)  !WAS 331199                         !A 
      ELSE                                                              
        DESCR(1) = IDES(331200)  !WAS 331198                         !A 
      ENDIF                                                             
                                                                        
*************************************************************           
*                                                                       
*     SET UP PARAMETERS FOR ENBUFR INTERFACE                            
*     NDES    - NUMBER OF DESCRIPTORS                                   
*     NELEM   - NUMBER OF ELEMENTS                                      
*     NOBS    - NUMBER OF REPORTS PER BUFR MESSAGE                      
*     NAMES   - STRING (EMPTY FOR BUOY TYPE)                            
*     CMPRES  - DATA COMPRESSION FLAG                                   
*     BLKSIZ  - BLOCKSIZE FOR MDB DATASET                               
*     TOR     - TIME OF RECEIPT ARRAY                                   
*     MESAGE  - OUTPUT BUFR MESAGE                                      
*     LEN     - LENGTH OF OUTPUT BUFR MESSAGE                           
*                                                                       
*************************************************************           
                                                                        
      NDES = 1                                                          
      NELEM = 600                                             !1.5 !1.4 
      NOBS = 1                                                          
      NAMES = ' '                                                       
      CMPRES = .FALSE.                                                  
      BLKSIZ = 27998                                               !1.6 
                                                                        
      CALL DATIM(NOW)                                                   
      DO 10 I = 0,4                                                     
        TOR(I+1) = NOW(8-I)                                             
10    CONTINUE                                                          
                                                                        
*************************************************************           
*                                                                       
*     ENCODE INTO BUFR MESSAGE                                          
*                                                                       
*************************************************************           
                                                                        
      CALL ENBUFR(DESCR,ARRAY,NDES,NELEM,NOBS,NAMES,TOR,                
     &            MESAGE,CMPRES,LEN)                                    
                                                                        
*************************************************************           
*                                                                       
* Set collecting centre & data type in section 1 of BUFR message   !1.6 
* (displacements for messages without total length at start!)      !1.6 
*                                                                       
*************************************************************           
                                                                        
      CALL CCCODE(287,ICCCC,CCCC)   ! CCCC code from table 001031  !1.6 
                                                                        
      IF (ICCCC.NE.65536) THEN      ! 2 BYTES OF 1'S - MISSING DATA     
        MESAGE(9:9) = CHAR(ICCCC/256)                                   
        MESAGE(10:10) = CHAR(MOD(ICCCC,256))                            
      ENDIF                                                             
                                                                        
      MESAGE(13:13) = CHAR(31)      ! DATA TYPE - OCEANOGRAPHIC         
                                                                        
*************************************************************           
*                                                                       
*     COMPILE INDEX ENTRY                                               
*                                                                       
*************************************************************           
                                                                        
      CALL BOYIND(ARRAY,OCOR,DRGFLG,ENTRY)                              
                                                                        
*************************************************************           
*                                                                       
*     STORE MESSAGE                                                     
*                                                                       
*************************************************************           
                                                                        
                                                                        
      IDENT=ENTRY(3:11)                                            !1.6
      IF (OCOR) THEN                                               !1.6
        ENTRY(3:11)=TTAAII(3:6)//CHAR(1)//CCCC                     !1.6
      ELSE                                                         !1.6
        ENTRY(3:11)=TTAAII(3:6)//CHAR(0)//CCCC                     !1.6
      ENDIF                                                        !1.6
      CALL AIRSTO(DATIME,ENTRY,MESAGE(1:LEN),NFT,BLKSIZ,IDENT,TOR) !1.6 
                                                                        
      RETURN                                                            
      END                                                               
