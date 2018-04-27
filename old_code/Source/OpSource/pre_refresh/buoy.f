      SUBROUTINE BUOY(POINT,BULEND,TTAAII,CCCC,OCOR,MIMJ,NFT,BULL)      
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : BUOY
!                                                                      
! PURPOSE       : TO ENCODE AND STORE BUOY/DRIFTER MESSAGES             
!                                                                      
! CALLED BY     : MDBSTOR
!                                                                      
! CALLS         : BULLED, BOYEXP, BOYENC                            !1.6          
!                                                                      
! PARAMETERS    : POINT - BULLETIN POINTER                              
!                 BULEND - END OF BULLETIN POINTER                      
!                 TTAAII - BULLETIN IDENTIFIER                          
!                 CCCC - COLLECTING CENTRE                              
!                 OCOR - CORRECTED REPORT FLAG                          
!                 MIMJ - WILL BE 'ZZYY'IN THIS CASE                     
!                 NFT - FT NO FOR STORAGE DATASET                       
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:34$
! $Source: /home/us0400/mdb/op/lib/source/RCS/buoy.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:34    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:40  usmdb
! Removed dummy argument REPLEN from call to BOYENC as not used
! in BOYENC. Removed unused variables, added copyright and
! modified header - S.Cox
!
! Revision 1.6  2000/03/10  10:24:48  10:24:48  usmdb (Generic MetDB acc
! 20 March 2000      C Long
! 1.6  Pass CCCC, not ICCCC, to BOYENC, so don't call CCCODE.
! 
! Revision 1.5  99/02/11  12:21:32  12:21:32  usmdb (Generic MDB account
! 15-02-1999. Increase decoded elements array from 400 to 600 to cope
! with large reports. Jon Lewthwaite
!
! Revision 1.4  98/09/16  16:10:42  16:10:42  usmdb (Generic MDB account
! 21/09/1998 Increase the size of the elements array to allow for large
! messages with many levels.   Jon Lewthwaite
!
! Revision 1.3  98/04/20  07:08:59  07:08:59  usmdb (Generic MDB account
! Dont store obs with missing Lat/Long                                !b
!
! Revision 1.2  1997/07/31 09:14:49  uspm
! First revision for  1
!
! Revision 1.1  1997/07/04 11:18:33  uspm
! Initial revision
!
! 04/06/96 REMOVAL OF COMMON BLOCK 'TBULL', REPLACED BY A NEW
!          PARAMETER 'BULL'.                             
!                                                                      
! 09/01/95 !A CORRECTION TO AVOID INDEX BEING CALCULATED ON AN INVALID
!             STRING                                  
!
! INTRODUCED 02/11/94                                              
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
                                                                        
      CHARACTER*(*)        BULL                                         
      CHARACTER*(*)        TTAAII                                       
      CHARACTER*(*)        CCCC                                         
      CHARACTER*(*)        MIMJ                                         
      CHARACTER*4096       REPORT                                       
      CHARACTER*132        HEAD

      REAL                 ARRAY(0:600)                       !1.5!1.4  
                                                                        
      INTEGER              POINT                                    !2.0
      INTEGER              BULEND                                       
      INTEGER              REPBEG,REPEND,REPLEN                         
      INTEGER              LEN                                          
      INTEGER              DATIME(5)                                    
      INTEGER              NFT                                          
                                                                        
      LOGICAL              OCOR                                     !2.0
      LOGICAL              DROGFLG                                      
                                                                        
*************************************************************           
*                                                                       
*     INITIALISE VARIABLES                                              
*                                                                       
*************************************************************           

      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/buoy.F,v $
     &'//'$ $Date: 30/01/2006 20:21:34$ $Revision: 1$'

*************************************************************           
*                                                                       
*     BULLETIN COMES INTO THE SUBROUTINE WITH POINT AS THE              
*     POSITION OF THE CHARACTER AFTER THE FIRST MIMJ. TO                
*     MAINTAIN CONSISTENCY WITH ALL THE REPORTS IN THE                  
*     BULLETIN THE POSITION IS RESET TO POINT AT THE FIRST              
*     MIMJ.                                                             
*                                                                       
*************************************************************           
                                                                        
10    POINT = POINT - 1                                                 
      IF (BULL(POINT:POINT+3) .NE. 'ZZYY') GOTO 10                      
                                                                        
*************************************************************           
*                                                                       
*     EDIT BULLETIN OF <CR>, <LF> AND DOUBLE SPACES                     
*                                                                       
*************************************************************           
                                                                        
      CALL BULLED (POINT,BULEND,BULL)                                   
                                                                        
*************************************************************           
*                                                                       
*     THE NEXT SECTION CONTAINS THE MAIN EXPANSION AND                  
*     STORAGE CALLS. FIRST THE BEGINNING AND LENGTH OF THE              
*     NEXT REPORT IN THE BULLETIN IS FOUND, THEN THAT REPORT            
*     IS EXPANDED, ENCODED INTO BUFR AND STORED                         
*                                                                       
*************************************************************           
                                                                        
*     FIND BEGINNING AND LENGTH OF REPORT                               
                                                                        
20    IF (BULL(POINT:POINT+3) .NE. MIMJ) THEN                           
        POINT = POINT + 1                                               
        IF (POINT .GT. BULEND) GOTO 999                                 
        GOTO 20                                                         
                                                                        
      ELSE                                                              
                                                                        
        REPBEG = POINT                                                  
                                                                        
*       '=' IS THE DELIMITER OF REPORT, BUT IT ISN'T ALWAYS             
*       THERE SO SEARCH FOR NEXT MIMJ FIRST                             
* !A    ADDED LOOP TO ENSURE THE INDEX FUNCTION IS ONLY CARRIED OUT     
* !A    IF BULEND  > REPBEG+5                                           
                                                                        
        IF (BULEND.LT.(REPBEG+5)) GOTO 999                       !A     
        REPLEN = INDEX(BULL(REPBEG+5:BULEND),MIMJ) + 5 - 1              
        REPEND = REPBEG + REPLEN - 1                                    
                                                                        
*       IF NO MIMJ THEN ASSUME END OF BULLETIN AS END OF REPORT         
                                                                        
        IF (REPLEN .EQ. 4) THEN                                         
          REPEND = BULEND                                               
          REPLEN = REPEND - REPBEG +1                                   
        ENDIF                                                           
                                                                        
*       NOW SEARCH FOR '=' WITHIN REPORT STRING AND REDEFINE            
*       REPLEN AND REPEND IF NECESSARY                                  
                                                                        
        LEN = INDEX(BULL(REPBEG:REPEND),'=') - 1                        
                                                                        
        IF (LEN.GT.-1) THEN                                             
          REPLEN = LEN                                                  
          REPEND = REPBEG + REPLEN -1                                   
        ENDIF                                                           
                                                                        
      ENDIF                                                             
                                                                        
      REPORT(1:REPLEN) = BULL(REPBEG:REPEND)                            
                                                                        
*************************************************************           
*                                                                       
*      EXPAND REPORT                                                    
*                                                                       
*************************************************************           
                                                                        
      CALL BOYEXP(REPORT,REPLEN,DATIME,ARRAY,DROGFLG)                   
                                                                        
*************************************************************           
*                                                                       
*     ENCODE AND STORE REPORT (if it has a lat/long)       !b           
*                                                                       
*************************************************************           
                                                                        
      IF (ARRAY(18).LE.90. .AND. ARRAY(18).GE.-90. .AND.   !b
     &   ARRAY(20).LE.180. .AND. ARRAY(20).GE.-180.) THEN  !b
        CALL BOYENC(ARRAY,DATIME,TTAAII,CCCC,OCOR,DROGFLG,NFT)      !2.0
      ENDIF
                                                                        
*************************************************************           
*                                                                       
*     RESET POINT AT THE END OF REPORT JUST PROCESSED                   
*                                                                       
*************************************************************           
                                                                        
      POINT = REPEND + 1                                                
                                                                        
*************************************************************           
*                                                                       
*     LOOP ROUND TO NEXT REPORT IN BULLETIN                             
*                                                                       
*************************************************************           
                                                                        
      POINT = POINT + 1                                                 
      IF (POINT.GT.BULEND) THEN                                         
        GOTO 999                                                        
      ELSE                                                              
        GOTO 20                                                         
      ENDIF                                                             
                                                                        
999   RETURN                                                            
      END                                                               
