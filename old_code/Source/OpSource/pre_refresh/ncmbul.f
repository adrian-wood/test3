      SUBROUTINE NCMBUL(POINT,BULEND,TTAAII,CCCC,YYGGGG,            !2.0
     &                  OCOR,CORNUM,MIMJ,NFTNCM,BULL)               !2.0
                                                                        
!-----------------------------------------------------------------------
!                                                                      
! PROGRAM     : NCMBUL
!                                                                      
! PURPOSE     : TO IDENTIFY INDIVIDUAL REPORTS WITHIN AN              
!               NCM BULLETIN AND EXTRACT RELEVANT HEADER              
!               INFORMATION TO FORM INDEX AND TO FORM 44              
!               BYTE HEADER THAT WILL PRECEED THE CHARACTER           
!               REPORT. NO EXPANSION WILL BE DONE ON STORAGE          
!                                                                      
! CALLED BY   : MDBSTOR                                   
!                                                                      
! CALLS       : OBHOUR,NCMIND,SETHDR,TAFREP,STAPOS                    
!                                                                      
! PARAMETERS  : POINT  - CURRENT POSITION IN BULLETIN                 
!                        (BEFORE STN ID IN FIRST REPORT)              
!               BULEND - END OF BULLETIN (BEFORE NNNN)                
!               TTAAII - WILL BE NCNC1, NCNC2, OR NCNC80              
!               CCCC   - COLLECTING CENTRE                            
!               YYGGGG - DAY AND HOUR OF REPORT                       
!               OCOR   - CORREECTED REPORT FLAG                       
!               CORNUM - CORRECTION NUMBER                            
!               MIMJ   -                                              !e
!               NFTNCM - FT NUMBER FOR NCM MDB DATASET                
!               BULL   - REPORT DATA                                  
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:42$
! $Source: /home/us0400/mdb/op/lib/source/RCS/ncmbul.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:42    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:40  usmdb
! Separated variable declaration and initialisation. Removed
! unused dummy arguments OAMD and AMDNUM. Removed argument
! STNID from call to NCMIND as not used in NCMIND. Added
! copyright and modified header - S.Cox
!
! Revision 1.3  98/03/12  09:29:39  09:29:39  usmdb (Generic MetDB accou
! Dont call STAPOS for Block 99 stations.
!                                                             
! Revision 1.2  1997/07/31 09:31:33  uspm                               
! First revision for 1                                                  
!                                                                       
! Revision 1.1  1997/07/04 13:02:12  uspm                               
! Initial revision                                                      
!                                                                       
! mar 98    Stop calling STAPOS when WMO block is 99 (UK             !i
!           CDL reports) J.Norton.                                    
!                                                                      
! mar 98    Tidy up unreferenced variables, use some more            !h
!           commonly used variable names and add IMPLICIT             
!           NONE statement.                                           
!                                                                      
! mar 98    Add validation of date and time values.                  !g
!
! mar 98    Change handling of errors from call to STAPOS            !f
!
! mar 98    Add handling OLDC reports.                               !e
!
! jan 98    Set value for preferred report option.                   !d
!
! jan 98    Add additional tests for station position.               !c
!
! 05/06/96  REMOVE COMMON BLOCK 'TBULL', REPLACED BY A NEW            
!           PARAMETER 'BULL'. REMOVAL OF ARRAY 'TOR' WHICH            
!           SERVED NO PURPOSE. NOT SURE WHY IT WAS INCLUDED.          
!
! MAR 96: GET POSITION FROM STAPOS RATHER THAN MDB                   !B
!
! 10/04/95  TWO ARGUMENTS ADDED TO OBHOUR CALL (TOL, RC)             !A
!
! INTRODUCED  : 06/03/95                                              
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
                                                                        
*************************************************************           
*                                                                       
*     VARIABLE EXPLANATIONS                                             
*    ***********************                                            
*                                                                       
*     STNID     - STATION IDENTIFIER - FOR UK STATIONS THIS WILL        
*                 BE REPORTED IN 3 FIGURES AND WILL BE PADDED OUT       
*                 TO 5 WITH BLOCK NO. FOR CONSISTENCY WITH              
*                 OVERSEAS STATIONS                                     
*                 USED FOR INDEX AND 44 BYTE HEADER                     
*     LAT,LON   - LAT AND LON OF STATION, EXTRACTED BY CALLING          
*                 STATION MASTER                                        
*                 USED IN INDEX AND 44 BYTE HEADER                      
*     NOW(9)    - SYSTEM DATE AND TIME                                  
*     OBDATE(5) - DATE AND TIME OF OBSERVATION                          
*                        (1)=YEAR,(2)=MONTH,(3)=DAY,(4)=HOUR,(5)=MIN    
*     OBDAY,OBHR- DAY AND HOUR OF OBSERVATION CONTAINED IN              
*                 BULLETIN HEADING                                      
*     TOL       - TOLERANCE (HOURS) FOR DECIDING WHETHER REPORT TOO OLD 
*     RC        - RETURN CODE FROM OBHOUR (0=OK, 4=TOLERANCE EXCEEDED)  
*     HEADER    - 44 BYTE HEADER THAT PRECEEDS CHARACTER REPORT         
*     RMISS     - MISSING DATA VALUE = -9999999.0                       
*                                                                       
*************************************************************           
                                                                        
*************************************************************           
*                                                                       
*     VARIABLE DECLARATIONS                                             
*                                                                       
*************************************************************           

      IMPLICIT NONE                                                  !h 
                                                                     !h 
      CHARACTER*(*)      BULL                                        !h 
      CHARACTER*4096     REPORT                                      !h 
                                                                     !h 
      CHARACTER*23       ENTRY         ! index entry                 !h 
      CHARACTER*(*)      YYGGGG,TTAAII                               !h 
      CHARACTER*5        STNID         ! station id                  !h 
      CHARACTER*(*)      CCCC                                        !h 
      CHARACTER*(*)      CORNUM                                     !2.0
      CHARACTER*132      HEAD                                        !h 
      CHARACTER*7        MIMJ                                        !h 
                                                                     !h 
      REAL               REXP(300)     ! report expansion array     !dh 
      REAL               LAT           ! latitude                   !Bh 
      REAL               LON           ! longitude                  !Bh 
      REAL               STNHTP        ! pressure sensor height     !Bh 
      REAL               STNHT         ! station height             !Bh 
      REAL               RMISS                                      !2.0
      INTEGER            NELM          ! number of non-flagged values!d 
      INTEGER            POINT         ! start of report in bulletin !h 
      INTEGER            BULEND        ! end of bulletin             !h 
      INTEGER            POS           ! position in report          !h 
      INTEGER            REPEND        ! end of report position within  
                                       ! bulletin                    !h 
      INTEGER            REPLEN        ! length of report            !h 
      INTEGER            NFTNCM        !                             !h 
      INTEGER            BLKSIZ                                      !h 
      INTEGER            NOW(9)                                      !h 
      INTEGER            OBDATE(5)                                   !h 
      INTEGER            OBDAY,OBHR,OBMIN,TOL,RC                     !h 
      INTEGER            IRC,I                                       !h 
      INTEGER            IVALUE                                      !h 
                                                                     !h 
      LOGICAL            OLDC         ! true if oldc report          eh 
      LOGICAL            OCOR                                       !2.0

      DATA               RMISS/-9999999.0/                          !2.0
      HEAD='                                                            
     &$Source: /home/us0400/mdb/op/lib/source/RCS/ncmbul.F,v $         
     &'//'$ $Date: 30/01/2006 20:23:42$ $Revision: 1$'                
                                                                        
*************************************************************           
*                                                                       
*     EDIT BULLETIN OF CR, LF AND DOUBLE SPACES                         
*                                                                       
*************************************************************           
                                                                        
      CALL BULLED(POINT,BULEND,BULL)                                    
                                                                        
*************************************************************           
*                                                                       
*     LOOP ROUND REPORTS IN BULLETIN                                    
*                                                                       
*************************************************************           
                                                                        
      DO WHILE (POINT.LT.BULEND)                                        
                                                                        
*************************************************************           
*                                                                       
*       FIND END OF NEXT REPORT IN BULLETIN, LENGTH OF REPORT           
*       POINTER WITHIN EACH REPORT                                      
*                                                                       
*                                                                       
**************************************************************          
                                                                        
        IF (BULL(POINT:POINT).EQ.' ') POINT = POINT + 1                 
                                                                        
        REPLEN = INDEX(BULL(POINT:),'=')                                
        REPEND = REPLEN + POINT - 1                                     
        REPORT = BULL(POINT:REPEND)                                     
                                                                        
*************************************************************           
*                                                                       
*       MOVE PAST OLDC AT START OF REPORTS                              
*                                                                       
*************************************************************           
                                                                        
                                                                     !e 
        IF(MIMJ.EQ.'OLDC'.AND.REPORT(1:5).EQ.'OLDC ')THEN            !e 
          POS = 6                                                    !e 
        ELSE                                                         !e 
          POS = 1                                                    !e 
        ENDIF                                                        !e 
                                                                        
        IF(POS.GE.REPLEN)THEN                                        !e 
          PRINT*,' NCMBUL SHORT REPORT >',BULL(POINT:REPEND),'<'     !e 
          GOTO 999                                                   !e 
        ENDIF                                                        !e 
                                                                        
*************************************************************           
*                                                                       
*       RESET DATE AND TIME GROUP FOR OLDC REPORTS                      
*                                                                       
*************************************************************           
                                                                        
        IF(MIMJ.EQ.'OLDC')THEN                                       !e 
          YYGGGG(1:4)=REPORT(POS:POS+3)                              !e 
          IF(REPORT(POS+4:POS+6).EQ.'00 ')THEN                       !e 
            POS=POS+7                                                !e 
          ELSE                                                       !e 
            POS=POS+5                                                !e 
          ENDIF                                                      !e 
          YYGGGG(5:6)='00'                                           !e 
          OLDC=.TRUE.                                                !e 
        ELSE                                                         !e 
          OLDC=.FALSE.                                               !e 
        ENDIF                                                        !e 
                                                                        
        IF(POS.GE.REPLEN)THEN                                        !e 
          PRINT*,' NCMBUL SHORT REPORT >',BULL(POINT:REPEND),'<'     !e 
          GOTO 999                                                   !e 
        ELSEIF(POS.GT.1)THEN                                         !e 
*                                                                       
*       REPOSITION REPORT TO START OF STATION ID                        
*                                                                       
          REPORT(1:REPLEN-POS+1)=REPORT(POS:REPLEN)                  !e 
          REPLEN=(REPLEN-POS)+1                                      !e 
          POS=1                                                      !e 
        ENDIF                                                        !e 
                                                                        
*************************************************************           
*                                                                       
*       INITIALISATION                                                  
*                                                                       
*************************************************************           
                                                                        
        BLKSIZ = 27998                                                  
                                                                        
        LAT = RMISS                                                     
        LON = RMISS                                                     
                                                                        
*************************************************************           
*                                                                       
*       POINTER SHOULD BE AT BEGINNING OF STN ID GROUP                  
*                                                                       
*       CHECK IF THIS GROUP IS 3 OR 5 FIGURE GROUP                      
*       IF IT IS A 3 FIGURE GROUP THEN PAD OUT TO 5 WITH WMO            
*       BLOCK NUMBER, IF IT IS 5 IT ALREADY HAS BLOCK NUMBER.           
*                                                                       
*************************************************************           
                                                                        
        IF (REPORT(POS+3:POS+3).EQ.' ') THEN                            
          STNID(3:5) = REPORT(POS:POS+2)                                
          STNID(1:2) = '03'                                             
        ELSE                                                            
          STNID(1:5) = REPORT(POS:POS+4)                                
        ENDIF                                                           
                                                                        
                                                                        
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
!There are three calls to stapos. The first looks up the requested    ! 
!station in the surface list. If the requested station is not found   ! 
!then a second call to stapos looks up the 'x' list. This list contains 
!stations that have not been identified as being upperair/surface.    ! 
!If the requested station is not still not found then a third call to ! 
!stapos looks up the 'u' list. This list contains stations that have  ! 
!been identified as being upperair stations.                          ! 
!If after the third call the requested station details are still not  ! 
!available then a message is output to warn of the situation          ! 
!NOTE. Function IVALUE returns missing data value if non numeric      ! 
!      character found in input string.                               ! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
                                                                        
        IF(STNID(1:2).EQ.'99')THEN                                   !i 
          LAT=RMISS                                                  !i 
          LON=RMISS                                                  !i 
          STNHTP=RMISS                                               !i 
          STNHT=RMISS                                                !i 
        ELSE                                                         !i 
          CALL STAPOS(IVALUE(STNID),'S',LAT,LON,STNHTP,STNHT,IRC)   !Bf 
                                                                        
          IF (IRC.EQ.16) THEN                                        !f 
            GOTO 999                                                 !f 
          ENDIF                                                      !f 
                                                                        
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !f 
            IRC=0                                                    !f 
            CALL STAPOS(IVALUE(STNID),'X',LAT,LON,STNHTP,STNHT,IRC)  !f 
          ENDIF                                                      !f 
                                                                        
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !f 
            IRC=0                                                    !f 
            CALL STAPOS(IVALUE(STNID),'U',LAT,LON,STNHTP,STNHT,IRC)  !f 
          ENDIF                                                      !f 
                                                                        
          IF ((LAT .EQ. RMISS) .AND. (LON .EQ. RMISS)) THEN          !f 
            WRITE(6,*)' NCMS (NCMBUL)--> STATION ',                     
     &          'NOT FOUND IN STAPOS ',STNID                         !f 
          ENDIF                                                      !f 
        ENDIF                                                        !i 
                                                                        
*************************************************************           
*                                                                       
*       CALL SYSTEM CLOCK TO GET TIME OF RECEIPT                        
*                                                                       
*************************************************************           
                                                                        
        CALL DATIM(NOW)                                                 
                                                                        
*************************************************************           
*                                                                       
*       DAY AND TIME OF OBSERVATION IS YYGGGG                           
*       NEED TO IDENTIFY MONTH AND YEAR OF OBSERVATION                  
*       CALL TO ROUTINE OBHOUR TO RETURN DATE                           
*        - ALLOWS FOR LAST MONTHS/YEARS DATA                            
*                                                                       
*************************************************************           
                                                                        
*       CONVERT CHARACTER FORM OF DAY AND HOUR TO INTEGER FORM          
*       ALSO EXTRACT MINTES OF OBSERVATION FOR REPORT HEADER            
        READ (YYGGGG(1:2),'(I2)') OBDAY                                 
        READ (YYGGGG(3:4),'(I2)') OBHR                                  
        READ (YYGGGG(5:6),'(I2)') OBMIN                                 
                                                                        
        IF(OBDAY.LT.1.OR.OBDAY.GT.31)THEN                          !g   
          PRINT*,' NCMBUL REPORT WITH INVALID OBDAY ',YYGGGG(1:2)  !g   
          GOTO 999                                                 !g   
        ENDIF                                                      !g   
        IF(OBHR.LT.0.OR.OBHR.GT.24)THEN                            !g   
          PRINT*,' NCMBUL REPORT WITH INVALID OBHR ',YYGGGG(3:4)   !g   
          GOTO 999                                                 !g   
        ENDIF                                                      !g   
        IF(OBMIN.LT.0.OR.OBMIN.GT.59)THEN                          !g   
          PRINT*,' NCMBUL REPORT WITH INVALID OBMIN ',YYGGGG(5:6)  !g   
          GOTO 999                                                 !g   
        ENDIF                                                      !g   
                                                                        
        OBDATE(3) = OBDAY                                               
        OBDATE(4) = OBHR                                                
        OBDATE(5) = OBMIN                                               
                                                                        
        TOL = 3                                    ! A                  
        CALL OBHOUR(OBDATE,OBDAY,OBHR,' ',TOL,RC)  ! A                  
        IF (RC .EQ. 4) RETURN                      ! A                  
                                                                        
*************************************************************           
*                                                                       
*       INDEX TYPE - 23BYTE CHAINED.                                    
*       CALL TO NCMIND TO COMPILE INDEX ENTRY                           
*                                                                       
*************************************************************           
                                                                        
        CALL NCMIND(ENTRY,OCOR,CORNUM,CCCC,TTAAII,                      
     &              OBHR,OBMIN,LAT,LON)                             !2.0
                                                                        
*************************************************************    !d     
*                                                                !d     
*       Expand report to find number of elements for setting     !d     
*       preferred flag.                                          !d     
*                                                                !d     
*************************************************************    !d     
                                                                 !d     
        DO I =1,300                                              !d     
          REXP(I)=RMISS                                          !d     
        ENDDO                                                    !d     
                                                                 !d     
        REXP(7)=OBHR                                             !d     
        REXP(1)=IVALUE(STNID)                                    !d     
                                                                 !d     
        CALL NCMEXP(REPORT,REPLEN,REXP)                          !d     
                                                                 !d     
! count met values                                               !d     
                                                                 !d     
        NELM=0                                                   !d     
                                                                 !d     
        DO I =11,54                                              !d     
          IF(REXP(I).NE.RMISS)NELM=NELM+1                        !d     
        ENDDO                                                    !d     
                                                                 !d     
! subtract flagged values                                        !d     
                                                                 !d     
        DO I =56,79                                              !d     
          IF(REXP(I).EQ.1)NELM=NELM-1                            !d     
        ENDDO                                                    !d     
                                                                 !d     
        IF(NELM.LT.0)NELM=0                                      !d     
                                                                        
        ENTRY(12:12)=CHAR(NELM)                                  !d     
                                                                        
*************************************************************           
*                                                                       
*       FINALLY STORE REPORT                                            
*       CALL TO TAFREP                                                  
*                                                                       
*************************************************************           
                                                                        
        IF(MIMJ.EQ.'OLDC')                                              
     &    PRINT*,' NCMBUL OLDC OBDATE ',OBDATE,' ENTRY >',ENTRY,'<'     
        CALL TAFREP(OBDATE,ENTRY,REPORT(1:REPLEN),NFTNCM,BLKSIZ,STNID)  
                                                                        
        POINT = REPEND + 2    ! BEGINNING OF NEXT REPORT                
                                                                        
      END DO  !WHILE (POINT.LT.BULEND)                                  
                                                                        
999   RETURN                                                            
      END                                                               
