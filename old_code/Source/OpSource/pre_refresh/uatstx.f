      SUBROUTINE UATSTX(STRING,PTR,LEVEL,L,MAXL,ID,ERROR)                       
                                                                                
      IMPLICIT NONE                                                             

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : UATSTX                                                       
!                                                                              
! PURPOSE       : TRY TO RECOVER FROM A SYNTAX ERROR IN A TTAA/TTCC            
!                                                                              
! DESCRIPTION   : THE AIM IS TO CORRECT THE FOLLOWING ERRORS                   
!                 - A LONG OR SHORT GROUP (SKIP THE LEVEL)                     
!                 - A CORRUPTED LEVEL INDICATOR (PRESSURE FIGURES)             
!                 - A WRONG WIND INDICATOR                                     
!                 A CORRECTION WILL ONLY BE ACCEPTED IF THE REST OF            
!                 THE REPORT HAS THE CORRECT SEQUENCE OF LEVELS,               
!                 THE ASSUMPTION BEING THAT A CORRECTION IS TOO                
!                 RISKY IF THERE IS MORE THAN ONE ERROR.                       
!                                                                              
! DATA TYPE(S)  : UPPER AIR TEMP PART A/C                                      
!                                                                              
! CALLED BY     : UASTDPR                                                      
!                                                                              
! PARAMETERS    : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES           
!                     - UNLESS THERE'S AN ERROR!                  (I/O)         
!                 (2) INPUT POINTER TO WHERE ERROR WAS RECOGNISED (I/O)         
!                 (3) ARRAY OF PRESSURE FIGURES WHICH TAG LEVELS   (I)         
!                 (4) SUBSCRIPT OF CURRENT LEVEL IN ABOVE ARRAY   (I/O)         
!                 (5) SUBSCRIPT OF LAST LEVEL IN ABOVE ARRAY       (I)         
!                 (6) INDICATOR FOR HIGHEST LEVEL WITH WIND (INT) (I/O)         
!                 (7) ERROR FLAG SET TO                            (O)         
!                     0 - first attempt at correction of this report           
!                     1 - no safe correction found                             
!                     2 - long or short group: pointer (and maybe              
!                          next level expected) reset                          
!                     3 - current pressure tag reset (in STRING)               
!                     4 - wind indicator (top level with wind) reset           
!                          (only in variable ID, not in STRING)                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:45$
! $Source: /home/us0400/mdb/op/lib/source/RCS/uatstx.F,v $
!                                                                              
! CHANGE RECORD :
!                                                                              
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:45    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:44:37  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.3  98/02/19  14:13:42  14:13:42  usmdb (Generic MetDB account)
! Code now re-instated in operational system.
!                                                             
! Revision 1.2  1997/07/31 11:46:38  uspm                                       
! First revision for MVS
!                                                                               
! Revision 1.1  1997/07/04 14:49:01  uspm                                       
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
                                                                                
!DECLARE CHARACTER                                                              
      CHARACTER STRING*(*)                                                      
                                                                                
      CHARACTER LEVEL(*)*2                                                      
      CHARACTER IND*1                                                           
      CHARACTER WND*2                                                           
      CHARACTER IL*2                                                            
      CHARACTER HEAD*132                                                        
      CHARACTER BADLEV*2                                                        
                                                                                
!DECLARE INTEGER                                                                
      INTEGER PTR                                                               
      INTEGER PEND                                                              
      INTEGER ERROR                                                             
      INTEGER ID                                                                
      INTEGER BADID                                                             
      INTEGER LEND                                                              
      INTEGER THISLEV                                                           
      INTEGER NEXTLEV                                                           
      INTEGER L                                                                 
      INTEGER I                                                                 
      INTEGER MAXL                                                              
      INTEGER LASTWO                                                            
      INTEGER LASTRI                                                            
      INTEGER NGROUPS                                                           
      INTEGER NX                                                                
      INTEGER J                                                                 
      INTEGER IVALUE                                                            
                                                                                
!INTEGER LOGICAL                                                                
      LOGICAL BADSEQ                                                            
                                                                                
                                                                                
      HEAD='                                                                    
     &$Source: /home/us0400/mdb/op/lib/source/RCS/uatstx.F,v $                 
     &'//'$ $Date: 30/01/2006 20:25:45$ $Revision: 1$'                      
                                                                                
*                                                                               
* Only one attempt at correction: too risky if more than one error.             
* because the sequences of pressure tags & wind indicators are not              
* decreasing, 1000mb being represented by 0, put a figure in front              
* to make it easier to check level against ID.                                  
*                                                                               
      IF (ERROR.GT.0) RETURN         ! ONLY ONE ATTEMPT ALLOWED!                
      IF (PTR.GT.LEN(STRING)) RETURN                                            
*                                                                               
      ERROR=1                        ! SET ERROR IN CASE NO SAFE FIX            
      WND='0'//CHAR(240+ID)          ! TO COPE WITH ZERO IND FOR 1000MB         
      IF (WND.EQ.'00') WND='10'      ! USE 2-FIG IND IN COMPARISONS             
*                                                                               
* Find length of groups left in section (look for start of next section)        
* & keep pointer to end of report for use later.                                
*                                                                               
      LEND=INDEX(STRING(PTR:),' 88')                                            
      IF (LEND.EQ.0) LEND=INDEX(STRING(PTR:),' 77')                             
      IF (LEND.EQ.0) LEND=INDEX(STRING(PTR:),' 66')                             
      IF (LEND.EQ.0) LEND=LEN(STRING(PTR:))                                     
      PEND=PTR+LEND-1                ! KEEP POINTER TO END OF REPORT            
*                                                                               
* If the length left implies a bad group (not 5 figures) look first             
* for the current level a few figures out, then for the                         
* next level (space followed by figures) & make the pointer correspond.         
* The sequence of levels will be checked from there later.                      
*                                                                               
      IF (MOD(LEND,6).NE.0) THEN                                                
        THISLEV=INDEX(STRING(PTR-5:PTR+5),' '//LEVEL(L))                        
        NEXTLEV=INDEX(STRING(PTR:PTR+25),' '//LEVEL(L+1))                       
        IF (THISLEV.GT.0) THEN                                                  
          ERROR=2                    ! LONG OR SHORT GROUP                      
          PTR=PTR-5+THISLEV          ! RESET POINTER                            
        ELSE IF (NEXTLEV.GT.0 .AND. MOD(LEND-NEXTLEV,6).EQ.0) THEN              
          ERROR=2                    ! LONG OR SHORT GROUP                      
          PTR=PTR+NEXTLEV            ! RESET POINTER                            
          L=L+1                      ! & NEXT LEVEL EXPECTED                    
        ENDIF                                                                   
      ELSE                                                                      
*                                                                               
* If the length is such that all the groups could have 5 figures, then          
* first look for the next level 2 or 3 groups on, depending on the wind         
* indicator: if it is found, then the tag at level L is suspect.                
*                                                                               
        IL='0'//LEVEL(L)(1:1)                                                   
        IF (IL.EQ.'00') IL='10'                                                 
*                                                                               
        IF ((IL.GE.WND .AND. LEVEL(L+1).EQ.STRING(PTR+18:PTR+19)) .OR.          
     -      (IL.LT.WND .AND. LEVEL(L+1).EQ.STRING(PTR+12:PTR+13)))THEN          
          ERROR=3                    ! PRESSURE TAG SUSPECT                     
          BADLEV=STRING(PTR:PTR+1)   ! keep it for error message                
          STRING(PTR:PTR+1)=LEVEL(L) ! RESET IT                                 
        ELSE                                                                    
*                                                                               
* If the next level is not where it is expected, either there is more           
* than one error or the wind indicator is wrong & we're one group out           
* - look for the current missing level (L) one group on or back.                
*                                                                               
          IL='0'//LEVEL(L-1)(1:1)                                               
          IF (IL.EQ.'00') IL='10'                                               
*                                                                               
          IF (STRING(PTR-6:PTR-5).EQ.LEVEL(L)) THEN                             
            IF (IL.GE.WND) THEN                                                 
              ERROR=4                                                           
              PTR=PTR-6                                                         
              IND=LEVEL(L-1)(1:1)                                               
*             IND=STRING(PTR-30:PTR-30)                                         
            ENDIF                                                               
          ELSE IF (STRING(PTR+6:PTR+7).EQ.LEVEL(L)) THEN                        
            IF (IL.LT.WND) THEN                                                 
              PTR=PTR+6                                                         
*                                                                               
* If the current level started one group back, i.e. for the previous            
* level three groups were expected but there were only two, then clearly        
* the last level with a wind was the level before that, so the indicator        
* can simply be reset as above.  If the current level starts one group          
* on, there must be more levels with winds than the indicator shows,            
* but we still have to work out how many.                                       
* Look for a level starting 2 groups or 3 from the end.  If a level             
* starts 3 groups from the end, then clearly all levels have winds;             
* if a level starts 2 groups from the end, then knowing how many groups         
* are left and (from the pressure figures) how many levels there are,           
* we can work out how many levels have winds.                                   
* If both groups could be starts of levels, i.e. the first two figures          
* could be a wind direction (3 groups back) or a temperature (2 groups          
* back) instead of a pressure, then there is too much uncertainty for           
* a correction to be risked.                                                    
*                                                                               
              DO I=L,MAXL                                                       
               IF (STRING(PEND-11:PEND-10).EQ.LEVEL(I)) LASTWO=I                
              ENDDO                                                             
*                                                                               
              DO I=L,MAXL                                                       
               IF (STRING(PEND-17:PEND-16).EQ.LEVEL(I)) LASTRI=I                
              ENDDO                                                             
*                                                                               
              NGROUPS=(PEND-PTR+1)/6                                            
              IF (LASTRI.GT.0 .AND. (LASTRI-L+1)*3.EQ.NGROUPS) THEN             
                IF (LASTWO.EQ.0) THEN                                           
                  ERROR=4                                                       
                  IND=STRING(PEND-17:PEND-17)                                   
                ENDIF                                                           
              ELSE IF (LASTWO.GT.0) THEN                                        
                NX=NGROUPS-(LASTWO-L+1)*2                                       
*                                                                               
* The pressure tag 2 groups from the end tells us how many levels to            
* expect in between.  NX is the number of groups left minus the number          
* of groups which these levels would need if none of them had a wind,           
* equal to the number of groups with winds (unless the tag is wrong!).          
* We can't reset ID if the last wind is for 250mb or 150mb (without             
* inserting ///// for the 200 or 100mb wind)                                    
*                                                                               
                IF (NX.GE.0 .AND. NX.LE.MAXL-L) THEN                            
                  IF (LEVEL(L+NX-1).NE.'25' .AND.                               
     &                LEVEL(L+NX-1).NE.'15') THEN                               
                    ERROR=4                                                     
                    IND=LEVEL(L+NX-1)(1:1)                                      
                  ENDIF                                                         
                ENDIF                                                           
              ENDIF                                                             
            ENDIF                                                               
          ENDIF                                                                 
        ENDIF                                                                   
      ENDIF                                                                     
***********************************************************************         
*                                                                               
* We now have a proposed correction - to pressure tag, wind indicator,          
* current pointer, next level to be looked for or some combination of           
* these.  So finally check levels L+1 to the end with these changes;            
* look for start of next level after 2 or 3 groups, depending on wind           
* indicator, and return with uncorrectable error if check fails.                
*                                                                               
***********************************************************************         
      IF (ERROR.GE.2) THEN                                                      
        I=1                                                                     
        J=PTR                                                                   
        BADSEQ=.FALSE.                                                          
*                                                                               
        IF (ERROR.EQ.4) THEN                                                    
          BADID=ID                   ! keep bad Id for error message            
          ID=IVALUE(IND)             ! TO RETURN RESET IND AS INTEGER           
          WND='0'//IND               ! RESET 2-FIGURE WND WITH NEW IND          
          IF (WND.EQ.'00') WND='10'  ! FOR USE IN COMPARISONS                   
        ENDIF                                                                   
*                                                                               
   23   IL='0'//LEVEL(L+I-1)(1:1)                                               
        IF (IL.EQ.'00') IL='10'                                                 
*                                                                               
        IF (IL.GE.WND) THEN                                                     
          IF (LEVEL(L+I).EQ.STRING(J+18:J+19)) THEN                             
            J=J+18                   ! NEXT LEVEL 3 GROUPS ON                   
          ELSE                                                                  
            BADSEQ=.TRUE.                                                       
          ENDIF                                                                 
        ELSE IF (IL.LT.WND) THEN                                                
          IF (LEVEL(L+I).EQ.STRING(J+12:J+13)) THEN                             
            J=J+12                   ! NEXT LEVEL 2 GROUPS ON                   
          ELSE                                                                  
            BADSEQ=.TRUE.                                                       
          ENDIF                                                                 
        ENDIF                                                                   
*                                                                               
        IF (BADSEQ) THEN                                                        
          ERROR=1                    ! NO CORRECTION ATTEMPTED                  
        ELSE IF (J.LT.PEND .AND. L+I.LT.MAXL) THEN                              
          I=I+1                      ! LOOK FOR NEXT LEVEL                      
          GO TO 23                                                              
        ENDIF                                                                   
      ENDIF                                                                     
*                                                                               
      IF (ERROR.EQ.2) PRINT *,'UATSTX: point past bad group to ',               
     &                        STRING(PTR:PTR+5)                                 
      IF (ERROR.EQ.3) PRINT *,'UATSTX: level indicator changed from ',          
     &                        BADLEV,' to ',LEVEL(L)                            
      IF (ERROR.EQ.4) PRINT *,'UATSTX: Id changed from ',BADID,' to ',ID        
      RETURN                                                                    
      END                                                                       
