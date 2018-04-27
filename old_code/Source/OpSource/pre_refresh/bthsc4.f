      SUBROUTINE BTHSC4(REPORT,REPLEN,EXPARR,IDFLG,ID)          !2.0!1.4   

!-----------------------------------------------------------------------
!                                                                      
! PROGRAM       : BTHSC4                                                
!                                                                      
! PURPOSE       : TO EXPAND SECTION 4 OF BATHY (IDENTIFIER)             
!                                                                      
! CALLED BY     : BTHEXP                                                
!                                                                      
! CALLS         : IVALUE CONVERTS FROM CHAR TO NUMERIC TYPE           
!                                                                      
! PARAMETERS    : REPORT   CHARACTER STRING OF REPORT (I)               
!                 REPLEN   LENGTH OF REPORT           (I)               
!                 EXPARR   EXPANSION ARRAY            (O)               
!                 IDFLG    FLAG FOR SHIP/BUOY ID      (O)               
!                 ID       SHIPS CALL SIGN            (O)               
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:21:09$
! $Source: /home/us0400/mdb/op/lib/source/RCS/bthsc4.F,v $
!                                                                      
! CHANGE RECORD :                                                     
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:21:09    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:37  usmdb
! Removed unused dummy arguments IERR and REPEND. Separated
! variable declaration and initialisation. Added copyright
! and modified header - S.Cox
!
! Revision 1.4  99/02/11  12:12:03  12:12:03  usmdb
! 15th February 1999 v(G)= 8 ev(G)= 1 John Norton
! Redo handling of finding callsigns to reduce complex
! structure and allow callsigns with numerics at start.
! 
! Revision 1.3  97/08/28  10:12:58  10:12:58  usjl (Jon Lewthwaite)
! Aug 9 Avoid INDEX operations on strings of length 0                 !a
!
! Revision 1.2  1997/07/31 09:14:08  uspm
! First revision for MVS
!
! Revision 1.1  1997/07/04 11:10:30  uspm
! Initial revision
!
! APR 95 - IMPROVE CODE TO FIND CHARACTER IDENTIFIER, AS GROUP AFTER
!          LAST 5-FIGURE GROUP
!
! 08/08/94 CORRECTION TO POINTER UPDATE DURING SEARCH FOR BUOY ID
!
! INTRODUCED : 11/07/94                                              
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
                                                                        
      CHARACTER*4096 REPORT                                             
      CHARACTER*9    ID                                                 
      CHARACTER*132  HEAD
      REAL           EXPARR(0:300)                                      
      REAL           MISING                                         !2.0
      REAL           DISP                                               
      INTEGER        LENGID        !Length of id                    !1.4
      INTEGER        REPLEN                                             
      INTEGER        BUOYID                                             
      INTEGER        INUMB         ! number of first non figure     !1.4
      INTEGER        IVALUE                                             
      LOGICAL        ALLFIG        ! True if string all figures     !1.4
      LOGICAL        IDFLG                                              
      LOGICAL        ESPACE        ! True if a character of id      !1.4
                                   !      found
      LOGICAL        OLFCR         ! True if next char is Line feed !1.4
                                   !      or carriage return
      LOGICAL        OSPACE        ! True if next char is space     !1.4

      DATA           MISING/-9999999./                              !2.0

*************************************************************           
*                                                                       
*     INITIALISE VARIABLES                                              
*     IDFLG - FLAG TO INDICATE SHIPS CALL SIGN IS REPORTED              
*     ID - SHIPS CALL SIGN                                              
*     DISP - DISPLACEMENT OF THE SHIPS CALL SIGN IN THE                 
*            STRING THAT WILL BE SENT TO ENCODING ROUTINE.              
*            BECAUSE ID IS THE ONLY STRING IN THE REPORT,               
*            IF THE CALL SIGN IS REPORTED DISP WILL ALWAYS              
*            TAKE A VALUE OF 1.                                         
*     BUOYID - BUOY IDENTIFIER (5 DIGIT GROUP)                          
*     ID AND BUOYID ARE MUTUALLY EXCLUSIVE                              
*                                                                       
*************************************************************           
                                                                        
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/bthsc4.F,v $
     &'//'$ $Date: 30/01/2006 20:21:09$ $Revision: 1$'

      IDFLG = .FALSE.                                                   
      ID = ' '                                                          
      DISP = 1                                                     !1.4 
      BUOYID = MISING                                                   
                                                                   !1.4
* STARTING AT THE END OF THE REPORT,                               !1.4 
*  ignore any equals signs and spaces at the end of the report     !1.4 
*  then loop back through the report until a space is found.       !1.4 
* The ID of the report is taken as the characters after the space  !1.4
* unless                                                           !1.4
*    The length of the callsign found is 5 characters and          !1.4
* preceeded by ' 99999' and contains five figures then it is       !1.4
* taken as a buoy id.                                              !1.4
*    If it is 5 numbers it is taken as part of the report          !1.4
* and the ID is set to spaces and buoy id taken as missing data.   !1.4
*    The length is more than 9 characters then it is assumed to be !1.4
* corrupted and the ID is set to spaces and buoy id taken as       !1.4
* missing data.                                                    !1.4
                                                                   !1.4
      ESPACE=.TRUE.                                                !1.4
      LENGID=-1                                                    !1.4
      OSPACE=.FALSE.                                               !1.4
                                                                   !1.4
      DO WHILE (.NOT.OSPACE.AND.LENGID.LT.REPLEN)                  !1.4
        LENGID=LENGID+1                                            !1.4
* test for equals signs and spaces at end of report                !1.4
        IF(REPORT(REPLEN-LENGID:REPLEN-LENGID).EQ.'=' .OR.         !1.4
     &       (ESPACE.AND.                                          !1.4
     &        (REPORT(REPLEN-LENGID:REPLEN-LENGID).EQ.' ')))THEN   !1.4
          LENGID=LENGID-1                                          !1.4
          REPLEN=REPLEN-1                                          !1.4
* Test for first space not at end of report this should delimit    !1.4
* start of callsign.                                               !1.4
        ELSEIF(REPORT(REPLEN-LENGID:REPLEN-LENGID).EQ.' ')THEN     !1.4
          ID(1:LENGID)=REPORT((REPLEN-LENGID+1):REPLEN)            !1.4
          IDFLG=.TRUE.                                             !1.4
          IF(LENGID.GT.9)THEN                                      !1.4
* corrupt callsign.                                                !1.4
            ID='         '                                         !1.4
          ELSEIF(LENGID.EQ.5)THEN                                  !1.4
            CALL NCHTST(1,LENGID,ALLFIG,INUMB,OSPACE,OLFCR,        !1.4
     &                      REPORT(REPLEN-LENGID+1:REPLEN))        !1.4
            IF(ALLFIG)THEN                                         !1.4
* part of report                                                   !1.4
              ID=' '                                               !1.4
              IF(REPORT(REPLEN-LENGID-6:REPLEN-LENGID-1).EQ.' 99999')
     &              THEN                                           !1.4
                BUOYID=IVALUE(REPORT((REPLEN-LENGID+1):REPLEN))    !1.4
                IDFLG=.FALSE.                                      !1.4
              ENDIF                                                !1.4
            ENDIF                                                  !1.4
          ENDIF                                                    !1.4
           OSPACE=.TRUE.                                           !1.4
* Reset espace when callsign end found at end of report            !1.4
        ELSE                                                       !1.4
          ESPACE=.FALSE.                                           !1.4
        ENDIF                                                      !1.4
      ENDDO                                                        !1.4
                                                                   !1.4

*************************************************************           
*                                                                       
*     ASSIGN VARIABLES TO ARRAY                                         
*                                                                       
*************************************************************           
                                                                        
      EXPARR(2) = DISP                                                  
      EXPARR(4) = BUOYID                                                
                                                                        
      RETURN                                                            
      END                                                               
