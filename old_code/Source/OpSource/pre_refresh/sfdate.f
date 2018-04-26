      SUBROUTINE SFDATE(HOUR,YYGGGG,DATIME)                             

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : SFDATE                                               
!                                                                      
! PURPOSE       : To work out date from hour in ob, day/hour in        
!                 bulletin heading and current time.                   
!                                                                      
! CALLED BY     : SFLOC                                                
!                                                                      
! CALLS         : DATIM, DATE13,DATE31                                 
!                                                                      
! PARAMETERS    : (1) hour from ob                                     
!                 (2) YYGGGG (day/hour/min) from bulletin heading      
!                 (3) year/month/day/hour/min array                    
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:12$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sfdate.F,v $
!                                                                      
! CHANGE RECORD :
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:12    Sheila Needham  
! $
! Revision 2.0  2001/07/03 10:43:52  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  98/05/14  16:09:04  16:09:04  usmdb (Generic MetDB accou
! Initial revision
!
! Made March 98 from code in TAFIND    
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

      INTEGER HOUR                                                      
      CHARACTER*6 YYGGGG                                                
      CHARACTER HEAD*132
      INTEGER DATIME(5),NOW(9)                                          
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sfdate.F,v $
     &'//'$ $Date: 30/01/2006 20:24:12$ $Revision: 1$' 
*********************************************************************** 
*                                                                     * 
* TAKE CURRENT YEAR & MONTH, DAY FROM BULLETIN & TIME FROM REPORT     * 
* UNLESS (1) 0Z DATA FOR 1ST BEFORE 0Z (MAY BE NEXT MONTH)            * 
*        (2) BULLETIN DAY > CURRENT DAY (0Z OB OR LAST MONTH'S DATA)  * 
*        (3) REPORT HOUR > BULLETIN HOUR (REPORT FOR DAY BEFORE)      * 
* (ASSUME EARLY 0Z DATA IF DATA TIME <0010 & CURRENT TIME >2330)      * 
*                                                                     * 
*********************************************************************** 
      CALL DATIM(NOW)                             ! CURRENT DATE        
      DATIME(1)=NOW(8)                            ! YEAR                
      DATIME(2)=NOW(7)                            ! MONTH               
*                                                                       
      READ (YYGGGG(1:2),'(I2)') DATIME(3)         ! DAY                 
      READ (YYGGGG(3:4),'(I2)') DATIME(4)         ! HOUR                
      READ (YYGGGG(5:6),'(I2)') DATIME(5)         ! MINUTE              
*                                                                       
      IF (YYGGGG(1:4).EQ.'0100' .AND. NOW(6).GE.28) THEN                
        CALL DATE31(NOW(6),NOW(7),NOW(8),NCENDY)  ! CURRENT CENTURY-DAY 
        CALL DATE13(NCENDY+1,NDAY,NMONTH,NYEAR)   ! TOMORROW'S DATE     
        IF (NDAY.EQ.1) THEN                       ! IF IT'S THE FIRST,  
          DATIME(2)=NMONTH                        ! NEXT MONTH'S DATA   
          DATIME(1)=NYEAR                                               
        ENDIF                                                           
      ELSE IF (DATIME(3).GT.NOW(6)) THEN          ! UNLESS ALMOST 0Z    
        IF (.NOT.(DATIME(3).EQ.NOW(6)+1 .AND. YYGGGG(3:4).LE.'0010'     
     -             .AND. NOW(5).EQ.23 .AND. NOW(4).GE.30)) THEN         
          DATIME(2)=DATIME(2)-1                   ! LAST MONTH'S DATA   
          IF (DATIME(2).EQ.0) THEN                                      
            DATIME(2)=12                          ! DECEMBER            
            DATIME(1)=DATIME(1)-1                 ! LAST YEAR           
          ENDIF                                                         
        ENDIF                                                           
      ELSE IF (HOUR.GT.DATIME(4)) THEN            ! YESTERDAY'S DATA    
        CALL DATE31(DATIME(3),NOW(7),NOW(8),NCENDY)                     
        CALL DATE13(NCENDY-1,DATIME(3),DATIME(2),DATIME(1))             
      ENDIF                                                             
*                                                                       
      RETURN                                                            
      END                                                               
