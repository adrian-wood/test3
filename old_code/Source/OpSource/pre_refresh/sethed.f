      SUBROUTINE SETHED(HEADER,RLAT,RLON,IH,IM,ID,IMON,IY,                      
     &                  ITH,ITM,NAMD,NCOR,CCCC)                                            

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : SETHED IN TFMTRT                                             
!                                                                              
! PURPOSE       : TO SET UP HEADER FOR A REPORT                                
!                                                                              
! DESCRIPTION   : FORMAT                                                        
!                                                                              
!     DATA HEADER/'0000Z DD/MM/YY LA.TTN LON.TTW CCCC TORM A C '/              
!                                                                              
! CALLED BY     : TFMTRT IN TFMTRT                                             
!                                                                              
! CALLS         : NIL                                                          
!                                                                              
! PARAMETERS    : (1)HEADER     44 BYTE OUTPUT HEADER                          
!                 (2)RLAT       REAL LATITUDE                                  
!                 (3)RLON       REAL LONGITUDE                                 
!                 (4)IH         DATE                                           
!                 (5)IM           TIME                                         
!                 (6)ID             OF                                         
!                 (7)IMON             OBSERVATION                              
!                 (8)IY                                                        
!                 (9)ITH        HOUR OF RECEIPT                                
!                (10)ITMMINUTE OF RECEIPT                                      
!                (11)NAMD       AMEND NUMBER                                   
!                (12)NCOR       COR NUMBER                                     
!                (13)CCCC       COLLECTING CENTRE                              
!                                                                              
!Y2K  01.07.1997  SETHED is Year 2000 compliant.                                
!Y2K                     Routine contains date management.                      
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:11$
! $Source: /home/us0400/mdb/op/lib/source/RCS/sethed.F,v $
!                                                                              
! CHANGE RECORD :                                                              
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:11    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:09  usmdb
! Added copyright, modified header - S.Cox
!
! Revision 1.2  97/08/04  13:32:46  13:32:46  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/12 08:59:30  uspm
! Initial revision
!
! 04/08/97  B    Amend for Year 2000 - use MOD to reduce 4-fig               
!                year to 2-fig - Jim Arnott                                  
!                                                                            
! 15/08/94  A    WHERE DAY, MONTH, YEAR ARE NOT RETRIEVEABLE FROM            
!                THE INDEX OR TRAILER BLANKS ARE INSERTED INTO               
!                THE HEADER                                                  
!                                                                              
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database 
! Team at the above address.
!-----------------------------------------------------------------------        

      IMPLICIT NONE

      CHARACTER*(*) HEADER                                                      
      CHARACTER*4 CCCC                                                          
      REAL RLAT,RLON,RMDI                                                           
      INTEGER IH,IM,ID,IMON,IY,ITH,ITM,NAMD,NCOR                                
                                                                               
      DATA RMDI/-9999999./                                                      
                                                                                
      CHARACTER*132      HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/sethed.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:11$ '
                                                                                                                                                              
      IF(RLAT.EQ.RMDI)THEN                                                      
        HEADER(16:21)=' '                                                       
        HEADER(23:29)=' '                                                       
      ELSE                                                                      
        IF(RLAT.GT.0)THEN                                                       
          HEADER(21:21)='N'                                                     
        ELSE                                                                    
          HEADER(21:21)='S'                                                     
        ENDIF                                                                   
        WRITE(HEADER(16:20),'(F5.2)')ABS(RLAT)                                  
        IF(RLON.GT.0)THEN                                                       
          HEADER(29:29)='E'                                                     
        ELSE                                                                    
          HEADER(29:29)='W'                                                     
        ENDIF                                                                   
        WRITE(HEADER(23:28),'(F6.2)')ABS(RLON)                                  
      ENDIF                                                                     
      IF ((ID.EQ.RMDI).OR.(IMON.EQ.RMDI).OR.(IY.EQ.RMDI)) THEN   !A             
        WRITE(HEADER(1:6),2) IH,IM                               !A             
        HEADER(7:14) = '        '                                !A             
      ELSE                                                                      
        WRITE(HEADER(1:14),1)IH,IM,ID,IMON,MOD(IY,100)           !B             
      ENDIF                                                                     
1     FORMAT(2I2.2,'Z ',2(I2.2,'/'),I2.2)                                       
2     FORMAT(2I2.2,'Z ')                                                        
      WRITE(HEADER(36:39),'(2I2.2)')ITH,ITM                                     
      HEADER(31:34)=CCCC                                                        
      WRITE(HEADER(41:43),'(I1,1X,I1)')NAMD,NCOR                                

      RETURN                                                                    
      END                                                                       
