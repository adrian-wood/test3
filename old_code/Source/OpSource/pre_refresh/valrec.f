      SUBROUTINE VALREC(IRECV,IFAIL,CERR)                                       

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : VALREC                                                       
!                                                                              
! PURPOSE       : TO VALIDATE A RANGES OF RECEIPT TIMES.                       
!                                                                              
! CALLED BY     : VALREQ IN MDBRT                                              
!                                                                              
! CALLS         : DT2HRS                                                       
!                                                                              
! PARAMETERS    : (1) IRECV(10) YYYY,MM,DD,HH,MM,YYYY,MM,DD,HH,MM              
!               : (2) IFAIL     8 FOR ERROR, 4 FOR WARNING                     
!               : (3) CERR      ERROR MESSAGE                                  
!                                                                              
!Y2K  26.06.1997  VALREC IS YEAR 2000 COMPLIANT.                                
!Y2K                     ROUTINE CONTAINS DATE MANAGEMENT.                      
!                                                                              
! REVISION INFO :                                                              
!
! $Revision: 1$
! $Date: 30/01/2006 20:25:59$
! $Source: /home/us0400/mdb/op/lib/source/RCS/valrec.F,v $
!
! CHANGE RECORD :                                                              
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:25:59    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:27  usmdb
! Removal of argument to DATCHK as no longer needed.
! Tidy of header, comments. Addition of copyright - S.Cox
!
! Revision 1.2  97/08/04  13:43:19  13:43:19  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K changes
! 
! Revision 1.1  1997/02/12 09:26:44  uspm
! Initial revision
!
! 05/03/93 COMPLETE REWRITE WITH A DIFFERENT ARGUMENT LIST
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
      
      INTEGER IRECV(10),IFAIL                                                   
      INTEGER IST, IND
      INTEGER DT2HRS
      CHARACTER*(*)  CERR                                                        
                                                                              
      CHARACTER*30   CMSG                                                         
      CHARACTER*132  HEAD
                                                                              
!-----------------------------------------------------------------------
! Declare string containing keywords used by HP utility RCS, ident to 
! allow automatic updating of files on checkin with change details and 
! identification of version of version used to make obj, exe
!------------------------------------------------------------------------
                                                                                
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/valrec.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:25:59$ '

      IST=0                                                                     
      IND=0                                                                     

!-  RECEIVED BEFORE FOUND                                                       

      IF(IRECV(1).NE.0)THEN                                                     

!-  CHECK START TIME IS VALID                                                   

        CALL DATCHK(IRECV(2),IRECV(3),(IRECV(4)*100+IRECV(5)),      !2.0
     &      IFAIL,CMSG)                                                         
        IF(IFAIL.EQ.8)THEN                                                      
          CERR='INVALID RECEIPT START'//CMSG                                    
          GOTO 999                                                              
        ENDIF                                                                   
        IST=DT2HRS(IRECV(1),IRECV(2),IRECV(3),IRECV(4))                         
      ENDIF                                                                     

!-  RECEIVED AFTER FOUND                                                        

      IF(IRECV(6).NE.0)THEN                                                     

!-  CHECK START TIME IS VALID                                                   

        CALL DATCHK(IRECV(7),IRECV(8),(IRECV(9)*100+IRECV(10)),     !2.0
     &      IFAIL,CMSG)                                                         
        IF(IFAIL.EQ.8)THEN                                                      
          CERR='INVALID RECEIPT END'//CMSG                                      
          GOTO 999                                                              
        ENDIF                                                                   
        IND=DT2HRS(IRECV(6),IRECV(7),IRECV(8),IRECV(9))                         
      ENDIF                                                                     

!-  CHECK START IS BEFORE END                                                   

      IF(IST.NE.0.AND.IND.NE.0)THEN                                             
        IF(IST.GT.IND)THEN                                                      
          CERR='RECEIPT TIMES WRONG WAY ROUND'                                  
          IFAIL=8                                                               
          GOTO 999                                                              
        ENDIF                                                                   
      ENDIF                                                                     

999   RETURN                                                                    
      END                                                                       
