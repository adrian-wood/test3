      SUBROUTINE SUBPER(ITIMST,TRANGE,INCR,ITIMND,ICMIN,RC)         !2.0

!-----------------------------------------------------------------------
!                                                                              
! PROGRAM       : SUBPER
!                                                                              
! PURPOSE       : COMPARE A GIVEN REPORT TIME (CENTURY-MINUTE)                 
!                 WITH A SEQUENCE OF INCREMENTED TIMES, OR PERIODS             
!                 GIVEN BY ADDING TRANGE TO THE START AND END TIMES.           
!
! CALLED BY     : TFMRET, SYNRET, UPRRET (ONLY IF INCREMENT REQUESTED)               
!                                                                              
! PARAMETERS    : (1) ITIMST INTEGER  START TIME (CENTURY-MINUTE) (I)           
!                 (2) TRANGE INTEGER  TIME RANGE IN MINUTES       (I)           
!                                     (TIME2-TIME1 FROM REQUEST)               
!                 (3) INCR   INTEGER  INCREMENT IN HOURS          (I)           
!                 (4) ITIMND INTEGER  END TIME (CENTURY MINUTE)   (I)           
!                 (5) ICMIN  INTEGER  REPORT CENTURY-MINUTE       (I)           
!                 (6) RC     INTEGER RETURN CODE. 0 IF ID MATCHED (O)
!                                                 1 IF NO MATCH
!                                                                              
!Y2K  26.06.1997  SUBPER IS YEAR 2000 COMPLIANT.                                
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:24:43$
! $Source: /home/us0400/mdb/op/lib/source/RCS/subper.F,v $
!                                                                             
! CHANGE RECORD :                                                              
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:24:43    Sheila Needham  
! $
! Revision 2.0  2001/01/08 11:59:16  usmdb
! Replaced ALTERNATE RETURN with a return code. Added
! copyright and modified header - S.Cox
!
! Revision 1.2  97/08/04  13:34:21  13:34:21  uspm (Pat McCormack)
! First revisioned version for MVS - with Y2K change
! 
! Revision 1.1  1997/02/12 09:10:12  uspm
! Initial revision
!
! INTRODUCED : 24/01/94                                                         
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

      INTEGER ITIMST,TRANGE,INCR,ITIMND,ICMIN                                   
      INTEGER IT,INC
      INTEGER RC                                                    !2.0
                                                                                
      CHARACTER*132      HEAD
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/subper.F,v $
     &'//' $Revision: 1$ $Date: 30/01/2006 20:24:43$ '                                                                                
                                                                              
      IT=ITIMST                 ! START TIME                                    
      INC=INCR*60               ! INCREMENT FROM HOURS TO MINUTES               
10    CONTINUE                                                                  

! ACCEPT IF UP TO TRANGE MINUTES AFTER START TIME (MAYBE INCREMENTED)           

      IF(ICMIN.GE.IT.AND.ICMIN.LE.IT+TRANGE)THEN                                
        RC=0                                                        !2.0
        RETURN                                                                  
      ELSE                                                                      

! SEE IF START TIME CAN BE INCREMENTED FURTHER IN PERIOD REQUESTED              

        IT=IT+INC               ! INCREMENT START TIME                          
        IF(IT.LE.ITIMND)GOTO 10 ! TRY AGAIN IF STILL LESS THAN END TIME         
      ENDIF                                                                     

      RC=1                                                          !2.0
      RETURN                                                        !2.0
      END                                                                       
