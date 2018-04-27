      SUBROUTINE FINDREC (RECORD, NEXTDS)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE    : FINDREC                                               
!                                                                      
! PURPOSE       : TO FIND A FREE SLOT IN A DATA SET STATUS RECORD FOR   
!                 STORAGE OF DETAILS OF THE NEXT TROPICS DATA SET.      
!                                                                      
! DESCRIPTION   : "FINDREC" SEARCHES THE DATA SET STATUS RECORDS IN THE 
!                 HOUSEKEEPING DATA SET UNTIL IT FINDS A SLOT WHICH CAN 
!                 BE USED TO HOLD DETAILS OF THE NEXT DATA SET RECEIVED 
!                 FROM TROPICS.                                         
!                                                                      
!                 SLOTS ARE LOOKED AT STARTING FROM IMMEDIATELY AFTER   
!                 THE PREVIOUSLY USED ONE: IF THIS ONE IS NOT FREE,     
!                 "HKUPDATE" IS CALLED TO UPDATE DATA SET INFORMATION.  
!                                                                      
!                 THE NUMBER OF THE FIRST FREE SLOT FOUND IS RETURNED   
!                 IN "NEXTDS", OR "-1" IF THERE ARE NO FREE SLOTS       
!                 AVAILABLE.                                            
!                                                                      
! USAGE         : CALL FINDREC (RECORD, NEXTDS)                         
!                                                                      
! PARAMETERS    : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF  
!                              HOUSEKEEPING RECORDS AS FAR AS THE END   
!                              OF THE DATA SET STATUS RECORDS.          
!                                                                       
!                 NEXTDS   O   NUMBER OF FREE SLOT FOUND (OR -1 IF      
!                              THERE ARE NONE AVAILABLE).               
!                                                                      
! CALLED BY     : MET.D.B. STORAGE MONITOR JOB.                         
!                                                                      
! CALLS         : HKUPDATE                                              
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:29$
! $Source: /home/us0400/mdb/op/lib/source/RCS/findrec.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:29    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:38  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/06/08  15:38:47  15:38:47  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, JULY 1999.                
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

!                                                             Variables
      INTEGER JDATA            ! LOOP VARIABLE FOR DATA SETS
      INTEGER LASTREC          ! LAST H.K. DATA SET RECORD READ
      INTEGER LATEST           ! NUMBER OF LATEST DATA SET READ
      INTEGER NACCESS          ! CURRENT H.K. ACCESS NUMBER
      INTEGER NCLEAR           ! ALL DATA SETS CLEARED UP TO HERE
      INTEGER NDATA            ! LOCATION OF DATA SET IN STATUS RECORD
      INTEGER NDREC            ! NO. OF CURRENT DATA SET STATUS RECORD
      INTEGER NDRECS           ! TOTAL NO. OF DATA SET STATUS RECORDS
      INTEGER NEXTDS           ! NUMBER OF SLOT FOR NEXT DATA SET
      INTEGER NUMDS            ! TOTAL NO. OF DATA SET SLOTS IN H.K.
!
      LOGICAL UPDATED          ! .TRUE. IF HKUPDATE HAS BEEN CALLED
      LOGICAL FIRST            ! .TRUE. IF FIRST CALL TO FINDREC
!
      CHARACTER ZERO           ! ALL ZEROES (HEX "00")
      CHARACTER FREEFLAG(10)   ! FLAGS FROM DATA SET STATUS RECORD
      CHARACTER*(*) RECORD(*)  ! HOUSEKEEPING DATA SET RECORDS
      CHARACTER*132 HEAD       ! FOR REVISION DETAILS
!                                                                 Saves
      SAVE FIRST, ZERO, NUMDS, LASTREC, FREEFLAG
!                                                                  Data
      DATA FIRST/.TRUE./, LASTREC/0/
!
!-----------------------------------------------------------------------
!  1. INITIALISATIONS (FIRST CALL ONLY)
!-----------------------------------------------------------------------
!
      IF (FIRST) THEN
!                                                  Revision information
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/findrec.F,v $
     &   '//'$Date: 30/01/2006 20:22:29$ $Revision: 1$'
!                                                       Other variables
         ZERO = CHAR(0)
         READ (RECORD(1),'(24X,A4)') NDRECS
         NUMDS = 10*NDRECS
         FIRST = .FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  2. READ HOUSEKEEPING STATUS RECORD
!-----------------------------------------------------------------------
!
      READ (RECORD(2),'(T9,3A4)') NACCESS, LATEST, NCLEAR
!
!-----------------------------------------------------------------------
!  3. LOOK FOR FREE DATA SET NUMBER
!-----------------------------------------------------------------------
!
      UPDATED = .FALSE.
!                                              Loop over data set slots
      DO JDATA=1,NUMDS
         NEXTDS = LATEST + JDATA
         NDREC = MOD(NEXTDS,NUMDS)/10 + 3
         NDATA = MOD(NEXTDS,10) + 1
!                                   Read data set flag if in new record
         IF (NDREC.NE.LASTREC) THEN
            READ (RECORD(NDREC),'(10A1)') FREEFLAG
            LASTREC = NDREC
         END IF
!                If not free and HKUPDATE hasn't been called, do it now
!
         IF (.NOT.UPDATED .AND. FREEFLAG(NDATA).NE.ZERO) THEN
            CALL HKUPDATE (RECORD)
            READ (RECORD(NDREC),'(10A1)') FREEFLAG
            UPDATED = .TRUE.
         END IF
!                     Jump out of loop if a free data set slot is found
!
         IF (FREEFLAG(NDATA).EQ.ZERO) RETURN
      END DO ! JDATA
!
!-----------------------------------------------------------------------
!  4. RETURN WITH "NEXTDS" SET TO -1 IF THERE ARE NO FREE SLOTS
!-----------------------------------------------------------------------
!
      NEXTDS = -1
      RETURN
      END
