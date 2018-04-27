      SUBROUTINE HKSTART (CLEAR, STORTYPE, NSTORE, RECORD, NDAY0, NSEC0)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE   : HKSTART                                               
!                                                                      
! PURPOSE      : TO INITIALISE THE HOUSEKEEPING DATA SET AT THE START  
!                OF A RUN OF THE MET.D.B. STORAGE MONITOR JOB.         
!                                                                      
! DESCRIPTION  : THE HOUSEKEEPING DATA SET IS EITHER (1) INITIALISED   
!                FROM SCRATCH, PREVIOUS CONTENTS BEING LOST, OR (2)    
!                CHECKED TO SEE THAT THE DETAILS IT CONTAINS ABOUT THE 
!                NUMBER AND TYPES OF DATA TO BE STORED ARE STILL       
!                APPLICABLE.                                           
!                                                                      
!                IN CASE (2), THE JOB ABENDS WITH A MESSAGE IF THERE   
!                ARE ANY DIFFERENCES. OTHERWISE, THE HOUSEKEEPING      
!                STATUS RECORD IS UPDATED INCLUDING THE FLAG TO SAY    
!                THAT THE MONITOR JOB IS NOW RUNNING.                  
!                                                                      
! USAGE        : CALL HKSTART                                          
!                     (RECORD, CLEAR, STORTYPE, NSTORE, NDAY0, NSEC0)  
!                                                                      
! PARAMETERS   : CLEAR    I  SET .TRUE. TO RECONSTRUCT ENTIRE HOUSE-   
!                            KEEPING DATA SET FROM SCRATCH, OR .FALSE. 
!                            TO RESTART WHERE LEFT OFF LAST TIME.      
!                                                                      
!                STORTYPE I  (C*8 ARRAY) DATA TYPE CODES FOR STORAGE   
!                            DATA SETS REQUIRED.                       
!                                                                      
!                NSTORE   I  NUMBER OF ENTRIES IN "STORTYPE" ARRAY.    
!                                                                      
!                RECORD   O  CHARACTER ARRAY TO HOLD THE CONTENTS OF   
!                            HOUSEKEEPING RECORDS AS FAR AS THE END    
!                            OF THE DATA SET STATUS RECORDS.           
!                                                                      
!                NDAY0    O) DATE & TIME (CENTURY DAY & SECS FROM 00Z) 
!                NSEC0    O) WHEN H.K. DATA SET WAS LAST INITIALISED.  
!                                                                      
! CALLED BY    : MET.D.B. STORAGE MONITOR JOB.                         
!                                                                      
! CALLS        : DATE31, DATIM, SYSABN, TELLOPS.                       
!                                                                      
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN   
!                ON UNIT 1.                                            
!
! REVISION INFO :
!                                                                      
! $Revision: 1$
! $Date: 30/01/2006 20:22:46$
! $Source: /home/us0400/mdb/op/lib/source/RCS/hkstart.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:46    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:51  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/06/08  15:39:34  15:39:34  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, AUGUST 1999.              
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
!
!                                                            Parameters
!
      INTEGER    MAXDATA         ! MAX. NO. OF DATA SET STATUS RECORDS
      INTEGER    MAXJOBS         ! MAXIMUM NUMBER OF JOB STATUS RECORDS
      INTEGER    MAXSTOR         ! MAXIMUM NUMBER OF STORAGE TYPES
      INTEGER    NUMREC          ! SIZE OF "RECORD" ARRAY
!
      PARAMETER (MAXDATA=160, MAXJOBS=50, MAXSTOR=50)
      PARAMETER (NUMREC=2+MAXDATA)
!                                                             Variables
      INTEGER I, J               ! VARIABLES FOR LOCAL USE
      INTEGER ISTORE             ! NUMBER OF STORAGE TYPES IN H.K.
      INTEGER LATEST             ! NUMBER OF LAST DATA SET READ
      INTEGER LENREC             ! RECORD LENGTH OF H.K. DATA SET
      INTEGER NACCESS            ! ACCESS COUNTER IN STATUS RECORD
      INTEGER NCLEAR             ! ALL DATA SETS CLEARED UP TO HERE
      INTEGER NDAY, NDAY0        ! CURRENT AND START CENTURY DAYS
      INTEGER NOW(8)             ! CURRENT DATE & TIME (FROM 'DATIM')
      INTEGER NSEC, NSEC0        ! CURRENT AND START SECONDS FROM 00Z
      INTEGER NSECS              ! NO. OF SECONDS FROM START OF STORAGE
      INTEGER NSTORE             ! NUMBER OF STORAGE TYPES REQUIRED
!
      LOGICAL CHANGED            ! FLAG SET IF ENVIRONMENT HAS CHANGED
      LOGICAL CLEAR              ! FLAG FOR TOTAL CLEAROUT OF H.K.
!
      CHARACTER ZERO, ONE          ! ONE-BYTE CHARACTERS '00' AND '01'
      CHARACTER*8 STORTYPE(NSTORE) ! ARRAY OF STORAGE TYPES REQUIRED
      CHARACTER*(*) RECORD(NUMREC) ! STARTING INFORMATION ON DATA SETS
      CHARACTER HEAD*132           ! REVISION DETAILS
!
!                                                  Revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/hkstart.F,v $
     &'//'$Date: 30/01/2006 20:22:46$ $Revision: 1$'
!                                                       Initialisations
      ZERO = CHAR(0)
      ONE  = CHAR(1)
!
!-----------------------------------------------------------------------
!  Get the current date and time
!-----------------------------------------------------------------------
!
      CALL DATIM (NOW)
      CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
      NSEC = (NOW(5)*60 + NOW(4))*60 + NOW(3)
!
!=======================================================================
!  1. CLEAR OUT WHOLE HOUSEKEEPING DATA SET IF REQUIRED
!     -------------------------------------------------
!     This is normally done if:
!       (1) a clearout was specifically requested ("CLEAR" set .TRUE.
!           in user's namelist input), or
!       (2) all old data has been processed by storage jobs ("CLEAR"
!           set by default in calling program).
!=======================================================================
!
      IF (CLEAR) THEN
!
!-----------------------------------------------------------------------
!  Fill RECORD array with binary zeroes and write to data set
!-----------------------------------------------------------------------
!                                         Set record 1 to binary zeroes
         LENREC = LEN(RECORD(1))
         DO J=1,LENREC
            RECORD(1)(J:J) = ZERO
         END DO ! J
!                                Fill whole of RECORD array with zeroes
         DO J=2,NUMREC
            RECORD(J) = RECORD(1)
         END DO ! J
!                           Set all h.k. data set records to zeroes (to
!                               delete pre-existing status information)
         DO J=1,NUMREC+MAXJOBS
            WRITE (1,REC=J) RECORD(1)
         END DO ! J
!                                             Start time = current time
         NDAY0 = NDAY
         NSEC0 = NSEC
!
!-----------------------------------------------------------------------
!  Create new record 1 with appropriate data and write out
!-----------------------------------------------------------------------
!                                                   Initialise record 1
         RECORD(1) = ' '
!                                                     Set integer words
         I = 0
         WRITE (RECORD(1)(1:44),'(12A4)') (NOW(J),J=8,3,-1), MAXDATA,
     &          MAXJOBS, MAXSTOR, I, NSTORE
!                                                Set storage data types
         I = 45
         DO J=1,NSTORE
            RECORD(1)(I:I+7) = STORTYPE(J)
            I = I + 8
         END DO ! J
!                                                    Write new record 1
         WRITE (1,REC=1) RECORD(1)
!
!-----------------------------------------------------------------------
!  Initialise data for record 2 (will get written out at end)
!-----------------------------------------------------------------------
!
         NSECS  = 0
         NACCESS = 0
         LATEST = 0
         NCLEAR = 0
!
!=======================================================================
!  2. IF PICKING UP WHERE LEFT OFF, READ HEADER AND DATA SET RECORDS
!     AND CHECK THAT STORAGE DETAILS HAVEN'T CHANGED
!=======================================================================
!
      ELSE
!                                                    Read records 1 & 2
         READ (1,REC=1) RECORD(1)
         READ (1,REC=2) RECORD(2)
!
!-----------------------------------------------------------------------
!  See if storage details have changed from last run
!-----------------------------------------------------------------------
!                                      Read old number of storage types
         CHANGED = .FALSE.
         READ (RECORD(1),'(T41,A4)') ISTORE
!                                                   Check with required
!                                                   number for this job
         IF (ISTORE.NE.NSTORE) THEN
            CHANGED = .TRUE.
         ELSE
!                          Check old list of storage types and set flag
!                           if different from requirements for this job
            I = 45
            DO J=1,NSTORE
               IF (RECORD(1)(I:I+7).NE.STORTYPE(J)) CHANGED = .TRUE.
               I = I + 8
            END DO ! J
         END IF
!
!-----------------------------------------------------------------------
!  If storage details have changed, print out a message and terminate
!-----------------------------------------------------------------------
!
         IF (CHANGED) THEN
            WRITE (6,'(/T5,A)')
     &        'JOB TERMINATING BECAUSE STORAGE ENVIRONMENT HAS CHANGED'
            WRITE (6,'(/T5,A,I4,A/(T8,8A9))')
     &        'OLD STORAGE LIST -', ISTORE, ' STORAGE TYPES',
     &        (RECORD(1)(8*I+37:8*I+44), I=1,ISTORE)
            WRITE (6,'( T5,A,I4,A/(T8,8A9))')
     &        'NEW STORAGE LIST -', NSTORE, ' STORAGE TYPES',
     &        (STORTYPE(I), I=1,NSTORE)
            WRITE (6,'(/T5,A//T5,A/)')
     &        'TRY RE-RUNNING THE JOB WITH "CLEAR =.TRUE."',
     &        'JOB WILL NOW TERMINATE WITH USER RETURN CODE 701'
!
!                                    SEND MESSAGE TO OPERATOR AND ABEND
            CALL TELLOPS
     &         ('MDB(E):  MDBFTP - STORAGE ENVIRONMENT HAS CHANGED.')
            CALL SYSABN (701)
         END IF
!
!-----------------------------------------------------------------------
!  Read data set status records into storage
!-----------------------------------------------------------------------
!
         READ (RECORD(1),'(T25,A4)') I ! No. of data set status records
         DO J=3,I+2
            READ (1,REC=J) RECORD(J)
         END DO ! J
!
!-----------------------------------------------------------------------
!  Get start day and second numbers (returned to calling program)
!-----------------------------------------------------------------------
!
         READ (RECORD(1),'(6A4)') (NOW(J),J=8,3,-1)
         CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY0)
         NSEC0 = (NOW(5)*60 + NOW(4))*60 + NOW(3)
         NSECS = 86400*(NDAY-NDAY0) + (NSEC-NSEC0)
!
!-----------------------------------------------------------------------
!  Read data from record 2 (updated version is written out at end)
!-----------------------------------------------------------------------
!
         READ (RECORD(2)(9:20),'(3A4)') NACCESS, LATEST, NCLEAR
      END IF
!
!=======================================================================
!  3. COMPLETE HOUSEKEEPING STATUS RECORD AND WRITE TO RECORD 2.
!=======================================================================
!
!                   Increment access number and set 'job running' flag.
!                            (The latter allows storage jobs to start.)
      NACCESS = NACCESS + 1
      RECORD(2)(1:4) = ONE//ZERO//ZERO//ZERO
!
      WRITE (RECORD(2)(5:20),'(4A4)') NSECS, NACCESS, LATEST, NCLEAR
      WRITE (1,REC=2) RECORD(2)
!                                             RETURN TO CALLING PROGRAM
      RETURN
      END
