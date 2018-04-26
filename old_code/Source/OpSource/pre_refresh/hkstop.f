      SUBROUTINE HKSTOP (RECORD, KODE)

      IMPLICIT NONE

!-----------------------------------------------------------------------
!                                                                      
! SUBROUTINE   : HKSTOP                                                
!                                                                      
! PURPOSE      : TO CHECK FOR TERMINATION OF ALL MET.D.B. STORAGE JOBS.
!                                                                      
! DESCRIPTION  : "HKSTOP" WAITS TO SEE THAT ALL MET.D.B. STORAGE JOBS  
!                HAVE INDICATED THAT THEY ARE TERMINATING. IF ANY DO   
!                NOT RESPOND IN A REASONABLE TIME (E.G. THEY MAY HAVE  
!                ALREADY TERMINATED ABNORMALLY), DETAILS OF THOSE      
!                STILL THOUGHT TO BE RUNNING ARE PRINTED OUT.          
!                                                                      
!                IF ALL WAITING DATA ARE TO BE STORED BEFORE TERMINA-  
!                TION, "KODE" SHOULD BE SPECIFIED AS 1: THEN "HKSTOP"  
!                WILL ALSO CHECK ON ANY OUTSTANDING DATA NOT STORED    
!                AND WILL PRINT OUT DETAILS BEFORE RETURNING.          
!                                                                      
! USAGE        : CALL HKSTOP (RECORD, KODE)                            
!                                                                      
! PARAMETERS   : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF  
!                             HOUSEKEEPING RECORDS AS FAR AS THE END   
!                             OF THE DATA SET STATUS RECORDS.          
!                                                                      
!                KODE    I/O  (INPUT) TERMINATION MODE AS FOLLOWS:     
!                                                                      
!                               0: TERMINATE STORAGE JOBS IMMEDIATELY. 
!                               1: WAIT FOR ALL DATA TO BE PROCESSED.  
!                                                                      
!                             (OUTPUT) RETURN CODE AS FOLLOWS:         
!                                                                      
!                               0: ALL WAITING DATA PROCESSED AND ALL  
!                                  STORAGE JOBS TERMINATED.            
!                               1: AT LEAST 1 JOB STILL RUNNING OR     
!                                  ABNORMALLY TERMINATED.              
!                               2: AT LEAST ONE DATA SET WITH DATA NOT 
!                                  PROCESSED BY STORAGE JOBS.          
!                               3: BOTH OF (1) AND (2) ABOVE.          
!                                                                      
! CALLED BY    : MET.D.B. STORAGE MONITOR PROGRAM.                     
!                                                                      
! CALLS        : HKUPDATE, SECSOUT, TELLOPS.                           
!                                                                      
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN   
!                ON UNIT 1.                                            
!                                                                      
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:47$
! $Source: /home/us0400/mdb/op/lib/source/RCS/hkstop.F,v $
!                                                                      
! CHANGE RECORD :                                                      
!                                                                      
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:47    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:39  usmdb
! Removed unused variable, added copyright and modified
! header - S.Cox
!
! Revision 1.1  2000/06/08  15:41:12  15:41:12  usmdb (Generic MetDB account)
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

!                                                            Parameters
!
      INTEGER  JLENGTH          ! BYTES USED IN JOB STATUS RECORDS
      INTEGER  LENTRY           ! SLOT LENGTH IN DATA SET STATUS RECORD
      INTEGER  MAXJOBS          ! MAXIMUM NUMBER OF JOB STATUS RECORDS
      INTEGER  MAXSTOR          ! MAXIMUM NUMBER OF STORAGE DATA TYPES
!
      PARAMETER (MAXSTOR=50, MAXJOBS=50, JLENGTH=28+4*MAXSTOR)
      PARAMETER (LENTRY=MAXSTOR+44)
!                                                             Variables
!
      INTEGER I, J, K, L        ! DOGSBODY VARIABLES FOR LOCAL USE
      INTEGER JCLEAR            ! HIGHEST DATA SET CLEARED BY JOB
      INTEGER JDS               ! LOOP VARIABLE FOR DATA SETS
      INTEGER JOB               ! LOOP VARIABLE FOR JOB STATUS RECORDS
      INTEGER JSTORE            ! LOOP VARIABLE FOR STORAGE TYPES
      INTEGER JOBNUM(MAXSTOR)   ! JOB NUMBERS FOR EACH STORAGE TYPE
      INTEGER JOBSTOP(MAXJOBS)  ! CURRRENT STATUS OF EACH STORAGE JOB
      INTEGER KODE              ! RETURN CODE
      INTEGER LASTFUL           ! PREVIOUS VALUE OF "NUMFUL"
      INTEGER LASTJOBS          ! PREVIOUS VALUE OF "NUMJOBS"
      INTEGER LATEST            ! NUMBER OF LAST DATA SET READ
      INTEGER NCLEAR            ! ALL DATA SETS CLEARED UP TO HERE
      INTEGER NDATA             ! LOCATION OF DATA SET IN STATUS RECORD
      INTEGER NDREC             ! NO. OF CURRENT DATA SET STATUS RECORD
      INTEGER NDSRECS           ! TOTAL NO. OF DATA SET STATUS RECORDS
      INTEGER NJREC, NJRECS     ! JOB STATUS RECORD NO. & TOTAL NUMBER
      INTEGER NREPEAT           ! NO. OF LOOKS SINCE H.K. STATUS CHANGED
      INTEGER NSTORE            ! NUMBER OF DATA TYPES BEING STORED
      INTEGER NSTORES           ! MAXIMUM NO. OF STORAGE DATA TYPES
      INTEGER NUMDS             ! MAXIMUM NUMBER OF DATA SET SLOTS
      INTEGER NUMJOBS           ! NUMBER OF STORAGE JOBS STILL RUNNING
      INTEGER NUMFUL            ! NUMBER OF DATA SETS STILL UNCLEARED
!
      LOGICAL WAITING           ! .TRUE. IF WAITING FOR JOBS TO STOP
!
      CHARACTER ZERO                ! HEX "00"
      CHARACTER*8 STORTYPE(MAXSTOR) ! STORAGE DATA TYPES (FROM RECORD 1)
      CHARACTER*8 UNSTORED(MAXSTOR) ! DATA TYPES NOT BEING STORED
      CHARACTER*(JLENGTH) JOBREC    ! JOB STATUS RECORD CONTENTS
      CHARACTER*(*) RECORD(*)       ! HOUSEKEEPING RECORDS
      CHARACTER*132 HEAD            ! FOR REVISION DETAILS
!                                                                  Data
      DATA JOBNUM/MAXSTOR*0/, JOBSTOP/MAXJOBS*-1/
!                                                  Revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/hkstop.F,v $
     &'//'$Date: 30/01/2006 20:22:47$ $Revision: 1$'
!
!=======================================================================
!  1. INITIALISATIONS
!=======================================================================
!
!                                             Decode data from record 1
      READ (RECORD(1)(25:),'(5A4,50A8)')
     &      NDSRECS, NJRECS, NSTORES, NJREC, NSTORE, STORTYPE
!
!                                          Get number of data set slots
      NUMDS = 10*NDSRECS
!                                 Get number of last data set processed
!                               and how far data sets have been cleared
!
      READ (RECORD(2)(13:20),'(2A4)') LATEST, NCLEAR
!
!-----------------------------------------------------------------------
!  Initialise number of uncleared data sets and number of storage jobs
!  still running. (The former is set to -1 if no checks are to be made
!  on uncleared data, i.e. if not waiting for all data to be cleared.)
!  Variables are initialised to 'worst case' values as the DO WHILE
!  loop below checks whether they have changed each time round.
!-----------------------------------------------------------------------
!
      IF (KODE.GT.0) THEN
         NUMFUL = LATEST - NCLEAR
      ELSE
         NUMFUL = -1
      END IF
      NUMJOBS = NJRECS
!
!-----------------------------------------------------------------------
!  Other initialisations (mainly for loop which follows)
!-----------------------------------------------------------------------
!
      ZERO = CHAR(0)
      NREPEAT = 0
      WAITING = .TRUE.
!
!=======================================================================
!  2. LOOP UNTIL ALL JOBS HAVE STOPPED (OR UNTIL PROGRESS TO CLEARING
!     ALL DATA SETS SEEMS TO HAVE TERMINATED)
!=======================================================================
!
      DO WHILE (WAITING .AND. NUMJOBS.GT.0)
!                                                       Wait 20 seconds
         CALL SECSOUT (20)
!                                Keep details from last time round loop
         LASTJOBS = NUMJOBS
         LASTFUL = NUMFUL
!
!-----------------------------------------------------------------------
!  Update status of all storage jobs (e.g. whether running or not).
!  This is not done if all jobs were terminated last time round loop.
!-----------------------------------------------------------------------
!
         NUMJOBS = 0 ! reset counter
!                                           Loop over jobs: jump out at
!                                            first unused status record
         DO JOB=1,NJRECS
            READ (1,REC=2+NDSRECS+JOB) JOBREC
            IF (JOBREC(1:4).EQ.ZERO//ZERO//ZERO//ZERO) GO TO 1
!
!                   Set flags for storage types indicating which job is
!                 storing them if this information has not yet been set
!
            IF (JOBSTOP(JOB).LT.0) THEN        ! info not yet set
               READ (JOBREC(25:28),'(A4)') K   ! No. of data types
               DO JSTORE=1,K
                  L = 25 + 4*JSTORE     ! Start of data type index
                  READ (JOBREC(L:L+3),'(A4)') J  ! Data type index
                  JOBNUM(J) = JOB
               END DO
            END IF
!
!-----------------------------------------------------------------------
!  Set job status for each job.  0=completed, 1=stopped prematurely,
!  2=still running (or abnormal termination), -1=do not yet know
!-----------------------------------------------------------------------
!
            IF (JOBREC(2:2).NE.ZERO) THEN      ! job has stopped
               READ (JOBREC(13:16),'(A4)') JCLEAR
               IF (JCLEAR.GE.LATEST) THEN      ! all data cleared
                  JOBSTOP(JOB) = 0
               ELSE                            ! data not looked at
                  JOBSTOP(JOB) = 1
               END IF
 !
            ELSE IF (JOBREC(1:1).NE.ZERO) THEN ! job still running
               JOBSTOP(JOB) = 2
               NUMJOBS = NUMJOBS + 1           ! counts running jobs
            END IF
         END DO ! JOB
    1    CONTINUE
!
!-----------------------------------------------------------------------
!  Update status of all data sets (e.g. whether cleared or not).
!  This is not done if all data sets were cleared last time round loop.
!-----------------------------------------------------------------------
!
         IF (LASTFUL.GT.0) THEN
            CALL HKUPDATE (RECORD)
            READ (RECORD(2)(13:20),'(2A4)') LATEST, NCLEAR
!
!                                   Count data sets with uncleared data
            NUMFUL = 0
            DO J=1,NDSRECS
               DO JDS=1,10
                  IF (RECORD(J+2)(JDS:JDS).NE.ZERO) NUMFUL = NUMFUL + 1
               END DO ! JDS
            END DO ! J
         END IF
!
!-----------------------------------------------------------------------
!  Count the number of times round the loop since things last changed
!  ("NREPEAT"). If this reaches 6, give up waiting and terminate loop.
!-----------------------------------------------------------------------
!
         IF (NUMFUL.LT.LASTFUL .OR. NUMJOBS.LT.LASTJOBS) THEN
            NREPEAT = 0
         ELSE
            NREPEAT = NREPEAT + 1
            IF (NREPEAT.GE.6) WAITING = .FALSE.
         END IF
      END DO ! WHILE
!
!=======================================================================
!  3. OUTPUT MESSAGES GIVING FINAL STATUS OF STORAGE JOBS AND DATA SETS
!=======================================================================
!                                                Initialise return code
      KODE = 0
!
!-----------------------------------------------------------------------
!  Messages for final status of storage jobs
!-----------------------------------------------------------------------
!                                           All storage jobs terminated
      IF (NUMJOBS.LE.0) THEN
         WRITE (6,'(/T5,A,T15,A)') 'HKSTOP:',
     &            'ALL STORAGE JOBS HAVE TERMINATED'
      ELSE
!                                         At least 1 job not terminated
         KODE = 1
         WRITE (6,'(1X)') ! (blank line)
!                                        Send error message to operator
         CALL TELLOPS
     &       ('MDB(E):  MDB STORAGE JOB STILL RUNNING OR ABENDED.')
!
!                             Loop over storage jobs printing a message
!                                if the job termination flag is not set
         DO JOB=1,NJRECS
            IF (JOBSTOP(JOB).EQ.2) THEN ! termination flag not set
               READ (1,REC=2+NDSRECS+JOB) JOBREC
               WRITE (6,'(T5,A,T15,3A)') 'HKSTOP:','JOB ',JOBREC(17:24),
     &                  ' STILL RUNNING OR ABNORMALLY TERMINATED'
            END IF
         END DO ! JOB
      END IF
!
!-----------------------------------------------------------------------
!  Messages for final status of data sets
!-----------------------------------------------------------------------
!                                                 All data sets cleared
      IF (NUMFUL.EQ.0) THEN
         WRITE (6,'(/T5,A,T15,A)') 'HKSTOP:',
     &            'ALL DATA HAS BEEN PROCESSED BY STORAGE JOBS'
!
      ELSE IF (NUMFUL.GT.0) THEN
!                                       At least 1 data set not cleared
         KODE = KODE + 2
!                                                     Heading for table
         WRITE (6,'(/T5,A,T15,A/)') 'HKSTOP:',
     &            'DATA IN THE FOLLOWING DATA SETS HAS NOT BEEN STORED:'
!
!                                        Send error message to operator
         CALL TELLOPS
     &       ('MDB(E): SOME DATA HAS NOT BEEN STORED IN MET.D.B.')
!
!                                 Loop over relevant data sets printing
!                                 a message if there's data not cleared
         DO JDS=NCLEAR+1,LATEST
            NDREC = MOD(JDS,NUMDS)/10 + 3  ! record number
            NDATA = MOD(JDS,10) + 1        ! slot in record
!
!                         Check data set number and 'data cleared' flag
!
            I = (NDATA-1)*LENTRY + 11            ! loc. of d.s. number
            READ (RECORD(NDREC)(I:I+3),'(A4)') J ! data set number
!
!                                      If there's any unstored data ...
!
            IF (J.EQ.JDS .AND. RECORD(NDREC)(NDATA:NDATA).NE.ZERO) THEN
               K = I + 44                  ! start of data indicators
               L = 0 ! 'unstored' counter
!                                    ... locate data types involved ...
               DO JSTORE=1,NSTORE
                  IF (RECORD(NDREC)(K:K).NE.ZERO) THEN
                     L = L + 1
                     UNSTORED(L) = STORTYPE(JSTORE)
                  END IF
                  K = K + 1
               END DO ! JSTORE
!                                         ... and print out the details
!
               I = I + 4                   ! start of d.s. name
               WRITE (6,'(T15,A,(T57,8A9))')
     &                   RECORD(NDREC)(I:I+39), (UNSTORED(J),J=1,L)
            END IF
         END DO ! JDS
!
!-----------------------------------------------------------------------
!  If there is still uncleared data, report on jobs prematurely
!  terminated and data types for which there is no storage job
!-----------------------------------------------------------------------
!
!                                                Loop over storage jobs
         DO JSTORE=1,NSTORE
!                                    Check whether data is being stored
!
            IF (JOBNUM(JSTORE).EQ.0) THEN ! it isn't
               WRITE (6,'(T5,A,T15,2A)') 'HKSTOP:',
     &              'NO JOB IS PROCESSING DATA TYPE ', STORTYPE(JSTORE)
!
!                              Check whether job terminated prematurely
!
            ELSE IF (JOBSTOP(JOBNUM(JSTORE)).EQ.1) THEN ! it did
               READ (1,REC=2+NDSRECS+JOBNUM(JSTORE)) JOBREC
               WRITE (6,'(T5,A,T15,3A)') 'HKSTOP:',
     &                  'JOB ', JOBREC(17:24),
     &                  ' WAS PREMATURELY TERMINATED'
!
!                                         Next line prevents repeats of
!                                         this message for the same job
               JOBSTOP(JOBNUM(JSTORE)) = 0
            END IF
         END DO ! JSTORE
      END IF
!                                             Return to calling program
      RETURN
      END
