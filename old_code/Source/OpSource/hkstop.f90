SUBROUTINE HKSTOP (RECORD, KODE)

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
! ARGUMENTS    : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF
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
! CALLS        : HKUPDATE, SECSOUT, TELLOPS
!                METDB_CREAD_DIR from MetDB_c_utils.c
!
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                ON UNIT 1.
!
! REVISION INFO :
!
! $Workfile: hkstop.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 04/03/2011 08:44:58$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         04/03/2011 08:44:58    Sheila Needham
!       Corrected displacement for STORTYPE in record 1
!  3    MetDB_Refresh 1.2         03/03/2011 11:56:55    Sheila Needham
!       Changes for C I/O and internal reads
!  2    MetDB_Refresh 1.1         14/02/2011 14:29:03    John Norton     After
!       porting for Monitor batches 1 & 2
!  1    MetDB_Refresh 1.0         07/02/2011 11:28:26    John Norton     f77
!       version of MONITOR porting batch 
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE hkupdate_mod
!USE secsout_mod     ! C Routine
!USE tellops_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: RECORD(*) !a01 HOUSEKEEPING RECORDS
INTEGER,          INTENT(INOUT) :: KODE      !a02 RETURN CODE

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAXSTOR=50           ! MAXIMUM NUMBER OF STORAGE DATA TYPES
INTEGER,     PARAMETER ::  JLENGTH=28+4*MAXSTOR ! BYTES USED IN JOB STATUS RECORDS
INTEGER,     PARAMETER ::  LENTRY=MAXSTOR+44    ! SLOT LENGTH IN DATA SET STATUS RECORD
INTEGER,     PARAMETER ::  MAXJOBS=50           ! MAXIMUM NUMBER OF JOB STATUS RECORDS
INTEGER,     PARAMETER ::  IUNIT=1              ! FT number for HKDS 
INTEGER,     PARAMETER ::  LENREC=950           ! HKDS record length
!
!                                                             Variables
INTEGER          ::  I    ! DOGSBODY VARIABLE FOR LOCAL USE
INTEGER          ::  INT4  ! Dummmy variable for TRANSFER function
INTEGER          ::  IOFLAG ! I/O status (not tested)
INTEGER          ::  J    ! DOGSBODY VARIABLE FOR LOCAL USE
INTEGER          ::  JCLEAR ! HIGHEST DATA SET CLEARED BY JOB
INTEGER          ::  JDS  ! LOOP VARIABLE FOR DATA SETS
INTEGER          ::  JOB  ! LOOP VARIABLE FOR JOB STATUS RECORDS
INTEGER          ::  JSTORE ! LOOP VARIABLE FOR STORAGE TYPES
INTEGER          ::  JOBNUM(MAXSTOR)=MAXSTOR*0 ! JOB NUMBERS FOR EACH STORAGE TYPE
INTEGER          ::  JOBSTOP(MAXJOBS)=MAXJOBS*(-1) ! CURRRENT STATUS OF EACH STORAGE JOB
INTEGER          ::  K    ! DOGSBODY VARIABLE FOR LOCAL USE
INTEGER          ::  L    ! DOGSBODY VARIABLE FOR LOCAL USE
INTEGER          ::  LASTFUL ! PREVIOUS VALUE OF "NUMFUL"
INTEGER          ::  LASTJOBS ! PREVIOUS VALUE OF "NUMJOBS"
INTEGER          ::  LATEST ! NUMBER OF LAST DATA SET READ
INTEGER          ::  NCLEAR ! ALL DATA SETS CLEARED UP TO HERE
INTEGER          ::  NDATA ! LOCATION OF DATA SET IN STATUS RECORD
INTEGER          ::  NDREC ! NO. OF CURRENT DATA SET STATUS RECORD
INTEGER          ::  NDSRECS ! TOTAL NO. OF DATA SET STATUS RECORDS
INTEGER          ::  NJREC, NJRECS ! JOB STATUS RECORD NO. & TOTAL NUMBER
INTEGER          ::  NREPEAT ! NO. OF LOOKS SINCE H.K. STATUS CHANGED
INTEGER          ::  NSTORE ! NUMBER OF DATA TYPES BEING STORED
INTEGER          ::  NSTORES ! MAXIMUM NO. OF STORAGE DATA TYPES
INTEGER          ::  NUMDS ! MAXIMUM NUMBER OF DATA SET SLOTS
INTEGER          ::  NUMJOBS ! NUMBER OF STORAGE JOBS STILL RUNNING
INTEGER          ::  NUMFUL ! NUMBER OF DATA SETS STILL UNCLEARED
INTEGER          ::  RECNO  ! Record number
!
LOGICAL          ::  WAITING ! .TRUE. IF WAITING FOR JOBS TO STOP
!
CHARACTER(LEN=1)       ::  ZERO     ! HEX "00"
CHARACTER(LEN=8)       ::  STORTYPE(MAXSTOR) ! STORAGE DATA TYPES (FROM RECORD 1)
CHARACTER(LEN=8)       ::  UNSTORED(MAXSTOR) ! DATA TYPES NOT BEING STORED
CHARACTER(LEN=JLENGTH) ::  JOBREC ! JOB STATUS RECORD CONTENTS
CHARACTER(LEN=LENREC)  ::  BUFFER ! I/O Buffer
! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!
!=======================================================================
!  1. INITIALISATIONS
!=======================================================================
!
!                                             Decode data from record 1
NDSRECS = TRANSFER(RECORD(1)(25:28),INT4)
NJRECS = TRANSFER(RECORD(1)(29:32),INT4)
NSTORES = TRANSFER(RECORD(1)(33:36),INT4)
NJREC = TRANSFER(RECORD(1)(37:40),INT4)
NSTORE =TRANSFER(RECORD(1)(41:44),INT4)
READ(RECORD(1)(45:),'(50A8)')STORTYPE
!
!                                          Get number of data set slots
NUMDS = 10*NDSRECS
!                                 Get number of last data set processed
!                               and how far data sets have been cleared
!
LATEST = TRANSFER(RECORD(2)(13:16),INT4)
NCLEAR = TRANSFER(RECORD(2)(17:20),INT4)
!
!-----------------------------------------------------------------------
!  Initialise number of uncleared data sets and number of storage jobs
!  still running. (The former is set to -1 if no checks are to be made
!  on uncleared data, i.e. if not waiting for all data to be cleared.)
!  Variables are initialised to 'worst case' values as the DO WHILE
!  loop below checks whether they have changed each time round.
!-----------------------------------------------------------------------
!
IF (KODE > 0) THEN
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
DOLABEL1: &
DO WHILE (WAITING .AND. NUMJOBS > 0)
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
DOLABEL2: &
   DO JOB=1,NJRECS
      RECNO = 2+NDSRECS+JOB
      CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,RECNO,IOFLAG)
      JOBREC(1:JLENGTH) = BUFFER(1:JLENGTH)
      IF (JOBREC(1:4) == ZERO//ZERO//ZERO//ZERO) EXIT DOLABEL2
!
!                   Set flags for storage types indicating which job is
!                 storing them if this information has not yet been set
!
      IF (JOBSTOP(JOB) < 0) THEN         ! info not yet set
         K = TRANSFER(JOBREC(25:28),INT4) ! No. of data types         
         DO JSTORE=1,K
            L = 25 + 4*JSTORE     ! Start of data type index
            J = TRANSFER(JOBREC(L:L+3),INT4) ! Data type index
            JOBNUM(J) = JOB
         END DO
      END IF
!
!-----------------------------------------------------------------------
!  Set job status for each job.  0=completed, 1=stopped prematurely,
!  2=still running (or abnormal termination), -1=do not yet know
!-----------------------------------------------------------------------
!
IFLABEL1: &
      IF (JOBREC(2:2) /= ZERO) THEN      ! job has stopped
         JCLEAR = TRANSFER(JOBREC(13:16),INT4)
         IF (JCLEAR >= LATEST) THEN      ! all data cleared
            JOBSTOP(JOB) = 0
         ELSE                            ! data not looked at
            JOBSTOP(JOB) = 1
         END IF
 !
      ELSE IF (JOBREC(1:1) /= ZERO) THEN ! job still running
         JOBSTOP(JOB) = 2
         NUMJOBS = NUMJOBS + 1           ! counts running jobs
      END IF IFLABEL1
   END DO DOLABEL2 ! JOB
!
!-----------------------------------------------------------------------
!  Update status of all data sets (e.g. whether cleared or not).
!  This is not done if all data sets were cleared last time round loop.
!-----------------------------------------------------------------------
!
IFLABEL2: &
   IF (LASTFUL > 0) THEN
      CALL HKUPDATE (RECORD)
      LATEST = TRANSFER(RECORD(2)(13:16),INT4)
      NCLEAR = TRANSFER(RECORD(2)(17:20),INT4)
!
!                                   Count data sets with uncleared data
      NUMFUL = 0
      DO J=1,NDSRECS
         DO JDS=1,10
            IF (RECORD(J+2)(JDS:JDS) /= ZERO) NUMFUL = NUMFUL + 1
         END DO ! JDS
      END DO ! J
   END IF IFLABEL2
!
!-----------------------------------------------------------------------
!  Count the number of times round the loop since things last changed
!  ("NREPEAT"). If this reaches 6, give up waiting and terminate loop.
!-----------------------------------------------------------------------
!
   IF (NUMFUL < LASTFUL .OR. NUMJOBS < LASTJOBS) THEN
      NREPEAT = 0
   ELSE
      NREPEAT = NREPEAT + 1
      IF (NREPEAT >= 6) WAITING = .FALSE.
   END IF
END DO DOLABEL1 ! WHILE
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
IFLABEL3: &
IF (NUMJOBS <= 0) THEN
   WRITE (6,'(/T5,A,T15,A)') 'HKSTOP:', &
            'ALL STORAGE JOBS HAVE TERMINATED'
ELSE
!                                         At least 1 job not terminated
   KODE = 1
   WRITE (6,'(1X)') ! (blank line)
!                                        Send error message to operator
   CALL TELLOPS &
       ('MDB(E):  MDB STORAGE JOB STILL RUNNING OR ABENDED.')
!
!                             Loop over storage jobs printing a message
!                                if the job termination flag is not set
   DO JOB=1,NJRECS
      IF (JOBSTOP(JOB) == 2) THEN ! termination flag not set
         RECNO = 2+NDSRECS+JOB
         CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,RECNO,IOFLAG)
         JOBREC(1:JLENGTH) = BUFFER(1:JLENGTH)
         WRITE (6,'(T5,A,T15,3A)') 'HKSTOP:','JOB ',JOBREC(17:24), &
                  ' STILL RUNNING OR ABNORMALLY TERMINATED'
      END IF
   END DO ! JOB
END IF IFLABEL3
!
!-----------------------------------------------------------------------
!  Messages for final status of data sets
!-----------------------------------------------------------------------
!                                                 All data sets cleared
IFLABEL4: &
IF (NUMFUL == 0) THEN
   WRITE (6,'(/T5,A,T15,A)') 'HKSTOP:', &
            'ALL DATA HAS BEEN PROCESSED BY STORAGE JOBS'
!
ELSE IF (NUMFUL > 0) THEN
!                                       At least 1 data set not cleared
   KODE = KODE + 2
!                                                     Heading for table
   WRITE (6,'(/T5,A,T15,A/)') 'HKSTOP:', &
            'DATA IN THE FOLLOWING DATA SETS HAS NOT BEEN STORED:'
!
!                                        Send error message to operator
   CALL TELLOPS &
       ('MDB(E): SOME DATA HAS NOT BEEN STORED IN MET.D.B.')
!
!                                 Loop over relevant data sets printing
!                                 a message if there's data not cleared
DOLABEL3: &
   DO JDS=NCLEAR+1,LATEST
      NDREC = MOD(JDS,NUMDS)/10 + 3  ! record number
      NDATA = MOD(JDS,10) + 1        ! slot in record
!
!                         Check data set number and 'data cleared' flag
!
      I = (NDATA-1)*LENTRY + 11            ! loc. of d.s. number
      J = TRANSFER(RECORD(NDREC)(I:I+3),INT4) ! data set number
!
!                                      If there's any unstored data ...
!
IFLABEL5: &
      IF (J == JDS .AND. RECORD(NDREC)(NDATA:NDATA) /= ZERO) THEN
         K = I + 44                  ! start of data indicators
         L = 0 ! 'unstored' counter
!                                    ... locate data types involved ...
         DO JSTORE=1,NSTORE
            IF (RECORD(NDREC)(K:K) /= ZERO) THEN
               L = L + 1
               UNSTORED(L) = STORTYPE(JSTORE)
            END IF
            K = K + 1
         END DO ! JSTORE
!                                         ... and print out the details
!
         I = I + 4                   ! start of d.s. name
         WRITE (6,'(T15,A,(T57,8A9))') &
                   RECORD(NDREC)(I:I+39), (UNSTORED(J),J=1,L)
      END IF IFLABEL5
   END DO DOLABEL3 ! JDS
!
!-----------------------------------------------------------------------
!  If there is still uncleared data, report on jobs prematurely
!  terminated and data types for which there is no storage job
!-----------------------------------------------------------------------
!
!                                                Loop over storage jobs
DOLABEL4: &
   DO JSTORE=1,NSTORE
!                                    Check whether data is being stored
!
IFLABEL6: &
      IF (JOBNUM(JSTORE) == 0) THEN ! it isn't
         WRITE (6,'(T5,A,T15,2A)') 'HKSTOP:', &
              'NO JOB IS PROCESSING DATA TYPE ', STORTYPE(JSTORE)
!
!                              Check whether job terminated prematurely
!
      ELSE IF (JOBSTOP(JOBNUM(JSTORE)) == 1) THEN ! it did
         RECNO = 2+NDSRECS+JOBNUM(JSTORE)
         CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,RECNO,IOFLAG)
         JOBREC(1:JLENGTH) = BUFFER(1:JLENGTH)
         WRITE (6,'(T5,A,T15,3A)') 'HKSTOP:', &
                  'JOB ', JOBREC(17:24), &
                  ' WAS PREMATURELY TERMINATED'
!
!                                         Next line prevents repeats of
!                                         this message for the same job
         JOBSTOP(JOBNUM(JSTORE)) = 0
      END IF IFLABEL6
   END DO DOLABEL4 ! JSTORE
END IF IFLABEL4
!                                             Return to calling program
RETURN
END SUBROUTINE HKSTOP
