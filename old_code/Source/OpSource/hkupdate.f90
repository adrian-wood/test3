SUBROUTINE HKUPDATE (RECORD)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : HKUPDATE
!
! PURPOSE       : TO UPDATE THE STATUS INFORMATION FOR DATA SETS IN THE
!                 HOUSEKEEPING DATA SET.
!
! DESCRIPTION   : "HKUPDATE" UPDATES THE INFORMATION ABOUT DATA SETS IN
!                 THE HOUSEKEEPING DATA SET, CLEARING DETAILS OF DATA
!                 WHICH HAS NOW BEEN STORED AND SETTING A FLAG WHEN THE
!                 WHOLE DATA SET HAS BEEN PROCESSED (IN WHICH CASE THE
!                 SLOT CAN BE REUSED FOR ANOTHER DATA SET).
!
!                 THE ROUTINE IS CALLED INTERMITTENTLY AND WILL PRINT
!                 WARNING MESSAGES IF (1) IT FINDS TWO STORAGE JOBS
!                 USING THE SAME STORAGE DATA SET, (2) A STORAGE JOB
!                 HAS BEEN INACTIVE FOR SOME TIME, OR (3) A DATA SET
!                 HAS BEEN PASSED BY ALL STORAGE JOBS BUT STILL HAS
!                 UNSTORED DATA IN IT.
!
! USAGE         : CALL HKUPDATE (RECORD)
!
! ARGUMENTS     : RECORD  I/O  CHARACTER ARRAY TO HOLD THE CONTENTS OF
!                              HOUSEKEEPING RECORDS AS FAR AS THE END
!                              OF THE DATA SET STATUS RECORDS.
!
! CALLED BY     : FINDREC, HKSTOP, MET.D.B. MONITOR JOB
!
! CALLS         : TELLOPS
!                 METDB_CWRITE_DIR and METDB_CREAD_DIR from MetDB_c_utils
!
! FILES USED    : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                 ON UNIT 1.
!
! REVISION INFO :
!
! $Workfile: hkupdate.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 03/03/2011 11:56:55$
!
! CHANGE RECORD :
!
! $Log:
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

!USE tellops_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: RECORD(*) !a01 HOUSEKEEPING RECORDS

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER,     PARAMETER ::  MAXDATA=160          ! MAX. NO. OF DATA SET STATUS RECORDS
INTEGER,     PARAMETER ::  MAXJOBS=50           ! MAXIMUM NUMBER OF JOB STATUS RECORDS
INTEGER,     PARAMETER ::  MAXTYPES=50          ! MAXIMUM NUMBER OF STORAGE DATA TYPES
INTEGER,     PARAMETER ::  LENTRY=MAXTYPES+44   ! SLOT LENGTH IN DATA SET STATUS RECORD
INTEGER,     PARAMETER ::  LENJOB=4*MAXTYPES+28 ! BYTES USED IN JOB STATUS RECORDS
INTEGER,     PARAMETER ::  MAXREC=2+MAXDATA     ! SIZE OF "RECORD" ARRAY
INTEGER,     PARAMETER ::  IUNIT=1              ! FT number for HKDS
INTEGER,     PARAMETER ::  LENREC=950           ! HKDS record length
!
!                                                             Variables
!
INTEGER          ::  I    ! INTEGER VARIABLE FOR LOCAL USE
INTEGER          ::  ICLEAR ! DATA SETS CLEARED BY STORE JOB TO HERE
INTEGER          ::  INT4   ! Dummy variable for TRANSFER function
INTEGER          ::  IOFLAG ! I/O status (not tested) 
INTEGER          ::  IPOS   ! Pointer to position in string
INTEGER          ::  J    ! INTEGER VARIABLE FOR LOCAL USE
INTEGER          ::  JACCESS ! ACCESS COUNTER IN JOB STATUS RECORD
INTEGER          ::  JDATA ! LOOP VARIABLE FOR DATA SETS
INTEGER          ::  JOB  ! LOOP VARIABLE FOR STORAGE JOBS
INTEGER          ::  LATEST ! NO. OF LAST DATA SET READ BY MONITOR
INTEGER          ::  MINCLEAR ! LATEST DATA SET CLEARED BY ALL JOBS
INTEGER          ::  N    ! INTEGER VARIABLE FOR LOCAL USE
INTEGER          ::  NACCESS ! ACCESS COUNTER IN H.K. STATUS RECORD
INTEGER          ::  NCLEAR ! DATA SETS CLEARED BY MONITOR TO HERE
INTEGER          ::  NDATA ! LOCATION OF DATA SET IN STATUS RECORD
INTEGER          ::  NDREC ! NO. OF CURRENT DATA SET STATUS RECORD
INTEGER          ::  NDRECS ! TOTAL NO. OF DATA SET STATUS RECORDS
INTEGER          ::  NDSTORE ! NUMBER OF DATA TYPES BEING STORED
INTEGER          ::  NDSTORES ! MAXIMUM NO. OF STORAGE DATA TYPES
INTEGER          ::  NJCLEAR(MAXJOBS) ! DATA SETS CLEARED TO HERE LAST TIME
INTEGER          ::  NJRECS ! TOTAL NUMBER OF JOB STATUS RECORDS
INTEGER          ::  NJOBS ! NUMBER OF JOB STATUS RECORDS IN USE
INTEGER          ::  NOTCLEAR ! OLDEST DATA SET WITH UNCLEARED DATA
INTEGER          ::  NSECS ! LAST TIME STORAGE JOB ACCESSED H.K.
INTEGER          ::  NTYPES(MAXJOBS)=MAXJOBS*(-1) ! NO. OF DATA TYPES STORED BY EACH JOB
INTEGER          ::  NTYPE(MAXJOBS,MAXTYPES) ! DATA TYPES STORED BY EACH JOB
INTEGER          ::  NUMDS ! MAXIMUM NUMBER OF DATA SET SLOTS
INTEGER          ::  RECNO ! Record number
!
LOGICAL          ::  ALLCLEAR ! .TRUE. IF DATA SET IS FULLY CLEARED
LOGICAL          ::  CHANGED(MAXREC) ! .TRUE. IF H.K. RECORD HAS CHANGED
LOGICAL          ::  FIRST=.TRUE. ! .TRUE. IF FIRST CALL TO HKUPDATE
LOGICAL          ::  READY=.FALSE. ! .TRUE. IF OK TO CHECK UNCLEARED DATA
!
CHARACTER(LEN=1)      :: ZERO ! HEX "00"
CHARACTER(LEN=4)      :: JFLAG ! FLAG BYTE IN JOB STATUS RECORD
CHARACTER(LEN=8)      :: JOBNAME ! JOB NAME FROM JOB STATUS RECORD
CHARACTER(LEN=8)      :: UNSTORED(MAXTYPES) ! DATA TYPES NOT BEING STORED
CHARACTER(LEN=LENJOB) :: JOBREC ! JOB STATUS RECORD CONTENTS
CHARACTER(LEN=80)     :: ERRTXT ! TEXT FOR OPERATOR MESSAGES
CHARACTER(LEN=LENREC) :: BUFFER ! I/O buffer
CHARACTER(LEN=4)      :: CH4  ! dummy variable for transfer function

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!                                                                 Saves
SAVE NTYPES, NTYPE, NJCLEAR, NUMDS, FIRST, READY,  ZERO
SAVE NDRECS, NJRECS, NDSTORES, NJOBS, NDSTORE
!
!=======================================================================
!  1. INITIALISATIONS
!=======================================================================
!                                       First call only:  Decode record
!                                            1 of housekeeping data set
IF (FIRST) THEN
   NDRECS   = TRANSFER(RECORD(1)(25:28),INT4)
   NJRECS   = TRANSFER(RECORD(1)(29:32),INT4)
   NDSTORES = TRANSFER(RECORD(1)(33:36),INT4)      
   NJOBS    = TRANSFER(RECORD(1)(37:40),INT4)
   NDSTORE  = TRANSFER(RECORD(1)(41:44),INT4)
   NUMDS = 10*NDRECS
   ZERO = CHAR(0)
   FIRST = .FALSE.
END IF
!                                                  Also decode record 2
!
NSECS   = TRANSFER(RECORD(2)(5:8),INT4)
NACCESS = TRANSFER(RECORD(2)(9:12),INT4)
LATEST  = TRANSFER(RECORD(2)(13:16),INT4)
NCLEAR  = TRANSFER(RECORD(2)(17:20),INT4)
!
!                                          Haven't changed anything yet
DO J=1,MAXREC
   CHANGED(J) = .FALSE.
END DO ! J
!                                          If there are no data sets to
!                                         clear out, return immediately
IF (NCLEAR >= LATEST) RETURN
!
!=======================================================================
!  2. LOOP OVER JOB STATUS RECORDS, READ EACH RECORD AND UPDATE DATA
!     SET STATUS INFORMATION
!=======================================================================
!
!                                         Loop over job status records.
!           (After the loop MINCLEAR will hold the most recent data set
!              cleared by ALL storage jobs (i.e. the minimum of all the
!             'last processed' entries in JSRs). This cannot be greater
!                        than LATEST so initialise to this value here.)
MINCLEAR = LATEST
DOLABEL1: &
DO JOB=1,NJRECS
!                                           Read next job status record
   RECNO = 2+NDRECS+JOB
   CALL METDB_CREAD_DIR(IUNIT,BUFFER,LENREC,RECNO,IOFLAG)
   JOBREC(1:LENJOB) = BUFFER(1:LENJOB)
!                                            Jump out of loop if end of
!                                    used job status records is reached
!
   IF (JOBREC(1:2) == ZERO//ZERO) EXIT DOLABEL1
!
!                                   Get job details if not already done
   IF (NTYPES(JOB) < 0) THEN
      READ (JOBREC,'(T17,A8)') JOBNAME
      NTYPES(JOB) = TRANSFER(JOBREC(25:28),INT4) 
      IPOS=29
      DO J = 1,NTYPES(JOB)
         NTYPE(JOB,J) = TRANSFER(JOBREC(IPOS:IPOS+3),INT4)
         IPOS = IPOS + 4
      END DO
!
!                          Initialise NJCLEAR if using "SDB1" data sets
!
      IF (NTYPES(JOB) > 0) NJCLEAR(JOB) = NCLEAR
   END IF
!
!-----------------------------------------------------------------------
!  If this is a new job, update the number of active job status records
!  in record 1
!-----------------------------------------------------------------------
!
IFLABEL1: &
   IF (JOB > NJOBS) THEN
      NJOBS = JOB
      RECORD(1)(37:40) = TRANSFER(NJOBS,CH4)
!
!-----------------------------------------------------------------------
!  If the job is using SDB1 data sets, update details in record 1,
!  associating job name with storage data sets it's using
!-----------------------------------------------------------------------
!
IFLABEL2: &
      IF (NTYPES(JOB) > 0) THEN
         N = 8*NDSTORES + 44 ! Just before start of job name info.
DOLABEL2: &
         DO J=1,NTYPES(JOB)
            I = N + 8*NTYPE(JOB,J)
IFLABEL3: &
            IF (RECORD(1)(I-7:I) == ' ') THEN
               RECORD(1)(I-7:I) = JOBNAME
            ELSE
!
!-----------------------------------------------------------------------
!  Warning message if another job is already processing the same data.
! (The message is output even if the two jobs are writing to different
! storage data sets as the monitor job can't tell if this is the case.)
!-----------------------------------------------------------------------
!
               JDATA = 44 + 8*NTYPE(JOB,J)
               WRITE (6,'(/T5,A,T15,8A)') 'HKUPDATE:', &
                    'WARNING - JOBS ', RECORD(1)(I-7:I), ' AND ', &
                    JOBNAME, ' ARE BOTH USING THE STORAGE DATA ', &
                    'SET FOR ', RECORD(1)(JDATA-7:JDATA), ' DATA'
!
!                                      Send warning message to operator
!
               ERRTXT = 'MDB(W):  ' // &
                        RECORD(1)(I-7:I) // ' & ' // JOBNAME // &
                      ' BOTH STORING ' // RECORD(1)(JDATA-7:JDATA)
               CALL TELLOPS (ERRTXT)
            END IF IFLABEL3
         END DO DOLABEL2 ! J
      END IF IFLABEL2
      CHANGED(1) = .TRUE. ! Because record 1 has changed
   END IF IFLABEL1
!
!-----------------------------------------------------------------------
!  If using "SDB1" data sets, find the latest one cleared by this job
!  (ICLEAR). We know that data types processed by this job have been
!  marked as cleared from data sets up to NJCLEAR: now update this as
!  far as data set ICLEAR.
!-----------------------------------------------------------------------
!                                     Loop over data sets newly cleared
IFLABEL4: &
   IF (NTYPES(JOB) > 0) THEN
!                                          Find latest cleared data set
      ICLEAR = TRANSFER(JOBREC(13:16),INT4)
      MINCLEAR = MIN0(MINCLEAR,ICLEAR)
!                                                   Loop over data sets
!                                             processed since last time
DOLABEL3: &
      DO JDATA=NJCLEAR(JOB)+1,ICLEAR
         NDREC = MOD(JDATA,NUMDS)/10 + 3 ! D.s. status record
         NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                       Read & check number on data set
!
         I = (NDATA-1)*LENTRY + 10   ! Just before data set info.
         N = TRANSFER(RECORD(NDREC)(I+1:I+4),INT4) ! Dataset number
IFLABEL5: &
         IF (N == JDATA) THEN
!                                   Set to zeroes all bytes correspond-
!                                     ing to data processed by this job
!
            I = I + 44   ! Just before 'no. of bulletins' bytes
            DO J=1,NTYPES(JOB)
               N = I + NTYPE(JOB,J)  ! Byte for Jth data type
               IF (RECORD(NDREC)(N:N) /= ZERO) THEN
                  RECORD(NDREC)(N:N) = ZERO
                  CHANGED(NDREC) = .TRUE.
               END IF
            END DO ! J
         END IF IFLABEL5
      END DO DOLABEL3 ! JDATA
!                                    Data sets now cleared up to ICLEAR
      NJCLEAR(JOB) = ICLEAR
   END IF IFLABEL4
!
!-----------------------------------------------------------------------
!  Print a warning if job hasn't done anything for a while. (Currently,
!  a 'while' is 10 minutes and at least 10 accesses to record 2.)
!-----------------------------------------------------------------------
!
   JFLAG   = JOBREC(1:4)
   J       = TRANSFER(JOBREC(5:8),INT4)
   JACCESS = TRANSFER(JOBREC(9:12),INT4)
IFLABEL6: &
   IF (JFLAG(1:1) /= ZERO) THEN ! job should be running
      J = NSECS - J
      JACCESS = NACCESS - JACCESS
      IF (J > 600 .AND. JACCESS > 10) THEN
         WRITE (6,'(/T5,A,T15,3A)') 'HKUPDATE:','WARNING - JOB ', &
                JOBREC(17:24),' MAY HAVE TERMINATED. PLEASE CHECK'
!
!                                        Send error message to operator
!
         ERRTXT = 'MDB(E):  HAS ' // JOBREC(17:24)
         ERRTXT(13+INDEX(ERRTXT(14:),' '):) = &
                ' STOPPED? PLEASE CHECK.'
         CALL TELLOPS (ERRTXT)
      END IF
   END IF IFLABEL6
END DO DOLABEL1 ! JOB
!
!=======================================================================
!  3. LOOP OVER DATA SETS MARKING THOSE FOR WHICH ALL DATA HAS NOW BEEN
!     PROCESSED BY STORAGE JOBS. THIS ENABLES THE SLOTS FOR THESE DATA
!     SETS TO BE REUSED.
!=======================================================================
!
NOTCLEAR = LATEST + 1
!
!-----------------------------------------------------------------------
!  Loop over data sets which may now be cleared. Data sets up to NCLEAR
!  were cleared last time: now the highest numbered one is LATEST.
!-----------------------------------------------------------------------
!                                                   Loop over data sets
DOLABEL4: &
DO JDATA=NCLEAR+1,LATEST
   NDREC = MOD(JDATA,NUMDS)/10 + 3 ! Data set status record
   NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                               Read number on data set
   I = (NDATA-1)*LENTRY + 10
   N = TRANSFER(RECORD(NDREC)(I+1:I+4),INT4) ! Dataset number
!
!                   Check number and see if there's still data to clear
!
IFLABEL7: &
   IF (N == JDATA .AND. RECORD(NDREC)(NDATA:NDATA) /= ZERO) THEN
!
!-----------------------------------------------------------------------
!  Loop over all storage types checking to see if there is any data not
!  cleared.  Jump out of loop when uncleared data is found.
!-----------------------------------------------------------------------
!
      I = I + 44        ! skip data set no. (4) & name (40)
      ALLCLEAR = .TRUE. ! until proved false
      J = 1             ! data type counter
!                                                  Loop over data types
      DO WHILE (ALLCLEAR .AND. J <= NDSTORE)
         I = I + 1
         IF (RECORD(NDREC)(I:I) /= ZERO) THEN ! not cleared
            ALLCLEAR = .FALSE.
            NOTCLEAR = MIN0(NOTCLEAR,JDATA)
         END IF
         J = J + 1
      END DO
!                                Set indicator byte if all data cleared
      IF (ALLCLEAR) THEN
         RECORD(NDREC)(NDATA:NDATA) = ZERO
         CHANGED(NDREC) = .TRUE.
      END IF
   END IF IFLABEL7
END DO DOLABEL4 ! JDATA
!
!=======================================================================
!  4. WARNING MESSAGES FOR DATA SETS WITH UNCLEARED DATA WHICH HAS
!     BEEN PASSED BY ALL STORAGE JOBS AND IS THEREFORE UNCLEARABLE.
!=======================================================================
!     (This section is not executed during the first 5 minutes or 10
!     accesses after the housekeeping data set has been reinitialised
!     to prevent printout being produced simply because one or more of
!     the storage jobs is slow to get started.)
!-----------------------------------------------------------------------
!                                       See if ready to check data sets
IF (.NOT.READY) THEN
   IF (NSECS > 300 .AND. NACCESS >= 10) READY = .TRUE.
END IF
!                                                Check unclearable data
IFLABEL8: &
IF (READY .AND. NOTCLEAR <= MINCLEAR) THEN
!                                                  Heading for printout
!
   WRITE (6,'(/T5,A,T15,2A/)') 'HKUPDATE:', 'WARNING - ', &
            'DATA IN THE FOLLOWING DATA SETS IS NOT BEING STORED:'
!
!                                      Send warning message to operator
   CALL TELLOPS &
        ('MDB(W):  SOME DATA IS NOT BEING STORED IN MET.D.B.')
!
!                                                   Loop over data sets
DOLABEL5: &
   DO JDATA=NOTCLEAR,MINCLEAR
      NDREC = MOD(JDATA,NUMDS)/10 + 3 ! Data set status record
      NDATA = MOD(JDATA,10) + 1       ! Location in record
!
!                                               Read number on data set
      I = (NDATA-1)*LENTRY + 10
      N = TRANSFER(RECORD(NDREC)(I+1:I+4),INT4) ! Dataset number
!
!                Check number (in case data set information has already
!              been overwritten) and see if there's still data to clear
!
IFLABEL9: &
      IF (N == JDATA.AND.RECORD(NDREC)(NDATA:NDATA) /= ZERO) THEN
!
!-----------------------------------------------------------------------
!  Loop over all storage types checking to see what data has not been
!  cleared.  Record the uncleared data types for printing out.
!-----------------------------------------------------------------------
!
         N = 0             ! to count uncleared data types
         I = I + 44
!                                                  Loop over data types
         DO J=1,NDSTORE
            I = I + 1
            IF (RECORD(NDREC)(I:I) /= ZERO) THEN ! not cleared
               N = N + 1
               UNSTORED(N) = RECORD(1)(8*J+37:8*J+44)
            END IF
         END DO ! J
!                              Print message if data set is unclearable
!
         IF (N > 0) THEN
            I = (NDATA-1)*LENTRY + 15  ! start of d.s. name
            WRITE (6,'(T15,A,(T57,8A9))') &
                      RECORD(NDREC)(I:I+39), (UNSTORED(J),J=1,N)
         END IF
      END IF IFLABEL9
   END DO DOLABEL5 ! JDATA
END IF IFLABEL8
!
!=======================================================================
!  5. WRITE OUT RECORDS WHICH HAVE CHANGED. ALSO UPDATE RECORD 2 BUT
!     DON'T WRITE OUT IN CASE CALLING PROGRAM NEEDS TO ADD SOMETHING.
!=======================================================================
!                                  Write out records which have changed
DO J=1,MAXREC
   IF (CHANGED(J)) THEN
      RECNO = J
      CALL METDB_CWRITE_DIR(IUNIT,RECORD(J),LENREC,RECNO,IOFLAG)
   END IF
END DO ! J
!                                            Update record 2 in storage
IF (NOTCLEAR-1 > NCLEAR) &
    RECORD(2)(17:20) = TRANSFER(NOTCLEAR-1,CH4)
!
!                                             Return to calling program
RETURN
END SUBROUTINE HKUPDATE
