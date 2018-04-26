 SUBROUTINE FINDJSR (JOBNAME, STOPFLAG, LJREC, JOBREC, NJREC)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : FINDJSR
!
! PURPOSE       : TO ASSIGN A JOB STATUS RECORD IN THE HOUSEKEEPING
!                 DATA SET TO A STORAGE JOB.
!
! DESCRIPTION   : "FINDJSR" ASSIGNS A JOB STATUS RECORD TO A STORAGE
!                 JOB, LOCATING AN EXISTING ONE WITH THE CORRECT JOB
!                 NAME IF POSSIBLE, OTHERWISE ALLOCATING A FREE ONE.
!
!                 IF USING AN EXISTING ONE, A CHECK IS MADE TO SEE THAT
!                 ITS CONTENTS ARE STILL APPLICABLE TO THE PRESENT JOB:
!                 IF THEY AREN'T, A MESSAGE IS PRINTED AND THE JOB WILL
!                 TERMINATE.
!
!                 FOR THE FIRST CALL ONLY, THE LAST ARGUMENT IS USED TO
!                 INDICATE WHETHER THE JOB GETS DATA SET INFORMATION
!                 FROM THE HOUSEKEEPING DATA SET.
!
!                 TERMINATION WITH A MESSAGE ALSO OCCURS IF A FREE JOB
!                 STATUS RECORD IS REQUIRED BUT NONE IS AVAILABLE.
!
! USAGE         : CALL FINDJSR (JOBNAME, JOBREC, NJREC, STOPFLAG)
!
! ARGUMENTS     : (1) JOBNAME   I  JOB NAME OF CURRENT STORAGE JOB.
!                 (2) STOPFLAG I/O as INPUT: .TRUE. IF JOB IS TO GET DATA SET
!                                        NAMES FROM THE HOUSEKEEPING DATA SET;
!                                        OTHERWISE .FALSE. (E.G. SSMI, ATOVS).
!                                  as OUTPUT: FLAG TO REQUEST JOB TERMINATION.
!                 (3) LJREC     I  LENGTH FOR JOB STATUS RECORD
!                 (4) JOBREC    O  JOB STATUS RECORD FOR THIS JOB,
!                                  PARTIALLY COMPLETED BUT NOT YET WRITTEN
!                                  TO HOUSEKEEPING DATA SET.
!                 (5) NJREC     O  JOB STATUS RECORD ASSIGNED TO THIS JOB.
!                                  (= -1 IF NO JOB STATUS RECORD AVAILABLE)
!
! CALLED BY     : NEXTFILE.
!
! FILES USED    : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                 ON UNIT 1.
!
! CALLS         : DSINFO, SECSOUT, TELLOPS
!                 METDB_CREAD_DIR and METDB_CWRITE_DIR in MetDB_c_utils.c
!
! REVISION INFO :
!
!
! $Workfile: findjsr.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 22/03/2011 10:10:28$
!
! CHANGE RECORD :
!
! $Log:
!  6    MetDB_Refresh 1.5         22/03/2011 10:10:28    Sheila Needham  Remove
!        print statements
!  5    MetDB_Refresh 1.4         09/03/2011 13:16:13    Sheila Needham  Some
!       corrections found in testing
!  4    MetDB_Refresh 1.3         04/03/2011 08:43:05    Sheila Needham
!       Removed redundant read statement
!  3    MetDB_Refresh 1.2         03/03/2011 11:56:55    Sheila Needham
!       Changes for C I/O and internal reads
!  2    MetDB_Refresh 1.1         28/01/2011 08:23:02    Sheila Needham  Put
!       SECSOUT back in.
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DSINFO_MOD

 IMPLICIT NONE

! Interface Arguments

 CHARACTER (LEN=8), INTENT(IN)        :: JOBNAME   ! (A1) NAME OF CURRENT STORAGE JOB
 LOGICAL, INTENT(INOUT)               :: STOPFLAG  ! (A2) .TRUE. IF JOB TERMINATION IS REQUIRED
 INTEGER, INTENT(IN)                  :: LJREC     ! (A3) LENGTH FOR JOB STATUS RECORD
 CHARACTER (LEN=LJREC), INTENT(OUT)   :: JOBREC    ! (A4) JOB STATUS RECORD FROM H.K. DATA SET
 INTEGER, INTENT(OUT)                 :: NJREC     ! (A5) STATUS RECORD ASSIGNED TO THIS JOB

! Local Parameters

 INTEGER, PARAMETER  :: MAXTYPES=50   ! MAXIMUM NUMBER OF STORAGE DATA TYPES
 INTEGER, PARAMETER  :: LENREC=950    ! RECORD LENGTH OF HOUSEKEEPING DATA SET
 INTEGER, PARAMETER  :: NWAIT1=15     ! TIME (S) TO PAUSE
 INTEGER, PARAMETER  :: NWAIT2=10     ! TIME (S) TO PAUSE
 INTEGER, PARAMETER  :: IUNIT=1       ! HKDS FT number

! Local Variables

 INTEGER  :: I                  ! INTEGER VARIABLE FOR LOCAL USE
 INTEGER  :: IOFLAG             ! IO status flag (not tested)
 INTEGER  :: ITYPES             ! NO. OF DATA TYPES IN JOB STATUS RECORD
 INTEGER  :: JREC               ! LOOP VARIABLE FOR JOB STATUS RECORDS
 INTEGER  :: JREC1              ! )
 INTEGER  :: JREC2              ! ) RANGE OF JOB STATUS RECORDS FOR LOOPS
 INTEGER  :: JTYPE              ! LOOP VARIABLE FOR STORAGE DATA TYPES
 INTEGER  :: JTYPES             ! NO. OF STORAGE TYPES USED BY THIS JOB
 INTEGER  :: NCLEAR             ! ALL DATA SETS CLEARED UP TO HERE
 INTEGER  :: NDRECS             ! TOTAL NO. OF DATA SET STATUS RECORDS
 INTEGER  :: NJRECS             ! TOTAL NUMBER OF JOB STATUS RECORDS
 INTEGER  :: NTYPES             ! NUMBER OF STORAGE DATA TYPES USED
 INTEGER  :: NUMLAST(MAXTYPES)  ! STORAGE DATA TYPES IN JOB STATUS REC.
 INTEGER  :: NUMTYPE(MAXTYPES)  ! STORAGE DATA TYPES USED BY THIS JOB
 INTEGER  :: RECNO              ! Record number in HKDS
 INTEGER  :: INT4               ! Dummy variable used in transfer function

 LOGICAL  :: CHANGED            ! .TRUE. IF STATUS RECORD ISN'T RIGHT
 LOGICAL  :: LOOKING            ! FLAG USED FOR 'DO WHILE' LOOPS
 LOGICAL  :: OLDJOB             ! .TRUE. IF JOB STATUS RECORD EXISTS

 CHARACTER (LEN=1)       :: ZERO            ! HEX "00"
 CHARACTER (LEN=1)       :: ONE             ! HEX "01"
 CHARACTER (LEN=1)       :: FLAG            ! 'JOB IS RUNNING' FLAG
 CHARACTER (LEN=44)      :: FDSN            ! Arg6 DSINFO
 CHARACTER (LEN=8)       :: TYPES(MAXTYPES) ! LIST OF STORAGE TYPES IN RECORD 1
 CHARACTER (LEN=LENREC)  :: RECORD          ! RECORD OF HOUSEKEEPING DATA SET
 CHARACTER (LEN=4)       :: CH4             ! Dummy variable for transfer function

!=======================================================================
!  1. INITIALISE SOME VARIABLES AND WAIT UNTIL FLAG IS SET IN THE
!     HOUSEKEEPING DATA SET TO SHOW THAT THE MONITOR JOB IS RUNNING
!=======================================================================
!                                                       Initialisations
 ZERO = CHAR(0)      ! Hex "00"
 ONE  = CHAR(1)      ! Hex "01"
 LOOKING = .TRUE.
!                                         Ensure monitor job is running
 DO WHILE (LOOKING)
         RECNO = 2
         CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,RECNO,IOFLAG)
         FLAG=RECORD(1:1)
         IF (FLAG == ONE) THEN  ! Monitor job is running
            LOOKING = .FALSE.
         ELSE                   ! Not running - wait a bit
           CALL SECSOUT (NWAIT1)
         END IF
 END DO ! WHILE (LOOKING)

!=======================================================================
!  2. READ HOUSEKEEPING HEADER AND STATUS RECORDS AND SEARCH THROUGH
!     JOB STATUS RECORDS FOR RECORD CORRESPONDING TO THIS JOB
!=======================================================================
!                                                    From header record
 NCLEAR = TRANSFER(RECORD(17:20),INT4)

!                                                    Read status record
 RECNO = 1
 CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,RECNO,IOFLAG)

 NDRECS = TRANSFER(RECORD(25:28),INT4)
 NJRECS = TRANSFER(RECORD(29:32),INT4)
 NTYPES = TRANSFER(RECORD(41:44),INT4)
 READ (RECORD,'(T45,50A8)')(TYPES(JTYPE), JTYPE=1,NTYPES)
 IF (.NOT. STOPFLAG) NTYPES = 0 ! so it doesn't look for data types
!
!-----------------------------------------------------------------------
!  Search for existing job status record with current job name
!-----------------------------------------------------------------------
!                                                       Initialisations
 NJREC = 0
 JREC = NDRECS + 2
 JREC2 = JREC + NJRECS ! Last job status record
 OLDJOB = .FALSE.
 LOOKING = .TRUE.
!                                          Loop over job status records
DO_JOBREC: &
 DO WHILE (LOOKING)
         JREC = JREC + 1
         CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,JREC,IOFLAG)
         JOBREC(1:LJREC) = RECORD(1:LJREC)
!                                              Check job name in record
!
         IF (JOBREC(17:24) == JOBNAME) THEN ! Record found for this job
            NJREC = JREC
            OLDJOB = .TRUE.
            LOOKING = .FALSE.
!
         ELSE IF (JOBREC(17:17) == ZERO) THEN ! Unused job status record
            JREC1 = JREC
            LOOKING = .FALSE.
!
         ELSE IF (JREC >= JREC2) THEN ! Record not found and none free
            NJREC = -1
            LOOKING = .FALSE.
         END IF
 END DO DO_JOBREC    ! WHILE (LOOKING)
!
!=======================================================================
!  3. IF NO JOB STATUS RECORD FOR THIS JOB WAS FOUND, LOOK FOR A FREE
!     ONE AND CLAIM IT
!=======================================================================
! NB. Since there may be other storage jobs looking for a free
!     record at the same time it is possible that there may be some
!     contention between them so when a free record is found, the job
!     'claims' it by immediately writing its job name into it; then
!     after waiting for 10 seconds it re-reads the record and if its
!     job name is still there, the record is considered assigned to
!     this job. Otherwise the job will look for another record.
!-----------------------------------------------------------------------

IF_NOJOBREC: &
 IF (NJREC == 0) THEN
         JREC = JREC1
         LOOKING = .TRUE.
!                                          Loop over job status records
DO_JOBREC2: &
         DO WHILE (LOOKING)
            CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,JREC,IOFLAG)
            JOBREC(1:LJREC) = RECORD(1:LJREC)
            IF (JOBREC(17:17) == ZERO) THEN       ! it's free
               JOBREC(17:24) = JOBNAME
               RECORD(1:LJREC) = JOBREC(1:LJREC)
               CALL METDB_CWRITE_DIR(IUNIT,RECORD,LENREC,JREC,IOFLAG)
               CALL SECSOUT (NWAIT2)              ! wait a bit
                                                  ! re-read it
               CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,JREC,IOFLAG)
               JOBREC(1:LJREC) = RECORD(1:LJREC)
               IF (JOBREC(17:24) == JOBNAME) THEN ! successfully claimed
                  NJREC = JREC
                  LOOKING = .FALSE.
!
               ELSE IF (JREC >= JREC2) THEN       ! no free record left
                  NJREC = -1
                  LOOKING = .FALSE.
               END IF
            END IF
!                                               Increment record number
            JREC = JREC + 1
         END DO DO_JOBREC2    ! WHILE (LOOKING)
 END IF IF_NOJOBREC
!
!=======================================================================
!  4. DETERMINE WHICH STORAGE DATA SETS ARE USED BY THE PRESENT JOB. IF
!     USING A NEW JOB STATUS RECORD, WRITE THE DETAILS IN IT:  IF NOT,
!     CHECK THAT THE STORED DETAILS ARE STILL APPLICABLE. IN THE LATTER
!     CASE, THE JOB TERMINATES IF THE STORAGE ENVIRONMENT IS DIFFERENT.
!=======================================================================
!
IF_JOBREC: &
 IF (NJREC > 0) THEN
!                               Set first 16 bytes of job status record
!
         JOBREC(1:4) = ONE // ZERO // ZERO // ZERO ! flags
         STOPFLAG = .FALSE. ! don't want to terminate job
         JTYPES = 0
!                                           Loop over storage data sets
!                                    (JREC1/2 used for unwanted output)
!                                    (Replace FLAG as Arg 6)
         FDSN=FLAG
         DO JTYPE=1,NTYPES
            CALL DSINFO (TYPES(JTYPE), -1, JREC1, JREC, JREC2, FDSN)
!
!                                             Make a list of those used
            IF (JREC > 0) THEN ! it's open
               JTYPES = JTYPES + 1
               NUMTYPE(JTYPES) = JTYPE
            END IF
         END DO
!
!-----------------------------------------------------------------------
!  Check data type details if using old job status record
!-----------------------------------------------------------------------
!
IF_OLDJOB: &
         IF (OLDJOB) THEN
!                                     Check the number of storage types
!
            ITYPES = TRANSFER(JOBREC(25:28),INT4)
            CHANGED = ITYPES /= JTYPES
!                                             Get list of storage types
            DO JTYPE=1,ITYPES
               I = 25 + 4*JTYPE
               NUMLAST(JTYPE) = TRANSFER(JOBREC(I:I+3),INT4)
            END DO ! JTYPE
!                                     Check that the list still applies
            IF (.NOT.CHANGED) THEN
               DO JTYPE=1,JTYPES
                  IF (NUMTYPE(JTYPE) /= NUMLAST(JTYPE)) CHANGED =.TRUE.
               END DO ! JTYPE
            END IF
!
!-----------------------------------------------------------------------
!  If storage details have changed, print out a message and terminate
!-----------------------------------------------------------------------
!
IF_CHANGED: &
            IF (CHANGED) THEN
               WRITE (6,'(/T5,A,T15,4A)') 'FINDJSR:', 'LIST OF ',   &
                 'STORAGE TYPES FOR JOB ', JOBNAME, ' HAS CHANGED'
               WRITE (6,'(/T5,A,I4,A/(T8,8A9))')                    &
                 'OLD STORAGE LIST -', ITYPES, ' STORAGE TYPES',    &
                 (TYPES(NUMLAST(JTYPE)), JTYPE=1,ITYPES)
               WRITE (6,'(/T5,A,I4,A/(T8,8A9))')                    &
                 'NEW STORAGE LIST -', JTYPES, ' STORAGE TYPES',    &
                 (TYPES(NUMTYPE(JTYPE)), JTYPE=1,JTYPES)
               WRITE (6,'(/T5,A)')                                  &
                 'TRY RE-RUNNING THE MONITOR JOB WITH "CLEAR =.TRUE."'
!
!                     SEND MESSAGE TO OPERATOR AND SET TERMINATION FLAG
!
               RECORD = 'MDB(W):  ' // JOBNAME
               I = INDEX(RECORD(9:),' ')
               RECORD(9+I:) = '- DATA TYPE LIST HAS CHANGED.'
               CALL TELLOPS (RECORD(1:50))
               STOPFLAG = .TRUE.
           END IF IF_CHANGED
!
!-----------------------------------------------------------------------
!  Add details to job status record if using a new one
!-----------------------------------------------------------------------
!
         ELSE
            JOBREC(13:16) = TRANSFER(NCLEAR,CH4)
            JOBREC(25:28) = TRANSFER(JTYPES,CH4)
            DO JTYPE=1,JTYPES
              I = 25 + 4*JTYPE
              JOBREC(I:I+3) = TRANSFER(NUMTYPE(JTYPE),CH4)
            END DO ! JTYPE
         END IF IF_OLDJOB
!
!======================================================================
!  5. ARRANGE FOR JOB TERMINATION IF NO JOB STATUS RECORDS ARE AVAILABLE
!=======================================================================
!
 ELSE
         WRITE (6,'(T5,A,T15,2A)') 'FINDJSR:',                      &
                  'NO FREE JOB STATUS RECORD IS AVAILABLE FOR ',JOBNAME
!
!                     SEND MESSAGE TO OPERATOR AND SET TERMINATION FLAG
!
         RECORD = 'MDB(W):  NO FREE JOB STATUS RECORD FOR ' // JOBNAME
         I = INDEX(RECORD(40:),' ')
         RECORD(39+I:) = '.'
         CALL TELLOPS (RECORD(1:50))
         STOPFLAG = .TRUE.
 END IF IF_JOBREC
!                                            Return to calling program
 RETURN
 END SUBROUTINE FINDJSR
