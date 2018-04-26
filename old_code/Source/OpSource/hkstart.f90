SUBROUTINE HKSTART (CLEAR, STORTYPE, NSTORE, RECORD, NDAY0, NSEC0)

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
! ARGUMENTS    : CLEAR    I  SET .TRUE. TO RECONSTRUCT ENTIRE HOUSE-
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
! CALLS        : DATE31, DATIM, SYSABN, TELLOPS
!                METDB_CREAD_DIR and METDB_CWRITE_DIR in MetDB_c_utils.c
!
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                ON UNIT 1.
!
! REVISION INFO :
!
! $Workfile: hkstart.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 18/01/2013 10:50:39$
!
! CHANGE RECORD :
!
! $Log:
!  5    Met_DB_Project 1.4         18/01/2013 10:50:39    Brian Barwell   Fix
!       to allow MONITOR to work with CLEAR=.TRUE.
!  4    MetDB_Refresh 1.3         03/03/2011 11:56:55    Sheila Needham
!       Changes for C I/O and internal reads
!  3    MetDB_Refresh 1.2         18/02/2011 14:35:41    John Norton     Rework
!        done as listed in review document MONITORBatches1&2.doc
!  2    MetDB_Refresh 1.1         14/02/2011 14:29:03    John Norton     After
!       porting for Monitor batches 1 & 2
!  1    MetDB_Refresh 1.0         07/02/2011 11:28:26    John Norton     f77
!       version of MONITOR porting batch 
! $
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

USE datim_mod
!USE sysabn_mod
!USE tellops_mod
USE zpdate_mod

IMPLICIT NONE

! Subroutine arguments:

LOGICAL,          INTENT(IN)    :: CLEAR            !a01 FLAG FOR TOTAL CLEAROUT OF H.K.
INTEGER,          INTENT(IN)    :: NSTORE           !a03 NUMBER OF STORAGE TYPES REQUIRED
CHARACTER(LEN=8), INTENT(IN)    :: STORTYPE(NSTORE) !a02 ARRAY OF STORAGE TYPES REQUIRED
CHARACTER(LEN=*), INTENT(OUT)   :: RECORD(:)        !a04 STARTING INFORMATION ON DATA SETS
INTEGER,          INTENT(OUT)   :: NDAY0            !a05 START CENTURY DAY
INTEGER,          INTENT(OUT)   :: NSEC0            !a06 START SECONDS FROM 00Z

! Local declarations:
!                                                            Parameters

INTEGER,     PARAMETER ::  MAXDATA=160      ! MAX. NO. OF DATA SET STATUS RECORDS
INTEGER,     PARAMETER ::  MAXJOBS=50       ! MAXIMUM NUMBER OF JOB STATUS RECORDS
INTEGER,     PARAMETER ::  MAXSTOR=50       ! MAXIMUM NUMBER OF STORAGE TYPES
INTEGER,     PARAMETER ::  IUNIT=1          ! HKDS FT number

!                                                             Variables
INTEGER          ::  I       ! VARIABLE FOR LOCAL USE
INTEGER          ::  INT4    ! Dummy variable for transfer function
INTEGER          ::  IOFLAG  ! I/O status (not tested)
INTEGER          ::  IPOS    ! Pointer to position in string
INTEGER          ::  ISTORE  ! NUMBER OF STORAGE TYPES IN H.K.
INTEGER          ::  J       ! VARIABLE FOR LOCAL USE
INTEGER          ::  LATEST  ! NUMBER OF LAST DATA SET READ
INTEGER          ::  LENREC  ! RECORD LENGTH OF H.K. DATA SET
INTEGER          ::  NACCESS ! ACCESS COUNTER IN STATUS RECORD
INTEGER          ::  NCLEAR  ! ALL DATA SETS CLEARED UP TO HERE
INTEGER          ::  NDAY    ! CURRENT DAY
INTEGER          ::  NOW(8)  ! CURRENT DATE & TIME (FROM 'DATIM')
INTEGER          ::  NSEC    ! CURRENT SECONDS FROM 00Z
INTEGER          ::  NSECS   ! NO. OF SECONDS FROM START OF STORAGE
INTEGER          ::  NUMREC  ! SIZE OF "RECORD" ARRAY
INTEGER          ::  RECNO   ! Record number

LOGICAL          ::  CHANGED ! FLAG SET IF ENVIRONMENT HAS CHANGED

CHARACTER(LEN=1) ::  ONE ! ONE-BYTE CHARACTER '01'
CHARACTER(LEN=1) ::  ZERO ! ONE-BYTE CHARACTER '00'
CHARACTER(LEN=4) ::  CH4  ! Dummy variable for transfer function

!-----------------------------------------------------------------------

!                                                       Initialisations
ZERO = CHAR(0)
ONE  = CHAR(1)
NUMREC = SIZE(RECORD,1)
LENREC = LEN(RECORD(1))                                              !5

!-----------------------------------------------------------------------
!  Get the current date and time
!-----------------------------------------------------------------------

CALL DATIM (NOW)
CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
NSEC = (NOW(5)*60 + NOW(4))*60 + NOW(3)

!=======================================================================
!  1. CLEAR OUT WHOLE HOUSEKEEPING DATA SET IF REQUIRED
!     -------------------------------------------------
!     This is normally done if:
!       (1) a clearout was specifically requested ("CLEAR" set .TRUE.
!           in user's namelist input), or
!       (2) all old data has been processed by storage jobs ("CLEAR"
!           set by default in calling program).
!=======================================================================

IFLABEL1: &
IF (CLEAR) THEN

!-----------------------------------------------------------------------
!  Fill RECORD array with binary zeroes and write to data set
!-----------------------------------------------------------------------
!                                         Set record 1 to binary zeroes
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
      RECNO = J
      CALL METDB_CWRITE_DIR(IUNIT,RECORD(1),LENREC,RECNO,IOFLAG)
   END DO ! J
!                                             Start time = current time
   NDAY0 = NDAY
   NSEC0 = NSEC

!-----------------------------------------------------------------------
!  Create new record 1 with appropriate data and write out
!-----------------------------------------------------------------------
!                                                   Initialise record 1
   RECORD(1) = ' '
!                                                     set integer words
   I = 0
   IPOS=1
   DO J=8,3,-1
     RECORD(1)(IPOS:IPOS+3) = TRANSFER(NOW(J),CH4)
     IPOS=IPOS+4
   END DO
   RECORD(1)(25:28) = TRANSFER(MAXDATA,CH4)
   RECORD(1)(29:32) = TRANSFER(MAXJOBS,CH4)
   RECORD(1)(33:36) = TRANSFER(MAXSTOR,CH4)
   RECORD(1)(37:40) = TRANSFER(I,CH4)
   RECORD(1)(41:44) = TRANSFER(NSTORE,CH4)
!                                                Set storage data types
   I = 45
   DO J=1,NSTORE
      RECORD(1)(I:I+7) = STORTYPE(J)
      I = I + 8
   END DO ! J
!                                                    Write new record 1
   RECNO = 1
   CALL METDB_CWRITE_DIR(IUNIT,RECORD(1),LENREC,RECNO,IOFLAG)

!-----------------------------------------------------------------------
!  Initialise data for record 2 (will get written out at end)
!-----------------------------------------------------------------------

   NSECS  = 0
   NACCESS = 0
   LATEST = 0
   NCLEAR = 0

!=======================================================================
!  2. IF PICKING UP WHERE LEFT OFF, READ HEADER AND DATA SET RECORDS
!     AND CHECK THAT STORAGE DETAILS HAVEN'T CHANGED
!=======================================================================

ELSE
!                                                    Read records 1 & 2
   DO J=1,2
     CALL METDB_CREAD_DIR(IUNIT,RECORD(J),LENREC,J,IOFLAG)
   END DO

!-----------------------------------------------------------------------
!  See if storage details have changed from last run
!-----------------------------------------------------------------------
!                                      Read old number of storage types
   CHANGED = .FALSE.
   ISTORE = TRANSFER(RECORD(1)(41:44),INT4)
!                                                   Check with required
!                                                   number for this job
   IF (ISTORE /= NSTORE) THEN
      CHANGED = .TRUE.
   ELSE
!                          Check old list of storage types and set flag
!                           if different from requirements for this job
      I = 45
      DO J=1,NSTORE
         IF (RECORD(1)(I:I+7) /= STORTYPE(J)) CHANGED = .TRUE.
         I = I + 8
      END DO ! J
   END IF

!-----------------------------------------------------------------------
!  If storage details have changed, print out a message and terminate
!-----------------------------------------------------------------------

IFLABEL2: &
   IF (CHANGED) THEN
      WRITE (6,'(/T5,A)') &
        'JOB TERMINATING BECAUSE STORAGE ENVIRONMENT HAS CHANGED'
      WRITE (6,'(/T5,A,I4,A/(T8,8A9))') &
        'OLD STORAGE LIST -', ISTORE, ' STORAGE TYPES', &
        (RECORD(1)(8*I+37:8*I+44), I=1,ISTORE)
      WRITE (6,'( T5,A,I4,A/(T8,8A9))') &
        'NEW STORAGE LIST -', NSTORE, ' STORAGE TYPES', &
        (STORTYPE(I), I=1,NSTORE)
      WRITE (6,'(/T5,A//T5,A/)') &
        'TRY RE-RUNNING THE JOB WITH "CLEAR =.TRUE."', &
        'JOB WILL NOW TERMINATE WITH USER RETURN CODE 701'

!                                    SEND MESSAGE TO OPERATOR AND ABEND
      CALL TELLOPS &
         ('MDB(E):  MDBFTP - STORAGE ENVIRONMENT HAS CHANGED.')
      CALL SYSABN (701)
   END IF IFLABEL2

!-----------------------------------------------------------------------
!  Read data set status records into storage
!-----------------------------------------------------------------------

   I = TRANSFER(RECORD(1)(25:28),INT4)   ! No. of data set status records
   DO J=3,I+2
      CALL METDB_CREAD_DIR( IUNIT,RECORD(J),LENREC,J,IOFLAG)
   END DO ! J

!-----------------------------------------------------------------------
!  Get start day and second numbers (returned to calling program)
!-----------------------------------------------------------------------

   IPOS=1
   DO J=8,3,-1
     NOW(J)=TRANSFER(RECORD(1)(IPOS:IPOS+3),INT4)
     IPOS=IPOS+4
   END DO

   CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY0)
   NSEC0 = (NOW(5)*60 + NOW(4))*60 + NOW(3)
   NSECS = 86400*(NDAY-NDAY0) + (NSEC-NSEC0)

!-----------------------------------------------------------------------
!  Read data from record 2 (updated version is written out at end)
!-----------------------------------------------------------------------

   NACCESS = TRANSFER(RECORD(2)(9:12),INT4)
   LATEST = TRANSFER(RECORD(2)(13:16),INT4)
   NCLEAR = TRANSFER(RECORD(2)(17:20),INT4)
END IF IFLABEL1

!=======================================================================
!  3. COMPLETE HOUSEKEEPING STATUS RECORD AND WRITE TO RECORD 2.
!=======================================================================

!                   Increment access number and set 'job running' flag.
!                            (The latter allows storage jobs to start.)
NACCESS = NACCESS + 1
RECORD(2)(1:4) = ONE//ZERO//ZERO//ZERO
RECORD(2)(5:8)   = TRANSFER(NSECS,CH4)
RECORD(2)(9:12)  = TRANSFER(NACCESS,CH4)
RECORD(2)(13:16) = TRANSFER(LATEST,CH4)
RECORD(2)(17:20) = TRANSFER(NCLEAR,CH4)
RECNO = 2
CALL METDB_CWRITE_DIR(IUNIT,RECORD(2),LENREC,RECNO,IOFLAG)

!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE HKSTART
