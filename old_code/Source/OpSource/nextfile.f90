SUBROUTINE NEXTFILE (DSN, NBULLS, STOPFLAG)

!-----------------------------------------------------------------------
!
! SUBROUTINE   : NEXTFILE
!
! PURPOSE      : TO INDICATE THE NEXT DATA SET FOR A MET.D.B. STORAGE
!                JOB TO LOOK AT.
!
! DESCRIPTION  : (A) WHEN CALLED BY A STORAGE JOB WHICH GETS MHS DATA
!                    SET NAMES FROM THE H.K. DATA SET, "NEXTFILE"
!
!                    1. RETURNS THE NAME OF THE NEXT DATA SET WITH
!                       DATA TO BE STORED BY THE JOB AND THE NUMBER
!                       OF BULLETINS TO STORE, WAITING IF NECESSARY
!                       UNTIL A DATA SET ARRIVES,
!
!                    2. UPDATES THE JOB STATUS RECORD FOR THE JOB,
!
!                    3. CHECKS THE FLAGS IN THE HOUSEKEEPING STATUS
!                       RECORD TO SEE IF TERMINATION OF STORAGE JOBS
!                       HAS BEEN REQUESTED.
!
!                (B) IF THE JOB IS NOT GETTING DATA SET NAMES FROM THE
!                    HOUSEKEEPING DATA SET, "NEXTFILE" JUST DOES (2)
!                    AND (3) ABOVE - IT DOES NOT RETURN ANY DATA SET
!                    NAMES OR WAIT FOR DATA TO ARRIVE.
!
!                WHEN FIRST CALLED, A JOB STATUS RECORD IS ASSIGNED BY
!                A CALL TO "FINDJSR": IN THIS CASE THE ARGUMENT LIST
!                IS USED TO INPUT INFORMATION (SEE BELOW).
!
! USAGE        : CALL NEXTFILE (DSN, NBULLS, STOPFLAG)
!
! ARGUMENTS    :  as INPUT - FIRST CALL ONLY:
!
!            (1) DSN       FIRST 8 CHARACTERS SHOULD CONTAIN JOB NAME.
!            (2) NBULLS    SECONDS TO WAIT WHEN SEARCHING FOR NEW DATA
!                          (DUMMY ARGUMENT FOR CASE (B) ABOVE).
!            (3) STOPFLAG  .TRUE. IF JOB WILL BE GETTING DATA SET
!                          NAMES FROM THE HOUSEKEEPING DATA SET.
!                          .FALSE. OTHERWISE (E.G. SSMI, ATOVS).
!
!                 as OUTPUT - ALL CALLS:
!
!            (1) DSN       (RETURNED ONLY FOR CASE (A) ABOVE) NAME OF
!                          NEXT DATA SET TO LOOK AT.
!            (2) NBULLS    (RETURNED ONLY FOR CASE (A) ABOVE) NUMBER
!                          OF BULLETINS TO STORE FROM NEXT DATA SET.
!            (3) STOPFLAG  FLAG TO INDICATE JOB TERMINATION.
!
! CALLED BY    : MET.D.B. STORAGE JOBS.
!
! CALLS        : DATE31, DATIM, FINDJSR, REPLYT, SECSOUT, TELLOPS
!                METDB_CREAD_DIR and METDB_CWRITE_DIR from MetDB_c_utils.c
!
! FILES USED   : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                ON UNIT 1.
!
! REVISION INFO :
!
! $Workfile: nextfile.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 13/05/2011 14:32:31$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         13/05/2011 14:32:31    Brian Barwell   Sata
!       set status record held in DSREC to keep it for next call.
!  4    MetDB_Refresh 1.3         04/05/2011 12:09:22    Sheila Needham
!       Correct format of internal read
!  3    MetDB_Refresh 1.2         04/03/2011 10:40:35    Sheila Needham
!       Replaced HKDS I/O with C routines; correct internal reads/writes.
!  2    MetDB_Refresh 1.1         28/01/2011 08:26:49    Sheila Needham
!       Reinstate secsout
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE FINDJSR_MOD
USE ZPDATE_MOD
USE DATIM_MOD

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT)  :: DSN       ! (A1) NAME OF NEXT DATA SET TO PROCESS
INTEGER, INTENT(INOUT)            :: NBULLS    ! (A2) WANTED BULLETINS IN NEXT DATA SET
LOGICAL, INTENT(INOUT)            :: STOPFLAG  ! (A3) .TRUE. IF JOB TERMINATION IS REQUIRED

! Local Parameters

INTEGER, PARAMETER  :: MAXTYPES=50         ! MAXIMUM NUMBER OF STORAGE DATA TYPE
INTEGER, PARAMETER  :: LENTRY=MAXTYPES+44  ! SLOT LENGTH IN DATA SET STATUS RECORD
INTEGER, PARAMETER  :: LENREC=950          ! LENGTH OF HOUSEKEEPING DATA SET
INTEGER, PARAMETER  :: IUNIT=1             ! FT number for HKDS

! Local Variables

INTEGER  :: I                 ! INTEGER VARIABLE FOR LOCAL USE
INTEGER  :: INT4              ! Dummy variable for TRANSFER function
INTEGER  :: IOS               ! I/O status
INTEGER  :: IPOS              ! Pointer to position in string
INTEGER  :: JCLEAR            ! DATA SETS TO HERE CLEARED BY THIS JOB
INTEGER  :: J                 ! Loop variable
INTEGER  :: JDS               ! LOOP VARIABLE FOR DATA SETS
INTEGER  :: JNEXT             ! NUMBER OF NEXT DATA SET TO LOOK AT
INTEGER  :: JTYPE             ! LOOP VARIABLE FOR STORAGE DATA TYPES
INTEGER  :: JTYPES            ! NO. OF STORAGE TYPES USED BY THIS JOB
INTEGER  :: LASTACC = -1      ! ACCESS COUNTER FOR LAST CALL
INTEGER  :: LASTDS  = 0       ! "LATEST" FROM PREVIOUSLY READ RECORD 2
INTEGER  :: LASTDT            ! LAST ACCESS TIME (FROM RECORD 2)
INTEGER  :: LASTREC = 0       ! LAST DATA SET STATUS RECORD READ
INTEGER  :: LATEST            ! NUMBER OF LAST DATA SET READ
INTEGER  :: LJREC             ! LENGTH PASSED FOR JOB STATUS RECORD
INTEGER  :: NACCESS           ! ACCESS COUNTER IN STATUS RECORD
INTEGER  :: NDATA             ! LOCATION OF DATA SET IN STATUS RECORD
INTEGER  :: NDAY              ! CURRENT CENTURY DAY
INTEGER  :: NDAY0             ! START CENTURY DAY
INTEGER  :: NDREC             ! NO. OF CURRENT DATA SET STATUS RECORD
INTEGER  :: NDRECS            ! TOTAL NO. OF DATA SET STATUS RECORDS
INTEGER  :: NDS               ! DATA SET NUMBER
INTEGER  :: NDT               ! TIME OF H.K. ACCESS FOR THIS CALL
INTEGER  :: NJREC             ! STATUS RECORD ASSIGNED TO THIS JOB
INTEGER  :: NJRECS            ! TOTAL NUMBER OF JOB STATUS RECORDS
INTEGER  :: NOW(8)            ! CURRENT DATE & TIME (FROM 'DATIM')
INTEGER  :: NSAME = 0         ! NO. OF CALLS WITHOUT CHANGE TO H.K.
INTEGER  :: NSEC              ! CURRENT SECONDS FROM 00Z
INTEGER  :: NSEC0             ! START SECONDS FROM 00Z
INTEGER  :: NTYPES            ! NO. OF STORAGE DATA TYPES IN H.K.
INTEGER  :: NUMDS             ! MAXIMUM NUMBER OF DATA SET SLOTS
INTEGER  :: NUMTYPE(MAXTYPES) ! STORAGE DATA TYPES USED BY THIS JOB
INTEGER  :: NWAIT             ! WAITING TIME (SECONDS)
INTEGER  :: RECNO             ! HKDS record number

LOGICAL  :: EXIT              ! .TRUE. TO TERMINATE WAIT FOR MORE DATA
LOGICAL  :: FIRST = .TRUE.    ! .TRUE. IF FIRST CALL TO SUBROUTINE

CHARACTER (LEN=1)         :: ZERO            ! HEX "00"
CHARACTER (LEN=1)         :: ONE             ! HEX "01"
CHARACTER (LEN=4)         :: FLAGS           ! STATUS FLAGS
CHARACTER (LEN=4)         :: CH4             ! Dummy variable for TRANSFER function
CHARACTER (LEN=8)         :: JOBNAME         ! NAME OF CURRENT STORAGE JOB
CHARACTER (LEN=8)         :: TYPES(MAXTYPES) ! LIST OF STORAGE TYPES IN RECORD 1
CHARACTER (LEN=MAXTYPES)  :: CONTENT         ! CONTENTS OF DATA SET
CHARACTER (LEN=LENREC)    :: RECORD          ! RECORD OF HOUSEKEEPING DATA SET
CHARACTER (LEN=LENREC)    :: JOBREC          ! JOB STATUS RECORD FROM H.K.
CHARACTER (LEN=LENREC)    :: DSREC           ! DATA SET STATUS RECORD FROM H.K.

SAVE ! Everything

! Initialize

EXIT = .FALSE.

!=======================================================================
!  1. (FIRST CALL ONLY) INITIALISE SOME VARABLES AND ASSIGN A JOB
!     STATUS RECORD TO THIS JOB
!=======================================================================

IF_FIRST: &
IF (FIRST) THEN ! first call
!                                                 Other initialisations
   NWAIT = NBULLS
   ZERO = CHAR(0)      ! Hex "00"
   ONE  = CHAR(1)      ! Hex "01"
!                                              Assign job status record
!             (Job name is in first 8 characters of DSN for first call)

   JOBNAME = DSN(1:8)
   LJREC = LENREC
   CALL FINDJSR (JOBNAME, STOPFLAG, LJREC, JOBREC, NJREC)

!                                                 Check for termination
IF_TERMCHK: &
   IF (.NOT.STOPFLAG) THEN
!                                       Read housekeeping header record
      RECNO = 1
      CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,RECNO,IOS)
      IPOS = 1
      DO J=8,3,-1
        NOW(J) = TRANSFER(RECORD(IPOS:IPOS+3),INT4)
        IPOS = IPOS+4
      END DO
      NDRECS = TRANSFER(RECORD(25:28),INT4)
      NJRECS = TRANSFER(RECORD(29:32),INT4)
      NTYPES = TRANSFER(RECORD(41:44),INT4)
      READ(RECORD(45:),'(50A8)')(TYPES(JTYPE),JTYPE=1,NTYPES)

!                                                        Get start time
      CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY0)
      NSEC0 = (NOW(5)*60 + NOW(4))*60 + NOW(3)
!                                                 No. of data set slots
      NUMDS = 10*NDRECS
!                                                Read job status record
      JCLEAR = TRANSFER(JOBREC(13:16),INT4)
      JOBNAME = JOBREC(17:24)
      JTYPES = TRANSFER(JOBREC(25:28),INT4)
      IPOS = 29
      DO J=1,JTYPES
        NUMTYPE(J) = TRANSFER(JOBREC(IPOS:IPOS+3),INT4)
        IPOS = IPOS + 4
      END DO
!                                         Get next data set to look at.
!              (Set to large number if not getting data sets names from
!            H.K. so that data set status records don't get looked at.)

      IF (JTYPES > 0) THEN
         JNEXT = JCLEAR + 1
      ELSE
         JNEXT = 99999999
      END IF
   END IF IF_TERMCHK

   EXIT = STOPFLAG
   FIRST = .FALSE.
END IF IF_FIRST

!=======================================================================
!  2. MAIN LOOP FOR LOCATION OF NEXT DATA SET TO LOOK AT.
!     LOOP CONTINUES UNTIL EITHER A DATA SET IS FOUND CONTAINING DATA
!     TO BE STORED BY THIS JOB, OR JOB TERMINATION IS REQUESTED.
!=======================================================================
!                                           Loop until exit flag is set
DO_LOOKDS: &
DO WHILE (.NOT.EXIT)
   JCLEAR = JNEXT - 1
   RECNO = 2
   CALL METDB_CREAD_DIR(IUNIT,RECORD,LENREC,RECNO,IOS)
   FLAGS = RECORD(1:4)
   LASTDT  = TRANSFER(RECORD(5:8),INT4)
   NACCESS = TRANSFER(RECORD(9:12),INT4)
   LATEST  = TRANSFER(RECORD(13:16),INT4)

!-----------------------------------------------------------------------
!  If flag set for immediate termination, set STOPFLAG to exit loop now
!-----------------------------------------------------------------------

IF_DSPROC: &
   IF (FLAGS(2:2) /= ZERO) THEN
      STOPFLAG = .TRUE.

!-----------------------------------------------------------------------
!  If there are data sets not yet processed, look for wanted bulletins
!-----------------------------------------------------------------------

   ELSE IF (JNEXT <= LATEST) THEN
!                                       Loop over unprocessed data sets
DO_NEXTDS: &
      DO JDS=JNEXT,LATEST
         NDREC = MOD(JDS,NUMDS)/10 + 3
         NDATA = MOD(JDS,10) + 1
!                                      Read data set status information
!                                          if not already held in store

         IF (NDREC /= LASTREC .OR. JDS > LASTDS) THEN
            CALL METDB_CREAD_DIR(IUNIT,DSREC,LENREC,NDREC,IOS)
            LASTREC = NDREC
            LASTDS = LATEST
         END IF
!                                      Extract data set name and number
!                                               and details of contents

         I = (NDATA-1)*LENTRY + 11    ! start of d.s. info.
         NDS = TRANSFER(DSREC(I:I+3),INT4)
         DSN = DSREC(I+4:I+43)
         CONTENT = DSREC(I+44:I+93)

IF_DSMATCH: &
         IF (NDS == JDS) THEN
!                                            See how many bulletins are
!                                           to be processed by this job
            NBULLS = 0
            DO JTYPE=1,JTYPES
               I = NUMTYPE(JTYPE)
               NBULLS = NBULLS + ICHAR(CONTENT(I:I))
            END DO
!                                              If >0, jump out of loops
            IF (NBULLS > 0) THEN
               JNEXT = JDS + 1
               EXIT = .TRUE.
               GO TO 3
            END IF
         END IF IF_DSMATCH
         JCLEAR = JDS
      END DO DO_NEXTDS
      JNEXT = LATEST + 1

!-----------------------------------------------------------------------
!  If no more data sets are waiting to be processed and flag is set for
!  termination when no data is waiting, set STOPFLAG to exit loop now
!-----------------------------------------------------------------------

   ELSE IF (FLAGS(3:3) /= ZERO) THEN
      STOPFLAG = .TRUE.

!-----------------------------------------------------------------------
!  If no more data sets are waiting to be processed and no termination
!  flags are set, wait for NWAIT seconds
!-----------------------------------------------------------------------

   ELSE IF (JTYPES > 0) THEN
     CALL SECSOUT (NWAIT)

!-----------------------------------------------------------------------
!  If not getting data set names from the housekeeping data set, set
!  flag to jump out of loop this time
!-----------------------------------------------------------------------

   ELSE
      EXIT = .TRUE.
   END IF IF_DSPROC

!-----------------------------------------------------------------------
!  Get the current time and check that the monitor job is still running
!-----------------------------------------------------------------------

 3 CONTINUE
   CALL DATIM (NOW)
   CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
   NSEC = (NOW(5)*60 + NOW(4))*60 + NOW(3)
   NDT = 86400*(NDAY-NDAY0) + (NSEC-NSEC0)

!                           Update last access number if it has changed

   IF (NACCESS /= LASTACC) THEN ! it has changed
      NSAME = 0
      LASTACC = NACCESS
   ELSE                         ! it hasn't changed
      NSAME = NSAME + 1
   END IF
!              Warning message if record 2 is unchanged for >10 minutes
!              after looking at least 5 times. (Not done if there are
!              still data sets to process. Also not done if any termin-
!              ation flag is set as record 2 doesn't get updated then.)
IF_TSTWARN: &
   IF (NSAME >= 5 .AND. JNEXT > LATEST .AND.    &
       FLAGS(2:3) == ZERO//ZERO) THEN
      IF (NDT-LASTDT > 600) THEN
         WRITE (6,'(T5,A,T15,3A)') 'NEXTFILE:', 'WARNING FROM ', &
          JOBNAME, ' - HAS MDB MONITOR JOB STOPPED?  PLEASE CHECK.'

!                                        Send error message to operator

         CALL TELLOPS('MDB(E):  HAS MDBFTP STOPPED?  PLEASE CHECK.')
      END IF
   END IF IF_TSTWARN

!-----------------------------------------------------------------------
!  If job termination has been requested, set flag to stop processing
!-----------------------------------------------------------------------

   CALL REPLYT (FLAGS)
   IF (FLAGS(1:1) /= ' ') STOPFLAG = .TRUE.

!-----------------------------------------------------------------------
!  If terminating, set flags in JSR and arrange to exit loop this time
!-----------------------------------------------------------------------

   IF (STOPFLAG) THEN ! job termination requested
      EXIT = .TRUE.
      JOBREC(1:4) = ZERO // ONE // ZERO // ZERO
   END IF

!-----------------------------------------------------------------------
!  Update and output new job status record
!-----------------------------------------------------------------------

   JOBREC(5:8)   = TRANSFER(NDT,CH4)
   JOBREC(9:12)  = TRANSFER(NACCESS,CH4)
   JOBREC(13:16) = TRANSFER(JCLEAR,CH4)
   CALL METDB_CWRITE_DIR(IUNIT,JOBREC,LENREC,NJREC,IOS)
END DO DO_LOOKDS  ! WHILE (.NOT.EXIT)
!                                  Return missing values if terminating
!                                          & using d.s. names from H.K.
IF (STOPFLAG .AND. JTYPES > 0) THEN
   DSN = ' '
   NBULLS = 0
END IF
!                                             Return to calling program
RETURN
END SUBROUTINE NEXTFILE
