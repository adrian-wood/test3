      SUBROUTINE FINDJSR (JOBNAME, STOPFLAG, JOBREC, NJREC)

      IMPLICIT NONE

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
! PARAMETERS    : JOBNAME   I  (C*8)  JOB NAME OF CURRENT STORAGE JOB.
!                 STOPFLAG I/O (INPUT) .TRUE. IF JOB IS TO GET DATA SET
!                                 NAMES FROM THE HOUSEKEEPING DATA SET;
!                                 OTHERWISE .FALSE. (E.G. SSMI, ATOVS).
!                              (OUTPUT) FLAG TO REQUEST JOB TERMINATION.
!                 JOBREC    O  (C*(*)) JOB STATUS RECORD FOR THIS JOB,
!                              PARTIALLY COMPLETED BUT NOT YET WRITTEN
!                              TO HOUSEKEEPING DATA SET.
!                 NJREC     O  JOB STATUS RECORD ASSIGNED TO THIS JOB.
!                              (= -1 IF NO JOB STATUS RECORD AVAILABLE)
!
! CALLED BY     : NEXTFILE.
!
! FILES USED    : MET.D.B. HOUSEKEEPING DATA SET MUST BE ALREADY OPEN
!                 ON UNIT 1.
!
! CALLS         : DSINFO, SECSOUT, TELLOPS.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:28$
! $Source: /home/us0400/mdb/op/lib/source/RCS/findjsr.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:28    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:21:14  usmdb
! Removed unused variables, added copyright and modified
! header - S.Cox
!
! Revision 1.1  2000/06/08  15:38:27  15:38:27  usmdb (Generic MetDB account)
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
      INTEGER MAXTYPES         ! MAXIMUM NUMBER OF STORAGE DATA TYPES
      INTEGER LENREC           ! RECORD LENGTH OF HOUSEKEEPING DATA SET
!
      PARAMETER (MAXTYPES=50, LENREC=950)
!                                                             Variables
!
      INTEGER I                 ! INTEGER VARIABLE FOR LOCAL USE
      INTEGER ITYPES            ! NO. OF DATA TYPES IN JOB STATUS RECORD
      INTEGER JREC              ! LOOP VARIABLE FOR JOB STATUS RECORDS
      INTEGER JREC1, JREC2      ! RANGE OF JOB STATUS RECORDS FOR LOOPS
      INTEGER JTYPE             ! LOOP VARIABLE FOR STORAGE DATA TYPES
      INTEGER JTYPES            ! NO. OF STORAGE TYPES USED BY THIS JOB
      INTEGER NCLEAR            ! ALL DATA SETS CLEARED UP TO HERE
!2.0deleteGER NDAY0             ! H.K. INITIALISATION CENTURY DAY
      INTEGER NDRECS            ! TOTAL NO. OF DATA SET STATUS RECORDS
      INTEGER NJREC             ! STATUS RECORD ASSIGNED TO THIS JOB
      INTEGER NJRECS            ! TOTAL NUMBER OF JOB STATUS RECORDS
!2.0deleteGER NSEC0             ! H.K. INITIALISATION SECONDS FROM 00Z
      INTEGER NTYPES            ! NUMBER OF STORAGE DATA TYPES USED
      INTEGER NUMLAST(MAXTYPES) ! STORAGE DATA TYPES IN JOB STATUS REC.
      INTEGER NUMTYPE(MAXTYPES) ! STORAGE DATA TYPES USED BY THIS JOB
!
      LOGICAL CHANGED           ! .TRUE. IF STATUS RECORD ISN'T RIGHT
      LOGICAL LOOKING           ! FLAG USED FOR 'DO WHILE' LOOPS
      LOGICAL OLDJOB            ! .TRUE. IF JOB STATUS RECORD EXISTS
      LOGICAL STOPFLAG          ! .TRUE. IF JOB TERMINATION IS REQUIRED
!
      CHARACTER ZERO, ONE         ! HEX "00" AND "01"
      CHARACTER FLAG              ! 'JOB RUNNING' FLAG
      CHARACTER*8 JOBNAME         ! NAME OF CURRENT STORAGE JOB
      CHARACTER*8 TYPES(MAXTYPES) ! LIST OF STORAGE TYPES IN RECORD 1
      CHARACTER*(LENREC) RECORD   ! RECORD OF HOUSEKEEPING DATA SET
      CHARACTER*(LENREC) JOBREC   ! JOB STATUS RECORD FROM H.K. DATA SET
      CHARACTER*132 HEAD          ! FOR REVISION DETAILS
!
!=======================================================================
!  1. INITIALISE SOME VARIABLES AND WAIT UNTIL FLAG IS SET IN THE
!     HOUSEKEEPING DATA SET TO SHOW THAT THE MONITOR JOB IS RUNNING
!=======================================================================
!                                                  Revision information
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/findjsr.F,v $
     &'//'$Date: 30/01/2006 20:22:28$ $Revision: 1$'
!                                                       Initialisations
      ZERO = CHAR(0)      ! Hex "00"
      ONE  = CHAR(1)      ! Hex "01"
      LOOKING = .TRUE.
!                                         Ensure monitor job is running
      DO WHILE (LOOKING)
         READ (1,REC=2) FLAG
         IF (FLAG.EQ.ONE) THEN  ! Monitor job is running
            LOOKING = .FALSE.
         ELSE                   ! Not running - wait a bit
            CALL SECSOUT (15)
         END IF
      END DO ! WHILE (LOOKING)
!
!=======================================================================
!  2. READ HOUSEKEEPING HEADER AND STATUS RECORDS AND SEARCH THROUGH
!     JOB STATUS RECORDS FOR RECORD CORRESPONDING TO THIS JOB
!=======================================================================
!                                                    Read header record
      READ (1,REC=2) RECORD(1:16), NCLEAR
!                                                    Read status record
      READ (1,REC=1) RECORD
      READ (RECORD,'(T25,2A4,8X,A4,50A8)') NDRECS, NJRECS, NTYPES,
     &              (TYPES(JTYPE), JTYPE=1,NTYPES)
      IF (.NOT.STOPFLAG) NTYPES = 0 ! so it doesn't look for data types
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
      DO WHILE (LOOKING)
         JREC = JREC + 1
         READ (1,REC=JREC) JOBREC
!                                              Check job name in record
!
         IF (JOBREC(17:24).EQ.JOBNAME) THEN ! Record found for this job
            NJREC = JREC
            OLDJOB = .TRUE.
            LOOKING = .FALSE.
!
         ELSE IF (JOBREC(17:17).EQ.ZERO) THEN ! Unused job status record
            JREC1 = JREC
            LOOKING = .FALSE.
!
         ELSE IF (JREC.GE.JREC2) THEN ! Record not found and none free
            NJREC = -1
            LOOKING = .FALSE.
         END IF
      END DO ! WHILE (LOOKING)
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

      IF (NJREC.EQ.0) THEN
         JREC = JREC1
         LOOKING = .TRUE.
!                                          Loop over job status records
         DO WHILE (LOOKING)
            READ (1,REC=JREC) JOBREC              ! read record
            IF (JOBREC(17:17).EQ.ZERO) THEN       ! it's free
               JOBREC(17:24) = JOBNAME
               WRITE (1,REC=JREC) JOBREC          ! claim it
               CALL SECSOUT (10)                  ! wait a bit
               READ (1,REC=JREC) JOBREC           ! re-read it
!
               IF (JOBREC(17:24).EQ.JOBNAME) THEN ! successfully claimed
                  NJREC = JREC
                  LOOKING = .FALSE.
!
               ELSE IF (JREC.GE.JREC2) THEN       ! no free record left
                  NJREC = -1
                  LOOKING = .FALSE.
               END IF
            END IF
!                                               Increment record number
            JREC = JREC + 1
         END DO ! WHILE (LOOKING)
      END IF
!
!=======================================================================
!  4. DETERMINE WHICH STORAGE DATA SETS ARE USED BY THE PRESENT JOB. IF
!     USING A NEW JOB STATUS RECORD, WRITE THE DETAILS IN IT:  IF NOT,
!     CHECK THAT THE STORED DETAILS ARE STILL APPLICABLE. IN THE LATTER
!     CASE, THE JOB TERMINATES IF THE STORAGE ENVIRONMENT IS DIFFERENT.
!=======================================================================
!
      IF (NJREC.GT.0) THEN
!                               Set first 16 bytes of job status record
!
         JOBREC(1:4) = ONE // ZERO // ZERO // ZERO ! flags
         STOPFLAG = .FALSE. ! don't want to terminate job
         JTYPES = 0
!                                           Loop over storage data sets
!                                    (JREC1/2 used for unwanted output)
         DO JTYPE=1,NTYPES
            CALL DSINFO (TYPES(JTYPE), -1, JREC1, JREC, JREC2, FLAG)
!
!                                             Make a list of those used
            IF (JREC.GT.0) THEN ! it's open
               JTYPES = JTYPES + 1
               NUMTYPE(JTYPES) = JTYPE
            END IF
         END DO
!
!-----------------------------------------------------------------------
!  Check data type details if using old job status record
!-----------------------------------------------------------------------
!
         IF (OLDJOB) THEN
!                                     Check the number of storage types
!
            READ (JOBREC,'(T25,A4)') ITYPES
            CHANGED = ITYPES.NE.JTYPES
!                                             Get list of storage types
            DO JTYPE=1,ITYPES
               I = 25 + 4*JTYPE
               READ (JOBREC(I:I+3),'(A4)') NUMLAST(JTYPE)
            END DO ! JTYPE
!                                     Check that the list still applies
            IF (.NOT.CHANGED) THEN
               DO JTYPE=1,JTYPES
                  IF (NUMTYPE(JTYPE).NE.NUMLAST(JTYPE)) CHANGED =.TRUE.
               END DO ! JTYPE
            END IF
!
!-----------------------------------------------------------------------
!  If storage details have changed, print out a message and terminate
!-----------------------------------------------------------------------
!
            IF (CHANGED) THEN
               WRITE (6,'(/T5,A,T15,4A)') 'FINDJSR:', 'LIST OF ',
     &           'STORAGE TYPES FOR JOB ', JOBNAME, ' HAS CHANGED'
               WRITE (6,'(/T5,A,I4,A/(T8,8A9))')
     &           'OLD STORAGE LIST -', ITYPES, ' STORAGE TYPES',
     &           (TYPES(NUMLAST(JTYPE)), JTYPE=1,ITYPES)
               WRITE (6,'(/T5,A,I4,A/(T8,8A9))')
     &           'NEW STORAGE LIST -', JTYPES, ' STORAGE TYPES',
     &           (TYPES(NUMTYPE(JTYPE)), JTYPE=1,JTYPES)
               WRITE (6,'(/T5,A)')
     &           'TRY RE-RUNNING THE MONITOR JOB WITH "CLEAR =.TRUE."'
!
!                     SEND MESSAGE TO OPERATOR AND SET TERMINATION FLAG
!
               RECORD = 'MDB(W):  ' // JOBNAME
               I = INDEX(RECORD(9:),' ')
               RECORD(9+I:) = '- DATA TYPE LIST HAS CHANGED.'
               CALL TELLOPS (RECORD(1:50))
               STOPFLAG = .TRUE.
           END IF
!
!-----------------------------------------------------------------------
!  Add details to job status record if using a new one
!-----------------------------------------------------------------------
!
         ELSE
            WRITE (JOBREC(13:16),'(A4)') NCLEAR
            I = 28 + 4*JTYPES
            WRITE (JOBREC(25:I),'(51A4)')
     &            JTYPES, (NUMTYPE(JTYPE), JTYPE=1,JTYPES)
         END IF
!
!=======================================================================
!  5. ARRANGE FOR JOB TERMINATION IF NO JOB STATUS RECORDS ARE AVAILABLE
!=======================================================================
!
      ELSE
         WRITE (6,'(T5,A,T15,2A)') 'FINDJSR:',
     &            'NO FREE JOB STATUS RECORD IS AVAILABLE FOR ',JOBNAME
!
!                     SEND MESSAGE TO OPERATOR AND SET TERMINATION FLAG
!
         RECORD = 'MDB(W):  NO FREE JOB STATUS RECORD FOR ' // JOBNAME
         I = INDEX(RECORD(40:),' ')                                 !2.0
         RECORD(39+I:) = '.'                                        !2.0
         CALL TELLOPS (RECORD(1:50))
         STOPFLAG = .TRUE.
      END IF
!                                            Return to callling program
      RETURN
      END
