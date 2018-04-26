PROGRAM DREGS

IMPLICIT NONE
!----------------------------------------------------------------------
!
! PROGRAM     : DREGS
!
! PURPOSE     : PROGRAM TO COLLECT DREGS MESSAGES FROM DATA SETS FOR
!               INDIVIDUAL STORAGE JOBS INTO THE MAIN DREGS DATA SET.
!
! DESCRIPTION : THE JOB READS JOB NAMES OUT OF THE HOUSEKEEPING DATA
!               SET (SPECIFIED ON AN "HKEEP" DD STATEMENT) AND LOOKS
!               FOR DREGS DATA SETS FOR EACH ONE (SOME JOBS MAY NOT
!               HAVE ONE). IT THEN COPIES ALL NEW DREGS MESSAGES TO
!               THE MAIN DREGS DATA SET (SPECIFIED ON A "DREGS" DD
!               STATEMENT) IN CHRONOLOGICAL ORDER. THE YEAR IS THEN
!               BLANKED OUT IN THE MESSAGE IN THE JOB'S DREGS DATA
!               SET TO INDICATE THAT THE MESSAGE HAS BEEN COPIED.
!
!               DREGS DATA SET NAMES FOR INDIVIDUAL STORAGE JOBS HAVE
!               THE FORM "hlq.slq.jjjjjjjj" WHERE "hlq" AND "slq" ARE
!               THE HIGH & SECOND LEVEL QUALIFIERS (SEE NAMELIST
!               BELOW) AND "jjjjjjjj" IS THE JOB NAME. DEFAULT NAMES
!               ARE OF THE FORM "MDB.DREGS.jjjjjjjj".
!
!               THE MET.D.B. STORAGE MONITOR JOB'S DREGS DATA SET
!               SHOULD BE SPECIFIED ON AN "FTPDREGS" DD STATEMENT.
!
! CALLS       : DYNALC
!
! NAMELIST    : DREGNAME  (UNIT 5).  CONTENTS AS FOLLOWS:
!
!               VARIABLE TYPE        DESCRIPTION             DEFAULT
!               -------- ----        -----------             -------
!                 HLQ    C*8    HIGH LEVEL QUALIFIER OF       'MDB'
!                               DREGS DATA SETS
!                 SLQ    C*16   SECOND LEVEL QUALIFIER OF    'DREGS'
!                               DREGS DATA SETS
!
!               NOTE: HLQ AND SLQ MUST BE LEFT JUSTIFIED IF SHORTER
!                     THAN THE FULL LENGTH OF THE STRING. EITHER CAN
!                     CONTAIN A DOT ('.') AS PART OF THE STRING.
!
! REVISION INFO:
!
! $Workfile: dregs.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 13/12/2010 12:17:06$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         13/12/2010 12:17:06    Richard Weedon
!       revision information updated
!  1    MetDB_Refresh 1.0         25/11/2010 12:12:17    Richard Weedon
!       Initial Version, passed test1
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

INTEGER,PARAMETER  ::  NJOBS=50     ! MAXIMUM NUMBER OF STORAGE JOBS
INTEGER,PARAMETER  ::  LENREC=656   ! RECORD LENGTH OF DREGS DATA SETS
!                                                             VARIABLES
INTEGER               ::  IOS    ! STATUS CODE I/O OR OPEN STATEMENT
INTEGER               ::  JOB    ! LOOP VARIABLE FOR STORAGE JOBS
INTEGER               ::  KOUNT  !  COUNTER FOR DREGS MESSAGES COPIED
INTEGER               ::  NJOB   ! JOB NUMBER (FOR LOCAL USE)
INTEGER               ::  LAST(0:NJOBS) ! LAST REC WRITTEN TO DREGS D/S
INTEGER               ::  LENDSN ! LENGTH OF DREGS DATA SET NAME
INTEGER               ::  LENGTH ! LENGTH OF TEXT STRING FOR I/O
INTEGER               ::  LENHLQ ! LEN OF COMPONENTS OF DREGS D.S. NAMES
INTEGER               ::  LENSLQ ! LEN OF COMPONENTS OF DREGS D.S. NAMES
INTEGER               ::  LATEST ! LAST REC. WRITTEN IN MAIN DREGS D/S
INTEGER               ::  MAXREC ! NUM OF RECORDS IN MAIN DREGS DATA SET
INTEGER               ::  MAXJOB ! HIGHEST JOB NO. OPEN DREGS DATA SET
INTEGER               ::  NDRECS ! NO.OF DATA SET STATUS RECORDS IN H.K.
INTEGER               ::  NEXT   ! NO. OF NEXT DREGS RECORD TO LOOK AT.
INTEGER               ::  NJRECS ! NO. OF JOB STATUS RECORDS IN H.K.
INTEGER               ::  NREC(0:NJOBS)    ! CUR REC NUM IN DREGS D/S
INTEGER               ::  NUMRECS(0:NJOBS) ! NO OF REC IN DREGS D/S
INTEGER               ::  NUNIT(0:NJOBS)   ! UNIT NO FOR DREGS D/S
INTEGER               ::  IUNIT  ! UNIT NO FOR CURRENT DREGS D/S

LOGICAL                 ::  LOOKING ! GENERAL FLAG FOR "DO WHILE" LOOPS
LOGICAL                 ::  NEWDREG ! FLAG FOR JOB'S DREGS D/S FOUND
LOGICAL                 ::  OPENED(0:NJOBS) ! FLAGS FOR OPEN DREGS D/S

CHARACTER(LEN=8)            ::   JOBNAME  ! NAME OF STORAGE JOB
CHARACTER(LEN=8)            ::   FILENAME ! FILE NAME FOR OPEN STATEMENT
CHARACTER(LEN=13)           ::   OLDEST   ! OLDEST MSG TIM
CHARACTER(LEN=13)           ::   YMDHL    ! TIME OF LAST DREG MESSAGE
CHARACTER(LEN=13)           ::   YMDHM(0:NJOBS) ! MESSAGE TIME
CHARACTER(LEN=13)           ::   YMDHN    ! TIME OF NEXT DREG MESSAGE
CHARACTER(LEN=44)           ::   DSN      ! DSNAME FOR 'DYNALC'
CHARACTER(LEN=48)           ::   DDNAME   ! DDNAME FOR 'DYNALC'
CHARACTER(LEN=LENREC)       ::   DREGMSG(0:NJOBS) ! NEXT DREGS MESSAGES
CHARACTER(LEN=16)           ::   C16*16    ! 16- AND 32-BYTE BUFFERS
CHARACTER(LEN=32)           ::   C32*32    ! 16- AND 32-BYTE BUFFERS
!CHARACTER HEAD*80                         ! REVISION INFORMATION
!    NAMELIST

CHARACTER(LEN=8)            ::   HLQ       ! HLQ = HIGH LEVEL QUALIFIER
CHARACTER(LEN=16)           ::   SLQ       ! SLQ = 2ND LEVEL QUALIFIER
NAMELIST /DREGNAME/ HLQ,    SLQ            ! SLQ = 2ND LEVEL QUALIFIER
DATA    HLQ,    SLQ &
     /'MDB', 'DREGS'/  ! DEFAULT IS 'MDB.DREGS...'
!  DATA
DATA OPENED/.FALSE., NJOBS*.FALSE./, MAXJOB/-1/, KOUNT/0/

!  REVISION INFORMATION
! HEAD = '$Workfile: dregs.f90$ ' //
!     &       '$Revision: 2$ $Date: 13/12/2010 12:17:06$'

!-----------------------------------------------------------------------
! 1.  OPEN HOUSEKEEPING AND MAIN DREGS DATA SETS ON UNITS 1 AND 2, AND
!     READ HEADER RECORDS IF NO I/O ERRORS.
!-----------------------------------------------------------------------

!                                                    READ NAMELIST DATA
READ (5, DREGNAME, END=1)
    1 LENHLQ = INDEX(HLQ,' ') - 1
LENSLQ = INDEX(SLQ,' ') - 1
IF (LENHLQ < 0) LENHLQ = LEN(HLQ)
IF (LENSLQ < 0) LENSLQ = LEN(SLQ)

!                                  OPEN HOUSEKEEPING DATA SET ON UNIT 1
LENGTH = 950
OPEN (1, FILE='HKEEP', STATUS='OLD', FORM='UNFORMATTED', &
          ACCESS='DIRECT', RECL=LENGTH, ACTION='READ', IOSTAT=IOS)
IF (IOS /= 0) THEN
   WRITE (6,'(/T8,A,I4)') &
     'ERROR OPENING HOUSEKEEPING DATA SET - "IOSTAT" =', IOS
   STOP
END IF
!                                    OPEN MAIN DREGS DATA SET ON UNIT 2

OPEN (2, FILE='DREGS', STATUS='OLD', FORM='FORMATTED', &
     ACCESS='DIRECT', RECL=LENREC, IOSTAT=IOS)
IF (IOS /= 0) THEN
   WRITE (6,'(/T8,A,I4)') &
     'ERROR OPENING MAIN DREGS DATA SET - "IOSTAT" =', IOS
   STOP
END IF
!                           READ HEADER RECORD OF HOUSEKEEPING DATA SET

LENGTH = MIN0(LENGTH,LENREC)
READ (1,REC=1) C32
READ (C32,'(T25,2A4)') NDRECS, NJRECS

!                                    READ HEADER OF MAIN DREGS DATA SET

READ (2,'(2I6)',REC=1) LATEST, MAXREC

!-----------------------------------------------------------------------
! 2.  LOOP OVER STORAGE JOBS, OPEN THE DREGS DATA SET FOR EACH JOB AND
!     READ ITS HEADER RECORD. UNIT NUMBERS START AT 10 AND INCREMENT.
!     "JOB"=0 CORRESPONDS TO THE MDB MONITOR JOB FOR WHICH A "FTPDREGS"
!     DD STATEMENT SHOULD BE SUPPLIED GIVING DETAILS OF ITS DREGS DATA
!     SET.  VALUES OF "JOB" GREATER THAN 0 ARE FOR STORAGE JOBS WHOSE
!     JOB NAMES ARE FOUND IN THE JOB STATUS RECORDS OF THE HOUSEKEEPING
!     DATA SET. THEIR DREGS DATA SETS SHOULD HAVE NAMES OF THE FORM
!     "hlq.slq.jjjjjjjj" WHERE "hlq" AND "slq" ARE THE HIGH & SECOND
!     LEVEL QUALIFIERS AND "jjjjjjjj" IS THE JOB NAME.
!-----------------------------------------------------------------------

IUNIT = 10
DO JOB=0,NJRECS
   NEWDREG = .FALSE.
!                               FOR MONITOR JOB, SET DDNAME AS FILENAME
   IFCONST1 : &
   IF (JOB == 0) THEN
      FILENAME = 'FTPDREGS'
      NEWDREG = .TRUE.
!                                 FOR STORAGE JOB, GET JOBNAME FROM JOB
!                                STATUS RECORD IN HOUSEKEEPING DATA SET
   ELSE IFCONST1
      NJOB = NDRECS + 2 + JOB ! (= JOB STATUS RECORD NUMBER)
      READ (1,REC=NJOB) C16, JOBNAME
!                                             SET DATA SET FROM JOBNAME
      IFCONST2 : &
      IF (JOBNAME >  '        ') THEN
         NEWDREG = .TRUE.
         IF (LENSLQ == 0) THEN
            DSN = HLQ(1:LENHLQ) // '.' // JOBNAME
         ELSE
            DSN = HLQ(1:LENHLQ) // '.' //&
                SLQ(1:LENSLQ) // '.' // JOBNAME
         END IF
!                                   CHECK WHETHER DREGS DATA SET EXISTS

         INQUIRE (FILE='/'//DSN, EXIST=NEWDREG, IOSTAT=IOS)
         IF (.NOT.NEWDREG) THEN
            WRITE (6,'(T5,2A)')&
             'NO DREGS DATA SET FOUND FOR JOB ', JOBNAME
         ELSE
!    DYNAMIC ALLOC. OF DREGS DATA SET
            FILENAME = JOBNAME
            DDNAME(41:48) = FILENAME
            LENDSN = INDEX(DSN,' ') - 1
            IF (LENDSN < 0) LENDSN = LEN(DSN)
            CALL DYNALC (DSN, LENDSN, 'CATALG', DDNAME)
         END IF
      END IF IFCONST2
   END IF IFCONST1
!         OPEN DREGS DATA SET FOR THIS STORAGE JOB
   IFCONST3 : &
   IF (NEWDREG) THEN
      OPEN (IUNIT, FILE=FILENAME, STATUS='OLD', FORM='FORMATTED',&
                 ACCESS='DIRECT', RECL=LENREC, IOSTAT=IOS)

!                                 CHECK RETURN CODE FROM OPEN STATEMENT

      IFCONST4 : &
      IF (IOS == 0) THEN        ! OPENED OK - READ HEADER
         READ (IUNIT,'(2I6)',REC=1) LAST(JOB), NUMRECS(JOB)

!                                       CLOSE IF "LAST" IS OUT OF RANGE

         IF (LAST(JOB) < 2 .OR. LAST(JOB) > NUMRECS(JOB)) THEN
            CLOSE (IUNIT)
         ELSE
            OPENED(JOB) = .TRUE.
            NUNIT(JOB) = IUNIT
            MAXJOB = JOB
!                                                         PRINT MESSAGE
            IF (FILENAME /= 'FTPDREGS') THEN
               WRITE (6,'(T5,2A)') &
              'DREGS DATA SET OPENED FOR ', JOBNAME
            ELSE
               WRITE (6,'(T5,A)') &
               'DREGS DATA SET OPENED FOR MONITOR JOB'
            END IF

            IUNIT = IUNIT + 1 ! (FOR NEXT TIME)
         END IF
!                                           OPEN FAILED - PRINT MESSAGE
      ELSE IFCONST4
         IF (JOB == 0) THEN
            WRITE (6,'(/T2,2A,I4)') 'ERROR OPENING DREGS DATA ',&
            'SET ON "FTPDREGS":  "IOSTAT" =', IOS
         ELSE
            WRITE (6,'(/T2,4A,I4)') 'ERROR OPENING DREGS DATA ',&
            'SET FOR ', JOBNAME, ' -  "IOSTAT" =', IOS
         END IF
      END IF IFCONST4
   END IF IFCONST3
END DO ! JOB
!                                           CLOSE HOUSEKEEPING DATA SET
CLOSE (1)

!-----------------------------------------------------------------------
! 3.  FIND THE OLDEST DREGS MESSAGE FOR EACH JOB
!-----------------------------------------------------------------------
!
NJOB = MAXJOB
MAXJOB = -1
!                                                        LOOP OVER JOBS
DO JOB=0,NJOB
   NEXT  = LAST(JOB)
   IUNIT = NUNIT(JOB)
   LOOKING = OPENED(JOB)
!          CHECK WHETHER THERE ARE ANY DREGS TO COPY
   IF (LOOKING) THEN
      READ (IUNIT,'(A13)',REC=NEXT) YMDHL
      IF (YMDHL(1:1) == ' ') THEN        ! No dregs to copy
         CLOSE (IUNIT)
         OPENED(JOB) = .FALSE.
         LOOKING = .FALSE.
      END IF
   END IF
!        LOOP BACK THROUGH DREGS RECORDS LOOKING
!        FOR THE OLDEST UNCOPIED MESSAGE
   DO WHILE (LOOKING)
      NREC(JOB) = NEXT    ! Oldest message found so far
      NEXT = NEXT - 1
      IF (NEXT < 2) NEXT = NUMRECS(JOB)
      READ (IUNIT,'(A)',REC=NEXT) YMDHN

!                                     GET THE TIME OF THE DREGS MESSAGE
!           (Stop at dreg already copied (year blanked out) or one that
!           has just come in (more recent time than previous record) or
!           if returned to start.)

      IF (YMDHN(1:1) == ' ' .OR.&         ! (already copied)
          YMDHN > YMDHL .OR. &            ! (just come in)
          NEXT == LAST(JOB)) THEN         ! (gone round once)
         READ (IUNIT,'(A)',REC=NREC(JOB)) DREGMSG(JOB)
         YMDHM(JOB) = DREGMSG(JOB)(1:13)  ! ('yyyymmdd hhmm')
         MAXJOB = JOB
         LOOKING = .FALSE.
      END IF
      YMDHL = YMDHN
   END DO
END DO ! JOB

!-----------------------------------------------------------------------
! 4.  COPY THE OLDEST DREGS MESSAGE TO THE MAIN DREGS DATA SET.
!-----------------------------------------------------------------------

DO WHILE (MAXJOB >= 0)
!                                         LOOK FOR OLDEST DREGS MESSAGE
   OLDEST = '9999999999999'
   DO JOB=0,MAXJOB
      IF (OPENED(JOB) .AND. YMDHM(JOB) < OLDEST) THEN
         OLDEST = YMDHM(JOB)
         NJOB = JOB
      END IF
   END DO
!                           COPY OLDEST MESSAGE TO MAIN DREGS DATA SET.
!        (NOTE:  MESSAGES FROM "STORCHEK" CAN OCCUR IN DREGS DATA SETS
!        FOR BOTH MONITOR AND STORAGE JOBS SO ONLY STORE THE ONE FROM
!        THE MONITOR JOB ("NJOB"=0).)

   IF (NJOB == 0 .OR. DREGMSG(NJOB)(22:29) /= 'STORCHEK') THEN
      LATEST = LATEST + 1
      IF (LATEST > MAXREC) LATEST = 2
      WRITE (2,'(A)',REC=LATEST) DREGMSG(NJOB)
      KOUNT = KOUNT + 1
   END IF
!                         BLANK OUT YEAR IN JOB'S DREGS DATA SET RECORD
!                      (THIS STOPS IT GETTING STORED AGAIN BY NEXT RUN)

   IUNIT = NUNIT(NJOB)
   DREGMSG(NJOB)(1:4) = '    '
   WRITE (IUNIT,'(A)',REC=NREC(NJOB)) DREGMSG(NJOB)

!                               CLOSE DATA SET IF NO MORE DREGS TO READ

   IFCONST5 : &
   IF (NREC(NJOB) == LAST(NJOB)) THEN
      CLOSE (IUNIT)
      OPENED(NJOB) = .FALSE.
!                                        UPDATE "MAXJOB" IF APPROPRIATE
      LOOKING = NJOB == MAXJOB
      DO WHILE (LOOKING)
         MAXJOB = MAXJOB - 1
         LOOKING = (MAXJOB >= 0) .AND. (.NOT.OPENED(MAXJOB))
      END DO
!                             FIND NEXT MESSAGE IN JOB'S DREGS DATA SET
   ELSE IFCONST5
      NEXT = NREC(NJOB) + 1
      IF (NEXT > NUMRECS(NJOB)) NEXT = 2   ! (REC. 1 IS A HEADER)
      READ (IUNIT,'(A)',REC=NEXT) DREGMSG(NJOB)
      YMDHM(NJOB) = DREGMSG(NJOB)(1:13)
      NREC(NJOB) = NEXT
   END IF IFCONST5
END DO
!                                  UPDATE HEADER OF MAIN DREGS DATA SET

READ (2,'(A)',REC=1) DREGMSG(0)
WRITE (DREGMSG(0)(1:6),'(I6)') LATEST
WRITE (2,'(A)',REC=1) DREGMSG(0)
CLOSE (2)
!                                                              FINISHED
WRITE (6,'(/T2,I7,A)') KOUNT, ' DREGS MESSAGES COPIED.'
STOP
END PROGRAM DREGS
