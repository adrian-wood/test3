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
! CALLS       : DYNALC                                              !1.2
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
! REVISION INFO :
!
! $Revision: 2$ $Author: Brian Barwell$
! $Date: 08/09/2006 16:13:53$
! $Folder: pre_refresh$ $Workfile: dregs.f$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         08/09/2006 16:13:53    Brian Barwell   Stop
!       occasional spurious copying of old data.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:07    Sheila Needham  
! $
! Revision 2.0  2001/05/31 13:27:42  usmdb
! Added copyright - S.Cox
!
! Revision 1.2  2000/09/06  10:50:53  10:50:53  usmdb (Generic MetDB account)
! 18 September 2000:  Version 1.2,  Brian Barwell.
! Dregs data sets opened by DDname to ensure shared access.
!
! Revision 1.1  2000/08/09  15:07:47  15:07:47  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL, 28 JULY 2000.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2006 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      INTEGER NJOBS          ! MAXIMUM NUMBER OF STORAGE JOBS
      INTEGER LENREC         ! RECORD LENGTH OF DREGS DATA SETS
      PARAMETER (NJOBS=50, LENREC=656)
!                                                             VARIABLES

      INTEGER IOS            ! STATUS CODE FROM I/O OR OPEN STATEMENT
      INTEGER JOB            ! LOOP VARIABLE FOR STORAGE JOBS
      INTEGER KOUNT          ! COUNTER FOR DREGS MESSAGES COPIED
      INTEGER NJOB           ! JOB NUMBER (FOR LOCAL USE)
      INTEGER LAST(0:NJOBS)  ! LAST RECORD WRITTEN TO IN DREGS DATA SET
      INTEGER*2 LENDSN       ! LENGTH OF DREGS DATA SET NAME        !1.2
      INTEGER LENGTH         ! LENGTH OF TEXT STRING FOR I/O
      INTEGER LENHLQ, LENSLQ ! LENGTHS OF COMPONENTS OF DREGS D.S. NAMES
      INTEGER LATEST         ! LAST REC. WRITTEN IN MAIN DREGS DATA SET
      INTEGER MAXREC         ! NUMBER OF RECORDS IN MAIN DREGS DATA SET
      INTEGER MAXJOB         ! HIGHEST JOB NO. WITH OPEN DREGS DATA SET
      INTEGER NDRECS         ! NO. OF DATA SET STATUS RECORDS IN H.K.
      INTEGER NEXT           ! NO. OF NEXT DREGS RECORD TO LOOK AT.
      INTEGER NJRECS         ! NO. OF JOB STATUS RECORDS IN H.K.
      INTEGER NREC(0:NJOBS)  ! CURRENT RECORD NUMBER IN DREGS DATA SET
      INTEGER NUMRECS(0:NJOBS) ! NUMBER OF RECORDS IN DREGS DATA SETS
      INTEGER NUNIT(0:NJOBS) ! UNIT NUMBERS FOR DREGS DATA SETS
      INTEGER IUNIT          ! UNIT NUMBER FOR CURRENT DREGS DATA SET

      LOGICAL LOOKING         ! GENERAL FLAG FOR "DO WHILE" LOOPS
      LOGICAL NEWDREG         ! FLAG FOR JOB'S DREGS DATA SET FOUND
      LOGICAL OPENED(0:NJOBS) ! FLAGS FOR OPEN DREGS DATA SETS

      CHARACTER*8 JOBNAME         ! NAME OF STORAGE JOB
      CHARACTER*8 FILENAME        ! FILE NAME FOR OPEN STATEMENT    !1.2
      CHARACTER*13 OLDEST         ! OLDEST MSG TIME ("YYYYMMDD HHMM")
      CHARACTER*13 YMDHL          ! TIME OF LAST DREG MESSAGE         !2
      CHARACTER*13 YMDHM(0:NJOBS) ! MESSAGE TIME ("YYYYMMDD HHMM")
      CHARACTER*13 YMDHN          ! TIME OF NEXT DREG MESSAGE         !2
      CHARACTER*44 DSN            ! DSNAME FOR 'DYNALC'             !1.2
      CHARACTER*48 DDNAME         ! DDNAME FOR 'DYNALC'             !1.2
      CHARACTER*(LENREC) DREGMSG(0:NJOBS) ! NEXT DREGS MESSAGES
      CHARACTER C16*16, C32*32    ! 16- AND 32-BYTE BUFFERS
      CHARACTER HEAD*80           ! REVISION INFORMATION              !2
!                                                              NAMELIST

      CHARACTER           HLQ*8,  SLQ*16  ! HLQ = HIGH LEVEL QUALIFIER
      NAMELIST /DREGNAME/ HLQ,    SLQ     ! SLQ = 2ND LEVEL QUALIFIER
      DATA                HLQ,    SLQ
     &                  /'MDB', 'DREGS'/  ! DEFAULT IS 'MDB.DREGS...'
!                                                                  DATA
      DATA OPENED/.FALSE., NJOBS*.FALSE./, MAXJOB/-1/, KOUNT/0/

!                                                  REVISION INFORMATION
      HEAD = '$Workfile: dregs.f$ ' //
     &       '$Revision: 2$ $Date: 08/09/2006 16:13:53$'

!-----------------------------------------------------------------------
! 1.  OPEN HOUSEKEEPING AND MAIN DREGS DATA SETS ON UNITS 1 AND 2, AND
!     READ HEADER RECORDS IF NO I/O ERRORS.
!-----------------------------------------------------------------------

!                                                    READ NAMELIST DATA
      READ (5, DREGNAME, END=1)
    1 LENHLQ = INDEX(HLQ,' ') - 1
      LENSLQ = INDEX(SLQ,' ') - 1
      IF (LENHLQ.LT.0) LENHLQ = LEN(HLQ)
      IF (LENSLQ.LT.0) LENSLQ = LEN(SLQ)

!                                  OPEN HOUSEKEEPING DATA SET ON UNIT 1
      LENGTH = 950
      OPEN (1, FILE='HKEEP', STATUS='OLD', FORM='UNFORMATTED',
     &      ACCESS='DIRECT', RECL=LENGTH, ACTION='READ', IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE (6,'(/T8,A,I4)')
     &          'ERROR OPENING HOUSEKEEPING DATA SET - "IOSTAT" =', IOS
         STOP
      END IF
!                                    OPEN MAIN DREGS DATA SET ON UNIT 2

      OPEN (2, FILE='DREGS', STATUS='OLD', FORM='FORMATTED',
     &      ACCESS='DIRECT', RECL=LENREC, IOSTAT=IOS)
      IF (IOS.NE.0) THEN
         WRITE (6,'(/T8,A,I4)')
     &            'ERROR OPENING MAIN DREGS DATA SET - "IOSTAT" =', IOS
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
         IF (JOB.EQ.0) THEN
            FILENAME = 'FTPDREGS'
            NEWDREG = .TRUE.
!                                 FOR STORAGE JOB, GET JOBNAME FROM JOB
!                                STATUS RECORD IN HOUSEKEEPING DATA SET
         ELSE
            NJOB = NDRECS + 2 + JOB ! (= JOB STATUS RECORD NUMBER)
            READ (1,REC=NJOB) C16, JOBNAME
!                                             SET DATA SET FROM JOBNAME
            IF (JOBNAME.GT. '        ') THEN
               NEWDREG = .TRUE.
               IF (LENSLQ.EQ.0) THEN
                  DSN = HLQ(1:LENHLQ) // '.' // JOBNAME             !1.2
               ELSE
                  DSN = HLQ(1:LENHLQ) // '.' //                     !1.2
     &                  SLQ(1:LENSLQ) // '.' // JOBNAME             !1.2
               END IF
!                                   CHECK WHETHER DREGS DATA SET EXISTS

               INQUIRE (FILE='/'//DSN, EXIST=NEWDREG, IOSTAT=IOS)   !1.2
               IF (.NOT.NEWDREG) THEN                               !1.2
                  WRITE (6,'(T5,2A)')                               !1.2
     &                'NO DREGS DATA SET FOUND FOR JOB ', JOBNAME   !1.2
               ELSE                                                 !1.2
!                                      DYNAMIC ALLOC. OF DREGS DATA SET
                  FILENAME = JOBNAME                                !1.2
                  DDNAME(41:48) = FILENAME                          !1.2
                  LENDSN = INDEX(DSN,' ') - 1                       !1.2
                  IF (LENDSN.LT.0) LENDSN = LEN(DSN)                !1.2
                  CALL DYNALC (DSN, LENDSN, 'CATALG', DDNAME)       !1.2
               END IF
            END IF
         END IF
!                              OPEN DREGS DATA SET FOR THIS STORAGE JOB
         IF (NEWDREG) THEN
            OPEN (IUNIT, FILE=FILENAME, STATUS='OLD', FORM='FORMATTED',
     &            ACCESS='DIRECT', RECL=LENREC, IOSTAT=IOS)

!                                 CHECK RETURN CODE FROM OPEN STATEMENT

            IF (IOS.EQ.0) THEN        ! OPENED OK - READ HEADER
               READ (IUNIT,'(2I6)',REC=1) LAST(JOB), NUMRECS(JOB)

!                                       CLOSE IF "LAST" IS OUT OF RANGE

               IF (LAST(JOB).LT.2 .OR. LAST(JOB).GT.NUMRECS(JOB)) THEN
                  CLOSE (IUNIT)
               ELSE
                  OPENED(JOB) = .TRUE.
                  NUNIT(JOB) = IUNIT
                  MAXJOB = JOB
!                                                         PRINT MESSAGE
                  IF (FILENAME.NE.'FTPDREGS') THEN                  !1.2
                     WRITE (6,'(T5,2A)')
     &                        'DREGS DATA SET OPENED FOR ', JOBNAME !1.2
                  ELSE
                     WRITE (6,'(T5,A)')
     &                        'DREGS DATA SET OPENED FOR MONITOR JOB'
                  END IF

                  IUNIT = IUNIT + 1 ! (FOR NEXT TIME)
               END IF
!                                           OPEN FAILED - PRINT MESSAGE
            ELSE
               IF (JOB.EQ.0) THEN
                  WRITE (6,'(/T2,2A,I4)') 'ERROR OPENING DREGS DATA ',
     &                     'SET ON "FTPDREGS":  "IOSTAT" =', IOS
               ELSE
                  WRITE (6,'(/T2,4A,I4)') 'ERROR OPENING DREGS DATA ',
     &                     'SET FOR ', JOBNAME, ' -  "IOSTAT" =', IOS
               END IF
            END IF
         END IF
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
!                             CHECK WHETHER THERE ARE ANY DREGS TO COPY
         IF (LOOKING) THEN                                            !2
            READ (IUNIT,'(A13)',REC=NEXT) YMDHL                       !2
            IF (YMDHL(1:1).EQ.' ') THEN        ! No dregs to copy     !2
               CLOSE (IUNIT)                                          !2
               OPENED(JOB) = .FALSE.                                  !2
               LOOKING = .FALSE.                                      !2
            END IF                                                    !2
         END IF                                                       !2
!                               LOOP BACK THROUGH DREGS RECORDS LOOKING
!                                       FOR THE OLDEST UNCOPIED MESSAGE
         DO WHILE (LOOKING)
            NREC(JOB) = NEXT    ! Oldest message found so far         !2
            NEXT = NEXT - 1                                           !2
            IF (NEXT.LT.2) NEXT = NUMRECS(JOB)                        !2
            READ (IUNIT,'(A)',REC=NEXT) YMDHN                         !2

!                                     GET THE TIME OF THE DREGS MESSAGE
!           (Stop at dreg already copied (year blanked out) or one that
!           has just come in (more recent time than previous record) or
!           if returned to start.)                                    !2

            IF (YMDHN(1:1).EQ.' ' .OR.          ! (already copied)    !2
     &          YMDHN.GT.YMDHL .OR.             ! (just come in)      !2
     &          NEXT.EQ.LAST(JOB)) THEN         ! (gone round once)   !2
               READ (IUNIT,'(A)',REC=NREC(JOB)) DREGMSG(JOB)          !2
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

      DO WHILE (MAXJOB.GE.0)
!                                         LOOK FOR OLDEST DREGS MESSAGE
         OLDEST = '9999999999999'
         DO JOB=0,MAXJOB
            IF (OPENED(JOB) .AND. YMDHM(JOB).LT.OLDEST) THEN
               OLDEST = YMDHM(JOB)
               NJOB = JOB
            END IF
         END DO
!                           COPY OLDEST MESSAGE TO MAIN DREGS DATA SET.
!        (NOTE:  MESSAGES FROM "STORCHEK" CAN OCCUR IN DREGS DATA SETS
!        FOR BOTH MONITOR AND STORAGE JOBS SO ONLY STORE THE ONE FROM
!        THE MONITOR JOB ("NJOB"=0).)

         IF (NJOB.EQ.0 .OR. DREGMSG(NJOB)(22:29).NE.'STORCHEK') THEN
            LATEST = LATEST + 1
            IF (LATEST.GT.MAXREC) LATEST = 2
            WRITE (2,'(A)',REC=LATEST) DREGMSG(NJOB)
            KOUNT = KOUNT + 1
         END IF
!                         BLANK OUT YEAR IN JOB'S DREGS DATA SET RECORD
!                      (THIS STOPS IT GETTING STORED AGAIN BY NEXT RUN)

         IUNIT = NUNIT(NJOB)
         DREGMSG(NJOB)(1:4) = '    '
         WRITE (IUNIT,'(A)',REC=NREC(NJOB)) DREGMSG(NJOB)

!                               CLOSE DATA SET IF NO MORE DREGS TO READ

         IF (NREC(NJOB).EQ.LAST(NJOB)) THEN
            CLOSE (IUNIT)
            OPENED(NJOB) = .FALSE.
!                                        UPDATE "MAXJOB" IF APPROPRIATE
            LOOKING = NJOB.EQ.MAXJOB
            DO WHILE (LOOKING)
               MAXJOB = MAXJOB - 1
               LOOKING = (MAXJOB.GE.0) .AND. (.NOT.OPENED(MAXJOB))
            END DO
!                             FIND NEXT MESSAGE IN JOB'S DREGS DATA SET
         ELSE
            NEXT = NREC(NJOB) + 1
            IF (NEXT.GT.NUMRECS(NJOB)) NEXT = 2  ! (REC. 1 IS A HEADER)
            READ (IUNIT,'(A)',REC=NEXT) DREGMSG(NJOB)
            YMDHM(NJOB) = DREGMSG(NJOB)(1:13)
            NREC(NJOB) = NEXT
         END IF
      END DO
!                                  UPDATE HEADER OF MAIN DREGS DATA SET

      READ (2,'(A)',REC=1) DREGMSG(0)
      WRITE (DREGMSG(0)(1:6),'(I6)') LATEST
      WRITE (2,'(A)',REC=1) DREGMSG(0)
      CLOSE (2)
!                                                              FINISHED
      WRITE (6,'(/T2,I7,A)') KOUNT, ' DREGS MESSAGES COPIED.'
      STOP
      END
