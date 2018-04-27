      PROGRAM GOESAMW

!-----------------------------------------------------------------------
!
! PROGRAM       : GOESAMW
!
! PURPOSE       : READ GOES WIND BULLETINS FROM U.S.A. AND PASS FOR
!                 STORAGE IN THE MDB.
!
! CALLS         : ASC2EB, GOESW1, MHSIFF, NEXTFILE, PARM, SATYPE,
!                 SECSOUT.
!
! FILES USED    : MET.D.B. HOUSEKEEPING DATA SET OPENED ON UNIT 1.
!                 MHS DATA SETS OPENED AND READ ON UNIT "IFT" (=20).
!
! REVISION INFO :
!
! $Revision: 2$
! $Date: 22/06/2007 14:42:21$
! $Source: /home/us0400/mdb/op/lib/source/RCS/goesamw.F,v $
!
! CHANGE RECORD :
!
!     MAR 98:  ORIGINAL VERSION BY BRIAN BARWELL.
!
! $Log:
!  2    Met_DB_Project 1.1         22/06/2007 14:42:21    Brian Barwell
!       Obsolete module used for storage of GOESAMW, GOESVIS and GOESWV data
!       which terminated in August 2006.
!  1    Met_DB_Project 1.0         30/01/2006 20:22:38    Sheila Needham  
! $
! Revision 2.1  2001/07/04 13:31:44  usmdb
! 2.1:  16 July 2001,  Brian Barwell,  Change 86/01.
! GOESAMW, GOESVIS and GOESWV identified from MHS data set name.
! MHSDIAG array introduced for last argument of calls to MHSIFF.
! Size of BULL array increased yet again from 400 to 1000.
!
! Revision 2.0  2001/05/31  13:27:48  13:27:48  usmdb (Generic MetDB account)
! Removed unused character statement INTWO and unused variables.
! Removed CCCC as not used in GOESW1 and therefore not needed
! in this routine. Added copyright and modified header - S.Cox
!
! Revision 1.6  2000/11/27  08:38:18  08:38:18  usmdb (Generic MetDB account)
! 1.6   27 November 2000   Brian Barwell
! Further increase in size of BULL array from 300 to 400.
!
! Revision 1.5  2000/07/31  11:01:08  11:01:08  usmdb (Generic MDB account)
! 1.5   31 July 2000   Brian Barwell
! Increase size of BULL array from 250 to 300.
!
! Revision 1.4  2000/06/08  15:31:09  15:31:09  usmdb (Generic MDB account)
! 19 June 2000  Brian Barwell
! 1.4  (1) Use new housekeeping data set.
!      (2) New test facility for "MHSP" data sets.
!
! Revision 1.3  99/07/12  16:18:05  16:18:05  usmdb (Generic MDB account)
! 19 July 1999, Infoman 64241, Brian Barwell, v(G)=33, ev(G)=18.
! Increase size of BULL array to hold 250 data records (was 200).
!
! Revision 1.2  99/04/12  10:59:53  10:59:53  usmdb (Generic MDB account)
! 19 April 1999, Infoman 62045, Brian Barwell, v(G)=33, ev(G)=18.
! Delete data set if it cannot be renamed (to avoid continuous
! re-processing). Also introduce some more modular programming.
!
! Revision 1.1  98/03/12  09:11:21  09:11:21  usmdb (Generic MDB account)
! Initial revision
!
! MAR 98:  ORIGINAL VERSION (BASED LOOSELY ON SATBUL).     (B.R.B.)
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
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!     PARAMETER STATEMENTS
!-----------------------------------------------------------------------
!
      INTEGER NWAIT, MAXHED, MAXREC                                 !1.2
      INTEGER NFLAGS, NITEMS                                        !1.2
      PARAMETER (NWAIT=30, MAXHED=120, MAXREC=2000)                 !2.1
      PARAMETER (NFLAGS=9, NITEMS=12)                               !1.2
!
!-----------------------------------------------------------------------
!     DECLARE VARIABLES
!-----------------------------------------------------------------------
!
      INTEGER NWAITS, IOLEVL, ITT, IEND                             !2.0
      INTEGER IUNIT, IREC, IOS, IDUMMY                              !1.4
      INTEGER J, JWAITS, JREC, KODE, INDX, NSTORE                   !1.2
      INTEGER NUMDS, NBUL, NBULLS, IFT, NSECS                       !2.0
      INTEGER ITEMS(0:NITEMS,MAXHED), KOUNT(3,0:MAXHED)             !1.2
      INTEGER LSEQ(99), LENIDX(99)                                  !1.2
      INTEGER MHSDIAG(8)      ! MHS DIAGNOSTICS (FOR "MHSIFF")      !2.1
      INTEGER MODE            ! TEST RUN MODE (=1 FOR 'MHSP')       !1.4
!
      CHARACTER DUMMY                                               !2.0
      CHARACTER*4 PARMIN                                            !2.0
      CHARACTER*6 BULHED, HEAD1(MAXHED), HEAD2(MAXHED)
      CHARACTER*8 DATYPE, DATYPS(MAXHED), DDN, OWNER, STAT          !1.2
      CHARACTER BUL18*18, DSN*44                                    !1.2
      CHARACTER*512 BULL(MAXREC)
      CHARACTER*27998 BUFFER                                        !1.2
      CHARACTER HEAD*132
      LOGICAL TERMIN, FLAGS(NFLAGS,MAXHED), BUFLAG                  !1.4
      LOGICAL TEST ! flag for old test mode - see below             !1.4
!
!-----------------------------------------------------------------------
!     COMMON BLOCK (FOR DYNAMIC ALLOCATION ONLY)
!-----------------------------------------------------------------------
!
      COMMON /COMAMW/ BULL, BUFFER                                  !1.2
!
!-----------------------------------------------------------------------
!     DATA STATEMENTS
!-----------------------------------------------------------------------
!
      DATA DUMMY, IDUMMY /' ', 0/ ! DUMMY ARGS. FOR "NEXTFILE"      !1.4
      DATA IFT/20/ ! UNIT NUMBER FOR INPUT
      DATA MODE/0/ ! OPERATIONAL MODE                               !1.4
      DATA NWAITS/1/, NSECS/0/, NUMDS /0/                           !1.2
      DATA KOUNT /3*0, MAXHED*0, MAXHED*0, MAXHED*0/                !1.2
!
!-----------------------------------------------------------------------
!     REVISION INFORMATION
!-----------------------------------------------------------------------
!
      HEAD='
     & $Source: /home/us0400/mdb/op/lib/source/RCS/goesamw.F,v $
     &'//' $Date: 22/06/2007 14:42:21$ $Revision: 2$ '
!
!-----------------------------------------------------------------------
!     CHECK FOR RUN IN OLD TEST MODE (I.E. "EXEC" STATEMENT INCLUDES
!     "PARM=TEST"). IN THIS MODE JOB DOES NOT INTERACT WITH OPERATOR
!     AND TERMINATES WHEN NO MORE FILES ARE WAITING TO BE PROCESSED.
!-----------------------------------------------------------------------
!
      CALL PARM (1,PARMIN)
      TEST = PARMIN.EQ.'TEST'
      IF (TEST) WRITE (6,'(T2,A/)')                                 !1.2
     &         'TEST RUN OF GOES HIGH-RES. A.M.W. PROGRAM'          !1.2
      IOLEVL = 1   ! I.E. HIGHER I/O LEVEL IS DEFAULT AT THE MOMENT
!
!-----------------------------------------------------------------------
!     READ INFORMATION ON BULLETIN PROCESSING AND STORAGE DATA SETS.
!-----------------------------------------------------------------------
!
      NBULLS = MAXHED                                               !1.2
      CALL INITSTOR (HEAD1, HEAD2, DATYPS,                          !1.2
     &               FLAGS, ITEMS, NBULLS, LENIDX, LSEQ, BUFFER)    !1.2
!
!-----------------------------------------------------------------------
!     ALLOCATE OR OPEN OTHER DATA SETS (BUFR TABLES, HOUSEKEEPING ETC.).
!-----------------------------------------------------------------------
!
      CALL DSINFO ('CODEFIG', 0, 0, IREC, KODE, DSN)                !1.2
      CALL DSINFO ('TABLEB',  0, 0, IREC, KODE, DSN)                !1.2
      CALL DSINFO ('TABLED',  0, 0, IREC, KODE, DSN)                !1.2
!
!                          IF NOT TEST MODE, OPEN HOUSEKEEPING DATA SET
!                           FOR READ ONLY AND CHECK FOR JOB TERMINATION
      IF (.NOT.TEST) THEN                                           !1.2
         CALL DSINFO ('HKEEP', 3, 1, IREC, KODE, DSN)               !1.4
!
!                          ALLOCATE JOB STATUS RECORD IN H.K. DATA SET.
!       ("TERMIN" IS USED TEMPORARILY HERE TO INDICATE WHETHER MHS DATA
!            SET NAMES ARE TO BE TAKEN FROM THE HOUSEKEEPING DATA SET.)
!
         TERMIN = .FALSE. ! i.e. not using 'SDB1' data              !1.4
         CALL NEXTFILE ('MDBGOES ', IDUMMY, TERMIN)                 !1.4
      END IF                                                        !1.2
!                              DEFINE MHS DATA SET NAME CHARACTERISTICS
      OWNER = 'MDB2'
   10 CONTINUE
      STAT = 'GR #'
      KODE = 0
!                                    CHECK FOR WAIT AND JOB TERMINATION
      IF (.NOT.TEST) THEN
         DO JWAITS=1,NWAITS
            CALL SECSOUT (NSECS)                                    !1.4
            CALL NEXTFILE (DUMMY, IDUMMY, TERMIN) ! to update JSR   !1.4
            IF (TERMIN) GO TO 999      ! MAIN DATA BANK JOB TERMINATION
         END DO ! JWAITS
         NWAITS = INT(1.4*FLOAT(NWAITS+1))
         IF (NWAITS.GT.20) NWAITS = 20 ! MAXIMUM WAIT = 10 MINUTES
      END IF
!                                                 LOOK FOR NEW DATA SET
      CALL MHSIFF (DSN, DDN, STAT, OWNER, MHSDIAG)                  !2.1
      KODE = MHSDIAG(1)                                             !2.1
      IF (DSN(1:4).EQ.'MHSP') MODE = 1   ! stops infinite cycling   !1.4
      IF (KODE.EQ.0) THEN ! DATA SET FOUND
!                                                     OPEN THE DATA SET
!
         OPEN (IFT, FILE=DDN, STATUS='OLD',FORM='FORMATTED', IOSTAT=IOS)
!
!                                      READ DATA SET IN 512-BYTE BLOCKS
         IREC = 0
         DO JREC=1,MAXREC
            READ (IFT, '(A)', END=15, IOSTAT=IOS) BULL(JREC)
            IF (IOS.GT.0) THEN
               WRITE (6,'(T14,A,I4)')
     &         'I/O ERROR READING GOES A.M.W. BULLETIN:  IOSTAT =', IOS
               WRITE (6,'(T2,A)') BULL(1)(1:128)
               GO TO 99
            END IF
            CALL ASC2EB (512, BULL(JREC))
            IREC = JREC
         END DO
!
!-----------------------------------------------------------------------
!        FIND START & END OF BULLETIN, AND IDENTIFY DATA TYPE
!-----------------------------------------------------------------------
!
!                                          SEARCH FOR START OF BULLETIN
   15    CONTINUE
         ITT = INDEX(BULL(1),'YYXX')
         IF (ITT.EQ.0) THEN
            WRITE (6,'(T14,A)') 'START OF MESSAGE ("YYXX") NOT FOUND'
            WRITE (6,'(T2,A)') BULL(1)(1:128)
            GO TO 99
         END IF
         ITT = ITT + 5  ! (TO SKIP "YYXX ")
!                                            SEARCH FOR END OF BULLETIN
!                    (DATA CONTAINS NO LETTERS EXCEPT FOR 'NNNN' AT THE
!                   END. HOWEVER, 'NNNN' COULD SPAN THE END OF A RECORD)
!
         IEND = INDEX(BULL(IREC),'NNNN')
         IF (IEND.GT.0) THEN
            IEND = (IREC-1)*512 + IEND - 2
         ELSE
            IEND = INDEX(BULL(IREC-1),'N')
            IF (IEND.EQ.0) THEN
               WRITE (6,'(T14,A)') 'FAILED TO FIND END OF MESSAGE'
               WRITE (6,'(T2,A)') BULL(1)(1:128)
               GO TO 99
            END IF
            IEND = (IREC-2)*512 + IEND - 2
         END IF
!                     INVENT BULLETIN HEADER FROM DSN AND GET DATA TYPE
!         (Time group in DSN (bytes 15-21) has the form 'Thhidds' where
!          'hh'= nominal hour, 'i'= hemisphere ('N' or 'S'), 'dd'= data
!           type (CD/VI/WV = cloud drift/visible/water vapour) and 's'=
!         satellite indicator (currently 8 or 9 for GOES-8 or GOES-10).
!
         BUL18  = 'GOES' // DSN(19:20) // ' KWBC ' // DSN(16:21)    !2.1
         BULHED = BUL18(1:6)
         NSTORE = 3
         CALL SATYPE (BULHED, HEAD1, HEAD2, NBULLS, NBUL)
!
!-----------------------------------------------------------------------
!        CHECK MESSAGE QUALITY AND REJECT IF UNSATISFACTORY.
!-----------------------------------------------------------------------
!
!                                  WARNING FOR UNKNOWN BULLETIN HEADING
         IF (NBUL.LE.0) THEN
            DATYPE = 'UNKNOWN '                                     !1.2
            INDX = 0                                                !1.2
            WRITE (6,'(T2,2A,T55,A)') BUL18,
     &               ':  UNRECOGNISED BULLETIN HEADING.'            !1.2
            WRITE (6,'(T2,A)') BULL(1)(1:128)
            GO TO 99
!
         ELSE IF (FLAGS(1,NBUL)) THEN ! STORAGE REQUIRED
!
!                                          GET STORAGE DATA SET DETAILS
            DATYPE = DATYPS(NBUL)
            INDX = ITEMS(0,NBUL)  ! (Row number in summary table)   !1.2
            CALL DSINFO (DATYPE, 0, IUNIT, IREC, J, DSN)            !1.2
            BUFLAG = .FALSE.
!                                         WARNING FOR UNKNOWN DATA TYPE
            IF (J.EQ.2) THEN                                        !1.2
               WRITE (6,'(T2,3A,T58,A)') BUL18,
     &               ':  UNRECOGNISED DATA TYPE: ', DATYPE          !1.2
               WRITE (6,'(T2,A)') BULL(1)(1:128)
!                                                    TEST FOR BUFR DATA
            ELSE IF (ITEMS(1,NBUL).LE.1) THEN                       !1.2
               WRITE (6,'(T2,2A,T55,A)') BUL18,
     &                  ':  CANNOT HANDLE BUFR BULLETINS.'          !1.2
               WRITE (6,'(T2,A)') BULL(1)(1:128)
!
!-----------------------------------------------------------------------
!           ALL O.K. SO STORE MESSAGE IN MET.D.B.
!-----------------------------------------------------------------------
!
            ELSE
               NSTORE = 2
               CALL GOESW1 (IUNIT, ITT, IEND, BULL)                 !2.0
               IF (IOLEVL.GT.0) WRITE (6,'(T2,3A)') DATYPE,         !2.1
     &              ' DATA PROCESSED FROM ', DSN                    !2.1
            END IF
!                                    MESSAGE FOR DATA TYPE NOT REQUIRED
         ELSE
            INDX = ITEMS(0,NBUL)                                    !1.4
            IF (ITEMS(2,NBUL).GE.1) WRITE(6,'(T2,4A,T65,A)') BUL18, !1.2
     &        ':  DATA TYPE ', DATYPE, ' STORAGE NOT REQUIRED.'     !1.2
         END IF
!                                                    INCREMENT COUNTERS
         KOUNT(1,INDX) = KOUNT(1,INDX) + 1                          !1.2
         KOUNT(NSTORE,INDX) = KOUNT(NSTORE,INDX) + 1                !1.2
!
!                                           CLOSE & RENAME OLD DATA SET
   99    CONTINUE
         CLOSE (IFT)
         STAT = 'R'
         CALL MHSIFF (DSN, DDN, STAT, OWNER, MHSDIAG)               !2.1
         KODE = MHSDIAG(1)                                          !2.1
!                                          CHECK FOR MHS RENAMING ERROR
         IF (KODE.NE.0) KODE = -2                                   !1.2
         NUMDS = NUMDS + 1
         NSECS = 0
         NWAITS = 1
!                                   NO MORE DATA SETS: GO BACK AND WAIT
      ELSE IF (KODE.EQ.4) THEN                                      !1.2
         IF (TEST .OR. MODE.EQ.1) TERMIN = .TRUE.                   !1.4
         NSECS = NWAIT
!                                      MHS ERROR LOCATING NEXT DATA SET
      ELSE
         NUMDS = NUMDS + 1                                          !1.2
         NSECS = 0                                                  !1.2
         NWAITS = 1                                                 !1.2
         KODE = -1                                                  !1.2
      END IF
!                                 MHS ERROR - TRY DELETING THE DATA SET
      IF (KODE.LT.0) THEN                                           !1.2
         WRITE (6,'(T4,A,I3,2A)') 'MHS ERROR - RETURN CODE', KODE,  !1.2
     &            '.  WILL DELETE ', DSN                            !1.2
         CALL MHSIFF (DSN, DDN, 'D  N', OWNER, MHSDIAG)             !2.1
         J = MHSDIAG(1)                                             !2.1
         IF (J.NE.0) TERMIN = .TRUE.   ! STILL BAD - GIVE UP        !1.4
         KODE = 4                                                   !1.2
      END IF                                                        !1.2
!                           UPDATE J.S.R. AND CHECK FOR JOB TERMINATION
!
      IF (.NOT.TEST) CALL NEXTFILE (DUMMY, IDUMMY, TERMIN)          !1.4
      IF (.NOT.TERMIN) GO TO 10                                     !1.4
!
!-----------------------------------------------------------------------
!     SUMMARY TABLE OF ALL MESSAGES PROCESSED
!-----------------------------------------------------------------------
!
  999 CONTINUE
      NBUL = 0                                                      !1.2
!                        Convert DATYPS array to row headings for table
!                                  (i.e. removes duplicated data types)
      DO J=1,NBULLS                                                 !1.2
         INDX = ITEMS(0,J)                                          !1.2
         DATYPS(INDX) = DATYPS(J)                                   !1.2
         NBUL = MAX0(NBUL,INDX)                                     !1.2
      END DO ! J                                                    !1.2
!                                                         Summary table
      CALL SUMMARY (NUMDS, NUMDS, KOUNT, DATYPS, NBUL)              !1.2
!
!-----------------------------------------------------------------------
!     MESSAGES FOR ABNORMAL JOB TERMINATION DUE TO MHSIFF ERROR
!-----------------------------------------------------------------------
!
      IF (KODE.LT.0) THEN                                           !1.2
         J = LEN(OWNER)                                             !1.2
         INDX = INDEX(OWNER,' ') - 1                                !1.2
         IF (INDX.LE.0) INDX = J                                    !1.2
         WRITE (6,'(/T2,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',  !1.2
     &            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'   !1.2
         IF (KODE.EQ.-1) THEN                                       !1.2
            WRITE(6,'(T2,A/)')'LOCATE THE NEXT DATA SET TO PROCESS.'!1.2
            WRITE (6,'(T2,2A)') 'TRY RE-RUNNING THE JOB: IF THE ',  !1.2
     &            'PROBLEM RECURS, TRY DELETING THE NEXT "',        !1.2
     &            OWNER(1:INDX), '" DATA SET (LOOK FOR "MHSR.*.*.', !1.2
     &            OWNER(1:INDX), '.S000").'                         !1.2
         ELSE IF (KODE.EQ.-2) THEN                                  !1.2
            WRITE (6,'(T2,2A/)')                                    !1.2
     &               'RENAME OR DELETE THE DATA SET ', DSN          !1.2
            WRITE (6,'(T2,2A)')                                     !1.2
     &               'CHECK THAT THIS DATA SET DOES NOT EXIST. ',   !1.2
     &               'IF IT DOES, DELETE IT AND RERUN THE JOB.'     !1.2
         END IF                                                     !1.2
         KODE = 800 - KODE                                          !1.2
         WRITE (6,'(/T2,A,I4/)')                                    !1.2
     &            'THIS JOB WILL NOW ABEND WITH USER CODE', KODE    !1.2
         CALL SYSABN (KODE)                                         !1.2
      END IF                                                        !1.2
!                                                     END OF PROCESSING
      STOP
      END
