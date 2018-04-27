      PROGRAM PAOBDAT                                               !1.2

!-----------------------------------------------------------------------
!
! PROGRAM     : PAOBDAT
!
! PURPOSE     : MAIN PROGRAM FOR STORAGE OF PAOBS IN MET.D.B.
!
! DESCRIPTION : - READS DETAILS OF BULLETINS TO PROCESS,
!               - OPENS STORAGE AND HOUSEKEEPING DATA SETS,
!               - LOOKS FOR MHS DATA SETS, WAITING IF NECESSARY,
!               - EXTRACTS BULLETINS & CALLS PAOBUL TO PROCESS,
!               - PRINTS SUMMARY OF PROCESSING AT END OF JOB.
!
! NAMELIST    : INSTOR  (UNIT 5).  CONTENTS AS FOLLOWS:
!
!               VARIABLE TYPE        DESCRIPTION             DEFAULT
!               -------- ----        -----------             -------
!               PRINT    L*4  FLAG TO PRODUCE 1-LINE MESSAGE  .TRUE.
!                             FOR EACH DATA SET PROCESSED.
!               NSECS    I*4  NO. OF SECS. TO WAIT BETWEEN      30
!                             ACCESSES OF H.K. DATA SET.
!               NDREG    I*4  UNIT NO. FOR DREGS DATA SET       99
!                             (< OR =0 IF NO DATA SET).
!               MAXWAITS I*4 ) MAX. & MIN. MULTIPLES OF        120
!               MINWAITS I*4 ) NSECS USED WHEN WAITING FOR      32
!                            ) DATA SETS (SET MAXWAITS=0
!                            ) TO STOP WHEN NO DATA LEFT)
!               JOBNAME  C*8  JOB NAME.                     'PAOBJOB'
!               OWNER    C*8  MHS DATA SET OWNER.            'PAOB1'
!
! FILES USED  : MET.D.B. HOUSEKEEPING DATA SET OPENED ON UNIT 1.
!               NAMELIST INPUT READ FROM UNIT 5.
!
! CALLS       : ASC2EB, BULLED, DATIM, DREG, DSINFO, FINDMHS,
!               INITSTOR, INITSTOR, MHSIFF, NEXTFILE, PAOBUL,
!               SATYPE, SUMMARY, SECSOUT, SYSABN.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:51$
! $Source: /home/us0400/mdb/op/lib/source/RCS/paobdat.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:51    Sheila Needham  
! $
! Revision 2.1  2003/03/28 09:26:12  usmdb
! 2.1.  2 April 2003.  Brian Barwell.  Change 21/03.
! Changes to locations in GTS header associated with the change
! from TROPICS to FROST.
!
! Revision 2.0  2001/07/03  10:43:43  10:43:43  usmdb (MetDB account c/o usjh)
! Missing 6th argument in 4th call to DSINFO. Added arg DSN and
! changed other DSINFO calls to use DSN instead of BULL.
! Removed unused variable, added copyright and modified header - S.Cox
!
! Revision 1.2  2000/06/08  15:31:27  15:31:27  usmdb (Generic MetDB account)
! 19 June 2000  Brian Barwell
! 1.2  (1) Use new housekeeping data set. (2) New test facility
!      for "MHSP" data sets. (3) Framework for DREGS facility.
!      (4) Program name change: PAOBDATA -> PAOBDAT.
!
! Revision 1.1  99/03/11  13:47:17  13:47:17  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL. OPERATIONAL: MARCH 1999.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!
      INTEGER    MAXHED,     NFLAGS,   NITEMS
      PARAMETER (MAXHED=120, NFLAGS=9, NITEMS=12)
!
      INTEGER ITEMS(0:NITEMS,MAXHED)
      INTEGER LSEQ(99), LENIDX(MAXHED)
      LOGICAL FLAGS(NFLAGS,MAXHED)
      CHARACTER*6 HEAD1(MAXHED), HEAD2(MAXHED)
      CHARACTER*8 BULTYP(MAXHED)
!
      INTEGER IDUMMY             ! DUMMY ARG. FOR "NEXTFILE"        !1.2
      INTEGER INDX               ! TYPE INDEX FOR MESSAGE COUNTER
      INTEGER ISEQ               ! BUFR SEQUENCE DESCRIPTOR FOR STORAGE
      INTEGER IUNIT              ! UNIT NUMBER OF STORAGE DATA SET
      INTEGER J                  ! GENERAL LOOP VARIABLE
      INTEGER KODE               ! RETURN CODE FROM VARIOUS SUBROUTINES
      INTEGER KOUNTS(3,0:MAXHED) ! COUNTERS FOR NUMBERS OF MESSAGES
      INTEGER LENREC             ! RECORD LENGTH OF STORAGE DATA SET
      INTEGER MLNGTH, MFINAL     ! MESSAGE LENGTH & LAST BYTE TO PRINT
      INTEGER MSGCODE            ! RETURN CODE FROM "FINDMHS"
      INTEGER MSTART, MEND       ! FIRST & LAST BYTES OF MSG IN "BULL"
      INTEGER NBUL, NBULLS       ! NO. & TOTAL NO. OF BULLETIN TYPES
      INTEGER NFILES             ! NUMBER OF DATA SETS PROCESSED SO FAR
      INTEGER NMSGS              ! NUMBER OF MESSAGES PROCESSED THIS D/S
      INTEGER NOW(9)             ! CURRENT DATE AND TIME (FROM "DATIM")
      INTEGER NSTORE             ! STORAGE INDEX FOR MESSAGE COUNTER
      INTEGER NTOTAL             ! NUMBER OF MESSAGES PROCESSED SO FAR
      INTEGER NWAITS             ! NO. OF WAITS BEFORE CALL TO FINDMHS
      LOGICAL STORFLAG           ! FLAG FOR STORAGE REQUIRED        !2.0
      LOGICAL TERMIN8            ! TERMINATION FLAG
      CHARACTER*1 DUMMY          ! DUMMY ARG. FOR "NEXTFILE"        !1.2
      CHARACTER*44 DSN           ! DSN ARG. FROM "DSINFO". NOT USED !2.0
      CHARACTER*8 DATYPE         ! DATA TYPE CODE
      CHARACTER*18 BUL18         ! 'TTAAII CCCC YYGGGG' FROM MESSAGE
      CHARACTER*132 HEAD         ! FOR REVISION INFORMATION
      CHARACTER*28672 BULL       ! 28K BUFFER TO HOLD BULLETINS
!                                                              NAMELIST
      CHARACTER*8 OWNER,  JOBNAME                                   !1.2
      LOGICAL               PRINT
      INTEGER                      NDREG, NSECS, MINWAITS, MAXWAITS !1.2
      NAMELIST /INSTOR/                                             !1.2
     &     OWNER,  JOBNAME, PRINT, NDREG, NSECS, MINWAITS, MAXWAITS !1.2
      DATA OWNER,  JOBNAME, PRINT, NDREG, NSECS, MINWAITS, MAXWAITS !1.2
     &   /'PAOB1','PAOBJOB',.TRUE.,   99,    30,       32,     120/ !1.2
!
      COMMON /COMBUL/ BULL ! (TO ENABLE DYNAMIC ALLOCATION)
!
!                                                       DATA STATEMENTS
      DATA NFILES/0/, NMSGS/0/, NTOTAL/0/                           !2.0
      DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/
      DATA MSGCODE /0/, DUMMY, IDUMMY /' ', 0/
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/paobdat.f,v $
     &'//'$Date: 30/01/2006 20:23:51$ $Revision: 1$'
!
!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETUP
!-----------------------------------------------------------------------
!                                          READ BULLETIN PROCESSING AND
!                                          STORAGE DATA SET INFORMATION
      NBULLS = MAXHED
      CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS,
     &               NBULLS, LENIDX, LSEQ, BULL)
!                                                    READ NAMELIST DATA
      READ (5, INSTOR, END=1)
      IF (NDREG.NE.99)                                              !1.2
     &    CALL DREG (NDREG, 'DREGUNIT', ' ', ' ', ' ', ' ', 0, ' ') !1.2
    1 CLOSE (5)                                                     !1.2
      NWAITS = MINWAITS                                             !1.2
!                                   OPEN HOUSEKEEPING DATA SET (UNIT 1)
!
      CALL DSINFO ('HKEEP',  3, 1, LENREC, KODE, DSN)               !2.0
!
!                                                  ALLOCATE BUFR TABLES
!
      CALL DSINFO ('TABLEB', 0, 0, LENREC, KODE, DSN)               !2.0
      CALL DSINFO ('TABLED', 0, 0, LENREC, KODE, DSN)               !2.0
!
!                           ALLOCATE JOB STATUS RECORD IN H.K. DATA SET
!      ("TERMIN8" IS USED TEMPORARILY HERE TO INDICATE WHETHER MHS DATA
!            SET NAMES ARE TO BE TAKEN FROM THE HOUSEKEEPING DATA SET.)
!
      TERMIN8 = .FALSE. ! i.e. not using 'SDB1' data                !1.2
      CALL NEXTFILE (JOBNAME, IDUMMY, TERMIN8)                      !1.2
!
!-----------------------------------------------------------------------
!     LOOP OVER INCOMING BULLETINS.
!-----------------------------------------------------------------------
!
      DO WHILE (.NOT.TERMIN8)
!                               LOCATE NEXT MESSAGE & CHECK RETURN CODE
!
         CALL FINDMHS (10, OWNER, BULL, MSTART, MLNGTH, MSGCODE)    !1.2
!
!                         MSGCODE=1 (END OF DATA SET) OR <0 (MHS ERROR)
!                         ---------------------------------------------
!
         IF (MSGCODE.EQ.1 .OR. MSGCODE.LT.0) THEN
            NFILES = NFILES + 1
            IF (PRINT) THEN
               CALL DATIM (NOW)
               WRITE (6,'(T3, I2.2,A,I2.2,A,I2.2, I7, 2A)')
     &                NOW(5), ':', NOW(4), ':', NOW(3), NMSGS,
     &               ' MESSAGES PROCESSED FROM  ', BULL(9:52)
            END IF
            NMSGS = 0
!                                          TERMINATE IF FATAL MHS ERROR
!
            IF (MSGCODE.LT.0) TERMIN8 = .TRUE.                      !1.2
!
!                                      MSGCODE=2 (NO DATA SETS WAITING)
!                                      --------------------------------
         ELSE IF (MSGCODE.EQ.2) THEN
            IF (MAXWAITS.LE.0) THEN ! TERMINATE NOW
               TERMIN8 = .TRUE.
            ELSE
               DO J=1,NWAITS
                  CALL SECSOUT (NSECS)                              !1.2
                  CALL NEXTFILE (DUMMY,IDUMMY,TERMIN8) ! update JSR !1.2
                  IF (TERMIN8) GO TO 999
               END DO ! J
               NWAITS = MIN0(NINT(1.51*NWAITS),MAXWAITS) ! FOR NEXT TIME
            END IF
         ELSE
!                                        MSGCODE=0 (NEXT MESSAGE FOUND)
!                                        ------------------------------
            NWAITS = MINWAITS
            MEND = MSTART + MLNGTH - 1
            MFINAL = MIN0(MEND,MSTART+131)
            NSTORE = 3
!                     CONVERT ASCII TO EBCDIC & EXTRACT BULLETIN HEADER
!
            CALL ASC2EB (MLNGTH, BULL(MSTART:MEND))                 !2.1
            BUL18 = BULL(MSTART+20:MSTART+37)                       !2.1
!                                               GET BULLETIN TYPE INDEX
!
            CALL SATYPE (BUL18(1:6), HEAD1, HEAD2, NBULLS, NBUL)
!
!                                              SET SOME STORAGE DETAILS
            IF (NBUL.GT.0) THEN
               DATYPE = BULTYP(NBUL)
               STORFLAG = FLAGS(1,NBUL)
               INDX = ITEMS(0,NBUL)  ! (Row number in summary table)
            END IF
!                                              UNKNOWN BULLETIN HEADING
            IF (NBUL.LE.0) THEN
               DATYPE = 'UNKNOWN '
               INDX = 0
               WRITE (6,'(T2,2A)') BUL18,
     &                  ':  UNRECOGNISED BULLETIN HEADING.'
               WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
!
            ELSE IF (STORFLAG) THEN
!
!-----------------------------------------------------------------------
!        FIND UNIT & RECORD LENGTH FOR STORAGE, AND CHECK RETURN CODE.
!-----------------------------------------------------------------------
!
               CALL DSINFO (DATYPE, 0, IUNIT, LENREC, KODE, DSN)    !2.0
!
!                                         WARNING FOR UNKNOWN DATA TYPE
               IF (KODE.EQ.2) THEN
                  WRITE (6,'(T2,3A)') BUL18,
     &                     ':  UNRECOGNISED DATA TYPE: ', DATYPE
                  WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
!
!                                     WARNING FOR INACCESSIBLE DATA SET
!
               ELSE IF (KODE.EQ.3) THEN
                  WRITE (6,'(T2,4A)') BUL18,':  INACCESSIBLE ',
     &                     ':  INACCESSIBLE DATA SET FOR: ', DATYPE
                  WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
!
               ELSE
!
!-----------------------------------------------------------------------
!        ALL OK, SO CALL MET.D.B. STORAGE ROUTINE.
!-----------------------------------------------------------------------
!
                  J = MSTART + 41                                   !2.1
                  ISEQ = LSEQ(IUNIT)
                  CALL BULLED (J, MEND, BULL)
                  CALL PAOBUL (BULL(J:MEND), ISEQ, IUNIT, LENREC)
!
!-----------------------------------------------------------------------
!     PRINT INFORMATION MESSAGE(OPTIONAL), UPDATE BULLETIN COUNTERS
!     AND RETURN FOR ANOTHER MESSAGE.
!-----------------------------------------------------------------------
!
                  NSTORE = 2
!                               OPTIONAL MESSAGE FOR SUCCESSFUL STORAGE
!
                  IF (ITEMS(2,NBUL).GE.1) WRITE (6,'(T2,3A)') BUL18,
     &                                   ':  DATA TYPE ', DATYPE
               END IF
!                           OPTIONAL MESSAGE FOR DATA TYPE NOT REQUIRED
            ELSE
               IF (ITEMS(2,NBUL).GE.1) WRITE (6,'(T2,4A)') BUL18,
     &             ':  DATA TYPE ', DATYPE, ' STORAGE NOT REQUIRED.'
            END IF
!
            KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
            KOUNTS(NSTORE,INDX) = KOUNTS(NSTORE,INDX) + 1
            NMSGS = NMSGS + 1
            NTOTAL = NTOTAL + 1
         END IF
!                UPDATE JSR IF NOT IN THE MIDDLE OF READING A DATA SET,
!                CHECK FOR JOB TERMINATION AND GO BACK FOR NEXT MESSAGE
!
         IF (MSGCODE.NE.0) CALL NEXTFILE (DUMMY, IDUMMY, TERMIN8)   !1.2
      END DO ! WHILE
!                                   SUMMARY TABLE OF MESSAGES PROCESSED
  999 CONTINUE
      NBUL = 0
      DO J=1,NBULLS
!                        Convert BULTYP array to row headings for table
!                                  (i.e. removes duplicated data types)
         INDX = ITEMS(0,J)
         BULTYP(INDX) = BULTYP(J)
         NBUL = MAX0(NBUL,INDX)
      END DO ! J
!                                                         Summary table
      CALL SUMMARY (NFILES, NTOTAL, KOUNTS, BULTYP, NBUL)
!
!                                 MESSAGES FOR ABNORMAL JOB TERMINATION
      IF (MSGCODE.LT.0) THEN
         WRITE (6,'(/T2,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',
     &            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
         IF (MSGCODE.EQ.-1) THEN
            WRITE (6,'(T2,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
            WRITE (6,'(T2,2A)') 'TRY RE-RUNNING THE JOB: IF THE ',
     &               'PROBLEM RECURS, TRY DELETING THE NEXT "PAOB1" ',
     &               'DATA SET (LOOK FOR "MHSR.*.*.PAOB1.S000").'
         ELSE IF (MSGCODE.EQ.-2) THEN
            WRITE (6,'(T2,2A/)')
     &               'RENAME OR DELETE THE DATA SET ', BULL(9:52)
            WRITE (6,'(T2,2A)')
     &               'CHECK THAT THIS DATA SET DOES NOT EXIST. ',
     &               'IF IT DOES, DELETE IT AND RERUN THE JOB.'
         END IF
         KODE = 800 - MSGCODE
         WRITE (6,'(/T2,A,I4/)')
     &               'THIS JOB WILL NOW ABEND WITH USER CODE', KODE
         CALL SYSABN (KODE)
      END IF
!
      STOP
      END
