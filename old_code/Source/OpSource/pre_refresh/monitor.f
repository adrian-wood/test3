      PROGRAM MONITOR

!---------------------------------------------------------------------
!
! PROGRAM     : MONITOR    (MET.D.B. STORAGE MONITOR JOB)
!
! LOCATION    : MDB.STORAGE.SRCE(MONITOR)
!
! PURPOSE     : TO RECEIVE DATA SETS FROM FROST, IDENTIFY BULLETINS
!               THEY CONTAIN AND PUT INFORMATION IN A HOUSEKEEPING
!               DATA SET. ALSO MONITORS MET.D.B. STORAGE JOBS.
!
! DESCRIPTION : THE FOLLOWING IS A BRIEF SUMMARY OF THE MAIN STEPS
!               IN THIS JOB. THE HOUSEKEEPING DATA SET IS USED FOR
!               COMMUNICATION WITH  MET.D.B. STORAGE JOBS.
!
!               - READ HEADER DETAILS OF BULLETINS TO BE STORED
!               - OPEN AND INITIALISE THE HOUSEKEEPING DATA SET
!
!               - LOOP OVER BULLETINS:
!                 - GET NEXT BULLETIN, OPENING NEW FROST DATA SET
!                   IF NECESSARY.
!
!                 - IF A NEW BULLETIN IS FOUND:-
!                   - DETERMINE BULLETIN TYPE.
!                   - IF ASCII, CONVERT TO EBCDIC.
!                   - DO ADDITIONAL CHECKS ON BULLETIN (IF ANY).
!
!                 - ELSE, IF THERE ARE NO NEW BULLETINS WAITING:
!                   - WAIT OR TERMINATE AS REQUIRED.
!
!                 - ELSE, IF END OF DATA SET REACHED:
!                   - PRINT 1-LINE INFORMATION MESSAGE (OPTIONAL).
!                   - WRITE DATA SET DETAILS TO HOUSEKEEPING DATA SET.
!                   - OCCASIONAL GENERAL UPDATE OF H.K. DATA SET.
!                 - END IF.
!
!                 - IF NOT IN THE MIDDLE OF A DATA SET:
!                   - UPDATE HOUSEKEEPING DATA SET STATUS RECORD.
!                   - FIND PLACE TO PUT INFO FOR NEXT DATA SET.
!
!               - END OF LOOP OVER BULLETINS.
!
!               - PRINT SUMMARY OF BULLETINS PROCESSED.
!               - SUPERVISE TERMINATION OF MET.D.B. STORAGE JOBS.
!               - ABEND IF UNRECOVERABLE MHS ERROR.
!               - STOP.
!
! NAMELIST    : INSTOR  (UNIT 5).  CONTENTS AS FOLLOWS:
!
!               VARIABLE TYPE        DESCRIPTION             DEFAULT
!               -------- ----        -----------             -------
!                CLEAR   L*4  FLAG TO SAY WHETHER G.K. DATA   .TRUE.
!                             SET IS TO BE CLEARED AT START.
!                PRINT   L*4  FLAG TO PRODUCE 1-LINE MESSAGE  .TRUE.
!                             FOR EACH DATA SET PROCESSED.
!                MODE    I*4  MODE FOR PARALLEL OR TEST RUN.     0
!                             (SEE STORAGE T.N. FOR DETAILS)
!                NDREG   I*4  UNIT NO. FOR DREGS DATA SET       99
!                             (< OR =0 IF NO DATA SET).
!                NWAIT   I*4  NO. OF SECS. TO WAIT IF THERE     30
!                             ARE NO DATA SETS TO PROCESS.
!                OWNER   C*8  MHS DATA SET OWNER              'SDB1'
!
! CALLS       : ABC, DATE31, DATIM, DREG, EBCDIC, FINDMHS, FINDREC,
!               HKSTART, HKSTOP, HKUPDATE, INITBULL, REPLYTU, SATYPE,
!               SECSOUT, STORCHEK, SUMMARY, SYSABN, TELLOPS.
!
! FILES USED  : MET.D.B. HOUSEKEEPING DATA SET OPENED ON UNIT 1.
!               UNIT 5 (NAMELIST INPUT) OPENED, READ AND AND CLOSED.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:23:24$
! $Source: /home/us0400/mdb/op/lib/source/RCS/monitor.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:24    Sheila Needham  
! $
! Revision 2.1  2003/03/28 09:17:48  usmdb
! 2.1.  2 April 2003.  Brian Barwell.  Change 21/03.
! Changes to locations in GTS header associated with the change
! from TROPICS to FROST.
!
! Revision 2.0  2001/09/05  09:24:53  09:24:53  usmdb (MetDB account c/o usjh)
! 17 Sep 2001:  2.0.  Brian Barwell.  Change 112/01.
! Make arguments to DREG consistent for HP compiler.
!
! Revision 1.1  2000/06/09  11:14:36  11:14:36  usmdb (Generic MetDB account)
! Initial revision
!
! 15/05/00 ORIGINAL OPERATIONAL VERSION. (B.BARWELL)
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
!                          Parameters relating to housekeeping data set

      INTEGER LENREC      ! RECORD LENGTH OF HOUSEKEEPING DATA SET
      INTEGER MAXSTOR     ! MAXIMUM NUMBER OF STORAGE DATA SET CODES
      INTEGER MAXJOBS     ! MAXIMUM NUMBER OF STORAGE JOBS
      INTEGER NDRECS      ! NUMBER OF DATA SET STATUS RECORDS
      INTEGER MAXREC      ! SIZE OF "RECORD" ARRAY
      INTEGER LENTRY      ! NUMBER OF BYTES IN DATA SET STATUS ENTRY
!
      PARAMETER (LENREC=950, MAXSTOR=50, MAXJOBS=50, NDRECS=160)
      PARAMETER (MAXREC=2+NDRECS, LENTRY=MAXSTOR+44)
!
!                       Parameters relating to bulletin header data set
!
      INTEGER MAXHED      ! MAXIMUM NUMBER OF BULLETIN HEADER RANGES
      INTEGER NFLAGS      ! NUMBER OF DATA PROCESSING FLAGS
      INTEGER NITEMS      ! NUMBER OF DATA PROCESSING ITEMS
!
      PARAMETER (MAXHED=160, NFLAGS=9, NITEMS=12)
!                                                             Variables
!
      INTEGER INDX               ! POINTER FOR MSG OR SUMMARY TABLE
      INTEGER IOS                ! STATUS CODE FROM I/O STATEMENT
      INTEGER I, J               ! VARIABLES FOR LOCAL USE
      INTEGER IWAIT              ! WAITING TIME FOR NEXT DATA SET
      INTEGER KODE               ! RETURN CODE (E.G. FROM 'DSINFO')
      INTEGER KOUNTS(3,0:MAXHED) ! COUNTERS FOR NUMBERS OF MESSAGES
      INTEGER LATEST             ! HIGHEST DATA SET NUMBER IN H.K.
      INTEGER MFIRST             ! FIRST BYTE OF MESSAGE AFTER HEADER
      INTEGER MLNGTH             ! MESSAGE LENGTH
      INTEGER MSGCODE            ! RETURN CODE FROM "FINDMSG"
      INTEGER MSTART, MEND       ! START AND FINISH BYTES OF MESSAGE
      INTEGER NACCESS            ! ACCESS COUNTER IN STATUS RECORD
      INTEGER NBUL, NBULLS       ! BULLETIN RANGE NO. & NUMBER OF RANGES
      INTEGER NCOL               ! COUNTER INDEX (2/3 = IS/ISN'T STORED)
      INTEGER NDATA              ! LOCATION OF DATA SET IN STATUS RECORD
      INTEGER NDAY, NDAY0        ! CURRENT AND START CENTURY DAYS
      INTEGER NDREC              ! NO. OF CURRENT DATA SET STATUS RECORD
      INTEGER NDSRECS            ! NO. OF DATA SET STATUS RECORDS IN HK.
      INTEGER NEXTDS             ! NUMBER ASSIGNED TO NEXT DATA SET
      INTEGER NFILES             ! NUMBER OF DATA SETS PROCESSED SO FAR
      INTEGER NFOUND(MAXSTOR)    ! BULLETINS OF EACH TYPE IN DATA SET
      INTEGER NFTOTL             ! BULLETINS TO STORE IN DATA SET
      INTEGER NMSGS              ! NUMBER OF MESSAGES FOUND IN DATA SET
      INTEGER NOW(8)             ! DATE/TIME OF RECEIPT (FROM 'DATIM')
      INTEGER NSEC, NSEC0        ! CURRENT AND START SECONDS FROM 00Z
      INTEGER NSECS              ! SECS SINCE H.K. DATA SET WAS REBUILT
      INTEGER NSTORE             ! NUMBER OF STORAGE DATA SET TYPES
      INTEGER NTOTAL             ! TOTAL OF MESSAGES FOUND SO FAR
      INTEGER NUMDS              ! MAXIMUM NUMBER OF DATA SET SLOTS
!
      LOGICAL STORFLAG           ! FLAG FOR STORAGE REQUIRED
      LOGICAL TERMIN8            ! OPERATOR'S TERMINATION FLAG
!
      CHARACTER ZERO, ONE        ! HEX "00" AND "01"
      CHARACTER S                ! 'S', OR ' ' IF ONLY 1 BULLETIN
      CHARACTER*2 ZZ             ! HEX "0000" (USED FOR CHECKING FLAGS)
      CHARACTER*3 CRCRLF         ! ASCII 'CR-CR-LF'
      CHARACTER*4 JOBFLAG        ! STATUS FLAGS FOR STORAGE JOB
      CHARACTER*7 MIMJ           ! 'MIMIMJMJ' FROM MESSAGE
      CHARACTER*8 DATYPE         ! CODE FOR DATA TYPE
      CHARACTER*8 STORTYPE(MAXSTOR) ! LIST OF STORAGE DATA SET CODES
      CHARACTER*18 BUL18         ! 'TTAAII CCCC YYGGGG' FROM MESSAGE
      CHARACTER*28672 BULL       ! 28K BUFFER
      CHARACTER*(LENREC) RECORD(MAXREC) ! HOUSEKEEPING DATA SET RECORDS
      CHARACTER*132 HEAD         ! FOR REVISION INFORMATION
!
!                        Variables relating to bulletin header data set
!                     (See Appendix B of storage Tech Note for details)
!
      INTEGER ITEMS(0:NITEMS,MAXHED)           ! PROCESSING ITEMS
      LOGICAL FLAGS(NFLAGS,MAXHED)             ! PROCESSING FLAGS
      CHARACTER*6 HEAD1(MAXHED), HEAD2(MAXHED) ! HEADER RANGES
      CHARACTER*8 BULTYP(MAXHED)               ! BULLETIN TYPES
!
!-----------------------------------------------------------------------
!     Common block (for dynamic allocation only) and namelist
!-----------------------------------------------------------------------
!
      COMMON /COMBUF/ BULL, RECORD
!                                                              Namelist
      CHARACTER*8       OWNER
      LOGICAL                   PRINT,  CLEAR
      INTEGER                                  NWAIT, MODE, NDREG
      NAMELIST /INSTOR/ OWNER,  PRINT,  CLEAR, NWAIT, MODE, NDREG
      DATA              OWNER,  PRINT,  CLEAR, NWAIT, MODE, NDREG
     &                /'SDB1', .TRUE., .FALSE.,   30,    0,    99/
!
!                                                       Data statements
      DATA NFILES/0/, NMSGS/0/, NTOTAL/0/, IWAIT/0/
      DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/
!
!-----------------------------------------------------------------------
!     Revision information
!-----------------------------------------------------------------------
!
      HEAD='
     &$Source: /home/us0400/mdb/op/lib/source/RCS/monitor.f,v $
     &'//'$Date: 30/01/2006 20:23:24$ $Revision: 1$'
!
!-----------------------------------------------------------------------
!     Initialisations and setup
!-----------------------------------------------------------------------
!
!                      Read bulletin header ranges & processing details
      NBULLS = MAXHED
      NSTORE = MAXSTOR
      CALL INITBULL (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, NBULLS,
     &               STORTYPE, NSTORE)
!
!                       Sort storage data types into alphabetical order
!                                           (NFOUND used as work array)
      CALL ABC (STORTYPE, NFOUND, NSTORE)
!
!-----------------------------------------------------------------------
!     Open housekeeping data set (which must be specified on a DD
!     statement with ddname "HKEEP")
!-----------------------------------------------------------------------
!
      OPEN (1, FILE='HKEEP', STATUS='OLD', FORM='UNFORMATTED',
     &      ACCESS='DIRECT', RECL=LENREC, IOSTAT=IOS)
!
!                         I/O error: Terminate with message to operator
      IF (IOS.NE.0) THEN
         WRITE (6,'(/T8,A,I4//T5,A)')
     &         'ERROR OPENING HOUSEKEEPING DATA SET - "IOSTAT" =', IOS,
     &         'JOB WILL NOW TERMINATE WITH USER RETURN CODE 800'
         CALL TELLOPS
     &      ('MDB(E):  I/O ERROR OPENING HOUSEKEEPING DATA SET.')
         CALL SYSABN (800)
      END IF
!
!-----------------------------------------------------------------------
!     Read the status record (record 2 of the housekeeping data set).
!     Set default value of "CLEAR" to "true" if flag is set to indicate
!     that storage was terminated last time with a request to wait
!     until all processed data had been stored. This forces the whole
!     housekeeping data set to be recreated from scratch this time.
!-----------------------------------------------------------------------
!
      ZERO = CHAR(0)
      ONE  = CHAR(1)
      ZZ   = ZERO//ZERO
      READ (1, REC=2) JOBFLAG
      CLEAR = JOBFLAG(1:2).EQ.ZZ
!                                                    Read namelist data
      OPEN (5, IOSTAT=IOS)
      IF (IOS.EQ.0) THEN
         READ (5, INSTOR, IOSTAT=IOS)
         IF (IOS.EQ.0) THEN
            IF (MODE.EQ.2) OWNER = ZZ//ZZ//ZZ//ZZ
!                                               Set unit for dregs d.s.
            IF (NDREG.NE.99)                            !2.0 (next line)
     &         CALL DREG (NDREG, 'DREGUNIT', ' ',' ',' ',' ', NOW, ' ')
         END IF
      END IF
      CLOSE (5)
!                      Read and/or initialise the housekeeping data set
!
      CALL HKSTART (CLEAR, STORTYPE, NSTORE, RECORD, NDAY0, NSEC0)
      READ (RECORD(1)(25:28),'(A4)') NDSRECS
      NUMDS = 10*NDSRECS
!                                                 Other initialisations
!
      CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF in ASCII
      NFTOTL = 0
      DO J=1,MAXSTOR
         NFOUND(J) = 0 ! (Bulletin counters)
      END DO ! J
      READ (RECORD(2)(13:16),'(A4)') LATEST ! Last data set read
      CALL REPLYTU (JOBFLAG) ! Issues message to operator first time
      NEXTDS = -1
      TERMIN8 = .FALSE.
!
!=======================================================================
!                 MAIN LOOP OVER INCOMING BULLETINS
!                 ---------------------------------
!     Keep processing until termination flag ("TERMIN8") is set.
!=======================================================================
!
      DO WHILE (.NOT.TERMIN8)
!
!-----------------------------------------------------------------------
!        Locate next message and check return code.
!        "NEXTDS" will be <0 if there is no free space in the
!        housekeeping data set to store information for another
!        storage data set. In this case "MSGCODE" is set to 2 (i.e.
!        "no data sets waiting") so that the job waits.
!-----------------------------------------------------------------------
!
         IF (NEXTDS.GT.0) THEN
            CALL FINDMHS (10, OWNER, BULL, MSTART, MLNGTH, MSGCODE)
         ELSE
            MSGCODE = 2 ! to force job to wait
         END IF
!
!-----------------------------------------------------------------------
!        "MSGCODE" = 0:  another bulletin found in current data set
!-----------------------------------------------------------------------
!
         IF (MSGCODE.EQ.0) THEN
            MEND = MSTART + MLNGTH - 1
!
!              Find first byte after 'CRCRLF' at end of header. (Header
!              is usually 41 bytes but may be 45, e.g. 'AMD' or 'COR'.)
!
            INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)          !2.1
            IF (INDX.LE.0) INDX = 5 !'CRCRLF' not found: assume 45 bytes
            MFIRST = MSTART + INDX + 40                             !2.1
!
!                        Extract bulletin header ('TTAAII CCCC YYGGGG')
!
            BUL18 = BULL(MSTART+20:MSTART+37)                       !2.1
            CALL EBCDIC (18, BUL18)
!                                      Look up list of header ranges to
!                      find range containing header of current bulletin
!
            CALL SATYPE (BUL18(1:6), HEAD1, HEAD2, NBULLS, NBUL)
            CALL DATIM (NOW)
!
            IF (NBUL.LE.0) THEN
!                                                 Unidentified bulletin
               DATYPE = 'UNKNOWN '
               STORFLAG = .FALSE.
               INDX = 0
!                                                       Printed message
               WRITE (6,'(T5,A,T15,2A)') 'MONITOR:',
     &                  'UNIDENTIFIED BULLETIN HEADER - ', BUL18
!
!                                      Dregs message (ASCII --> EBCDIC)
               S = BUL18(1:1)
               IF ((S.LT.'G'.AND.S.NE.'D') .OR. S.GT.'R' .OR. S.EQ.'N')
     &            CALL EBCDIC (MLNGTH-4, BULL(MSTART+4:MEND))
               CALL DREG (1, 'UNIDENTIFIED BULLETIN HEADER.',
     &                    BULL(MFIRST:MEND), 'MONITOR', '??? ',
     &                    BUL18, NOW, ' ')
            ELSE
!                                                      Set storage flag
               DATYPE = BULTYP(NBUL)
               STORFLAG = FLAGS(1,NBUL)
               INDX = ITEMS(0,NBUL)  ! (Row number in summary table)
!
!                                   Pass information to DREG subroutine
!
               CALL DREG (0, ' ', ' ', ' ', DATYPE, BUL18, NOW, ' ')
!
!                   If wanted ASCII data, do ASCII to EBCDIC conversion
!
               IF (STORFLAG .AND. ITEMS(1,NBUL).EQ.2) THEN
                  CALL EBCDIC (MLNGTH-4, BULL(MSTART+4:MEND))
!
!                              Do additional storage checks if required
!
                  IF (FLAGS(2,NBUL)) CALL STORCHEK
     &               (BULL(MFIRST:MEND), BUL18, DATYPE, STORFLAG, MIMJ)
               END IF
            END IF
!
!-----------------------------------------------------------------------
!           If still wanted, store details of data set contents.
!-----------------------------------------------------------------------
!
            NCOL = 3  ! (Default = 'rejects' column of summary table)
!
            IF (STORFLAG) THEN
!                                  Look up list of data types to locate
!                                that corresponding to current bulletin
!
               CALL SATYPE (DATYPE, STORTYPE, STORTYPE, NSTORE, I)
               IF (I.GT.0) THEN
                  NFOUND(I) = NFOUND(I) + 1
                  NFTOTL = NFTOTL + 1
                  NCOL = 2    ! ('Stored' column of summary table)
               END IF
            END IF
!                                            Output message if required
!
            IF (NBUL.GT.0 .AND. ITEMS(2,NBUL).GE.1) THEN
               IF (STORFLAG) THEN
                  WRITE (6,'(T2,3A)') BUL18, ':  DATA TYPE ', DATYPE
               ELSE
                  WRITE (6,'(T2,4A)') BUL18, ':  DATA TYPE ', DATYPE,
     &                              ' STORAGE NOT REQUIRED.'
               END IF
            END IF
!                     Update statistics for summary table at end of job
!
            KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
            KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1
            NMSGS = NMSGS + 1
            NTOTAL = NTOTAL + 1
!
!-----------------------------------------------------------------------
!        "MSGCODE" = 2:  no nore data sets waiting to be processed
!                       (or no free slot in data set status records)
!-----------------------------------------------------------------------
!
         ELSE IF (MSGCODE.EQ.2) THEN
!
!                                Set termination flag if mode 1 (unless
!                                just waiting for a free data set slot)
!
            IF (MODE.EQ.1 .AND. NEXTDS.GT.0) THEN
               RECORD(2)(1:3) = ZZ//ONE
               TERMIN8 = .TRUE.
!                                 If in mode 2 and main run termination
!                            flag is set, set same flag in parallel run
!                          (unless just waiting for free data set slot)
!
            ELSE IF (MODE.EQ.2 .AND.
     &               OWNER(2:3).NE.ZZ .AND. NEXTDS.GT.0) THEN
               RECORD(2)(1:3) = ZERO//OWNER(2:3)
               TERMIN8 = .TRUE.
!                                      No termination - just wait a bit
            ELSE
               CALL SECSOUT (IWAIT)
               IWAIT = NWAIT
            END IF
!
!-----------------------------------------------------------------------
!        "MSGCODE" < 0:  unrecoverable mhs error - terminate job
!-----------------------------------------------------------------------
!
         ELSE IF (MSGCODE.LT.0) THEN
            RECORD(2)(1:3) = ZZ//ONE
            TERMIN8 = .TRUE.
!
!-----------------------------------------------------------------------
!        "MSGCODE" = 1:  finished with data set
!-----------------------------------------------------------------------
!
         ELSE IF (MSGCODE.EQ.1) THEN
            NFILES = NFILES + 1
!                                        Information message (optional)
            IF (PRINT) THEN
               S = 'S'
               IF (NMSGS.EQ.1) S = ' '
               CALL DATIM (NOW)
               WRITE (6,'(T2, I2.2,A,I2.2,A,I2.2, I6, 4A)')
     &                NOW(5), ':', NOW(4), ':', NOW(3), NMSGS,
     &               ' BULLETIN', S, ' PROCESSED FROM  ', BULL(9:52)
            END IF
            NMSGS = 0 ! (Reset)
!                                         Locate data set status record
!
            IF (NFTOTL.GT.0) THEN ! At least 1 wanted bulletin
               LATEST = NEXTDS
               BULL(12:12) = 'P' ! (because renamed to "MHSP")
               NDREC = MOD(LATEST,NUMDS)/10 + 3 ! status record number
               NDATA = MOD(LATEST,10) + 1 ! slot no. in status record
!
!                    Update status record. (When "MSGCODE"=1, "FINDMHS"
!               returns the data set name starting in byte 9 of "BULL")
!
               RECORD(NDREC)(NDATA:NDATA) = CHAR(NFTOTL)
               I = LENTRY*(NDATA-1) + 10            ! start of slot
               WRITE (RECORD(NDREC)(I+1:I+4),'(A4)') LATEST ! d.s. nmbr.
               RECORD(NDREC)(I+5:I+44) = BULL(9:48) ! data set name
!
!                               Set bytes indicating data types present
               I = I + 44
               DO J=1,NSTORE
                  RECORD(NDREC)(I+J:I+J) = CHAR(NFOUND(J))
               END DO ! J
               WRITE (1,REC=NDREC) RECORD(NDREC)
!
!                                       Occasional clearout of old data
!
               IF (MOD(NFILES,20).EQ.0) CALL HKUPDATE (RECORD)
!
!                                            Reset observation counters
               NFTOTL = 0
               DO J=1,MAXSTOR
                  NFOUND(J) = 0
               END DO ! J
            END IF
         END IF
!
!-----------------------------------------------------------------------
!        If not processing a data set, update record 2
!-----------------------------------------------------------------------
!
         IF (MSGCODE.NE.0) THEN
!                                             Check for job termination
            CALL REPLYTU (JOBFLAG)
            IF (JOBFLAG(1:1).NE.' ') THEN
               TERMIN8 = .TRUE.
               IF (JOBFLAG(1:1).EQ.'U') THEN       ! Rapid stop
                  RECORD(2)(1:3) = ZERO//ONE//ZERO
               ELSE                                ! Tidy stop
                  RECORD(2)(1:3) = ZZ//ONE
               END IF
            END IF
!                                                      Get current time
            CALL DATIM (NOW)
            CALL DATE31 (NOW(6), NOW(7), NOW(8), NDAY)
            NSEC = (NOW(5)*60 + NOW(4))*60 + NOW(3)
            NSECS = 86400*(NDAY-NDAY0) + (NSEC-NSEC0)
!
!                                     Update housekeeping status record
!
            READ (RECORD(2),'(T9,A4)') NACCESS
            NACCESS = NACCESS + 1
            WRITE (RECORD(2)(5:16),'(3A4)') NSECS, NACCESS, LATEST
            WRITE (1,REC=2) RECORD(2)
!
!-----------------------------------------------------------------------
!           Find a free number ("NEXTDS") for the next data set.
!           This will be returned as -1 if there is no free slot
!           in the housekeeping data set.
!-----------------------------------------------------------------------
!
            CALL FINDREC (RECORD, NEXTDS)
         END IF
!                                          Go back for the next message
      END DO ! WHILE
!
!=======================================================================
!     SUMMARY TABLE OF MESSAGES PROCESSED AND TERMINATION MESSAGES.
!=======================================================================
!
      NBUL = 0
!                        Convert BULTYP array to row headings for table
!                                  (i.e. removes duplicated data types)
      DO J=1,NBULLS
         INDX = ITEMS(0,J)
         BULTYP(INDX) = BULTYP(J)
         NBUL = MAX0(NBUL,INDX)
      END DO ! J
!                                                         Summary table
      CALL SUMMARY (NFILES, NTOTAL, KOUNTS, BULTYP, NBUL)
!
!                             Terminate data processing by storage jobs
      KODE = 0
      IF (RECORD(2)(3:3).NE.ZERO) KODE = 1
      CALL HKSTOP (RECORD, KODE)
!                                  Messages for unrecoverable MHS error
      IF (MSGCODE.LT.0) THEN
         J = LEN(OWNER)
         INDX = INDEX(OWNER,' ') - 1
         IF (INDX.LE.0) INDX = J
         WRITE (6,'(/T9,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',
     &            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
         IF (MSGCODE.EQ.-1) THEN
            WRITE (6,'(T9,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
            WRITE (6,'(T9,6A)') 'TRY RE-RUNNING THE JOB: IF THE ',
     &               'PROBLEM RECURS, TRY DELETING THE NEXT "',
     &               OWNER(1:INDX), '" DATA SET (LOOK FOR "MHSR.*.*.',
     &               OWNER(1:INDX), '.S000").'
         ELSE IF (MSGCODE.EQ.-2) THEN
            WRITE (6,'(T9,2A/)') 'RENAME THE DATA SET ', BULL(9:52)
            WRITE (6,'(T9,2A)') 'CHECK THAT THE CORRESPONDING ',
     &               'DATA SET BEGINNING "MHSP" DOES NOT EXIST.',
     &               'IF IT DOES, DELETE ONE OR THE OTHER; CHECK ', !2.1
     &               'FOR SIMILAR DUPLICATES; THEN RERUN.'          !2.1
         END IF
!                                    Send message to operator and ABEND
         KODE = 800 - MSGCODE
         WRITE (6,'(/T9,2A,I4/)') 'STORAGE JOB WILL NOW ABEND ',
     &                            'WITH USER CODE', KODE
         CALL TELLOPS ('MDB(E):  FATAL ERROR IN MHS ROUTINE "MHSIFF".')
         CALL SYSABN (KODE)
      END IF
!
      STOP
      END
