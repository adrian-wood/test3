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
!                OWNER   C*8  MHS DATA SET OWNER             'SDB1'
!                MHSTYPE C*4  MHS dataset type 'OPER' to     'TEST'
!                             process MHSR datasets 'TEST' to
!                             process MHSP datasets.
!                RENAME  L*4  FLAG to say whether to rename   .TRUE.
!                             MHS datasets or not. Only used
!                             for integration testing.
!
! CALLS       : ABC, DATE31, DATIM, DREG, EBCDIC, FINDMHS, FINDREC,
!               HKSTART, HKSTOP, HKUPDATE, INITBULL, REPLYTU, SATYPE,
!               SECSOUT, STORCHEK, SUMMARY, SYSABN, TELLOPS
!               METDB_COPEN, METDB_CREAD_DIR and METDB_CWRITE_DIR from
!                 MetDB_c_utils.C
!
! FILES USED  : MET.D.B. HOUSEKEEPING DATA SET OPENED ON UNIT 1.
!               UNIT 5 (NAMELIST INPUT) OPENED, READ AND AND CLOSED.
!
! REVISION INFO :
!
! $Workfile: monitor.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 10/10/2011 13:03:39$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         10/10/2011 13:03:39    Sheila Needham
!       Initialise NTOTAL
!  8    MetDB_Refresh 1.7         27/09/2011 09:11:32    Sheila Needham
!       Corrected typo
!  7    MetDB_Refresh 1.6         16/09/2011 10:22:24    Sheila Needham  Fix
!       MHS dataset name put in the HKDS for integration testing (no renaming)
!  6    MetDB_Refresh 1.5         07/06/2011 16:05:44    Brian Barwell   OPEN
!       for namelist modified. IF block for print statment restructured.
!       MSTYPE used to set output MHS names.
!  5    MetDB_Refresh 1.4         15/04/2011 14:54:16    Sheila Needham  Added
!       variable MHSTYPE to &INSTOR defaulted to 'TEST'
!  4    MetDB_Refresh 1.3         04/03/2011 09:26:46    Sheila Needham
!       Replace HKDS I/O with C routines and correct internal reads
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
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:

USE abc_mod
USE datim_mod
USE dreg_mod
USE ebcdic_mod
USE findmhs_mod
USE findrec_mod
USE hkstart_mod
USE hkupdate_mod
USE hkstop_mod
USE initbull_mod
USE int2ch_mod !function
!USE replytu_mod  !Assembler
USE satype_mod
!USE secsout_mod     ! C Routine
USE storchek_mod
USE summary_mod
!USE sysabn_mod
!USE tellops_mod
USE zpdate_mod

IMPLICIT NONE

! Local declarations:
!                          Parameters relating to housekeeping data set

INTEGER,     PARAMETER ::  LENREC=950        ! RECORD LENGTH OF HOUSEKEEPING DATA SET
INTEGER,     PARAMETER ::  MAXSTOR=50        ! MAXIMUM NUMBER OF STORAGE DATA SET CODES
INTEGER,     PARAMETER ::  MAXJOBS=50        ! MAXIMUM NUMBER OF STORAGE JOBS
INTEGER,     PARAMETER ::  NDRECS=160        ! NUMBER OF DATA SET STATUS RECORDS
INTEGER,     PARAMETER ::  MAXREC=2+NDRECS    ! SIZE OF "RECORD" ARRAY
INTEGER,     PARAMETER ::  LENTRY=MAXSTOR+44 ! NUMBER OF BYTES IN DATA SET STATUS ENTRY
INTEGER,     PARAMETER ::  IUNIT=1           ! FT number for HKDS

!                       Parameters relating to bulletin header data set

INTEGER,     PARAMETER ::  MAXHED=160 ! MAXIMUM NUMBER OF BULLETIN HEADER RANGES
INTEGER,     PARAMETER ::  NFLAGS=9   ! NUMBER OF DATA PROCESSING FLAGS
INTEGER,     PARAMETER ::  NITEMS=12  ! NUMBER OF DATA PROCESSING ITEMS

!                                                             Variables
INTEGER          ::  INDX  ! POINTER FOR MSG OR SUMMARY TABLE
INTEGER          ::  IOS   ! STATUS CODE FROM I/O STATEMENT
INTEGER          ::  I     ! VARIABLE FOR LOCAL USE
INTEGER          ::  INT4  ! Dummy variable for TRANSFER function
INTEGER          ::  IWAIT=0 ! WAITING TIME FOR NEXT DATA SET
INTEGER          ::  J     ! VARIABLE FOR LOCAL USE
INTEGER          ::  KODE  ! RETURN CODE (E.G. FROM 'DSINFO')
INTEGER          ::  KOUNTS(3,0:MAXHED) ! COUNTERS FOR NUMBERS OF MESSAGES
INTEGER          ::  LATEST ! HIGHEST DATA SET NUMBER IN H.K.
INTEGER          ::  MEND  ! FINISH BYTES OF MESSAGE
INTEGER          ::  MFIRST ! FIRST BYTE OF MESSAGE AFTER HEADER
INTEGER          ::  MLNGTH ! MESSAGE LENGTH
INTEGER          ::  IOMODE   ! I/O Mode for C Open
INTEGER          ::  MSGCODE ! RETURN CODE FROM "FINDMSG"
INTEGER          ::  MSTART ! START BYTES OF MESSAGE
INTEGER          ::  NACCESS ! ACCESS COUNTER IN STATUS RECORD
INTEGER          ::  NBUL  ! BULLETIN RANGE NO.
INTEGER          ::  NBULLS ! NUMBER OF BULLETIN RANGES
INTEGER          ::  NCOL  ! COUNTER INDEX (2/3 = IS/ISN'T STORED)
INTEGER          ::  NDATA ! LOCATION OF DATA SET IN STATUS RECORD
INTEGER          ::  NDAY  ! CURRENT CENTURY DAY
INTEGER          ::  NDAY0 ! START CENTURY DAY
INTEGER          ::  NDREC ! NO. OF CURRENT DATA SET STATUS RECORD
INTEGER          ::  NDSRECS ! NO. OF DATA SET STATUS RECORDS IN HK.
INTEGER          ::  NEXTDS ! NUMBER ASSIGNED TO NEXT DATA SET
INTEGER          ::  NFILES=0 ! NUMBER OF DATA SETS PROCESSED SO FAR
INTEGER          ::  NFOUND(MAXSTOR) ! BULLETINS OF EACH TYPE IN DATA SET
INTEGER          ::  NFTOTL=0 ! BULLETINS TO STORE IN DATA SET
INTEGER          ::  NMSGS=0 ! NUMBER OF MESSAGES FOUND IN DATA SET
INTEGER          ::  NOW(8) ! DATE/TIME OF RECEIPT (FROM 'DATIM')
INTEGER          ::  NSEC  ! CURRENT SECONDS FROM 00Z
INTEGER          ::  NSEC0 ! START SECONDS FROM 00Z
INTEGER          ::  NSECS ! SECS SINCE H.K. DATA SET WAS REBUILT
INTEGER          ::  NSTORE ! NUMBER OF STORAGE DATA SET TYPES
INTEGER          ::  NTOTAL=0 ! TOTAL OF MESSAGES FOUND SO FAR
INTEGER          ::  NUMDS ! MAXIMUM NUMBER OF DATA SET SLOTS
INTEGER          ::  RECNO ! Housekeeping dataset record number

LOGICAL          ::  STORFLAG ! FLAG FOR STORAGE REQUIRED
LOGICAL          ::  TERMIN8 ! OPERATOR'S TERMINATION FLAG

CHARACTER(LEN=1) ::  ONE         ! HEX "01"
CHARACTER(LEN=1) ::  ZERO        ! HEX "00"
CHARACTER(LEN=1) ::  S           ! 'S', OR ' ' IF ONLY 1 BULLETIN
CHARACTER(LEN=2) ::  ZZ          ! HEX "0000" (USED FOR CHECKING FLAGS)
CHARACTER(LEN=3) ::  CRCRLF      ! ASCII 'CR-CR-LF'
CHARACTER(LEN=4) ::  JOBFLAG     ! STATUS FLAGS FOR STORAGE JOB
CHARACTER(LEN=4) ::  CH4         ! Dummy variable for TRANSFER function
CHARACTER(LEN=7) ::  MIMJ        ! 'MIMIMJMJ' FROM MESSAGE
CHARACTER(LEN=8) ::  DATYPE      ! CODE FOR DATA TYPE
CHARACTER(LEN=8) ::  STORTYPE(MAXSTOR) ! LIST OF STORAGE DATA SET CODES
CHARACTER(LEN=9)  :: HKDS        ! Housekeeping dataset ddname
CHARACTER(LEN=18) ::  BUL18      ! 'TTAAII CCCC YYGGGG' FROM MESSAGE
CHARACTER(LEN=28672) ::  BULL    ! 28K BUFFER
CHARACTER(LEN=LENREC) ::  RECORD(MAXREC) ! HOUSEKEEPING DATA SET RECORDS

!                        Variables relating to bulletin header data set
!                     (See Appendix B of storage Tech Note for details)

INTEGER          ::  ITEMS(0:NITEMS,MAXHED) ! PROCESSING ITEMS
LOGICAL          ::  FLAGS(NFLAGS,MAXHED) ! PROCESSING FLAGS
CHARACTER(LEN=6) :: HEAD1(MAXHED) ! HEADER RANGES
CHARACTER(LEN=6) :: HEAD2(MAXHED) ! HEADER RANGES
CHARACTER(LEN=8) :: BULTYP(MAXHED)    ! BULLETIN TYPES
!                                                              Namelist
CHARACTER(LEN=8)  ::  OWNER   = 'SDB1'
LOGICAL           ::  PRINT   = .TRUE.
LOGICAL           ::  CLEAR   = .FALSE.
INTEGER           ::  NWAIT   = 30
INTEGER           ::  MODE    = 0
INTEGER           ::  NDREG   = 99
CHARACTER(LEN=4)  ::  MHSTYPE = 'TEST'
LOGICAL           ::  RENAME = .TRUE.
NAMELIST /INSTOR/ OWNER, PRINT, CLEAR, NWAIT, MODE, NDREG, MHSTYPE, &
                  RENAME

!                                                       Data statements
DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/

!-----------------------------------------------------------------------
!     Initialisations and setup
!-----------------------------------------------------------------------
!                      Read bulletin header ranges & processing details
NBULLS = MAXHED
NSTORE = MAXSTOR
CALL INITBULL (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, NBULLS, STORTYPE, &
               NSTORE)
!                       Sort storage data types into alphabetical order
!                                           (NFOUND used as work array)
CALL ABC (STORTYPE, NFOUND, NSTORE)

!-----------------------------------------------------------------------
!     Open housekeeping data set (which must be specified on a DD
!     statement with ddname "HKEEP")
!-----------------------------------------------------------------------

HKDS = 'DD:HKEEP'//CHAR(0)
IOMODE = 3                     ! readwrite
CALL METDB_COPEN(IUNIT,HKDS,IOMODE,IOS)

!                         I/O error: Terminate with message to operator
IF (IOS /= 0) THEN
   WRITE (6,'(/T8,A,I4//T5,A)') &
         'ERROR OPENING HOUSEKEEPING DATA SET - "IOSTAT" =', IOS, &
         'JOB WILL NOW TERMINATE WITH USER RETURN CODE 800'
   CALL TELLOPS ('MDB(E):  I/O ERROR OPENING HOUSEKEEPING DATA SET.')
   CALL SYSABN (800)
END IF

!-----------------------------------------------------------------------
!     Read the status record (record 2 of the housekeeping data set).
!     Set default value of "CLEAR" to "true" if flag is set to indicate
!     that storage was terminated last time with a request to wait
!     until all processed data had been stored. This forces the whole
!     housekeeping data set to be recreated from scratch this time.
!-----------------------------------------------------------------------

ZERO = CHAR(0)
ONE  = CHAR(1)
ZZ   = ZERO//ZERO
RECNO = 2
CALL METDB_CREAD_DIR(IUNIT,RECORD(2),LENREC,RECNO,IOS)
JOBFLAG = RECORD(2)(1:2)
CLEAR = JOBFLAG(1:2) == ZZ
!                                                    Read namelist data
OPEN (5, FILE='DD:FT05F001', ACTION='READ', IOSTAT=IOS)
IF (IOS == 0) THEN
   READ (5, INSTOR, IOSTAT=IOS)
   IF (IOS == 0) THEN
      IF (MODE == 2) OWNER = ZZ//ZZ//ZZ//ZZ
!                                               Set unit for dregs d.s.
      IF (NDREG /= 99) &
         CALL DREG (NDREG, 'DREGUNIT', ' ',' ',' ',' ', NOW, ' ')
   END IF
END IF
CLOSE (5)
!                      Read and/or initialise the housekeeping data set

CALL HKSTART (CLEAR, STORTYPE, NSTORE, RECORD, NDAY0, NSEC0)
NDSRECS = TRANSFER(RECORD(1)(25:28),INT4)
NUMDS = 10*NDSRECS
!                                                 Other initialisations

CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF in ASCII
NFTOTL = 0
DO J=1,MAXSTOR
   NFOUND(J) = 0 ! (Bulletin counters)
END DO ! J
LATEST = TRANSFER(RECORD(2)(13:16),INT4) ! Last dataset read
CALL REPLYTU (JOBFLAG) ! Issues message to operator first time
NEXTDS = -1
TERMIN8 = .FALSE.

!=======================================================================
!                 MAIN LOOP OVER INCOMING BULLETINS
!                 ---------------------------------
!     Keep processing until termination flag ("TERMIN8") is set.
!=======================================================================

DOLABEL1: &
DO WHILE (.NOT.TERMIN8)

!-----------------------------------------------------------------------
!        Locate next message and check return code.
!        "NEXTDS" will be <0 if there is no free space in the
!        housekeeping data set to store information for another
!        storage data set. In this case "MSGCODE" is set to 2 (i.e.
!        "no data sets waiting") so that the job waits.
!-----------------------------------------------------------------------

   IF (NEXTDS > 0) THEN
      CALL FINDMHS (10, OWNER, BULL, MSTART, MLNGTH, MSGCODE, MHSTYPE)
   ELSE
      MSGCODE = 2 ! to force job to wait
   END IF

!-----------------------------------------------------------------------
!        "MSGCODE" = 0:  another bulletin found in current data set
!-----------------------------------------------------------------------

IFLABEL1: &
   IF (MSGCODE == 0) THEN
      MEND = MSTART + MLNGTH - 1

!              Find first byte after 'CRCRLF' at end of header. (Header
!              is usually 41 bytes but may be 45, e.g. 'AMD' or 'COR'.)

      INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)
      IF (INDX <= 0) INDX = 5 !'CRCRLF' not found: assume 45 bytes
      MFIRST = MSTART + INDX + 40

!                        Extract bulletin header ('TTAAII CCCC YYGGGG')

      BUL18 = BULL(MSTART+20:MSTART+37)
      CALL EBCDIC (18, BUL18)
!                                      Look up list of header ranges to
!                      find range containing header of current bulletin

      CALL SATYPE (BUL18(1:6), HEAD1, HEAD2, NBULLS, NBUL)
      CALL DATIM (NOW)

IFLABEL2: &
      IF (NBUL <= 0) THEN
!                                                 Unidentified bulletin
         DATYPE = 'UNKNOWN '
         STORFLAG = .FALSE.
         INDX = 0
!                                                       Printed message
         WRITE (6,'(T5,A,T15,2A)') 'MONITOR:', &
                  'UNIDENTIFIED BULLETIN HEADER - ', BUL18

!                                      Dregs message (ASCII --> EBCDIC)
         S = BUL18(1:1)
         IF ((S < 'G'.AND.S /= 'D') .OR. S > 'R' .OR. S == 'N') &
            CALL EBCDIC (MLNGTH-4, BULL(MSTART+4:MEND))
         CALL DREG (1, 'UNIDENTIFIED BULLETIN HEADER.', &
                    BULL(MFIRST:MEND), 'MONITOR', '??? ', &
                    BUL18, NOW, ' ')
      ELSE
!                                                      Set storage flag
         DATYPE = BULTYP(NBUL)
         STORFLAG = FLAGS(1,NBUL)
         INDX = ITEMS(0,NBUL)  ! (Row number in summary table)

!                                   Pass information to DREG subroutine

         CALL DREG (0, ' ', ' ', ' ', DATYPE, BUL18, NOW, ' ')

!                   If wanted ASCII data, do ASCII to EBCDIC conversion

         IF (STORFLAG .AND. ITEMS(1,NBUL) == 2) THEN
            CALL EBCDIC (MLNGTH-4, BULL(MSTART+4:MEND))

!                              Do additional storage checks if required

            IF (FLAGS(2,NBUL)) CALL STORCHEK &
               (BULL(MFIRST:MEND), BUL18, DATYPE, STORFLAG, MIMJ)
         END IF
      END IF IFLABEL2

!-----------------------------------------------------------------------
!           If still wanted, store details of data set contents.
!-----------------------------------------------------------------------

      NCOL = 3  ! (Default = 'rejects' column of summary table)

IFLABEL3: &
      IF (STORFLAG) THEN
!                                  Look up list of data types to locate
!                                that corresponding to current bulletin

         CALL SATYPE (DATYPE, STORTYPE, STORTYPE, NSTORE, I)
         IF (I > 0) THEN
            NFOUND(I) = NFOUND(I) + 1
            NFTOTL = NFTOTL + 1
            NCOL = 2    ! ('Stored' column of summary table)
         END IF
      END IF IFLABEL3
!                                            Output message if required
IFLABEL4: &
      IF (NBUL > 0) THEN
        IF (ITEMS(2,NBUL) >= 1) THEN
          IF (STORFLAG) THEN
             WRITE (6,'(T2,3A)') BUL18, ':  DATA TYPE ', DATYPE
          ELSE
             WRITE (6,'(T2,4A)') BUL18, ':  DATA TYPE ', DATYPE, &
                              ' STORAGE NOT REQUIRED.'
          END IF
        END IF
      END IF IFLABEL4
!                     Update statistics for summary table at end of job

      KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
      KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1
      NMSGS = NMSGS + 1
      NTOTAL = NTOTAL + 1

!-----------------------------------------------------------------------
!        "MSGCODE" = 2:  no nore data sets waiting to be processed
!                       (or no free slot in data set status records)
!-----------------------------------------------------------------------

   ELSE IF (MSGCODE == 2) THEN
!                                Set termination flag if mode 1 (unless
!                                just waiting for a free data set slot)
IFLABEL5: &
      IF (MODE == 1 .AND. NEXTDS > 0) THEN
         RECORD(2)(1:3) = ZZ//ONE
         TERMIN8 = .TRUE.
!                                 If in mode 2 and main run termination
!                            flag is set, set same flag in parallel run
!                          (unless just waiting for free data set slot)

      ELSE IF (MODE == 2 .AND. OWNER(2:3) /= ZZ .AND. NEXTDS > 0) THEN
         RECORD(2)(1:3) = ZERO//OWNER(2:3)
         TERMIN8 = .TRUE.
!                                      No termination - just wait a bit
      ELSE
         CALL SECSOUT (IWAIT)
         IWAIT = NWAIT
      END IF IFLABEL5

!-----------------------------------------------------------------------
!        "MSGCODE" < 0:  unrecoverable mhs error - terminate job
!-----------------------------------------------------------------------

   ELSE IF (MSGCODE < 0) THEN
      RECORD(2)(1:3) = ZZ//ONE
      TERMIN8 = .TRUE.

!-----------------------------------------------------------------------
!        "MSGCODE" = 1:  finished with data set
!-----------------------------------------------------------------------

   ELSE IF (MSGCODE == 1) THEN
      NFILES = NFILES + 1
!                                        Information message (optional)
      IF (PRINT) THEN
         S = 'S'
         IF (NMSGS == 1) S = ' '
         CALL DATIM (NOW)
         WRITE (6,'(T2, I2.2,A,I2.2,A,I2.2, I6, 4A)') &
                NOW(5), ':', NOW(4), ':', NOW(3), NMSGS, &
               ' BULLETIN', S, ' PROCESSED FROM  ', BULL(9:52)
      END IF
      NMSGS = 0 ! (Reset)
!                                         Locate data set status record
IFLABEL6: &
      IF (NFTOTL > 0) THEN ! At least 1 wanted bulletin
         LATEST = NEXTDS
!-----------------------------------------------------------------------
! Dataset name goes in HKDS.  For operational running MHSR datasets are
! renamed to MHSP, for test running MHSP datasets are renamed to MHST,  
! and for integration tests the datasets are not renamed so put MHSP
! in the HKDS.
!----------------------------------------------------------------------- 
         IF (MHSTYPE == 'TEST') THEN
           IF (RENAME) THEN
             BULL(12:12) = 'T' ! (Test: "MHSP" renamed to "MHST")
           ELSE
             BULL(12:12) = 'P' ! (Test: "MHSP" not renamed)
           END IF
         ELSE
           BULL(12:12) = 'P' ! (Opnl: "MHSR" renamed to "MHSP")
         END IF
         NDREC = MOD(LATEST,NUMDS)/10 + 3 ! status record number
         NDATA = MOD(LATEST,10) + 1 ! slot no. in status record

!                    Update status record. (When "MSGCODE"=1, "FINDMHS"
!               returns the data set name starting in byte 9 of "BULL")

         RECORD(NDREC)(NDATA:NDATA) = INT2CH(NFTOTL)
         I = LENTRY*(NDATA-1) + 10            ! start of slot
         RECORD(NDREC)(I+1:I+4) = TRANSFER(LATEST,CH4) ! d.s. nmbr.
         RECORD(NDREC)(I+5:I+44) = BULL(9:48) ! data set name

!                               Set bytes indicating data types present
         I = I + 44
         DO J=1,NSTORE
            RECORD(NDREC)(I+J:I+J) = INT2CH(NFOUND(J))
         END DO ! J
         CALL METDB_CWRITE_DIR(IUNIT,RECORD(NDREC),LENREC,NDREC,IOS)

!                                       Occasional clearout of old data

         IF (MOD(NFILES,20) == 0) CALL HKUPDATE (RECORD)

!                                            Reset observation counters
         NFTOTL = 0
         DO J=1,MAXSTOR
            NFOUND(J) = 0
         END DO ! J
      END IF IFLABEL6
   END IF IFLABEL1

!-----------------------------------------------------------------------
!        If not processing a data set, update record 2
!-----------------------------------------------------------------------

IFLABEL7: &
   IF (MSGCODE /= 0) THEN
!                                             Check for job termination
      CALL REPLYTU (JOBFLAG)
      IF (JOBFLAG(1:1) /= ' ') THEN
         TERMIN8 = .TRUE.
         IF (JOBFLAG(1:1) == 'U') THEN       ! Rapid stop
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

!                                     Update housekeeping status record

      NACCESS = TRANSFER(RECORD(2)(9:12),INT4)
      NACCESS = NACCESS + 1
      RECORD(2)(5:8)   = TRANSFER(NSECS,CH4)
      RECORD(2)(9:12)  = TRANSFER(NACCESS,CH4)
      RECORD(2)(13:16) = TRANSFER(LATEST,CH4)
      RECNO = 2
      CALL METDB_CWRITE_DIR(IUNIT,RECORD(2),LENREC,RECNO,IOS)

!-----------------------------------------------------------------------
!           Find a free number ("NEXTDS") for the next data set.
!           This will be returned as -1 if there is no free slot
!           in the housekeeping data set.
!-----------------------------------------------------------------------

      CALL FINDREC (RECORD, NEXTDS)
   END IF IFLABEL7
!                                          Go back for the next message
END DO DOLABEL1 ! WHILE

!=======================================================================
!     SUMMARY TABLE OF MESSAGES PROCESSED AND TERMINATION MESSAGES.
!=======================================================================

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

!                             Terminate data processing by storage jobs
KODE = 0
IF (RECORD(2)(3:3) /= ZERO) KODE = 1
CALL HKSTOP (RECORD, KODE)
!                                  Messages for unrecoverable MHS error
IFLABEL8: &
IF (MSGCODE < 0) THEN
   J = LEN(OWNER)
   INDX = INDEX(OWNER,' ') - 1
   IF (INDX <= 0) INDX = J
   WRITE (6,'(/T9,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ', &
            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
IFLABEL9: &
   IF (MSGCODE == -1) THEN
      WRITE (6,'(T9,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
      WRITE (6,'(T9,6A)') 'TRY RE-RUNNING THE JOB: IF THE ', &
               'PROBLEM RECURS, TRY DELETING THE NEXT "', &
               OWNER(1:INDX), '" DATA SET (LOOK FOR "MHSR.*.*.', &
               OWNER(1:INDX), '.S000").'
   ELSE IF (MSGCODE == -2) THEN
      WRITE (6,'(T9,2A/)') 'RENAME THE DATA SET ', BULL(9:52)
      WRITE (6,'(T9,2A)') 'CHECK THAT THE CORRESPONDING ', &
               'DATA SET BEGINNING "MHSP" DOES NOT EXIST.', &
               'IF IT DOES, DELETE ONE OR THE OTHER; CHECK ', &
               'FOR SIMILAR DUPLICATES; THEN RERUN.'
   END IF IFLABEL9
!                                    Send message to operator and ABEND
   KODE = 800 - MSGCODE
   WRITE (6,'(/T9,2A,I4/)') 'STORAGE JOB WILL NOW ABEND ', &
                            'WITH USER CODE', KODE
   CALL TELLOPS ('MDB(E):  FATAL ERROR IN MHS ROUTINE "MHSIFF".')
   CALL SYSABN (KODE)
END IF IFLABEL8

STOP
END PROGRAM MONITOR
