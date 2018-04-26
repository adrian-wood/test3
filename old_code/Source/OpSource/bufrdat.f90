PROGRAM BUFRDAT

!-----------------------------------------------------------------------
!
! PROGRAM     : BUFRDAT
!
! PURPOSE     : To store selected BUFR bulletins in the MetDB.
!
! DESCRIPTION : This job processes MHS data sets of BUFR bulletins on
!               the GPCS and stores the data in the MetDB.
!
!               Data sets must have fixed length records of size 4096
!               (GTS data via FROST) or 512 (data received by FTP).
!               The OWNER parameter in the namelist is used to identify
!               data sets to process and GTSDATA indicates which type
!               of data it is. FROST data and FTP data cannot be
!               processed in the same job.
!
!               The following is a brief summary of the main steps in
!               this job. The housekeeping data set is used for
!               communication with the MetDB monitor job.
!
!                - Read namelist input giving control information
!                - Read header details of bulletins to be stored
!                - Open storage data sets for all data types processed
!                - Open and initialise the housekeeping data set
!
!                - Loop over bulletins:
!                  - Get next bulletin, opening new FROST data set
!                    if necessary.
!
!                  - If end of data set reached:
!                    - Print diagnostic information messages if any.
!                    - Print 1-line information message (optional).
!
!                  - Else, if there are no new bulletins waiting:
!                    - Wait or terminate as required.
!
!                  - Else, if a new bulletin is found:-
!                    - Get GTS header (or make pseudo header).
!                    - Determine bulletin type from header.
!                    - Check bulletin quality & do other checks needed.
!                    - Identify appropriate storage data set.
!                    - Call BUFREP to store the bulletin.
!                    - Check return code for storage problems.
!
!                  - If not in the middle of a data set:-
!                    - Update housekeeping data set status record.
!                  - End if.
!                - End of loop over bulletins.
!
!                - Print summary of bulletins processed.
!                - Abend if unrecoverable MHS error.
!                - Stop.
!
! NAMELIST    : INSTOR  (Unit 2).  Contents as follows:
!
!               Variable Type        Description              Default
!               -------- ----        -----------              -------
!               JOBNAME  C*8  Job name.                      'MDB??? '
!               OWNER    C*8  MHS data set owner               'BUF1'
!               PRINT    L*4  Flag to produce 1-line message   .TRUE.
!                              for each data set processed.
!               GTSDATA  L*4  Flag to treat input data sets   .FALSE.
!                              as GTS data from FROST.
!               MODE     I*4  Mode for parallel or test run.     0
!                              (See storage T.N. for details)
!               NDREG    I*4  Unit no. for dregs data set        0
!                             (< or =0 if no data set).
!               NWAIT    I*4  No. of secs. to wait if there     30
!                              are no data sets to process.
!               MHSTYPE   C*4  MHS dataset type 'OPER' to     'TEST'
!                             process MHSR datasets 'TEST' to
!                             process MHSP datasets.
!
! CALLS       : BUFRCHEK, BUFREP, DATIM, DREG, DSINFO, EBCDIC,
!               FINDFTP, FINDMHS, INITSTOR, NEXTFILE, SATYPE,
!               SECSOUT, STORBUFR, SUMMARY
!
! FILES USED  : Unit  1:  MetDB housekeeping data set.
!               Unit  2:  Namelist input (closed after data read in).
!               Unit 10:  MHS data sets (closed when finished with).
!
! HISTORY     : Original version written by Brian Barwell, Nov. 2000.
!
! REVISION INFO:
!
! $Workfile: bufrdat.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 26/06/2012 16:58:48$
!
! CHANGE RECORD:
!
! $Log:
!  10   MetDB_Refresh 1.9         26/06/2012 16:58:48    Brian Barwell   'BUFR
!       not found' message suppressed for NIL reports.
!  9    MetDB_Refresh 1.8         24/05/2011 15:13:40    Brian Barwell
!       Increase size of BULL to 512000. Delete COMMON block.
!  8    MetDB_Refresh 1.7         15/04/2011 14:54:54    Sheila Needham  Added
!       variable MHSTYPE to &INSTOR defaulted to 'TEST'
!  7    MetDB_Refresh 1.6         22/03/2011 16:24:51    Sheila Needham  Remove
!        silent_mod - test routines are included as obj files for test or
!       assembler for oper.
!  6    MetDB_Refresh 1.5         17/03/2011 15:58:01    Sheila Needham  Revert
!        to an internal write statement
!  5    MetDB_Refresh 1.4         16/03/2011 09:52:36    Sheila Needham
!       Correct TTAAII initialisation.
!  4    MetDB_Refresh 1.3         15/03/2011 09:44:33    Sheila Needham
!       Updated OPEN unit 2
!  3    MetDB_Refresh 1.2         07/03/2011 09:43:55    John Norton     After
!       updating for C I/O routines. Ready for review.
!  2    MetDB_Refresh 1.1         27/01/2011 16:47:12    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         26/01/2011 13:44:42    Alison Weir
!       Initial f77 version - BUFRDAT5
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
USE bufrchek_mod
USE bufrep_mod
USE datim_mod
USE dreg_mod
USE dsinfo_mod
USE ebcdic_mod
USE findftp_mod
USE findmhs_mod
USE initstor_mod
USE nextfile_mod
USE satype_mod
USE storbufr_mod
USE summary_mod
! USE silent_mod  ! for test version of tellops

IMPLICIT NONE

! Parameters:

INTEGER, PARAMETER :: BULSIZ=512000 ! Size of BULL
INTEGER, PARAMETER :: MAXHED=120    ! Max no of bulletin header ranges
INTEGER, PARAMETER :: NFLAGS=9      ! No. of data processing flags
INTEGER, PARAMETER :: NITEMS=12     ! No. of data processing items

! Variable declarations:

INTEGER  ::  IBTYP              ! BUFR type from message
INTEGER  ::  IBSUB              ! BUFR subtype from message
INTEGER  ::  IDUMMY             ! Dummy argument for NEXTFILE
INTEGER  ::  INDX               ! Pointer to 2nd subscript of KOUNTS
INTEGER  ::  IOS                ! Return status from I/O statement
INTEGER  ::  IUNIT              ! Unit number of storage data set
INTEGER  ::  IUNITDUM = 0       ! Dummy unit no used as input to DSINFO
INTEGER  ::  IUNITHK  = 1       ! Unit number of housekeeping d/s
INTEGER  ::  J                  ! Variable for loops or other local use
INTEGER  ::  KODE               ! Return code from subroutine
INTEGER  ::  KOUNTS(3,0:MAXHED) = 0 ! Counters for numbers of messages
INTEGER  ::  LENIDX(99)  = 0    ! Index lengths for units 1-99
INTEGER  ::  LENBUL             ! Length of BUFR bulletin
INTEGER  ::  LENREC             ! Record length of storage data set
INTEGER  ::  LSEQ(99)    = 0    ! BUFR sequences for units 1-99
INTEGER  ::  MBUFR              ! Location of "BUFR"
INTEGER  ::  M7777              ! Location of end of "7777"
INTEGER  ::  MFIRST             ! First byte of message after header
INTEGER  ::  MEND               ! Finish byte of message
INTEGER  ::  NBTYP              ! Expected BUFR type
INTEGER  ::  NBSUB              ! Expected BUFR subtype
INTEGER  ::  MLNGTH             ! Length of message (bytes
INTEGER  ::  MSGCODE = 2        ! Return code from FINDMSG or FINDFTP
INTEGER  ::  MSTART             ! Location of start of message
INTEGER  ::  NBUL               ! Bulletin range no.
INTEGER  ::  NBULLS             ! Number of bulletin ranges
INTEGER  ::  NCOL               ! Counter index (2/3 = is/isn't stored)
INTEGER  ::  NFILES  = 0        ! Number of data sets processed so far
INTEGER  ::  NMSGS   = 0        ! Bulletins so far in current data set
INTEGER  ::  NOW(8)  = 0        ! Current date/time (from DATIM)
INTEGER  ::  NSTORED = 0        ! Bulletins stored from MHS data set
INTEGER  ::  NTOTAL  = 0        ! Total of messages found so far
INTEGER  ::  NUMBAD  = 0        ! No. of uncreatable index entries
INTEGER  ::  NUMBIG  = 0        ! No. of bulletins too big to store
INTEGER  ::  NUMDUP  = 0        ! No. of duplicate bulletins rejected
INTEGER  ::  NUMFUL  = 0        ! No. of bulletins rejected by full d/s
INTEGER  ::  NUMOLD  = 0        ! No. of bulletins too old to store
INTEGER  ::  NUMPRE  = 0        ! No. of bulletins with data in future

LOGICAL  ::  STORFLAG           ! Flags for 'storage required'
LOGICAL  ::  TERMIN8            ! Operator's termination flag

CHARACTER(LEN=10)    ::   LETTER = 'ZABCDEFGHI'
CHARACTER(LEN=BULSIZ)::   BULL       ! Buffer for holding bulletins
CHARACTER(LEN=18)    ::   BUL18      ! 'TTAAII CCCC YYGGGG' from message
CHARACTER(LEN=3)     ::   CRCRLF     ! ASCII 'CR-CR-LF'
CHARACTER(LEN=8)     ::   DATYPE     ! MetDB code for data type
CHARACTER(LEN=44)    ::   DSN        ! Data set name
CHARACTER(LEN=1)     ::   DUMMY      ! Dummy argument for NEXTFILE
CHARACTER(LEN=8)     ::   DUMMYBULL  ! Dummy argument for DREG
CHARACTER(LEN=3)     ::   NIL        ! ASCII 'NIL'                  !10
CHARACTER(LEN=1)     ::   S          ! 'S', or ' ' if only 1 bulletin
CHARACTER(LEN=44)    ::   TEXT44     ! 44-character text string
CHARACTER(LEN=6)     ::   TTAAII     ! 'TTAAII' from message header

!                        Variables relating to bulletin header data set

INTEGER  ::  ITEMS(0:NITEMS,MAXHED)         ! Data processing items
LOGICAL  ::  FLAGS(NFLAGS,MAXHED)           ! Data processing flags
CHARACTER(LEN=6)  ::  HEAD1(MAXHED)         ! Header ranges
CHARACTER(LEN=6)  ::  HEAD2(MAXHED)         ! Header ranges
CHARACTER(LEN=8)  ::  BULTYP(MAXHED)        ! Storage data set codes

!-----------------------------------------------------------------------
!     NAMELIST  (SEE ABOVE FOR DETAILS AND DEFAULTS)
!-----------------------------------------------------------------------

CHARACTER(LEN=8)  ::  JOBNAME = 'MDB??? '
CHARACTER(LEN=8)  ::  OWNER   = 'BUF1'
CHARACTER(LEN=4)  ::  MHSTYPE = 'TEST'
LOGICAL           ::  PRINT   = .TRUE.
LOGICAL           ::  GTSDATA = .FALSE.
INTEGER           ::  MODE    = 0
INTEGER           ::  NWAIT   = 30
INTEGER           ::  NDREG   = 0

NAMELIST /INSTOR/ JOBNAME,  OWNER, PRINT, GTSDATA, MODE, NWAIT, NDREG, &
                  MHSTYPE

!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETUP
!-----------------------------------------------------------------------

CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF IN ASCII
NIL    = CHAR(78)//CHAR(73)//CHAR(76) ! 'NIL' IN ASCII              !10

!                                                    Read namelist data
OPEN (2, FILE='DD:FT02F001', ACTION='READ', IOSTAT=IOS)
IF (IOS == 0) THEN
   READ (2, INSTOR, IOSTAT=IOS)
!                                           Set unit for dregs data set
   IF (IOS == 0 .AND. NDREG /= 99)  &
       CALL DREG (NDREG, 'DREGUNIT', DUMMYBULL, ' ', ' ', ' ', NOW, ' ')
   CLOSE (2)
END IF
!                                          Read bulletin processing and
!                                          storage data set information
NBULLS = MAXHED
CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS,  &
               ITEMS, NBULLS, LENIDX, LSEQ, BULL)

!                                   Open housekeeping data set (unit 1)

CALL DSINFO ('HKEEP', 3, IUNITHK, LENREC, KODE, TEXT44)

!                                                  Allocate BUFR tables

CALL DSINFO ('CODEFIG', 0,  IUNITDUM, LENREC, KODE, TEXT44)
CALL DSINFO ('TABLEB',  0,  IUNITDUM, LENREC, KODE, TEXT44)
CALL DSINFO ('TABLED',  0,  IUNITDUM, LENREC, KODE, TEXT44)

!                      Assign job status record.
!          (For this first call to NEXTFILE, "TERMIN8" is used on input
!           to indicate whether MHS data set names are to be taken from
!              the H.K. data set. Thereafter it is a termination flag.)

DSN(1:8) = JOBNAME
TERMIN8 = .FALSE. ! (i.e. MHS data set names not got from H.K.)
CALL NEXTFILE (DSN, IDUMMY, TERMIN8)

!=======================================================================
!                 MAIN LOOP OVER INCOMING BULLETINS
!                 ---------------------------------
!     Keep processing until termination flag ("TERMIN8") is set.
!=======================================================================

do_until_term: &
DO WHILE (.NOT.TERMIN8)

   IF (GTSDATA) THEN
      CALL FINDMHS (10, OWNER, BULL, MSTART, MLNGTH, MSGCODE, MHSTYPE)
   ELSE
      CALL FINDFTP (10, OWNER, BULL, MSTART, MLNGTH, DSN, MSGCODE, MHSTYPE)
   END IF

!                         MSGCODE=1 (end of data set) or <0 (MHS error)
!                         ---------------------------------------------
iflabel1: &
   IF (MSGCODE == 1 .OR. MSGCODE < 0) THEN
      NFILES = NFILES + 1
!                                                   Diagnostic messages

!                                              Old data
      IF (NUMOLD > 0) THEN
         WRITE (6,'(T3,I6,1X,A)') NUMOLD,  &
          'bulletins rejected by check for old data.'
         NUMOLD = 0
      END IF
!                                              Duplicate bulletins
      IF (NUMDUP > 0) THEN
         WRITE (6,'(T3,I6,1X,A)') NUMDUP,  &
          'bulletins rejected by duplicate data check.'
         NUMDUP = 0
      END IF
!                                              Storage data set full
      IF (NUMFUL > 0) THEN
         WRITE (6,'(T3,I6,1X,A)') NUMFUL,  &
          'bulletins rejected because storage data set was full.'
         NUMFUL = 0
      END IF
!                                              Bulletins too big
      IF (NUMBIG > 0) THEN
         WRITE (6,'(T3,I6,1X,A)') NUMBIG,  &
          'bulletins too big to store in data set.'
         NUMBIG = 0
      END IF
!                                              Data time in future
      IF (NUMPRE > 0) THEN
         WRITE (6,'(T3,I6,1X,A)') NUMPRE,  &
          'bulletins rejected with data times in the future.'
         NUMPRE = 0
      END IF
!                                              Uncreatable indexes
      IF (NUMBAD > 0) THEN
         WRITE (6,'(T3,I6,1X,2A)') NUMBAD, 'index entries ',  &
          'could not be made owing to bad or missing data.'
         NUMBAD = 0
      END IF
!                                          Optional information message
      IF (PRINT) THEN
         S = 'S'
         IF (NMSGS == 1) S = ' '
         CALL DATIM (NOW)
         WRITE (6,'(T2, I2.2,A,I2.2,A,I2.2, I6, 1X, 5A)')        &
                NOW(5), ':', NOW(4), ':', NOW(3), NMSGS, DATYPE, &
               ' BULLETIN', S, ' PROCESSED FROM ', BULL(9:52)
      END IF
      NMSGS = 0   ! (Reset)
      NSTORED = 0 ! (Reset)
!                                          Terminate if fatal MHS error

      IF (MSGCODE < 0) TERMIN8 = .TRUE.

!                                      MSGCODE=2 (no data sets waiting)
!                                      --------------------------------
   ELSE IF (MSGCODE == 2) THEN  iflabel1

! The lines below were added to enable job MDBBUF2 to process both
! GOES BUFR winds received from FROST (owner 'BUF2') and GMS BUFR
! winds received by ftp from NETLINK (owner 'GMS1'). It flips the
! owner and the GTS flag whenever it finds no data waiting.

      IF (OWNER == 'BUF2') THEN
        OWNER = 'GMS1'
        GTSDATA = .FALSE.
      ELSE IF (OWNER == 'GMS1') THEN
        OWNER = 'BUF2'
        GTSDATA = .TRUE.
      END IF
!                                                 MODE 1: Terminate now

      IF (MODE == 1 .AND. OWNER /= 'GMS1') THEN
         TERMIN8 = .TRUE.
!                                                 MODE 0: Wait for data
      ELSE
         CALL SECSOUT (NWAIT)
      END IF
!                                        MSGCODE=0 (next message found)
!                                        ------------------------------
   ELSE
      MEND = MSTART + MLNGTH - 1
      CALL DATIM (NOW)  ! Time of receipt
iflabel2: &
      IF (GTSDATA) THEN

!                      Find first byte after 'CRCRLF' at end of header.
!                           (Header is usually 41 bytes but may be 45.)

         INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)
         IF (INDX <= 0) INDX = 5 !'CRCRLF' not found: try 45 bytes
         MFIRST = MSTART + INDX + 40

!                        Extract bulletin header ('TTAAII CCCC YYGGGG')

         BUL18 = BULL(MSTART+20:MSTART+37)
         CALL EBCDIC (18, BUL18)
         TTAAII = BUL18(1:6)
      ELSE
         MFIRST = MSTART
         CALL BUFRCHEK (BULL(MFIRST:MEND), NOW, MBUFR, LENBUL,  &
                        IBTYP, IBSUB, KODE)
!                                                     Pseudo-GTS header
         J = MOD(ICHAR(DSN(7:7)),10) + 1 !
         S = LETTER(J:J)
         J = INDEX(DSN//' ',' ') - 1   ! Last character of DSN
         WRITE (TTAAII,'(A,2Z2.2,A)') S, IBTYP, IBSUB, DSN(J:J)
                  WRITE (BUL18,'(2A)') 'BUFR data - ', TTAAII
      END IF iflabel2
!                       Look up list of header ranges to find data type

      CALL SATYPE (TTAAII, HEAD1, HEAD2, NBULLS, NBUL)

!                                        Set data type and storage flag

iflabel3: &
      IF (NBUL <= 0) THEN    ! Header not in list
         DATYPE = 'UNKNOWN '
         STORFLAG = .FALSE.
         INDX = 0
!                                                       Printed message
         WRITE (6,'(T5,A,T15,2A)') 'BUFRDAT:',  &
                  'Unidentified bulletin header - ', BUL18

      ELSE
         DATYPE = BULTYP(NBUL)
         STORFLAG = FLAGS(1,NBUL)
         INDX = ITEMS(0,NBUL)  ! (Row number in summary table)

!                                   Pass information to DREG subroutine

         CALL DREG (0, DUMMYBULL, DUMMYBULL,DUMMYBULL, DATYPE, BUL18, NOW, DSN)
      END IF iflabel3

!-----------------------------------------------------------------------
!           CHECK MESSAGE QUALITY.
!-----------------------------------------------------------------------

iflabel4: &
      IF (STORFLAG) THEN
         TEXT44 = ' '
!                             Check message and get BUFR type & subtype

         IF (GTSDATA) CALL BUFRCHEK (BULL(MFIRST:MEND), NOW,  &
                           MBUFR, LENBUL, IBTYP, IBSUB, KODE)
         WRITE (TEXT44,'(A,2I4)')  &
                       'BUFR type & subtype =', IBTYP, IBSUB

!                 Start of BUFR message not found (but check for 'NIL')
iflabel5: &
         IF (KODE == 1) THEN
            IF (INDEX(BULL(MFIRST:MEND),NIL) == 0)               &  !10
            WRITE (6,'(T2,4A)') BUL18, ':  Data type ', DATYPE,  &
                     ' Start of BUFR message not found.'
            STORFLAG = .FALSE.
!                                         End of BUFR message not found
         ELSE IF (KODE == 2) THEN  iflabel5
            WRITE (6,'(T2,4A,T73,A)') BUL18, ':  Data type ',  &
                DATYPE, ' End of BUFR message not found.', TEXT44
            STORFLAG = .FALSE.
!                                             Check BUFR type & subtype
         ELSE
            NBTYP = ITEMS(6,NBUL)
            NBSUB = ITEMS(7,NBUL)
iflabel6: &
            IF ((NBTYP >= 0 .AND. NBTYP /= IBTYP) .OR.   &
                (NBSUB >= 0 .AND. NBSUB /= IBSUB)) THEN
               WRITE (6,'(T2,5A)') BUL18, ':  Data type ',  &
                                   DATYPE, ' Unexpected ', TEXT44
               STORFLAG = .FALSE.
!                                           Message is OK: set pointers
!                                          and convert header to EBCDIC
            ELSE
               IF (GTSDATA) CALL EBCDIC   &
                  (MFIRST-MSTART, BULL(MSTART:MFIRST-1))
               MBUFR = MFIRST + MBUFR - 1
               M7777 = MBUFR + LENBUL - 1

               IF (FLAGS(2,NBUL)) CALL STORBUFR (DATYPE,  &
                   BUL18, BULL(MBUFR:M7777),              &
                   ITEMS(0,NBUL), STORFLAG)
            END IF iflabel6
         END IF iflabel5
      END IF iflabel4

!-----------------------------------------------------------------------
!        FIND UNIT & RECORD LENGTH FOR STORAGE, AND CHECK RETURN CODE.
!-----------------------------------------------------------------------

      NCOL = 3 ! (Assume reject until proved good)

iflabel7: &
      IF (STORFLAG) THEN

         CALL DSINFO (DATYPE, -1, IUNIT, LENREC, KODE, TEXT44)

!                                         Warning for unknown data type
iflabel8: &
         IF (KODE == 2) THEN
            WRITE (6,'(T2,3A,T58,A)') BUL18,  &
                     ':  Unrecognised data type: ', DATYPE,TEXT44

!                         Warning for data set not open or inaccessible

         ELSE IF (KODE == 3 .OR. IUNIT == 0) THEN  iflabel8
            WRITE (6,'(T2,3A,T58,A)') BUL18,  &
                  ':  Inaccessible data set for: ', DATYPE,TEXT44

!-----------------------------------------------------------------------
!        ALL OK, SO CALL MET.D.B. STORAGE ROUTINE.
!-----------------------------------------------------------------------

         ELSE
            CALL BUFREP (IUNIT, LENREC, NOW, FLAGS(1:,NBUL),    &
                 ITEMS(1:,NBUL), LSEQ(IUNIT), BULL(MBUFR:M7777), &
                 KODE)
!                                                     Check return code

iflabel9: &
            IF (KODE <= 10) THEN       ! Message stored
               NCOL = 2
               NSTORED = NSTORED + 1
            ELSE IF (KODE == 11) THEN  ! Rejected - old data
               NUMOLD = NUMOLD + 1
            ELSE IF (KODE == 12) THEN  ! Rejected - duplicate
               NUMDUP = NUMDUP + 1
            ELSE IF (KODE == 31) THEN  ! Rejected - no room
               IF (NUMFUL == 0)  &
                   WRITE (6,'(/T5,A,T15,A,I3/)') 'BUFRDAT:', &
                  'Storage data set full - Unit', IUNIT
               NUMFUL = NUMFUL + 1
            ELSE IF (KODE == 32) THEN  ! Rejected - too big
               NUMBIG = NUMBIG + 1
            ELSE IF (KODE == 43) THEN  ! Rejected - future time
               NUMPRE = NUMPRE + 1
            ELSE IF (KODE == 44) THEN  ! Rejected - bad data
               NUMBAD = NUMBAD + 1

!                               Switch off storage if data set unusable

            ELSE IF (KODE >= 24 .AND. KODE <= 27) THEN
               WRITE (6,'(T5,A,T15,2A/)') 'BUFRDAT:',  &
                  DATYPE, ' storage will now be terminated.'
               DO J=NBUL,NBULLS
                  IF (ITEMS(0,J) == ITEMS(0,NBUL))  &
                      FLAGS(1,J) = .FALSE.
               END DO ! J
               CLOSE (IUNIT)
            END IF iflabel9

!-----------------------------------------------------------------------
!     PRINT INFORMATION MESSAGE (OPTIONAL), UPDATE BULLETIN COUNTERS
!     AND RETURN FOR ANOTHER MESSAGE.
!-----------------------------------------------------------------------
!                               Optional message for successful storage

            IF (ITEMS(2,NBUL) >= 1) WRITE (6,'(T2,3A)') BUL18, &
                      ':  Data type ', DATYPE
         END IF iflabel8
!                           Optional message for data type not required
      ELSE
         IF (ITEMS(2,NBUL) >= 1)  &
            WRITE (6,'(T2,4A)') BUL18, ':  Data type ', DATYPE,  &
                                       '   Storage not required.'
      END IF iflabel7

      KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
      KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1

      NMSGS  = NMSGS  + 1  ! Messages so far in this data set
      NTOTAL = NTOTAL + 1  ! Total messages so far
   END IF iflabel1
!                                Update job status record if not in the
!                                   middle of a data set, check for job
!                              termination and go back for next message

   IF (MSGCODE /= 0) CALL NEXTFILE (DUMMY, IDUMMY, TERMIN8)
END DO   do_until_term

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

!                                  Messages for unrecoverable MHS error
iflabel10: &
IF (MSGCODE < 0) THEN
   J = LEN(OWNER)
   INDX = INDEX(OWNER,' ') - 1
   IF (INDX <= 0) INDX = J
   WRITE (6,'(/T5,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',  &
            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
   IF (MSGCODE == -1) THEN
      WRITE (6,'(T5,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
      WRITE (6,'(T5,2A/4A)') 'TRY RE-RUNNING THE JOB: IF THE ',  &
               'PROBLEM RECURS, TRY DELETING THE NEXT "',        &
               OWNER(1:INDX), '" DATA SET (LOOK FOR "MHSR.*.*.', &
               OWNER(1:INDX), '.S000").'
   ELSE IF (MSGCODE == -2) THEN
      WRITE (6,'(T5,2A/)') 'RENAME THE DATA SET ', BULL(9:52)
      WRITE (6,'(T5,2A)') 'CHECK THAT THE CORRESPONDING ',     &
               'DATA SET BEGINNING "MHSP" DOES NOT EXIST.',    &
               'IF IT DOES, DELETE ONE OR THE OTHER; CHECK ',  &
               'FOR SIMILAR DUPLICATES; THEN RERUN.'
   END IF
!                                    Send message to operator and ABEND
   KODE = 800 - MSGCODE
   WRITE (6,'(/T5,2A,I4/)') 'STORAGE JOB WILL NOW ABEND ',  &
                            'WITH USER CODE', KODE
   CALL TELLOPS ('MDB(E):  FATAL ERROR IN MHS ROUTINE "MHSIFF".')
   CALL SYSABN (KODE)
END IF iflabel10
!                                                     That's all folks!
STOP
END PROGRAM BUFRDAT
