      PROGRAM BUFRDAT
!
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
!
! CALLS       : BUFRCHEK, BUFREP, DATIM, DREG, DSINFO, EBCDIC,
!               FINDFTP, FINDMHS, INITSTOR, NEXTFILE, SATYPE,
!               SECSOUT, STORBUFR, SUMMARY                          !2.2
!
! FILES USED  : Unit  1:  MetDB housekeeping data set.
!               Unit  2:  Namelist input (closed after data read in).
!               Unit 10:  MHS data sets (closed when finished with).
!
! HISTORY     : Original version written by Brian Barwell, Nov. 2000.
!
! REVISION INFO:
!
! $Workfile: bufrdat.f$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 27/02/2008 10:01:57$
!
! CHANGE RECORD:
!
! $Log:
!  2    Met_DB_Project 1.1         27/02/2008 10:01:57    Brian Barwell   Count
!        instances of return code 32 from BUFREP and output warning message if
!        any.
!  1    Met_DB_Project 1.0         30/01/2006 20:21:21    Sheila Needham  
! $
! Revision 2.4  2004/03/03 15:02:00  usmdb
! 2.4.  15 March 2004.  Brian Barwell.  Change 17/04.
! Add bulletin header ('BUL18') to argument of call to STORBUFR
! and correct the 'ITEMS' argument.
!
! Revision 2.3  2003/03/27  14:49:31  14:49:31  usmdb (MetDB account c/o usjh)
! 2.3.  2 April 2003.  Brian Barwell.  Change 21/03.
! Changes to locations in GTS header associated with the change
! from TROPICS to FROST. Also make MDBBUF2 alternate between
! 'BUF2' and 'GMS1' MHS data sets.
!
! Revision 2.2  2002/10/07  15:21:02  15:21:02  usmdb (MetDB account c/o usjh)
! 21 Oct 2002.  2.2.  Brian Barwell.  Change 89/02.
! Call to STORBUFR added for additional checks on BUFR bulletins.
!
! Revision 2.1  2001/06/07  15:16:55  15:16:55  usmdb (Generic MetDB account)
! Changed 4 ? to 3 ? plus a space - S.Cox
!
! Revision 2.0  2001/06/06 10:13:36  usmdb
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2004 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                            Parameters

      INTEGER MAXHED      ! Maximum number of bulletin header ranges
      INTEGER NFLAGS      ! Number of data processing flags
      INTEGER NITEMS      ! Number of data processing items

      PARAMETER (MAXHED=120, NFLAGS=9, NITEMS=12)
!                                                             Variables

      INTEGER IBTYP, IBSUB       ! BUFR type & subtype from message
      INTEGER IDUMMY             ! Dummy argument for NEXTFILE
      INTEGER INDX               ! Pointer to 2nd subscript of KOUNTS
      INTEGER IOS                ! Return status from I/O statement
      INTEGER IUNIT              ! Unit number of storage data set
      INTEGER J                  ! Variable for loops or other local use
      INTEGER KODE               ! Return code from subroutine
      INTEGER KOUNTS(3,0:MAXHED) ! Counters for numbers of messages
      INTEGER LENIDX(99)         ! Index lengths for units 1-99
      INTEGER LENBUL             ! Length of BUFR bulletin
      INTEGER LENREC             ! Record length of storage data set
      INTEGER LSEQ(99)           ! BUFR sequences for units 1-99
      INTEGER MBUFR, M7777       ! Locations of "BUFR" & end of "7777"
      INTEGER MFIRST             ! First byte of message after header
      INTEGER MEND               ! Finish byte of message
      INTEGER NBTYP, NBSUB       ! Expected BUFR type & subtype
      INTEGER MLNGTH             ! Length of message (bytes
      INTEGER MSGCODE            ! Return code from FINDMSG or FINDFTP
      INTEGER MSTART             ! Location of start of message
      INTEGER NBUL, NBULLS       ! Bulletin range no. & number of ranges
      INTEGER NCOL               ! Counter index (2/3 = is/isn't stored)
      INTEGER NFILES             ! Number of data sets processed so far
      INTEGER NMSGS              ! Bulletins so far in current data set
      INTEGER NOW(8)             ! Current date/time (from DATIM)
      INTEGER NSTORED            ! Bulletins stored from MHS data set
      INTEGER NTOTAL             ! Total of messages found so far
      INTEGER NUMBAD             ! No. of uncreatable index entries
      INTEGER NUMBIG             ! No. of bulletins too big to store !2
      INTEGER NUMDUP             ! No. of duplicate bulletins rejected
      INTEGER NUMFUL             ! No. of bulletins rejected by full d/s
      INTEGER NUMOLD             ! No. of bulletins too old to store
      INTEGER NUMPRE             ! No. of bulletins with data in future

      LOGICAL STORFLAG           ! Flags for 'storage required'
      LOGICAL TERMIN8            ! Operator's termination flag

      CHARACTER*10    LETTER     ! 'ZABCDEFGHI'
      CHARACTER*28672 BULL       ! 28K buffer
      CHARACTER*18    BUL18      ! 'TTAAII CCCC YYGGGG' from message
      CHARACTER*3     CRCRLF     ! ASCII 'CR-CR-LF'
      CHARACTER*8     DATYPE     ! MetDB code for data type
      CHARACTER*44    DSN        ! Data set name
      CHARACTER*80    HEAD       ! For revision information          !2
      CHARACTER       DUMMY      ! Dummy argument for NEXTFILE
      CHARACTER       S          ! 'S', or ' ' if only 1 bulletin read
      CHARACTER*44    TEXT44     ! 44-character text string
      CHARACTER*6     TTAAII     ! 'TTAAII' from message header

!                        Variables relating to bulletin header data set

      INTEGER ITEMS(0:NITEMS,MAXHED)           ! Data processing items
      LOGICAL FLAGS(NFLAGS,MAXHED)             ! Data processing flags
      CHARACTER*6 HEAD1(MAXHED), HEAD2(MAXHED) ! Header ranges
      CHARACTER*8 BULTYP(MAXHED)               ! Storage data set codes

!                            Common block (for dynamic allocation only)
      COMMON /COMBUF/ BULL

!-----------------------------------------------------------------------
!     NAMELIST  (SEE ABOVE FOR DETAILS AND DEFAULTS)
!-----------------------------------------------------------------------

      CHARACTER*8  JOBNAME,  OWNER
      LOGICAL                       PRINT, GTSDATA
      INTEGER                                       MODE, NWAIT, NDREG
      NAMELIST
     &    /INSTOR/ JOBNAME,  OWNER, PRINT, GTSDATA, MODE, NWAIT, NDREG
      DATA         JOBNAME,  OWNER, PRINT, GTSDATA, MODE, NWAIT, NDREG
     &          /'MDB??? ', 'BUF1',.TRUE., .FALSE.,    0,    30,     0/

!                                                       Data statements
      DATA MSGCODE /2/, NOW /8*0/
      DATA NFILES, NSTORED, NMSGS, NTOTAL/4*0/
      DATA NUMBAD, NUMBIG, NUMDUP, NUMFUL, NUMOLD, NUMPRE /6*0/      !2
      DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/
      DATA LENIDX/99*0/, LSEQ/99*0/, LETTER /'ZABCDEFGHI'/

!-----------------------------------------------------------------------
!     REVISION INFORMATION
!-----------------------------------------------------------------------

      HEAD = '$Workfile: bufrdat.f$ ' //
     &       '$Revision: 2$ $Date: 27/02/2008 10:01:57$'

!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETUP
!-----------------------------------------------------------------------

      CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF IN ASCII

!                                                    Read namelist data
      OPEN (2, IOSTAT=IOS)
      IF (IOS.EQ.0) THEN
         READ (2, INSTOR, IOSTAT=IOS)
!                                           Set unit for dregs data set
         IF (IOS.EQ.0 .AND. NDREG.NE.99)
     &       CALL DREG (NDREG, 'DREGUNIT', ' ', ' ', ' ', ' ', NOW, ' ')
         CLOSE (2)
      END IF
!                                          Read bulletin processing and
!                                          storage data set information
      NBULLS = MAXHED
      CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS,
     &               ITEMS, NBULLS, LENIDX, LSEQ, BULL)

!                                   Open housekeeping data set (unit 1)

      CALL DSINFO ('HKEEP', 3, 1, LENREC, KODE, TEXT44)

!                                                  Allocate BUFR tables

      CALL DSINFO ('CODEFIG', 0,  0, LENREC, KODE, TEXT44)
      CALL DSINFO ('TABLEB',  0,  0, LENREC, KODE, TEXT44)
      CALL DSINFO ('TABLED',  0,  0, LENREC, KODE, TEXT44)

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

      DO WHILE (.NOT.TERMIN8)

         IF (GTSDATA) THEN
            CALL FINDMHS (10, OWNER, BULL, MSTART, MLNGTH, MSGCODE)
         ELSE
            CALL FINDFTP (10, OWNER, BULL, MSTART, MLNGTH, DSN, MSGCODE)
         END IF

!                         MSGCODE=1 (end of data set) or <0 (MHS error)
!                         ---------------------------------------------

         IF (MSGCODE.EQ.1 .OR. MSGCODE.LT.0) THEN
            NFILES = NFILES + 1
!                                                   Diagnostic messages

!                                              Old data
            IF (NUMOLD.GT.0) THEN
               WRITE (6,'(T3,I6,1X,A)') NUMOLD,
     &          'bulletins rejected by check for old data.'
               NUMOLD = 0
            END IF
!                                              Duplicate bulletins
            IF (NUMDUP.GT.0) THEN
               WRITE (6,'(T3,I6,1X,A)') NUMDUP,
     &          'bulletins rejected by duplicate data check.'
               NUMDUP = 0
            END IF
!                                              Storage data set full
            IF (NUMFUL.GT.0) THEN
               WRITE (6,'(T3,I6,1X,A)') NUMFUL,
     &          'bulletins rejected because storage data set was full.'
               NUMFUL = 0
            END IF
!                                              Bulletins too big     !2
            IF (NUMBIG.GT.0) THEN                                    !2
               WRITE (6,'(T3,I6,1X,A)') NUMBIG,                      !2
     &          'bulletins too big to store in data set.'            !2
               NUMBIG = 0                                            !2
            END IF                                                   !2
!                                              Data time in future
            IF (NUMPRE.GT.0) THEN
               WRITE (6,'(T3,I6,1X,A)') NUMPRE,
     &          'bulletins rejected with data times in the future.'
               NUMPRE = 0
            END IF
!                                              Uncreatable indexes
            IF (NUMBAD.GT.0) THEN
               WRITE (6,'(T3,I6,1X,2A)') NUMBAD, 'index entries ',
     &          'could not be made owing to bad or missing data.'
               NUMBAD = 0
            END IF
!                                          Optional information message
            IF (PRINT) THEN
               S = 'S'
               IF (NMSGS.EQ.1) S = ' '
               CALL DATIM (NOW)
               WRITE (6,'(T2, I2.2,A,I2.2,A,I2.2, I6, 1X, 5A)')
     &                NOW(5), ':', NOW(4), ':', NOW(3), NMSGS, DATYPE,
     &               ' BULLETIN', S, ' PROCESSED FROM ', BULL(9:52)
            END IF
            NMSGS = 0   ! (Reset)
            NSTORED = 0 ! (Reset)
!                                          Terminate if fatal MHS error

            IF (MSGCODE.LT.0) TERMIN8 = .TRUE.

!                                      MSGCODE=2 (no data sets waiting)
!                                      --------------------------------
         ELSE IF (MSGCODE.EQ.2) THEN

! The lines below were added to enable job MDBBUF2 to process both  !2.3
! GOES BUFR winds received from FROST (owner 'BUF2') and GMS BUFR   !2.3
! winds received by ftp from NETLINK (owner 'GMS1'). It flips the   !2.3
! owner and the GTS flag whenever it finds no data waiting.         !2.3

            IF (OWNER.EQ.'BUF2') THEN                               !2.3
              OWNER = 'GMS1'                                        !2.3
              GTSDATA = .FALSE.                                     !2.3
            ELSE IF (OWNER.EQ.'GMS1') THEN                          !2.3
              OWNER = 'BUF2'                                        !2.3
              GTSDATA = .TRUE.                                      !2.3
            END IF                                                  !2.3
!                                                 MODE 1: Terminate now

            IF (MODE.EQ.1 .AND. OWNER.NE.'GMS1') THEN               !2.3
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
            IF (GTSDATA) THEN

!                      Find first byte after 'CRCRLF' at end of header.
!                           (Header is usually 41 bytes but may be 45.)

               INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)       !2.3
               IF (INDX.LE.0) INDX = 5 !'CRCRLF' not found: try 45 bytes
               MFIRST = MSTART + INDX + 40                          !2.3

!                        Extract bulletin header ('TTAAII CCCC YYGGGG')

               BUL18 = BULL(MSTART+20:MSTART+37)                    !2.3
               CALL EBCDIC (18, BUL18)
               TTAAII = BUL18(1:6)
            ELSE
               MFIRST = MSTART
               CALL BUFRCHEK (BULL(MFIRST:MEND), NOW, MBUFR, LENBUL,
     &                        IBTYP, IBSUB, KODE)
!                                                     Pseudo-GTS header
               J = MOD(ICHAR(DSN(7:7)),10) + 1 !
               S = LETTER(J:J)
               J = INDEX(DSN//' ',' ') - 1   ! Last character of DSN
               WRITE (TTAAII,'(A,2Z2,A)') S, IBTYP, IBSUB, DSN(J:J)
               WRITE (BUL18,'(2A)') 'BUFR data - ', TTAAII
            END IF
!                       Look up list of header ranges to find data type

            CALL SATYPE (TTAAII, HEAD1, HEAD2, NBULLS, NBUL)

!                                        Set data type and storage flag

            IF (NBUL.LE.0) THEN    ! Header not in list
               DATYPE = 'UNKNOWN '
               STORFLAG = .FALSE.
               INDX = 0
!                                                       Printed message
               WRITE (6,'(T5,A,T15,2A)') 'BUFRDAT:',
     &                  'Unidentified bulletin header - ', BUL18

            ELSE
               DATYPE = BULTYP(NBUL)
               STORFLAG = FLAGS(1,NBUL)
               INDX = ITEMS(0,NBUL)  ! (Row number in summary table)

!                                   Pass information to DREG subroutine

               CALL DREG (0, ' ', ' ', ' ', DATYPE, BUL18, NOW, DSN)
            END IF

!-----------------------------------------------------------------------
!           CHECK MESSAGE QUALITY.
!-----------------------------------------------------------------------

            IF (STORFLAG) THEN
               TEXT44 = ' '
!                             Check message and get BUFR type & subtype

               IF (GTSDATA) CALL BUFRCHEK (BULL(MFIRST:MEND), NOW,
     &                           MBUFR, LENBUL, IBTYP, IBSUB, KODE)
               WRITE (TEXT44,'(A,2I4)')
     &                       'BUFR type & subtype =', IBTYP, IBSUB

!                                       Start of BUFR message not found
               IF (KODE.EQ.1) THEN
                  WRITE (6,'(T2,4A)') BUL18, ':  Data type ', DATYPE,
     &                     ' Start of BUFR message not found.'
                  STORFLAG = .FALSE.
!                                         End of BUFR message not found
               ELSE IF (KODE.EQ.2) THEN
                  WRITE (6,'(T2,4A,T73,A)') BUL18, ':  Data type ',
     &                DATYPE, ' End of BUFR message not found.', TEXT44
                  STORFLAG = .FALSE.
!                                             Check BUFR type & subtype
               ELSE
                  NBTYP = ITEMS(6,NBUL)
                  NBSUB = ITEMS(7,NBUL)
                  IF ((NBTYP.GE.0 .AND. NBTYP.NE.IBTYP) .OR.
     &                (NBSUB.GE.0 .AND. NBSUB.NE.IBSUB)) THEN
                     WRITE (6,'(T2,5A)') BUL18, ':  Data type ',
     &                                   DATYPE, ' Unexpected ', TEXT44
                     STORFLAG = .FALSE.
!                                           Message is OK: set pointers
!                                          and convert header to EBCDIC
                  ELSE
                     IF (GTSDATA) CALL EBCDIC                       !2.3
     &                  (MFIRST-MSTART, BULL(MSTART:MFIRST-1))      !2.3
                     MBUFR = MFIRST + MBUFR - 1
                     M7777 = MBUFR + LENBUL - 1

                     IF (FLAGS(2,NBUL)) CALL STORBUFR (DATYPE,      !2.2
     &                   BUL18, BULL(MBUFR:M7777),                  !2.4
     &                   ITEMS(0,NBUL), STORFLAG)                   !2.4
                  END IF
               END IF
            END IF

!-----------------------------------------------------------------------
!        FIND UNIT & RECORD LENGTH FOR STORAGE, AND CHECK RETURN CODE.
!-----------------------------------------------------------------------

            NCOL = 3 ! (Assume reject until proved good)

            IF (STORFLAG) THEN

               CALL DSINFO (DATYPE, -1, IUNIT, LENREC, KODE, TEXT44)

!                                         Warning for unknown data type
               IF (KODE.EQ.2) THEN
                  WRITE (6,'(T2,3A,T58,A)') BUL18,
     &                     ':  Unrecognised data type: ', DATYPE,TEXT44

!                         Warning for data set not open or inaccessible

               ELSE IF (KODE.EQ.3 .OR. IUNIT.EQ.0) THEN
                  WRITE (6,'(T2,3A,T58,A)') BUL18,
     &                  ':  Inaccessible data set for: ', DATYPE,TEXT44

!-----------------------------------------------------------------------
!        ALL OK, SO CALL MET.D.B. STORAGE ROUTINE.
!-----------------------------------------------------------------------

               ELSE
                  CALL BUFREP (IUNIT, LENREC, NOW, FLAGS(1,NBUL),
     &                 ITEMS(1,NBUL), LSEQ(IUNIT), BULL(MBUFR:M7777),
     &                 KODE)
!                                                     Check return code

                  IF (KODE.LE.10) THEN       ! Message stored
                     NCOL = 2
                     NSTORED = NSTORED + 1
                  ELSE IF (KODE.EQ.11) THEN  ! Rejected - old data
                     NUMOLD = NUMOLD + 1
                  ELSE IF (KODE.EQ.12) THEN  ! Rejected - duplicate
                     NUMDUP = NUMDUP + 1
                  ELSE IF (KODE.EQ.31) THEN  ! Rejected - no room
                     IF (NUMFUL.EQ.0)
     &                   WRITE (6,'(/T5,A,T15,A,I3/)') 'BUFRDAT:',
     &                  'Storage data set full - Unit', IUNIT
                     NUMFUL = NUMFUL + 1
                  ELSE IF (KODE.EQ.32) THEN  ! Rejected - too big    !2
                     NUMBIG = NUMBIG + 1                             !2
                  ELSE IF (KODE.EQ.43) THEN  ! Rejected - future time
                     NUMPRE = NUMPRE + 1
                  ELSE IF (KODE.EQ.44) THEN  ! Rejected - bad data
                     NUMBAD = NUMBAD + 1

!                               Switch off storage if data set unusable

                  ELSE IF (KODE.GE.24 .AND. KODE.LE.27) THEN
                     WRITE (6,'(T5,A,T15,2A/)') 'BUFRDAT:',
     &                  DATYPE, ' storage will now be terminated.'
                     DO J=NBUL,NBULLS
                        IF (ITEMS(0,J).EQ.ITEMS(0,NBUL))
     &                      FLAGS(1,J) = .FALSE.
                     END DO ! J
                     CLOSE (IUNIT)
                  END IF

!-----------------------------------------------------------------------
!     PRINT INFORMATION MESSAGE (OPTIONAL), UPDATE BULLETIN COUNTERS
!     AND RETURN FOR ANOTHER MESSAGE.
!-----------------------------------------------------------------------
!                               Optional message for successful storage

                  IF (ITEMS(2,NBUL).GE.1) WRITE (6,'(T2,3A)') BUL18,
     &                      ':  Data type ', DATYPE
               END IF
!                           Optional message for data type not required
            ELSE
               IF (ITEMS(2,NBUL).GE.1)
     &            WRITE (6,'(T2,4A)') BUL18, ':  Data type ', DATYPE,
     &                                       '   Storage not required.'
            END IF

            KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
            KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1

            NMSGS  = NMSGS  + 1  ! Messages so far in this data set
            NTOTAL = NTOTAL + 1  ! Total messages so far
         END IF
!                                Update job status record if not in the
!                                   middle of a data set, check for job
!                              termination and go back for next message

         IF (MSGCODE.NE.0) CALL NEXTFILE (DUMMY, IDUMMY, TERMIN8)
      END DO ! WHILE

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
      IF (MSGCODE.LT.0) THEN
         J = LEN(OWNER)
         INDX = INDEX(OWNER,' ') - 1
         IF (INDX.LE.0) INDX = J
         WRITE (6,'(/T5,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',
     &            'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
         IF (MSGCODE.EQ.-1) THEN
            WRITE (6,'(T5,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
            WRITE (6,'(T5,2A/4A)') 'TRY RE-RUNNING THE JOB: IF THE ',
     &               'PROBLEM RECURS, TRY DELETING THE NEXT "',
     &               OWNER(1:INDX), '" DATA SET (LOOK FOR "MHSR.*.*.',
     &               OWNER(1:INDX), '.S000").'
         ELSE IF (MSGCODE.EQ.-2) THEN
            WRITE (6,'(T5,2A/)') 'RENAME THE DATA SET ', BULL(9:52)
            WRITE (6,'(T5,2A)') 'CHECK THAT THE CORRESPONDING ',
     &               'DATA SET BEGINNING "MHSP" DOES NOT EXIST.',
     &               'IF IT DOES, DELETE ONE OR THE OTHER; CHECK ', !2.3
     &               'FOR SIMILAR DUPLICATES; THEN RERUN.'          !2.3
         END IF
!                                    Send message to operator and ABEND
         KODE = 800 - MSGCODE
         WRITE (6,'(/T5,2A,I4/)') 'STORAGE JOB WILL NOW ABEND ',
     &                            'WITH USER CODE', KODE
         CALL TELLOPS ('MDB(E):  FATAL ERROR IN MHS ROUTINE "MHSIFF".')
         CALL SYSABN (KODE)
      END IF
!                                                     That's all folks!
      STOP
      END
