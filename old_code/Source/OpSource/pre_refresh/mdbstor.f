      PROGRAM MDBSTOR

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM     : MDBSTOR    (MET.D.B. STORAGE JOB)
!
! LOCATION    : MDB.STORAGE.SRCE(MDBSTOR)
!
! PURPOSE     : TO STORE SELECTED BULLETINS IN THE MET.D.B.  DATA
!               TYPES TO BE PROCESSED ARE INPUT VIA NAMELIST AND DATA
!               SET NAMES ARE OBTAINED FROM A HOUSEKEEPING DATA SET.
!
! DESCRIPTION : THE FOLLOWING IS A BRIEF SUMMARY OF THE MAIN STEPS
!               IN THIS JOB. THE HOUSEKEEPING DATA SET IS USED FOR
!               COMMUNICATION WITH THE MET.D.B. STORAGE MONITOR JOB.
!
!               - READ HEADER DETAILS OF BULLETINS TO BE STORED.
!               - OPEN MET.D.B. STORAGE DATA SET REQUIRED.
!               - OPEN THE HOUSEKEEPING DATA SET.
!               - ASSIGN AND INITIALISE A JOB STATUS RECORD.
!
!               - LOOP OVER BULLETINS:
!                 - GET NEXT BULLETIN, WAITING FOR AND OPENING NEW
!                   FROST DATA SET IF NECESSARY.
!
!                 - IF A NEW BULLETIN IS FOUND:-
!                   - DETERMINE BULLETIN TYPE.
!                   - IF BUFR, CHECK BULLETIN STRUCTURE.
!                   - IF ASCII, CONVERT TO EBCDIC.
!                   - DO ADDITIONAL CHECKS ON BULLETIN (IF ANY).
!                   - IDENTIFY APPROPRIATE STORAGE DATA SET.
!                   - CALL APPROPRIATE ROUTINE TO STORE THE BULLETIN.
!                 - END IF.
!
!               - END OF LOOP OVER BULLETINS.
!
!               - PRINT SUMMARY OF BULLETINS PROCESSED.
!               - STOP.
!
! NAMELIST    : INSTOR  (UNIT 2).  CONTENTS AS FOLLOWS:
!
!               VARIABLE TYPE        DESCRIPTION             DEFAULT
!               -------- ----        -----------             -------
!                PRINT   L*4  FLAG TO PRODUCE 1-LINE MESSAGE  .TRUE.
!                             FOR EACH DATA SET PROCESSED.
!                NWAIT   I*4  NO. OF SECS. TO WAIT IF THERE     30
!                             ARE NO DATA SETS TO PROCESS.
!                NDREG   I*4  UNIT NO. FOR DREGS DATA SET       99
!                             (< OR =0 IF NO DATA SET).
!                JOBNAME C*8  JOB NAME.                     'MDB???? '
!                TYPES   C*8  (ARRAY) LIST OF DATA TYPES      BLANKS
!                             TO BE STORED BY THIS JOB.
!
! CALLS       : ADVBUL, AIRBUL, AMDAR, BATESBUL, BUFRCHEK, BULHED,   !2
!               BUOY, CLMBUL, DATIM, DECORD, DREG, DSINFO, EBCDIC,   !2
!               ENHBUL, ERSIND, GETHDR, INITSTOR, METBUL, NCMBUL,    !2
!               NEXTFILE, NEXTMSG, PRESTEL, SAMBUL, SATYPE, SFERICS, !2
!               SRWBUL, STBBUL, STORCHEK, SUMMARY, SYNBUL, SYNHED,   !3
!               TAFBUL, TBUSBUL, TRKBUL, UABUL, WINPRO, TROPBUL      !3
!
! FILES USED  : MHS DATA SETS OPENED AND CLOSED ON UNIT 10.
!               UNIT 2 (NAMELIST INPUT) OPENED, READ AND CLOSED.
!
! REVISION INFO :
!
! $Workfile: mdbstor.f$ $Folder: pre_refresh$
! $Revision: 3$ $Date: 22/04/2008 14:07:14$
!
! CHANGE RECORD :
!
! $Log:
!  3    Met_DB_Project 1.2         22/04/2008 14:07:14    Brian Barwell
!       Delete SFLOC storage section. Store new data type MOBSYN using SYNBUL.
!  2    Met_DB_Project 1.1         25/10/2007 10:03:46    Brian Barwell
!       Delete sections for SATEM and PLAINOB storage, comment out that for
!       HEALTHRR and delete all reference to array VAL2.
!  1    Met_DB_Project 1.0         30/01/2006 20:23:17    Sheila Needham  
! $
! Revision 2.5  2003/03/28 09:08:17  usmdb
! 2.5.  2 April 2003.  Brian Barwell.  Change 21/03.
! Changes to locations in GTS header associated with the change
! from TROPICS to FROST.
!
! Revision 2.4  2001/12/05  09:09:40  09:09:40  usmdb (MetDB account c/o usjh)
! Pass OCOR to STBBUL - S.Cox
!
! Revision 2.3  2001/11/06  10:11:03  10:11:03  usmdb (Generic MetDB account)
! 2.3.  19 November 2001.  Brian Barwell.  Change 141/01.
! Pass bulletin to AMDAR subroutine starting at TTAAii.
!
! Revision 2.2  2001/09/05  09:16:14  09:16:14  usmdb (Generic MetDB account)
! 17 Sept 2001     C Long (2.2) and Brian Barwell (2.2a)
! 2.2  Call BATESBUL rather than BTHBUL or TESBUL
!      (to cope with BATHYs & TESACs in the same bulletin)
! 2.2a Make arguments to DREG consistent for HP compiler.
!
! Revision 2.1  2001/07/03  10:38:24  10:38:24  usmdb (Generic MetDB account)
! Removed arguments IPT, BULEND, YYGGGG from call to HRRBUL as not
! used in HRRBUL. Removed arguments OAMD, AMDNUM from call to
! NCMBUL as not used in NCMBUL. Removed argument CORNUM from calls
! to PLAINOB and PRESTEL as not used in PLAINOB or PRESTEL. Removed
! argument YYGGGG from call to SFERICS as not used in SFERICS.
! Removed 6 arguments from call to STBBUL as not used in
! STBBUL. Removed arguments AMDNUM and OAMD from call to SYNBUL
! as not used in SYNBUL. Removed argument CCCC from call to TBUSBUL
! as not used in TBUSBUL. Removed argument CORNUM from call to
! TROPBUL as not used in TROPBUL. Removed arguments TTAAII and
! YYGGGG from call to TESBUL as not used in TESBUL. Removed argument
! TTAAII from call to BTHBUL as not used in BTHBUL - S.Cox
!
! Revision 2.0  2001/05/31  14:16:40  14:16:40  usmdb (Generic MetDB account)
! Removed unused variable CONTENTS. Removed argument IPT
! from ADVBUL call, as not used in ADVBUL. Removed
! argument MIMJ from CLMBUL call, as not used in CLMBUL.
! Removed argument YYGGGG from BTHBUL call as not used in
! BTHBUL. Added copyright and modified header - S.Cox
!
! Revision 1.4  2001/02/06  11:54:23  11:54:23  usmdb (Generic MetDB account)
! 19 Feb 2001    C Long
! 1.4  Call TROPBUL to store tropical cyclone messages.
! 1.4a Just allocate beacon list (now opened in BECRET).
!      Also code to store old SEAICE bulletins deleted.
!
! Revision 1.3  2001/01/15  10:35:17  10:35:17  usmdb (Generic MDB account)
! Change date: 22JAN01  R Hirst
! Extra argument MIMJ in calls to ENHBUL and SRWBUL.
!
! Revision 1.2  2000/07/10  11:14:04  11:14:04  usmdb (Generic MDB account)
! 17 July 2000   Brian Barwell.
! 1.2  Convert bulletin header to EBCDIC for BUFR bulletins.
!
! Revision 1.1  2000/06/09  11:16:46  11:16:46  usmdb (Generic MDB account)
! Initial revision
!
! 15/05/00:  ORIGINAL OPERATIONAL VERSION. (B.BARWELL)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2008 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

!                                                            PARAMETERS

      INTEGER MAXSTOR     ! MAXIMUM NUMBER OF STORAGE DATA SET CODES
      INTEGER MAXHED      ! MAXIMUM NUMBER OF BULLETIN HEADER RANGES
      INTEGER NFLAGS      ! NUMBER OF DATA PROCESSING FLAGS
      INTEGER NITEMS      ! NUMBER OF DATA PROCESSING ITEMS

      PARAMETER (MAXSTOR=50, MAXHED=120, NFLAGS=9, NITEMS=12)
!                                                             VARIABLES

      INTEGER IBTYP, IBSUB       ! BUFR TYPE & SUBTYPE FROM MESSAGE
      INTEGER INDX               ! POINTER TO 2ND SUBSCRIPT OF "KOUNTS"
      INTEGER IOS                ! RETURN STATUS FROM I/O STATEMENT
      INTEGER IUNIT              ! UNIT NUMBER OF STORAGE DATA SET
      INTEGER J                  ! VARIABLE FOR LOCAL USE
      INTEGER KODE               ! RETURN CODE (E.G. FROM 'DSINFO')
      INTEGER KOUNTS(3,0:MAXHED) ! COUNTERS FOR NUMBERS OF MESSAGES
      INTEGER LENIDX(99)         ! INDEX LENGTHS FOR UNITS 1-99
      INTEGER LENBUL             ! LENGTH OF BUFR BULLETIN
      INTEGER LENREC             ! RECORD LENGTH OF STORAGE DATA SET
      INTEGER LSEQ(99)           ! BUFR SEQUENCES FOR UNITS 1-99
      INTEGER MBUFR, M7777       ! LOCATIONS OF "BUFR" & END OF "7777"
      INTEGER MFIRST             ! FIRST BYTE OF MESSAGE AFTER HEADER
      INTEGER MLNGTH, MFINAL     ! MESSAGE LENGTH & LAST BYTE FOR PRINT
      INTEGER MSGCODE            ! RETURN CODE FROM "FINDMSG"
      INTEGER MSTART, MEND       ! START AND FINISH BYTES OF MESSAGE
      INTEGER NBTYP, NBSUB       ! EXPECTED BUFR TYPE & SUBTYPE
      INTEGER NBUL, NBULLS       ! BULLETIN RANGE NO. & NUMBER OF RANGES
      INTEGER NCOL               ! COUNTER INDEX (2/3 = IS/ISN'T STORED)
      INTEGER NFILES             ! NUMBER OF DATA SETS PROCESSED SO FAR
      INTEGER NFOUND             ! WANTED BULLETINS FOUND IN DATA SET
      INTEGER NIL(1)             ! ZERO IN ARRAY FOR CALLS TO DREG !2.2a
      INTEGER NMSGS              ! WANTED BULLETINS EXPECTED IN DATA SET
      INTEGER NOW(8)             ! DATE/TIME OF RECEIPT (FROM 'DATIM')
      INTEGER NTOTAL             ! TOTAL OF MESSAGES FOUND SO FAR
      INTEGER NTYPES             ! NUMBER OF DATA TYPES IN NAMELIST

      LOGICAL BUFLAG, STORFLAG   ! FLAGS FOR BUFR & STORAGE REQUIRED
      LOGICAL TERMIN8            ! OPERATOR'S TERMINATION FLAG
      LOGICAL NEWFILE            ! FLAG TO INDICATE NEW DATA SET

      CHARACTER S                ! 'S', OR ' ' IF ONLY 1 BULLETIN STORED
      CHARACTER*3 CRCRLF         ! ASCII 'CR-CR-LF'
      CHARACTER*4 CSTORE         ! FOR TEMPORARY 4-CHARACTER STORAGE
      CHARACTER*7 MIMJ           ! 'MIMIMJMJ' FROM MESSAGE
      CHARACTER*8 DATYPE         ! CODE FOR DATA TYPE
      CHARACTER*18 BUL18         ! 'TTAAII CCCC YYGGGG' FROM MESSAGE
      CHARACTER*40 ERRTXT        ! 40-CHARACTER DREGS MESSAGE
      CHARACTER*40 TEXT40        ! 40-CHARACTER TEXT STRING
      CHARACTER*44 DSN           ! DATA SET NAME (FROM "DSINFO")
      CHARACTER*80 HEAD          ! FOR REVISION INFORMATION          !2
      CHARACTER*28676 BULL       ! 28K BUFFER + 4 BYTES FOR "NNNN"

!                        VARIABLES RELATING TO BULLETIN HEADER DATA SET

      INTEGER ITEMS(0:NITEMS,MAXHED)           ! DATA PROCESSING ITEMS
      LOGICAL FLAGS(NFLAGS,MAXHED)             ! DATA PROCESSING FLAGS
      CHARACTER*6 HEAD1(MAXHED), HEAD2(MAXHED) ! HEADER RANGES
      CHARACTER*8 BULTYP(MAXHED)               ! STORAGE DATA SET CODES

!                                  COMMON (FOR DYNAMIC ALLOCATION ONLY)
      COMMON /COMBUF/ BULL
!                                                   DATA INITIALISATION
      DATA NIL /0/                                                 !2.2a

!-----------------------------------------------------------------------
!                   THE FOLLOWING VARIABLES ARE USED FOR BUFR DATA ONLY

      INTEGER MAXVALS, MAXDES    ! MAX DATA VALUES AND DESCRIPTORS
      PARAMETER (MAXVALS=32000, MAXDES=999)

      INTEGER DESCR(MAXDES)      ! BUFR DESCRIPTORS (FROM BULLETIN)
      INTEGER INLIST             ! SIZE OF 'LIST' ARRAY
      INTEGER LIST(8)            ! DESCRIPTORS TO BE FOUND BY 'DECORD'
      INTEGER NDES               ! NO. OF BUFR DESCRIPTORS IN BULLETIN
      INTEGER NOBS               ! NUMBER OF OBSERVATIONS IN BULLETIN
      REAL VALS(MAXVALS)         ! ARRAY OF DECODED DATA VALUES

      DATA INLIST/8/, LIST/1025,1026,1027,1028,1029,1030,1282,1538/

!                                  COMMON (FOR DYNAMIC ALLOCATION ONLY)
      COMMON /COMBUFR/ DESCR, VALS                                   !2

!-----------------------------------------------------------------------
!                    THE FOLLOWING VARIABLES ARE USED BY ALL DATA TYPES
!                                  EXCEPT ERS DATA, AMDARS AND WINDPROF
!-----------------------------------------------------------------------

      INTEGER IERR                ! ERROR CODE FROM "SYNHED"
      INTEGER IPT, BULEND         ! POINTERS TO START & END OF BULLETIN
      LOGICAL OERR                ! ERROR CODE FROM STORAGE ROUTINES
      LOGICAL OAMD, OCOR          ! AMENDMENT & CORRECTION FLAGS
      CHARACTER*2 AMDNUM, CORNUM  ! AMENDMENT & CORRECTION NUMBERS
      CHARACTER*4 CCCC            ! COLLECTING CENTRE
      CHARACTER*6 TTAAII, YYGGGG  ! BULLETIN HEADER AND DATE/TIME

!-----------------------------------------------------------------------
!                                                              NAMELIST
!-----------------------------------------------------------------------

      LOGICAL           PRINT
      INTEGER                  NWAIT, NDREG
      CHARACTER*8                            JOBNAME,   TYPES(MAXSTOR)
      NAMELIST /INSTOR/ PRINT, NWAIT, NDREG, JOBNAME,   TYPES
      DATA              PRINT, NWAIT, NDREG, JOBNAME,   TYPES
     &                /.TRUE.,    30,   99, 'MDB???? ', MAXSTOR*' '/

!                                                       DATA STATEMENTS
      DATA NFILES/0/, NTOTAL/0/, NFOUND /0/
      DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/
      DATA NEWFILE/.TRUE./, LSEQ/99*0/

!-----------------------------------------------------------------------
!     REVISION INFORMATION
!-----------------------------------------------------------------------

      HEAD = '$Workfile: mdbstor.f$ ' //
     &       '$Revision: 3$ $Date: 22/04/2008 14:07:14$'

!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETUP
!-----------------------------------------------------------------------

      CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF IN ASCII

!                                                    READ NAMELIST DATA
      OPEN (2, IOSTAT=IOS)
      IF (IOS.EQ.0) THEN
         READ (2, INSTOR, IOSTAT=IOS)
!                                           SET UNIT FOR DREGS DATA SET
         IF (IOS.EQ.0 .AND. NDREG.NE.99)               !2.2a (next line)
     &       CALL DREG (NDREG, 'DREGUNIT', ' ', ' ', ' ', ' ', NIL, ' ')
         CLOSE (2)
      END IF
!                             MAKE DATA SET OF WANTED GTS HEADER RANGES
      NTYPES = MAXSTOR
      CALL GETHDR (TYPES, NTYPES)
!                                          READ BULLETIN PROCESSING AND
!                                          STORAGE DATA SET INFORMATION
      NBULLS = MAXHED
      CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS,
     &               ITEMS, NBULLS, LENIDX, LSEQ, BULL)

!                                   OPEN HOUSEKEEPING DATA SET (UNIT 1)

      CALL DSINFO ('HKEEP', 3, 1, LENREC, KODE, DSN)
!-----------------------------------------------------------------------
!                                                        BUFR DATA ONLY
      CALL DSINFO ('CODEFIG', 0, 0, LENREC, KODE, DSN)
      CALL DSINFO ('TABLEB',  0, 0, LENREC, KODE, DSN)
      CALL DSINFO ('TABLED',  0, 0, LENREC, KODE, DSN)

!                                                  LNDSYN & SHPSYN ONLY

      CALL DSINFO ('ELEMIDX', 0, 0, LENREC, KODE, DSN)
!                                                           AIREPS ONLY
      CALL DSINFO ('MDBBCN',  0, 0, LENREC, KODE, DSN)             !1.4a
      CALL DSINFO ('STNICAO', 0, 0, LENREC, KODE, DSN)

!                          BOGUS, CLIMAT, ESAWS, HEALTHRR, METARS, NCM,
!                         SAMOSX, SREW, TAF, TBUS, TRACKOB & UAIR ONLY

      CALL DSINFO ('STNABRV', 0,  0, LENREC, KODE, DSN)
!-----------------------------------------------------------------------
!                      ASSIGN JOB STATUS RECORD AND GET FIRST DATA SET.
!          (FOR THIS FIRST CALL TO NEXTFILE, "TERMIN8" IS USED ON INPUT
!           TO INDICATE WHETHER MHS DATA SET NAMES ARE TO BE TAKEN FROM
!              THE H.K. DATA SET. THEREAFTER IT IS A TERMINATION FLAG.)

      NMSGS = NWAIT
      DSN(1:8) = JOBNAME
      TERMIN8 = .TRUE.   ! (I.E. GET MHS DATA SET NAMES FROM H.K.)
      CALL NEXTFILE (DSN, NMSGS, TERMIN8)

!-----------------------------------------------------------------------
!     LOOP OVER INCOMING BULLETINS.
!-----------------------------------------------------------------------

      DO WHILE (.NOT.TERMIN8)
!                                      OPEN DATA SET IF IT IS A NEW ONE
         IF (NEWFILE) THEN
            OPEN (10, FILE='/'//DSN, STATUS='OLD', ACTION='READ',
     &                IOSTAT=IOS)
            CALL DATIM (NOW) ! TIME OF RECEIPT
            NEWFILE = .FALSE.
         END IF
!                               LOCATE NEXT MESSAGE & CHECK RETURN CODE
         IF (IOS.EQ.0) THEN
            CALL NEXTMSG (10, BULL, MSTART, MLNGTH, MSGCODE)
         ELSE
            MSGCODE = 2
         END IF
!                       MSGCODE = 1 (END OF DATA SET) OR 2 (READ ERROR)
!                       -----------------------------------------------

         IF (MSGCODE.NE.0) THEN
            NFILES = NFILES + 1
            IF (PRINT) THEN
               S = 'S'
               IF (NFOUND.EQ.1) S = ' '
               WRITE (6,'(T2, 2(I2.2,A),I2.2, I6, 4A)')
     &                NOW(5), ':', NOW(4), ':', NOW(3), NFOUND,
     &               ' BULLETIN', S, ' STORED FROM  ', DSN
            END IF
!                                                        CLOSE DATA SET
            IF (MSGCODE.EQ.1) THEN
               CLOSE (10, STATUS='KEEP')
               IF (NFOUND.NE.NMSGS) WRITE (6,'(T5,A,T15,2A,2I6)')
     &            'MDBSTOR:', 'WARNING - BULLETIN TOTALS FOUND ',
     &            'AND EXPECTED DON''T AGREE:', NFOUND, NMSGS
               NFOUND = 0
            END IF
!                                         FIND NEXT DATA SET TO PROCESS

            CALL NEXTFILE (DSN, NMSGS, TERMIN8)
            NEWFILE = .TRUE.
!                                      MSGCODE = 0 (NEXT MESSAGE FOUND)
!                                      --------------------------------
         ELSE
            MEND = MSTART + MLNGTH - 1
            NCOL = 3     ! REJECT (RESET TO 2 LATER IF O.K.)

!              FIND FIRST BYTE AFTER 'CRCRLF' AT END OF HEADER. (HEADER
!              IS USUALLY 41 BYTES BUT MAY BE 45, E.G. 'AMD' OR 'COR'.)

            INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)          !2.5
            IF (INDX.LE.0) INDX = 5 !'CRCRLF' NOT FOUND: ASSUME 45 BYTES
            MFIRST = MSTART + INDX + 40                             !2.5
            MFINAL = MIN0(MEND,MSTART+131)
!                                                LOCATE BULLETIN HEADER
            BUL18 = BULL(MSTART+20:MSTART+37)                       !2.5
            CALL EBCDIC (18, BUL18)
!                                 LOOK UP LIST OF HEADER RANGES TO FIND
!                           RANGE CONTAINING HEADER OF CURRENT BULLETIN

            CALL SATYPE (BUL18(1:6), HEAD1, HEAD2, NBULLS, NBUL)

!                                              SET STORAGE & BUFR FLAGS
            IF (NBUL.LE.0) THEN
               DATYPE = 'UNKNOWN '
               STORFLAG = .FALSE.
               INDX = 0
            ELSE
               DATYPE = BULTYP(NBUL)
               STORFLAG = FLAGS(1,NBUL)
               BUFLAG = ITEMS(1,NBUL).EQ.1
               INDX = ITEMS(0,NBUL)  ! (Row number in summary table)

!                                   PASS INFORMATION TO DREG SUBROUTINE

               CALL DREG (0, ' ', ' ', ' ', DATYPE, BUL18, NOW, DSN)
            END IF

!-----------------------------------------------------------------------
!        IF BUFR DATA, CHECK MESSAGE QUALITY.
!-----------------------------------------------------------------------

            IF (STORFLAG) THEN
               TEXT40 = ' '
!                             CHECK MESSAGE AND GET BUFR TYPE & SUBTYPE
               KODE = 0
               IF (BUFLAG) THEN
                  NFOUND = NFOUND + 1
                  CALL BUFRCHEK (BULL(MFIRST:MEND), NOW, MBUFR, LENBUL,
     &                           IBTYP, IBSUB, KODE)
                  WRITE (TEXT40,'(A,2I4)') 'BUFR TYPE & SUBTYPE =',
     &                           IBTYP, IBSUB

!                                       START OF BUFR MESSAGE NOT FOUND
                  IF (KODE.EQ.1) THEN
                     WRITE (6,'(T2,4A)') BUL18, ':  DATA TYPE ', DATYPE,
     &                        ' START OF BUFR MESSAGE NOT FOUND.'
                     WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
                     CALL DREG (10, 'START OF BUFR MESSAGE NOT FOUND.',
     &                BULL(MFIRST:MEND),'MDBSTOR',' ',' ',NIL,' ') !2.2a
                     STORFLAG = .FALSE.
!                                         END OF BUFR MESSAGE NOT FOUND
                  ELSE IF (KODE.EQ.2) THEN
                     WRITE (6,'(T2,4A,T73,A)') BUL18, ':  DATA TYPE ',
     &                DATYPE, ' END OF BUFR MESSAGE NOT FOUND.', TEXT40
                     WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
                     CALL DREG (11, 'END OF BUFR MESSAGE NOT FOUND.',
     &                BULL(MFIRST:MEND),'MDBSTOR',' ',' ',NIL,' ') !2.2a
                     STORFLAG = .FALSE.
!                                             CHECK BUFR TYPE & SUBTYPE
                  ELSE
                     NBTYP = ITEMS(6,NBUL)
                     NBSUB = ITEMS(7,NBUL)
                     IF ((NBTYP.GE.0 .AND. NBTYP.NE.IBTYP) .OR.
     &                   (NBSUB.GE.0 .AND. NBSUB.NE.IBSUB)) THEN
                        WRITE (6,'(T2,5A)') BUL18, ':  DATA TYPE ',
     &                                   DATYPE, ' UNEXPECTED ', TEXT40
                        WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
                        WRITE (ERRTXT,'(A,2(I5,A))')
     &                                'BUFR TYPE & SUBTYPE =',
     &                                 IBTYP, ' &', IBSUB, '.'
                        CALL DREG (12, ERRTXT, BULL(MFIRST:MEND),
     &                            'MDBSTOR', ' ', ' ', NIL, ' ')   !2.2a
                        STORFLAG = .FALSE.
!                                           MESSAGE IS OK: SET POINTERS
!                                          AND CONVERT HEADER TO EBCDIC
                     ELSE
                        CALL EBCDIC (MFIRST-MSTART,                 !2.5
     &                               BULL(MSTART:MFIRST-1))         !2.5
                        MBUFR = MFIRST + MBUFR - 1
                        M7777 = MBUFR + LENBUL - 1
                     END IF
                  END IF

!-----------------------------------------------------------------------
!        IF CHARACTERS, CONVERT TO EBCDIC AND CHECK STORAGE CRITERIA.
!-----------------------------------------------------------------------

!                                               CONVERT ASCII TO EBCDIC
               ELSE IF (ITEMS(1,NBUL).EQ.2) THEN
                  CALL EBCDIC (MLNGTH, BULL(MSTART:MEND))           !2.5

!                                      CHECK FOR EXTRA STORAGE CRITERIA
                  MIMJ = ' '
                  IF (FLAGS(2,NBUL)) CALL STORCHEK
     &                (BULL(MFIRST:MEND), BUL18, DATYPE, STORFLAG, MIMJ)
                  IF (STORFLAG) NFOUND = NFOUND + 1
               END IF
            END IF

!-----------------------------------------------------------------------
!        FIND UNIT & RECORD LENGTH FOR STORAGE, AND CHECK RETURN CODE.
!-----------------------------------------------------------------------

            IF (STORFLAG) THEN
               CALL DSINFO (DATYPE, -1, IUNIT, LENREC, KODE, DSN)

!                                         WARNING FOR UNKNOWN DATA TYPE
               IF (KODE.EQ.2) THEN
                  WRITE (6,'(T2,3A,T58,A)') BUL18,
     &                     ':  UNRECOGNISED DATA TYPE: ', DATYPE,TEXT40
                  WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
                  CALL DREG (13, 'UNRECOGNISED DATA TYPE.',
     &             BULL(MFIRST:MEND),'MDBSTOR',DATYPE,' ',NIL,' ') !2.2a

!                                     WARNING FOR INACCESSIBLE DATA SET

               ELSE IF (KODE.EQ.3) THEN
                  WRITE (6,'(T2,3A,T58,A)') BUL18,
     &                  ':  INACCESSIBLE DATA SET FOR: ', DATYPE,TEXT40
                  WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
                  CALL DREG (14, 'INACCESSIBLE STORAGE DATA SET.',
     &             BULL(MFIRST:MEND),'MDBSTOR',DATYPE,' ',NIL,' ') !2.2a

!                             FORGET IT IF STORAGE DATA SET IS NOT OPEN

               ELSE IF (IUNIT.EQ.0) THEN
                  NFOUND = NFOUND - 1

!                              ADD 'NNNN' AND ENSURE 6-CHARACTER TTAAII
!                                   (REQUIRED BY 'BULHED' AND 'SYNHED')
               ELSE
                  J = INDEX(BULL(MSTART+20:MSTART+25),' ')          !2.5
                  DO WHILE (J.GT.0)      ! (I.E. SPACE IN "TTAAII")
                     J = MSTART + 19 + J                            !2.5
                     BULL(J:J) = '/'
                     J = INDEX(BULL(MSTART+20:MSTART+25),' ')       !2.5
                  END DO
!
                  CSTORE = BULL(MEND+1:MEND+4)
                  BULL(MEND+1:MEND+4) = 'NNNN'
                  IERR = 0
                  NCOL = 2 ! BULLETIN OK

!-----------------------------------------------------------------------
!        ALL OK, SO CALL MET.D.B. STORAGE ROUTINE.
!
!   (INDENTING CHANGED HERE TO AVOID GETTING INDENTED OFF THE SCREEN!)
!-----------------------------------------------------------------------

!                                                  STORE METAR BULLETIN
      IF (DATYPE.EQ.'METARS') THEN
         CALL BULHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, BULL(MSTART:MEND+4))
         CALL METBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                    STORE TAF BULLETIN

      ELSE IF (DATYPE.EQ.'TAFS'. OR. DATYPE.EQ.'ATAFS') THEN
         CALL BULHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, BULL(MSTART:MEND+4))
         CALL TAFBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                 STORE SAMOSX BULLETIN
      ELSE IF (DATYPE.EQ.'SAMOSX') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL SAMBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                    STORE NCM BULLETIN
      ELSE IF (DATYPE.EQ.'NCM') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL NCMBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,              !2.1
     &                OCOR,CORNUM, MIMJ, IUNIT, BULL(MSTART:MEND+4))

!                                    STORE SYNOP BULLETIN (LAND & SHIP)
      ELSE IF (DATYPE.EQ.'LNDSYN' .OR.                               !3
     &         DATYPE.EQ.'SHPSYN' .OR. DATYPE.EQ.'MOBSYN') THEN      !3
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL SYNBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OCOR,
     &                CORNUM, MIMJ, IUNIT, BULL(MSTART:MEND+4))     !2.1

!                                                  STORE SATOB BULLETIN
      ELSE IF (DATYPE.EQ.'SATOBS') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL STBBUL (IPT, BULEND, TTAAII, CCCC, OCOR, IUNIT,       !2.4
     &                BULL(MSTART:MEND+4))                          !2.1
!                                                   STORE BUOY BULLETIN
      ELSE IF (DATYPE.EQ.'BUOY') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL  BUOY  (IPT, BULEND, TTAAII,CCCC,
     &                OCOR, MIMJ(1:4), IUNIT, BULL(MSTART:MEND+4))

!                                                 STORE CLIMAT BULLETIN
      ELSE IF (DATYPE.EQ.'CLIMAT') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL CLMBUL (IPT, BULEND, TTAAII,CCCC,
     &                OCOR, IUNIT, BULL(MSTART:MEND+4))             !2.0

!                                                   STORE SREW BULLETIN
      ELSE IF (DATYPE.EQ.'SREW') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL SRWBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,
     &                OCOR,CORNUM, MIMJ, IUNIT,                     !1.3
     &                BULL(MSTART:MEND+4))
!                                                  STORE AIREP BULLETIN
      ELSE IF (DATYPE.EQ.'AIREP') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL AIRBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,
     &                IUNIT, 81, BULL(MSTART:MEND+4))              !1.4a

!                                      STORE TROPICAL ADVISORY BULLETIN
      ELSE IF (DATYPE.EQ.'TROPADV') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL ADVBUL (BULEND, TTAAII,CCCC,YYGGGG, OCOR, IUNIT,
     &                BULL(MSTART:MEND+4))                          !2.0
!                                              STORE UPPER AIR BULLETIN
      ELSE IF (DATYPE.EQ.'UPRAIR') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL UABUL  (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC,
     &                YYGGGG, CORNUM, IUNIT)
!                                                   STORE TBUS BULLETIN
      ELSE IF (DATYPE.EQ.'TBUS') THEN
         CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                 OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL TBUSBUL (IPT, BULEND, TTAAII,YYGGGG,
     &                 IUNIT, OERR, BULL(MSTART:MEND+4))            !2.1

!                                          STORE HEALTH RESORT BULLETIN
!                                          (Data stopped in March 2007)
!     ELSE IF (DATYPE.EQ.'HEALTHRR') THEN                            !2
!        CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,  !2
!    &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))  !2
!        IF (IERR.EQ.0)                                              !2
!    &   CALL HRRBUL (TTAAII, CCCC, IUNIT, OCOR, CORNUM,             !2
!    &                BULL(MSTART:MEND+4))                           !2
!                                                  STORE ESAWS BULLETIN
      ELSE IF (DATYPE.EQ.'ESAWS') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL ENHBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,
     &                OCOR,CORNUM, MIMJ, IUNIT,                     !1.3
     &                BULL(MSTART:MEND+4))
!                                                 STORE SFERIC BULLETIN
      ELSE IF (DATYPE.EQ.'SFERICS') THEN
         CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                 OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL SFERICS (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC,
     &                 CORNUM, IUNIT)                               !2.1
!                                                STORE PRESTEL BULLETIN
      ELSE IF (DATYPE.EQ.'PRESTEL') THEN
         CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                 OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL PRESTEL (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC,
     &                 YYGGGG, IUNIT)                               !2.1
!                                         STORE BATHY OR TESAC BULLETIN
      ELSE IF (DATYPE.EQ.'SUBSEA') THEN
            CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                   OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
            IF (IERR.EQ.0)
     &      CALL BATESBUL (BULL(MSTART:MEND+4), TTAAII, CCCC,      !2.2
     &                     OCOR, MIMJ(1:4), IUNIT)                 !2.2
!                                                STORE TRACKOB BULLETIN
      ELSE IF (DATYPE.EQ.'TRKOB') THEN
         CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
     &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
         IF (IERR.EQ.0)
     &   CALL TRKBUL (BULL(MSTART:MEND+4), BULEND, TTAAII, CCCC,
     &                YYGGGG, CORNUM, IUNIT)

!                                  STORE TROPICAL CYCLONE BULLETIN !1.4

      ELSE IF (DATYPE.EQ.'TROPCYCL') THEN                          !1.4
         CALL SYNHED(IPT,BULEND,TTAAII,CCCC,YYGGGG,OAMD,AMDNUM,
     &               OCOR,CORNUM,MIMJ,IERR,BULL(MSTART:MEND+4))    !1.4
         IF (IERR.EQ.0)                                            !1.4
     &   CALL TROPBUL (BULL(MSTART:MEND), TTAAII, CCCC,
     &                YYGGGG, IUNIT)                               !2.1
!                                               STORE ERS DATA BULLETIN
      ELSE IF (DATYPE(1:3).EQ.'ERS') THEN
         NDES = MAXDES
         NOBS = MAXVALS
         CALL DECORD (DESCR, VALS, TEXT40, NDES, NOBS,
     &                BULL(MSTART:MEND+4), .FALSE., LIST, INLIST)
         IF (NOBS.GT.0 .AND. NDES*NOBS.LE.MAXVALS) THEN
            CALL ERSIND (VALS, NDES, NOBS, DESCR, BULL(MBUFR:M7777),
     &                   IUNIT)
         ELSE
            WRITE (6,'(T5,A,T15,3A,2I6)') 'MDBSTOR:',
     &         'FAILED TO DECODE DATA TYPE ', DATYPE,
     &         ' - "NDES" & "NOBS" =', NDES, NOBS
         END IF
!                                                  STORE AMDAR BULLETIN
!                                             (TTAAii starts at byte 15)
      ELSE IF (DATYPE.EQ.'AMDARS') THEN
         CALL AMDAR (BULL(MSTART+14:MEND+4), BUFLAG, IUNIT)         !2.3

!                                          STORE WIND PROFILER BULLETIN
      ELSE IF (DATYPE.EQ.'WINDPROF') THEN
         CALL WINPRO (BULL(MSTART:MEND+4), IUNIT)
!                                                  SOME OTHER DATA TYPE
      ELSE
         WRITE (6,'(T5,A,T15,2A)') 'MDBSTOR:',
     &            'CANNOT PROCESS DATA TYPE ', DATYPE
         NCOL = 3
      END IF
!                                       WARNING FOR 'SYNHED' ERROR CODE
      IF (IERR.GT.0) THEN
         WRITE (6,'(T5,A,T15,2A,I8)') 'MDBSTOR:',
     &            'ERROR CODE FROM "SYNHED" FOR DATA TYPE ', DATYPE,IERR
         NCOL = 3
         WRITE(ERRTXT,'(A,I3,A)') 'ERROR CODE FROM "SYNHED" =',IERR,'.'
         CALL DREG (16, ERRTXT, BULL(MFIRST:MEND), 'MDBSTOR',
     &              DATYPE, ' ', NIL, ' ')                         !2.2a
      END IF

!-----------------------------------------------------------------------
!     PRINT INFORMATION MESSAGE (OPTIONAL), UPDATE BULLETIN COUNTERS
!     AND RETURN FOR ANOTHER MESSAGE. (ORIGINAL INDENTING RESUMED)
!-----------------------------------------------------------------------

!                              RESTORE WHAT WAS OVERWRITTEN WITH "NNNN"

                  BULL(MEND+1:MEND+4) = CSTORE

!                               OPTIONAL MESSAGE FOR SUCCESSFUL STORAGE

                  IF (ITEMS(2,NBUL).GE.1) WRITE (6,'(T2,3A)') BUL18,
     &                      ':  DATA TYPE ', DATYPE
               END IF
!                           OPTIONAL MESSAGE FOR DATA TYPE NOT REQUIRED
            ELSE
               IF (ITEMS(2,NBUL).GE.1)
     &            WRITE (6,'(T2,4A)') BUL18, ':  DATA TYPE ', DATYPE,
     &                                       '   STORAGE NOT REQUIRED.'
            END IF

            KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
            KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1
            NTOTAL = NTOTAL + 1
         END IF
!                                          GO BACK FOR THE NEXT MESSAGE
      END DO ! WHILE

!-----------------------------------------------------------------------
!     SUMMARY TABLE OF MESSAGES PROCESSED AND TERMINATION MESSAGES.
!-----------------------------------------------------------------------

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

      STOP
      END
