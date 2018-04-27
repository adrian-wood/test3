PROGRAM STORAMD

!-----------------------------------------------------------------------
!
! PROGRAM     : STORAMD    (MET.D.B. AMDAR STORAGE JOB)
!
! PURPOSE     : TO STORE AMDAR BULLETINS IN THE MET.D.B.
!               THIS PROGRAM IS BASED ON MDBSTOR AND WAS WRITTEN IN
!               NOVEMBER 2012 TO OVERCOME AMDAR STORAGE PROBLEMS.
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
!                   - CALL AMDAR STORAGE ROUTINE TO STORE THE BULLETIN.
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
!                TYPES   C*8  (ARRAY) THIS SHOULD CONTAIN     BLANKS
!                             JUST 'AMDARS'.
!
! CALLS       : AMDBUL, BUFRCHEK, DATIM, DREG, DSINFO, EBCDIC, GETHDR,
!               INITSTOR, NEXTFILE, NEXTMSG, SATYPE, STORCHEK, SUMMARY.
!
! FILES USED  : MHS DATA SETS OPENED AND CLOSED ON UNIT 10.
!               UNIT 2 (NAMELIST INPUT) OPENED, READ AND CLOSED.
!
! REVISION INFO :
!
! $Workfile: storamd.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 13/02/2013 16:08:09$
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         13/02/2013 16:08:09    Brian Barwell   New
!       AMDAR storage program.
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2013 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE amdbul_mod
USE bufrchek_mod
USE datim_mod
USE dreg_mod
USE dsinfo_mod
USE ebcdic_mod
USE gethdr_mod
USE initstor_mod
USE nextfile_mod
USE nextmsg_mod
USE satype_mod
USE storchek_mod
USE summary_mod

IMPLICIT NONE
!                                                            PARAMETERS

INTEGER, PARAMETER          :: MAXSTOR=50             ! MAXIMUM NUMBER OF STORAGE DATA SET CODES
INTEGER, PARAMETER          :: MAXHED=120             ! MAXIMUM NUMBER OF BULLETIN HEADER RANGES
INTEGER, PARAMETER          :: NFLAGS=9               ! NUMBER OF DATA PROCESSING FLAGS
INTEGER, PARAMETER          :: NITEMS=12              ! NUMBER OF DATA PROCESSING ITEMS

!                                                             VARIABLES

INTEGER                     :: IBTYP                  ! BUFR Type from MESSAGE
INTEGER                     :: IBSUB                  ! SUBTYPE FROM MESSAGE
INTEGER                     :: INDX                   ! POINTER TO 2ND SUBSCRIPT OF "KOUNTS"
INTEGER                     :: IOS                    ! RETURN STATUS FROM I/O STATEMENT
INTEGER                     :: IUNIT                  ! UNIT NUMBER OF STORAGE DATA SET
INTEGER                     :: J                      ! VARIABLE FOR LOCAL USE
INTEGER                     :: KODE                   ! RETURN CODE (E.G. FROM 'DSINFO')
INTEGER                     :: KOUNTS(3,0:MAXHED)     ! COUNTERS FOR NUMBERS OF MESSAGES
INTEGER                     :: LENIDX(99)             ! INDEX LENGTHS FOR UNITS 1-99
INTEGER                     :: LENBUL                 ! LENGTH OF BUFR BULLETIN
INTEGER                     :: LENREC                 ! RECORD LENGTH OF STORAGE DATA SET
INTEGER                     :: LSEQ(99)=0               ! BUFR SEQUENCES FOR UNITS 1-99
INTEGER                     :: MBUFR                  ! LOCATION OF "BUFR"
INTEGER                     :: M7777                  ! LOCATIONS OF END OF "7777"
INTEGER                     :: MFIRST                 ! FIRST BYTE OF MESSAGE AFTER HEADER
INTEGER                     :: MLNGTH                 ! MESSAGE LENGTH
INTEGER                     :: MFINAL                 ! LAST BYTE FOR PRINT
INTEGER                     :: MSGCODE                ! RETURN CODE FROM "FINDMSG"
INTEGER                     :: MSTART                 ! START BYTES OF MESSAGE
INTEGER                     :: MEND                   ! FINISH BYTES OF MESSAGE
INTEGER                     :: NBTYP                  ! EXPECTED BUFR TYPE
INTEGER                     :: NBSUB                  ! EXPECTED BUFR SUBTYPE
INTEGER                     :: NBUL                   ! BULLETIN RANGE NO. & NUMBER OF RANGES
INTEGER                     :: NBULLS                 ! BULLETIN NUMBER OF RANGES
INTEGER                     :: NCOL                   ! COUNTER INDEX (2/3 = IS/ISN'T STORED)
INTEGER                     :: NFILES=0               ! NUMBER OF DATA SETS PROCESSED SO FAR
INTEGER                     :: NFOUND=0               ! WANTED BULLETINS FOUND IN DATA SET
INTEGER                     :: NIL(8)=0               ! ZERO IN ARRAY FOR CALLS TO DREG
INTEGER                     :: NMSGS                  ! WANTED BULLETINS EXPECTED IN DATA SET
INTEGER                     :: NOW(8)                 ! DATE/TIME OF RECEIPT (FROM 'DATIM')
INTEGER                     :: NTOTAL=0               ! TOTAL OF MESSAGES FOUND SO FAR
INTEGER                     :: NTYPES                 ! NUMBER OF DATA TYPES IN NAMELIST

LOGICAL                     :: BUFLAG                 ! FLAGS FOR BUFR REQUIRED
LOGICAL                     :: NEWFILE=.TRUE.         ! FLAG TO INDICATE NEW DATA SET
LOGICAL                     :: STORFLAG               ! FLAGS FOR STORAGE REQUIRED
LOGICAL                     :: TERMIN8                ! OPERATOR'S TERMINATION FLAG

CHARACTER(LEN=18)           :: BUL18                  ! 'TTAAII CCCC YYGGGG' FROM MESSAGE
CHARACTER(LEN=28676)        :: BULL                   ! 28K BUFFER + 4 BYTES FOR "NNNN"
CHARACTER(LEN=3)            :: CRCRLF                 ! ASCII 'CR-CR-LF'
CHARACTER(LEN=4)            :: CSTORE                 ! FOR TEMPORARY 4-CHARACTER STORAGE
CHARACTER(LEN=8)            :: DATYPE                 ! CODE FOR DATA TYPE
CHARACTER(LEN=44)           :: DSN                    ! DATA SET NAME (FROM "DSINFO")
CHARACTER(LEN=40)           :: ERRTXT                 ! 40-CHARACTER DREGS MESSAGE
CHARACTER(LEN=7)            :: MIMJ                   ! 'MIMIMJMJ' FROM MESSAGE
CHARACTER(LEN=1)            :: S                      ! 'S', OR ' ' IF ONLY 1 BULLETIN STORED
CHARACTER(LEN=40)           :: TEXT40                 ! 40-CHARACTER TEXT STRING

!                        Dummy variables required for calls
CHARACTER(LEN=8)            :: dummyBull              ! used for where a dummy argument passed for BULL
INTEGER                     :: unitToSet              ! used for unit numbers in calls to DSINFO, original code just
                                                      ! passed a number but there is a potential output if
                                                      ! unit number already open.

!                        VARIABLES RELATING TO BULLETIN HEADER DATA SET

CHARACTER(LEN=8)            :: BULTYP(MAXHED)               ! STORAGE DATA SET CODES
LOGICAL                     :: FLAGS(NFLAGS,MAXHED)  ! DATA PROCESSING FLAGS
CHARACTER(LEN=6)            :: HEAD1(MAXHED), HEAD2(MAXHED) ! HEADER RANGES
INTEGER                     :: ITEMS(0:NITEMS,MAXHED) ! DATA PROCESSING ITEMS

!-----------------------------------------------------------------------
!                    THE FOLLOWING VARIABLES ARE USED BY ALL DATA TYPES
!                                  EXCEPT ERS DATA, AMDARS AND WINDPROF
!-----------------------------------------------------------------------

INTEGER                     :: IERR               ! ERROR CODE FROM "SYNHED"
INTEGER,PARAMETER           :: LRECL=4096         ! block size of MHS datasets

!-----------------------------------------------------------------------
!                                                              NAMELIST
!-----------------------------------------------------------------------

LOGICAL                     :: PRINT
INTEGER                     :: NWAIT
INTEGER                     :: NDREG
CHARACTER*8                 :: JOBNAME,   TYPES(MAXSTOR)
NAMELIST /INSTOR/ PRINT, NWAIT, NDREG, JOBNAME,   TYPES
DATA              PRINT, NWAIT, NDREG, JOBNAME,   TYPES  &
                /.TRUE.,    30,   99, 'MDB???? ', MAXSTOR*' '/

!                                                       DATA STATEMENTS
DATA KOUNTS /3*0, MAXHED*0, MAXHED*0, MAXHED*0/

!-----------------------------------------------------------------------
!     INITIALISATIONS AND SETUP
!-----------------------------------------------------------------------

CRCRLF = CHAR(13)//CHAR(13)//CHAR(10) ! CR-CR-LF IN ASCII

!                                                    READ NAMELIST DATA
OPEN (2, FILE='DD:FT02F001', ACTION='READ', IOSTAT=IOS)
IF (IOS == 0) THEN
  READ (2, INSTOR, IOSTAT=IOS)
!                                           SET UNIT FOR DREGS DATA SET
  IF (IOS == 0 .AND. NDREG /= 99) &
    CALL DREG (NDREG, 'DREGUNIT', dummyBull, ' ', ' ', ' ', NIL, ' ')
  CLOSE (2)
END IF
!                             MAKE DATA SET OF WANTED GTS HEADER RANGES
NTYPES = MAXSTOR
CALL GETHDR (TYPES, NTYPES)
!                                          READ BULLETIN PROCESSING AND
!                                          STORAGE DATA SET INFORMATION
NBULLS = MAXHED
CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, ITEMS, NBULLS,  &
               LENIDX, LSEQ, BULL)
!                                   OPEN HOUSEKEEPING DATA SET (UNIT 1)
unitToSet = 1
CALL DSINFO ('HKEEP', 3, unitToSet, LENREC, KODE, DSN)

!                                                  ALLOCATE BUFR TABLES
unitToSet = 0
CALL DSINFO ('CODEFIG', 0, unitToSet, LENREC, KODE, DSN)
unitToSet = 0
CALL DSINFO ('TABLEB',  0, unitToSet, LENREC, KODE, DSN)
unitToSet = 0
CALL DSINFO ('TABLED',  0, unitToSet, LENREC, KODE, DSN)

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

BulletinLoop: &
DO WHILE (.NOT.TERMIN8)
!                                      OPEN DATA SET IF IT IS A NEW ONE
  IF (NEWFILE) THEN
    OPEN (10, FILE="//'"//DSN//"'", ACTION='READ', &
              IOSTAT=IOS,ACCESS='DIRECT',RECL=LRECL)
    CALL DATIM (NOW) ! TIME OF RECEIPT
    NEWFILE = .FALSE.
  END IF
!                               LOCATE NEXT MESSAGE & CHECK RETURN CODE
  IF (IOS == 0) THEN
    CALL NEXTMSG (10, BULL(1:28672), MSTART, MLNGTH, MSGCODE)
  ELSE
    MSGCODE = 2
  END IF
!                       MSGCODE = 1 (END OF DATA SET) OR 2 (READ ERROR)
!                       -----------------------------------------------
MsgCodeCheck1: &
  IF (MSGCODE /= 0) THEN
    NFILES = NFILES + 1
    IF (PRINT) THEN
      S = 'S'
      IF (NFOUND == 1) S = ' '
        WRITE (6,'(T2, 2(I2.2,A),I2.2, I6, 4A)') &
             NOW(5), ':', NOW(4), ':', NOW(3), NFOUND, &
            ' BULLETIN', S, ' STORED FROM  ', DSN
    END IF
!                                                        CLOSE DATA SET
    IF (MSGCODE == 1) THEN
      CLOSE (10, STATUS='KEEP')
      IF (NFOUND /= NMSGS) WRITE (6,'(T5,A,T15,2A,2I6)') &
         'MDBSTOR:', 'WARNING - BULLETIN TOTALS FOUND ', &
         'AND EXPECTED DON''T AGREE:', NFOUND, NMSGS
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

    INDX = INDEX(BULL(MSTART+38:MSTART+44),CRCRLF)
    IF (INDX <= 0) INDX = 5 !'CRCRLF' NOT FOUND: ASSUME 45 BYTES
    MFIRST = MSTART + INDX + 40
    MFINAL = MIN0(MEND,MSTART+131)
!                                                LOCATE BULLETIN HEADER
    BUL18 = BULL(MSTART+20:MSTART+37)
    CALL EBCDIC (18, BUL18)
!                                 LOOK UP LIST OF HEADER RANGES TO FIND
!                           RANGE CONTAINING HEADER OF CURRENT BULLETIN

    CALL SATYPE (BUL18(1:6), HEAD1, HEAD2, NBULLS, NBUL)

!                                              SET STORAGE & BUFR FLAGS
    IF (NBUL <= 0) THEN
      DATYPE = 'UNKNOWN '
      STORFLAG = .FALSE.
      INDX = 0
    ELSE
      DATYPE = BULTYP(NBUL)
      STORFLAG = FLAGS(1,NBUL)
      BUFLAG = ITEMS(1,NBUL) == 1
      INDX = ITEMS(0,NBUL)  ! (Row number in summary table)

!                                   PASS INFORMATION TO DREG SUBROUTINE

      CALL DREG (0,dummyBull,dummyBull,dummyBull, DATYPE, BUL18, NOW, DSN)
    END IF

!-----------------------------------------------------------------------
!        IF BUFR DATA, CHECK MESSAGE QUALITY.
!-----------------------------------------------------------------------
StorFlagCheck: &
    IF (STORFLAG) THEN
      TEXT40 = ' '
!                             CHECK MESSAGE AND GET BUFR TYPE & SUBTYPE
      KODE = 0

BufFlagCheck: &
      IF (BUFLAG) THEN
        NFOUND = NFOUND + 1
        CALL BUFRCHEK (BULL(MFIRST:MEND), NOW, MBUFR, LENBUL,&
                        IBTYP, IBSUB, KODE)
        WRITE (TEXT40,'(A,2I4)') 'BUFR TYPE & SUBTYPE =', &
                        IBTYP, IBSUB
!                                       START OF BUFR MESSAGE NOT FOUND
KodeCheck: &
        IF (KODE == 1) THEN
          WRITE (6,'(T2,4A)') BUL18, ':  DATA TYPE ', DATYPE,&
                     ' START OF BUFR MESSAGE NOT FOUND.'
          WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
          CALL DREG (10, 'START OF BUFR MESSAGE NOT FOUND.', &
              BULL(MFIRST:MEND),'MDBSTOR',' ',' ',NIL,' ')
          STORFLAG = .FALSE.
!                                         END OF BUFR MESSAGE NOT FOUND
        ELSE IF (KODE == 2) THEN
          WRITE (6,'(T2,4A,T73,A)') BUL18, ':  DATA TYPE ', &
            DATYPE, ' END OF BUFR MESSAGE NOT FOUND.', TEXT40
          WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
          CALL DREG (11, 'END OF BUFR MESSAGE NOT FOUND.', &
          BULL(MFIRST:MEND),'MDBSTOR',' ',' ',NIL,' ')
          STORFLAG = .FALSE.
!                                             CHECK BUFR TYPE & SUBTYPE
        ELSE
          NBTYP = ITEMS(6,NBUL)
          NBSUB = ITEMS(7,NBUL)

BufSubTypeCheck: &
          IF ((NBTYP >= 0 .AND. NBTYP /= IBTYP) .OR. &
              (NBSUB >= 0 .AND. NBSUB /= IBSUB)) THEN
            WRITE (6,'(T2,5A)') BUL18, ':  DATA TYPE ', &
                     DATYPE, ' UNEXPECTED ', TEXT40
            WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
            WRITE (ERRTXT,'(A,2(I5,A))') &
                    'BUFR TYPE & SUBTYPE =', &
                     IBTYP, ' &', IBSUB, '.'
            CALL DREG (12, ERRTXT, BULL(MFIRST:MEND), &
                    'MDBSTOR', ' ', ' ', NIL, ' ')
            STORFLAG = .FALSE.
!                                           MESSAGE IS OK: SET POINTERS
!                                          AND CONVERT HEADER TO EBCDIC
          ELSE
            CALL EBCDIC (MFIRST-MSTART,  &
                         BULL(MSTART:MFIRST-1))
            MBUFR = MFIRST + MBUFR - 1
            M7777 = MBUFR + LENBUL - 1
          END IF BufSubTypeCheck
        END IF KodeCheck

!-----------------------------------------------------------------------
!        IF CHARACTERS, CONVERT TO EBCDIC AND CHECK STORAGE CRITERIA.
!-----------------------------------------------------------------------

!                                               CONVERT ASCII TO EBCDIC
      ELSE IF (ITEMS(1,NBUL) == 2) THEN
        CALL EBCDIC (MLNGTH, BULL(MSTART:MEND))

!                                      CHECK FOR EXTRA STORAGE CRITERIA
        MIMJ = ' '
        IF (FLAGS(2,NBUL)) CALL STORCHEK &
           (BULL(MFIRST:MEND), BUL18, DATYPE, STORFLAG, MIMJ)
        IF (STORFLAG) NFOUND = NFOUND + 1
      END IF BufFlagCheck
    END IF StorFlagCheck

!-----------------------------------------------------------------------
!        FIND UNIT & RECORD LENGTH FOR STORAGE, AND CHECK RETURN CODE.
!-----------------------------------------------------------------------

StorFlagCheck2: &
    IF (STORFLAG) THEN
      CALL DSINFO (DATYPE, -1, IUNIT, LENREC, KODE, DSN)

!                                         WARNING FOR UNKNOWN DATA TYPE
KodeCheck2: &
      IF (KODE == 2) THEN
        WRITE (6,'(T2,3A,T58,A)') BUL18, &
                   ':  UNRECOGNISED DATA TYPE: ', DATYPE,TEXT40
        WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
        CALL DREG (13, 'UNRECOGNISED DATA TYPE.', &
        BULL(MFIRST:MEND),'MDBSTOR',DATYPE,' ',NIL,' ')

!                                     WARNING FOR INACCESSIBLE DATA SET
      ELSE IF (KODE == 3) THEN
        WRITE (6,'(T2,3A,T58,A)') BUL18, &
              ':  INACCESSIBLE DATA SET FOR: ', DATYPE,TEXT40
        WRITE (6,'(T2,A)') BULL(MSTART:MFINAL)
        CALL DREG (14, 'INACCESSIBLE STORAGE DATA SET.', &
        BULL(MFIRST:MEND),'MDBSTOR',DATYPE,' ',NIL,' ')

!                             FORGET IT IF STORAGE DATA SET IS NOT OPEN

      ELSE IF (IUNIT == 0) THEN
        NFOUND = NFOUND - 1
!                              ADD 'NNNN' AND ENSURE 6-CHARACTER TTAAII
!                                   (REQUIRED BY 'BULHED' AND 'SYNHED')
      ELSE
        J = INDEX(BULL(MSTART+20:MSTART+25),' ')
        DO WHILE (J > 0)       ! (I.E. SPACE IN "TTAAII")
          J = MSTART + 19 + J
          BULL(J:J) = '/'
          J = INDEX(BULL(MSTART+20:MSTART+25),' ')
        END DO
!
        CSTORE = BULL(MEND+1:MEND+4)
        BULL(MEND+1:MEND+4) = 'NNNN'
        IERR = 0
        NCOL = 2 ! BULLETIN OK

!-----------------------------------------------------------------------
!        ALL OK, SO CALL MET.D.B. STORAGE ROUTINE.
!-----------------------------------------------------------------------

!                                                  STORE AMDAR BULLETIN
!                                             (TTAAii starts at byte 15)
        IF (DATYPE == 'AMDARS')   &
          CALL AMDBUL (BULL(MSTART+14:MEND+4), BUFLAG, IUNIT)

!                                       WARNING FOR 'SYNHED' ERROR CODE
        IF (IERR > 0) THEN
          WRITE (6,'(T5,A,T15,2A,I8)') 'MDBSTOR:', &
              'ERROR CODE FROM "SYNHED" FOR DATA TYPE ', DATYPE,IERR
          NCOL = 3
          WRITE(ERRTXT,'(A,I3,A)') 'ERROR CODE FROM "SYNHED" =',IERR,'.'
          CALL DREG (16, ERRTXT, BULL(MFIRST:MEND), 'MDBSTOR', &
            DATYPE, ' ', NIL, ' ')
        END IF

!-----------------------------------------------------------------------
!     PRINT INFORMATION MESSAGE (OPTIONAL), UPDATE BULLETIN COUNTERS
!     AND RETURN FOR ANOTHER MESSAGE.
!-----------------------------------------------------------------------

!                              RESTORE WHAT WAS OVERWRITTEN WITH "NNNN"

        BULL(MEND+1:MEND+4) = CSTORE

!                               OPTIONAL MESSAGE FOR SUCCESSFUL STORAGE

        IF (ITEMS(2,NBUL) >= 1) WRITE (6,'(T2,3A)') BUL18, &
                      ':  DATA TYPE ', DATYPE
      END IF KodeCheck2
!                           OPTIONAL MESSAGE FOR DATA TYPE NOT REQUIRED
    ELSE IF (NBUL > 0) THEN
      IF (ITEMS(2,NBUL) >= 1) WRITE (6,'(T2,4A)')  &
          BUL18, ':  DATA TYPE ', DATYPE, '   STORAGE NOT REQUIRED.'
    END IF StorFlagCheck2

    KOUNTS(1,INDX) = KOUNTS(1,INDX) + 1
    KOUNTS(NCOL,INDX) = KOUNTS(NCOL,INDX) + 1
    NTOTAL = NTOTAL + 1
  END IF MsgCodeCheck1
!                                          GO BACK FOR THE NEXT MESSAGE
END DO BulletinLoop

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
END DO
!                                                         Summary table
CALL SUMMARY (NFILES, NTOTAL, KOUNTS, BULTYP, NBUL)

STOP
END PROGRAM STORAMD
