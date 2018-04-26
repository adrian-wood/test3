PROGRAM MDBSTOR

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
! CALLS       : ADVBUL, AIRBUL, AMDAR, BATESBUL, BUFRCHEK, BULHED,
!               BUOY, CLMBUL, DATIM, DECORD, DREG, DSINFO, EBCDIC,
!               ENHBUL, ERSIND, GETHDR, INITSTOR, METBUL, NCMBUL,
!               NEXTFILE, NEXTMSG, PRESTEL, SAMBUL, SATYPE, SFERICS,
!               SRWBUL, STBBUL, STORCHEK, SUMMARY, SYNBUL, SYNHED,
!               TAFBUL, TBUSBUL, TRKBUL, UABUL, WINPRO, TROPBUL
!
! FILES USED  : MHS DATA SETS OPENED AND CLOSED ON UNIT 10.
!               UNIT 2 (NAMELIST INPUT) OPENED, READ AND CLOSED.
!
! REVISION INFO :
!
! $Workfile: mdbstor.f90$ $Folder: OpSource$
! $Revision: 13$ $Date: 03/06/2011 09:24:53$
! $Author: Richard Weedon$
!
! CHANGE RECORD :
!
! $Log:
!  13   MetDB_Refresh 1.12        03/06/2011 09:24:53    Richard Weedon
!       Modification to call to nextmsg.
!  12   MetDB_Refresh 1.11        24/03/2011 16:45:04    Brian Barwell   Check
!       NBUL>0 before 'STORAGE NOT REQUIRED' message.
!  11   MetDB_Refresh 1.10        10/03/2011 12:09:27    Sheila Needham  Put
!       dummy strings in call to DREG
!  10   MetDB_Refresh 1.9         10/03/2011 10:53:23    Sheila Needham
!       Changed MHS open to direct access
!  9    MetDB_Refresh 1.8         01/02/2011 16:36:23    Brian Barwell   FILE
!       and ACTION supplied for OPEN statement for unit 2.
!  8    MetDB_Refresh 1.7         25/01/2011 09:48:35    Stan Kellett    Rework
!        done after rework of MDBSTORBatch10
!  7    MetDB_Refresh 1.6         17/01/2011 11:16:58    Stan Kellett    OPEN
!       statements changed, 2010 copyright changed to 2011.
!       Ready for review
!  6    MetDB_Refresh 1.5         17/01/2011 10:48:36    Stan Kellett    Now
!       compiles
!  5    MetDB_Refresh 1.4         13/01/2011 14:01:42    Stan Kellett    use
!       statements added with interface mod files
!  4    MetDB_Refresh 1.3         31/12/2010 14:03:46    Stan Kellett    enddo
!       changed to end do
!       endif to end if
!       some of old revision info deleted and moved Implicit none
!  3    MetDB_Refresh 1.2         31/12/2010 12:25:28    Stan Kellett    Long
!       do and If Labels added. got rid of some gotos with do while loops,
!       corrected some indenting.
!  2    MetDB_Refresh 1.1         30/12/2010 16:19:41    Stan Kellett
!       comments changed to !
!       HEAD removed
!       changed to free format by removing leading spaces
!       Continuation lines moved to end of lines rather than start
!  1    MetDB_Refresh 1.0         30/12/2010 14:58:49    Stan Kellett
!       initial f77 version of files before porting
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
USE advbul_mod
USE airbul_mod
USE amdar_mod
USE batesbul_mod
USE bufrchek_mod
USE bulhed_mod
USE buoy_mod
USE clmbul_mod
USE datim_mod
USE decord_mod
USE dreg_mod
USE dsinfo_mod
USE ebcdic_mod
USE enhbul_mod
USE ersind_mod
USE gethdr_mod
USE initstor_mod
USE metbul_mod
USE ncmbul_mod
USE nextfile_mod
USE nextmsg_mod
USE prestel_mod
USE satype_mod
USE sambul_mod
USE storchek_mod
USE sferics_mod
USE srwbul_mod
USE stbbul_mod
USE summary_mod
USE synbul_mod
USE synhed_mod
USE tafbul_mod
USE tbusbul_mod
USE trkbul_mod
USE tropbul_mod
USE uabul_mod
USE winpro_mod

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
!                   THE FOLLOWING VARIABLES ARE USED FOR BUFR DATA ONLY

INTEGER, PARAMETER          :: MAXVALS=32000      ! MAX DATA VALUES
INTEGER, PARAMETER          :: MAXDES=999         ! MAX DATA DESCRIPTORS

INTEGER                     :: DESCR(MAXDES)      ! BUFR DESCRIPTORS (FROM BULLETIN)
INTEGER                     :: INLIST=8           ! SIZE OF 'LIST' ARRAY
INTEGER                     :: LIST(8)            ! DESCRIPTORS TO BE FOUND BY 'DECORD'
INTEGER                     :: NDES               ! NO. OF BUFR DESCRIPTORS IN BULLETIN
INTEGER                     :: NOBS               ! NUMBER OF OBSERVATIONS IN BULLETIN
REAL                        :: VALS(MAXVALS)      ! ARRAY OF DECODED DATA VALUES

DATA LIST/1025,1026,1027,1028,1029,1030,1282,1538/


!-----------------------------------------------------------------------
!                    THE FOLLOWING VARIABLES ARE USED BY ALL DATA TYPES
!                                  EXCEPT ERS DATA, AMDARS AND WINDPROF
!-----------------------------------------------------------------------

INTEGER                     :: IERR               ! ERROR CODE FROM "SYNHED"
INTEGER                     :: IPT                ! POINTER TO START BULLETIN
INTEGER                     :: BULEND             ! POINTER TO END OF BULLETIN
INTEGER,PARAMETER           :: LRECL=4096        ! block size of MHS datasets
LOGICAL                     :: OERR               ! ERROR CODE FROM STORAGE ROUTINES
LOGICAL                     :: OAMD               ! AMENDMENT FLAG
LOGICAL                     :: OCOR               ! CORRECTION FLAG
CHARACTER*2                 :: AMDNUM             ! AMENDMENT NUMBER
CHARACTER*2                 :: CORNUM             ! CORRECTION NUMBER
CHARACTER*4                 :: CCCC               ! COLLECTING CENTRE
CHARACTER*6                 :: TTAAII             ! BULLETIN HEADER
CHARACTER*6                 :: YYGGGG             ! DATE/TIME

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
CALL INITSTOR (HEAD1, HEAD2, BULTYP, FLAGS, &
               ITEMS, NBULLS, LENIDX, LSEQ, BULL)

!                                   OPEN HOUSEKEEPING DATA SET (UNIT 1)

unitToSet = 1
CALL DSINFO ('HKEEP', 3, unitToSet, LENREC, KODE, DSN)
!-----------------------------------------------------------------------
!                                                        BUFR DATA ONLY
unitToSet = 0
CALL DSINFO ('CODEFIG', 0, unitToSet, LENREC, KODE, DSN)
unitToSet = 0
CALL DSINFO ('TABLEB',  0, unitToSet, LENREC, KODE, DSN)
unitToSet = 0
CALL DSINFO ('TABLED',  0, unitToSet, LENREC, KODE, DSN)

!                                                  LNDSYN & SHPSYN ONLY

unitToSet = 0
CALL DSINFO ('ELEMIDX', 0, unitToSet, LENREC, KODE, DSN)
!                                                           AIREPS ONLY
unitToSet = 0
CALL DSINFO ('MDBBCN',  0, unitToSet, LENREC, KODE, DSN)
unitToSet = 0
CALL DSINFO ('STNICAO', 0, unitToSet, LENREC, KODE, DSN)

!                          BOGUS, CLIMAT, ESAWS, HEALTHRR, METARS, NCM,
!                         SAMOSX, SREW, TAF, TBUS, TRACKOB & UAIR ONLY

unitToSet = 0
CALL DSINFO ('STNABRV', 0,  unitToSet, LENREC, KODE, DSN)
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

!                                                  STORE METAR BULLETIN
StorageCheck: &
        IF (DATYPE == 'METARS') THEN
          CALL BULHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, BULL(MSTART:MEND+4))
          CALL METBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                    STORE TAF BULLETIN

        ELSE IF (DATYPE == 'TAFS' .OR. DATYPE == 'ATAFS') THEN
          CALL BULHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, BULL(MSTART:MEND+4))
          CALL TAFBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                 STORE SAMOSX BULLETIN
        ELSE IF (DATYPE == 'SAMOSX') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL SAMBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
                 OCOR,CORNUM, IUNIT, OERR, BULL(MSTART:MEND+4))

!                                                    STORE NCM BULLETIN
        ELSE IF (DATYPE == 'NCM') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
               OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL NCMBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,  &
                 OCOR,CORNUM, MIMJ, IUNIT, BULL(MSTART:MEND+4))

!                                    STORE SYNOP BULLETIN (LAND & SHIP)
        ELSE IF (DATYPE == 'LNDSYN' .OR.    &
            DATYPE == 'SHPSYN' .OR. DATYPE == 'MOBSYN') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL SYNBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, OCOR, &
              CORNUM, MIMJ, IUNIT, BULL(MSTART:MEND+4))

!                                                  STORE SATOB BULLETIN
        ELSE IF (DATYPE == 'SATOBS') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL STBBUL (IPT, BULEND, TTAAII, CCCC, OCOR, IUNIT, &
              BULL(MSTART:MEND+4))
!                                                   STORE BUOY BULLETIN
        ELSE IF (DATYPE == 'BUOY') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL  BUOY  (IPT, BULEND, TTAAII,CCCC, &
              OCOR, MIMJ(1:4), IUNIT, BULL(MSTART:MEND+4))

!                                                 STORE CLIMAT BULLETIN
        ELSE IF (DATYPE == 'CLIMAT') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL CLMBUL (IPT, BULEND, TTAAII,CCCC, &
              OCOR, IUNIT, BULL(MSTART:MEND+4))

!                                                   STORE SREW BULLETIN
        ELSE IF (DATYPE == 'SREW') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL SRWBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, &
              OCOR,CORNUM, MIMJ, IUNIT, &
              BULL(MSTART:MEND+4))
!                                                  STORE AIREP BULLETIN
        ELSE IF (DATYPE == 'AIREP') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
             OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL AIRBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG, &
              IUNIT, 81, BULL(MSTART:MEND+4))
!                                             STORE TROPICAL ADVISORY BULLETIN
        ELSE IF (DATYPE == 'TROPADV') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL ADVBUL (BULEND, TTAAII,CCCC,YYGGGG, OCOR, IUNIT, &
          BULL(MSTART:MEND+4))
!                                              STORE UPPER AIR BULLETIN
        ELSE IF (DATYPE == 'UPRAIR') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
            OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL UABUL  (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC, &
              YYGGGG, CORNUM, IUNIT)
!                                                   STORE TBUS BULLETIN
        ELSE IF (DATYPE == 'TBUS') THEN
          CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,&
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL TBUSBUL (IPT, BULEND, TTAAII,YYGGGG, &
              IUNIT, OERR, BULL(MSTART:MEND+4))

!                                          STORE HEALTH RESORT BULLETIN
!                                          (Data stopped in March 2007)
!     ELSE IF (DATYPE == 'HEALTHRR') THEN
!        CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,
!    &                OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
!        IF (IERR == 0)
!    &   CALL HRRBUL (TTAAII, CCCC, IUNIT, OCOR, CORNUM,
!    &                BULL(MSTART:MEND+4))
!                                                  STORE ESAWS BULLETIN
        ELSE IF (DATYPE == 'ESAWS') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,&
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL ENHBUL (IPT, BULEND, TTAAII,CCCC,YYGGGG,&
              OCOR,CORNUM, MIMJ, IUNIT,  &
              BULL(MSTART:MEND+4))
!                                                 STORE SFERIC BULLETIN
        ELSE IF (DATYPE == 'SFERICS') THEN
          CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM,&
             OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL SFERICS (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC, &
              CORNUM, IUNIT)
!                                                STORE PRESTEL BULLETIN
        ELSE IF (DATYPE == 'PRESTEL') THEN
          CALL SYNHED  (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
             OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL PRESTEL (BULL(MSTART:MEND+4), IPT, BULEND, TTAAII, CCCC, &
             YYGGGG, IUNIT)
!                                       STORE BATHY OR TESAC BULLETIN
        ELSE IF (DATYPE == 'SUBSEA') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
              OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL BATESBUL (BULL(MSTART:MEND+4), TTAAII, CCCC, &
             OCOR, MIMJ(1:4), IUNIT)
!                                                STORE TRACKOB BULLETIN
        ELSE IF (DATYPE == 'TRKOB') THEN
          CALL SYNHED (IPT, BULEND, TTAAII,CCCC,YYGGGG, OAMD,AMDNUM, &
             OCOR,CORNUM, MIMJ, IERR, BULL(MSTART:MEND+4))
          IF (IERR == 0) &
          CALL TRKBUL (BULL(MSTART:MEND+4), BULEND, TTAAII, CCCC, &
              YYGGGG, CORNUM, IUNIT)

!                                  STORE TROPICAL CYCLONE BULLETIN
        ELSE IF (DATYPE == 'TROPCYCL') THEN
          CALL SYNHED(IPT,BULEND,TTAAII,CCCC,YYGGGG,OAMD,AMDNUM, &
             OCOR,CORNUM,MIMJ,IERR,BULL(MSTART:MEND+4))
          IF (IERR == 0)  &
          CALL TROPBUL (BULL(MSTART:MEND), TTAAII, CCCC, &
              YYGGGG, IUNIT)
!                                               STORE ERS DATA BULLETIN
        ELSE IF (DATYPE(1:3) == 'ERS') THEN
          NDES = MAXDES
          NOBS = MAXVALS
          CALL DECORD (DESCR, VALS, TEXT40, NDES, NOBS, &
               BULL(MSTART:MEND+4), .FALSE., LIST, INLIST)
          IF (NOBS > 0 .AND. NDES*NOBS <= MAXVALS) THEN
            CALL ERSIND (VALS, NDES, NOBS, DESCR, BULL(MBUFR:M7777), &
              IUNIT)
          ELSE
            WRITE (6,'(T5,A,T15,3A,2I6)') 'MDBSTOR:', &
              'FAILED TO DECODE DATA TYPE ', DATYPE, &
              ' - "NDES" & "NOBS" =', NDES, NOBS
          END IF
!                                                  STORE AMDAR BULLETIN
!                                             (TTAAii starts at byte 15)
        ELSE IF (DATYPE == 'AMDARS') THEN
          CALL AMDAR (BULL(MSTART+14:MEND+4), BUFLAG, IUNIT)

!                                        STORE WIND PROFILER BULLETIN
        ELSE IF (DATYPE == 'WINDPROF') THEN
          CALL WINPRO (BULL(MSTART:MEND+4), IUNIT)
!                                                  SOME OTHER DATA TYPE
        ELSE
          WRITE (6,'(T5,A,T15,2A)') 'MDBSTOR:', &
             'CANNOT PROCESS DATA TYPE ', DATYPE
          NCOL = 3
        END IF StorageCheck
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
END PROGRAM MDBSTOR
