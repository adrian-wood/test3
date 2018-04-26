SUBROUTINE GRIBFIND (DATYPE, IDATA, ITIME, ISELECT, KEYWRD, &
                     LTEST, KODE, IUNIT, LENREC)

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBFIND
!
! PURPOSE      : TO LOCATE THE NEXT GRIB DATA SET TO RETRIEVE AND
!                OPEN IT USING 'GRIBOPEN'.
!
! DESCRIPTION  : 'GRIBFIND' SEARCHES THE INDEX DATA SET FOR THE
!                REQUIRED DATA TYPE AND FINDS THE DATE AND TIME OF
!                THE NEXT AVAILABLE GRIB DATA SET. IF THIS IS WITHIN
!                THE REQUESTED TIME WINDOW, 'GRIBOPEN' IS CALLED TO
!                OPEN IT (RESTORING FROM TAPE IF NECESSARY) AND THE
!                RETURN CODE ('KODE') IS SET TO 4, OTHERWISE 'KODE'
!                IS RETURNED AS 8.'KODE' IS SET TO 16 IN THE EVENT
!                OF A MASS RESTORE FAILURE.
!
!                ALTERNATIVELY, IF 'LATEST' HAS BEEN SPECIFIED BY THE
!                USER, THE NEXT FIELD IS LOCATED BY DOING A BACKWARD
!                SEARCH FROM THE CURRENT TIME.
!
!                IF THE USER CODED 'SELECT' IN THE MDB REQUEST STRING,
!                ONLY THE REQUESTED FIELDS FOR EACH DATA TIME ARE
!                SEARCHED FOR. SEE DOCUMENTATION FOR MORE DETAILS.
!
!                EACH RECORD OF THE INDEX DATA SET CORRESPONDS TO 1
!                YEAR. THERE IS 1 BYTE FOR EACH HOUR + 1 UNUSED BYTE
!                BETWEEN DAYS (=25 BYTES/DAY) MAKING 775 (=31*25)
!                BYTES/MONTH AND 9300 (=12*775) BYTES/YEAR. THE YEAR
!                IS HELD IN BYTES 1-4 AND 00Z JANUARY 1 STARTS IN
!                BYTE 7, SO THE RECORD LENGTH IS 9306.
!
!                'KODE' SHOULD BE SPECIFIED AS <0 IF THIS IS A NEW
!                RETRIEVAL REQUEST, ELSE INPUT ANY POSITIVE VALUE.
!
! USAGE        : CALL GRIBFIND (DATYPE, IDATA, ITIME, ISELECT,
!                       KEYWRD, LTEST, KODE, IUNIT, LENREC)
!
! PARAMETERS   : DATYPE   I   (CHARACTER*(*)) MET.D.B. DATA SUBTYPE.
!                IDATA    I   5-ELEMENT INTEGER ARRAY WITH INDEX DATA
!                             SET DETAILS AS OUTPUT BY 'DDICT':
!                               1  LENGTH OF DATA SET NAME,
!                               2  DATA SET RECORD LENGTH,
!                               3  REQUIRED FT NUMBER,
!                              4&5 (NOT USED FOR GRIB DATA).
!                ITIME    I   (8-ELEMENT INTEGER ARRAY) USER'S START
!                             AND END TIMES AS OUTPUT BY 'DDICT'.
!                             (EACH TIME IS YYYY, MM, DD, HH.)
!                ISELECT  I   (50-ELEMENT INTEGER ARRAY) USER'S
!                             DATA SELECTION PARAMETER LIST.
!                KEYWRD   I   MDB KEYWORD ARRAY (ELEM. 3 ='LATEST')
!                LTEST    I   SET .TRUE. FOR EXTRA PRINT STATEMENTS.
!                KODE    I/O  RETURN CODE. OUTPUT VALUES ARE:
!                               4  NEXT DATA SET FOUND AND OPENED,
!                               8  NO MORE DATA IN TIME WINDOW.
!                              16  MASS RESTORE FAILURE.
!                             A NEGATIVE INPUT VALUE IS USED TO
!                             INDICATE A NEW RETRIEVAL REQUEST.
!                IUNIT    O   UNIT NUMBER OF GRIB DATA SET OPENED.
!                LENREC   O   RECORD LENGTH OF GRIB DATA SET OPENED.
!
! CALLED BY    : GRIBRET
!
! CALLS        : GRIBOPEN
!
! REVISION INFO :
!
! $Revision: 9$
! $Date: 21/03/2012 11:16:38$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribfind.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         21/03/2012 11:16:38    Sheila Needham  Return
!        ISTAT=16 for MASS restore failures
!  8    MetDB_Refresh 1.7         20/12/2010 12:45:54    Sheila Needham
!       Initialise IUNIT and LENREC
!  7    MetDB_Refresh 1.6         29/11/2010 08:44:43    Sheila Needham  SAVE
!       all variables
!  6    MetDB_Refresh 1.5         19/11/2010 10:13:52    Richard Weedon  USE
!       statement for datim added
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:16:36    John Norton     
!  1    MetDB_Refresh 1.0         13/10/2010 16:00:38    John Norton     f77
!       code copied out of StarTeam
!       MIDE\Met_DB_Project\Development\Source\OpSource
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE gribopen_mod
USE datim_mod
!
! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(*), INTENT(IN)        ::  DATYPE ! MET.D.B. GRIB DATA TYPE
INTEGER, INTENT(IN)             ::  IDATA(5) ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
INTEGER, INTENT(IN)             ::  ITIME(8) ! START AND END TIMES OF REQUEST PERIOD
INTEGER, INTENT(IN)             ::  ISELECT(50) ! ARRAY OF DATA SELECTION PARAMETERS
LOGICAL, INTENT(IN)             ::  KEYWRD(:) ! MDB KEYWORDS (= ARRAY 'FOUND' IN 'MDB')
LOGICAL, INTENT(IN)             ::  LTEST ! .TRUE. FOR EXTRA PRINTOUT (PASSED TO GRIBOPEN)
INTEGER, INTENT(INOUT)          ::  KODE ! STATUS CODE RETURNED BY THIS ROUTINE
INTEGER, INTENT(OUT)            ::  IUNIT ! UNIT NUMBER FOR GRIB DATA SET (FROM GRIBOPEN)
INTEGER, INTENT(OUT)            ::  LENREC ! RECORD LENGTH OF GRIB DATA SET

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!                                                             VARIABLES
INTEGER      ::  I ! GENERAL VARIABLE FOR LOCAL USE
INTEGER      ::  IBYTE1 ! LOCATION OF START TIME IN INDEX RECORD
INTEGER      ::  IBYTE2 ! LOCATION OF END TIME IN INDEX RECORD
INTEGER      ::  IEXTRA ! OFFSET BETWEEN YEAR AND BLOCK NUMBER
INTEGER      ::  J ! GENERAL VARIABLE FOR LOCAL USE
INTEGER      ::  JBYTE ! POINTER TO A BYTE IN 'RECORD'
INTEGER      ::  JLAST ! LAST BYTE TO LOOK AT FOR 'LATEST' FIELD
INTEGER      ::  LASTYR ! END YEAR OF SEARCH WINDOW
INTEGER      ::  MSGLOC ! LOCATION OF MESSAGE NUMBER IN DSN
INTEGER      ::  NBLOKS ! NUMBER OF BLOCKS IN INDEX DATA SET
INTEGER      ::  NBYTE ! BYTE NO. IN INDEX RECORD FOR CURRENT DATA SET
INTEGER      ::  NDXREC = 9306 ! RECORD LENGTH OF INDEX DATA SET
INTEGER      ::  NHYEAR ! YEAR FROM CURRENT RECORD OF INDEX DATA SET
INTEGER      ::  NMSG = 1 ! MESSAGE NUMBER OF CURRENT MESSAGE
INTEGER      ::  NMSGS ! NO. OF GRIB MESSAGES FOR EACH DATA TIME
INTEGER      ::  NOW(8) ! CURRENT DATE AND TIME (FROM 'DATIM')
INTEGER      ::  NREC ! INDEX RECORD NUMBER FOR CURRENT DATA SET
INTEGER      ::  NSELECT = 0 ! CURRENT POSITION IN ISELECT ARRAY
INTEGER      ::  NSTORE ! STORAGE PERIOD IN YRS (FROM INDEX HEADER)
INTEGER      ::  NYEAR ! REQUIRED YEAR FOR DATA SEARCH
INTEGER      ::  NYEARS ! DATA RETENTION PERIOD (YEARS)
INTEGER      ::  NYMDH(4) ! DATE/TIME OF GRIB DATA SET (YY, MM, DD, HH)
!
LOGICAL      ::  FOUND ! .TRUE. IF GRIB FIELD FOUND
LOGICAL      ::  LOOKING ! .TRUE. WHILE SEARCHING FOR NEXT GRIB DATA SET
!
CHARACTER(8)    :: OLDTYPE = ' ' ! DATA TYPE FOR PREVIOUS CALL
CHARACTER(9306) :: RECORD ! RECORD READ FROM GRIB DATA SET
CHARACTER(44)   :: SKEL ! SKELETON DATA SET NAME FOR GRIB DATA SETS
!

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!                                                   DATA INITIALISATION
!
!                                                                 SAVES
SAVE
IUNIT = 0
LENREC = 0
!
IF (DATYPE /= OLDTYPE) THEN
  READ (IDATA(3),REC=1) NBLOKS, NYEARS, NSTORE, I, J, SKEL
  IF (LTEST) THEN
    PRINT*,'GRIBFIND: RECORD 1,NBLOKS,NYEARS,NSTORE,I,J,SKEL', &
            NBLOKS,NYEARS,NSTORE,I,J,SKEL
  END IF
  IEXTRA = NBLOKS - NYEARS + 1
  NMSGS = MOD(I,65536)      ! Messages for each data time
                            !  (held in first 2 bytes of I)
  MSGLOC = INDEX(SKEL,'##') ! Location of msg no. in dsn
  OLDTYPE = DATYPE
END IF
!
!=======================================================================
!   IF THIS IS A NEW RETRIEVAL REQUEST, READ THE FIRST RECORD OF THE
!   INDEX DATA SET OCCURRING WITHIN THE REQUIRED TIME WINDOW.
!=======================================================================
!
IFLABEL1: &
IF (KODE < 0) THEN
!                                  CHECK FOR USER'S DATA SELECTION LIST
!
  IF (KEYWRD(35)) THEN    ! LIST PRESENT
    NSELECT = 1
    NMSG = ISELECT(1)
  ELSE                    ! LIST NOT PRESENT
    NSELECT = 0
    NMSG = 1
  END IF
!                                IF KEYWRD(3) IS .FALSE. (NOT 'LATEST'),
!                                 LOOK FOR FIELDS IN USER'S TIME WINDOW
IFLABEL2: &
  IF (.NOT.KEYWRD(3)) THEN
!
!              GET BYTE NUMBERS FOR START & END TIMES IN REQUEST STRING
!            (25 BYTES/DAY, 775 BYTES/MONTH - SEE DESCRIPTION AT START)
!
    IBYTE1 = 775*(ITIME(2)-1) + 25*(ITIME(3)-1) + ITIME(4) + 7
    IBYTE2 = 775*(ITIME(6)-1) + 25*(ITIME(7)-1) + ITIME(8) + 7
!
!                                                       INITIALISATIONS
    NYEAR = ITIME(1)  ! START YEAR
    LASTYR = ITIME(5) ! END YEAR
    NBYTE = IBYTE1    ! BYTE FOR START DATE/HOUR
    IF (LTEST) THEN
      PRINT*,'GRIBFIND: NYEAR,IEXTRA',NYEAR,IEXTRA
    END IF
!
!-----------------------------------------------------------------------
!   FIND INDEX RECORD CORRESPONDING TO START YEAR
!-----------------------------------------------------------------------
!
!                                              FIND INDEX RECORD NUMBER
    NREC = IEXTRA + MOD(NYEAR,NYEARS)
!                                                 READ THE INDEX RECORD

    READ (IDATA(3),REC=NREC) RECORD
!                                                  GET YEAR FROM HEADER
    READ (RECORD(1:4),'(I4)') NHYEAR
!                                              IF WRONG YEAR, SKIP DATA
    IF (NHYEAR /= NYEAR) NBYTE = NDXREC
!
  ELSE
!=======================================================================
!   IF THE LATEST FIELD IS REQUIRED (KEYWRD(3)=.TRUE.) WORK BACKWARDS
!   FROM CURRENT TIME AND STOP AT THE FIRST FIELD FOUND.
!=======================================================================
!
    IBYTE1 = NDXREC
    IBYTE2 = NDXREC
!                             CURRENT TIME & BYTE IN INDEX DATA SET
    CALL DATIM (NOW)
    NBYTE = 775*(NOW(7)-1) + 25*(NOW(6)-1) + NOW(5) + 7
    J = 0  ! Year counter
!                                   BACKWARD LOOP OVER STORED YEARS
    FOUND = .FALSE.
DOLABEL1: &
    DO WHILE (.NOT.FOUND .AND. J <= NSTORE)
      NYEAR = NOW(8) - J
!                                                 FIND INDEX RECORD
      NREC = IEXTRA + MOD(NYEAR,NYEARS)
!                                             READ THE INDEX RECORD

      READ (IDATA(3),REC=NREC) RECORD
!                                              GET YEAR FROM HEADER
      READ (RECORD(1:4),'(I4)') NHYEAR
!                                            CHECK FOR CORRECT YEAR
IFLABEL3: &
      IF (NHYEAR == NYEAR) THEN
!                                     GET RANGE OF BYTES TO LOOK AT
!
        JBYTE = 9305  ! Byte for 23Z 31 December
        JLAST = 7     ! Byte for 00Z 01 January
        IF (J == 0)      JBYTE = NBYTE      ! This year
        IF (J == NSTORE) JLAST = NBYTE + 1  ! Oldest year
!
!                          BACKWARD LOOP OVER BYTES UNTIL 'X' FOUND
!
        DO WHILE (.NOT.FOUND .AND. JBYTE >= JLAST)
          IF (RECORD(JBYTE:JBYTE) == 'X') THEN
            IBYTE1 = JBYTE
            IBYTE2 = JBYTE
            FOUND = .TRUE.
          ELSE
            JBYTE = JBYTE - 1
          END IF
        END DO
      END IF IFLABEL3
!                              INCREMENT YEAR COUNTER FOR NEXT TIME
      J = J + 1
    END DO DOLABEL1
!
    NBYTE = IBYTE1    ! Byte for start date/hour
    LASTYR = NYEAR    ! Last year to look at
  END IF IFLABEL2
END IF IFLABEL1
!
!=======================================================================
!   SEARCH INDEX DATA SET FOR NEXT FLAG INDICATING THE PRESENCE OF A
!   GRIB DATA SET AND CHECK THAT IT IS WITHIN THE TIME WINDOW.
!=======================================================================
!
LOOKING = .TRUE.
DOLABEL2: &
DO WHILE (LOOKING)
!                                       LOOK FOR FLAG FOR NEXT DATA SET
  J = INDEX(RECORD(NBYTE:),'X')
!
!-----------------------------------------------------------------------
!   NO FLAG FOUND IN RECORD FOR CURRENT YEAR
!-----------------------------------------------------------------------
!
IFLABEL4: &
  IF (J == 0) THEN
!                                         STOP IF END OF REQUEST PERIOD
IFLABEL5: &
    IF (NYEAR == LASTYR) THEN
      KODE = 8
      LOOKING = .FALSE.
!                                                ELSE TRY THE NEXT YEAR
    ELSE
      NYEAR = NYEAR + 1
      NREC = IEXTRA + MOD(NYEAR,NYEARS)
      READ (IDATA(3),REC=NREC) RECORD
      READ (RECORD(1:4),'(I4)') NHYEAR
!                                               CHECK FOR MATCHING YEAR
      IF (NHYEAR == NYEAR) THEN ! OK
        NBYTE = 7       ! (00Z JANUARY 1)
      ELSE ! WRONG YEAR
        NBYTE = NDXREC  ! (LAST BYTE IN RECORD - ALWAYS ' ')
      END IF
    END IF IFLABEL5
!
!-----------------------------------------------------------------------
!   NEXT GRIB DATA SET FLAG FOUND
!-----------------------------------------------------------------------
!
  ELSE
    NBYTE = NBYTE + J - 1
!                                     STOP IF PAST END OF SEARCH WINDOW
!
IFLABEL6: &
    IF (NYEAR == LASTYR .AND. NBYTE > IBYTE2) THEN
      KODE = 8
      LOOKING = .FALSE.
!                                        CHECK FOR VALID MESSAGE NUMBER
     ELSE IF (NMSG <= NMSGS) THEN
      NYMDH(1) = MOD(NYEAR,100) ! 2-DIGIT YEAR
!
!                CONVERT BYTE NUMBER TO MONTH, DAY, HOUR (25 BYTES/DAY,
!                           775 BYTES/MONTH - SEE DESCRIPTION AT START)
      J = NBYTE - 7
      NYMDH(2) = J/775 + 1
      J = MOD(J,775)
      NYMDH(3) = J/25 + 1
      NYMDH(4) = MOD(J,25)
!                                  PUT MESSAGE NUMBER IN SKEL IF NEEDED
      IF (MSGLOC > 0) &
          WRITE (SKEL(MSGLOC:MSGLOC+1),'(I2.2)') NMSG
!
!                                         LOCATE AND OPEN GRIB DATA SET
!

      CALL GRIBOPEN (NYMDH, SKEL, LTEST, IUNIT, LENREC, KODE)
!
!                                   ERROR EXIT FOR MASS RESTORE FAILURE
      IF (KODE == 16) RETURN
!                                    STOP SEARCH IF OPENED SUCCESSFULLY
!
      IF (KODE == 4) LOOKING = .FALSE.
    END IF IFLABEL6
!                               RESET POINTERS FOR NEXT TIME ROUND LOOP
!                                           (1) SELECTION LIST SUPPLIED
IFLABEL7: &
    IF (NSELECT > 0) THEN
      NSELECT = NSELECT + 1  ! Next location in list
!
      IF (NSELECT > 50 .OR. &          ! If end of array or
          ISELECT(NSELECT) < 0) THEN   ! no more data values,
        NSELECT = 1         ! return to start of list for ...
        NBYTE = NBYTE + 1   ! ... next data time
      END IF
      NMSG = ISELECT(NSELECT)  ! Next message number
    ELSE
!                                          (2) ALL GRIB FIELDS REQUIRED
!
      NMSG = NMSG + 1          ! Next message number
      IF (NMSG > NMSGS) THEN   ! If no more for this time,
        NMSG = 1               ! get first message for ...
        NBYTE = NBYTE + 1      ! ... next data time
      END IF
    END IF IFLABEL7
  END IF IFLABEL4
END DO DOLABEL2
!                                             RETURN TO CALLING PROGRAM
RETURN
END SUBROUTINE GRIBFIND
