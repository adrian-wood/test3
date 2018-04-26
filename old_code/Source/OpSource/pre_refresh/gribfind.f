      SUBROUTINE GRIBFIND (DATYPE, IDATA, ITIME, ISELECT, KEYWRD,   !2.3
     &                     LTEST, KODE, IUNIT, LENREC)              !2.3

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
!                IS RETURNED AS 8.
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
! USAGE        : CALL GRIBFIND (DATYPE, IDATA, ITIME, ISELECT,      !2.3
!                       KEYWRD, LTEST, KODE, IUNIT, LENREC)         !2.3
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
!                ISELECT  I   (50-ELEMENT INTEGER ARRAY) USER'S     !3
!                             DATA SELECTION PARAMETER LIST.        !2.3
!                KEYWRD   I   MDB KEYWORD ARRAY (ELEM. 3 ='LATEST') !2.1
!                LTEST    I   SET .TRUE. FOR EXTRA PRINT STATEMENTS.
!                KODE    I/O  RETURN CODE. OUTPUT VALUES ARE:
!                               4  NEXT DATA SET FOUND AND OPENED,
!                               8  NO MORE DATA IN TIME WINDOW.
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
! $Revision: 6$
! $Date: 07/04/2009 16:25:00$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribfind.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  6    Met_DB_Project 1.5         07/04/2009 16:25:00    Richard Weedon  Line
!       346 ST comment added
!  5    Met_DB_Project 1.4         07/04/2009 16:23:04    Richard Weedon
!       Select array parameter corrected line 346
!  4    Met_DB_Project 1.3         03/04/2009 11:58:54    Richard Weedon
!       Revisioned under CHG9502
!  3    Met_DB_Project 1.2         03/04/2009 10:24:53    Richard Weedon
!       Revisioned under CR9502
!  2    Met_DB_Project 1.1         02/04/2009 11:19:40    Richard Weedon
!       Select parameter increased to 50 CHG 7636
!  1    Met_DB_Project 1.0         30/01/2006 20:22:41    Sheila Needham  
! $
! Revision 2.3  2003/03/05  16:23:32  16:23:32  usmdb (MetDB account c/o usjh)
! Changed KEYWRD(34) to KEYWRD(*) - S.Cox
! ISELECT added to argument list and used in searching for data
! if there is more than one field for each data time - B Barwell
!
! Revision 2.2  2002/06/10  15:11:32  15:11:32  usmdb (MetDB account c/o usjh)
! 2.2.  17 June 2002.  Brian Barwell.  Change 45/02.
! Works for data with more than one GRIB message for the same time.
!
! Revision 2.1  2001/08/09  09:25:57  09:25:57  usmdb (Generic MetDB account)
! 2.1.  20 August 2001.  Brian Barwell.  Change 105/01.
! Add KEYWRD to argument and new section to find 'LATEST' field.
!
! Revision 2.0  2001/01/08  11:58:43  11:58:43  usmdb (Generic MetDB account)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/12/08  14:16:11  14:16:11  usmdb (Generic MDB account)
! Initial revision
!
! ORIGINAL VERSION BY BRIAN BARWELL, AUGUST 2000.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2002 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE
!                                                             VARIABLES
      INTEGER I         ! GENERAL VARIABLE FOR LOCAL USE            !2.2
      INTEGER IBYTE1    ! LOCATION OF START TIME IN INDEX RECORD
      INTEGER IBYTE2    ! LOCATION OF END TIME IN INDEX RECORD
      INTEGER IDATA(5)  ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
      INTEGER IEXTRA    ! OFFSET BETWEEN YEAR AND BLOCK NUMBER
      INTEGER ISELECT(50) ! ARRAY OF DATA SELECTION PARAMETERS      !3
      INTEGER ITIME(8)  ! START AND END TIMES OF REQUEST PERIOD
      INTEGER IUNIT     ! UNIT NUMBER FOR GRIB DATA SET (FROM GRIBOPEN)
      INTEGER J         ! GENERAL VARIABLE FOR LOCAL USE
      INTEGER JBYTE     ! POINTER TO A BYTE IN 'RECORD'             !2.1
      INTEGER JLAST     ! LAST BYTE TO LOOK AT FOR 'LATEST' FIELD   !2.1
      INTEGER KODE      ! STATUS CODE RETURNED BY THIS ROUTINE
      INTEGER LASTYR    ! END YEAR OF SEARCH WINDOW                 !2.1
      INTEGER LENREC    ! RECORD LENGTH OF GRIB DATA SET
      INTEGER MSGLOC    ! LOCATION OF MESSAGE NUMBER IN DSN         !2.2
      INTEGER NBLOKS    ! NUMBER OF BLOCKS IN INDEX DATA SET
      INTEGER NBYTE     ! BYTE NO. IN INDEX RECORD FOR CURRENT DATA SET
      INTEGER NDXREC    ! RECORD LENGTH OF INDEX DATA SET
      INTEGER NHYEAR    ! YEAR FROM CURRENT RECORD OF INDEX DATA SET
      INTEGER NMSG      ! MESSAGE NUMBER OF CURRENT MESSAGE         !2.2
      INTEGER NMSGS     ! NO. OF GRIB MESSAGES FOR EACH DATA TIME   !2.2
      INTEGER NOW(8)    ! CURRENT DATE AND TIME (FROM 'DATIM')      !2.1
      INTEGER NREC      ! INDEX RECORD NUMBER FOR CURRENT DATA SET
      INTEGER NSELECT   ! CURRENT POSITION IN ISELECT ARRAY         !2.3
      INTEGER NSTORE    ! STORAGE PERIOD IN YRS (FROM INDEX HEADER) !2.1
      INTEGER NYEAR     ! REQUIRED YEAR FOR DATA SEARCH
      INTEGER NYEARS    ! DATA RETENTION PERIOD (YEARS)
      INTEGER NYMDH(4)  ! DATE/TIME OF GRIB DATA SET (YY, MM, DD, HH)
!
      LOGICAL FIRST     ! .TRUE. IF FIRST CALL TO SUBROUTINE
      LOGICAL FOUND     ! .TRUE. IF GRIB FIELD FOUND                !2.1
      LOGICAL KEYWRD(*) ! MDB KEYWORDS (= ARRAY 'FOUND' IN 'MDB')   !2.3
      LOGICAL LOOKING   ! .TRUE. WHILE SEARCHING FOR NEXT GRIB DATA SET
      LOGICAL LTEST     ! .TRUE. FOR EXTRA PRINTOUT (PASSED TO GRIBOPEN)
!
      CHARACTER*(*)  DATYPE  ! MET.D.B. GRIB DATA TYPE
      CHARACTER*132  HEAD    ! FOR REVISION INFORMATION
      CHARACTER*8    OLDTYPE ! DATA TYPE FOR PREVIOUS CALL
      CHARACTER*9306 RECORD  ! RECORD READ FROM GRIB DATA SET
      CHARACTER*44   SKEL    ! SKELETON DATA SET NAME FOR GRIB DATA SETS
!
!                                                   DATA INITIALISATION
!
      DATA OLDTYPE/' '/, FIRST/.TRUE./, NDXREC/9306/                !2.3
      DATA NMSG/1/, NSELECT/0/                                      !2.3
!                                                                 SAVES
      SAVE OLDTYPE, NBLOKS, NYEARS, NMSGS, SKEL, IEXTRA, MSGLOC     !2.2
      SAVE NMSG, NSELECT, NBYTE, NREC, RECORD, FIRST                !2.3
!
!                                REVISION INFORMATION (FIRST CALL ONLY)
      IF (FIRST) THEN
        HEAD='
     &  $Source: /data/us0400/mdb/op/lib/source/RCS/gribfind.f,v $
     &  '//'$Date: 07/04/2009 16:25:00$ $Revision: 6$'
        FIRST = .FALSE.
      END IF
!                               READ INDEX DATA HEADER IF NEW DATA TYPE
      IF (DATYPE.NE.OLDTYPE) THEN
        READ (IDATA(3),REC=1) NBLOKS, NYEARS, NSTORE, I, J, SKEL    !2.2
        IEXTRA = NBLOKS - NYEARS + 1
        NMSGS = MOD(I,65536)      ! Messages for each data time     !2.2
                                  !  (held in first 2 bytes of I)   !2.2
        MSGLOC = INDEX(SKEL,'##') ! Location of msg no. in dsn      !2.2
        OLDTYPE = DATYPE
      END IF
!
!=======================================================================
!   IF THIS IS A NEW RETRIEVAL REQUEST, READ THE FIRST RECORD OF THE
!   INDEX DATA SET OCCURRING WITHIN THE REQUIRED TIME WINDOW.
!=======================================================================
!
      IF (KODE.LT.0) THEN
!                                  CHECK FOR USER'S DATA SELECTION LIST
!
        IF (KEYWRD(35)) THEN    ! LIST PRESENT                      !2.3
          NSELECT = 1                                               !2.3
          NMSG = ISELECT(1)                                         !2.3
        ELSE                    ! LIST NOT PRESENT                  !2.3
          NSELECT = 0                                               !2.3
          NMSG = 1                                                  !2.3
        END IF                                                      !2.3
!                                IF KEYWRD(3) IS .FALSE. (NOT 'LATEST'),
!                                 LOOK FOR FIELDS IN USER'S TIME WINDOW
        IF (.NOT.KEYWRD(3)) THEN                                    !2.1
!
!              GET BYTE NUMBERS FOR START & END TIMES IN REQUEST STRING
!            (25 BYTES/DAY, 775 BYTES/MONTH - SEE DESCRIPTION AT START)
!
          IBYTE1 = 775*(ITIME(2)-1) + 25*(ITIME(3)-1) + ITIME(4) + 7
          IBYTE2 = 775*(ITIME(6)-1) + 25*(ITIME(7)-1) + ITIME(8) + 7
!
!                                                       INITIALISATIONS
          NYEAR = ITIME(1)  ! START YEAR
          LASTYR = ITIME(5) ! END YEAR                              !2.1
          NBYTE = IBYTE1    ! BYTE FOR START DATE/HOUR
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
          IF (NHYEAR.NE.NYEAR) NBYTE = NDXREC
!
        ELSE
!=======================================================================
!   IF THE LATEST FIELD IS REQUIRED (KEYWRD(3)=.TRUE.) WORK BACKWARDS
!   FROM CURRENT TIME AND STOP AT THE FIRST FIELD FOUND.
!=======================================================================
!
          IBYTE1 = NDXREC                                           !2.1
          IBYTE2 = NDXREC                                           !2.1
!                             CURRENT TIME & BYTE IN INDEX DATA SET !2.1
          CALL DATIM (NOW)                                          !2.1
          NBYTE = 775*(NOW(7)-1) + 25*(NOW(6)-1) + NOW(5) + 7       !2.1
          J = 0  ! Year counter                                     !2.1
!                                   BACKWARD LOOP OVER STORED YEARS !2.1
          FOUND = .FALSE.                                           !2.1
          DO WHILE (.NOT.FOUND .AND. J.LE.NSTORE)                   !2.1
            NYEAR = NOW(8) - J                                      !2.1
!                                                 FIND INDEX RECORD !2.1
            NREC = IEXTRA + MOD(NYEAR,NYEARS)                       !2.1
!                                             READ THE INDEX RECORD !2.1
            READ (IDATA(3),REC=NREC) RECORD                         !2.1
!                                              GET YEAR FROM HEADER !2.1
            READ (RECORD(1:4),'(I4)') NHYEAR                        !2.1
!                                            CHECK FOR CORRECT YEAR !2.1
            IF (NHYEAR.EQ.NYEAR) THEN                               !2.1
!                                     GET RANGE OF BYTES TO LOOK AT !2.1
!                                                                   !2.1
              JBYTE = 9305  ! Byte for 23Z 31 December              !2.1
              JLAST = 7     ! Byte for 00Z 01 January               !2.1
              IF (J.EQ.0)      JBYTE = NBYTE      ! This year       !2.1
              IF (J.EQ.NSTORE) JLAST = NBYTE + 1  ! Oldest year     !2.1
!                                                                   !2.1
!                          BACKWARD LOOP OVER BYTES UNTIL 'X' FOUND !2.1
!                                                                   !2.1
              DO WHILE (.NOT.FOUND .AND. JBYTE.GE.JLAST)            !2.1
                IF (RECORD(JBYTE:JBYTE).EQ.'X') THEN                !2.1
                  IBYTE1 = JBYTE                                    !2.1
                  IBYTE2 = JBYTE                                    !2.1
                  FOUND = .TRUE.                                    !2.1
                ELSE                                                !2.1
                  JBYTE = JBYTE - 1                                 !2.1
                END IF                                              !2.1
              END DO                                                !2.1
            END IF                                                  !2.1
!                              INCREMENT YEAR COUNTER FOR NEXT TIME !2.1
            J = J + 1                                               !2.1
          END DO                                                    !2.1
!                                                                   !2.1
          NBYTE = IBYTE1    ! Byte for start date/hour              !2.1
          LASTYR = NYEAR    ! Last year to look at                  !2.1
        END IF                                                      !2.1
      END IF
!
!=======================================================================
!   SEARCH INDEX DATA SET FOR NEXT FLAG INDICATING THE PRESENCE OF A
!   GRIB DATA SET AND CHECK THAT IT IS WITHIN THE TIME WINDOW.
!=======================================================================
!
      LOOKING = .TRUE.
      DO WHILE (LOOKING)
!                                       LOOK FOR FLAG FOR NEXT DATA SET
        J = INDEX(RECORD(NBYTE:),'X')
!
!-----------------------------------------------------------------------
!   NO FLAG FOUND IN RECORD FOR CURRENT YEAR
!-----------------------------------------------------------------------
!
        IF (J.EQ.0) THEN
!                                         STOP IF END OF REQUEST PERIOD
          IF (NYEAR.EQ.LASTYR) THEN                                 !2.1
            KODE = 8
            LOOKING = .FALSE.
!                                                ELSE TRY THE NEXT YEAR
          ELSE
            NYEAR = NYEAR + 1
            NREC = IEXTRA + MOD(NYEAR,NYEARS)
            READ (IDATA(3),REC=NREC) RECORD
            READ (RECORD(1:4),'(I4)') NHYEAR
!                                               CHECK FOR MATCHING YEAR
            IF (NHYEAR.EQ.NYEAR) THEN ! OK
              NBYTE = 7       ! (00Z JANUARY 1)
            ELSE ! WRONG YEAR
              NBYTE = NDXREC  ! (LAST BYTE IN RECORD - ALWAYS ' ')
            END IF
          END IF
!
!-----------------------------------------------------------------------
!   NEXT GRIB DATA SET FLAG FOUND
!-----------------------------------------------------------------------
!
        ELSE
          NBYTE = NBYTE + J - 1
!                                     STOP IF PAST END OF SEARCH WINDOW
!
          IF (NYEAR.EQ.LASTYR .AND. NBYTE.GT.IBYTE2) THEN           !2.1
            KODE = 8
            LOOKING = .FALSE.
!                                        CHECK FOR VALID MESSAGE NUMBER
           ELSE IF (NMSG.LE.NMSGS) THEN                             !2.3
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
            IF (MSGLOC.GT.0)                                        !2.2
     &          WRITE (SKEL(MSGLOC:MSGLOC+1),'(I2.2)') NMSG         !2.2
!
!                                         LOCATE AND OPEN GRIB DATA SET
!
            CALL GRIBOPEN (NYMDH, SKEL, LTEST, IUNIT, LENREC, KODE)
!
!                                    STOP SEARCH IF OPENED SUCCESSFULLY
!
            IF (KODE.EQ.4) LOOKING = .FALSE.
          END IF
!                               RESET POINTERS FOR NEXT TIME ROUND LOOP
!                                           (1) SELECTION LIST SUPPLIED
          IF (NSELECT.GT.0) THEN                                    !2.3
            NSELECT = NSELECT + 1  ! Next location in list          !2.3
!
            IF (NSELECT.GT.50 .OR.           ! If end of array or   !ST 6
     &          ISELECT(NSELECT).LT.0) THEN  ! no more data values, !2.3
              NSELECT = 1         ! return to start of list for ... !2.3
              NBYTE = NBYTE + 1   ! ... next data time              !2.3
            END IF                                                  !2.3
            NMSG = ISELECT(NSELECT)  ! Next message number          !2.3
          ELSE                                                      !2.3
!                                          (2) ALL GRIB FIELDS REQUIRED
!
            NMSG = NMSG + 1          ! Next message number          !2.3
            IF (NMSG.GT.NMSGS) THEN  ! If no more for this time,    !2.3
              NMSG = 1               ! get first message for ...    !2.2
              NBYTE = NBYTE + 1      ! ... next data time
            END IF                                                  !2.2
          END IF                                                    !2.3
        END IF
      END DO
!                                             RETURN TO CALLING PROGRAM
      RETURN
      END
