      PROGRAM GRIBDAT

      IMPLICIT NONE

!-----------------------------------------------------------------------
!
! PROGRAM     : GRIBDAT
!
! PURPOSE     : MAIN PROGRAM FOR COPYING AND RENAMING GRIB FILES.
!
! DESCRIPTION : "GRIBDAT" LOOKS FOR MHS DATA SETS CONTAINING GRIB
!               DATA AND COPIES EACH ONE TO A NEW DISK DATA SET WITH
!               A SUITABLE NAME. THE EXPECTED DCB FOR BOTH DATA SETS
!               IS
!                     DCB=(RECFM=FB,LRECL=9216,BLKSIZE=27648)
!
!               AND THE DATA SET NAME IS OF THE FORM
!
!                           MDB.DAdatype.Dyymmdd.Thh##              !2.1
!
!               WHERE THE YEAR, MONTH, DAY AND HOUR ARE TAKEN FROM
!               SECTION 1 OF THE GRIB MESSAGE AND '##' (WHICH MAY   !2.1
!               NOT BE PRESENT) IS A MESSAGE NUMBER EXTRACTED FROM  !2.1
!               THE LAST TWO DIGITS OF THE MHS DATA SET NAME.       !2.1
!
!               THE JOB SHOULD BE RUN WITH A STORAGE MONITOR JOB AND
!               CAN BE RUN AS A BATCH JOB OR SET TO RUN CONTINUOUSLY.
!               A HOUSEKEPING DATA SET IS NEEDED WITH DDNAME 'HKEEP'
!               AND THE APPROPRIATE INDEX DATA SET WITH A DDNAME OF
!               'INDEX'. THESE ARE OPENED ON UNITS 1 AND 2.
!
!               EACH RECORD OF THE INDEX DATA SET CORRESPONDS TO 1
!               YEAR. THERE IS 1 BYTE FOR EACH HOUR + 1 UNUSED BYTE
!               BETWEEN DAYS (=25 BYTES/DAY) MAKING 775 (=31*25)
!               BYTES/MONTH AND 9300 (=12*775) BYTES/YEAR. THE YEAR
!               IS HELD IN BYTES 1-4 AND 00Z JANUARY 1 STARTS IN
!               BYTE 7, SO THE RECORD LENGTH IS 9306.
!
! NAMELIST    : INSTOR  (UNIT 5).  CONTENTS AS FOLLOWS:
!
!               VARIABLE TYPE        DESCRIPTION             DEFAULT
!               -------- ----        -----------             -------
!               PRINT    L*4  FLAG TO PRODUCE 1-LINE MESSAGE  .TRUE.
!                             FOR EACH DATA SET PROCESSED.
!               NSECS    I*4  NO. OF SECS. TO WAIT BETWEEN      30
!                             ACCESSES OF H.K. DATA SET.
!               NWAITS   I*4  NO. OF MULTIPLES OF NSECS USED    10
!                             WHEN WAITING FOR DATA SETS (SET
!                             NWAITS=0 TO STOP IF NO DATA LEFT)
!               JOBNAME  C*8  JOB NAME.                     'GRIBJOB'
!               OWNER    C*8  MHS DATA SET OWNER.             'GRB1'
!               DISK     C*6  DISK FOR OUTPUT FILE           'OPR103'
!
! FILES USED  : MET.D.B. HOUSEKEEPING DATA SET OPENED ON UNIT 1.
!               INDEX DATA SET FOR GRIB DATA SETS OPENED ON UNIT 2.
!               NAMELIST INPUT READ FROM UNIT 5.
!               INPUT GRIB DATA SETS OPENED ON UNIT 10.
!               OUTPUT GRIB DATA SETS CREATED ON UNIT 20.
!
! CALLS       : CENTURY, DATIM, FILEINF, ICHAR2, ICHAR3, MHSIFF,    !2.1
!               NEXTFILE, SECSOUT, SYSABN.                          !2.1
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:40$
! $Source: /data/us0400/mdb/op/lib/source/RCS/gribdat.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:40    Sheila Needham  
! $
! Revision 2.1  2002/06/10  15:11:14  15:11:14  usmdb (Generic MetDB account)
! 2.1.  17 June 2002.  Brian Barwell.  Change 45/02.
! Extended to store more than one GRIB message for the same time.
! Also modifications for GRIB-2 fields and MHSIFF diagnostics.
! 
! Revision 2.0  2001/05/31 14:16:39  usmdb
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/12/08  15:20:55  15:20:55  usmdb (Generic MetDB account)
! Initial revision
!
! ORIGINAL VERSION WRITTEN BY BRIAN BARWELL, 31/08/00
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

!                                                            PARAMETERS
!
      INTEGER LENREC             ! RECORD LENGTH OF GRIB DATA SET
      INTEGER LENBLK             ! BLOCK SIZE OF GRIB DATA SET
      INTEGER NBREC              ! NUMBER OF RECORDS PER BLOCK
      INTEGER NTBLK              ! NUMBER OF BLOCKS PER TRACK
      INTEGER NTREC              ! NUMBER OF RECORDS PER TRACK
      INTEGER NCTRK              ! NUMBER OF TRACKS PER CYLINDER
      PARAMETER (LENREC=9216, NBREC=3, NTBLK=2, NCTRK=15)
      PARAMETER (LENBLK=LENREC*NBREC, NTREC=NTBLK*NBREC)
!                                                             VARIABLES
      INTEGER CENTURY            ! Y2K FUNCTION TO SET CENTURY
      INTEGER I                  ! GENERAL COUNTER                  !2.1
      INTEGER ICHAR2             ! FUNCTION TO CONVERT C*2 TO I*4   !2.1
      INTEGER ICHAR3             ! FUNCTION TO CONVERT C*3 TO I*4   !2.1
      INTEGER IDUMMY             ! DUMMY ARG. FOR "NEXTFILE"
      INTEGER IEXTRA             ! USED IN CALCULATING INDEX RECORD NO.
      INTEGER IPT                ! POINTER FOR BYTE IN GRIB MESSAGE
      INTEGER IOS                ! STATUS RETURED FROM I/O STATEMENTS
      INTEGER J                  ! GENERAL LOOP VARIABLE
      INTEGER KODE               ! RETURN CODE FROM 'MHSIFF'
      INTEGER LENGTH             ! LENGTH OF GRIB MESSAGE (FROM SCTN. 0)
      INTEGER LEN1, LEN2         ! LENGTH OF GRIB DATA SET NAMES
      INTEGER MGRIB              ! GRIB VERSION NUMBER IN MESSAGE   !2.1
      INTEGER MHSDIAG(8)         ! DIAGNOSTIC INFO FROM 'MHSIFF'    !2.1
      INTEGER NBLOKS             ! TOTAL NO. OF BLOCKS IN INDEX DATA SET
      INTEGER NCOPIED            ! NUMBER OF DATA SETS COPIED SO FAR
      INTEGER NDYEAR             ! 4-DIGIT YEAR FOR DATA TIME
      INTEGER NFILES             ! NUMBER OF DATA SETS PROCESSED SO FAR
      INTEGER NGRIB              ! GRIB VERSION NUMBER IN INDEX D/S !2.1
      INTEGER NKEEP              ! DATA RETENTION PERIOD (YEARS)
      INTEGER NOW(8)             ! CURRENT DATE & TIME (FROM 'DATIM')
      INTEGER NREC               ! RECORD COUNTER
      INTEGER NRECS              ! ESTIMATED RECORDS FOR GRIB DATA SET
      INTEGER NSPACE             ! NUMBER OF TRKS OR CYLS FOR OUTPUT
      INTEGER NTIME(5)           ! YR,MON,DAY,HR,MIN FROM GRIB MESSAGE
      INTEGER NTRKS              ! ESTIMATED TRACKS FOR GRIB DATA SET
      INTEGER NXREC              ! RECORD NUMBER IN INDEX DATA SET
      INTEGER NXYEAR             ! 4-DIGIT YEAR FOR INDEX TIME
      INTEGER NYEARS             ! NO. OF INDEX BLOCKS IN INDEX DATA SET
!
      LOGICAL REPEAT             ! FLAG FOR DUPLICATE GRIB MESSAGE
      LOGICAL GOOD               ! FLAG FOR GOOD (STORABLE) MESSAGE !2.1
      LOGICAL TERMIN8            ! TERMINATION FLAG
!
      CHARACTER*1   DUMMY        ! DUMMY ARG. FOR "NEXTFILE"
      CHARACTER*3   SPACE        ! SPACE CODE ('TRK' OR 'CYL')
      CHARACTER*8   DDN          ! DDNAME (RETURNED BY MHSIFF ROUTINE)
      CHARACTER*44  DSN1, DSN2   ! INPUT AND OUTPUT GRIB DATA SET NAMES
      CHARACTER*4   GRIB         ! 'GRIB' IN ASCII CHARACTERS
      CHARACTER*(LENREC) GRIBREC ! BUFFER TO HOLD GRIB RECORD
      CHARACTER*9306 INDXREC     ! BUFFER TO HOLD INDEX RECORD
      CHARACTER*132 HEAD         ! FOR REVISION INFORMATION
      CHARACTER*44  SKEL         ! SKELETON DATA SET NAME
      CHARACTER*2   YMDH(4)      ! DATE/TIME INDICATORS IN SKELETON NAME
!
!                                                    NAMELIST VARIABLES
!                                   (SEE COMMENTS AT START FOR DETAILS)
      CHARACTER*6       DISK
      CHARACTER*8              OWNER,  JOBNAME
      LOGICAL                                    PRINT
      INTEGER                                           NSECS, NWAITS
      NAMELIST /INSTOR/ DISK,  OWNER,  JOBNAME,  PRINT, NSECS, NWAITS
      DATA              DISK,  OWNER,  JOBNAME,  PRINT, NSECS, NWAITS
     &               /'OPR103','GRB1','GRIBJOB', .TRUE.,   30,     10/
!
!                                                       DATA STATEMENTS
      DATA YMDH /'YY','MM','DD','HH'/
      DATA NFILES, NCOPIED /2*0/, KODE /0/, LEN1 /1/, NXYEAR/0/
      DATA DUMMY, IDUMMY /' ', 0/
!                                                  REVISION INFORMATION
      HEAD='
     &$Source: /data/us0400/mdb/op/lib/source/RCS/gribdat.F,v $
     &'//'$Date: 30/01/2006 20:22:40$ $Revision: 1$'
!
!=======================================================================
!     INITIALISATIONS AND SETUP
!=======================================================================
!
      GRIB = CHAR(71) // CHAR(82) // CHAR(73) // CHAR(66)  ! 'GRIB'
!
!                                                    READ NAMELIST DATA
      OPEN (5,IOSTAT=IOS)
      IF (IOS.EQ.0) THEN
        READ (5, INSTOR, IOSTAT=IOS)
        CLOSE (5)
      END IF
!             OPEN HOUSEKEEPING DATA SET (UNIT 1) & CHECK FOR I/O ERROR
!
      OPEN (1, FILE='HKEEP', ACCESS='DIRECT', RECL=950, IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE (6,'(/T8,A,I4//T5,A)')
     &         'ERROR OPENING HOUSEKEEPING DATA SET - "IOSTAT" =', IOS,
     &         'JOB WILL NOW TERMINATE WITH USER RETURN CODE 800'
        CALL SYSABN (800)
      END IF
!                    OPEN INDEX DATA SET (UNIT 2) & CHECK FOR I/O ERROR
!
      OPEN (2, FILE='INDEX', ACCESS='DIRECT', RECL=9306, IOSTAT=IOS)
      IF (IOS.NE.0) THEN
        WRITE (6,'(/T8,A,I4//T5,A)')
     &         'ERROR OPENING INDEX DATA SET - "IOSTAT" =', IOS,
     &         'JOB WILL NOW TERMINATE WITH USER RETURN CODE 810'
        CALL SYSABN (810)
      END IF
!                  READ HEADER OF INDEX DATA SET AND CHECK GRIB EDITION
!
      READ (2, REC=1) NBLOKS, NYEARS, NKEEP, I, J, SKEL             !2.1
      NGRIB = I/65536  ! GRIB edition (in first 2 bytes of I)       !2.1
      TERMIN8 = NGRIB.LT.1 .OR. NGRIB.GT.2                          !2.1
      IF (TERMIN8) WRITE (6,'(/T3,A,I6/)')                          !2.1
     &   'INVALID GRIB EDITION NUMBER IN INDEX DATA SET', NGRIB     !2.1
      IEXTRA = NBLOKS - NYEARS + 1
!
!                           ALLOCATE JOB STATUS RECORD IN H.K. DATA SET
!         ("GOOD" IS USED TEMPORARILY HERE TO INDICATE WHETHER MHS DATA
!            SET NAMES ARE TO BE TAKEN FROM THE HOUSEKEEPING DATA SET.)
!
      GOOD = .FALSE. ! I.E. NOT USING 'SDB1' DATA                   !2.1
      CALL NEXTFILE (JOBNAME, IDUMMY, GOOD)                         !2.1
!
!=======================================================================
!     LOOP OVER INCOMING GRIB DATA SETS.
!=======================================================================
!
      DO WHILE (.NOT.TERMIN8)
!                              LOCATE NEXT DATA SET & CHECK RETURN CODE
!
        CALL MHSIFF (DSN1, DDN, 'GR  ', OWNER, MHSDIAG)             !2.1
        KODE = MHSDIAG(1)                                           !2.1
!
!-----------------------------------------------------------------------
!       KODE=0:  NEW DATA SET FOUND - OPEN IT.
!-----------------------------------------------------------------------
!
        IF (KODE.EQ.0) THEN
          NFILES = NFILES + 1
          LEN1 = INDEX(DSN1,' ') - 1
          IF (LEN1.LT.0) LEN1 = LEN(DSN1)
!
!                     OPEN THE DATA SET (UNIT 10) & CHECK FOR I/O ERROR
          OPEN (10,
     &          FILE=DDN, STATUS='OLD', FORM='FORMATTED', IOSTAT=IOS)
          IF (IOS.NE.0) THEN
            WRITE (6,'(T5,3A,I4)') 'I/O ERROR OPENING FILE "',
     &               DSN1(1:LEN1), '".  "IOSTAT" =', IOS
          ELSE
            GOOD = .FALSE.  ! Message quality not yet proved
            LENGTH = 0      ! Length not yet found
!
!-----------------------------------------------------------------------
!           EXTRACT GRIB EDITION FROM MESSAGE AND CHECK AGAINST INDEX.
!-----------------------------------------------------------------------
!
!                                                   FIND GRIB SECTION 0
            READ (10,'(A)',IOSTAT=IOS) GRIBREC
            IPT = INDEX(GRIBREC(1:LENREC-40),GRIB)  ! 'GRIB'        !2.1
            MGRIB = ICHAR(GRIBREC(IPT+7:IPT+7))     ! Edition no.   !2.1
            IF (IPT.EQ.0) THEN ! 'GRIB' NOT FOUND
              WRITE (6,'(T5,3A)')
     &                 '"GRIB" NOT FOUND IN FILE "', DSN1(1:LEN1), '".'
!
!                                                   CHECK GRIB EDITIONS
            ELSE IF (MGRIB.NE.NGRIB) THEN                           !2.1
              WRITE (6,'(T5,2A,2I8)') 'GRIB EDITIONS IN MESSAGE ',  !2.1
     &                 'AND INDEX DON''T AGREE:', MGRIB, NGRIB      !2.1
            ELSE
!
!-----------------------------------------------------------------------
!           DECODE MESSAGE LENGTH FROM GRIB SECTION 0.
!-----------------------------------------------------------------------
!
              IF (MGRIB.EQ.1) THEN                                  !2.1
                LENGTH = ICHAR3(GRIBREC(IPT+4:IPT+6))               !2.1
                GOOD = .TRUE. ! Message OK                          !2.1
              ELSE                                                  !2.1
                I = ICHAR3(GRIBREC(IPT+8:IPT+10))                   !2.1
                J = ICHAR3(GRIBREC(IPT+11:IPT+13))                  !2.1
                IF (I.NE.0 .OR. J.GT.32767) THEN                    !2.1
                  WRITE (6,'(T5,2A,Z20)') 'GRIB MESSAGE ',          !2.1
     &                     'IS TOO LONG:', GRIBREC(IPT+8:IPT+15)    !2.1
                ELSE
                  LENGTH = 65536*J + ICHAR2(GRIBREC(IPT+14:IPT+15)) !2.1
                  GOOD = .TRUE. ! Message OK                        !2.1
                END IF                                              !2.1
              END IF                                                !2.1
            END IF
!
!-----------------------------------------------------------------------
!           IF OK, DECODE DATE AND TIME FROM GRIB SECTION 0.
!-----------------------------------------------------------------------
!
            IF (GOOD) THEN
              IF (MGRIB.EQ.1) THEN                ! GRIB 1:         !2.1
                I = IPT + 20                      ! Century year    !2.1
                NTIME(1) = ICHAR(GRIBREC(I:I))    ! in byte 21      !2.1
!
              ELSE                                ! GRIB 2:         !2.1
                I = IPT + 29                      ! Full year in    !2.1
                NTIME(1) = ICHAR2(GRIBREC(I-1:I)) ! bytes 29-30     !2.1
              END IF                                                !2.1
              NTIME(1) = MOD(NTIME(1),100) ! Last 2 digits of year
!
              DO J=2,5      ! Month, day, hour, minute              !2.1
                I = I + 1                                           !2.1
                NTIME(J) = ICHAR(GRIBREC(I:I))                      !2.1
              END DO ! J
!
!-----------------------------------------------------------------------
!             ESTIMATE DISK SPACE REQUIRED AND MAKE DATA SET NAME.
!-----------------------------------------------------------------------
!
              IPT = IPT + LENGTH - 1
              NRECS = (IPT-1)/LENREC + 1    ! NUMBER OF RECORDS
              NTRKS = (NRECS-1)/NTREC  + 1  ! NUMBER OF TRACKS
              IF (NTRKS.LT.NCTRK) THEN
                SPACE = 'TRK'
                NSPACE = NTRKS
              ELSE
                SPACE = 'CYL'
                NSPACE = (NTRKS-1)/NCTRK  + 1  ! NUMBER OF CYLINDERS
              END IF
!                                              CREATE NEW DATA SET NAME
              DSN2 = SKEL
!                                    (1) MESSAGE NUMBER
              I = INDEX(DSN2,'##')                                  !2.1
              IF (I.GT.0) DSN2(I:I+1) = DSN1(LEN1-1:LEN1)           !2.1
!
!                                    (2) DATE AND TIME
              DO J=1,4
                I = INDEX(DSN2,YMDH(J))                             !2.1
                WRITE (DSN2(I:I+1),'(I2.2)') NTIME(J)               !2.1
              END DO ! J
              LEN2 = INDEX(DSN2//' ',' ') - 1
!
!                                 CHECK WHETHER DATA SET ALREADY EXISTS
!
              INQUIRE (FILE='/'//DSN2, EXIST=REPEAT, IOSTAT=IOS)
              IF (REPEAT) THEN
                WRITE (6,'(T5,3A)') 'GRIB DATA REJECTED - DATA SET "',
     &                               DSN2(1:LEN2), '" ALREADY EXISTS.'
              ELSE
!
!-----------------------------------------------------------------------
!               DEFINE, CREATE, OPEN AND FILL OUTPUT DATA SET.
!-----------------------------------------------------------------------
!
!                                DEFINE CHARACTERISTICS OF NEW DATA SET
!
                CALL FILEINF (KODE, 'DEVICE', '3390', 'VOLSER', DISK,
     &               SPACE, NSPACE, 'SECOND', 1, 'RECFM', 'FB',
     &              'LRECL', LENREC, 'BLKSIZE', LENBLK)
!
!                                                 CHECK FILE DEFINITION
                IF (KODE.NE.0) THEN
                  WRITE (6,'(T5,3A,I4)') 'BAD OUTPUT FILE DEFINITION ',
     &                     '- RETURN CODE IS', KODE
                ELSE
!                     OPEN NEW DATA SET (UNIT 20) & CHECK FOR I/O ERROR
!
                  OPEN (20, FILE='/'//DSN2, STATUS='NEW', IOSTAT=IOS)
                  IF (IOS.NE.0) THEN
                    WRITE (6,'(T5,3A,I4)') 'I/O ERROR OPENING FILE "',
     &                         DSN2(1:LEN2), '".  "IOSTAT" =', IOS
                  ELSE
!                                     COPY TO NEW DATA SET AND CLOSE IT
                    NREC = 0
                    DO WHILE (IOS.EQ.0)
                      NREC = NREC + 1
                      WRITE (20,'(A)') GRIBREC
                      READ  (10,'(A)',IOSTAT=IOS) GRIBREC
                    END DO
                    NCOPIED = NCOPIED + 1
                    CLOSE (20)
!
!-----------------------------------------------------------------------
!                   UPDATE INDEX DATA SET.
!-----------------------------------------------------------------------
!
                    NDYEAR = NTIME(1) + CENTURY(NTIME(1)) ! 4-digit yr.
                    IF (NDYEAR.NE.NXYEAR) THEN ! READ NEW INDEX RECORD
                      NXREC = MOD(NDYEAR,NYEARS) + IEXTRA
                      READ (2, REC=NXREC) INDXREC
                      READ (INDXREC(1:4),'(I4)') NXYEAR
!
!                              CREATE NEW RECORD IF OLD ONE HAS EXPIRED
!
                      CALL DATIM (NOW)
                      IF (NXYEAR.LT.NOW(8)-NKEEP) THEN ! EXPIRED
                        INDXREC = ' '
                        WRITE (INDXREC(1:4),'(I4)') NDYEAR
                        NXYEAR = NDYEAR
                      END IF
                    END IF
!                         UPDATE INDEX RECORD IF NOT DONE AND WRITE OUT
!                   (25 BYTES/DAY, 775 BYTES/MONTH - SEE 'DESCRIPTION')
!
                    IF (NXYEAR.NE.NDYEAR) THEN
                      WRITE (6,'(T5,A,T15,2A,2I6)') 'GRIBDAT:',
     &                         'INDEX RECORD NOT AVAILABLE - ',
     &                         'DATA & INDEX YEARS =', NDYEAR,NXYEAR
                    ELSE
                      J = 775*(NTIME(2)-1) + 25*(NTIME(3)-1)
     &                       + NTIME(4) + 7
                      IF (INDXREC(J:J).NE.'X') THEN                 !2.1
                        INDXREC(J:J) = 'X'
                        WRITE (2, REC=NXREC) INDXREC
                      END IF                                        !2.1
                    END IF
!                                             PRINT INFORMATION MESSAGE
                    IF (PRINT) THEN
                      CALL DATIM (NOW)
                      WRITE (6,'(T2, I2.2,A,I2.2,A,I2.2, 5A)') NOW(5),
     &                  ':', NOW(4), ':', NOW(3), '  ', DSN1(1:LEN1),
     &                  ' copied to ', DSN2(1:LEN2), '.'            !2.1
                    END IF
                  END IF
                END IF
              END IF
            END IF
          END IF
!                                       CLOSE AND RENAME INPUT DATA SET
          CLOSE (10)
          CALL MHSIFF (DSN1, DDN, 'R   ', OWNER, MHSDIAG)           !2.1
          KODE = MHSDIAG(1)                                         !2.1
          IF (KODE.NE.0) KODE = -2  ! MHS RENAME FAILED
!
!-----------------------------------------------------------------------
!       KODE=4  NO DATA SETS WAITING - TERMINATE OR WAIT.
!-----------------------------------------------------------------------
!
        ELSE IF (KODE.EQ.4) THEN
          IF (NWAITS.LE.0) THEN   ! TERMINATE NOW
            TERMIN8 = .TRUE.
          ELSE                    ! WAIT FOR MORE DATA
            CALL SECSOUT (NSECS)
            DO J=2,NWAITS
              CALL NEXTFILE (DUMMY,IDUMMY,TERMIN8)  ! TO UPDATE JOB
                                                    ! STATUS RECORD
              IF (TERMIN8) GO TO 999
              CALL SECSOUT (NSECS)
            END DO ! J
          END IF
!
!-----------------------------------------------------------------------
!       KODE NOT 0 OR 4:  MHS PROBLEM - FLAG DATA SET FOR DELETION
!-----------------------------------------------------------------------
!
        ELSE
          KODE = -1
        END IF
!                               IF MHS ERROR, TRY DELETING THE DATA SET
        IF (KODE.LT.0) THEN
          WRITE (6,'(T5,A,I3,3A)') 'MHS ERROR - RETURN CODE', KODE,
     &             '.  WILL DELETE "', DSN1(1:LEN1), '".'
          CALL MHSIFF (DSN1, DDN, 'D  N', OWNER, MHSDIAG)           !2.1
          IF (MHSDIAG(1).EQ.0) THEN                                 !2.1
            KODE = 0
          ELSE
            TERMIN8 = .TRUE.  ! STILL BAD - GIVE UP
          END IF
        END IF
!                                              UPDATE JOB STATUS RECORD
        CALL NEXTFILE (DUMMY,IDUMMY,TERMIN8)
      END DO ! WHILE
!
!=======================================================================
!     PRINT SUMMARY AND TERMINATE JOB
!=======================================================================
!
  999 CONTINUE
      WRITE (6,'(/T5,I4,A,I4,A)') NFILES, ' GRIB DATA SETS READ AND',
     &          NCOPIED, ' COPIED.'
!
!             MESSAGES FOR ABNORMAL JOB TERMINATION DUE TO MHSIFF ERROR
!
      IF (KODE.LT.0) THEN
        J = INDEX(OWNER,' ') - 1  ! (J used for length of OWNER here)
        IF (J.LE.0) J = LEN(OWNER)
        WRITE (6,'(/T2,2A)') 'AN ERROR OCCURRED IN THE MESSAGE ',
     &           'HANDLING ROUTINE "MHSIFF" WHILE ATTEMPTING TO'
        IF (KODE.EQ.-1) THEN
          WRITE (6,'(T2,A/)') 'LOCATE THE NEXT DATA SET TO PROCESS.'
          WRITE (6,'(T2,2A)') 'TRY RE-RUNNING THE JOB: IF THE ',
     &             'PROBLEM RECURS, TRY DELETING THE NEXT "',
     &              OWNER(1:J), '" DATA SET (LOOK FOR "MHSR.*.*.',
     &              OWNER(1:J), '.*").'
        ELSE IF (KODE.EQ.-2) THEN
          WRITE (6,'(T2,2A/)') 'RENAME OR DELETE THE DATA SET ', DSN1
          WRITE (6,'(T2,2A)')
     &             'CHECK THAT THIS DATA SET DOES NOT EXIST. ',
     &             'IF IT DOES, DELETE IT AND RERUN THE JOB.'
        END IF
        KODE = 800 - KODE
        WRITE (6,'(/T2,A,I4/)')
     &           'THIS JOB WILL NOW ABEND WITH USER CODE', KODE
        CALL SYSABN (KODE)
      END IF
!                                                     END OF PROCESSING
      STOP
      END
