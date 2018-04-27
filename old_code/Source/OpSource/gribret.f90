SUBROUTINE GRIBRET (DATYPE, DSNX, NOBS, IDATA, KEYWRD, LTEST, &
                    ISELECT, ITIME, ISTAT, CREP, RVALS)

!----------------------------------------------------------------------
! SUBROUTINE   : GRIBRET
!
! PURPOSE      : TO RETURN REQUESTED GRIB FIELDS IN A USER-SUPPLIED
!                CHARACTER ARRAY.
!
! DESCRIPTION  : 'GRIBRET' RETRIEVES THE GRIB FIELD(S) MATCHING THE
!                DATA TYPE ('DATYPE') AND TIME WINDOW ('ITIME') AND
!                RETURNS THE DATA IN THE USER-SUPPLIED ARRAY 'CREP',
!                A CHARACTER*(*) ARRAY OF DIMENSION 'NOBS'.
!
!                IF TWO OR MORE FIELDS ARE REQUESTED THEY WILL BE
!                RETURNED IN ONE CALL (IF 'CREP' IS BIG ENOUGH),
!                FIELDS BEING CONCATENATED IN THE ARRAY.
!
!                IF 'CREP' IS NOT LARGE ENOUGH TO HOLD THE FIELD(S),
!                IT IS FILLED WITH AS MUCH DATA AS IT CAN HOLD AND A
!                STATUS CODE ('ISTAT') OF 4 IS RETURNED TO INDICATE
!                MORE DATA TO COME.  THE NEXT CALL WILL THEN RETURN
!                ANOTHER BATCH OF DATA STARTING FROM WHERE THE
!                PREVIOUS CALL LEFT OFF.  WHEN THE FINAL BATCH OF
!                DATA IS RETURNED THE UNUSED PART OF 'CREP' IS PADDED
!                WITH BINARY ZEROES AND AN 'ISTAT' OF 0 IS RETURNED.
!                IF NO DATA IS FOUND AN 'ISTAT' OF 8 IS RETURNED.
!
!                AFTER EACH CALL, THE VALUES IN THE REAL ARRAY
!                'RVALS' WILL CONTAIN THE NUMBER OF BYTES OF DATA
!                RETURNED IN THE CORRESPONDING ELEMENTS OF 'CREP'.
!                'RVALS' IS TREATED AS 1-DIMENSIONAL IN THIS ROUTINE
!                AND THEREFORE REQUIRES THE USER TO SPECIFY 'NELEM'=1
!                WHEN CALLING 'MDB' (SEE MET.D.B. TECH. NOTE. 3.)
!
! USAGE        : CALL GRIBRET (DATYPE, DSNX, NOBS, IDATA, KEYWRD,
!                     LTEST, ISELECT, ITIME, ISTAT, CREP, RVALS)
!
! PARAMETERS   : DATYPE   I   (CHARACTER*(*)) MET.D.B. DATA SUBTYPE.
!                DSNX     I   (CHARACTER*(*)) NAME OF INDEX DATA SET.
!                NOBS     I   DIMENSION OF 'CREP' ARRAY.
!                IDATA    I   5-ELEMENT INTEGER ARRAY WITH INDEX DATA
!                             SET DETAILS AS OUTPUT BY 'DDICT':
!                               1  LENGTH OF DATA SET NAME,
!                               2  DATA SET RECORD LENGTH,
!                               3  REQUIRED FT NUMBER,
!                              4&5 (NOT USED FOR GRIB DATA).
!                KEYWRD   I   ARRAY OF MDB KEYWORDS (FOR 'GRIBFIND')
!                LTEST    I   SET .TRUE. FOR EXTRA PRINT STATEMENTS.
!                ISELECT  I   LIST OF WANTED DATA SELECTION VALUES
!                ITIME   I/O  (8-ELEMENT INTEGER ARRAY) USER'S START
!                             AND END TIMES AS OUTPUT BY 'DDICT'.
!                             (EACH TIME IS YYYY, MM, DD, HHMM.) THE
!                             WINDOW MAY BE NARROWED BY 'GRIBINDX'.
!                ISTAT   I/O  RETRIEVAL REQUEST STATUS:
!                               0  (INPUT)  NEW RETRIEVAL REQUEST,
!                                  (OUTPUT) RETRIEVAL COMPLETED,
!                               4  (INPUT)  CONTINUATION OF RETRIEVAL,
!                                  (OUTPUT) MORE DATA STILL TO COME,
!                               8  (OUTPUT) NO DATA AVAILABLE,
!                              16  (OUTPUT) FATAL ERROR IN RETRIEVAL.
!                CREP     O   (CHARACTER*(*)) ARRAY TO HOLD RETRIEVED
!                             GRIB DATA.
!                RVALS    O   (REAL ARRAY) NTH VALUE RETURNS THE
!                             NUMBER OF BYTES OF DATA IN CREP(N).
!
! CALLED BY    : MDB
!
! CALLS        : GRIBFIND, GRIBINDX, GRIBREAD
!
! REVISION INFO :
!
! $Workfile: gribret.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 22/04/2012 18:13:52$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         22/04/2012 18:13:52    Sheila Needham  Fixed
!       bug that occurs when the user array has less than one record left
!       (9216 btes) when the end of dataset is encountered.
!  9    MetDB_Refresh 1.8         15/03/2012 14:14:18    Sheila Needham
!       Changed intent on NOBS to inout
!  8    MetDB_Refresh 1.7         15/03/2012 12:58:41    Sheila Needham
!       Changes for RADOP and RADRF
!  7    MetDB_Refresh 1.6         29/11/2010 08:42:05    Sheila Needham  SAVE
!       all variables
!  6    MetDB_Refresh 1.5         22/11/2010 14:17:00    Stan Kellett    RVALS
!       dimension changed to NOBS from :
!  5    MetDB_Refresh 1.4         18/11/2010 11:45:43    John Norton     After
!       doing merge batch 9 changes.
!  4    MetDB_Refresh 1.3         15/11/2010 14:24:55    John Norton
!       Updated after rework for batch 2
!  3    MetDB_Refresh 1.2         29/10/2010 10:51:47    John Norton
!       Porting batch 2. Initial ported version
!  2    MetDB_Refresh 1.1         13/10/2010 16:17:17    John Norton     
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

USE gribfind_mod
USE gribindx_mod
USE gribread_mod

! <Data Modules>


IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)        ::  DATYPE ! MET.D.B.DATA TYPE
CHARACTER(*), INTENT(IN)        ::  DSNX ! NAME OF INDEX DATA SET
INTEGER, INTENT(INOUT)          ::  NOBS ! NO. OF ELEMENTS IN USER BUFFER
INTEGER, INTENT(IN)             ::  IDATA(5) ! INFORMATION ABOUT INDEX DATA SET (SEE ABOVE)
LOGICAL, INTENT(IN)             ::  KEYWRD(:) ! MDB KEYWORDS (= 'FOUND' ARRAY IN 'MDB')
LOGICAL, INTENT(IN)             ::  LTEST ! .TRUE. IF DIAGNOSTIC PRINTOUT IS REQUIRED
INTEGER, INTENT(IN)             ::  ISELECT(50) ! USER'S LIST OF DATA SELECTION VALUES
INTEGER, INTENT(INOUT)          ::  ITIME(8) ! START AND END TIMES OF REQUEST PERIOD
INTEGER, INTENT(INOUT)          ::  ISTAT ! STATUS CODE RETURNED BY THIS ROUTINE
CHARACTER(*), INTENT(OUT)       ::  CREP(NOBS) ! USER BUFFER TO HOLD    GRIB DATA
REAL, INTENT(OUT)               ::  RVALS(NOBS) ! NUMBER OF OUTPUT BYTES IN EACH 'CREP' ELEMENT

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

!                                                            PARAMETERS
!
INTEGER,PARAMETER          ::  MAXREC = 9216 ! MAXIMUM RECORD LENGTH FOR GRIB DATA SET
!                                                             VARIABLES
!
INTEGER      ::  IUNIT ! UNIT NUMBER FOR GRIB DATA SET
INTEGER      ::  J ! GENERAL LOOP VARIABLE
INTEGER      ::  KODE ! RETURN CODE FROM SUBROUTINES CALLED
INTEGER      ::  LEFT ! NUMBER OF BYTES OF MESSAGE LEFT TO TRANSFER
INTEGER      ::  LENREC ! RECORD LENGTH OF GRIB DATA SET
INTEGER      ::  NCOPIED ! NUMBER OF BYTES COPIED FROM 'BUFFER' SO FAR
INTEGER      ::  NEMPTY ! UNFILLED SPACE IN ELEMENT OF USER BUFFER
INTEGER      ::  NEXTIN ! NEXT BYTE TO COPY FROM 'BUFFER'
INTEGER      ::  NEXTOUT ! NEXT BYTE TO COPY TO IN USER BUFFER
INTEGER      ::  NFILLED ! FILLED SPACE IN ELEMENT OF USER BUFFER
INTEGER      ::  NHHMM1 ! HOURS AND MINUTES ('HHMM') FOR START TIME
INTEGER      ::  NHHMM2 ! HOURS AND MINUTES ('HHMM') FOR END TIME
INTEGER      ::  NLAST ! NUMBER OF WANTED BYTES IN 'BUFFER'
INTEGER      ::  NLMNT ! CURRENT ELEMENT OF USER BUFFER
INTEGER      ::  NSIZE ! SIZE OF EACH ELEMENT OF USER BUFFER
INTEGER      ::  N2COPY ! NUMBER OF BYTES STILL TO COPY FROM 'BUFFER'
!
!
LOGICAL      ::  COPYING ! .TRUE. IF DATA TRANSFER IS IN PROGRESS
LOGICAL      ::  FOUND ! .TRUE. IF SOME DATA RETURNED TO USER THIS CALL
LOGICAL      ::  NEWDATA = .TRUE. ! .TRUE. IF NO GRIB DATA SET IS OPEN
LOGICAL      ::  NEWRQST ! .TRUE. IF THIS CALL IS A NEW RETRIEVAL REQUEST
!
CHARACTER(MAXREC) ::  BUFFER ! INTERNAL BUFFER FOR 1 GRIB RECORD
CHARACTER(8)      ::  OLDTYPE = ' ' ! MET.D.B. DATA TYPE FOR PREVIOUS CALL
!

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------
!                                                                 SAVES
!
SAVE
!
!
!-----------------------------------------------------------------------
!  CHECK FOR NEW RETRIEVAL REQUEST
!-----------------------------------------------------------------------
!
IF (ISTAT == 0) THEN ! NEW REQUEST
   NEWRQST = .TRUE.
   NEWDATA = .TRUE.
!                                         CHECK FOR INVALID STATUS CODE
ELSE IF (ISTAT /= 4) THEN
   WRITE (6,'(T5,A,T15,A,I4)') 'GRIBRET:', &
             'INVALID INPUT STATUS CODE - ', ISTAT
   ISTAT = 16
   RETURN
END IF
!
!-----------------------------------------------------------------------
!  INITIALISATIONS
!-----------------------------------------------------------------------
!                                      INITIALISE USER BUFFER TO ZEROES
NSIZE = LEN(CREP(1))
!                                          (1) FIRST ELEMENT
DO J=1,NSIZE
   CREP(1)(J:J) = CHAR(0)
END DO ! J
RVALS(1) = 0
!                                          (2) OTHER ELEMENTS
DO J=2,NOBS
   CREP(J) = CREP(1)
   RVALS(J) = 0
END DO ! JLMNT
!                                              INITIALISE POINTERS ETC.
NLMNT = 1
NFILLED = 0
COPYING = .TRUE.
FOUND = .FALSE.
!                              CONVERT HOURS FROM 'HHMM' TO 'HH' FORMAT
NHHMM1 = ITIME(4)
NHHMM2 = ITIME(8)
ITIME(4) = ITIME(4)/100
ITIME(8) = ITIME(8)/100
!                                               CHECK FOR NEW DATA TYPE
IF (DATYPE /= OLDTYPE) THEN
   NEWDATA = .TRUE.
   CALL GRIBINDX (DSNX, IDATA, ITIME, KODE)

!                                                     CHECK RETURN CODE
   IF (KODE == 4) THEN
      OLDTYPE = DATYPE
   ELSE
      COPYING = .FALSE.  ! FORCES QUICK EXIT
      OLDTYPE = ' '
   END IF
END IF
!
!=======================================================================
!  LOOP UNTIL ALL DATA HAS BEEN TRANSFERRED OR USER'S BUFFER IS FILLED
!=======================================================================
!
DOLABEL1: &
DO WHILE (COPYING)
!                               IF NO DATA SET OPEN, LOOK FOR A NEW ONE
IFLABEL1: &
   IF (NEWDATA) THEN
      IF (NEWRQST) THEN
         KODE = -1         ! INDICATES NEW REQUEST
         NEWRQST = .FALSE.
      ELSE
         KODE = ISTAT
      END IF
!
      CALL GRIBFIND (DATYPE, IDATA, ITIME, ISELECT, KEYWRD, &
                     LTEST, KODE, IUNIT, LENREC)

!
!                                         'KODE'=4: NEW DATA SET OPENED
IFLABEL2: &
      IF (KODE == 4) THEN
         LEFT = -1
         CALL GRIBREAD &
              (IUNIT, LENREC, LEFT, NCOPIED, BUFFER, KODE)

         IF (KODE == 16) THEN
            COPYING = .FALSE.  ! FORCES QUICK EXIT
         ELSE IF (KODE < 8) THEN
            NLAST = MIN0(LEFT+NCOPIED,LENREC)
            NEWDATA = .FALSE.
         END IF
!                                           'KODE'=8: NO MORE DATA SETS
      ELSE
         COPYING = .FALSE.
      END IF IFLABEL2
!                                                  POINTERS FOR BUFFERS
   ELSE
      FOUND = .TRUE.
      NEXTIN  = NCOPIED + 1
      NEXTOUT = NFILLED + 1
      NEMPTY = NSIZE - NFILLED
      N2COPY = NLAST - NCOPIED
!
!-----------------------------------------------------------------------
!  FILL CURRENT ELEMENT OF USER'S BUFFER WITH DATA FROM TRANSFER BUFFER
!-----------------------------------------------------------------------
!
IFLABEL3: &
      IF (NEMPTY < N2COPY) THEN
         NCOPIED = NCOPIED + NEMPTY
         CREP(NLMNT)(NEXTOUT:NSIZE) = BUFFER(NEXTIN:NCOPIED)
         RVALS(NLMNT) = NSIZE
         LEFT = LEFT - NEMPTY
!                                          INCREMENT ELEMENT NUMBER ...
         IF (NLMNT < NOBS) THEN
            NLMNT = NLMNT + 1
            NFILLED = 0
!                                    ... OR STOP IF ALL ELEMENTS FILLED
         ELSE
            IF (KODE == 8 .OR. KODE == 12) THEN
              KODE = 12    ! dataset is closed
            ELSE        ! DATASET IS STILL OPEN
              KODE = 4  ! (MORE DATA TO COME)
            END IF
            COPYING = .FALSE.
         END IF
!
!-----------------------------------------------------------------------
!  EMPTY DATA FROM TRANSFER BUFFER INTO USER'S BUFFER & READ MORE IN
!-----------------------------------------------------------------------
!
      ELSE
         IF (KODE == 12) KODE = 8   
         NFILLED = NFILLED + N2COPY
         CREP(NLMNT)(NEXTOUT:NFILLED) = BUFFER(NEXTIN:NLAST)
         RVALS(NLMNT) = NFILLED
         LEFT = LEFT - N2COPY

IFLABEL4: &
         IF (KODE == 8) THEN  ! LOOK FOR NEXT DATASET
            NEWDATA=.TRUE.
         ELSE IF (KODE == 4) THEN
!                                     LOOK FOR ANOTHER RECORD
            CALL GRIBREAD &
                 (IUNIT, LENREC, LEFT, NCOPIED, BUFFER, KODE)
!
!                                                     CHECK RETURN CODE
            IF (KODE == 16) THEN
               COPYING = .FALSE.     ! FORCES QUICK EXIT
            ELSE IF (KODE == 8) THEN  ! ENSURE LAST REC IS PROCESSED
               CLOSE (IUNIT)
               NLAST = MIN0(LEFT+NCOPIED,LENREC)
            ELSE
               NLAST = MIN0(LEFT+NCOPIED,LENREC)
            END IF
         END IF IFLABEL4
      END IF IFLABEL3
   END IF IFLABEL1
END DO DOLABEL1
!
!=======================================================================
!  SET RETRIEVAL STATUS CODE.
!=======================================================================
!            'KODE'=8 MEANS NO DATA FOUND THIS CALL BUT DATA MAY HAVE
!              BEEN FOUND BY PREVIOUS CALLS AS INDICATED BY 'FOUND'.
!            'KODE'=12 MEANS WE'RE STILL TRANSFERRING DATA FROM THE 
!              LAST RECORD BUT THERE'S NO MORE TO BE READ FROM THIS
!              DATASET
!            'KODE'=4 MEANS THERE MAY BE MORE DATA TO COME, EITHER IN
!              THIS DATASET OR ANOTHER ONE 
!
IF (FOUND .AND. KODE == 8) THEN
   ISTAT = 0
ELSE IF (KODE == 12)THEN
   ISTAT = 4
ELSE
   ISTAT = KODE
END IF
!                         RETURN ACTUAL NUMBER OF ARRAY ELEMENTS FILLED
IF (ISTAT <= 4) NOBS = NLMNT
!                         RESTORE 'ITIME' AND RETURN TO CALLING PROGRAM
ITIME(4) = NHHMM1
ITIME(8) = NHHMM2
RETURN
END SUBROUTINE GRIBRET
