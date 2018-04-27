SUBROUTINE FINDMHS (NFT, OWNER, MESAGE, MSTART, MLNGTH, MSGCODE, MHSTYPE)

!-----------------------------------------------------------------------
!
! SUBROUTINE : FINDMHS
!
! PURPOSE    : TO FIND A NEW MESSAGE & PUT IT IN THE USER'S BUFFER
!              RETURNING A POINTER TO THE START OF THE MESSAGE AND
!              THE MESSAGE LENGTH. A NON-ZERO RETURN CODE IS SET
!              IF THERE'S AN MHS PROBLEM, THE END OF A DATA SET IS
!              REACHED OR IF THERE ARE NO DATA SETS WAITING.
!
!                THE DATA SETS TO BE READ MUST BE IN THE FORMAT
!              DESCRIBED IN GPCS TECH. NOTE 40 (SEE THE SECTION
!              'MESSAGE STORAGE' AND APPENDIX A).
!
! CALLED BY  : ANYTHING WHICH READS GTS DATA FTP'D FROM TROPICS.
!
! USAGE      : CALL FINDMHS
!                      (NFT, OWNER, MESAGE, MSTART, MLNGTH, MSGCODE, MHSTYPE)
!
! ARGUMENTS : NFT     (INPUT) UNIT NO. OF MESSAGE FILE TO BE READ.
!              OWNER   (INPUT) DATA SET OWNERSHIP - E.G. 'MDB1'.
!                              (CHARACTER - UP TO 8 BYTES)
!              MESAGE  (IN/OUT) CHARACTER STRING TO HOLD MESSAGES.
!              MSTART  (OUTPUT) START BYTE OF NEXT MESSAGE.
!              MLNGTH  (OUTPUT) LENGTH OF NEXT MESSAGE IN BYTES.
!              MSGCODE (OUTPUT) RETURN CODE - CODED AS FOLLOWS:
!                    -2 - MHSIFFC ERROR WHILE RENAMING DATA SET,
!                    -1 - MHSIFFC ERROR LOCATING NEW DATA SET,
!                     0 - MESSAGE SUCCESSFULLY FOUND,
!                     1 - END OF DATA SET - NO MESSAGE RETURNED,
!                     2 - NO MORE DATA SETS WAITING AT PRESENT.
!              MHSTYPE  (I) C*4 'TEST' for MHSP->MHST
!                               'OPER' for MHSR->MHSP
!
!              MESSAGE LENGTH AND START POINTER ARE RETURNED AS ZERO
!              UNLESS RETURN CODE = 0.
!
! CALLS      : DREG, MHSIFFC (C function), NEXTMSG.
!
! REVISION INFO :
!
! $Workfile: findmhs.f90$ $Folder: OpSource$
! $Revision: 10$ $Date: 24/05/2011 15:18:03$
!
! CHANGE RECORD :
!
! $Log:
!  10   MetDB_Refresh 1.9         24/05/2011 15:18:03    Brian Barwell   Delete
!        variable LENGCH and tidy up code.
!  9    MetDB_Refresh 1.8         18/05/2011 09:45:26    Stan Kellett    If MHS
!        catelogue error and cant open file then retry up to 3 times as could
!       be the MHS routines have not completed.
!  8    MetDB_Refresh 1.7         15/04/2011 14:55:43    Sheila Needham  Extra
!       argument to choose between MHSP and MHSR datasets to be processed.
!  7    MetDB_Refresh 1.6         22/03/2011 14:52:23    Sheila Needham
!       Changes following review
!  6    MetDB_Refresh 1.5         16/03/2011 09:47:26    Sheila Needham
!       Delimit strings with CHAR(0) for C function
!  5    MetDB_Refresh 1.4         15/03/2011 10:34:33    Sheila Needham
!       Changes for new MHSIFFC routine
!  4    MetDB_Refresh 1.3         08/03/2011 11:40:53    Sheila Needham
!       Changed open statement to direct access
!  3    MetDB_Refresh 1.2         31/01/2011 17:20:25    John Norton     After
!       rework on BUFRDAT batch 4 done.
!  2    MetDB_Refresh 1.1         27/01/2011 17:55:16    John Norton
!       BUFRDAT batch 4 ported now ready for review.
!  1    MetDB_Refresh 1.0         26/01/2011 13:49:39    John Norton
!       Pre-porting version of f77 code batch BUFRDAT4
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
! Use statements:

USE dreg_mod
USE nextmsg_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,           INTENT(IN)    :: NFT     !a01 UNIT NUMBER OF MESSAGE FILE
CHARACTER(LEN=*),  INTENT(IN)    :: OWNER   !a02 OWNERSHIP OF REQUIRED DATASET
CHARACTER(LEN=*),  INTENT(INOUT) :: MESAGE  !a03 BUFFER CONTAINING MESSAGE(S)
INTEGER,           INTENT(OUT)   :: MSTART  !a04 POINTER TO START OF MESSAGE
INTEGER,           INTENT(OUT)   :: MLNGTH  !a05 LENGTH OF MESSAGE (BYTES)
INTEGER,           INTENT(OUT)   :: MSGCODE !a06 RETURN CODE FROM "FINDMHS"
CHARACTER(LEN=4),  INTENT(IN)    :: MHSTYPE !a07 Type of MHS d/s (see comments above)

! Local declarations:

INTEGER, PARAMETER :: LENREC = 4096  ! Fixed record length for MHS datasets
INTEGER, PARAMETER :: MAXRETRIES = 3 ! Max number of times to try opening MHS dataset before giving up
INTEGER, PARAMETER :: WAITTIME = 3   ! number of seconds to wait before retrying to open MHS dataset

INTEGER          ::  IOFLAG     ! STATUS FLAG FROM OPEN STATEMENT
INTEGER          ::  MHSCODE    ! RETURN CODE FROM "MHSIFFC"
INTEGER          ::  NTMSG(8)=0 ! Dummy argument for DREG, set to zero.
INTEGER          ::  NXTCODE    ! RETURN CODE FROM "NEXTMSG"
INTEGER          ::  retryOpen  ! Number of times tried to open MHS dataset

LOGICAL          ::  OPEN=.FALSE. ! .TRUE. IF A DATA SET IS BEING READ
LOGICAL          ::  FIRST=.TRUE. ! Initialisations first time called

CHARACTER(LEN=44) :: DSN        ! DATA SET NAME (RETURNED BY "MHSIFFC")
CHARACTER(LEN=8)  :: OLDOWNER='?' ! DATA SET OWNERSHIP FOR LAST CALL
CHARACTER(LEN=4)  :: STAT       ! STATUS FOR CALLS TO "MHSIFFC"
CHARACTER(LEN=8)  :: DUMMYBULL  ! Dummy variable for DREG
CHARACTER(LEN=4)  :: HLQ        ! 4th character of MHS dataset name
CHARACTER(LEN=1)  :: MHSOLD       ! MHS d/s type to search for
CHARACTER(LEN=1)  :: MHSNEW       ! MHS d/s type to rename to
!                                                                 SAVES
SAVE

IF (FIRST) THEN
  MHSOLD = 'P'
  MHSNEW = 'T'
  IF (MHSTYPE == 'OPER')THEN
    MHSOLD = 'R'
    MHSNEW = 'P'
  END IF
  FIRST = .FALSE.
END IF
!                                           Initialise output arguments
MSTART = 0
MLNGTH = 0
MSGCODE = 0 ! I.E. ASSUME OK UNTIL PROVED OTHERWISE

!-----------------------------------------------------------------------
!   CHECK THAT REQUESTED DATA SET OWNERSHIP IS THE SAME AS LAST TIME.
!   IF IT ISN'T AND A DATA SET IS CURRENTLY BEING READ, JUST OUTPUT
!   A WARNING MESSAGE, RENAME THE DATA SET AND RETURN. (A DATA SET
!   WITH THE NEW OWNERSHIP WILL NOT BE OPENED UNTIL THE NEXT CALL.)
!-----------------------------------------------------------------------
!                                            CHECK IF OWNER HAS CHANGED
IF (OWNER /= OLDOWNER) THEN
   IF (OPEN) THEN
      WRITE (6,'(T5,A,T15,2A)') 'FINDMHS:', 'NEW OWNERSHIP - ', &
            'OLD DATA SET STILL OPEN IS BEING CLOSED AND RENAMED.'
      MSGCODE = 1
   ELSE
      OLDOWNER = OWNER
   END IF
END IF

!-----------------------------------------------------------------------
!   LOOK FOR A NEW DATA SET IF THERE ISN'T ONE CURRENTLY BEING READ.
!   IF THERE ISN'T ONE WAITING, RETURN WITH RETURN CODE 2.
!   IF AN ERROR OCCURS IN MHSIFFC, SET RETURN CODE TO -1.
!   OTHERWISE CONTINUE PROCESSING WITH RETURN CODE SET TO 0.
!-----------------------------------------------------------------------
!                          LOOK FOR NEW DATA SET IF NONE CURRENTLY OPEN
IFLABEL1: &
IF (.NOT.OPEN) THEN
   STAT = 'GET'
   HLQ = MHSOLD
   CALL MHSIFFC (DSN,DELIMIT(STAT),DELIMIT(HLQ),DELIMIT(OWNER), MHSCODE)

!                                   NEW DATA SET FOUND: RETURN CODE > 0
IFLABEL2: &
   IF (MHSCODE > 0) THEN
      IOFLAG = 8  ! initialise IOFLAG for Do while loop
      retryOpen = 0 ! initialise retry variable

! Try up to 3 times to open the MHS dataset
      DO WHILE ( (IOFLAG /= 0) .AND. (retryOpen < MAXRETRIES))
         OPEN (NFT, FILE="//'"//DSN//"'", ACTION='READ', IOSTAT=IOFLAG, &
             ACCESS='DIRECT',RECL=LENREC)

! If IOFLAG indicates an error opening the file, increment retryOpen
! and wait to give time for cataloguing to complete
         IF (IOFLAG /= 0) then
            retryOpen = retryOpen + 1
            WRITE(6,*)'FINDMHS: MHS error retry ',retryOpen
            CALL SECSOUT(WAITTIME)
         END IF
      END DO

      OPEN = .TRUE.
!                                            I/O ERROR OPENING DATA SET
      IF (IOFLAG /= 0) THEN
         WRITE (6,'(T5,A,T15,3A)') 'FINDMHS:', 'I/O ERROR IN ', &
                  'OPEN STATEMENT - SKIPPING DATA SET ', DSN
         MSGCODE = 1
      ELSE
         CALL DREG (0, DUMMYBULL, DUMMYBULL,DUMMYBULL, DUMMYBULL, ' ', NTMSG, DSN)
      END IF
!                                 NO DATA SETS WAITING: RETURN CODE = 0
   ELSE IF (MHSCODE == 0) THEN
      MSTART = 0
      MLNGTH = 0
      MSGCODE = 2
      RETURN
!                                     ERROR IN MHSIFFC: RETURN CODE = -1
   ELSE
      MSGCODE = -1
   END IF IFLABEL2
END IF IFLABEL1
!                                       LOCATE NEXT MESSAGE IN DATA SET
IF (MSGCODE == 0) THEN
   CALL NEXTMSG (NFT, MESAGE, MSTART, MLNGTH, NXTCODE)
   IF (NXTCODE /= 0) MSGCODE = 1 ! TO GET RID OF DATA SET
END IF

!-----------------------------------------------------------------------
!   IF THERE ARE NO MORE MESSAGES LEFT TO PROCESS IN THIS DATA SET, IT
!   IS RENAMED FROM "MHSR..."  TO "MHSP..." AND CONTROL IS RETURNED TO
!   THE CALLING PROGRAM EVEN THOUGH NO MESSAGE DETAILS ARE RETURNED.
!   THIS IS TO ALLOW THE USER TO DO OTHER THINGS (E.G. CHECK FOR JOB
!   TERMINATION) BEFORE OPENING ANOTHER DATA SET. THE RETURN CODE IS 1
!   AND THE DDNAME AND DATA SET NAME ARE RETURNED IN THE BUFFER
!   (BYTES 1-8 AND 9-52 RESPECTIVELY).
!-----------------------------------------------------------------------

IFLABEL3: &
IF (MSGCODE /= 0) THEN ! FINISHED WITH DATA SET
   MSTART = 0
   MLNGTH = 0
!                                 STORE DATA SET DETAILS IN DATA BUFFER
   MESAGE = '        '//DSN
!                                         CLOSE AND RENAME THE DATA SET
   IF (OPEN) THEN
      CLOSE (NFT, STATUS='KEEP', IOSTAT=IOFLAG)
      STAT = 'REN'
      HLQ = MHSNEW
      CALL MHSIFFC (DELIMIT(DSN),DELIMIT(STAT),DELIMIT(HLQ),DELIMIT(OLDOWNER), MHSCODE)
      OPEN = .FALSE.
!                                      RENAMING ERROR: RETURN CODE = -2

      IF (MHSCODE /= 0) MSGCODE = -2
   END IF
!                                    TRY DELETING DATA SET IF MHS ERROR
   IF (MSGCODE < 0) THEN
      WRITE (6,'(T5,A,T15,2A,I3,2A)') 'FINDMHS:', 'MHS ERROR - ', &
               'RETURN CODE', MSGCODE, '.  WILL DELETE ', DSN
      STAT = 'DEL'
      HLQ = MHSOLD
      CALL MHSIFFC (DELIMIT(DSN),DELIMIT(STAT),DELIMIT(HLQ),DELIMIT(OWNER), MHSCODE)
      IF (MHSCODE == 0) MSGCODE = 1 ! GOT RID OF DATA SET
   END IF
END IF IFLABEL3

RETURN
CONTAINS
  FUNCTION DELIMIT(CH)
!
! PURPOSE    : To add a CHAR(0) delimiter to the end of a character
!              string for use with C functions.
! Input      : CH a string, with or without trailing blanks
! Output     : the string with trailing blanks removed and a CHAR(0) added
!
  CHARACTER(LEN=*)         :: CH
  CHARACTER(LEN=LEN(CH)+1) :: DELIMIT
  INTEGER                  :: I

  I=INDEX(CH,' ')
  IF (I > 0) THEN
    DELIMIT = CH(1:I-1)//CHAR(0)
  ELSE
    DELIMIT = CH//CHAR(0)
  END IF
  END FUNCTION DELIMIT

END SUBROUTINE FINDMHS
