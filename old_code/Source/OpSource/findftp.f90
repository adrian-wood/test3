SUBROUTINE FINDFTP &
           (NFT, OWNER, MESAGE, MSTART, MLNGTH, DSNAME, MSGCODE, MHSTYPE)
!
!-----------------------------------------------------------------------
!
! SUBROUTINE:  FINDFTP
!
! PURPOSE:     To find the next message in a data set of BUFR messages
!              received via FTP.
!
! DESCRIPTION: FINDFTP finds the next message in a data set of BUFR
!              messages and returns a pointer (MSTART) to the start of
!              the message in the user's buffer (MESAGE), and also the
!              length of the message (MLNGTH).
!
!              A non-zero return code is set if there's an MHS problem,
!              the end of a data set is reached or if there are no data
!              sets waiting:- see the parameter list below for details.
!
!              The data sets to be read must contain BUFR bulletins
!              using BUFR edition 2 or higher and must have fixed
!              length records with a record length of 512.
!
!              FINDFTP does the same for non-GTS data that FINDMHS
!              does for GTS data sets received via TROPICS.
!
!              No message is returned when a data set is closed but in
!              this case the DDNAME and data set name are returned in
!              bytes 1-8 and 9-52 of the user buffer. (This is for
!              compatibility with FINDMHS.)
!
! USAGE:       CALL FINDFTP
!               (NFT, OWNER, MESAGE, MSTART, MLNGTH, DSNAME, MSGCODE, MHSTYPE)
!
! ARGUMENTS:  ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!              NFT      (I)  Unit no. of message file to be read.
!              OWNER    (I)  (CHARACTER - up to 8 bytes) MHS data set
!                            ownership - e.g. 'BUF1'.
!              MESAGE  (I/O) Character buffer to hold messages.
!              MSTART   (O)  Start byte of next message.
!              MLNGTH   (O)  Length of next message in bytes.
!              DSNAME   (O)  (C*44) Name of data set currently open.
!              MSGCODE  (O)  Return code - coded as follows:
!
!                    -2 - MHSIFFC error while renaming data set.
!                    -1 - MHSIFFC error locating new data set.
!                     0 - Message successfully found.
!                     1 - End of data set - no message returned.
!                     2 - No more data sets waiting at present.
!              MHSTYPE  (I) C*4 'TEST' for MHSP->MHST
!                               'OPER' for MHSR->MHSP
!
!              Message length and start pointer are returned as zero
!              unless return code = 0.
!
!              Apart from the addition of DSNAME, the argument list
!              is identical to that for FINDMHS (used for GTS data.)
!
! CALLED BY:   BUFRDAT
!
! CALLS:       DREG, ICHAR3, MHSIFFC, READ512
!
! HISTORY:     Original version written by Brian Barwell, November.2000.
!
! REVISION INFO:
!
! $Workfile: findftp.f90$ $Folder: OpSource$
! $Revision: 11$ $Date: 24/05/2011 16:15:59$
!
! CHANGE RECORD:
!
! $Log:
!  11   MetDB_Refresh 1.10        24/05/2011 16:15:59    Brian Barwell   Remove
!        spurious comment text left in last time.
!  10   MetDB_Refresh 1.9         24/05/2011 15:17:17    Brian Barwell   Tidy
!       up code.
!  9    MetDB_Refresh 1.8         18/05/2011 09:45:08    Stan Kellett    If MHS
!        catelogue error and cant open file then retry up to 3 times as could
!       be the MHS routines have not completed.
!  8    MetDB_Refresh 1.7         15/04/2011 14:55:43    Sheila Needham  Extra
!       argument to choose between MHSP and MHSR datasets to be processed.
!  7    MetDB_Refresh 1.6         22/03/2011 14:53:30    Sheila Needham
!       Changes following review
!  6    MetDB_Refresh 1.5         16/03/2011 09:48:39    Sheila Needham
!       Delimit strings with CHAR(0) for C function
!  5    MetDB_Refresh 1.4         15/03/2011 10:44:05    Sheila Needham
!       Changes for new MHSIFFC routine
!  4    MetDB_Refresh 1.3         08/03/2011 11:40:44    Sheila Needham
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
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
! Use statements:

USE ichar3_mod    !Function
USE dreg_mod
USE read512_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,           INTENT(IN)    :: NFT     !a01 Unit number of message file
CHARACTER(LEN=*),  INTENT(IN)    :: OWNER   !a02 Mhs ownership of required dataset
CHARACTER(LEN=*),  INTENT(INOUT) :: MESAGE  !a03 Buffer for data read from MHS data set
INTEGER,           INTENT(OUT)   :: MSTART  !a04 Pointer to start of BUFR message
INTEGER,           INTENT(OUT)   :: MLNGTH  !a05 Length of BUFR message (bytes)
CHARACTER(LEN=44), INTENT(OUT)   :: DSNAME  !a06 Data set name (returned to calling pgm.)
INTEGER,           INTENT(OUT)   :: MSGCODE !a07 Return code (see comments above)
CHARACTER(LEN=4),  INTENT(IN)    :: MHSTYPE !a08 Type of MHS d/s (see comments above)

! Local declarations:

INTEGER, PARAMETER :: LENREC=512     ! Fixed record length for FTP'd datasets
INTEGER, PARAMETER :: MAXRETRIES = 3 ! Max number of times to try opening MHS dataset before giving up
INTEGER, PARAMETER :: WAITTIME = 3   ! number of seconds to wait before retrying to open MHS dataset
INTEGER          ::  IEDTN      ! BUFR edition number
INTEGER          ::  IOFLAG     ! Status flag from OPEN statement
INTEGER          ::  KEEPCH     ! For temporary storage of NEXTCH
INTEGER          ::  LENGCH     ! Length of character variable MESAGE
INTEGER          ::  LEVEL=0    ! Processing level (described below)
INTEGER          ::  MEND       ! Pointer to end of BUFR message
INTEGER          ::  MHSCODE    ! Return code from MHSIFFC
INTEGER          ::  MSHIFT     ! Amount of text shift in buffer
INTEGER          ::  NEXTCH     ! Pointer to next byte to look at
INTEGER          ::  NTMSG(8)=0 ! Dummy argument for DREG, set to zero.
INTEGER          ::  READCODE=0 ! Return code from READ512
INTEGER          ::  retryOpen  ! Number of times tried to open MHS dataset

LOGICAL          ::  FIRST=.TRUE. ! .TRUE. if first call to subroutine

CHARACTER(LEN=4)  :: BUFR         ! 'BUFR' in ASCII characters
CHARACTER(LEN=4)  :: SEVENS       ! '7777' in ASCII characters
CHARACTER(LEN=44) :: DSN=' '      ! Data set name (returned by MHSIFFC)
CHARACTER(LEN=8)  :: OLDOWNER='?' ! Data set ownership for last call
CHARACTER(LEN=4)  :: HLQ          ! 4th character in MHS dataset name
CHARACTER(LEN=1)  :: MHSOLD       ! MHS d/s type to search for
CHARACTER(LEN=1)  :: MHSNEW       ! MHS d/s type to rename to
CHARACTER(LEN=4)  :: STAT         ! MHS request type
CHARACTER(LEN=8)  :: DUMMYBULL    ! Dummy variable for DREG

!                                                       Save statements
SAVE

!=======================================================================
!  REVISION INFORMATION AND INITIALISATIONS  (FIRST CALL ONLY)
!=======================================================================

IF (FIRST) THEN  ! First call
!                                                       Initialisations

  BUFR   = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)  ! 'BUFR'
  SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55)  ! '7777'
  MHSOLD = 'P'
  MHSNEW = 'T'
  IF (MHSTYPE == 'OPER')THEN
    MHSOLD = 'R'
    MHSNEW = 'P'
  END IF
  FIRST = .FALSE.
END IF

! Initialise output arguments
  DSNAME = ' '
  MSTART = 0
  MLNGTH = 0

!=======================================================================
!  CHECK THAT REQUESTED MHS DATA SET OWNERSHIP IS THE SAME AS LAST TIME.
!  IF IT ISN'T AND A DATA SET IS CURRENTLY BEING READ, JUST OUTPUT A
!  WARNING MESSAGE, RENAME THE DATA SET AND RETURN. (IN THIS CASE A DATA
!  SET WITH THE NEW OWNERSHIP WILL NOT BE OPENED UNTIL THE NEXT CALL.)
!=======================================================================

IF (OWNER /= OLDOWNER) THEN  ! Owner has changed
  IF (LEVEL > 0) THEN
    WRITE (6,'(T5,A,T15,2A)') 'FINDFTP:', 'New ownership - ', &
            'old data set still open is being closed and renamed.'
    LEVEL = 4
  ELSE
    OLDOWNER = OWNER
  END IF
END IF

!=======================================================================
!  SEARCH FOR NEW BULLETIN.
!  KEEP PROCESSING UNTIL LEVEL 3 OR HIGHER IS REACHED.
!=======================================================================
!  Processing status is described by 'LEVEL' as follows:

!       LEVEL = 0:  No data set open.
!       LEVEL = 1:  Data set with BUFR bulletins found and opened.
!       LEVEL = 2:  'BUFR' found at start of next bulletin.
!       LEVEL = 3:  '7777' found at end of next bulletin.
!       LEVEL = 4:  Data set still open but finished with.
!       LEVEL = 5:  Data set closed but not renamed.
!       LEVEL = 6:  Data set closed and renamed.
!       LEVEL = 7:  No data sets waiting to be processed.
!
!  Processing continues until level 3 or higher is reached.
!  Before returning, 'LEVEL' is reset to a suitable value (= 0 or 1)
!  for the next call to FINDFTP.
!-----------------------------------------------------------------------
! Get length of MESAGE
LENGCH = LEN(MESAGE)

DOLABEL1: &
DO WHILE (LEVEL < 3)

!-----------------------------------------------------------------------
!  LEVEL 0:  NO DATA SET FOUND YET.
!            USE 'MHSIFFC' TO FIND NEXT DATA SET NAME, IF ANY.
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (LEVEL == 0) THEN
    STAT='GET'
    HLQ=MHSOLD
  CALL MHSIFFC (DSN,DELIMIT(STAT),DELIMIT(HLQ), DELIMIT(OWNER), MHSCODE)

!                                       MHSCODE > 0: New data set found
IFLABEL2: &
    IF (MHSCODE > 0) THEN
      IOFLAG = 8    ! initialise IOFLAG for Do while loop
      retryOpen = 0 ! initialise number of retries

! Try upto 3 times to try and open the MHS dataset name
      DO WHILE ( (IOFLAG /= 0) .AND. (retryOpen < MAXRETRIES))
        OPEN (NFT, FILE="//'"//DSN//"'", ACTION='READ', IOSTAT=IOFLAG, &
            ACCESS='DIRECT',RECL=LENREC)

! If IOFLAG indicates an error opening the file, increment retryOpen and 
! wait to give time for cataloguing to complete.

        IF (IOFLAG /= 0) then
          retryOpen = retryOpen + 1
          WRITE(6,*)'FINDFTP: MHS error retry ',retryOpen,' ',DSN
          CALL SECSOUT(WAITTIME)
        END IF
      END DO

      NEXTCH = LENGCH + 1
      LEVEL = 1
      READCODE = 0
!                                            I/O error opening data set
      IF (IOFLAG /= 0) THEN
        WRITE (6,'(T5,A,T15,3A)') 'FINDFTP:', 'I/O error in ', &
                 'open statement - skipping data set ', DSN
        LEVEL = 4
      ELSE
        CALL DREG (0, DUMMYBULL, DUMMYBULL, DUMMYBULL, DUMMYBULL, ' ', NTMSG, DSN)
      END IF
!                                     MHSCODE = 0: no data sets waiting
    ELSE IF (MHSCODE == 0) THEN
      LEVEL = 7
!                                  MHSCODE not >= 0:  error in MHSIFFC
    ELSE
      MSGCODE = -1  ! MHSIFFC 'get' error
      LEVEL = 6
    END IF IFLABEL2
  END IF IFLABEL1
  DSNAME = DSN

!-----------------------------------------------------------------------
!  LEVEL 1:  DATA SET FOUND AND OPENED.
!            FIND 'BUFR' AT START OF NEXT MESSAGE, READING IN MORE DATA
!            IF NEEDED.  (LOOKING ONLY AS FAR AS BYTE LENGCH-4 ENSURES
!            THAT ALL OF BUFR SECTION 0 WILL BE IN STORE.)
!-----------------------------------------------------------------------

IFLABEL3: &
  IF (LEVEL == 1) THEN
!                                                       Look for 'BUFR'
    IF (NEXTCH < LENGCH-6) THEN
      MSTART = INDEX(MESAGE(NEXTCH:LENGCH-4),BUFR)
    ELSE
      MSTART = 0
    END IF
!                                     If BUFR not found, read more data
IFLABEL4: &
    IF (MSTART == 0) THEN
!                                        Set LEVEL to 4 if no more data
IFLABEL5: &
      IF (READCODE == 1) THEN
        LEVEL = 4
!                                 Otherwise read some more data records
      ELSE
        NEXTCH = MAX0(NEXTCH,LENGCH-7)
        CALL READ512 (NFT, MESAGE, NEXTCH, READCODE)

!                                    Skip rest of data set if I/O error
        IF (READCODE == 2) THEN
          LEVEL = 4
        END IF
      END IF IFLABEL5
!                                                 Find start of message
    ELSE
      MSTART = NEXTCH + MSTART - 1
      NEXTCH = MSTART
      LEVEL = 2  ! 'BUFR' found
    END IF IFLABEL4
  END IF IFLABEL3

!-----------------------------------------------------------------------
!  LEVEL 2:  'BUFR' FOUND AT START OF MESSAGE.
!            FIND '7777' AT END OF MESSAGE, READING IN MORE DATA IF
!            NEEDED. SKIP MESSAGE IF NOT FOUND IN EXPECTED LOCATION.
!-----------------------------------------------------------------------

IFLABEL6: &
  IF (LEVEL == 2) THEN
!                                                   BUFR edition number
    IEDTN = ICHAR(MESAGE(MSTART+7:MSTART+7))

    IF (IEDTN >= 2) THEN
      MLNGTH = ICHAR3(MESAGE(MSTART+4:MSTART+6))

!                                 (Note: Editions <2 not yet supported)
    ELSE
      WRITE (6,'(T5,A,T15,A,I5)') 'FINDFTP:', &
               'Unable to read bulletins with BUFR edition', IEDTN
      NEXTCH = NEXTCH + 4  ! Skip 'BUFR'
      LEVEL = 1
    END IF

!-----------------------------------------------------------------------
!   LOOK FOR '7777' AT END OF MESSAGE, READING IN MORE DATA IF
!   NECESSARY.
!-----------------------------------------------------------------------
!                                      Expected end of message location
    MEND = MSTART + MLNGTH - 1
!                                       Read more data if not in buffer
IFLABEL7: &
    IF (MEND > LENGCH) THEN
!                                  Warning if no more data left to read
IFLABEL8: &
      IF (READCODE == 1) THEN
        WRITE (6,'(T5,A,T15,A)') 'FINDFTP:', &
                  'End of last message not found in data set'
        LEVEL = 4
!                                        Read in some more data records
      ELSE
        KEEPCH = NEXTCH
        CALL READ512 (NFT, MESAGE, NEXTCH, READCODE)

!                                    Skip rest of data set if I/O error
IFLABEL9: &
        IF (READCODE == 2) THEN
          LEVEL = 4
!                                Compute data shift and update pointers
        ELSE
          MSHIFT = KEEPCH - NEXTCH
          MSTART = MSTART - MSHIFT
          MEND   = MEND   - MSHIFT

!                      If end is still not read in, message is too long
!                     or length is corrupt. Go back for another message

          IF (MEND > LENGCH) THEN
            WRITE (6,'(T5,A,T15,A,I9)') 'FINDFTP:', &
                    'Message length too great - length =', MLNGTH
            NEXTCH = MSTART + 4  ! Skip 'BUFR'
            LEVEL = 1
          END IF
        END IF IFLABEL9
      END IF IFLABEL8
    END IF IFLABEL7
!                                  Check '7777' is in expected location
IFLABEL10: &
    IF (LEVEL == 2) THEN
IFLABEL11: &
      IF (MEND <= 3 .OR. MESAGE(MEND-3:MEND) /= SEVENS) THEN
        WRITE (6,'(T5,A,T15,A)') 'FINDFTP:', &
                 '"7777" not found at end of BUFR message'
        NEXTCH = MSTART + 4  ! Skip 'BUFR'
        LEVEL = 1
!                                           Bulletin found successfully
      ELSE
        MSGCODE = 0
        NEXTCH = MEND + 1
        LEVEL = 1  ! Reset for next call
        RETURN
      END IF IFLABEL11
    END IF IFLABEL10
  END IF IFLABEL6
END DO DOLABEL1

!=======================================================================
!  IF THERE ARE NO MORE MESSAGES LEFT TO PROCESS IN THIS DATA SET, IT
!  IS RENAMED FROM "MHSR..."  TO "MHSP..." AND CONTROL IS RETURNED TO
!  THE CALLING PROGRAM EVEN THOUGH NO MESSAGE DETAILS ARE RETURNED.
!  THIS IS TO ALLOW THE USER TO DO OTHER THINGS (E.G. CHECK FOR JOB
!  TERMINATION) BEFORE OPENING ANOTHER DATA SET. THE RETURN CODE IS 1
!  AND THE DDNAME AND DATA SET NAME ARE RETURNED IN THE BUFFER
!  ('MESAGE' BYTES 1-8 AND 9-52 RESPECTIVELY).
!=======================================================================

MSTART = 0
MLNGTH = 0
!                                      LEVEL 7: No data sets, so return
IF (LEVEL == 7) THEN
  MSGCODE = 2
  LEVEL = 0  ! Reset for next call
  RETURN
END IF
!                                       LEVEL 4: Close current data set
IF (LEVEL <= 4) THEN
  CLOSE (NFT, STATUS='KEEP', IOSTAT=IOFLAG)

!                                   Print message if I/O error occurred

  IF (READCODE == 2) WRITE (6,'(T5,A,T15,A)') 'FINDFTP:', &
      'I/O error in READ512 - skipping rest of data set'
END IF
!                                          LEVEL 5: Rename MHS data set
IF (LEVEL <= 5) THEN
  STAT ='REN'
  HLQ = MHSNEW
  CALL MHSIFFC (DELIMIT(DSN),DELIMIT(STAT),DELIMIT(HLQ), DELIMIT(OLDOWNER), MHSCODE)

!                                          Check for MHS renaming error
  IF (MHSCODE /= 0) MSGCODE = -2
END IF
!                                      MHS error: try deleting data set
IF (MSGCODE < 0) THEN
  WRITE (6,'(T5,A,T15,2A,I3,2A)') 'FINDFTP:', 'MHS error - ', &
           'return code', MSGCODE, '.  Will delete ', DSN
  STAT='DEL'
  HLQ= MHSOLD
  CALL MHSIFFC (DELIMIT(DSN),DELIMIT(STAT),DELIMIT(HLQ), DELIMIT(OWNER), MHSCODE)
  IF (MHSCODE /= 0) RETURN
END IF
!                                 Store data set details in data buffer
MESAGE = '        '// DSN
!                              Set return code and new LEVEL; clear DSN
MSGCODE = 1
LEVEL = 0  ! Reset for next call
DSN = ' '  ! Forget DSN
!                                                                Return
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
END SUBROUTINE FINDFTP
