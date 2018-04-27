      SUBROUTINE FINDFTP
     &           (NFT, OWNER, MESAGE, MSTART, MLNGTH, DSNAME, MSGCODE)
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
!              the message in the user's 28K buffer (MESAGE), and also
!              the length of the message (MLNGTH).
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
! USAGE:       CALL FINDMHS
!               (NFT, OWNER, MESAGE, MSTART, MLNGTH, DSNAME, MSGCODE)
!
! PARAMETERS:  ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!              NFT      (I)  Unit no. of message file to be read.
!              OWNER    (I)  (CHARACTER - up to 8 bytes) MHS data set
!                            ownership - e.g. 'BUF1'.
!              MESAGE  (I/O) 28K character buffer to hold messages.
!              MSTART   (O)  Start byte of next message.
!              MLNGTH   (O)  Length of next message in bytes.
!              DSNAME   (O)  (C*44) Name of data set currently open.
!              MSGCODE  (O)  Return code - coded as follows:
!
!                    -2 - MHSIFF error while renaming data set.
!                    -1 - MHSIFF error locating new data set.
!                     0 - Message successfully found.
!                     1 - End of data set - no message returned.
!                     2 - No more data sets waiting at present.
!
!              Message length and start pointer are returned as zero
!              unless return code = 0.
!
!              Apart from the addition of DSNAME, the argument list
!              is identical to that for FINDMHS (used for GTS data.)
!
! CALLED BY:   BUFRDAT
!
! CALLS:       DREG, ICHAR3, MHSIFF, READ512
!
! HISTORY:     Original version written by Brian Barwell, November.2000.
!
! REVISION INFO:
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:27$
! $Source: /home/us0400/mdb/op/lib/source/RCS/findftp.F,v $
!
! CHANGE RECORD:
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:27    Sheila Needham  
! $
! Revision 2.1  2001/08/09 09:26:58  usmdb
! 2.1.  20 August 2001.  Brian Barwell.  Change 106/01.
! Delete data set without renaming after an MHSIFF 'get' error.
!
! Revision 2.0  2001/06/06  10:14:56  10:14:56  usmdb (Generic MetDB account)
! Initial version
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!                                                             Parameter
!
      INTEGER    LENGCH        ! Length of character variable (28K)
      PARAMETER (LENGCH=28672) ! = 28K - must agree with similar
!                                  statement in READ512
!                                                              Integers
      INTEGER IEDTN      ! BUFR edition number
      INTEGER IOFLAG     ! Status flag from OPEN statement
      INTEGER KEEPCH     ! For temporary storage of NEXTCH
      INTEGER LEVEL      ! Processing level (described below)
      INTEGER MEND       ! Pointer to end of BUFR message
      INTEGER MHSCODE    ! Return code from MHSIFF
      INTEGER MHSDIAG(8) ! Diagnostic information from MHSIFF
      INTEGER MLNGTH     ! Length of BUFR message (bytes)
      INTEGER MSGCODE    ! Return code (see comments above)
      INTEGER MSHIFT     ! Amount of text shift in buffer
      INTEGER MSTART     ! Pointer to start of BUFR message
      INTEGER NEXTCH     ! Pointer to next byte to look at
      INTEGER NFT        ! Unit number of message file
      INTEGER READCODE   ! Return code from READ512
!                                                              Logicals
      LOGICAL FIRST      ! .TRUE. if first call to subroutine
!                                                            Characters
      CHARACTER*4 BUFR       ! 'BUFR' in ASCII characters
      CHARACTER*4 SEVENS     ! '7777' in ASCII characters
      CHARACTER*(*) OWNER    ! Mhs ownership of required dataset
      CHARACTER*8 DDN        ! Data set DDNAME (returned by MHSIFF)
      CHARACTER*44 DSN       ! Data set name (returned by MHSIFF)
      CHARACTER*44 DSNAME    ! Data set name (returned to calling pgm.)
      CHARACTER*(LENGCH) MESAGE ! 28K buffer containing message(s)
      CHARACTER*8 OLDOWNER   ! Data set ownership for last call
      CHARACTER HEAD*132     ! Revision details
!                                                     External function
!
      INTEGER ICHAR3         ! C*3 to I*4 conversion routine
!
!                                                       Save statements
      SAVE BUFR, DDN, DSN, FIRST, LEVEL
      SAVE NEXTCH, OLDOWNER, READCODE, SEVENS
!                                                   Data initialisation
      DATA FIRST /.TRUE./, LEVEL /0/
      DATA DSN /' '/, OLDOWNER /'?'/  ! No data set or owner yet
      DATA READCODE /0/
!
!=======================================================================
!  REVISION INFORMATION AND INITIALISATIONS  (FIRST CALL ONLY)
!=======================================================================
!
      IF (FIRST) THEN  ! First call
!                                                  Revision information
        HEAD='
     &  $Source: /home/us0400/mdb/op/lib/source/RCS/findftp.F,v $
     &  '//'$Date: 30/01/2006 20:22:27$ $Revision: 1$'
        FIRST = .FALSE.
!                                                       Initialisations
!
        BUFR   = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)  ! 'BUFR'
        SEVENS = CHAR(55) // CHAR(55) // CHAR(55) // CHAR(55)  ! '7777'
      END IF
!
!=======================================================================
!  CHECK THAT REQUESTED MHS DATA SET OWNERSHIP IS THE SAME AS LAST TIME.
!  IF IT ISN'T AND A DATA SET IS CURRENTLY BEING READ, JUST OUTPUT A
!  WARNING MESSAGE, RENAME THE DATA SET AND RETURN. (IN THIS CASE A DATA
!  SET WITH THE NEW OWNERSHIP WILL NOT BE OPENED UNTIL THE NEXT CALL.)
!=======================================================================
!
      IF (OWNER.NE.OLDOWNER) THEN  ! Owner has changed
        IF (LEVEL.GT.0) THEN
          WRITE (6,'(T5,A,T15,2A)') 'FINDFTP:', 'New ownership - ',
     &            'old data set still open is being closed and renamed.'
          LEVEL = 4
        ELSE
          OLDOWNER = OWNER
        END IF
      END IF
!
!=======================================================================
!  SEARCH FOR NEW BULLETIN.
!  KEEP PROCESSING UNTIL LEVEL 3 OR HIGHER IS REACHED.
!=======================================================================
!  Processing status is described by 'LEVEL' as follows:
!
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
!
      DO WHILE (LEVEL.LT.3)
!
!-----------------------------------------------------------------------
!  LEVEL 0:  NO DATA SET FOUND YET.
!            USE 'MHSIFF' TO FIND NEXT DATA SET NAME, IF ANY.
!-----------------------------------------------------------------------
!
        IF (LEVEL.EQ.0) THEN
          CALL MHSIFF (DSN, DDN, 'G  N', OWNER, MHSDIAG)
          MHSCODE = MHSDIAG(1)
!                                       MHSCODE = 0: New data set found
          IF (MHSCODE.EQ.0) THEN
            OPEN (NFT, FILE=DDN, STATUS='OLD', ACTION='READ',
     &            IOSTAT=IOFLAG)
            NEXTCH = LENGCH + 1
            LEVEL = 1
            READCODE = 0
!                                            I/O error opening data set
            IF (IOFLAG.NE.0) THEN
              WRITE (6,'(T5,A,T15,3A)') 'FINDFTP:', 'I/O error in ',
     &                 'open statement - skipping data set ', DSN
              LEVEL = 4
            ELSE
              CALL DREG (0, ' ', ' ', ' ', ' ', ' ', 0, DSN)
            END IF
!                                     MHSCODE = 4: no data sets waiting
          ELSE IF (MHSCODE.EQ.4) THEN
            LEVEL = 7
!                                  MHSCODE not 1 or 4:  error in MHSIFF
          ELSE
            MSGCODE = -1  ! MHSIFF 'get' error
            LEVEL = 6                                               !2.1
          END IF
        END IF
        DSNAME = DSN
!
!-----------------------------------------------------------------------
!  LEVEL 1:  DATA SET FOUND AND OPENED.
!            FIND 'BUFR' AT START OF NEXT MESSAGE, READING IN MORE DATA
!            IF NEEDED.  (LOOKING ONLY AS FAR AS BYTE LENGCH-4 ENSURES
!            THAT ALL OF BUFR SECTION 0 WILL BE IN STORE.)
!-----------------------------------------------------------------------
!
        IF (LEVEL.EQ.1) THEN
!                                                       Look for 'BUFR'
          IF (NEXTCH.LT.LENGCH-6) THEN
            MSTART = INDEX(MESAGE(NEXTCH:LENGCH-4),BUFR)
          ELSE
            MSTART = 0
          END IF
!                                     If BUFR not found, read more data
          IF (MSTART.EQ.0) THEN
!                                        Set LEVEL to 4 if no more data
            IF (READCODE.EQ.1) THEN
              LEVEL = 4
!                                 Otherwise read some more data records
            ELSE
              NEXTCH = MAX0(NEXTCH,LENGCH-7)
              CALL READ512 (NFT, MESAGE, NEXTCH, READCODE)
!
!                                    Skip rest of data set if I/O error
              IF (READCODE.EQ.2) THEN
                LEVEL = 4
              END IF
            END IF
!                                                 Find start of message
          ELSE
            MSTART = NEXTCH + MSTART - 1
            NEXTCH = MSTART
            LEVEL = 2  ! 'BUFR' found
          END IF
        END IF
!
!-----------------------------------------------------------------------
!  LEVEL 2:  'BUFR' FOUND AT START OF MESSAGE.
!            FIND '7777' AT END OF MESSAGE, READING IN MORE DATA IF
!            NEEDED. SKIP MESSAGE IF NOT FOUND IN EXPECTED LOCATION.
!-----------------------------------------------------------------------
!
        IF (LEVEL.EQ.2) THEN
!                                                   BUFR edition number
          IEDTN = ICHAR(MESAGE(MSTART+7:MSTART+7))
!
          IF (IEDTN.GE.2) THEN
            MLNGTH = ICHAR3(MESAGE(MSTART+4:MSTART+6))
!
!                                 (Note: Editions <2 not yet supported)
          ELSE
            WRITE (6,'(T5,A,T15,A,I5)') 'FINDFTP:',
     &               'Unable to read bulletins with BUFR edition', IEDTN
            NEXTCH = NEXTCH + 4  ! Skip 'BUFR'
            LEVEL = 1
          END IF
!
!-----------------------------------------------------------------------
!   LOOK FOR '7777' AT END OF MESSAGE, READING IN MORE DATA IF
!   NECESSARY.
!-----------------------------------------------------------------------
!                                      Expected end of message location
          MEND = MSTART + MLNGTH - 1
!                                       Read more data if not in buffer
          IF (MEND.GT.LENGCH) THEN
!                                  Warning if no more data left to read
            IF (READCODE.EQ.1) THEN
              WRITE (6,'(T5,A,T15,A)') 'FINDFTP:',
     &                  'End of last message not found in data set'
              LEVEL = 4
!                                        Read in some more data records
            ELSE
              KEEPCH = NEXTCH
              CALL READ512 (NFT, MESAGE, NEXTCH, READCODE)
!
!                                    Skip rest of data set if I/O error
              IF (READCODE.EQ.2) THEN
                LEVEL = 4
!                                Compute data shift and update pointers
              ELSE
                MSHIFT = KEEPCH - NEXTCH
                MSTART = MSTART - MSHIFT
                MEND   = MEND   - MSHIFT
!
!                      If end is still not read in, message is too long
!                     or length is corrupt. Go back for another message
!
                IF (MEND.GT.LENGCH) THEN
                  WRITE (6,'(T5,A,T15,A,I9)') 'FINDFTP:',
     &                    'Message length too great - length =', MLNGTH
                  NEXTCH = MSTART + 4  ! Skip 'BUFR'
                  LEVEL = 1
               END IF
              END IF
            END IF
          END IF
!                                  Check '7777' is in expected location
          IF (LEVEL.EQ.2) THEN
            IF (MEND.LE.3 .OR. MESAGE(MEND-3:MEND).NE.SEVENS) THEN
              WRITE (6,'(T5,A,T15,A)') 'FINDFTP:',
     &                 '"7777" not found at end of BUFR message'
              NEXTCH = MSTART + 4  ! Skip 'BUFR'
              LEVEL = 1
!                                           Bulletin found successfully
            ELSE
              MSGCODE = 0
              NEXTCH = MEND + 1
              LEVEL = 1  ! Reset for next call
              RETURN
            END IF
          END IF
        END IF
      END DO
!
!=======================================================================
!  IF THERE ARE NO MORE MESSAGES LEFT TO PROCESS IN THIS DATA SET, IT
!  IS RENAMED FROM "MHSR..."  TO "MHSP..." AND CONTROL IS RETURNED TO
!  THE CALLING PROGRAM EVEN THOUGH NO MESSAGE DETAILS ARE RETURNED.
!  THIS IS TO ALLOW THE USER TO DO OTHER THINGS (E.G. CHECK FOR JOB
!  TERMINATION) BEFORE OPENING ANOTHER DATA SET. THE RETURN CODE IS 1
!  AND THE DDNAME AND DATA SET NAME ARE RETURNED IN THE BUFFER
!  ('MESAGE' BYTES 1-8 AND 9-52 RESPECTIVELY).
!=======================================================================
!
      MSTART = 0
      MLNGTH = 0
!                                      LEVEL 7: No data sets, so return
      IF (LEVEL.EQ.7) THEN
        MSGCODE = 2
        LEVEL = 0  ! Reset for next call
        RETURN
      END IF
!                                       LEVEL 4: Close current data set
      IF (LEVEL.LE.4) THEN
        CLOSE (NFT, STATUS='KEEP', IOSTAT=IOFLAG)
!
!                                   Print message if I/O error occurred
!
        IF (READCODE.EQ.2) WRITE (6,'(T5,A,T15,A)') 'FINDFTP:',
     &      'I/O error in READ512 - skipping rest of data set'
      END IF
!                                          LEVEL 5: Rename MHS data set
      IF (LEVEL.LE.5) THEN
        CALL MHSIFF (DSN, DDN, 'R  N', OLDOWNER, MHSDIAG)
        MHSCODE = MHSDIAG(1)
!                                          Check for MHS renaming error
        IF (MHSCODE.NE.0) MSGCODE = -2
      END IF
!                                      MHS error: try deleting data set
      IF (MSGCODE.LT.0) THEN
        WRITE (6,'(T5,A,T15,2A,I3,2A)') 'FINDFTP:', 'MHS error - ',
     &           'return code', MSGCODE, '.  Will delete ', DSN
        CALL MHSIFF (DSN, DDN, 'D  N', OWNER, MHSDIAG)
        IF (MHSDIAG(1).NE.0) RETURN
      END IF
!                                 Store data set details in data buffer
      MESAGE = DDN // DSN                                           !2.1
!                              Set return code and new LEVEL; clear DSN
      MSGCODE = 1
      LEVEL = 0  ! Reset for next call
      DSN = ' '  ! Forget DSN                                       !2.1
!                                                                Return
      RETURN
      END
