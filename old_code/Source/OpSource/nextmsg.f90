SUBROUTINE NEXTMSG (NFT, MESAGE, MSTART, MLNGTH, MSGCODE)
!
!
!-----------------------------------------------------------------------
!
! SUBROUTINE : NEXTMSG
!
! PURPOSE    : TO FIND THE NEXT MESSAGE IN THE DATA SET ON UNIT
!              "NFT", PUT IT IN THE USER'S BUFFER AND RETURN A
!              POINTER TO THE START OF THE MESSAGE AND THE MESSAGE
!              LENGTH. IF THERE ARE NO MORE MESSAGES TO BE READ,
!              THE RETURN CODE IS SET TO 1 AND "MSTART" AND
!              "MLNGTH" ARE SET TO ZERO.
!                THE DATA SETS TO BE READ MUST BE IN THE FORMAT
!              DESCRIBED IN GPCS TECH. NOTE 40 (SEE THE SECTION
!              'MESSAGE STORAGE' AND APPENDIX A).
!
! CALLED BY  : ANYTHING WHICH READS GTS DATA FTP'D FROM FROST.      !2.1
!
! USAGE      : CALL NEXTMSG (NFT, MESAGE, MSTART, MLNGTH, MSGCODE)
!
! ARGUMENTS : NFT     (INPUT) UNIT NO. OF MESSAGE FILE TO BE READ.
!              MESAGE  (IN/OUT) BUFFER TO HOLD MESSAGES.
!              MSTART  (OUTPUT) START BYTE OF NEXT MESSAGE.
!              MLNGTH  (OUTPUT) LENGTH OF NEXT MESSAGE IN BYTES.
!              MSGCODE (OUTPUT) RETURN CODE - CODED AS FOLLOWS:
!                     0 - MESSAGE SUCCESSFULLY FOUND,
!                     1 - END OF DATA SET - NO MESSAGE RETURNED,
!                     2 - I/O ERROR READING DATA SET.
!
!              THE MESSAGE LENGTH "MLNGTH" IS RETURNED AS ZERO IF NO
!              MORE MESSAGES ARE FOUND (I.E. "MSGCODE" > 0).
!
! CALLS      : EBCDIC, READ4K.                                      !2.1
!
! REVISION INFO :
!
! $Revision:
! $Date:
! $Source:  $
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         27/05/2011 10:40:52    Brian Barwell   LENGCH
!        added to SAVE statement
!  8    MetDB_Refresh 1.7         24/05/2011 15:21:04    Brian Barwell   Get
!       LENGCH from LEN(MESAGE).
!       
!  7    MetDB_Refresh 1.6         09/03/2011 13:32:46    Sheila Needham
!       Initialise NEWDATA
!  6    MetDB_Refresh 1.5         22/12/2010 13:20:25    Sheila Needham  Fix
!       typo
!  5    MetDB_Refresh 1.4         22/12/2010 13:15:17    Sheila Needham  Change
!        MESAGE to assumed length
!  4    MetDB_Refresh 1.3         22/12/2010 11:46:42    Sheila Needham
!       Updated following review
!  3    MetDB_Refresh 1.2         16/12/2010 10:19:56    Richard Weedon
!       updated for port
!  2    MetDB_Refresh 1.1         14/12/2010 13:47:31    Richard Weedon  
!  1    MetDB_Refresh 1.0         07/12/2010 14:30:45    Richard Weedon
!       initial version
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
! Interfaces
USE read4k_mod
USE ebcdic_mod
!
IMPLICIT NONE
!
! Arguments

INTEGER,         INTENT(IN)    :: NFT      ! UNIT NUMBER OF MESSAGE FILE
CHARACTER(LEN=*),INTENT(INOUT) :: MESAGE
INTEGER,         INTENT(OUT)   :: MSTART   ! POINTER TO START OF MESS
INTEGER,         INTENT(OUT)   :: MLNGTH   ! LEN OF MESSAGE
INTEGER,         INTENT(OUT)   :: MSGCODE  ! RETURN CODE FROM "NEXTMSG"

! Local Variables

CHARACTER(LEN=3) ::  ETX           ! IND FOR START & END OF MESSAGE
LOGICAL          ::  FIRST =.TRUE. ! FLAG FOR FIRST CALL TO "NEXTMSG"
INTEGER          ::  KEEPCH        ! FOR TEMP STORAGE OF "NEXTCH"
INTEGER          ::  LENGCH        ! LENGTH OF "MESAGE" STRING
INTEGER          ::  LENGTH        ! LEN OF MESSAGE (LOCAL VARIABLE)
LOGICAL          ::  LOST          ! .TRUE. IF DATA SYNCH HAS BEEN LOST
INTEGER          ::  MSHIFT        ! AMOUNT OF TEXT SHIFT IN BUFFER
INTEGER          ::  MEND          ! POINTERS TO START AND END  OF MESS
LOGICAL          ::  NEWDATA=.TRUE.! FLAG FOR NEW DATA SET EXPECTED
INTEGER          ::  NEXTCH        ! POINTER TO NEXT BYTE TO LOOK AT
INTEGER          ::  READCODE      ! RETURN CODE FROM "READ4K"
CHARACTER(LEN=4) ::  SOH           ! IND FOR START & END OF MESSAGE

!                                                                 SAVES
SAVE READCODE, NEWDATA, LENGCH, NEXTCH, FIRST, LOST, SOH, ETX

!-----------------------------------------------------------------------
!   INITIALISATIONS DONE ON (1) FIRST CALL ONLY OR (2) NEW DATA SET.
!-----------------------------------------------------------------------
!                                               ASCII CHARACTER STRINGS
IF (FIRST) THEN
   SOH = CHAR(01)//CHAR(13)//CHAR(13)//CHAR(10) ! SOH-CR-CR-LF
   ETX = CHAR(13)//CHAR(10)//CHAR(03)           ! CR-LF-ETX
   FIRST = .FALSE.     ! TO PREVENT RE-INITIALISATIONS
END IF
!                        RESET SOME VARIABLES IF READING A NEW DATA SET
IF (NEWDATA) THEN
   LENGCH = LEN(MESAGE)
   NEXTCH = LENGCH + 1
   READCODE = 0
   LOST = .FALSE.
END IF
!                               ASSUME THE WORST UNTIL PROVED OTHERWISE

MSGCODE = 1       ! NO MORE MESSAGES IN CURRENT DATA SET
MLNGTH = 0        ! LENGTH = 0 IF NO MESSAGE FOUND
NEWDATA = .TRUE.  ! EXPECT A NEW DATA SET NEXT TIME

!-----------------------------------------------------------------------
!   LOCATE START OF NEXT MESSAGE BY LOOKING FOR START-OF-MESSAGE
!   SEQUENCE (HEXADECIMAL "010D0D0A") IN DATA SET, READING IN MORE
!   DATA IF NECESSARY.
!-----------------------------------------------------------------------

2  CONTINUE                                           !      FIND START SEQUENCE
   IF (NEXTCH > LENGCH) THEN
   MSTART = 0
ELSE
   MSTART = INDEX(MESAGE(NEXTCH:LENGCH),SOH)
END IF
!                            IF NOT FOUND, READ MORE DATA AND TRY AGAIN
ifconstr1 : &
IF (MSTART == 0) THEN
!                                        RETURN IF NO MORE DATA TO READ
   IF (READCODE == 1) THEN
      MSGCODE = 1
      RETURN
   END IF
!                           READ MORE DATA (BUT KEEP AT LEAST 13 BYTES)

   NEXTCH = MAX0(NEXTCH,LENGCH-12)
   CALL READ4K (NFT, MESAGE, NEXTCH, READCODE)

!                                I/O ERROR IN READ4K: SKIP REST OF DATA
   IF (READCODE == 2) THEN
      MSGCODE = 2
      WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',&
      'I/O ERROR IN READ4K - SKIPPING REST OF DATA SET'
      RETURN
   END IF
!                                    TRY AGAIN TO FIND START OF MESSAGE
   GO TO 2
END IF ifconstr1
!                              FIND START OF 8-CHARACTER MESSAGE LENGTH
MSTART = NEXTCH + MSTART - 11
NEXTCH = MSTART

!-----------------------------------------------------------------------
!   DECODE MESSAGE LENGTH AND ADD 10 TO INCLUDE THE 8-CHARACTER LENGTH
!   ITSELF AND THE 2-CHARACTER FORMAT IDENTIFIER.
!-----------------------------------------------------------------------

CALL EBCDIC (8, MESAGE(MSTART:MSTART+7))
READ (MESAGE(MSTART:MSTART+7),'(I8)') LENGTH
LENGTH = LENGTH + 10
LENGTH = MAX0(LENGTH,14)

!-----------------------------------------------------------------------
!   LOCATE END OF MESSAGE BY LOOKING FOR END-OF-MESSAGE SEQUENCE
!   (HEX "0D0A03") IN DATA SET, READING IN MORE DATA IF NECESSARY.
!-----------------------------------------------------------------------
!                                       COMPUTE EXPECTED END OF MESSAGE
3  CONTINUE
MEND = MSTART + LENGTH - 1
!                                READ MORE DATA IF THIS ISN'T IN BUFFER
ifconstr2 : &
IF (MEND > LENGCH) THEN
!                                  WARNING IF NO MORE DATA LEFT TO READ
   IF (READCODE == 1) THEN
      IF (.NOT.LOST) WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',&
      'END OF LAST MESSAGE NOT FOUND IN DATA SET'
      RETURN
   END IF
!                                        READ IN SOME MORE DATA RECORDS
   KEEPCH = NEXTCH
   CALL READ4K (NFT, MESAGE, NEXTCH, READCODE)

!                                I/O ERROR IN READ4K: SKIP REST OF DATA
   IF (READCODE == 2) THEN
      MSGCODE = 2
      WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',&
      'I/O ERROR IN READ4K - SKIPPING REST OF DATA SET'
      RETURN
   END IF
!                                COMPUTE DATA SHIFT AND UPDATE POINTERS
   MSHIFT = KEEPCH - NEXTCH
   MSTART = MSTART - MSHIFT
   MEND   = MEND   - MSHIFT

!                   IF END IS STILL NOT READ IN, MESSAGE IS TOO LONG OR
!                     LENGTH IS CORRUPT:  JUMP 8 BYTES TO SKIP START OF
!                   MESSAGE AND GO BACK TO FIND THE NEXT START SEQUENCE

   IF (MEND > LENGCH) THEN
      IF (.NOT.LOST) WRITE (6,'(T5,A,T15,A,I9)') 'NEXTMSG:',&
          'MESSAGE TOO LONG TO STORE - LENGTH =', LENGTH
      NEXTCH = NEXTCH + 14
      LOST = .TRUE.
      GO TO 2
   END IF
END IF ifconstr2
!               CHECK FOR END-OF-MESSAGE SEQUENCE IN EXPECTED LOCATION.
!         ("LOST" IS A FLAG SET WHEN SYNCHRONISATION OF MESSAGE TEXT IS
!        LOST. THIS STAYS SET UNTIL SYNCHRONISATION IS RECOVERED AND IS
!          USED TO SUPPRESS CERTAIN WARNING MESSAGES DURING THAT TIME.)

IF (MEND <= 2 .OR. MESAGE(MEND-2:MEND) /= ETX) THEN
   IF (.NOT.LOST) THEN
      WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:', &
      'EXPECTED END-OF-MESSAGE INDICATOR NOT FOUND'
   END IF
!                        JUMP 14 BYTES TO SKIP START OF CURRENT MESSAGE
!                           AND GO BACK TO FIND THE NEXT START SEQUENCE
   LOST = .TRUE.
   NEXTCH = NEXTCH + 14
   GO TO 2
END IF
!                            ALL SEEMS O.K. - RETURN WITH RETURN CODE 0
MLNGTH = LENGTH
MSGCODE = 0
NEWDATA = .FALSE.
NEXTCH = MEND + 1
!
RETURN
END SUBROUTINE NEXTMSG
