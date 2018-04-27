      SUBROUTINE NEXTMSG (NFT, MESAGE, MSTART, MLNGTH, MSGCODE)

      IMPLICIT NONE

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
! PARAMETERS : NFT     (INPUT) UNIT NO. OF MESSAGE FILE TO BE READ.
!              MESAGE  (IN/OUT) 28K BUFFER TO HOLD MESSAGES.
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
! $Revision: 1$
! $Date: 30/01/2006 20:23:46$
! $Source: /home/us0400/mdb/op/lib/source/RCS/nextmsg.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:23:46    Sheila Needham  
! $
! Revision 2.1  2003/03/28 09:21:20  usmdb
! 2.1.  2 April 2003.  Brian Barwell.  Change 21/03.
! Changes to locations in GTS header associated with the change
! from TROPICS to FROST.
!
! Revision 2.0  2001/07/03  10:43:41  10:43:41  usmdb (MetDB account c/o usjh)
! Added copyright and modified header - S.Cox
!
! Revision 1.1  2000/03/10  10:05:37  10:05:37  usmdb (Generic MetDB account)
! Initial revision
!
! MARCH 1999:  ORIGINAL VERSION  (BRIAN BARWELL)
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

      INTEGER    LENGCH      ! LENGTH OF CHARACTER VARIABLE (28K)
      PARAMETER (LENGCH=7*4096) ! 7*4K = 28K - MUST AGREE WITH SIMILAR
!                                 STATEMENT IN "READ4K" ROUTINE
!                                                              INTEGERS
      INTEGER KEEPCH         ! FOR TEMPORARY STORAGE OF "NEXTCH"
      INTEGER LENGTH         ! LENGTH OF MESSAGE (LOCAL VARIABLE)
      INTEGER MLNGTH         ! LENGTH OF MESSAGE (RETURNED TO USER)
      INTEGER MSGCODE        ! RETURN CODE FROM "NEXTMSG"
      INTEGER MSHIFT         ! AMOUNT OF TEXT SHIFT IN BUFFER
      INTEGER MSTART, MEND   ! POINTERS TO START AND END  OF MESSAGE
      INTEGER NEXTCH         ! POINTER TO NEXT BYTE TO LOOK AT
      INTEGER NFT            ! UNIT NUMBER OF MESSAGE FILE
      INTEGER READCODE       ! RETURN CODE FROM "READ4K"
!                                                              LOGICALS
      LOGICAL FIRST      ! FLAG FOR FIRST CALL TO "NEXTMSG"
      LOGICAL HEADSET    ! .TRUE. IF 'HEAD' HAS BEEN SET
      LOGICAL LOST       ! .TRUE. IF DATA SYNCHRONISATION HAS BEEN LOST
      LOGICAL NEWDATA    ! FLAG FOR NEW DATA SET EXPECTED
!                                                            CHARACTERS
      CHARACTER*(LENGCH) MESAGE ! 28K BUFFER CONTAINING MESSAGE(S)
      CHARACTER SOH*4, ETX*3 ! INDICATORS FOR START & END OF MESSAGE
      CHARACTER HEAD*132     ! REVISION DETAILS
!                                                                 SAVES
      SAVE READCODE, NEWDATA, NEXTCH, FIRST, LOST, SOH, ETX
!                                                                  DATA
      DATA FIRST /.TRUE./    ! .TRUE. IF FIRST CALL TO SUBROUTINE
      DATA HEADSET /.FALSE./ ! 'HEAD' NOT YET SET
      DATA NEWDATA /.TRUE./  ! ALWAYS NEW DATA FIRST TIME
!
!                                                  REVISION INFORMATION
      IF (.NOT.HEADSET) THEN ! NOT YET DONE
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/nextmsg.f,v $
     &   '//'$Date: 30/01/2006 20:23:46$ $Revision: 1$'
         HEADSET = .TRUE.
      END IF
!
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
         NEXTCH = LENGCH + 1
         READCODE = 0
         LOST = .FALSE.
      END IF
!                               ASSUME THE WORST UNTIL PROVED OTHERWISE
!
      MSGCODE = 1       ! NO MORE MESSAGES IN CURRENT DATA SET
      MLNGTH = 0        ! LENGTH = 0 IF NO MESSAGE FOUND
      NEWDATA = .TRUE.  ! EXPECT A NEW DATA SET NEXT TIME
!
!-----------------------------------------------------------------------
!   LOCATE START OF NEXT MESSAGE BY LOOKING FOR START-OF-MESSAGE
!   SEQUENCE (HEXADECIMAL "010D0D0A") IN DATA SET, READING IN MORE
!   DATA IF NECESSARY.
!-----------------------------------------------------------------------
!                                                   FIND START SEQUENCE
    2 IF (NEXTCH.GT.LENGCH) THEN
         MSTART = 0
      ELSE
         MSTART = INDEX(MESAGE(NEXTCH:LENGCH),SOH)
      END IF
!                            IF NOT FOUND, READ MORE DATA AND TRY AGAIN
      IF (MSTART.EQ.0) THEN
!                                        RETURN IF NO MORE DATA TO READ
         IF (READCODE.EQ.1) THEN
            MSGCODE = 1
            RETURN
         END IF
!                           READ MORE DATA (BUT KEEP AT LEAST 13 BYTES)
!
         NEXTCH = MAX0(NEXTCH,LENGCH-12)                            !2.1
         CALL READ4K (NFT, MESAGE, NEXTCH, READCODE)
!
!                                I/O ERROR IN READ4K: SKIP REST OF DATA
         IF (READCODE.EQ.2) THEN
            MSGCODE = 2
            WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',
     &               'I/O ERROR IN READ4K - SKIPPING REST OF DATA SET'
            RETURN
         END IF
!                                    TRY AGAIN TO FIND START OF MESSAGE
         GO TO 2
      END IF
!                              FIND START OF 8-CHARACTER MESSAGE LENGTH
!
      MSTART = NEXTCH + MSTART - 11                                 !2.1
      NEXTCH = MSTART
!
!-----------------------------------------------------------------------
!   DECODE MESSAGE LENGTH AND ADD 10 TO INCLUDE THE 8-CHARACTER LENGTH
!   ITSELF AND THE 2-CHARACTER FORMAT IDENTIFIER.
!-----------------------------------------------------------------------
!
      CALL EBCDIC (8, MESAGE(MSTART:MSTART+7))                      !2.1
      READ (MESAGE(MSTART:MSTART+7),'(I8)') LENGTH                  !2.1
      LENGTH = LENGTH + 10                                          !2.1
      LENGTH = MAX0(LENGTH,14)                                      !2.1
!
!-----------------------------------------------------------------------
!   LOCATE END OF MESSAGE BY LOOKING FOR END-OF-MESSAGE SEQUENCE
!   (HEX "0D0A03") IN DATA SET, READING IN MORE DATA IF NECESSARY.
!-----------------------------------------------------------------------
!                                       COMPUTE EXPECTED END OF MESSAGE
    3 MEND = MSTART + LENGTH - 1
!                                READ MORE DATA IF THIS ISN'T IN BUFFER
      IF (MEND.GT.LENGCH) THEN
!                                  WARNING IF NO MORE DATA LEFT TO READ
         IF (READCODE.EQ.1) THEN
            IF (.NOT.LOST) WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',
     &          'END OF LAST MESSAGE NOT FOUND IN DATA SET'
            RETURN
         END IF
!                                        READ IN SOME MORE DATA RECORDS
         KEEPCH = NEXTCH
         CALL READ4K (NFT, MESAGE, NEXTCH, READCODE)
!
!                                I/O ERROR IN READ4K: SKIP REST OF DATA
         IF (READCODE.EQ.2) THEN
            MSGCODE = 2
            WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',
     &               'I/O ERROR IN READ4K - SKIPPING REST OF DATA SET'
            RETURN
         END IF
!                                COMPUTE DATA SHIFT AND UPDATE POINTERS
         MSHIFT = KEEPCH - NEXTCH
         MSTART = MSTART - MSHIFT
         MEND   = MEND   - MSHIFT
!
!                   IF END IS STILL NOT READ IN, MESSAGE IS TOO LONG OR
!                     LENGTH IS CORRUPT:  JUMP 8 BYTES TO SKIP START OF
!                   MESSAGE AND GO BACK TO FIND THE NEXT START SEQUENCE
!
         IF (MEND.GT.LENGCH) THEN
            IF (.NOT.LOST) WRITE (6,'(T5,A,T15,A,I9)') 'NEXTMSG:',
     &                  'MESSAGE LENGTH TOO GREAT - LENGTH =', LENGTH
            NEXTCH = NEXTCH + 14                                    !2.1
            LOST = .TRUE.
            GO TO 2
         END IF
      END IF
!               CHECK FOR END-OF-MESSAGE SEQUENCE IN EXPECTED LOCATION.
!         ("LOST" IS A FLAG SET WHEN SYNCHRONISATION OF MESSAGE TEXT IS
!        LOST. THIS STAYS SET UNTIL SYNCHRONISATION IS RECOVERED AND IS
!          USED TO SUPPRESS CERTAIN WARNING MESSAGES DURING THAT TIME.)
!
      IF (MEND.LE.2 .OR. MESAGE(MEND-2:MEND).NE.ETX) THEN
         IF (.NOT.LOST) THEN
            WRITE (6,'(T5,A,T15,A)') 'NEXTMSG:',
     &               'EXPECTED END-OF-MESSAGE INDICATOR NOT FOUND'
         END IF
!                        JUMP 14 BYTES TO SKIP START OF CURRENT MESSAGE
!                           AND GO BACK TO FIND THE NEXT START SEQUENCE
         LOST = .TRUE.
         NEXTCH = NEXTCH + 14                                       !2.1
         GO TO 2
      END IF
!                            ALL SEEMS O.K. - RETURN WITH RETURN CODE 0
      MLNGTH = LENGTH
      MSGCODE = 0
      NEWDATA = .FALSE.
      NEXTCH = MEND + 1
!
      RETURN
      END
