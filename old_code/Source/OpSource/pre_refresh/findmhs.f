      SUBROUTINE FINDMHS (NFT, OWNER, MESAGE, MSTART, MLNGTH, MSGCODE)
!
      IMPLICIT NONE
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
!                      (NFT, OWNER, MESAGE, MSTART, MLNGTH, MSGCODE)
!
! PARAMETERS : NFT     (INPUT) UNIT NO. OF MESSAGE FILE TO BE READ.
!              OWNER   (INPUT) DATA SET OWNERSHIP - E.G. 'MDB1'.
!                              (CHARACTER - UP TO 8 BYTES)
!              MESAGE  (IN/OUT) 28K CHARACTER STRING TO HOLD MESSAGES.
!              MSTART  (OUTPUT) START BYTE OF NEXT MESSAGE.
!              MLNGTH  (OUTPUT) LENGTH OF NEXT MESSAGE IN BYTES.
!              MSGCODE (OUTPUT) RETURN CODE - CODED AS FOLLOWS:
!                    -2 - MHSIFF ERROR WHILE RENAMING DATA SET,
!                    -1 - MHSIFF ERROR LOCATING NEW DATA SET,
!                     0 - MESSAGE SUCCESSFULLY FOUND,
!                     1 - END OF DATA SET - NO MESSAGE RETURNED,
!                     2 - NO MORE DATA SETS WAITING AT PRESENT.
!
!              MESSAGE LENGTH AND START POINTER ARE RETURNED AS ZERO
!              UNLESS RETURN CODE = 0.
!
! CALLS      : MHSIFF, NEXTMSG.
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 30/01/2006 20:22:29$
! $Source: /home/us0400/mdb/op/lib/source/RCS/findmhs.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         30/01/2006 20:22:29    Sheila Needham  
! $
! Revision 2.0  2001/05/31 14:16:38  usmdb
! Added copyright and modified header - S.Cox
! Changed last argument in MHSIFF to 8-element array - B.Barwell.
!
! Revision 1.1  2000/03/10  10:04:25  10:04:25  usmdb (Generic MetDB account)
! Initial revision
!
! MARCH 1999:  ORIGINAL VERSION  (BRIAN BARWELL)
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2001 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
!
      INTEGER    LENGCH      ! LENGTH OF CHARACTER VARIABLE (28K)
      PARAMETER (LENGCH=7*4096) ! 7*4K = 28K - MUST AGREE WITH SIMILAR
!                                 STATEMENT IN "READ4K" ROUTINE
!                                                              INTEGERS
      INTEGER IOFLAG         ! STATUS FLAG FROM OPEN STATEMENT
      INTEGER MHSCODE        ! RETURN CODE FROM "MHSIFF"
      INTEGER MHSDIAG(8)     ! DIAGNOSTIC INFORMATION FROM MHSIFF   !2.0
      INTEGER MLNGTH         ! LENGTH OF MESSAGE (BYTES)
      INTEGER MSGCODE        ! RETURN CODE FROM "FINDMHS"
      INTEGER MSTART         ! POINTER TO START OF MESSAGE
      INTEGER NFT            ! UNIT NUMBER OF MESSAGE FILE
      INTEGER NXTCODE        ! RETURN CODE FROM "NEXTMSG"
!                                                              LOGICALS
      LOGICAL HEADSET    ! .TRUE. IF 'HEAD' HAS BEEN SET
      LOGICAL OPEN       ! .TRUE. IF A DATA SET IS BEING READ
!                                                            CHARACTERS
      CHARACTER*(*) OWNER    ! OWNERSHIP OF REQUIRED DATASET
      CHARACTER*8 DDN        ! DATA SET DDNAME (RETURNED BY "MHSIFF")
      CHARACTER*44 DSN       ! DATA SET NAME (RETURNED BY "MHSIFF")
      CHARACTER*(LENGCH) MESAGE ! 28K BUFFER CONTAINING MESSAGE(S)
      CHARACTER*8 OLDOWNER   ! DATA SET OWNERSHIP FOR LAST CALL
      CHARACTER*6 STAT       ! STATUS FOR CALLS TO "MHSIFF"
      CHARACTER HEAD*132     ! REVISION DETAILS
!                                                                 SAVES
      SAVE OPEN, DDN, DSN, OLDOWNER
!                                                                  DATA
      DATA HEADSET /.FALSE./ ! 'HEAD' NOT YET SET
      DATA OLDOWNER /'?'/    ! NO PREVIOUS OWNER YET
      DATA OPEN /.FALSE./    ! NO DATA SET OPEN YET
!                                                  REVISION INFORMATION
      IF (.NOT.HEADSET) THEN ! NOT YET DONE
         HEAD='
     &   $Source: /home/us0400/mdb/op/lib/source/RCS/findmhs.F,v $
     &   '//'$Date: 30/01/2006 20:22:29$ $Revision: 1$'
         HEADSET = .TRUE.
      END IF
!
      MSGCODE = 0 ! I.E. ASSUME OK UNTIL PROVED OTHERWISE
!
!-----------------------------------------------------------------------
!   CHECK THAT REQUESTED DATA SET OWNERSHIP IS THE SAME AS LAST TIME.
!   IF IT ISN'T AND A DATA SET IS CURRENTLY BEING READ, JUST OUTPUT
!   A WARNING MESSAGE, RENAME THE DATA SET AND RETURN. (A DATA SET
!   WITH THE NEW OWNERSHIP WILL NOT BE OPENED UNTIL THE NEXT CALL.)
!-----------------------------------------------------------------------
!                                            CHECK IF OWNER HAS CHANGED
      IF (OWNER.NE.OLDOWNER) THEN
         IF (OPEN) THEN
            WRITE (6,'(T5,A,T15,2A)') 'FINDMHS:', 'NEW OWNERSHIP - ',
     &            'OLD DATA SET STILL OPEN IS BEING CLOSED AND RENAMED.'
            MSGCODE = 1
         ELSE
            OLDOWNER = OWNER
         END IF
      END IF
!
!-----------------------------------------------------------------------
!   LOOK FOR A NEW DATA SET IF THERE ISN'T ONE CURRENTLY BEING READ.
!   IF THERE ISN'T ONE WAITING, RETURN WITH RETURN CODE 2.
!   IF AN ERROR OCCURS IN MHSIFF, SET RETURN CODE TO -1.
!   OTHERWISE CONTINUE PROCESSING WITH RETURN CODE SET TO 0.
!-----------------------------------------------------------------------
!                          LOOK FOR NEW DATA SET IF NONE CURRENTLY OPEN
      IF (.NOT.OPEN) THEN
         STAT = 'G  N'
         CALL MHSIFF (DSN, DDN, STAT, OWNER, MHSDIAG)               !2.0
         MHSCODE = MHSDIAG(1)                                       !2.0
!                                   NEW DATA SET FOUND: RETURN CODE = 0
         IF (MHSCODE.EQ.0) THEN
            OPEN (NFT, FILE='/'//DSN, STATUS='OLD', ACTION='READ',
     &            IOSTAT=IOFLAG)
            OPEN = .TRUE.
!                                            I/O ERROR OPENING DATA SET
            IF (IOFLAG.NE.0) THEN
               WRITE (6,'(T5,A,T15,3A)') 'FINDMHS:', 'I/O ERROR IN ',
     &                  'OPEN STATEMENT - SKIPPING DATA SET ', DSN
               MSGCODE = 1
            ELSE
               CALL DREG (0, ' ', ' ', ' ', ' ', ' ', 0, DSN)
            END IF
!                                 NO DATA SETS WAITING: RETURN CODE = 2
         ELSE IF (MHSCODE.EQ.4) THEN
            MSTART = 0
            MLNGTH = 0
            MSGCODE = 2
            RETURN
!                                     ERROR IN MHSIFF: RETURN CODE = -1
         ELSE
            MSGCODE = -1
         END IF
      END IF
!                                       LOCATE NEXT MESSAGE IN DATA SET
      IF (MSGCODE.EQ.0) THEN
         CALL NEXTMSG (NFT, MESAGE, MSTART, MLNGTH, NXTCODE)
         IF (NXTCODE.NE.0) MSGCODE = 1 ! TO GET RID OF DATA SET
      END IF
!
!-----------------------------------------------------------------------
!   IF THERE ARE NO MORE MESSAGES LEFT TO PROCESS IN THIS DATA SET, IT
!   IS RENAMED FROM "MHSR..."  TO "MHSP..." AND CONTROL IS RETURNED TO
!   THE CALLING PROGRAM EVEN THOUGH NO MESSAGE DETAILS ARE RETURNED.
!   THIS IS TO ALLOW THE USER TO DO OTHER THINGS (E.G. CHECK FOR JOB
!   TERMINATION) BEFORE OPENING ANOTHER DATA SET. THE RETURN CODE IS 1
!   AND THE DDNAME AND DATA SET NAME ARE RETURNED IN THE BUFFER
!   (BYTES 1-8 AND 9-52 RESPECTIVELY).
!-----------------------------------------------------------------------
!
      IF (MSGCODE.NE.0) THEN ! FINISHED WITH DATA SET
         MSTART = 0
         MLNGTH = 0
!                                 STORE DATA SET DETAILS IN DATA BUFFER
         MESAGE = DDN // DSN
!                                         CLOSE AND RENAME THE DATA SET
         IF (OPEN) THEN
            CLOSE (NFT, STATUS='KEEP', IOSTAT=IOFLAG)
            STAT = 'R  N'
            CALL MHSIFF (DSN, DDN, STAT, OLDOWNER, MHSDIAG)         !2.0
            MHSCODE = MHSDIAG(1)                                    !2.0
            OPEN = .FALSE.
!                                      RENAMING ERROR: RETURN CODE = -2
!
            IF (MHSCODE.NE.0) MSGCODE = -2
         END IF
!                                    TRY DELETING DATA SET IF MHS ERROR
         IF (MSGCODE.LT.0) THEN
            WRITE (6,'(T5,A,T15,2A,I3,2A)') 'FINDMHS:', 'MHS ERROR - ',
     &               'RETURN CODE', MSGCODE, '.  WILL DELETE ', DSN
            STAT = 'D  N'
            CALL MHSIFF (DSN, DDN, STAT, OWNER, MHSDIAG)            !2.0
            IF (MHSDIAG(1).EQ.0) MSGCODE = 1 ! GOT RID OF DATA SET  !2.0
         END IF
      END IF
!
      RETURN
      END
