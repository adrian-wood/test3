      SUBROUTINE ADVBUL(BULEND,TTAAII,CCCC,YYGGGG,OCOR,FT,BULL)     !2.0

!-----------------------------------------------------------------------
!
! PROGRAM       : ADVBUL
!
! PURPOSE       : To store tropical advisories in the MDB.
!
! DESCRIPTION   : Tropical advisories are received at infrequent
!                 intervals, depending on the amount of storm activity,
!                 and are stored in characters as received.
!                 The originating centre is used as identifier.
!                 There is no check for multiple reports in a single
!                 bulletin: each bulletin is assumed to contain a
!                 single advisory.
!
! CALLED BY     : MDBSTOR                                           !2.0
!
! CALLS         : DATIM, OBHOUR, TRPSTO                             !1.5
!
! PARAMETERS    : (1) BULEND  - End of buletin  (before NNNN)
!                 (2) TTAAII  - Bulletin identifier
!                 (3) CCCC    - Originating centre
!                 (4) YYGGGG  - Bulletin day and hour
!                 (5) OCOR    - Corrected report flag
!                 (6) FT      - FT number
!                 (7) BULL    - Bulletin to be stored.
!
! REVISION INFO :
!
! $Workfile: advbul.F$ $Folder: pre_refresh$
! $Revision: 2$ $Date: 31/08/2010 15:08:20$
!
! CHANGE RECORD :
!
! $Log:
!  2    Met_DB_Project 1.1         31/08/2010 15:08:20    Brian Barwell
!       Change warning message for future times from 1 hour to 3 hours.
!  1    Met_DB_Project 1.0         30/01/2006 20:20:33    Sheila Needham  
! $
! Revision 2.1  2002/02/08 10:37:44  usmdb
! For non-IBM mainframe platforms, initialise START so it is past
! the end of the bulletin header as for IBM mainframe code - S.Cox
!
! Revision 2.0  2001/05/31  13:27:17  13:27:17  usmdb (Generic MetDB account)
! Removed unused argument POINT. Added copyright and modified
! header - S.Cox
!
! Revision 1.6  98/11/23  16:42:58  16:42:58  usmdb (Generic MetDB account)
! 25 Nov 1998: Correct date handling (change of month) & accept
! bulletin received in hour before header time - C Long
!
! note: revision 1.5 in error - S.Cox
!
! Revision 1.4  98/11/12  08:52:03  08:52:03  usmdb (Generic MDB account
! 16/11/1998 Trap reports that have a timestamp in the future and don't
! store these reports so that we don't reject many more. Jon Lewthwaite
!
! Revision 1.3  1998/06/11 15:31:51  usmdb
! Change routine to use TAFREP for storage
!
! Revision 1.2  1997/07/31 09:03:01  uspm
! First revision for COSMOS
!
! Revision 1.1  1997/07/03 13:28:37  uspm
! Initial revision
!
! 02/02/98 : Change routine to use TAFREP for storage                !A
!
! 27/09/96 : Remove call to BULLED to allow all punctuation in report
!            to remain intact.
!
! 23/09/96 : Introduced
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

      IMPLICIT NONE

! Declare Character

      CHARACTER*(*)      BULL        ! Report data (bulletin).
      CHARACTER*(*)      CCCC        ! Originating centre.
      CHARACTER*(*)      TTAAII      ! Bulletin identifier.
      CHARACTER*(*)      YYGGGG      ! Bulletin time.

      CHARACTER*80       HEAD        ! Revision information          !2
      CHARACTER*9        IDENT       ! Report identifier.
      CHARACTER*23       ENTRY       ! Index entry                   !A

! Declare Integer

      INTEGER            CORNUM      ! 2 if COR, 0 if not
      INTEGER            BULEND      ! End of bulletin.
      INTEGER            FT          ! File allocation number.

      INTEGER            NOW(9)      ! current date/time
      INTEGER            OBDATE(5)   ! bulletin date/time
      INTEGER            TOR(5)      ! time of receipt (=NOW)
      INTEGER            RC          ! OBHOUR return code           !1.6
      INTEGER            I
      INTEGER            START       ! Start of TROPADV report      !2.1

! Declare Logical

      LOGICAL            OCOR        ! Flag set if bulletin is a

! Initialise variables

      HEAD = '$Workfile: advbul.F$ ' //
     &       '$Revision: 2$ $Date: 31/08/2010 15:08:20$'

! Call the system clock to get the current date/time.

      CALL DATIM(NOW)
      DO I=1,5
        TOR(I)=NOW(9-I)
      ENDDO

! Take the bulletin day/time & complete the date using OBHOUR:     !1.6
! OBHOUR called with the same day/hour for report & bulletin just  !1.6
! sets year/month & rejects data too far ahead of current time.    !1.6
! RC=4 if bulletin more than an hour ahead of current time         !1.6

      READ (YYGGGG(1:6),'(3I2)') OBDATE(3),OBDATE(4),OBDATE(5)
      CALL OBHOUR(OBDATE,OBDATE(3),OBDATE(4),'TROP',0,RC)          !1.6
      IF (RC.GT.0) THEN                                            !1.6
        PRINT *,'Tropical advisory more than 3 hours ahead of'     !2
        PRINT *,'current time: time must be wrong',OBDATE          !1.6
        RETURN                                                     !1.6
      ENDIF                                                        !1.6

! Set-up index entry.  Set top bit of hour byte if COR.
! ENTRY(3:11) goes in trailer & is replaced by IDENT in index.

      ENTRY(:)=' '

      IF (OCOR) THEN
        ENTRY(1:1) = CHAR(OBDATE(4)+128)
        CORNUM=2
      ELSE
        ENTRY(1:1) = CHAR(OBDATE(4))
        CORNUM=0
      ENDIF

      ENTRY(1:1)=CHAR(OBDATE(4))//CHAR(0)   !Hour
      ENTRY(2:2)=CHAR(OBDATE(5))            !Minutes
      ENTRY(3:6)=TTAAII(1:4)                !TTAA to go in trailer
      ENTRY(7:7)=CHAR(CORNUM)               !Correction Number
      ENTRY(8:9)=TTAAII(5:6)                !II (CCCC is identifier!)
      ENTRY(12:12)=CHAR(1)                  !No. of Obs
      ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0) ! no lat/long

! Now store the report, with the CCCC as identifier.

      IDENT(1:4)=CCCC                       !CCCC in 9-character string

#if defined (MVS)
      START=35                                                      !2.1
#else
      START=21       !- no TROPICS header of 14 characters          !2.1
#endif

      CALL TRPSTO(OBDATE,ENTRY,BULL(START:BULEND),FT,27998,
     &            IDENT,TOR)                                        !2.1

      RETURN
      END
