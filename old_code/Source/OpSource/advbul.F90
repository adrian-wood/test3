SUBROUTINE ADVBUL(BULEND,TTAAII,CCCC,YYGGGG,OCOR,FT,BULL)

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
! CALLED BY     : MDBSTOR
!
! CALLS         : DATIM, OBHOUR, TRPSTO
!
! ARGUMENTS     : (1) BULEND  - End of buletin  (before NNNN)
!                 (2) TTAAII  - Bulletin identifier
!                 (3) CCCC    - Originating centre
!                 (4) YYGGGG  - Bulletin day and hour
!                 (5) OCOR    - Corrected report flag
!                 (6) FT      - FT number
!                 (7) BULL    - Bulletin to be stored.
!
! REVISION INFO :
!
! $Workfile: advbul.F90$ $Folder: OpSource$
! $Revision: 3$ $Date: 08/04/2011 11:14:12$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         08/04/2011 11:14:12    Brian Barwell   Ensure
!        all characters of IDENT are initialised.
!  2    MetDB_Refresh 1.1         14/01/2011 11:21:03    Alison Weir     Ported
!        to f95 - MDBSTOR batch12
!  1    MetDB_Refresh 1.0         05/01/2011 17:04:51    Alison Weir
!       MDBSTOR batch 12 initial F77 version
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

! Use statements:

USE datim_mod
USE obhour_mod
USE trpsto_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER,          INTENT(IN)    ::  BULEND  !A01 End of bulletin.
CHARACTER(LEN=*), INTENT(IN)    ::  TTAAII  !A02 Bulletin identifier
CHARACTER(LEN=*), INTENT(IN)    ::  CCCC    !A03 Originating centre.
CHARACTER(LEN=*), INTENT(IN)    ::  YYGGGG  !A04 Bulletin time.
LOGICAL,          INTENT(IN)    ::  OCOR    !A05 Correction flag.
INTEGER,          INTENT(IN)    ::  FT      !A06 File allocation number.
CHARACTER(LEN=*), INTENT(IN)    ::  BULL    !A07 Report data (bulletin).

! Local declarations:

CHARACTER(LEN=9)  ::       IDENT       ! Report identifier.
CHARACTER(LEN=23) ::       ENTRY       ! Index entry

INTEGER           ::       CORNUM      ! 2 if COR, 0 if not
INTEGER           ::       NOW(9)      ! current date/time
INTEGER           ::       OBDATE(5)   ! bulletin date/time
INTEGER           ::       TOR(5)      ! time of receipt (=NOW)
INTEGER           ::       RC          ! OBHOUR return code
INTEGER           ::       I
INTEGER           ::       START       ! Start of TROPADV report

! Call the system clock to get the current date/time.

CALL DATIM(NOW)
DO I=1,5
  TOR(I)=NOW(9-I)
END DO

! Take the bulletin day/time & complete the date using OBHOUR:
! OBHOUR called with the same day/hour for report & bulletin just
! sets year/month & rejects data too far ahead of current time.
! RC=4 if bulletin more than an hour ahead of current time

READ (YYGGGG(1:6),'(3I2)') OBDATE(3),OBDATE(4),OBDATE(5)
CALL OBHOUR(OBDATE,OBDATE(3),OBDATE(4),'TROP',0,RC)
IF (RC > 0) THEN
  PRINT *,'Tropical advisory more than 3 hours ahead of'
  PRINT *,'current time: time must be wrong',OBDATE
  RETURN
END IF

! Set-up index entry.  Set top bit of hour byte if COR.
! ENTRY(3:11) goes in trailer & is replaced by IDENT in index.

ENTRY(:)=' '

IF (OCOR) THEN
  ENTRY(1:1) = CHAR(OBDATE(4)+128)
  CORNUM=2
ELSE
  ENTRY(1:1) = CHAR(OBDATE(4))
  CORNUM=0
END IF

ENTRY(1:1)=CHAR(OBDATE(4))//CHAR(0)   !Hour
ENTRY(2:2)=CHAR(OBDATE(5))            !Minutes
ENTRY(3:6)=TTAAII(1:4)                !TTAA to go in trailer
ENTRY(7:7)=CHAR(CORNUM)               !Correction Number
ENTRY(8:9)=TTAAII(5:6)                !II (CCCC is identifier!)
ENTRY(12:12)=CHAR(1)                  !No. of Obs
ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0) ! no lat/long

! Now store the report, with the CCCC as identifier.

IDENT=CCCC                            !CCCC in 9-character string

#if defined (MVS)
START=35
#else
START=21       !- no TROPICS header of 14 characters
#endif

CALL TRPSTO (OBDATE, ENTRY, BULL(START:BULEND), FT, 27998, IDENT, TOR)

RETURN
END SUBROUTINE ADVBUL
