SUBROUTINE TBUSBUL(POINT,BEND,TTAAII,YYGGGG,NFT,OERR,BULL)

!-----------------------------------------------------------------------
!
! PROGRAM       : TBUSBUL
!
! PURPOSE       : To pass a TBUS message to the TAF/METAR storage
!                 program, preparing a pseudo-station-identifier and
!                 starting an Index entry.
!
! DESCRIPTION   : Unlike the majority of text messages from GTS, the
!                 TBUS include "=" not as end-of-report indicators but
!                 as simple character-group separators; we may not,
!                 therefore, use use all the usual storage routines as
!                 "black boxes".  I have combined the functions of
!                 xxxBUL and TAFIND routines here as all the testing
!                 for end-of-report and station identifiers may be
!                 removed, - leaving nothing for TBUSBUL to do other
!                 than call TBUSIND - - this would have been the only
!                 data type handled by SDBSYN to require a separate
!                 xxxIND routine !
!
!                 In most data types,reports are extracted from bulle-
!                 tins and SYNOPT has set the start pointer in the
!                 bulletin past the header.  I this case, the header
!                 itself is an important part of the data to be stored
!                 so reset the pointer there.
!
! CALLED BY     : MDBSTOR
!
! CALLS         : TAFREP, DATIM
!
! ARGUMENTS     : (1) POINT    Pointer to where to start in bulletin
!                 (2) BEND     Number of last character in bulletin
!                 (3) TTAAII   TT type of bulletin (TBUSnn)
!                 (4) YYGGGG   Time of bulletin
!                 (5) NFT      FT number for TBUS storage
!                 (6) OERR     Logical set if report splitting error
!                 (7) BULL     Report text
!
! REVISION INFO :
!
!
! $Workfile: tbusbul.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 16/12/2010 13:31:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         16/12/2010 13:31:35    Alison Weir
!       Changes following review - comment in USE
!  1    MetDB_Refresh 1.0         14/12/2010 16:21:09    Rosemary Lavery
!       Initial Port
! $
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

USE TAFREP_MOD
USE DATIM_MOD

IMPLICIT NONE

! Interface Arguments

INTEGER, INTENT(INOUT)            :: POINT    ! (A1)
INTEGER, INTENT(INOUT)            :: BEND     ! (A2)
CHARACTER (LEN=6), INTENT(IN)     :: TTAAII   ! (A3)
CHARACTER (LEN=6), INTENT(IN)     :: YYGGGG   ! (A4)
INTEGER, INTENT(INOUT)            :: NFT      ! (A5)
LOGICAL, INTENT(OUT)              :: OERR     ! (A6)
CHARACTER (LEN=*), INTENT(INOUT)  :: BULL     ! (A7)

! Local Parameters

INTEGER, PARAMETER  :: BLKSIZ = 27998   ! size of MDB.TBUS archive set

! Local Variables

INTEGER             :: NOW(8)     ! Array to retrieve system date/time
INTEGER             :: DATIME(5)  ! Array for preparing index time.

CHARACTER (LEN=23)  :: ENTRY      ! Initialise index entry.

! ---------------------------------------------------------------------

! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

! Reset POINT to start of bulletin Id so full message is stored.

POINT=INDEX(BULL,'TBUS')

! Set bulletin header and blank flags into index entry.

ENTRY( 3:11)=TTAAII
ENTRY(17:17)=CHAR(0)

! Set missing position in index entry

ENTRY(13:16)=CHAR(128)//CHAR(0)//CHAR(128)//CHAR(0)

! Initialise variables to be passed to TAFREP - not used with this datatype

OERR=.FALSE.

! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

!     Pass message and associated info. on for storing in the database
!
!     Take current Year & Month, Day & time from bulletin

CALL DATIM(NOW)                           ! Current date

DATIME(1)=NOW(8)                          ! Year
DATIME(2)=NOW(7)                          ! Month

READ (YYGGGG(1:2),'(I2)') DATIME(3)       ! Day
READ (YYGGGG(3:4),'(I2)') DATIME(4)       ! Hour
READ (YYGGGG(5:6),'(I2)') DATIME(5)       ! Minute

!     Check "YY" falls within valid day range

IF (DATIME(3)  >=  32) THEN
  WRITE (6,*) 'BAD DAY GROUP (YY): ',YYGGGG(1:2), BULL(POINT:POINT+50)
  GO TO 9
END IF

!     Check "GGGG" falls within valied time range

IF (DATIME(4) >= 24 .OR. DATIME(5) >= 60) THEN
  PRINT *,'BAD TIME GROUP: ',YYGGGG(3:6),'   ', BULL(POINT:POINT+50)
  GO TO 9
END IF

!     Do not store out-of-date information (ie yesterday's data).

IF (NOW(6) /= DATIME(3)) GOTO 9

! *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *   *

!     Repeat the Identifier sequence from ENTRY as the last arg.
!      to ensure that bulletin header is stored in index.

CALL TAFREP (DATIME,ENTRY,BULL(POINT:BEND),NFT,BLKSIZ,ENTRY(3:11))

  9   RETURN
END SUBROUTINE TBUSBUL
