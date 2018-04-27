SUBROUTINE WANTTIME (ENTRY, NVER, NXTIM1, NTIM1, NTIM2, KEEP)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  WANTTIME
!
!    To compare the time window in an MDB request string with that in
!    an index entry from a storage data set.
!
! DESCRIPTION:
!
!    WANTTIME checks the time window in a user's MDB request string
!    against the data time window indicated by the times in an index
!    entry. The result is indicated by the output variable KEEP.
!
!    If the data time window lies entirely outside the user's window,
!    none of the observations are wanted and KEEP is set to -1.
!
!    If the data time window lies entirely within the user's window,
!    all of the observations are wanted and KEEP is set to +1.
!
!    If there is a pertial overlap between the user's time window and
!    the data time window, it is not possible to say which observations
!    are wanted without decoding the BUFR message and checking the
!    observation times individually. In this case KEEP is set to zero.
!
! USAGE:  CALL WANTTIME (ENTRY, NVER, NXTIM1, NTIM1, NTIM2, KEEP)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    ENTRY    I   C*(*)  Index entry from MetDB storage data set
!    NVER     I   I*4    Index entry version number (1 or 2)
!    NXTIM1   I   I*4    Century minutes for start of index entry day
!    NTIM1    I   I*4    Century minutes for user's start date/time
!    NTIM2    I   I*4    Century minutes for user's end date/time
!    KEEP     O   i*4    'Wanted' flag, -1, 0 or +1 (see Description)
!
! CALLS:  WANTTIME does not call any other routines.
!
! REVISION INFO:
!
!    $Workfile: wanttime.f90$ $Folder: OpSource$
!    $Revision: 3$ $Date: 30/11/2010 09:27:34$
!
! CHANGE RECORD:
!
! $Log:
!  3    MetDB_Refresh 1.2         30/11/2010 09:27:34    Brian Barwell
!       Correct bug in calculation of latest ob date/time.
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
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

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(*), INTENT(IN)        ::  ENTRY ! Index entry
INTEGER, INTENT(IN)             ::  NVER ! Index entry version number (1 or 2)
INTEGER, INTENT(IN)             ::  NXTIM1 ! Century minutes for start of index period
INTEGER, INTENT(IN)             ::  NTIM1 ! Century minutes for user's start date/time
INTEGER, INTENT(IN)             ::  NTIM2 ! Century minutes for user's end date/time
INTEGER, INTENT(OUT)            ::  KEEP ! 'Keep' flag from time window checking

! Local declarations:

INTEGER      ::  IDTIM1 ! Century minutes for earliest ob date/time
INTEGER      ::  IDTIM2 ! Century minutes for latest ob date/time
INTEGER      ::  IMINS ! Time from index entry (minutes after 00Z)
INTEGER      ::  IPDT(2) ! Start bytes of date/time for index versions
INTEGER      ::  IPT ! Pointer to byte in index entry
INTEGER      ::  IXTIM1 ! Century minutes for start of index day

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA IPDT /3,10/

IPT = IPDT(NVER)             ! First byte of time info.
IXTIM1 = 1440*(NXTIM1/1440)  ! Start of index day

!                                                 Earliest ob date/time
IMINS = 60*ICHAR(ENTRY(IPT  :IPT  )) + &
           ICHAR(ENTRY(IPT+1:IPT+1))
IDTIM1 = IXTIM1 + IMINS
!                                                   Latest ob date/time
IMINS = 60*ICHAR(ENTRY(IPT+3:IPT+3)) + &
           ICHAR(ENTRY(IPT+4:IPT+4))
IDTIM2 = IXTIM1 + IMINS
!                         Dates may need adjusting if period covers 00Z

IF (IDTIM1 < NXTIM1) THEN        ! Ob time window is after 00Z
  IDTIM1 = IDTIM1 + 1440
  IDTIM2 = IDTIM2 + 1440

ELSE IF (IDTIM2 < IDTIM1) THEN   ! Ob time window includes 00Z
  IDTIM2 = IDTIM2 + 1440
END IF
!                 Reject all obs if entirely outside user's time window

!          Data too early  or   Data too late
IF (IDTIM2 < NTIM1 .OR. IDTIM1 > NTIM2) THEN
  KEEP = -1
!                    Keep all obs if entirely within user's time window

!              Data not early   and   Data not late
ELSE IF (IDTIM1 >= NTIM1 .AND. IDTIM2 <= NTIM2) THEN
  KEEP = 1
!                         Obs need checking individually: set KEEP to 0
ELSE
  KEEP = 0
END IF

RETURN
END SUBROUTINE WANTTIME
