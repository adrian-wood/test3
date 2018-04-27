SUBROUTINE IDMAKE (VALUES, CSTR, NOBS, NSKIP, IDNDX)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  IDMAKE
!
!    IDMAKE makes an identifier to go in an index entry for a BUFR
!    message with character identifiers.
!
! DESCRIPTION:
!
!    IDMAKE makes an 8-character identifier for an index entry for a
!    BUFR message having character identifiers (i.e. not integers such
!    as satellite IDs). The result is returned in IDNDX and its nth
!    character will be the nth character of the individual observation
!    identifiers if they are all the same or an asterisk (*) if not.
!
!    For example, if the message contained SYNOPs for the stations
!    03171, 03377, 03672, 03772 and 03876, IDNDX would be '03*7*   '.
!
!    The argument NSKIP enables the first few characters of each
!    identifier to be ignored. This argument is provided for GPSIWV
!    data for which the first 5 characters are skipped when making
!    index entries. Normally NSKIP will be zero.
!
! USAGE:  CALL IDMAKE (VALUES, CSTR, NOBS, NSKIP, IDNDX)
!
! PARAMETERS:
!
!    Name    I/O  Type   Size   Description
!    ----    ---  ----   ----   -----------
!    VALUES   I   R*4    NOBS   'Values' for IDs in BUFR decoder output
!    CSTR     I   C*(*)         Character strings from BUFR decoder
!    NOBS     I   I*4           Number of obs. in decoded message
!    NSKIP    I   I*4           Number of characters to skip in ID
!    IDNDX    O   C*8           Identifier to go in index entry
!
! CALLS:  IDMAKE does not call any other routines.
!
! REVISION INFO:
!
!   $Workfile: idmake.f90$ $Folder: OpSource$
!   $Revision: 3$ $Date: 24/10/2011 14:48:03$
!
! CHANGE RECORD:
!
! $Log:
!  3    Met_DB_Project 1.2         24/10/2011 14:48:03    Brian Barwell
!       Correct ID length for the case NSKIP>0.
!  2    Met_DB_Project 1.1         05/10/2011 15:45:59    Brian Barwell
!       Modify to allow for the possibility of bulletins with ID missing.
!  1    Met_DB_Project 1.0         27/09/2011 16:22:54    Brian Barwell   New
!       subroutine to create a character ID for an index entry.
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

IMPLICIT NONE
!                     Subroutine arguments
!                     --------------------

INTEGER,          INTENT(IN)  :: NOBS  ! No. of obs in decoded message
REAL,             INTENT(IN)  :: VALUES(NOBS) ! 'Values' for IDs
CHARACTER(LEN=*), INTENT(IN)  :: CSTR  ! Character string from decoder
INTEGER,          INTENT(IN)  :: NSKIP ! No. of ID characters to skip
CHARACTER(LEN=8), INTENT(OUT) :: IDNDX ! Identifier to go in index entry

!                        Local variables
!                        ---------------

INTEGER I               ! Integer variable for local use
INTEGER IDLEN           ! Number of characters in each identifier
INTEGER IOBS            ! Observation counter
INTEGER J               ! Location of ID character in CSTR array
INTEGER JNDX            ! Loop variable for loop over ID characters
INTEGER JOB             ! Loop variable for loop over observations
INTEGER J1, J2          ! Location of start and end of ID in CSTR

LOGICAL SAME            ! .TRUE. if all observations have same ID

!-----------------------------------------------------------------------

!           Extract the first identifier and store in IDNDX
!           -----------------------------------------------

I = NINT(VALUES(1))            ! ID 'Value' for first observation
IF (I > 0) THEN                ! If not missing data,
  IDLEN = I/65536 - NSKIP      !  get ID characters to copy ...      !3
  IDLEN = MIN0(IDLEN,8)        !  ... but not more than 8
  J1    = MOD(I,65536) + NSKIP !  CSTR start address for ID
  J2    = J1 + IDLEN - 1       !  CSTR end address for ID
  IDNDX = CSTR(J1:J2)          !  ID of first observation
ELSE                           ! If ID is missing,
  IDLEN = 0                    !  set length to zero
  IDNDX = ' '                  !  and ID to blanks
END IF
!               Check whether all obs have the same ID
!               --------------------------------------
!                (Will often happen so worth checking)
IOBS = 1
SAME = .TRUE.
DO WHILE (SAME .AND. IOBS < NOBS)
  IOBS = IOBS + 1
  I = NINT(VALUES(IOBS))
  IF (I > 0) THEN                   ! If not missing data,
    J1   = MOD(I,65536) + NSKIP     !  ID start byte
    J2   = J1 + IDLEN - 1           !  ID end byte
    SAME = IDNDX == CSTR(J1:J2)     !  Check ID against IDNDX
  ELSE                              ! If ID missing,
    SAME = IDNDX == '        '      !  check for blank IDNDX
  END IF
END DO
!            If not all the same, loop over remaining obs.
!            ---------------------------------------------

IF (.NOT.SAME) THEN
  DO JOB=IOBS,NOBS
    I = NINT(VALUES(JOB))

!   Compare ID with IDNDX putting '*' in IDNDX where characters differ
!   ------------------------------------------------------------------

    IF (I > 0) THEN                  ! If ID not missing data,
      IF (IDLEN == 0) THEN           !  If ID length not yet set,    !3
        IDLEN = I/65536 - NSKIP      !   get ID characters to copy   !3
        IDLEN = MIN0(IDLEN,8)        !   ... but not more than 8     !3
      END IF                                                         !3
      J = MOD(I,65536) + NSKIP       ! Get ID start byte

      DO JNDX=1,IDLEN
        IF (IDNDX(JNDX:JNDX) /= CSTR(J:J)) IDNDX(JNDX:JNDX) = '*'
        J = J + 1
      END DO

!          If ID is missing, treat it as 8 blank characters
!          ------------------------------------------------
    ELSE
      DO JNDX=1,8
        IF (IDNDX(JNDX:JNDX) /= ' ') IDNDX(JNDX:JNDX) = '*'
      END DO
    END IF
  END DO
END IF

RETURN
END SUBROUTINE IDMAKE
