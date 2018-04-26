SUBROUTINE WANTID (ENTRY, NVER, NSAT, CHID, NPLATS, KEEP)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  WANTID
!
!    To check an identifier in an index entry against a user-supplied
!    list of wanted identifiers.
!
! DESCRIPTION:
!
!    WANTID compares the identifier in an index entry (ENTRY) with a
!    user-supplied list of identifiers and sets the output variable
!    KEEP as follows:
!
!      If the identifier fails to match with anything in the user's
!      list, KEEP is set to -1 to show that the message is not wanted.
!
!      If the identifier exactly matches at least one item in the
!      user's list, KEEP is set to 1 to show that all observations in
!      the message are wanted.
!
!      If there is a partial match between the identifier and one or
!      more in the user's list (i.e. the identifier contains at least
!      one asterisk) KEEP is set to 0 to show that some observations
!      may be wanted but each one will need to be checked after BUFR
!      decoding.
!
!    NVER is the index entry version number and should be specified
!    as 1 for satellite data (26-byte entries) and 2 for other data
!    (33-byte entries).
!
!    For satellite data, a list of required satellite identifiers
!    should be supplied in the integer array NSAT. Each one will be
!    compared with the satellite number in the index entry and KEEP
!    will be output as +1 or -1 depending on whether there is or
!    isn't a match.
!
!    For non-satellite data, a list of required identifiers should
!    be supplied in the CHARACTER*9 array CHID. These can be partial
!    identifiers and may contain asterisks as 'wild' characters (e.g.
!    code '03' for all UK land SYNOPs or '03**7' for all UK land SYNOPs
!    ending in '7').  KEEP will be set to +1, 0 or -1 after comparison
!    with the identifier in the index entry (which may also contain
!    one or more asterisks).
!
!    NPLATS should be specified as the number of identifiers supplied
!    (in the NSAT array for satellite data or in the CHID array for
!    non-satellite data).
!
! USAGE:  CALL WANTID (ENTRY, NVER, NSAT, CHID, NPLATS, KEEP)
!
! PARAMETERS:
!
!    Name   I/O  Type  Size   Description
!    ----   ---  ----  ----   -----------
!    ENTRY   I   C*(*)      Index entry (26 or 33 bytes)
!    NVER    I   I*4        Index entry version number (1 or 2)
!    NSAT    I   I*4    *   List of wanted satellite identifiers
!    CHID    I   C*9    *   List of wanted character identifiers
!    NPLATS  I   I*4        Number of identifiers in NSAT or CHID
!    KEEP    O   I*4       'Wanted' flag, -1, 0 or +1 (see Description)
!
! CALLS:  WANTID does not call any other routines.
!
! REVISION INFO:
!
! $Workfile: wantid.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 26/06/2012 17:05:48$
!
! CHANGE RECORD:
!
! $Log:
!  2    MetDB_Refresh 1.1         26/06/2012 17:05:48    Brian Barwell   Bug
!       fix: only update KEEP to a higher value (i.e. better match).
!  1    MetDB_Refresh 1.0         22/08/2011 15:53:09    Brian Barwell   New
!       routine for ccomparing user & index identifiers
! $
!     Original version by Brian Barwell, December 2010.
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments

CHARACTER(LEN=*), INTENT(IN) :: ENTRY    ! Index entry
INTEGER, INTENT(IN)  :: NVER    ! Index entry version number (1 or 2)
INTEGER, INTENT(IN)  :: NSAT(:) ! User's requested satellite IDs
CHARACTER(LEN=9), INTENT(IN) ::  CHID(:) ! User's requested platform IDs
INTEGER, INTENT(IN)  :: NPLATS  ! Number of platforms specified by user
INTEGER, INTENT(OUT) :: KEEP    ! 'Keep' flag from identifier checking

! Local variables

INTEGER :: I        ! Integer variable for local use
INTEGER :: ICH      ! Location of character in CHID element
INTEGER :: ICHAR2   ! Function to convert C*2 to I*4
INTEGER :: IDLEN    ! Length of identifier in index entry
INTEGER :: IKEEP    ! Local 'keep' flag for a given platform         !2
INTEGER :: ISAT     ! Satellite identifier in index entry
INTEGER :: LENCH    ! Length of character identifier in CHID

CHARACTER        :: CH     ! Character in CHID array
CHARACTER(LEN=8) :: IDENT  ! Platform identifier from index entry

!-----------------------------------------------------------------------
!     INDEX ENTRY FORMAT 1: SATELLITE DATA
!-----------------------------------------------------------------------

VERCHECK: &
IF (NVER.EQ.1) THEN
!                                           Decode satellite identifier
  I = ICHAR2(ENTRY(1:2))
  ISAT = MOD(I,1024)
!                              Assume bulletin not wanted until checked
  KEEP = -1
!                                Check identifier against requested IDs
  I = 0
  DO WHILE (KEEP < 0)
    I = I + 1
    IF (NSAT(I) == ISAT) KEEP = 1  ! ID match - keep bulletin
    IF (I >= NPLATS) EXIT          ! No more IDs to check
  END DO

!-----------------------------------------------------------------------
!     INDEX ENTRY FORMAT 2: NON-SATELLITE DATA
!-----------------------------------------------------------------------

ELSE IF (NVER == 2) THEN
!                             Get ID from index entry & find its length
  IDENT = ENTRY(2:9)
  IDLEN = INDEX(IDENT,' ') - 1
  IF (IDLEN <= 0) IDLEN = 8
!                                       Assume not wanted until checked
  KEEP = -1
!                                         Loop over requested platforms
PLATLOOP: &
  DO I=1,NPLATS                                                      !2
!                                                Get platform ID length
    LENCH = INDEX(CHID(I),' ') - 1
    IF (LENCH < 0) LENCH = 9
!                                         No match if longer than IDLEN
LENCHECK: &
    IF (LENCH <= IDLEN) THEN
!                               No further testing if identifiers agree
IDCHECK: &
      IF (IDENT(1:LENCH) == CHID(I)(1:LENCH)) THEN                   !2
        KEEP = 1                                                     !2
      ELSE                                                           !2
!                                     Check IDs one character at a time
        IKEEP = 1                                                    !2
CHARLOOP: &
        DO ICH=1,LENCH                                               !2
          CH = CHID(I)(ICH:ICH)
!                                     '*' in CHID matches with anything
          IF (CH /= '*') THEN
!                                       '*' in index ID: individual obs
!                                                   need checking later
            IF (IDENT(ICH:ICH) == '*') THEN
              IKEEP = 0                                              !2
!                                      Characters don't agree: no match

            ELSE IF (IDENT(ICH:ICH) /= CH) THEN
              IKEEP = -1                                             !2
              EXIT                                                   !2
            END IF
          END IF
        END DO CHARLOOP
!                                       If match is better, update KEEP
        IF (IKEEP > KEEP) KEEP = IKEEP                               !2
      END IF IDCHECK
    END IF LENCHECK
!                        If fully matched, no more checking is required
    IF (KEEP == 1) EXIT                                              !2
  END DO PLATLOOP
END IF VERCHECK

RETURN
END SUBROUTINE WANTID
