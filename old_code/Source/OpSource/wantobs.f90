SUBROUTINE WANTOBS (VALUES, CSTR, NOBS, LDISP, KEEP, NTIME, AREA, &
                    RPOLE, CHID, NPLATS, WANTOB, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  WANTOBS
!
!    To indicate which observations in a BUFR message are wanted for
!    retrieval.
!
! DESCRIPTION:
!
!    WANTOBS returns an array of flags in the LOGICAL*1 array WANTOB
!    specifying which observations in a BUFR message are required for
!    retrieval. Information on the required time window (NTIME), area
!    (AREA, RPOLE) and identifiers (CHID, NPLATS) obtained from the
!    user's MDB request string is used.  Also input are the number of
!    observations (NOBS), the data values from a BUFR decode (VALUES)
!    and the displacements in this array of selected items (LDISP).
!
!    Checking against the index entry should have been done and the
!    results presented in the 3-element array KEEP. This contains flags
!    for time, area and identifier, the flag values being -1 if none of
!    the observations are wanted, +1 if all observations are wanted and
!    zero if observations need to be checked individually. WANTOBS does
!    this individual checking. Clearly, there is no point in calling
!    WANTOBS if any of the flags are -1 or if all of the flags are +1.
!
!    If the BUFR message lacks information essential for doing the
!    checking a non-zero return code is set (see list below) and
!    control is immediately returned to the calling program.
!
!    It is up to the user to check that the WANTOB array is big enough
!    (i.e. at least NOBS elements). It is recommended that this check
!    is done before WANTOBS is called.
!
! USAGE:  CALL WANTOBS (VALUES, NOBS, LDISP, KEEP, NTIME, AREA, RPOLE,
!                       CHID, NPLATS, WANTOB)
!
! ARGUMENTS:
!
!    Name   I/O  Type Size    Description
!    ----   ---  ---- ----    -----------
!    VALUES  I   R*4    *   (Array dimensioned (NOBS,*))  Array of data
!                              values from a decoded BUFR message
!    CSTR    I   C*(*)      String of character data from BUFR decode
!    NOBS    I   I*4        Number of observations in BUFR message
!    LDISP   I   I*4   16   Displacements in VALUES for selected items
!    KEEP    I   I*4    3   Index entry check flags (see Description)
!    NTIME   I   I*4    8   User's start and end times (y,m,d,hhmm)*2
!    AREA    I   R*4    5   User's area (Flag, N, W, S, E)
!    RPOLE   I   R*4 2      Latitude and longitude of rotated pole
!    CHID    I   C*(*)  *   Array of user's platform identifiers
!    NPLATS  I   I*4        Number of identifiers in CHID array
!    WANTOB  O   L*1    *   'Observation wanted' flags
!    ICODE   O   I*4        Return code (see below)
!
! RETURN CODES:
!
!      0  Array of 'observation wanted' flags created with no errors.
!    221  Observation time not available in BUFR message
!    222  Latitude & longitude not available in BUFR message
!    223  Station identifiers not available in BUFR message
!
! CALLS:  ROTAREA
!
! REVISION INFO:
!
!    $Workfile: wantobs.f90$ $Folder: OpSource$
!    $Revision: 10$ $Date: 26/06/2012 17:07:37$
!
! CHANGE RECORD:
!
! $Log:
!  10   MetDB_Refresh 1.9         26/06/2012 17:07:37    Brian Barwell   Code
!       added to handle 7-digit WMO IDs added. Some code improvements.
!  9    MetDB_Refresh 1.8         24/11/2011 09:54:37    Sheila Needham  Fixed
!       formatting issues caused by ftp from els018
!  8    MetDB_Refresh 1.7         24/11/2011 09:09:33    Sheila Needham
!       Correction to earliest/latest range check
!  7    MetDB_Refresh 1.6         22/08/2011 15:51:19    Brian Barwell
!       Identifier checking section added.
!  6    MetDB_Refresh 1.5         26/04/2011 15:31:26    Brian Barwell   Get
!       OBLAT and OBLON the right way round.
!  5    MetDB_Refresh 1.4         09/02/2011 17:37:36    Sheila Needham  Change
!        declaration of CHID
!  4    MetDB_Refresh 1.3         20/12/2010 16:26:40    Sheila Needham
!       Initialise ICODE
!  3    MetDB_Refresh 1.2         25/11/2010 17:11:51    Alison Weir
!       Comment in USE statement
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE rotarea_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  NPLATS ! Number of platforms specified by user
INTEGER, INTENT(IN)             ::  NOBS ! Number of observations in message
REAL, INTENT(IN)                ::  VALUES(NOBS,*) ! Data values from BUFR decode
CHARACTER(LEN=*), INTENT(IN)    ::  CSTR ! Character strings fron BUFR decode
INTEGER, INTENT(IN)             ::  LDISP(*) ! Displacements for selected data items
INTEGER, INTENT(IN)             ::  KEEP(3) ! 'Keep' flags for time, area and identifier
INTEGER, INTENT(IN)             ::  NTIME(8) ! User's start and end times (y,m,d,hhmm)*2
REAL, INTENT(IN)                ::  AREA(5) ! User's area (Flag, N, W, S, E)
REAL, INTENT(IN)                ::  RPOLE(2) ! Latitude and longitude of rotated pole
CHARACTER(*), INTENT(IN)        ::  CHID(NPLATS)  ! requested platform  identifiers
LOGICAL(kind=1), INTENT(OUT)    ::  WANTOB(*) ! .TRUE. if obs are wanted
INTEGER, INTENT(OUT)            ::  ICODE ! Integer return code (see above for values)

! Local declarations:

INTEGER      ::  I         ! Integer variable for local use
INTEGER      ::  ICH       ! Byte number in identifier
INTEGER      ::  ILEN      ! Length of character identifier in CSTR
INTEGER      ::  IOB       ! Loop variable for loop over observations
INTEGER      ::  IPLAT     ! Loop variable for platform identifiers
INTEGER      ::  ITIME(5)  ! Observation time (y,m,d,h or hhmm,m)

REAL         ::  OBLAT     ! Observation latitude
REAL         ::  OBLON     ! Observation longitude
REAL         ::  RLAT      ! True ob latitude when using rotated pole
REAL         ::  RLON      ! True ob longitude when using rotated pole

LOGICAL      ::  CHECKID   ! Control flag for wanted ID loop
LOGICAL      ::  IDCH      ! .TRUE. if identifier is characters
LOGICAL      ::  IDWMO5    ! .TRUE. if identifier is WMO blk/stn    !10
LOGICAL      ::  IDWMO7    ! .TRUE. if ID is WMO region/subarea/id  !10

CHARACTER(LEN=1) :: CH     ! Character extracted for local use
CHARACTER(LEN=8) :: OBID   ! Observation platform identifier

!-----------------------------------------------------------------------
ICODE = 0
!                          Assume obs are wanted until proved otherwise
WANTOB(1:NOBS) = .TRUE.                                             !10

!=======================================================================
! (1)  CHECK OBSERVATION TIMES AGAINST USER'S TIME WINDOW
!=======================================================================

IFLABEL1: &
IF (KEEP(1) == 0) THEN
!                            Check for availability of time information

  DO I=1,5  ! (Year, month, day, hour, minute)
    IF (LDISP(I) <= 0) THEN
      WRITE (6,'(T5,A,T15,A)') 'WANTOBS:', &
               'OBSERVATION TIME NOT AVAILABLE IN BUFR MESSAGE'
      ICODE = 221
!                                      If not available, reject all obs
      WANTOB(1:NOBS) = .FALSE.                                      !10
      RETURN
    END IF
  END DO
!                                                Loop over observations
DOLABEL1: &
  DO IOB=1,NOBS
!                         Get observation time & check for missing data
    DO I=1,5
      ITIME(I) = NINT(VALUES(IOB,LDISP(I+2)))
      IF (ITIME(I) < 0) WANTOB(IOB) = .FALSE.
    END DO
!                                     Convert hour and minute to 'hhmm'
IFLABEL2: &
    IF (WANTOB(IOB)) THEN
      ITIME(4) = 100*ITIME(4) + ITIME(5)  ! 'hhmm'

!                      Check observation time against user's start time
      I = 1
      DO WHILE (WANTOB(IOB) .AND. I <= 4)
        IF (ITIME(I) < NTIME(I)) WANTOB(IOB) = .FALSE.
        IF (ITIME(I) /= NTIME(I)) EXIT
        I = I + 1
      END DO
!                        Check observation time against user's end time
      I = 1
      DO WHILE (WANTOB(IOB) .AND. I <= 4)
        IF (ITIME(I) > NTIME(I+4)) WANTOB(IOB) = .FALSE.
        IF (ITIME(I) /= NTIME(I+4)) EXIT
        I = I + 1
      END DO
    END IF IFLABEL2
  END DO DOLABEL1
END IF IFLABEL1

!=======================================================================
! (2)  CHECK OBSERVATION POSITIONS AGAINST USER'S LAT/LONG BOX
!=======================================================================

IFLABEL3: &
IF (KEEP(2) == 0) THEN
!                                  Check for availability of lat & long
!     Longitude          Latitude
  IF (LDISP(1) <= 0 .OR. LDISP(2) <= 0) THEN
    WRITE (6,'(T5,A,T15,A)') 'WANTOBS:', &
             'LATITUDE & LONGITUDE NOT AVAILABLE IN BUFR MESSAGE'
    ICODE = 222
!                                      If not available, reject all obs
    WANTOB(1:NOBS) = .FALSE.                                        !10
    RETURN
  END IF
!                                                Loop over observations
DOLABEL2: &
  DO IOB=1,NOBS
IFLABEL4: &
    IF (WANTOB(IOB)) THEN
!                                                        Get lat & long
      OBLON = VALUES(IOB,LDISP(1))
      OBLAT = VALUES(IOB,LDISP(2))
!                                                Check for missing data
IFLABEL5: &
      IF (OBLAT < -999.0 .OR. OBLON < -999.0) THEN
        WANTOB(IOB) = .FALSE.
      ELSE

!-----------------------------------------------------------------------
! (2A)  Rotated pole: convert to lat & long to rotated co-ordinates
!-----------------------------------------------------------------------

        IF (AREA(1) == 1.0) THEN       ! Rotated pole
          RLAT = OBLAT
          RLON = OBLON
          CALL ROTAREA (RLAT, RLON, OBLAT, OBLON, RPOLE)
        END IF

!-----------------------------------------------------------------------
! (2B)   Check for observation outside range of user's latitudes
!-----------------------------------------------------------------------

!            Ob too far N          Ob too far S
        IF (OBLAT > AREA(2) .OR. OBLAT < AREA(4)) THEN
          WANTOB(IOB) = .FALSE.

!-----------------------------------------------------------------------
! (2C)  Check for observation outside range of user's longitudes
!-----------------------------------------------------------------------

        ELSE
!                                         User's box doesn't cross 180W
          IF (AREA(3) <= AREA(5)) THEN

!                Ob too far W         Ob too far E
            IF (OBLON < AREA(3) .OR. OBLON > AREA(5)) &
                WANTOB(IOB) = .FALSE.
          ELSE
!                                               User's box crosses 180W

!                Ob too far W          Ob too far E
            IF (OBLON < AREA(3) .AND. OBLON > AREA(5)) &
              WANTOB(IOB) = .FALSE.
          END IF
        END IF
      END IF IFLABEL5
    END IF IFLABEL4
  END DO DOLABEL2
END IF IFLABEL3

!=======================================================================
! (3)  CHECK OBSERVATION IDENTIFIERS AGAINST USER'S LIST OF PLATFORMS
!=======================================================================

IFLABEL7: &
IF (KEEP(3) == 0) THEN

!-----------------------------------------------------------------------
! (3A)  Check for type of identifier in decoded BUFR message
!       (Note: LDISP 14 (2nd character ID) is not yet used.)
!-----------------------------------------------------------------------
!                                             (1) Character ID
  IDCH = LDISP(13) < 0
!                                             (2) WMO numbers
!          WMO block           WMO station
  IDWMO5 = LDISP(10) > 0 .AND. LDISP(11) > 0                        !10

!          WMO region          WMO subarea         Platform ID
  IDWMO7 = LDISP(9)  > 0 .AND. LDISP(10) > 0 .AND. LDISP(12) > 0    !10

!                                Return with ICODE 223 if no identifier

  IF (.NOT.IDCH .AND. .NOT.IDWMO5 .AND. LDISP(12) <= 0) THEN        !10
    WRITE (6,'(T5,A,T15,A)') 'WANTOBS:',  &
             'STATION IDENTIFIERS NOT AVAILABLE IN BUFR MESSAGE'
    ICODE = 223
    RETURN
  END IF

!-----------------------------------------------------------------------
! (3B)  Loop over observations in BUFR message and extract ID
!-----------------------------------------------------------------------

OBLOOP: &
  DO IOB=1,NOBS
!                              Ignore ob if already flagged as unwanted
    IF (.NOT.WANTOB(IOB)) CYCLE                                     !10
    CHECKID = .FALSE.                                               !10
!                                              (1) Character identifier
    IF (IDCH) THEN
      I = -LDISP(13)/10
      I = NINT(VALUES(IOB,I))
      IF (I.GT.0) THEN                      ! Valid ID
        ILEN = MIN0(8,I/65536)              ! Length (not >8)
        I = MOD(I,65536)                    ! Start byte
        OBID = CSTR(I:I+ILEN-1)             ! Identifier
        IF (OBID.NE.' ') CHECKID = .TRUE.                           !10
      END IF
    END IF                                                          !10
!                                      (2) WMO block and station number
    IF (.NOT.CHECKID .AND. IDWMO5) THEN                             !10
      I = 1000*NINT(VALUES(LDISP(10),IOB)) + NINT(VALUES(LDISP(11),IOB))
      IF (I.GT.0 .AND. I.LT.100000) THEN    ! Valid ID
        WRITE (OBID,'(I5.5,3X)') I
        CHECKID = .TRUE.                                            !10
      END IF
    END IF                                                          !10
!                                          (3) WMO region, subarea, ID.
    IF (.NOT.CHECKID .AND. IDWMO7) THEN                             !10
      I = 1000000*NINT(VALUES(LDISP(9),IOB)) +  & ! 1-digit region  !10
           100000*NINT(VALUES(LDISP(10),IOB)) + & ! 1-digit subarea !10
                  NINT(VALUES(LDISP(12),IOB))     ! 5-digit ID      !10
      IF (I.GT.0 .AND. I.LT.10000000) THEN        ! Valid ID        !10
        WRITE (OBID,'(I7.7,1X)') I                                  !10
        CHECKID = .TRUE.                                            !10
      END IF
    END IF                                                          !10
!                                                 (4) Other integer ID.
    IF (.NOT.CHECKID .AND. LDISP(12) > 0) THEN                      !10
      I = NINT(VALUES(LDISP(12),IOB))
      IF (I > 0 .AND. I < 100000000) THEN ! Valid ID                !10
        WRITE (OBID,'(I8.8)') I
        CHECKID = .TRUE.                                            !10
      END IF
    END IF                                                          !10

!-----------------------------------------------------------------------
! (3C)  Loop over wanted identifiers for comparing with ID in message
!-----------------------------------------------------------------------

PLATLOOP: &
    DO IPLAT=1,NPLATS                                               !10
      WANTOB(IOB) = .TRUE. ! Reset for next PLAT                    !10

!-----------------------------------------------------------------------
! (3D)  Compare observation and wanted IDs one character at a time
!-----------------------------------------------------------------------

      DO ICH=1,8                      ! Up to 8 characters          !10
        CH = CHID(IPLAT)(ICH:ICH)
!                                 Check for end of requested identifier

        IF (CH.EQ.' ') THEN     ! ID matched so keep observation
          EXIT                  ! No more checking needed           !10

!              Check for failure to match if not 'wild' character ('*')

        ELSE IF (CH.NE.'*' .AND. CH.NE.OBID(ICH:ICH)) THEN
          WANTOB(IOB) = .FALSE. ! Mismatch: reject observation      !10
          EXIT                  ! Go on to next CHID                !10
        END IF
      END DO

      IF (WANTOB(IOB)) EXIT     ! Go to next ob if this is wanted   !10
    END DO PLATLOOP
  END DO OBLOOP
END IF IFLABEL7

RETURN
END SUBROUTINE WANTOBS
