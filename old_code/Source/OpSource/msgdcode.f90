SUBROUTINE MSGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS, &
                     NELEMS, NHIST, NCHANS, NCHOP)

!-----------------------------------------------------------------------
!
! SUBROUTINE  : MSGDCODE
!
! PURPOSE     : To decode an MSGWINDS bulletin and get channel numbers
!               for each observation.
!
! DESCRIPTION : MSGDCODE decodes an MSGWINDS bulletin, compares the
!               channel frequencies for each observation against data
!               for MSG channels to find channel numbers.
!               Channel numbering (MSG-1 frequencies):
!
!                     Channel         Wavelength  Frequency
!                      number   Type   (Microns)     (Hz)
!                     -------   ----  ----------  ---------
!                        1      VIS       0.6     4.721E+14
!                        2      VIS       0.8     3.701E+14
!                        3      NIR       1.6     1.874E+14
!                        4       IR       3.9     7.687E+13
!                        5       WV       6.3     4.797E+13
!                        6       WV       7.4     4.079E+13
!                        7       IR       8.7     3.446E+13
!                        8       O3       9.7     3.103E+13
!                        9       IR      10.8     2.776E+13
!                       10       IR      12.0     2.498E+13
!                       11      CO2      13.4     2.237E+13
!                       12     HRVIS      0.8     3.997E+14
!
!               N.B. Frequency determination is done by comparing the
!                    decoded frequency against a set of non-overlapping
!                    ranges for the 12 channels. MSG-1 data was used to
!                    choose the ranges: as future satellites may have
!                    slightly different channel frequencies, the ranges
!                    should be checked to see that they still apply.
!
!               Channel numbers for the observations are returned in an
!               array NCHANS and a histogram of numbers of observations
!               for each channel is returned in the array NHIST.  Also,
!               the number of different channels found (NCHOP, equal
!               to the number of BUFR bulletins to create for storage)
!               is returned. NCHOP is set to zero if the bulletin can
!               be stored without re-encoding or -1 if it should be
!               completely rejected.
!
!               If there are decoding problems (e.g. VALUES array too
!               small, frequency descriptor not found, or more than
!               MAXDES descriptors), a zero value of NOBS is returned.
!
!               If there are more than MAXOBS obs, channel numbers for
!               only the first MAXOBS are returned in NCHANS.
!
! USAGE       : CALL MSGDCODE (MSGBULL, MAXOBS, MAXVALS, VALUES, NOBS,
!                              NELEMS, CHANS, NCHOP)
!
! ARGUMENTS   : ('I'=Input, 'O'=Output, 'I/O'= Both)
!
!     No.  Arg.  I/O Type  Size   Contents
!     --  -----  --- ----  ----   --------
!      1  MSGBULL I  C(*)       BUFR MSGWINDS bulletin ('BUFR...7777').
!      2  MAXOBS  I   I4        Size of NCHANS array in calling program.
!      3  MAXVALS I   I4        Size of VALUES array in calling program.
!      4  VALUES  O   R4 (NOBS,*)  User's array of decoded elements
!                                    with values for NOBS obs.
!      5  NOBS    O   I4        First dimension of VALUES (no. of obs).
!      6  NELEMS  O   I4        Number of decoded elements per ob.
!      7  NHIST   O   I4   12   Number of obs present for each channel
!      8  NCHANS  O   I4    *   Array of channel numbers for each ob.
!                                 (reduced to MAXOBS obs if necessary).
!      9  NCHOP   O   I4        No. of bulletins to create for storage
!
! CALLED BY   : STOREUM
!
! CALLS       : DEBUFR, DESFXY
!
! HISTORY     : Original version by Brian Barwell, February 2004.
!
! REVISION INFO :
!
!
! $Workfile: msgdcode.f90$ $Folder: OpSource$
! $Revision: 1$ $Date: 27/01/2011 16:19:32$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         27/01/2011 16:19:32    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

USE DEBUFR_mod
USE DESFXY_mod

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(IN)  :: MSGBULL   ! (a1) MSG bulletin to be decoded
INTEGER, INTENT(IN)            :: MAXOBS    ! (a2) Size of user's NCHANS array
INTEGER, INTENT(IN)            :: MAXVALS   ! (a3) Size of user's VALUES array
REAL, INTENT(OUT)              :: VALUES(:) ! (a4) Array of decoded values from MSGBULL
INTEGER, INTENT(OUT)           :: NOBS      ! (a5) Number of observations in BUFR message
INTEGER, INTENT(OUT)           :: NELEMS    ! (a6) Number of decoded elements per observation
INTEGER, INTENT(OUT)           :: NHIST(12) ! (a7) Numbers of obs present for 12 channels
INTEGER, INTENT(OUT)           :: NCHANS(:) ! (a8) Channel numbers for obs in bulletin
INTEGER, INTENT(OUT)           :: NCHOP     ! (a9) Number of bulletins to create for storage

! Parameters

INTEGER, PARAMETER  :: MAXDES=300  ! Maximum number of BUFR descriptors
                                   ! (MSGWINDS currently has 251)

! Variables

INTEGER  :: JCHAN     ! Loop variable for loop over channels
INTEGER  :: JDES      ! Loop variable for loop over descriptors
INTEGER  :: JOB       ! Loop variable for loop over observations
INTEGER  :: LISTDES(MAXDES) ! BUFR descriptor sequence for message
INTEGER  :: NBAD      ! Number of obs for which channel is unknown
INTEGER  :: NDES      ! Number of descriptors in BUFR sequence
INTEGER  :: NF        ! Decoded parts of BUFR descriptor 'FXXYYY'
INTEGER  :: NX        ! Decoded parts of BUFR descriptor 'FXXYYY'
INTEGER  :: NY        ! Decoded parts of BUFR descriptor 'FXXYYY'
INTEGER  :: NSKIP     ! Number of values before first frequency
INTEGER  :: NUMCHAN   ! Channel number for current observation
INTEGER, SAVE  ::  N2153 = 1  ! Loc. of frequency (BUFR 002153) in seq.

REAL        :: FREQ      ! Channel frequency for current observation
REAL, SAVE  :: FRQ1(12)  ! Upper limit of freq. range for 12 channels
REAL, SAVE  :: FRQ2(12)  ! Lower limit of freq. range for 12 channels
REAL        :: OLDFREQ   ! Channel frequency for last observation

LOGICAL     ::  FOUND     ! .TRUE. if frequency descriptor found

CHARACTER (LEN=1000)  :: NAMES   ! String for decoded characters

!                                        Upper (FRQ1) and lower (FRQ2)
!                           limits of frequency ranges for 12 channels

DATA FRQ1 /6.00E14, 3.85E14, 2.60E14, 1.00E14, 6.00E13, 4.40E13, &
           3.75E13, 3.25E13, 2.95E13, 2.60E13, 2.35E13, 4.40E14/
DATA FRQ2 /4.40E14, 2.60E14, 1.00E14, 6.00E13, 4.40E13, 3.75E13, &
           3.25E13, 2.95E13, 2.60E13, 2.35E13, 1.50E13, 3.85E14/

!-----------------------------------------------------------------------
!     INITIALISATIONS
!-----------------------------------------------------------------------
!                                Channel histogram
DO JCHAN=1,12
  NHIST(JCHAN) = 0
END DO ! JCHAN
!                                Channel numbers
DO JOB=1,MAXOBS
  NCHANS(JOB) = 0
END DO ! JOB
!                                Bulletins to make
NCHOP = -1   ! Reject

!-----------------------------------------------------------------------
!     BUFR DECODE OF MSGWINDS BULLETIN
!-----------------------------------------------------------------------

NDES = MAXDES
NOBS = MAXVALS
CALL DEBUFR (LISTDES, VALUES, NAMES, NDES, NOBS, MSGBULL, .FALSE.)

!                             Check that arrays are big enough (if not,
!                             DEBUFR prints message and sets NOBS to 0)
IF (NOBS == 0) RETURN
!                              Find the number of decoded values per ob
NELEMS = 0
DO JDES=1,NDES
  CALL DESFXY (LISTDES(JDES), NF,NX,NY)
  IF (NF == 0) NELEMS = NELEMS + 1
END DO ! JDES

!-----------------------------------------------------------------------
!     LOCATE FREQUENCY DESCRIPTOR (002153) IN BUFR SEQUENCE
!-----------------------------------------------------------------------

FOUND = .FALSE. ! Descriptor not yet found
N2153 = 0       ! Location not yet known
JDES = 0
!                                              Loop through descriptors
DO WHILE (.NOT.FOUND .AND. JDES < NDES)
  JDES = JDES + 1
  IF (LISTDES(JDES) < 16384) THEN
    N2153 = N2153 + 1
    IF (LISTDES(JDES) == 665) FOUND = .TRUE. ! (665 = 2*256 + 153)
  END IF
END DO
!                                        Frequency descriptor not found
IF (.NOT.FOUND) THEN
  WRITE (6,'(T5,A,T15,2A)') 'MSGDCODE:', 'Channel frequency ',   &
                 'descriptor (BUFR 002153) not found in message'
  NOBS = 0  ! To skip message
  RETURN
END IF

!-----------------------------------------------------------------------
!     DEDUCE CHANNEL NUMBERS AND FLAG CHANNELS PRESENT
!-----------------------------------------------------------------------
!                     Loop over obs and get the number for each channel

OLDFREQ = 0.0           ! Frequency of last ob. not available
NUMCHAN = 0             ! Channel no. of last ob. not available
NBAD = 0                ! No. of obs with unknown channel number
NSKIP = NOBS*(N2153-1)  ! No. of values before first frequency

DO_obs: &
DO JOB=1,MIN0(NOBS,MAXOBS)
  FREQ = VALUES(NSKIP+JOB)  ! Channel frequency

!                         Find channel number if different from last ob

  IF (FREQ /= OLDFREQ) THEN
    NUMCHAN = 0  ! No match yet
    JCHAN = 1    ! Start from channel 1

!                    Compare FREQ with frequency ranges for 12 channels

    DO WHILE (NUMCHAN == 0 .AND. JCHAN <= 12)
      IF (FREQ <= FRQ1(JCHAN) .AND.  &
                FREQ > FRQ2(JCHAN)) NUMCHAN = JCHAN !   Found
      JCHAN = JCHAN + 1  ! Next channel
    END DO
!
    OLDFREQ = FREQ  ! Save frequency
  END IF
!                                                  Store channel number
  NCHANS(JOB) = NUMCHAN
!                                                      Update histogram
  IF (NUMCHAN > 0) THEN
    NHIST(NUMCHAN) = NHIST(NUMCHAN) + 1
!                                             If not OK, increment NBAD
  ELSE
    NBAD = NBAD + 1
  END IF
END DO DO_obs   ! JOB

!-----------------------------------------------------------------------
!     FIND NUMBER OF DIFFERENT CHANNELS (= NUMBER OF BULLETINS TO MAKE)
!-----------------------------------------------------------------------
!                                  Warning message for unknown channels

IF (NBAD > 0) WRITE (6,'(T5,A,T15,A,I5,A,I5,A)') 'MSGDCODE:', &
          'Unknown channel for', NBAD, ' out of', NOBS,       &
          ' obs in bulletin'
!                           Find how many bulletins to make for storage
NCHOP = 0
DO JCHAN=1,12
  IF (NHIST(JCHAN) > 0) NCHOP = NCHOP + 1
END DO ! JCHAN
!                                            Reject bulletin if all bad
IF (NCHOP == 0) THEN
  NCHOP = -1
!                        Set NCHOP to 0 if it can be stored as received

!             Only 1 channel    No bad obs.     NCHANS big enough
ELSE IF (NCHOP == 1 .AND. NBAD == 0 .AND. NOBS <= MAXOBS) THEN
  NCHOP = 0
END IF
!                                             Return to calling program
RETURN
END SUBROUTINE MSGDCODE
