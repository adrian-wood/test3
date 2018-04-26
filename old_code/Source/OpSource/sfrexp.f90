SUBROUTINE SFREXP(HEX,ARRAY,NELEM)

!-----------------------------------------------------------------------
!
! PROGRAM       : SFREXP
!
! PURPOSE       : Expand bit string of SFERICS fixes
!
! CALLED BY     : SFERIC
!
! CALLS         : 'CONTAIN'ed function SFRNUM, CENTURY
!
! ARGUMENTS     : (1) SFERICS bulletin (in readable hex)            (i)
!                 (2) value array (not yet ready for BUFR encoding)(io)
!                 (3) number of values in array                     (o)
!
! REVISION INFO :
!
! $Workfile: sfrexp.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 22/01/2011 15:38:52$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         22/01/2011 15:38:52    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         17/01/2011 16:06:08    Sheila Needham
!       Initial F77 version
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

! Interfaces

USE century_mod

IMPLICIT NONE

! Arguments

CHARACTER(LEN=*),INTENT(IN)    :: HEX         !(1)
REAL,            INTENT(INOUT) :: ARRAY(:)    !(2)
INTEGER,         INTENT(OUT)   :: NELEM       !(3)

! Local Variables

INTEGER          :: ADLAT
INTEGER          :: ADLONG
CHARACTER(LEN=4) :: BINARY(0:15)
CHARACTER(LEN=1) :: BITS(60000) ! 4 times max bulletin length of 15K
INTEGER          :: DELETE
INTEGER          :: FIXEL
INTEGER          :: FLAG(4)
INTEGER          :: I
INTEGER          :: IEND
INTEGER          :: J
INTEGER          :: K
INTEGER          :: L
INTEGER          :: LAT
INTEGER          :: LONG
INTEGER          :: N
INTEGER          :: NFLAG
INTEGER          :: NREJECT
INTEGER          :: YEAR

DATA BINARY /'0000','0001','0010','0011',   &
             '0100','0101','0110','0111',   &
             '1000','1001','1010','1011',   &
             '1100','1101','1110','1111'/
!
! Convert the figures and letters representing hex (as transmitted) to a
! string of figures (0 or 1) representing the corresponding bit string.
! The string can contain CRLF, so ignore any characters other than
! letters & figures.
!
L=0                                 ! length of output string
dolabel1: &
DO I=1,LEN(HEX)-1
  IF (HEX(I:I) >= '0' .AND. HEX(I:I) <= '9') THEN
    N=ICHAR(HEX(I:I))-ICHAR('0')    ! figure
    BITS(L+1)=BINARY(N)(1:1)        ! corresponding bits
    BITS(L+2)=BINARY(N)(2:2)
    BITS(L+3)=BINARY(N)(3:3)
    BITS(L+4)=BINARY(N)(4:4)
    L=L+4                           ! 4 nore bits in output string
  ELSE IF (HEX(I:I) >= 'A' .AND. HEX(I:I) <= 'F') THEN
    N=ICHAR(HEX(I:I))-ICHAR('A')+10 ! letter
    BITS(L+1)=BINARY(N)(1:1)        ! corresponding bits
    BITS(L+2)=BINARY(N)(2:2)
    BITS(L+3)=BINARY(N)(3:3)
    BITS(L+4)=BINARY(N)(4:4)
    L=L+4
  END IF                            ! skip if not letter or figure
END DO dolabel1
!
! A zero has been inserted wherever five successive ones occur in the
! bit string.  Delete any inserted zeros and at the same time note the
! positions of the flags (111111) delimiting parts 1 & 2.
!
DELETE=1
I=1
NFLAG=0
dolabel2: &
DO WHILE (I < L-13-DELETE .AND. NFLAG < 4)

iflabel1: &
  IF (BITS(I) == '1' .AND. BITS(I+1) == '1' .AND.   &
    BITS(I+2) == '1' .AND. BITS(I+3) == '1' .AND.   &
    BITS(I+4) == '1') THEN
    IF (BITS(I+5) == '0') THEN
      DO J=I+5,L-DELETE
        BITS(J)=BITS(J+1)
      END DO
      DELETE=DELETE+1
      I=I+5
    ELSE IF (BITS(I+5) == '1' .AND. BITS(I+6) == '0') THEN
      NFLAG=NFLAG+1
      FLAG(NFLAG)=I-1
      I=I+7
    ELSE
      I=I+1
    END IF
  ELSE
    I=I+1
  END IF iflabel1
END DO dolabel2
!----------------------------------------------------------------------
!
! The bit string now consists of fix sequences, in which each absolute
! position may be followed by several relative ones; these sequences
! come in two parts, delimited by flags, each starting with time etc.
! Loop round the two parts, expanding first the day etc then the fixes.
!
!----------------------------------------------------------------------
I=FLAG(1)+24
IEND=FLAG(2)
!                         add 24 to skip flag itself, part no & control
IF (NFLAG == 2) THEN
  FIXEL=13
ELSE
  print *,'SFREXP:',NFLAG,'flags found, so returning'
  ARRAY(15)=0       ! set number of fixes to zero
  RETURN
END IF
!
! Date/time is given in 4-bit figures rather than true hex
! (2-figure year, so use standard function to decide century!)
!
YEAR=SFRNUM(BITS,I,I+3)*10+SFRNUM(BITS,I+4,I+7)
ARRAY(1)=YEAR+CENTURY(YEAR) ! year

! month, day, hour, minute (2 figures each)

ARRAY(2)=SFRNUM(BITS,I+8,I+11)*10+SFRNUM(BITS,I+12,I+15)
ARRAY(3)=SFRNUM(BITS,I+16,I+19)*10+SFRNUM(BITS,I+20,I+23)
ARRAY(4)=SFRNUM(BITS,I+24,I+27)*10+SFRNUM(BITS,I+28,I+31)
ARRAY(5)=SFRNUM(BITS,I+32,I+35)*10+SFRNUM(BITS,I+36,I+39)

! ARRAY(6) & ARRAY(7) will be set at the end
!     ARRAY(J+5)=SFRNUM(BITS,I+40,I+47)   ! stations
! Check bits for stations involved before setting pointers below?

ARRAY(8)=1                          ! pointer to name
ARRAY(9)=10                         ! pointer to name
ARRAY(10)=19                        ! pointer to name
ARRAY(11)=28                        ! pointer to name
ARRAY(12)=37                        ! pointer to name
ARRAY(13)=46                        ! pointer to name
ARRAY(14)=55                        ! pointer to name
NREJECT=SFRNUM(BITS,I+48,I+55)      ! reject count
I=I+56
J=15                                ! 15 header values (with count
!
! Start with absolute fix sequence (lat, long, error, stations).  If
! no data, 11111 marks the end.  A latitude can't start with 11111
! because 4 ones after the sign bit would give a latitude of -96.)
!
! Convert lat in fortieths & long in twentieths to hundredths
!
dolabel3: &
DO WHILE (.NOT.(BITS(I) == '1' .AND. BITS(I+1) == '1'    &
        .AND. BITS(I+2) == '1' .AND. BITS(I+3) == '1'    &
        .AND. BITS(I+4) == '1') .AND. I < IEND)
!
! 13-bit longitude in two's complement form
!
  LONG=SFRNUM(BITS,I+13,I+25)
  IF (BITS(I+13) == '1') LONG=LONG-2**13
  LONG=LONG*5
  ARRAY(J+10)=LONG/100.
!
! 12-bit latitude preceded by sign bit
!
  LAT=SFRNUM(BITS,I+1,I+12)
  LAT=LAT*5/2
  IF (BITS(I) == '1') LAT=-LAT
  ARRAY(J+9)=LAT/100.

! Encode range from 0 to 1.5**N km (but set value of 006021 in metres)

  N=SFRNUM(BITS,I+26,I+29)          ! estimated error
  ARRAY(J+11)=0
  ARRAY(J+12)=(1.5**N)*1000.        ! convert to km (& m)
  ARRAY(J+13)=NREJECT               ! threshold

  ARRAY(J+1)=7                      ! number of stations
  DO K=1,7                          ! bit for each station
    ARRAY(J+1+K)=SFRNUM(BITS,I+29+K,I+29+K)
    IF (ARRAY(J+1+K) == 1) THEN     ! flip bit for 031031:
      ARRAY(J+1+K)=0                ! zero if station involved
    ELSE IF (ARRAY(J+1+K) == 0) THEN
      ARRAY(J+1+K)=1
    END IF
  END DO
  J=J+13                            ! 13 values per fix
  I=I+37
!
! Now look for fix sequences relative to this position.  There may be
! none, in which case 11111 follows, then either another absolute
! position or the end of the part (another 11111 terminator).
! (max lat increment is 30 fortieths, not 31, to avoid terminator)
!
dolabel4: &
  DO WHILE (.NOT.(BITS(I) == '1' .AND. BITS(I+1) == '1'  &
          .AND. BITS(I+2) == '1' .AND. BITS(I+3) == '1'  &
          .AND. BITS(I+4) == '1') .AND. I < IEND)
!
! Expand relative fix sequence (lat & long increments, error, stations)
! (latitude increment can only be negative, longitude is signed)
!
    ADLAT=SFRNUM(BITS,I,I+4)
    ARRAY(J+9)=(LAT-ADLAT*5/2)/100.

    ADLONG=SFRNUM(BITS,I+5,I+9)
    IF (BITS(I+5) == '1') ADLONG=ADLONG-2**5
    ARRAY(J+10)=(LONG+ADLONG*5)/100.

    N=SFRNUM(BITS,I+10,I+13)        ! estimated error
    ARRAY(J+11)=0
    ARRAY(J+12)=(1.5**N)*1000.      ! convert to km (& m)
    ARRAY(J+13)=NREJECT             ! threshold

    ARRAY(J+1)=7                    ! number of stations
    DO K=1,7                        ! bit for each station
      ARRAY(J+1+K)=SFRNUM(BITS,I+13+K,I+13+K)
      IF (ARRAY(J+1+K) == 1) THEN   ! flip bit for 031031:
        ARRAY(J+1+K)=0              ! zero if station involved
      ELSE IF (ARRAY(J+1+K) == 0) THEN
        ARRAY(J+1+K)=1
      END IF
    END DO
    J=J+13                          ! 13 values per fix
    I=I+21
  END DO dolabel4
  I=I+5                             ! past 11111 for last rel. fix
END DO dolabel3
!
! Store selector station & net gain (db).  First search (back from last
! 6-ones flag) for previous terminator to see if there is room between
! it and the end flag for extra two items. The CRC and 2 extra items
! are aligned back from the end flag.
!  The selector station is indicated by an 8-bit number, encoded as a
! 4-bit code figure:
!    (0 - not used now, but may be in future!)
!     1 - target system (Beaufort Park)
!     2 - Hemsby
!     3 - Camborne
!     4 - Stornoway
!     5 - Lerwick
!     6 - Cyprus
!    (7 - not used)
!     8 - B-model (Beaufort Park)
!     9 - Gibraltar
!
K=1
DO WHILE (.NOT.(BITS(IEND-16-K-4) == '1' .AND.      &
                BITS(IEND-16-K-3) == '1' .AND.      &
                BITS(IEND-16-K-2) == '1' .AND.      &
                BITS(IEND-16-K-1) == '1' .AND.      &
                BITS(IEND-16-K) == '1') .AND. I < IEND)
  K=K+1
END DO

IF (K >= 17) THEN
  ARRAY(6)=SFRNUM(BITS,IEND-32,IEND-25)
  ARRAY(7)=SFRNUM(BITS,IEND-24,IEND-17)
ELSE
  ARRAY(6)=0
  ARRAY(7)=0
END IF
!
! Store the number of fixes (from array subscript & elements per fix)
!
NELEM=J
ARRAY(15)=(J-15)/FIXEL
RETURN
CONTAINS
!
!--------------------------------------------------------------------
! Function to get number from string of 0's or 1's (readable characters)
!
  FUNCTION SFRNUM(BITS,START,END)
  IMPLICIT NONE
! Arguments
  CHARACTER(LEN=1),INTENT(IN) :: BITS(:)
  INTEGER,         INTENT(IN) :: START
  INTEGER,         INTENT(IN) :: END
  INTEGER                     :: SFRNUM
! Local variables
  INTEGER :: TWOTON
  INTEGER :: I

  SFRNUM=0
  TWOTON=1
  DO I=END-START,0,-1
    IF (BITS(START+I) == '1') SFRNUM=TWOTON+SFRNUM
    TWOTON=TWOTON*2
  END DO
  RETURN
  END FUNCTION SFRNUM
END SUBROUTINE SFREXP
