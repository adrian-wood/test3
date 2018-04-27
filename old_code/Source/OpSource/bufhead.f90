SUBROUTINE BUFHEAD (BULL)

IMPLICIT NONE
INTEGER :: IPT              ! Pointer to BUFR bulletin
INTEGER :: I1,I2,I3
INTEGER :: NT
INTEGER :: I
INTEGER :: NOW(9)           ! Date/time in BUFR section 1
INTEGER :: IVER             ! BUFR version number

LOGICAL :: SECT2

CHARACTER(*) ::  BULL

IVER = ICHAR(BULL(8:8))            ! BUFR version number

IF (IVER.LT.2) THEN
  IPT = 8
ELSE
  IPT = 12
END IF
!
I1 = ICHAR(BULL(IPT:IPT))          ! BUFR Master table code
WRITE (6,'(T8,A,2I4)')         &
     &         'BUFR version and Master Table .........', IVER, I1

IF (IVER.EQ.4) THEN
  I1 = ICHAR(BULL(IPT+10:IPT+10))  ! Master table version
  I2 = ICHAR(BULL(IPT+11:IPT+11))  ! Local table version
ELSE
  I1 = ICHAR(BULL(IPT+7:IPT+7))    ! Master table version
  I2 = ICHAR(BULL(IPT+8:IPT+8)) ! Local table version
END IF
WRITE (6,'(T8,A,2I4)')                    &
     &         'BUFR Master & Local Table versions ....', I1, I2

IF (IVER.EQ.3) THEN
  I1 = ICHAR(BULL(IPT+2:IPT+2))    ! Originating sub-centre
  I2 = ICHAR(BULL(IPT+1:IPT+1))    ! Originating centre
  IPT = 15
ELSE
  I1 = ICHAR2(BULL(IPT+1:IPT+2))   ! Originating sub-centre
  I2 = ICHAR2(BULL(IPT+3:IPT+4))   ! Originating centre
  IPT = 17
  IF (IVER.LT.3) IPT = 11
END IF
IF (IVER.GE.3) THEN
  WRITE (6,'(T8,A,2I4)')                   &
     &           'Originating centre & subcentre ........', I1, I2
ELSE
  WRITE (6,'(T8,A,2I4)')                  &
     &           'Originating centre ....................', I1
END IF

I1 = ICHAR(BULL(IPT:IPT))          ! Update sequence number
I2 = ICHAR(BULL(IPT+1:IPT+1))      ! Section 2 indicator
SECT2 = I2.GE.128                  ! Section 2 flag
WRITE (6,'(T8,A,I4,L4)')      &
     &         'Update sequence & Section 2 flag ......', I1, SECT2

I1 = ICHAR(BULL(IPT+2:IPT+2))      ! Data category (Table A)
I2 = ICHAR(BULL(IPT+3:IPT+3))      ! Data sub-category
IF (IVER.EQ.4) THEN
  I3 = ICHAR(BULL(IPT+4:IPT+4))    ! Local data sub-category
  WRITE (6,'(T8,A,3I4)')      &
     &           'Data category and sub-categories ......', I1, I2, I3
  IPT = 26
ELSE
  WRITE (6,'(T8,A,3I4)')      &
     &           'Data category and sub-category ........', I1, I2
  IPT = IPT + 7
END IF

IF (IVER.EQ.4) THEN
  NOW(1) = ICHAR2(BULL(IPT-2:IPT-1))           ! Year
  NT = 6
ELSE
  CALL DATIM (NOW)
  NOW(1) = ICHAR(BULL(IPT-1:IPT-1)) + 2000     ! Year
  IF (NOW(1).GT.NOW(8)) NOW(1) = NOW(1) - 100
  NT = 5
END IF
DO I=2,5
  NOW(I) = ICHAR(BULL(IPT:IPT))    ! Month, day, hour, minute
  IPT = IPT + 1
END DO
IF (IVER.EQ.4) NOW(6) = ICHAR(BULL(IPT:IPT))   ! Second
  WRITE (6,'(T8,A,6I4)')     &
     &           'Date and time .................... ', (NOW(I),I=1,NT)

RETURN
CONTAINS
      INTEGER FUNCTION ICHAR2 (C2)
      IMPLICIT NONE
!                                                             Variables
      CHARACTER*2   C2   ! 2 characters to convert to integer
!                                       Convert 2 characters to integer
      ICHAR2 = 256*ICHAR(C2(1:1)) + ICHAR(C2(2:2))
!                                                                Return
      RETURN
      END FUNCTION ICHAR2

END SUBROUTINE BUFHEAD
