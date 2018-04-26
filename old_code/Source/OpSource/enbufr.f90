SUBROUTINE ENBUFR(DESCR,VALUES,ND,NELEM,NOBS,NAMES,DATIME,MESAGE,  &
                        CMPRES,L,IVER)

!-----------------------------------------------------------------------
!
! ROUTINE       : ENBUFR
!
! PURPOSE       : to make a complete bufr message, given a descriptor
!               : sequence (which will be expanded if necessary) and
!               : an array of values corresponding to the expanded
!               : sequence (i.e. the user must know the expansion
!               : beforehand!)
!
! CALLED BY     : user...
!
! CALLS         : DESFXY, ENCODE, VALUE
!
! ARGUMENTS     : (1) descriptor sequence (may need expanding)
!                 (2) nobs*nelem array of values
!                 (3) number of descriptors (input, then output;
!                      returned as zero if there's an error.)
!                 (4) number of elements in array (nelem)
!                 (5) number of observations in array (nobs)
!                 (6) any character values (with pointers in array)
!                 (7) five integers: year, month, day, hour, minute
!                 (8) character string for bufr message
!                 (9) flag set if compression required
!                 (10) total length of bufr message
!                 (11) Table B version number
!
! REVISION INFO :
!
! $Workfile: enbufr.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:58:02    Sheila Needham  Closer
!        to F90 standard
!  1    MetDB_Refresh 1.0         04/10/2010 09:50:06    Sheila Needham
!       Initial f90 port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE value_mod
USE desfxy_mod
USE encode_mod

IMPLICIT NONE

INTEGER :: DATIME(5)
INTEGER :: DESCR(*)
INTEGER :: F
INTEGER :: FLOPT       ! flag for optional section
INTEGER :: I
INTEGER :: IBEFOR      ! number of bits before value in string
INTEGER :: ID
INTEGER :: L
INTEGER :: L1
INTEGER :: L2
INTEGER :: L3
INTEGER :: L4
INTEGER :: N
INTEGER :: ND
INTEGER :: NED         ! BUFR edtion number
INTEGER :: NELEM
INTEGER :: NOBS
INTEGER :: IVER     ! Table B version number
INTEGER :: NYEAR
! INTEGER :: VALUE     ! function to get integer from bits
INTEGER :: X
INTEGER :: Y

LOGICAL :: CMPRES
LOGICAL :: FIRST

REAL :: VALUES(NOBS,NELEM)

CHARACTER(LEN=4) :: BUFR
CHARACTER(LEN=2) :: CCCCNO
CHARACTER(LEN=*) :: MESAGE
CHARACTER(LEN=*) :: NAMES
CHARACTER(LEN=4) :: SEVENS

!-----------------------------------------------------------------------
! Save all variables.
!-----------------------------------------------------------------------

SAVE

!-----------------------------------------------------------------------
! Data statements
!-----------------------------------------------------------------------

DATA FIRST/.TRUE./

!-----------------------------------------------------------------------
! Revision information.
! Initialise variables BUFR and SEVENS (in ASCII).
!-----------------------------------------------------------------------

IF (FIRST) THEN
  FIRST=.FALSE.
  BUFR= CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)
  SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)
END IF

!-----------------------------------------------------------------------
!  if number of descriptors passed as zero, use the descriptors in the
!  message passed: look for 'bufr', then use the section lengths (skip-
!  ping any section 2) to reach the descriptors in section 3.
!-----------------------------------------------------------------------

IF (ND == 0) THEN
  N=1
   10   IF (MESAGE(N:N+3) /= BUFR) THEN
    IF (N < 100) THEN
      N=N+1
      GO TO 10
    ELSE
      PRINT *,' NO DESCRIPTORS PASSED, BUT NO BUFR MESSAGE EITHER'
      RETURN
    END IF
  END IF
  N=N+4

!-----------------------------------------------------------------------
! skip section 1.  the length after 'bufr' is (from edition 2 on) the
! length of the whole message, so skip it first if it points to '7777'.
! the minimum total length is (section by section) 8+18+0+10+4+4=44.
!-----------------------------------------------------------------------

  IBEFOR = 0
  NED = VALUE(MESAGE(N+3:N+3),IBEFOR,8)  ! BUFR edition no.

  IBEFOR=0
  L=VALUE(MESAGE(N:N+2),IBEFOR,24)
  IF (L >= 44 .AND. MESAGE(N+L-8:N+L-5) == SEVENS) THEN
    N=N+4
    IBEFOR=0
    L1=VALUE(MESAGE(N:N+2),IBEFOR,24)
  ELSE
    L1=L
  END IF
  IF (NED == 2 .OR. NED == 3) THEN
    CCCCNO = CHAR(0) // MESAGE(N+5:N+5)  ! Ignore sub-centre
  ELSE
    CCCCNO = MESAGE(N+4:N+5)
  END IF
!                           Check flag for presence of optional section
  IF (NED < 4) THEN
    IBEFOR = 56        ! Edtns. 0-3: flag in byte 8
  ELSE
    IBEFOR = 72        ! Edtn. 4: flag in byte 10
  END IF
  FLOPT = VALUE(MESAGE(N:N+9),IBEFOR,1)
  N=N+L1

!-----------------------------------------------------------------------
! if there's a section 2 (optional), skip it.
!-----------------------------------------------------------------------

  IF (FLOPT == 1) THEN
    IBEFOR=0
    L2=VALUE(MESAGE(N:N+2),IBEFOR,24)
    N=N+L2
  END IF

END IF !- nd == 0

!-----------------------------------------------------------------------
!  set up section 1 of message (section 2 missing) with 'BUFR' at start
!-----------------------------------------------------------------------

MESAGE(1:4)=BUFR

! length

MESAGE(5:7)=CHAR(0)//CHAR(0)//CHAR(18)

! bufr revision number

MESAGE(8:8)=CHAR(0)

! originating centre (country/centre)
! if nd=0, cccc as in input message.
! if nd>0, set met office cccc

IF (ND == 0) MESAGE(9:10)=CCCCNO
IF (ND > 0) MESAGE(9:10)=CHAR(0)//CHAR(74)

! cor sequence number

MESAGE(11:11)=CHAR(0)

! optional section flag in first bit

MESAGE(12:12)=CHAR(0)

! bufr message type (from table a)

MESAGE(13:13)=CHAR(255)

! message subtype (local number)

MESAGE(14:14)=CHAR(0)

! number of non-standard table set

MESAGE(15:16)=CHAR(0)//CHAR(1)

! year, month, day, hour, minute
! (century-year is 100 (not 0!) when year is a multiple of 100)

NYEAR=MOD(DATIME(1),100)
IF (NYEAR == 0) NYEAR=100
MESAGE(17:17)=CHAR(NYEAR)

DO I=2,5
  MESAGE(16+I:16+I)=CHAR(DATIME(I))
END DO

! finally one byte of padding

MESAGE(22:22)=CHAR(0)

!----------------------------------------------section 3----------------
! if number of descriptors was zero, copy descriptors from message
!-----------------------------------------------------------------------

IF (ND == 0) THEN
  IBEFOR=0
  L3=VALUE(MESAGE(N:N+2),IBEFOR,24)
  ND=(L3-7)/2
  DO I=1,ND
    ID=N+7+(I-1)*2
    IBEFOR=0
    DESCR(I)=VALUE(MESAGE(ID:ID+1),IBEFOR,16)
  END DO
END IF

!----------------------------------------------section 3----------------
! put descriptors (plus number of obs & compression flag) in section 3
!
! L is length of section 3 (7 bytes before descriptors, padding at end)
!-----------------------------------------------------------------------

L=7+ND*2+1
MESAGE(23:25)=CHAR(0)//CHAR(L/256)//CHAR(MOD(L,256))
MESAGE(26:26)=CHAR(0)

! number of observations

MESAGE(27:28)=CHAR(NOBS/256)//CHAR(MOD(NOBS,256))

! observed data, may be compressed

IF (CMPRES) THEN
  MESAGE(29:29)=CHAR(128+64)
ELSE
  MESAGE(29:29)=CHAR(128)
END IF

! split descriptor into fxx & yyy

DO I=0,ND-1
  CALL DESFXY(DESCR(I+1),F,X,Y)

!-----------------------------------------------------------------------
! the category of a class d descriptor for a whole observation corres-
! ponds to the kind of data in class a, so set that byte in section 1.
!-----------------------------------------------------------------------

  IF (I == 0 .AND. F == 3 .AND. X >= 7 .AND. X <= 12) THEN
    MESAGE(13:13)=CHAR(X-7)
  END IF

! put fxx in one byte & yyy in other

  MESAGE(30+I*2:30+I*2)=CHAR(F*64+X)
  MESAGE(31+I*2:31+I*2)=CHAR(Y)
END DO

!-----------------------------------------------------------------------
! now encode the bit string in section 4 and put 7777 at the end.
! (arguments for bufr4 as for this program, but l4 & no datime)
!
! L=length of sections 0,1 & 3  (no section 2)
!-----------------------------------------------------------------------

L=4+18+L

CALL ENCODE(DESCR,VALUES,ND,NELEM,NOBS,NAMES,MESAGE(L+1:),  &
     &            CMPRES,L4,IVER)

!-----------------------------------------------------------------------
! l4 is returned as zero if there's an error in bufr4.  if so, return
! nd=0 to warn the user to stop; if not, put 7777 at the end.
!-----------------------------------------------------------------------

IF (L4 == 0) THEN
  ND=0
ELSE
  L=L+L4
  MESAGE(L+1:L+4)=SEVENS
  L=L+4
END IF

RETURN
END SUBROUTINE ENBUFR
