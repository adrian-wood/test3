SUBROUTINE DEBUFR(DESCR,VALUES,NAMES,ND,NOBS,STRING,DSPLAY)

!-----------------------------------------------------------------------
!
! ROUTINE       : DEBUFR
!
! PURPOSE       : to decode bufr message
!
! DESCRIPTION   : get the descriptors, number of reports and flag to
!               : indicate compression from section 3, skipping
!               : earlier sections, & call decode to do the rest.
!
! CALLED BY     : user...
!
! CALLS         : BUFDATA, DESFXY, VALUE
!
! ARGUMENTS     : (1) sequence of descriptors        (to be returned)
!               : (2) array for values               (to be returned)
!               : (3) for any character values       (to be returned)
!               : (4) number of descriptors          (to be returned)
!               :      (passed as length of descriptor array)
!               : (5) number of reports              (to be returned)
!               :      (passed as length of value array)
!               : (6) bufr message
!               : (7) flag set if display of values required
!
! REVISION INFO :
!
! $Workfile: debufr.f90$ $Folder: OpSource$
! $Revision: 9$ $Date: 21/02/2011 11:30:16$
!
! CHANGE RECORD :
!
! $Log:
!  9    MetDB_Refresh 1.8         21/02/2011 11:30:16    Alison Weir     (:)
!       changed to (*) for BUFRSHEL
!  8    MetDB_Refresh 1.7         09/02/2011 16:44:37    Sheila Needham  Add
!       diagnostic print
!  7    MetDB_Refresh 1.6         17/11/2010 12:23:58    Sheila Needham  Remove
!        redundant call to DESFXY
!  6    MetDB_Refresh 1.5         09/11/2010 13:54:22    Richard Weedon
!       updated to full f95 standard
!  5    MetDB_Refresh 1.4         20/10/2010 10:13:27    Sheila Needham  Remove
!        print statements
!  4    MetDB_Refresh 1.3         20/10/2010 09:16:31    Sheila Needham  F90 up
!        to and including interfaces
!  3    MetDB_Refresh 1.2         18/10/2010 09:33:35    Sheila Needham
!       Developer tests complete
!  2    MetDB_Refresh 1.1         07/10/2010 10:57:41    Sheila Needham  Closer
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

USE bufdata_mod
USE desfxy_mod
USE value_mod

IMPLICIT NONE

!-----------------------------------------------------------------------
! declare variables.
!-----------------------------------------------------------------------

! Arguments

INTEGER         ,INTENT(OUT)   :: DESCR(*) ! Sequence of BUFR descriptors
REAL            ,INTENT(OUT)   :: VALUES(*) ! Decoded values from message
CHARACTER(LEN=*),INTENT(OUT)   :: NAMES ! Decoded character strings from message
INTEGER         ,INTENT(INOUT) :: ND
INTEGER         ,INTENT(INOUT) :: NOBS
CHARACTER(LEN=*),INTENT(IN)    :: STRING
LOGICAL         ,INTENT(IN)    :: DSPLAY  ! Flag for printout of decode

! Local Variables

INTEGER ::  F
INTEGER ::  FLOPT   ! Flag for optional section
INTEGER ::  I
INTEGER ::  IBEFOR  ! Number of bits before value in string
INTEGER ::  L
INTEGER ::  L0      ! Length of entire BUFR message
INTEGER ::  L1      ! Length of BUFR section 2
INTEGER ::  L2      ! Length of BUFR section 2
INTEGER ::  L3      ! Length of BUFR section 3
INTEGER ::  IVER    ! Table B version number
INTEGER ::  LMAX    ! Max pos. in STRING to check for 'BUFR'
INTEGER ::  MAXDES
INTEGER ::  MAXVAL
INTEGER ::  N
INTEGER ::  NED     ! BUFR edition number in message
INTEGER ::  RC      ! Return code from DECODE
INTEGER ::  X
INTEGER ::  Y


CHARACTER(LEN=4) ::  BUFR    ! 'BUFR' in ASCII characters
CHARACTER(LEN=4) ::  SEVENS  ! '7777' in ASCII characters

LOGICAL ::  CMPRES  ! Data compression flag

!-----------------------------------------------------------------------
! initialise variables BUFR and SEVENS (in ASCII).
!-----------------------------------------------------------------------

BUFR=CHAR(66)//CHAR(85)//CHAR(70)//CHAR(82)
SEVENS=CHAR(55)//CHAR(55)//CHAR(55)//CHAR(55)

MAXDES=ND
MAXVAL=NOBS

!-----------------------------------------------------------------------
! find 'BUFR' (in ASCII) at start
!-----------------------------------------------------------------------

LMAX=MIN(100,LEN(STRING))

N=1
DO WHILE (STRING(N:N+3) /= BUFR .AND. N < LMAX)
  N=N+1
END DO

IF (N == LMAX) THEN
  PRINT *,' BUFR NOT FOUND'
  NOBS=0
  RETURN
ELSE
  N=N+4                    ! move past BUFR
ENDIF

!-----------------------------------------------------------------------
! Skip section 1. The length after 'BUFR' is (from edition 2 on) the
! length of the whole message, so skip it first if it points to '7777'.
! The minimum total length is (section by section) 8+18+0+10+4+4=44.
!
! If edition number is 2 or more, skip length anyway.
! (At one time it was not clear whether edition would change from 0
! to 1 or 1 to 2, so checking the edition number alone seemed risky.)
!-----------------------------------------------------------------------

IBEFOR=0
NED = VALUE(STRING(N+3:N+3),IBEFOR,8)   ! BUFR edition no.

IBEFOR=0
L=VALUE(STRING(N:N+2),IBEFOR,24)
IF ((L >= 44 .AND. STRING(N+L-8:N+L-5) == SEVENS) .OR.    &
     &            NED >= 2) THEN
  L0=L                              ! Entire message length
  N=N+4                             ! Move to start of sec. 1
  IBEFOR=0
  L1=VALUE(STRING(N:N+2),IBEFOR,24) ! Length of section 1
ELSE
  L1=L                              ! Length of section 1
ENDIF

!    Get Table B version number from message
IF (NED < 4) THEN
  IBEFOR = 80        ! Edtns. 0-3: Version in byte 11
ELSE
  IBEFOR = 104       ! Edtn. 4 : in byte 14
ENDIF
IVER = VALUE(STRING(N:N+13),IBEFOR,8)

!                           Check flag for presence of optional section
IF (NED < 4) THEN
  IBEFOR = 56        ! Edtns. 0-3: flag in byte 8
ELSE
  IBEFOR = 72        ! Edtn. 4: flag in byte 10
END IF
FLOPT = VALUE(STRING(N:N+9),IBEFOR,1)
N=N+L1

!-----------------------------------------------------------------------
! if there's a section 2 (optional), skip it.
!-----------------------------------------------------------------------

IF (FLOPT == 1) THEN
  IBEFOR=0
  L2=VALUE(STRING(N:N+2),IBEFOR,24)
  N=N+L2
ENDIF

!-----------------------------------------------------------------------
! find no. of reports & compression flag
!-----------------------------------------------------------------------

IBEFOR=0
L3=VALUE(STRING(N:N+2),IBEFOR,24)
IBEFOR=0
NOBS=VALUE(STRING(N+4:N+5),IBEFOR,16)

IBEFOR=1
IF (VALUE(STRING(N+6:N+6),IBEFOR,1) == 1) THEN
  CMPRES=.TRUE.
ELSE
  CMPRES=.FALSE.
ENDIF

! Find number of descriptors & return if there aren't any.

ND=(L3-7)/2
IF (ND <= 0) THEN
  PRINT *,' No descriptors in message ',STRING
  NOBS=0
  RETURN
ENDIF

!-----------------------------------------------------------------------
! copy the descriptors to fullwords
! (consecutive 16-bit fields, so don't reset IBEFOR in between)
!-----------------------------------------------------------------------

IBEFOR=0
DO I=1,ND
  DESCR(I)=VALUE(STRING(N+7:),IBEFOR,16)
END DO

!-----------------------------------------------------------------------
! handle section 4 in bufdata
!-----------------------------------------------------------------------

N=N+L3
CALL BUFDATA(DESCR,VALUES,NAMES,ND,NOBS,STRING(N:),         &
     &             CMPRES,DSPLAY,MAXDES,MAXVAL,IVER,RC)

IF (RC /= 0) THEN
  CALL DESFXY(DESCR(ND),F,X,Y)
  PRINT *,' DEBUFR: RC FROM BUFDATA IS ',RC
  PRINT *,' ERROR',ND,'-TH DESCRIPTOR IS',F*100000+X*1000+Y
  NOBS=0
ENDIF

!-----------------------------------------------------------------------
! return to calling program.
!-----------------------------------------------------------------------

RETURN
END SUBROUTINE DEBUFR
