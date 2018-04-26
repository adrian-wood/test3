SUBROUTINE BUFRHED(MESSAGE,EDITION,TABLES,CENTRE, &
                   DATYPE,DATIME,LOCAL1,LOCAL2,   &
                   CMPRES,NOBS,ND,DESCR,DSPLAY)

!-----------------------------------------------------------------------
!
! PROGRAM        : BUFRHED
!
! PURPOSE        : To return most things other than the data
!                  in section 4 from a BUFR message.
!                  Either everything is returned (by setting ALL
!                  in the 7th argument) or the fields required
!                  are selected by initialising the arguments.
!
! CALLED BY      : WINPRO
!
! CALLS          : VALUE, FXXYYY (both functions)
!
! ARGUMENTS      :
!  1) MESSAGE      char*(*)   (i)    - Input BUFR Message
!  2) EDITION      integer    (i/o)  - Edition number
!                                       (returned if input non-zero)
!  3) TABLES(3)    integer    (i/o)  - Table type & two versions
!                                       (returned if input non-zero)
!  4) CENTRE(2)    integer    (i/o)  - Centre & subcentre
!                                       (returned if input non-zero)
!  5) DATYPE(2)    integer    (i/o)  - Data type & subtype
!                                       (returned if input non-zero)
!  6) DATIME(5)    integer    (i/o)  - Year, month, day, hour, minute
!                                       (returned if input non-zero)
!  7) LOCAL1       character  (i/o)  - Any local extra in section 1
!                                       (returned if string length >1)
!                                       (input 'ALL' if all wanted)
!  8) LOCAL2       character  (i/o)  - Any optional section 2
!                                       (returned if string length >1)
!  9) CMPRES       logical     (o)   - set if data compressed
! 10) NOBS         integer    (i/o)  - number of obs
!                                       (returned if input non-zero)
! 11) ND           integer    (i/o)  - number of descriptors in DESCR
!                                       (returned if input non-zero)
! 12) DESCR(ND)    integer    (i/o)  - descriptors as in section 3
!                                       (returned if ND input non-zero)
! 13) DSPLAY       logical    (i)    - set if data is to be printed
!
! REVISION INFO :
!
! $Revision: 5$
!
!$Log:
! 5    MetDB_Refresh 1.4         28/01/2011 09:41:53    Sheila Needham  Add
!      CONTAINS for local function
! 4    MetDB_Refresh 1.3         22/12/2010 09:22:49    Sheila Needham  Updated
!       following review
! 3    MetDB_Refresh 1.2         16/12/2010 17:13:03    Stan Kellett    added
!      $Log info
! 2    MetDB_Refresh 1.1         08/12/2010 10:10:36    Richard Weedon  add use
!       statement for value_mod
! 1    MetDB_Refresh 1.0         25/11/2010 10:56:30    Richard Weedon  Initial
!       Port. Some work on FXXYYY function may be needed
!$
!
! CHANGE RECORD :
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
! Interfaces
USE value_mod

IMPLICIT NONE
! Arguments

CHARACTER(LEN=*),INTENT(IN)    :: MESSAGE    ! (1) INPUT BUFR MESSAGE
INTEGER,         INTENT(INOUT) :: EDITION    ! (2) BUFR edition number
INTEGER,         INTENT(INOUT) :: TABLES(:)  ! (3)
INTEGER,         INTENT(INOUT) :: CENTRE(:)  ! (4)
INTEGER,         INTENT(INOUT) :: DATYPE(:)  ! (5)
INTEGER,         INTENT(INOUT) :: DATIME(:)  ! (6)
CHARACTER(LEN=*),INTENT(INOUT) :: LOCAL1     ! (7) SECTION 1 EXTRA DATA
CHARACTER(LEN=*),INTENT(INOUT) :: LOCAL2     ! (8) SECTION 2 DATA
LOGICAL         ,INTENT(OUT)   :: CMPRES     ! (9) true if data compressed
INTEGER         ,INTENT(INOUT) :: NOBS       ! (10) number of obs
INTEGER         ,INTENT(INOUT) :: ND         ! (11) number of descriptors
INTEGER         ,INTENT(INOUT) :: DESCR(:)   ! (12)
LOGICAL         ,INTENT(IN)    :: DSPLAY     ! (13) true for print of values

! Local variables

LOGICAL    ::   ALL          ! set to return everything
INTEGER    ::   EDN          ! BUFR edition number
INTEGER    ::   I            ! loop variable
INTEGER    ::   IBEFOR       ! bits before value
INTEGER    ::   INPUTND      ! dimension of DESCR
INTEGER    ::   L1           ! lengths of BUFR sections
INTEGER    ::   L2           ! lengths of BUFR sections
INTEGER    ::   L3           ! lengths of BUFR sections
INTEGER    ::   N            ! pointer to BUFR message
INTEGER    ::   OPTBIT       ! set if section 2 exists

! Initialise
CMPRES = .FALSE.
ALL=LOCAL1 == 'ALL'

! 'BUFR' is followed by a 3-byte length and a 1-byte edition number.
! If the edition number is less than 2, the length is of section 1.
! Otherwise it's the total length of the message, and the length of
! section 1 follows, with master table in the next byte.

INPUTND=ND                            ! keep array dimension
N=4                                   ! skip 'BUFR'
IBEFOR=0
L1=VALUE(MESSAGE(N+1:N+3),IBEFOR,24)  ! L1? or total length?
EDN=ICHAR(MESSAGE(N+4:N+4))
IF (EDITION /= 0) THEN
  EDITION=EDN                         ! return it if requested
  IF (DSPLAY) print *,EDITION,'is BUFR edition'
END IF

IF (EDN >= 2) THEN                    ! L1 is next length
  N=N+4                               ! skip total length
  IBEFOR=0
  L1=VALUE(MESSAGE(N+1:N+3),IBEFOR,24)
  IF (TABLES(1) /= 0) THEN
    TABLES(1)=ICHAR(MESSAGE(N+4:N+4)) ! master table (discipline)
  END IF
END IF

! Return anything requested from section 1
! N.B. Subcentre comes BEFORE centre - for historical reasons!

IF (ALL .OR. CENTRE(1) /= 0) THEN
  CENTRE(2)=ICHAR(MESSAGE(N+5:N+5))   ! generating subcentre
  CENTRE(1)=ICHAR(MESSAGE(N+6:N+6))   ! generating centre
  IF (DSPLAY) print *,CENTRE(1),'is generating centre'
  IF (DSPLAY) print *,CENTRE(2),'is generating subcentre'
END IF

IF (ALL .OR. DATYPE(1) /= 0) THEN
  DATYPE(1)=ICHAR(MESSAGE(N+9:N+9))   ! data category
  DATYPE(2)=ICHAR(MESSAGE(N+10:N+10)) ! data subcategory
  IF (DSPLAY) print *,DATYPE(1),'is data category'
  IF (DSPLAY) print *,DATYPE(2),'is data subcategory'
END IF

IF (ALL .OR. TABLES(1) /= 0) THEN
  TABLES(2)=ICHAR(MESSAGE(N+11:N+11)) ! master tables version
  TABLES(3)=ICHAR(MESSAGE(N+12:N+12)) ! local tables version
  IF (DSPLAY) print *,TABLES(1),'is discipline of master tables'
  IF (DSPLAY) print *,TABLES(2),'is version of master tables'
  IF (DSPLAY) print *,TABLES(3),'is version of local tables'
END IF

IFCONST1 : &
IF (ALL .OR. DATIME(1) /= 0) THEN
  DATIME(1)=ICHAR(MESSAGE(N+13:N+13)) ! year (of century)
  DATIME(2)=ICHAR(MESSAGE(N+14:N+14)) ! month
  DATIME(3)=ICHAR(MESSAGE(N+15:N+15)) ! day
  DATIME(4)=ICHAR(MESSAGE(N+16:N+16)) ! hour
  DATIME(5)=ICHAR(MESSAGE(N+17:N+17)) ! minute

  IF (DATIME(1) >= 85) DATIME(1)=DATIME(1)+1900
  IF (DATIME(1) < 85) DATIME(1)=DATIME(1)+2000

  WRITE (*,1) DATIME(1)*10000+DATIME(2)*100+DATIME(3), &
     DATIME(4)*100+DATIME(5)
    1     FORMAT (I10,'/',I4.4,'Z is time in section 1')
END IF IFCONST1

! If section 1 has local data (length>18) return that if wanted
! And see if there's an optional section 2.

IF (L1 > 18 .AND. (ALL .OR. LEN(LOCAL1) > 1)) THEN
  LOCAL1=MESSAGE(N+18:N+L1)
  IF (DSPLAY) print *,LOCAL1,' is local part of section 1'
END IF

OPTBIT=ICHAR(MESSAGE(N+8:N+8))/128
N=N+L1

! If there's a section 2 (optional), return it if wanted

IF (OPTBIT == 1) THEN
  IBEFOR=0
  L2=VALUE(MESSAGE(N+1:N+3),IBEFOR,24)
  IF (ALL .OR. LEN(LOCAL2) > 1) THEN
    LOCAL2=MESSAGE(N+5:N+L2)
    IF (DSPLAY) print *,LOCAL2,' is optional section 2'
  END IF
  N=N+L2
END IF

! Return compression flag (2nd bit in byte 7; top bit is observed/not)

IBEFOR=1
CMPRES=VALUE(MESSAGE(N+7:N+7),IBEFOR,1) == 1

! Number of obs (reports, data subsets...)

IBEFOR=0
IF (ALL .OR. NOBS /= 0) THEN
  NOBS=VALUE(MESSAGE(N+5:N+6),IBEFOR,16)
  IF (DSPLAY) THEN
    IF (CMPRES) print *,NOBS,'obs, compressed'
    IF (.NOT.CMPRES) print *,NOBS,'obs, not compressed'
  END IF
END IF

! Return descriptors (in 16-bit form) avoiding array overflow.

IFCONST2 : &
IF (ALL .OR. INPUTND /= 0) THEN
  IBEFOR=0
  L3=VALUE(MESSAGE(N+1:N+3),IBEFOR,24)
  ND=(L3-7)/2

  IF (ND > INPUTND) THEN
    PRINT *,ND,'descriptors in message',INPUTND,'fill array!'
    ND=INPUTND
  END IF

  IBEFOR=56  ! skip first 7 bytes (including length) of section 3
  DO I=1,ND
    DESCR(I)=VALUE(MESSAGE(N+1:N+1),IBEFOR,16)
  END DO

  IF (DSPLAY) print *,ND,'descriptors:'
  IF (DSPLAY) WRITE (*,'((10I8.6))') (FXXYYY(DESCR(I)),I=1,ND)
END IF IFCONST2

RETURN
!-----------------------------------------------------------------------

  CONTAINS

    FUNCTION FXXYYY(DESCR)
    !
    USE DESFXY_mod
    IMPLICIT NONE
    INTEGER  ::  DESCR
    INTEGER  ::  F
    INTEGER  ::  X
    INTEGER  ::  Y
    INTEGER  ::  FXXYYY
    CALL DESFXY(DESCR,F,X,Y)
    FXXYYY=F*100000+X*1000+Y
    RETURN
    END FUNCTION FXXYYY
!-----------------------------------------------------------------------

END SUBROUTINE BUFRHED
