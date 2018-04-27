SUBROUTINE BATHY(REPORT,TTAAII,CCCC,OCOR,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : BATHY
!
! PURPOSE       : To expand, encode & store one BATHY report
!
! CALLED BY     : BATESBUL
!
! CALLS         : BTHSCn (n=1,4), BTHIND,
!                 ENBUFR, CCCODE, AIRSTO
!
! ARGUMENTS     : REPORT   to be expanded & stored                  (I)
!                 TTAAii   bulletin identifier                      (I)
!                 CCCC     originating centre                       (I)
!                 OCOR     correction flag                          (I)
!                 NFT      FT number for BATHY & TESAC              (I)
!
! REVISION INFO:
!
! $Workfile: bathy.f90$ $Folder: OpSource$
! $Revision: 6$ $Date: 25/01/2011 15:30:49$
!
! CHANGE RECORD:
!
! $Log:
!  6    MetDB_Refresh 1.5         25/01/2011 15:30:49    Rosemary Lavery minor
!       uopdates post review
!  5    MetDB_Refresh 1.4         19/01/2011 10:23:42    Richard Weedon  final
!       version
!  4    MetDB_Refresh 1.3         19/01/2011 10:22:16    Richard Weedon
!       amended copyright information
!  3    MetDB_Refresh 1.2         19/01/2011 10:18:01    Richard Weedon  Tidied
!        up
!  2    MetDB_Refresh 1.1         19/01/2011 10:13:41    Richard Weedon  added
!       if_constr1 and renamed comments on var declarations
!  1    MetDB_Refresh 1.0         18/01/2011 13:49:56    Richard Weedon  Passes
!        basic compilation test. Extra var added to enbufr call.
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2011 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
! Modules
USE bthsc1_mod
USE bthsc2_mod
USE bthsc3_mod
USE bthsc4_mod
USE bthind_mod
USE enbufr_mod
USE cccode_mod
USE airsto_mod
USE ides_mod
USE datim_mod
!
!
IMPLICIT NONE
!
! For alphanumeric BATHY bulletins without optional sections and
! with n levels, the following approximate lengths in bytes apply:
!    Original message: 6n+96      BUFR message: 4.25n+77
!    Message trailer:  23         Total length: 10.25n+196
!    BUFR descriptors: 2n+21      Values for encoding: 4n+40
! Values in the PARAMETER statement below allow for up to 1600 levels
! in the BUFR message but a bit more in the original. The highest
! seen to date (24/4/2006) is 1347 levels. A check in BTHSC2 limits
! the BUFR message to 1600 levels - this will need changing as well
! as parameters below if there is a need to increase the limit.
!
! Arguments
CHARACTER(LEN=*),INTENT(IN)        ::       REPORT
CHARACTER(LEN=*),INTENT(IN)        ::       TTAAII
CHARACTER(LEN=4),INTENT(IN)        ::       CCCC
LOGICAL,INTENT(IN)                 ::       OCOR
INTEGER,INTENT(IN)                 ::       NFT   ! FT number for storag

! Local Variable Declarations
INTEGER,PARAMETER  ::   LEVELS=1600         ! Max. levels in BUFR mess
INTEGER,PARAMETER  ::   LARRAY=2*LEVELS+21  ! Size of descriptor array
INTEGER,PARAMETER  ::   LSTRING=17200       ! Length of MESAGE string
INTEGER,PARAMETER  ::   LVALS=2*LARRAY      ! Size of EXPARR array
INTEGER,PARAMETER  ::   IVER = 13           ! current version (to enbufr)

CHARACTER(LEN=9)       ::   ID
CHARACTER(LEN=9)       ::   IDENT        !- copy of ID
! CHARACTER(LEN=4)     ::   MIMJ
CHARACTER(LEN=23)      ::   ENTRY
CHARACTER(LEN=LSTRING) ::   MESAGE

REAL               ::    EXPARR(0:LVALS)
REAL               ::    TOTDEP      ! total depth

INTEGER            ::    REPLEN      ! length of character string
INTEGER            ::    MESLEN      ! length of BUFR message
INTEGER            ::    DATIME(5)
INTEGER            ::    I
INTEGER            ::    IERR
INTEGER            ::    POS         ! pointer to charr string
INTEGER            ::    ARAYPOS     ! array pointer (to end of profile)
INTEGER            ::    BLKSIZ
INTEGER            ::    ICCCC       ! number corresponding to CCCC
INTEGER            ::    DESCR(LARRAY)
INTEGER            ::    TOR(5)
INTEGER            ::    NOW(8)
INTEGER            ::    NOBS
INTEGER            ::    NDESCR

LOGICAL            ::    CMPRES
LOGICAL            ::    IDFLG         ! set if call sign, not number


! Set first element in array to indicate that every
! other element is quality control bit - not as yet used

EXPARR(0)=1.0
DO I=1,LVALS
  EXPARR(I)=-9999999.0
END DO

! Expand the report section by section, starting with section 4 to get
! the call sign.  Only go on past section 1 if there's no error.
! Keep a copy of ID in IDENT. This is because ENBUFR will convert ID
! from EBCDIC to ASCII and we need to pass an un-converted ID to AIRSTO.

REPLEN=LEN(REPORT)
CALL BTHSC4(REPORT,REPLEN,EXPARR,IDFLG,ID)
IDENT=ID

POS=1
CALL BTHSC1(REPORT,REPLEN,EXPARR,POS,DATIME,REPORT(1:4),IERR)

if_constr1 : &
IF (IERR == 0) THEN
  CALL BTHSC2(REPORT,REPLEN,EXPARR,TOTDEP,POS,ARAYPOS)
  CALL BTHSC3(REPORT,REPLEN,EXPARR,TOTDEP,POS,ARAYPOS)

! Make an index entry.

  CALL BTHIND(EXPARR,IDFLG,IDENT,OCOR,ENTRY)

! Get system time as time of receipt to be used by ENBUFR

  CALL DATIM(NOW)
  DO I=0,4
    TOR(I+1)=NOW(8-I)
  END DO

! Put report before BUFR message in string to be stored

  MESAGE(1:REPLEN)=REPORT

! Encode BUFR message

  DESCR(1)=IDES(331203)
  NDESCR=1
  NOBS=1
  CMPRES=.FALSE.
  CALL ENBUFR(DESCR,EXPARR,NDESCR,LVALS,NOBS,ID,TOR,&
                MESAGE(REPLEN+1:),CMPRES,MESLEN,IVER)

! Find BUFR code figure corresponding to CCCC.
! Put this & data type (31=oceanographic) in section 1 of BUFR message.

  IF (CCCC /= ' ') THEN
    CALL CCCODE(287,ICCCC,CCCC)
    MESAGE(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
    MESAGE(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))
  END IF

  MESAGE(REPLEN+13:REPLEN+13)=CHAR(31)

!-----------------------------------------------------------------------
! Set bulletin details to go in trailer and call AIRSTO
!-----------------------------------------------------------------------

  ENTRY(3:6)=TTAAII(1:4)
  ENTRY(7:7)=CHAR(0)
  IF (OCOR) ENTRY(7:7)=CHAR(1)
  ENTRY(8:11)=CCCC
  BLKSIZ=27998

  CALL AIRSTO(DATIME,ENTRY,MESAGE(:REPLEN+MESLEN),&
                  NFT,BLKSIZ,IDENT,TOR)
END IF if_constr1

RETURN
END SUBROUTINE BATHY
