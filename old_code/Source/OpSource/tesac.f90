SUBROUTINE TESAC(REPORT,TTAAII,CCCC,OCOR,NFT)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESAC
!
! PURPOSE       : To expand, encode & store one TESAC report
!
! CALLED BY     : BATESBUL
!
! CALLS         : TESSCn (n=1,5), TESIND,
!                 ENBUFR, CCCODE, AIRSTO, DATIM
!                 IDES
!
! ARGUMENTS     : REPORT   to be expanded & stored                   (I)
!                 TTAAii   bulletin identifier                       (I)
!                 CCCC     originating centre                        (I)
!                 OCOR     correction flag                           (I)
!                 NFT      FT number for BATHY & TESAC               (I)
!
! REVISION INFO :
!
! $Workfile: tesac.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 06/06/2011 16:10:51$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         06/06/2011 16:10:51    Brian Barwell   Reduce
!        MAXREPL to 11571 to prevent MESAGE overflow in BUFR encoding.
!  3    MetDB_Refresh 1.2         01/06/2011 08:25:11    Sheila Needham  Added
!       a terminator for truncated reports to stop over-run in supsequent
!       routines
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
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

! Use statements:
USE tessc1_mod
USE tessc2_mod
USE tessc3_mod
USE tessc4_mod
USE tessc5_mod
USE tesind_mod
USE enbufr_mod
USE cccode_mod
USE airsto_mod
USE datim_mod
USE ides_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT)  ::  REPORT   !A01
CHARACTER(LEN=*), INTENT(IN)     ::  TTAAII   !A02
CHARACTER(LEN=4), INTENT(IN)     ::  CCCC     !A03
LOGICAL,          INTENT(IN)     ::  OCOR     !A04 FLAG FOR CORRECTION
INTEGER,          INTENT(IN)     ::  NFT      !A05 FT number for storage

! Local declarations:

! For alphanumeric TESAC bulletins without optional sections and
! with n levels, the following approximate lengths in bytes apply:
!    Original message: 18n+51     BUFR message: 6.125n+77
!    Message trailer:  23         Total length: 24.125n+151
! Values in the PARAMETER statement below allow for up to 640
! levels. The highest seen to date (10/3/2006) is 520 levels.
! (Length of merged messages is about 48.75n+1093 but the number
! of levels selected for merging is limited by the merge table.)

INTEGER, PARAMETER   :: LARRAY=4000   ! size of value/descriptor arrays
INTEGER, PARAMETER   :: LSTRING=15600 ! size of text/message strings
INTEGER, PARAMETER   :: MAXREPL=11571 ! max text length for string
INTEGER, PARAMETER   :: BLKSIZ=27998  ! blocksize of storage data set
INTEGER, PARAMETER   :: IVER=13       ! table B version number

CHARACTER(LEN=9)       ::  ID
CHARACTER(LEN=9)       ::  IDENT
CHARACTER(LEN=9)       ::  IDUM ='XXXXXXXXX' ! dummy ID to delimit truncated reports
CHARACTER(LEN=23)      ::  ENTRY      ! index entry
CHARACTER(LEN=LSTRING) ::  MESAGE     ! report & then BUFR message

INTEGER            ::  REPLEN    ! report length
INTEGER            ::  MESLEN    ! BUFR message length
INTEGER            ::  I         ! short-term loop variable
INTEGER            ::  POS       ! pointer to character string
INTEGER            ::  ARRPOS2   ! array pointer (end of T/sal profile)
INTEGER            ::  ARRPOS3   ! array pointer (end of current profile
INTEGER            ::  IERR      ! error flag - may be set by TESSCn
INTEGER            ::  ICCCC     ! number corresponding to CCCC
INTEGER            ::  NOBS      ! number of obs in BUFR message (=1)
INTEGER            ::  DATIME(5) ! date & time of data (input)
INTEGER            ::  NOW(8),TOR(5) ! current date & time
INTEGER            ::  NDESCR
INTEGER            ::  DESCR(LARRAY)

REAL               ::  EXPARR(0:LARRAY)
REAL               ::  ENDVALS(3)  ! sea depth & IxIxIxXrXr to go on end

LOGICAL            ::  CMPRES
LOGICAL            ::  IDFLG

! Put arrays in dynamic common so that if expansion array overflows,
! it will overflow into arrays not used till encode...

COMMON /TESACOM/ EXPARR,DESCR,MESAGE

! Set first value to show that q/c bits precede all other values

EXPARR(0)=1
DO I=1,LARRAY
  EXPARR(I)=-9999999.0
END DO

! If the report is too long for the strings and arrays provided,
! truncate it to avoid abending.  But call TESSC5 first to get
! the identifier from the end before resetting the length!
! Put a dummy ID containing characters on the end so that other routines
! know when to stop processing.
! Keep a copy of ID in IDENT. This is because ENBUFR will convert ID
! from EBCDIC to ASCII and we need to pass an un-converted ID to AIRSTO.

REPLEN=LEN(REPORT)

CALL TESSC5(REPORT,EXPARR,IDFLG,ID)
IDENT=ID

IF (REPLEN > MAXREPL) THEN
  PRINT*,'TESAC for ',ID,' truncated from ',REPLEN
  DO I=MAXREPL,MAXREPL-20,-1
    IF(INDEX(REPORT(I:I),' ') == 1) THEN
      REPORT(I+1:)=IDUM
      REPLEN=I+9
      EXIT
    END IF
  END DO
  PRINT*,'Length reset to',REPLEN
  PRINT*,REPORT(1:120)
END IF

! Put the report in characters (which may have been truncated above)
! at the start of the string to be stored.

MESAGE(1:REPLEN)=REPORT

! Expand report section by section, then encode & store.
! (Sea depth can come from either section 2 or section 4, so is
! passed from TESSC2 to be finally stored on the end by TESSC4,
! together with recently added instrumentation details.)

POS=1
CALL TESSC1(REPORT,POS,EXPARR,DATIME,IERR)

IF (IERR == 0) THEN
  CALL TESSC2(REPORT,POS,EXPARR,ARRPOS2,ENDVALS)
  CALL TESSC3(REPORT,POS,EXPARR,ARRPOS2,ARRPOS3)
  CALL TESSC4(REPORT,POS,EXPARR,ARRPOS3,ENDVALS)
END IF

! If the expansion worked, first make an index entry

IFIERR0: &
IF (IERR == 0) THEN
  CALL TESIND(EXPARR,IDFLG,IDENT,OCOR,ENTRY)

! Get current time for ENBUFR to set as time of receipt

  CALL DATIM(NOW)
  DO I=0,4
    TOR(I+1)=NOW(8-I)
  END DO

! Encode BUFR message

  DESCR(1)=IDES(331195)
  NDESCR=1
  NOBS=1
  CMPRES=.FALSE.
  CALL ENBUFR(DESCR,EXPARR,NDESCR,LARRAY,NOBS,ID,TOR,  &
              MESAGE(REPLEN+1:),CMPRES,MESLEN,IVER)

! Find number corresponding to CCCC in code table 001031.
! Put this number & data type into section 1 of BUFR message.
! Data type is 31 for oceanographic data

  IF(CCCC /= ' ') THEN
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

  CALL AIRSTO(DATIME,ENTRY,MESAGE(:REPLEN+MESLEN),  &
              NFT,BLKSIZ,IDENT,TOR)
END IF IFIERR0

RETURN
END SUBROUTINE TESAC
