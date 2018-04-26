SUBROUTINE AIRENC(AIR_ARAY1,AIR_ARAY2,ARYFLG,INDX_ARAY1,     &
                  INDX_ARAY2,SIGN,BEAC_NAME,REPORT,REPLEN,   &
                  SPAN,CCCC,NFT,TOR,TTAAII)

!-----------------------------------------------------------------------
!
! PROGRAM       : AIRENC
!
! PURPOSE       : Routine calls the main storage routines
!
! DESCRIPTION   : The character report is tagged to the front of the
!                 BUFR message. The decoded values are passed to the
!                 BUFR encode rooutine and the final message is passed
!                 to AMDSTO
!
! CALLED BY     : AIRARP
!
! CALLS         : AIRIND,DATIM,AIRCOD,AIRSTO,CCCODE
!
! ARGUMENTS     : (1) AIR_ARAY1 - data array                     (i/o)
!                 (2) AIR_ARAY2 - midpoint data array            (i/o)
!                 (3) ARYFLG    - number of reports (1 or 2)       (i)
!                 (4) INDX_ARAY1 - value array                     (i)
!                 (5) INDX_ARAY2 - value array for midpoint        (i)
!                 (6) SIGN       - callsign                        (i)
!                 (7) BEAC_NAME  - beacon name                     (i)
!                 (8) REPORT     - report text                     (i)
!                 (9) REPLEN     - length of report                (i)
!                 (10) SPAN      - time span of reports in bulletin (i)
!                 (11) CCCC      - collecting centre                (i)
!                 (12) NFT       - MDB dataset unit number          (i)
!                 (13) TOR       - Set to current time             (i/o)
!                 (14) TTAAII    - Bulletin ID                      (i)
!
! REVISION INFO :
!
! $Workfile: airenc.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 12/01/2011 16:43:35$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         12/01/2011 16:43:35    Sheila Needham  Ready
!       for review
!  1    MetDB_Refresh 1.0         11/01/2011 10:43:12    Sheila Needham
!       Initial F77 version
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Interfaces

USE cccode_mod
USE airind_mod
USE datim_mod
USE aircod_mod
USE airsto_mod

IMPLICIT NONE

! Arguments

REAL,INTENT(INOUT)             :: AIR_ARAY1(18)
REAL,INTENT(INOUT)             :: AIR_ARAY2(18)
INTEGER,INTENT(IN)             :: ARYFLG
REAL,INTENT(IN)                :: INDX_ARAY1(8)
REAL,INTENT(IN)                :: INDX_ARAY2(8)
CHARACTER(LEN=8),INTENT(IN)    :: SIGN    !aircraft callsign
CHARACTER(LEN=8),INTENT(IN)    :: BEAC_NAME
CHARACTER(LEN=*),INTENT(IN)    :: REPORT !Raw character report
INTEGER,INTENT(IN)             :: REPLEN !length of character report
INTEGER,INTENT(IN)             :: SPAN   !time span of reports in bulletin
CHARACTER(LEN=*),INTENT(IN)    :: CCCC
INTEGER,INTENT(IN)             :: NFT    !data set unit number for MDB
INTEGER,INTENT(INOUT)          :: TOR(5) !time or reciept
CHARACTER(LEN=*),INTENT(IN)    :: TTAAII

! Local Variables

LOGICAL          :: ARTMCK
CHARACTER(LEN=4) :: BUFR       !BUFR indicator
INTEGER          :: DATIME(5)  !DATIME array
LOGICAL          :: DATIME_TEST !Indicates date/time in valid range
INTEGER          :: DESCR(25)  !BUFR descriptor
CHARACTER(LEN=23):: ENTRY      ! Index entry
LOGICAL          :: FIRST=.TRUE. !TRUE if 1st time in routine
INTEGER          :: I          !used in loops
INTEGER          :: ICCCC      !Bufr code number for collecting centre
CHARACTER(LEN=9) :: IDENT
INTEGER          :: IDES
INTEGER          :: ISTART     ! ident starts at REPORT(ISTART:)
INTEGER          :: LIDENT     ! length of ident (for INDEX to find it)
INTEGER          :: LENGTH     !length of BUFR message
INTEGER          :: LMESS      !length of BUFR+CHAR message
INTEGER          :: LOOP
CHARACTER(LEN=10240) :: MESAGE !BUFR+CHAR message
INTEGER          :: NDESC      !number of BUFR descriptors
INTEGER          :: NOW(8)
INTEGER          :: SYS_YEAR   !Current system year
INTEGER          :: TOTSTORE

SAVE

! Initialise variables

IF (FIRST) THEN
  FIRST=.FALSE.
  BUFR = CHAR(66) // CHAR(85) // CHAR(70) // CHAR(82)
END IF

! Loop round the obs (2 of them if MID), storing them in time
! order, MID first, to make it easier to find the next midpoint.

DO LOOP=ARYFLG,1,-1

  DO I=1,25
    DESCR(I)=-9999999
  END DO
  NDESC=1
  MESAGE(:)=' '
  DESCR(1)=IDES(311196)
  DO I=1,5
    DATIME(I)=-9999999
  END DO
  TOTSTORE=0
  DATIME_TEST=.FALSE.

!----------------------------------------------------------------------
!Call cccode to convert collecting centre to BUFR code table number
!----------------------------------------------------------------------

  CALL CCCODE(287,ICCCC,CCCC)
!----------------------------------------------------------------------
! Make an index entry for this report
!----------------------------------------------------------------------

  CALL AIRIND(INDX_ARAY1,INDX_ARAY2,LOOP,SIGN,TTAAII,CCCC, &
              DATIME,ENTRY,IDENT,SPAN)

!----------------------------------------------------------------------
!Get system time now and use it as the time of receipt for this report
!----------------------------------------------------------------------

  CALL DATIM(NOW)
  DO I=1,5
    TOR(I)=NOW(9-I)
  END DO
  SYS_YEAR=TOR(1)              !Set current system year
!---------------------------------------------------------------------
! Put text (starting with IDENT) at start of string to be stored
! Return if ident has 8 characters (no space in *8 string)
! - or if ident not found in report (shouldn't happen!)
!---------------------------------------------------------------------

  LIDENT=INDEX(IDENT(1:8),' ')
  IF (LIDENT == 0) THEN
    PRINT *,' AIRENC rejected 8-character identifier:'
    PRINT *,IDENT,'   ',CCCC,'   ',REPORT(1:50)
    RETURN
  END IF
  ISTART=INDEX(REPORT,IDENT(1:LIDENT))
  IF (ISTART == 0) RETURN
  MESAGE(1:REPLEN-ISTART+1)=REPORT(ISTART:REPLEN)

!---------------------------------------------------------------------
! Encode the message into BUFR
!---------------------------------------------------------------------

  CALL AIRCOD(AIR_ARAY1,AIR_ARAY2,LOOP,SIGN,BEAC_NAME,    &
              DESCR,NDESC,REPLEN,MESAGE,LENGTH,TOR)

!---------------------------------------------------------------------
!Set overall length of BUFR mesage (BUFR + CHAR).
!Call to storage routine - after checking there is a BUFR message.
!---------------------------------------------------------------------

  LMESS=LENGTH+REPLEN

!---------------------------------------------------------------------
!index entry,datime array and the message are passed to AIRSTO for
!storage.
!Also insert the collecting centre BUFR no into section 1
!---------------------------------------------------------------------

  IF (MESAGE(REPLEN+1:REPLEN+4)  ==  BUFR) THEN
    MESAGE(REPLEN+13:REPLEN+13)=CHAR(4)
    MESAGE(REPLEN+9:REPLEN+9)=CHAR(ICCCC/256)
    MESAGE(REPLEN+10:REPLEN+10)=CHAR(MOD(ICCCC,256))

!----------------------------------------------------------------------
!Check that the date time of the AIREP report is within valid range.
!If it is DATIME_TEST will be TRUE and the report stored else an
!error message will be printed.
!----------------------------------------------------------------------

    DATIME_TEST=ARTMCK(DATIME,SYS_YEAR)
    IF (DATIME_TEST) THEN
      CALL AIRSTO(DATIME,ENTRY,MESAGE(1:LMESS),NFT,27998,IDENT, &
      TOR)
    ELSE
      WRITE(6,*)'AIREPS(AIRENC):Warning - Bad Date/Time group ',&
      DATIME,REPORT(1:50)
    END IF
  END IF
END DO

RETURN    !return to get next message or bulletin
END SUBROUTINE AIRENC
