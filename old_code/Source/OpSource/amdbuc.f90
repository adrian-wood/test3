SUBROUTINE AMDBUC(BULL,NREP,IPOINT,ILEN,BDAY,BHOUR,RC)

!-----------------------------------------------------------------------
!
! PROGRAM       : AMDBUC
!
! PURPOSE       : SEARCH BULLETIN OF AMDARS IN CHARACTERS FOR RECORD
!                 LENGTHS AND DELETE CARRIAGE RETURNS, LINEFEEDS
!                 AND SPACES OCCURRING MORE THAN ONCE.
!
! CALLED BY     : AMDAR
!
! CALLS         : OFIGTS, BULLED
!
! ARGUMENTS     : (1) BULL   BULLETIN
!                 (2) NREP   NUMBER OF REPORTS IN BULLETIN
!                 (3) IPOINT ARRAY OF REPORT POINTERS
!                 (4) ILEN   ARRAY OF REPORT LENGTHS
!                 (5) BDAY   DAY IN BULLETIN HEADING
!                 (6) BHOUR  HOUR IN BULLETIN HEADING
!                 (7) RC     RETURN CODE. 0 REPORT OK
!                                         1 REPORT REJECTED
!
! REVISION INFO :
!
! $Workfile: amdbuc.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 21/01/2011 14:26:49$
!
! CHANGE RECORD :
!
! $Log:
!  2    MetDB_Refresh 1.1         21/01/2011 14:26:49    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         20/01/2011 13:09:48    Alison Weir
!       Initial f77 version - MDBSTORBatch20
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

! Use statements:
USE ofigts_mod
USE bulled_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(INOUT) :: BULL         !a1
INTEGER,          INTENT(OUT)   :: NREP         !a2
INTEGER,          INTENT(OUT)   :: IPOINT(:)    !a3
INTEGER,          INTENT(OUT)   :: ILEN(:)      !a4
INTEGER,          INTENT(OUT)   :: BDAY         !a5
INTEGER,          INTENT(OUT)   :: BHOUR        !a6
INTEGER,          INTENT(OUT)   :: RC           !a7

! Local declarations:

!----------------------------------------------------------------------
!Declare Integer
!----------------------------------------------------------------------

INTEGER         :: ADAY
INTEGER         :: AHOUR
INTEGER         :: I
INTEGER         :: IU
INTEGER         :: IAMDAR
INTEGER         :: IMODES
INTEGER         :: ISTART
INTEGER         :: IEND
INTEGER         :: IADS
INTEGER         :: LBULL

!----------------------------------------------------------------------
!Declare Logical
!----------------------------------------------------------------------

LOGICAL         :: LDATIM

!----------------------------------------------------------------------
!SAVE and Initialize local variables
!----------------------------------------------------------------------

SAVE

!----------------------------------------------------------------------
! Initialise report count, pointer,length arrays and intent(out) args.
!----------------------------------------------------------------------

NREP=0
LBULL=LEN(BULL)                  !Length of BULL
DO I=1,100
  IPOINT(I)=0                    !Pointer
  ILEN(I)=0                      !Length of report array
END DO

BDAY=0
BHOUR=0

!----------------------------------------------------------------------
! Replace carriage return line feed sequences by spaces.
!----------------------------------------------------------------------

ISTART=1
IEND=INDEX(BULL,'NNNN')+3
CALL BULLED(ISTART,IEND,BULL)

!----------------------------------------------------------------------
! Find and convert day and hour from bulletin heading.
! Find MiMiMjMj as AMDAR, MODES or ADS,  then
! find reports' day/time group in bulletin heading.
!----------------------------------------------------------------------

IU=INDEX(BULL(1:MIN(LBULL,100)),'U')  ! TTAAii starts with U?
IF (IU == 0) THEN
  PRINT *,' AMDBUC: TTAAii not U..... in ',  &
  BULL(1:MIN(LBULL,80))
  RC=1
  RETURN
END IF

LDATIM=OFIGTS(BULL,IU+12,IU+17)   ! 6 figs after TTAAII/CCCC?
IF (.NOT.LDATIM) THEN
  PRINT *,' AMDBUC: <6 figs after CCCC in ',  &
  BULL(IU:MIN(LBULL,IU+80))
  RC=1
  RETURN
END IF
READ (BULL(IU+12:IU+13),'(I2)') BDAY  ! Convert day
READ (BULL(IU+14:IU+15),'(I2)') BHOUR ! Convert hour
!
IFLABEL1: &
IF (INDEX(BULL(1:MIN(LBULL,100)),'AMDAR') /= 0) THEN
  IAMDAR=INDEX(BULL(1:MIN(LBULL,100)),'AMDAR') ! 'AMDAR' DISP
  LDATIM=OFIGTS(BULL,IAMDAR+6,IAMDAR+9) ! 4 figs after 'AMDAR'?
  IF (.NOT.LDATIM) THEN
    PRINT *,' AMDBUC: <4 figs after AMDAR in ',  &
    BULL(IU:MIN(LBULL,IU+80))
    RC=1
    RETURN
  END IF
  READ (BULL(IAMDAR+6:IAMDAR+7),'(I2)') ADAY
  READ (BULL(IAMDAR+8:IAMDAR+9),'(I2)') AHOUR

ELSE IF (INDEX(BULL(1:MIN(LBULL,100)),'MODES') /= 0) THEN   IFLABEL1
  IMODES=INDEX(BULL(1:MIN(LBULL,100)),'MODES') ! Find MODES
  LDATIM=OFIGTS(BULL,IMODES+6,IMODES+9) ! 4 figs?
  IF (.NOT.LDATIM) THEN
    PRINT *,' AMDBUC: <4 figs after MODES in ',  &
    BULL(IU:MIN(LBULL,IU+80))
    RC=1
    RETURN
  END IF
  READ (BULL(IMODES+6:IMODES+7),'(I2)') ADAY
  READ (BULL(IMODES+8:IMODES+9),'(I2)') AHOUR

ELSE IF (INDEX(BULL(1:MIN(LBULL,100)),'ADS') /= 0) THEN     IFLABEL1
  IADS=INDEX(BULL(1:MIN(LBULL,100)),'ADS') ! Find ADS Feb 94
  LDATIM=OFIGTS(BULL,IADS+4,IADS+7)   ! 4 figs?
  IF (.NOT.LDATIM) THEN
    PRINT *,' AMDBUC: <4 figs after ADS in ',  &
    BULL(IU:MIN(LBULL,IU+80))
    RC=1
    RETURN
  END IF
  READ (BULL(IADS+4:IADS+5),'(I2)') ADAY
  READ (BULL(IADS+6:IADS+7),'(I2)') AHOUR

ELSE
  PRINT *,' AMDBUC: not AMDAR, MODES or ADS: ',  &
  BULL(IU:MIN(LBULL,IU+80))
  RC=1
  RETURN
END IF IFLABEL1  ! END OF FEB 94 CHANGES

!----------------------------------------------------------------------
! Check that these days & hours form a period of less than 24 hours.
! if not, skip bulletin. if so, pass them on to decide day of report.
!----------------------------------------------------------------------

IF ((BDAY == ADAY .AND. BHOUR >= AHOUR) .OR.    &
    (BDAY == ADAY+1 .AND. BHOUR < AHOUR) .OR.   &
    (BDAY == 1 .AND. ADAY >= 28 .AND. BHOUR < AHOUR)) THEN
  CONTINUE
ELSE
  PRINT *,'BAD DATE/TIMES IN AMDAR BULLETIN: ',BULL(IU:IAMDAR+9)
  RC=1
  RETURN
END IF

!----------------------------------------------------------------------
! Now search string for starts of reports, listing pointers & lengths.
!----------------------------------------------------------------------

I=IAMDAR+9

30 CONTINUE
I=I+1

!----------------------------------------------------------------------
! Test for 3-figure phase of flight indicator for new report start
! and test for equal sign for end of report (either test sufficient).
! Skip report if it's too short: it needs more than 20 characters
! to contain temperature or wind after ident, position, time & FL.
!----------------------------------------------------------------------

IFLABEL2: &
IF (BULL(I:I+2) == 'LVR' .OR. BULL(I:I+2) == 'LVW' .OR.  &
    BULL(I:I+2) == 'ASC' .OR. BULL(I:I+2) == 'DES' .OR.  &
    BULL(I-2:I-2) == '=') THEN
  NREP=NREP+1                    ! NO OF REPORTS FOUND
  IPOINT(NREP)=I                 ! POSITION OF NEW REPORT

IFLABEL3: &
  IF (NREP > 1) THEN
    ILEN(NREP-1)=IPOINT(NREP)-IPOINT(NREP-1)-1
    IF(ILEN(NREP-1) > 90)THEN    ! IF REPORT LENGTH TOO LONG,
      NREP=NREP-2                ! SKIP NEW REPORT & LAST REPORT
      IF (NREP <= 0) THEN
        PRINT *,' AMDBUC: long ob skipped, no more obs in',  &
         ' bulletin ',BULL(IU:IU+17)
        RC=1
        RETURN
      END IF
      RC=0
      RETURN                     ! SKIP REMAINING REPORTS
    END IF
    IF (ILEN(NREP-1) < 20) THEN
      NREP=NREP-1
      print *,' AMDBUC: ',  &
              NREP,'th ob in ',BULL(IU:IU+17),' is short: ',  &
              BULL(IPOINT(NREP):IPOINT(NREP)+ILEN(NREP))
    END IF
  END IF IFLABEL3

END IF IFLABEL2
IF (BULL(I:I+3) /= 'NNNN') THEN
  GOTO 30
END IF

!----------------------------------------------------------------------
! NREP values of IPOINT have been set, NREP-1 of ILEN, so NREP is one
! more than the number of reports.  Decrement it.
!----------------------------------------------------------------------

NREP=NREP-1
RC=0

RETURN
END SUBROUTINE AMDBUC
