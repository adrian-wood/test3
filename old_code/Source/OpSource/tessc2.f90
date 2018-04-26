SUBROUTINE TESSC2(REPORT,POS,EXPARR,POS1A,ENDVALS)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC2
!
! PURPOSE       : To expand section 2 of TESAC
!                 (temperature & salinity profile)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! ARGUMENTS     : REPORT   character string        (I)
!                 POS      pointer to report      (I/O)
!                 EXPARR   expansion array        (I/O)
!                 POS1A    end of profile in array (O)
!                 ENDVALS  depth, temp, salinity   (O)
!
! REVISION INFO :
!
! $Workfile: tessc2.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 06/04/2011 10:02:20$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         06/04/2011 10:02:20    Brian Barwell
!       Restructured DO WHILE loop and IF test.
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
! $
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

USE metdb_com_mod, only : RMISS
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    ::  REPORT        !a01
INTEGER,          INTENT(INOUT) ::  POS           !a02
REAL,             INTENT(INOUT) ::  EXPARR(0:*)   !a03
INTEGER,          INTENT(OUT)   ::  POS1A         !a04
REAL,             INTENT(OUT)   ::  ENDVALS(3)    !a05

! Local declarations:

REAL             ::      DEPTH
REAL             ::      TEMP
REAL             ::      SALIN
INTEGER          ::      REPLEN
INTEGER          ::      POS1
INTEGER          ::      NLEVELS  ! number of levels (replications)
INTEGER          ::      K1       ! digitization indicator
INTEGER          ::      K2       ! salinity/depth measurement method
LOGICAL          ::      GARBLED  ! true if 2-group not found

! POS1 is where the count of levels will be stored in the value array;
! depth, temperature & salinity subscripts are relative to POS1.

REPLEN=LEN(REPORT)
ENDVALS(1)=RMISS          ! sea depth
ENDVALS(2)=RMISS          ! IxIxIx
ENDVALS(3)=RMISS          ! XrXr
K1=RMISS
K2=RMISS
POS1=29
NLEVELS=0

! POS at start points to start of section 2 - should be 888

IFLABEL1: &
IF (REPORT(POS:POS+2) == '888' .AND. POS+4 <= REPLEN) THEN

! 888 is followed by k1 (code table 2262), values 7 or 8, BUFR 0 or 1.

  K1=IVALUE(REPORT(POS+3:POS+3))
  IF (K1 == 7) THEN
    K1=0
  ELSE IF (K1 == 8) THEN
    K1=1
  ELSE
    K1=RMISS
  END IF

! k2 follows: values of 0 to 3, no conversion needed (code table 2263)

  K2=IVALUE(REPORT(POS+4:POS+4))
  IF (K2 < 0 .OR. K2 > 3) K2=RMISS

! Move POS to next group, IxIxIxXrXr (XBT instrument & recorder type).
! This group, made mandatory in 2000, could be confused with the first
! depth group: if it starts with a 2, assume it's instrumentation if
! another 2-group follows, depth if a 3- or 4-group follows.
! That is, skip it if it's not a 2-group or if a 2-group follows.

  POS=POS+6
  IF (REPORT(POS:POS) /= '2' .OR.       &
      REPORT(POS+6:POS+6) == '2') THEN
    ENDVALS(2)=IVALUE(REPORT(POS:POS+2))      ! IxIxIx
    ENDVALS(3)=IVALUE(REPORT(POS+3:POS+4))    ! XrXr
    POS=POS+6
  END IF

! Loop round depth, temperature & salinity groups (starting 2, 3
! or 4 respectively) until one of the following is reached:
!   66... at start of section 3 (current profile),
!   55555 at start of section 4,
!   call sign (with letters) or 99999 before buoy number in section 5,
!   00000 indicating that lowest data is at bottom (end of section 2)

  GARBLED=.FALSE.

DOLABEL1: &
  DO WHILE (POS+4 <= REPLEN)
    IF (REPORT(POS:POS+4) == '99999' .OR.       &
        REPORT(POS:POS+4) == '55555' .OR.       &
        REPORT(POS:POS+1) == '66'    .OR.       &
        REPORT(POS:POS+4) == '00000' .OR.       &
        IVALUE(REPORT(POS:POS+4)) == -9999999) EXIT

IFLABEL2: &
    IF (.NOT.GARBLED .AND. REPORT(POS:POS) == '2') THEN
      DEPTH=IVALUE(REPORT(POS+1:POS+4))
      POS=POS+6

! Convert temperature from hundredths Celsius to real degrees Kelvin

      TEMP=RMISS
      IF (POS+4 <= REPLEN .AND. REPORT(POS:POS) == '3') THEN
        TEMP=IVALUE(REPORT(POS+1:POS+4))
        IF (TEMP >= 5000) TEMP=5000-TEMP
        IF (TEMP /= RMISS) TEMP=TEMP*0.01+273.15
        POS=POS+6
      END IF

! Convert salinity from hundredths of parts per thousand to real

      SALIN=RMISS
      IF (POS+4 <= REPLEN .AND. REPORT(POS:POS) == '4') THEN
        SALIN=IVALUE(REPORT(POS+1:POS+4))
        IF (SALIN /= RMISS) SALIN=SALIN*0.01
        POS=POS+6
      END IF

! If T or salinity reported, store either or both with depth

      IF (TEMP /= RMISS .OR. SALIN /= RMISS) THEN
        NLEVELS=NLEVELS+1
        EXPARR(POS1+6*NLEVELS-4)=DEPTH
        EXPARR(POS1+6*NLEVELS-2)=TEMP
        EXPARR(POS1+6*NLEVELS)=SALIN
      END IF

! If 2-group (depth) not found where expected, stop expanding profile.

    ELSE
      IF (.NOT.GARBLED) PRINT *,   &
        'TESSC2 stopped at unexpected group: ',REPORT(POS:POS+4)
      GARBLED=.TRUE.
      POS=POS+6
    END IF IFLABEL2
  END DO DOLABEL1

! If profile loop was ended by 00000, store last depth as total depth
! & skip the 00000 (bottom indicator) itself.

  IF (POS+4 <= REPLEN) THEN
    IF (REPORT(POS:POS+4) == '00000') THEN
      IF (NLEVELS >= 1) ENDVALS(1)=EXPARR(POS1+(6*NLEVELS)-4)
      POS=POS+6
    END IF
  END IF
END IF IFLABEL1

! Set POS1A to end of first profile in value array.

POS1A=POS1+6*NLEVELS

! Put number of levels & k1/k2 in values array.

EXPARR(POS1)=NLEVELS
EXPARR(26)=K1
EXPARR(28)=K2

RETURN
END SUBROUTINE TESSC2
