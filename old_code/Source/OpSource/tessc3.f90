SUBROUTINE TESSC3(REPORT,POS,EXPARR,POS1A,POS2A)

!-----------------------------------------------------------------------
!
! PROGRAM       : TESSC3
!
! PURPOSE       : To expand section 3 of TESAC (current profile)
!
! CALLED BY     : TESAC
!
! CALLS         : IVALUE   converts figures to numbers
!
! ARGUMENTS     : REPORT   character string                       (I)
!                 POS      pointer to report                     (I/O)
!                 EXPARR   expansion array                       (I/O)
!                 POS1A    end of temp/salinity profile in array  (I)
!                 POS2A    end of current profile in array        (O)
!
! REVISION INFO :
!
! $Workfile: tessc3.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 16/05/2011 11:40:26$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         16/05/2011 11:40:26    Stan Kellett    split
!       do statement so replen checked first and rest in a if statement
!  3    MetDB_Refresh 1.2         16/05/2011 11:19:37    Stan Kellett    extra
!       error checking added to Dolabel1
!  2    MetDB_Refresh 1.1         19/01/2011 10:25:53    Alison Weir     Ported
!        to f95
!  1    MetDB_Refresh 1.0         17/01/2011 13:15:24    Alison Weir
!       Initial f77 version - MDBSTOR batch 18.
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
USE metdb_com_mod, only : RMISS
USE ivalue_mod

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    ::  REPORT       !a01
INTEGER,          INTENT(INOUT) ::  POS          !a02
REAL,             INTENT(INOUT) ::  EXPARR(0:*)  !a03
INTEGER,          INTENT(IN)    ::  POS1A        !a04
INTEGER,          INTENT(OUT)   ::  POS2A        !a05

! Local declarations:

REAL            ::        DEPTH
REAL            ::        CDIR
REAL            ::        CSPEED
INTEGER         ::        REPLEN
INTEGER         ::        POS2
INTEGER         ::        NLEVELS    ! NUMBER OF DEPTH LAYERS
INTEGER         ::        K3
INTEGER         ::        K4
INTEGER         ::        K6
LOGICAL         ::        GARBLED

! POS2 is where the count of levels will be stored in the value array;
! depth and current direction & speed subscripts are relative to POS2.

REPLEN=LEN(REPORT)
K3=RMISS
K4=RMISS
K6=RMISS
POS2=POS1A+7
NLEVELS=0

! POS at start should point to 66 at start of current profile

IFLABEL1: &
IF (POS+4 <= REPLEN .AND. REPORT(POS:POS+1) == '66') THEN

! 66 is followed by k6, k4 & k3   (see code tables 2267, 2265, 2264)
!   k6: how ship's velocity & motion is removed from measurements
!   k4: period of current measurement (1-9 correspond to BUFR 11-19)
!   k3: duration & time...

  K6=IVALUE(REPORT(POS+2:POS+2))

  K4=IVALUE(REPORT(POS+3:POS+3))
  IF (K4 /= RMISS) K4=K4+10

  K3=IVALUE(REPORT(POS+4:POS+4))

! Move POS to next group

  POS=POS+6

! Loop round depth & current groups (depth group marked 2) until
! one of the following is reached:
!   55555 at start of section 4
!   call sign (letters) or 99999 before buoy number in section 5

  GARBLED=.FALSE.

DOLABEL1: &
  DO WHILE(POS+4 <= REPLEN)
	  
    IF (REPORT(POS:POS+4) == '99999' .OR.            &
        REPORT(POS:POS+4) == '55555' .OR.            &
        REPORT(POS:POS+1) == '66'    .OR.            &
        REPORT(POS:POS+4) == '00000' .OR.            &
        IVALUE(REPORT(POS:POS+4)) == -9999999 .OR.   &	
       (REPORT(POS:POS) >= 'A'.AND.REPORT(POS:POS) <= 'Z')) EXIT

IFLABEL2: &
    IF (.NOT.GARBLED .AND. REPORT(POS:POS) == '2') THEN
      DEPTH=IVALUE(REPORT(POS+1:POS+4))
      POS=POS+6

! Convert direction from tens of degrees (direction towards, not from!)
! Convert speed from cm/s to m/s.

IFLABEL3: &
      IF (POS+4 <= REPLEN) THEN
        CDIR=IVALUE(REPORT(POS:POS+1))
        CSPEED=IVALUE(REPORT(POS+2:POS+4))
        IF (CDIR /= RMISS) CDIR=CDIR*10.
        IF (CSPEED /= RMISS) CSPEED=CSPEED*0.01

        NLEVELS=NLEVELS+1
        EXPARR(POS2+(6*NLEVELS)-4)=DEPTH
        EXPARR(POS2+(6*NLEVELS)-2)=CDIR
        EXPARR(POS2+(6*NLEVELS))=CSPEED

        POS=POS+6
      END IF IFLABEL3

! If 2-group (depth) not found where expected, stop expanding profile
! (but loop on until a delimiting group is found).

    ELSE
      IF (.NOT.GARBLED) PRINT *,   &
        'TESSC3 stopped at unexpected group: ',REPORT(POS:POS+4)
      GARBLED=.TRUE.
      POS=POS+6
    END IF IFLABEL2
  END DO DOLABEL1
END IF IFLABEL1

! Set POS2A to end of current profile in value array.

POS2A=POS2+6*NLEVELS

! Put number of levels & k6/k4/k3 in values array.

EXPARR(POS1A+2)=K6
EXPARR(POS1A+4)=K4
EXPARR(POS1A+6)=K3
EXPARR(POS1A+7)=NLEVELS

RETURN
END SUBROUTINE TESSC3
