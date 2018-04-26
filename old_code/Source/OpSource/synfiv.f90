SUBROUTINE SYNFIV(REPORT,POINT,NGRPS,IG, &
    IR,IE,EEE,SUN2,SUN1,RNET,GLSO,DISO,LW,SW,SWNET,SDIR, &
    RNET24,GLSO24,DISO24,LW24,SW24,SWNET24,SDIR24,PCHG24)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNFIV
!
! PURPOSE       : TO EXPAND 5-GROUPS AND ASSOCIATED RADIATION GROUPS
!                 FROM SECTION 3 OF A SYNOP REPORT.
!
! DESCRIPTION   : EVAPOTRANSPIRATION & 24-HOUR PRESSURE CHANGE ARE
!                 SIMPLE, UNLIKE 55... GROUPS, WHICH CAN BE SUNSHINE
!                 (55SSS/553SS) OR RADIATION INDICATORS (55407 ETC),
!                 & MAY BE FOLLOWED BY RADIATION GROUPS STARTING 0-6.
!                 THE INITIAL 0-6 SAYS WHICH RADIATION ELEMENT (SEE
!                 ARGUMENT LIST) & THE INDICATOR GROUPS EFFECTIVELY
!                 ADD 7 & 8 TO THIS LIST, AVOIDING CONFUSION WITH 7-
!                 & 8-GROUPS IN SECTION 3, BUT LEAVING A 6-GROUP AS
!                 EITHER RADIATION OR RAINFALL, A 5-GROUP RADIATION
!                 OR ANOTHER MAIN 5-GROUP.
!
! CALLED BY     : SYNXP3
!
! CALLS         : IVALUE
!
! ARGUMENTS     : (1)REPORT  CHAR    COMPLETE REORT               (I)
!                 (2)POINT   INTEGER START OF FIRST 5-GROUP       (I)
!                 (3)NGRPS   INTEGER NUMBER OF GROUPS IN SECTION  (I)
!                 (4)IG      INTEGER GROUP NUMBER               (I/O)
!                 (5)IR      INTEGER RAINFALL REPORTING FLAG      (I)
!                 (6)IE      REAL    EVAPO INSTR.                 (O)
!                 (7)EEE     REAL    24 HR EVAPOTRANSPIRATION     (O)
!                 (8)SUN2    REAL    DAILY SUNSHINE               (O)
!                 (9)SUN1    REAL    HOURLY SUNSHINE              (O)
!                (10)RNET    REAL    HOURLY NET RAD.              (O)
!                (11)GLSO    REAL    HOURLY GLOBAL SOLAR RAD.     (O)
!                (12)DISO    REAL    HOURLY DIFFUSE SOLAR RAD.    (O)
!                (13)LW      REAL    HOURLY NET LONGWAVE RAD.     (O)
!                (14)SW      REAL    HOURLY SHORTWAVE RAD.        (O)
!                (15)SWNET   REAL    HOURLY NET SHORTWAVE         (O)
!                (16)SDIR    REAL    HOURLY DIRECT SOLAR RAD.     (O)
!                (17)RNET24  REAL    DAILY NET RAD.               (O)
!                (18)GLSO24  REAL    DAILY GLOBAL SOLAR RAD.      (O)
!                (19)DISO24  REAL    DAILY DIFFUSE SOLAR RAD.     (O)
!                (20)LW24    REAL    DAILY NET LONGWAVE RAD.      (O)
!                (21)SW24    REAL    DAILY SHORTWAVE RAD.         (O)
!                (22)SWNET24 REAL    DAILY NET SHORTWAVE          (O)
!                (23)SDIR24  REAL    DAILY DIRECT SOLAR RADIATION (O)
!                (24)PCHG24  REAL    24 HOUR PRESSURE CHANGE      (O)
!
! REVISION INFO :
!
! $Workfile: synfiv.f90$ $Folder: OpSource$
! $Revision: 3$ $Date: 22/03/2011 15:00:51$
!
! CHANGE RECORD :
!
! $Log:
!  3    MetDB_Refresh 1.2         22/03/2011 15:00:51    Brian Barwell   Tests
!       in IF and DO WHILE statements rewritten in 3 places.
!  2    MetDB_Refresh 1.1         23/12/2010 10:48:08    John Norton     Ported
!        version for MDBSTOR batches 6 & 7
!  1    MetDB_Refresh 1.0         23/12/2010 10:16:13    John Norton
!       Pre-ported version but with changed extension for synop routines.
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

! Use statements:
! <Interfaces>

USE ivalue_mod    !function

! <Data Modules>

USE metdb_com_mod, only : MISSIN

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT  !a01
INTEGER,          INTENT(INOUT) :: POINT   !a02 to first character of a group
INTEGER,          INTENT(IN)    :: NGRPS   !a03 number of groups in section
INTEGER,          INTENT(INOUT) :: IG      !a04 number of current group in section
INTEGER,          INTENT(IN)    :: IR      !a05 rainfall indicator
REAL,             INTENT(INOUT) :: IE      !a06 Type of instrumentation for evaporation (iE)
REAL,             INTENT(INOUT) :: EEE     !a07 Evaporation or evapotranspiration (EEE)
REAL,             INTENT(INOUT) :: SUN2    !a08 24 hour Sunshine amount (SSS)
REAL,             INTENT(INOUT) :: SUN1    !a09 1 hour Sunshine amount (SS)
REAL,             INTENT(INOUT) :: RNET    !a10 1 hour Net radiation
REAL,             INTENT(INOUT) :: GLSO    !a11 1 hour global radiation
REAL,             INTENT(INOUT) :: DISO    !a12 1 hour diffuse radiation
REAL,             INTENT(INOUT) :: LW      !a13 1 hour Longwave radiation
REAL,             INTENT(INOUT) :: SW      !a14 1 hour shortwave radiation
REAL,             INTENT(INOUT) :: SWNET   !a15 1 hour net shortwave radiation
REAL,             INTENT(INOUT) :: SDIR    !a16 1 hour direct solar radiation
REAL,             INTENT(INOUT) :: RNET24  !a17 24 hour Net radiation
REAL,             INTENT(INOUT) :: GLSO24  !a18 24 hour global radiation
REAL,             INTENT(INOUT) :: DISO24  !a19 24 hour diffuse radiation
REAL,             INTENT(INOUT) :: LW24    !a20 24 hour Longwave radiation
REAL,             INTENT(INOUT) :: SW24    !a21 24 hour shortwave radiation
REAL,             INTENT(INOUT) :: SWNET24 !a22 24 hour net shortwave radiation
REAL,             INTENT(INOUT) :: SDIR24  !a23 24 hour direct solar radiation
REAL,             INTENT(INOUT) :: PCHG24  !a24 24 hour Pressure change amount (P24P24P24)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  J1 ! second figure of 5-group
INTEGER          ::  J2 ! third figure of 5-group
INTEGER          ::  J4 ! last figure of 5-group
INTEGER          ::  I ! value from several figures
INTEGER          ::  ID ! indicates which radiation element
INTEGER          ::  SCALE ! set to 10**4 if daily, 10**3 if hourly


! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Loop round 5-groups (outer loop) & any following radiation groups
! (inner loop)

DOLABEL1: &
DO WHILE (IG <= NGRPS)
  IF (REPORT(POINT:POINT) /= '5') EXIT
  J1=IVALUE(REPORT(POINT+1:POINT+1))

! Use the second figure (J1) to identify the (main) 5-group.
! We're only interested in evapotranspiration (<4), sunshine and
! radiation (5) and 24-hour pressure change (8-9): ignore 4, 6 & 7.

! J1<4 indicates 5EEEiE: convert EEE from tenths of mm to kg/m**2
! (SYNOP code 1806, BUFR code 002004)

IFLABEL1: &
  IF (J1 >= 0 .AND. J1 <= 3) THEN
    I=IVALUE(REPORT(POINT+1:POINT+3))
    IF (I /= MISSIN) EEE=I*0.1
    IE=IVALUE(REPORT(POINT+4:POINT+4))

! J1=8 or 9 means 24-hour pressure change in mb & tenths: convert to Pa

  ELSE IF (J1 == 8 .OR. J1 == 9) THEN
    I=IVALUE(REPORT(POINT+2:POINT+4))
    IF (I /= MISSIN) THEN
      IF (J1 == 8) THEN
        PCHG24=I*10.
      ELSE IF (J1 == 9) THEN
        PCHG24=-I*10.
      END IF
    END IF

! This leaves sunshine and radiation groups.  The starting groups are
! 55sss, 553ss (daily & hourly sunshine), 55407, 55408, 55507, 55508.

  ELSE IF (J1 == 5) THEN                ! if second figure is 5,
    J2=IVALUE(REPORT(POINT+2:POINT+2))  ! look at third figure
IFLABEL2: &
    IF (J2 >= 0 .AND. J2 < 3) THEN      ! 55sss, daily sunshine
      I=IVALUE(REPORT(POINT+2:POINT+4)) ! tenths of hour
      IF (I /= MISSIN) SUN2=I*0.1       ! to hours
      SCALE=10000                       ! daily amounts follow
    ELSE IF (J2 == 3) THEN              ! 553ss, hourly sunshine
      I=IVALUE(REPORT(POINT+3:POINT+4)) ! tenths of hour
      IF (I /= MISSIN) SUN1=I*6         ! to minutes
      SCALE=1000                        ! hourly amounts follow
    ELSE IF (J2 == 4 .OR. J2 == 5) THEN ! 4 if daily, 5 if hourly
      J4=IVALUE(REPORT(POINT+4:POINT+4))! J4 sets ID=7 or ID=8
      IF (J2 == 4) SCALE=10000          ! daily radiation next
      IF (J2 == 5) SCALE=1000           ! hourly radiation next
    ELSE IF (REPORT(POINT+2:POINT+4) == '///') THEN ! no daily sun
      SCALE=10000                       ! so daily amounts next
    ELSE
      SCALE=MISSIN                      ! no radiation to follow
    END IF IFLABEL2

! If scale has been set, the 5-group just handled may be followed by
! radiation groups (one or more), daily or hourly as given by scale.
! Loop until another 55-group is found (5 but not 55 may be radiation!)
! - or 56-, 57-, 58-, 59-, all of which end the radiation section.

! N.B. Radiation groups MAY follow: only 55407 etc say they MUST follow.
! 55/// & 553// are obviously not worth reporting unless radiation does
! follow, but no regulation rules out these groups on their own - and
! some stations do send skeleton groups!

IFLABEL3: &
    IF (SCALE /= MISSIN) THEN
DOLABEL2: &
      DO WHILE (IG < NGRPS)
        IF (REPORT(POINT+6:POINT+7) >= '55' .AND. &
            REPORT(POINT+6:POINT+7) <= '59') EXIT
        IG=IG+1
        POINT=POINT+6
        ID=IVALUE(REPORT(POINT:POINT))
IFLABEL4: &
        IF (J2 == 4 .OR. J2 == 5) THEN
          ID=J4

! If a 6-group does not follow one of 55407 etc, then assume it is
! rainfall (and return) if rainfall is expected in section 3 and
! this is either the last group in section 3 or the last 6-group
! (i.e. a higher first figure follows)
! or if it starts 699 (most likely to be rainfall in mm).

        ELSE IF (ID == 6) THEN
          IF (IR == 0 .OR. IR == 2) THEN
            IF (IG == NGRPS) RETURN
            IF (REPORT(POINT+6:POINT+6) > '6') RETURN
          END IF
          IF (REPORT(POINT+1:POINT+2) == '99') RETURN
        ELSE IF (ID > 6) THEN
          RETURN
        END IF IFLABEL4

! If we've reached this point, the group must be radiation.

        I=IVALUE(REPORT(POINT+1:POINT+4))
IFLABEL5: &
        IF (I /= MISSIN) THEN
          I=I*SCALE
IFLABEL6: &
          IF (SCALE == 10000) THEN
            IF (ID == 0) RNET24=I
            IF (ID == 1) RNET24=-I
            IF (ID == 2) GLSO24=I
            IF (ID == 3) DISO24=I
            IF (ID == 4) LW24=-I
            IF (ID == 5) LW24=I
            IF (ID == 6) SW24=I
            IF (ID == 7) SWNET24=I
            IF (ID == 8) SDIR24=I
          ELSE IF (SCALE == 1000.) THEN
            IF (ID == 0) RNET=I
            IF (ID == 1) RNET=-I
            IF (ID == 2) GLSO=I
            IF (ID == 3) DISO=I
            IF (ID == 4) LW=-I
            IF (ID == 5) LW=I
            IF (ID == 6) SW=I
            IF (ID == 7) SWNET=I
            IF (ID == 8) SDIR=I
          END IF IFLABEL6
        END IF IFLABEL5
      END DO DOLABEL2              ! loop round radiation groups
    END IF IFLABEL3
  END IF IFLABEL1
  POINT=POINT+6
  IG=IG+1
END DO DOLABEL1                    ! loop round groups in section
RETURN
END SUBROUTINE SYNFIV
