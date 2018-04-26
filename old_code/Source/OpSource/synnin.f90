SUBROUTINE SYNNIN(REPORT,NINGPS,WNDKTS,WPER, &
                  T1GUST,T2GUST,VGUST,T1GUS,T2GUS,VGUS, &
                  T1MEAN,T2MEAN,VMEAN, &
                  LSEA,SEASTE,SEAVIS,VV)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : SYNNIN
!
! PURPOSE       : TO GET DATA FROM 9-GROUPS IN SECTION 3 OF SYNOP:
!                 WIND, SEA & VISIBILITY
!
! DESCRIPTION   : LOOK FOR 904.. & 907.. (TIME GROUPS), 910.. & 911..
!                 (MAX GUST), 912.. (MAX MEAN WIND), 920.., 921.. AND
!                 924.. (SEA) AND 961.. (VISIBILITY IN DENSE FOG).
!
! CALLED BY     : SYNXP3
!
! ARGUMENTS     : (1) REPORT CHAR    REST OF REPORT STRING        (I)
!                 (2) NINGPS INTEGER NUMBER OF 9-GROUPS           (I)
!                 (3) WNDKTS LOGICAL TRUE IF UNITS ARE KNOTS      (I)
!                 (4) WPER   REAL    PAST WEATHER PERIOD (HOURS)  (I)
!                 (5) T1GUST REAL    START OF GUST PERIOD         (O)
!                 (6) T2GUST REAL    END OF GUST PERIOD           (O)
!                 (7) VGUST  REAL    MAX GUST                     (O)
!                 (8) T1MEAN REAL    START OF MAX WIND PERIOD     (O)
!                 (9) T2MEAN REAL    END OF MAX WIND PERIOD       (O)
!                (10) VMEAN  REAL    MAX MEAN WIND                (O)
!                (11) LSEA   LOGICAL SET IF SEA GROUP FOUND       (O)
!                (12) SEASTE REAL    STATE OF SEA                 (O)
!                (13) SEAVIS REAL    VISIBILITY TOWARDS SEA       (O)
!                (14) VV     REAL    VISIBILITY                   (O)
!
! REVISION INFO :
!
! $Workfile: synnin.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 23/12/2010 10:48:08$
!
! CHANGE RECORD :
!
! $Log:
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
USE metdb_com_mod, only : MISSIN, KTS2MPS, RMISS

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

CHARACTER(LEN=*), INTENT(IN)    :: REPORT         !a01 Rest of report string
INTEGER,          INTENT(IN)    :: NINGPS         !a02
LOGICAL,          INTENT(IN)    :: WNDKTS         !a03
REAL,             INTENT(IN)    :: WPER           !a04 Past weather period
REAL,             INTENT(INOUT) :: T1GUST         !a05 Start gust period of gust 911 group
REAL,             INTENT(INOUT) :: T2GUST         !a06 End gust period of gust 911 group
REAL,             INTENT(INOUT) :: VGUST          !a07 911 group gust value (ff/fff)
REAL,             INTENT(INOUT) :: T1GUS          !a08 Start gust period of gust 910 group
REAL,             INTENT(INOUT) :: T2GUS          !a09 End gust period of gust 910 group
REAL,             INTENT(INOUT) :: VGUS           !a10 910 group gust value (ff/fff)
REAL,             INTENT(INOUT) :: T1MEAN         !a11 Start mean period of 912 group
REAL,             INTENT(INOUT) :: T2MEAN         !a12 End mean period of 912 group
REAL,             INTENT(INOUT) :: VMEAN          !a13 912 group mean value (ff/fff)
LOGICAL,          INTENT(INOUT) :: LSEA           !a14
REAL,             INTENT(INOUT) :: SEASTE         !a15 State of sea 924 group (S)
REAL,             INTENT(INOUT) :: SEAVIS         !a16 Horiz vis towards sea 924 group (Vs)
REAL,             INTENT(INOUT) :: VV             !a17 Visibility (VV)

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  FIRSTCHR
INTEGER          ::  I
INTEGER          ::  IG
INTEGER          ::  IND
INTEGER          ::  LASTIND

LOGICAL          ::  PROCESS   !Set to false when next group already processed.

REAL             ::  END
REAL             ::  START
REAL             ::  TT
REAL             ::  VSCONV(0:9)
REAL             ::  WIND

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

!
! SINGLE-FIGURE VISIBILITY CODE (4300): CONVERT TO LOWEST VALUE IN RANGE
!
DATA VSCONV/0.,50.,200.,500.,1000.,2000.,4000.,10000., &
          20000.,50000./

!
! LOOP OVER 9-GROUPS (WHICH MAY BE FOLLOWED BY 0-GROUPS FOR WINDS >100
! - HENCE NOT SIMPLY DO 10 IG=1,NINGPS!
!
LASTIND=0
PROCESS=.TRUE.
DOLABEL1: &
DO IG=1,NINGPS
  FIRSTCHR=((IG-1)*6)+1
IFLABEL1: &
  IF(REPORT(FIRSTCHR:FIRSTCHR) == '9'.AND.PROCESS)THEN
    IND=IVALUE(REPORT(FIRSTCHR+0:FIRSTCHR+2))
!**********************************************************************
!
! WIND GROUPS
!
!**********************************************************************
!
! WIND GROUPS MAY BE PRECEDED BY TIME GROUPS: 907.. CHANGES PERIOD FROM
! 10 MINUTES, 904.. CHANGES END OF PERIOD FROM REPORT TIME TO EARLIER.
! SUCH A CHANGE ONLY APPLIES TO THE NEXT GROUP.
!
IFLABEL2: &
    IF (IND == 904 .OR. IND == 907) THEN
      I=IVALUE(REPORT(FIRSTCHR+3:FIRSTCHR+4))
      IF (I <= 60) THEN
        TT=I*6.0               ! TENTHS OF AN HOUR TO MINUTES
      ELSE IF (I >= 61 .AND. I <= 66) THEN
        TT=(I-60+6)*60         ! WHOLE HOURS (7-12) TO MINUTES
      ELSE IF (I > 66) THEN
        TT=RMISS
      END IF
      LASTIND=IND              ! KEEP 904/907 TO SET PERIOD
!
! 9-GROUPS WITH WINDS IN MAY HAVE SPEED=99 AND A 0-GROUP TO FOLLOW
! IF THE SPEED IS >100.  CONVERT KNOTS TO M/S IF NECESSARY.
!
    ELSE IF (IND == 910 .OR. IND == 911 .OR. IND == 912) THEN
      I=IVALUE(REPORT(FIRSTCHR+3:FIRSTCHR+4))
      IF(I == 99)THEN
! Check if last group in report, if it is then stop processing '9' groups
        IF (IG == NINGPS) RETURN
        IF (REPORT(FIRSTCHR+6:FIRSTCHR+7) == '00') THEN
          I=IVALUE(REPORT(FIRSTCHR+8:FIRSTCHR+10))
          PROCESS=.FALSE.
        END IF
      END IF
!
      WIND=I
      IF (I /= MISSIN .AND. WNDKTS) WIND=WIND*KTS2MPS
!
! THE PERIOD IS EITHER THE LAST 10 MINUTES, OR THE PRESENT WEATHER
! PERIOD, OR THE LAST TT MINUTES (TT AS IN 907.. GROUP) OR THE 10
! MINUTES ENDING TT MINUTES BEFORE THE REPORT TIME (TT AS IN 904..).
! EXPRESS THESE PERIODS AS MINUTES RELATIVE TO THE REPORT TIME.
!
    IFLABEL3: &
      IF (LASTIND == 907 .AND. TT /= RMISS) THEN
        START=-TT
        END=0
      ELSE IF (LASTIND == 904 .AND. TT /= RMISS) THEN
        START=-TT-10.
        END=-TT
      ELSE IF (LASTIND == 904 .OR. LASTIND == 907) THEN
        START=RMISS
        END=RMISS
      ELSE
        START=WPER*60.          ! CONVERT HOURS TO MINUTES
        END=0
      END IF IFLABEL3
      LASTIND=0
!
! AND SET THE APPROPRIATE ELEMENT (GUST OR MEAN WIND)
!
    IFLABEL4: &
      IF (IND == 910) THEN      ! GUST IN LAST 10 MINUTES
        T1GUST=-10.
        T2GUST=0
        VGUST=WIND
      ELSE IF (IND == 911) THEN ! GUST IN ANY OTHER PERIOD
        T1GUS=START
        T2GUS=END
        VGUS=WIND
      ELSE IF (IND == 912) THEN
        T1MEAN=START
        T2MEAN=END
        VMEAN=WIND
      END IF IFLABEL4
!**********************************************************************
!
! SEA GROUPS (920.., 921.., 924..)
!
!**********************************************************************
!
! STATE OF SEA: BUFR TABLE SAME AS WMO SYNOP CODE TABLE.  (IGNORE WIND
! FORCE IN LAST FIGURE, THE ONLY DIFFERENCE BETWEEN 920.. & 921..)
!
    ELSE IF (IND == 920 .OR. IND == 921) THEN
      LSEA=.TRUE.
      SEASTE=IVALUE(REPORT(FIRSTCHR+3:FIRSTCHR+3))
!
! STATE OF SEA & VISIBILITY: CONVERT VISIBILITY TO METRES FOR BUFR.
!
    ELSE IF (IND == 924) THEN
      LSEA=.TRUE.
      SEASTE=IVALUE(REPORT(FIRSTCHR+3:FIRSTCHR+3))
      I=IVALUE(REPORT(FIRSTCHR+4:FIRSTCHR+4))
      IF (I /= MISSIN) SEAVIS=VSCONV(I)
!**********************************************************************
!
! DENSE FOG GROUP (961..)           SEE W1W1 IN CODE TABLE 4687
! Set value close to midpoint of range (only tens of metres
! in BUFR 020001, so can't set 75m etc)
!
!**********************************************************************
    ELSE IF (IND == 961) THEN
      I=IVALUE(REPORT(FIRSTCHR+3:FIRSTCHR+4)) ! W1W1
      IF (I == 47) VV=80.       ! 60-90M
      IF (I == 48) VV=50.       ! 30-60M
      IF (I == 49) VV=20.       ! <30M
    END IF IFLABEL2
  ELSE IF(.NOT.PROCESS)THEN
! Only ignore next group
    PROCESS=.TRUE.
  END IF IFLABEL1
!
END DO DOLABEL1
RETURN
END SUBROUTINE SYNNIN
