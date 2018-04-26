SUBROUTINE UAXPAND(OB,TT,ARRAY,NUM_LEV,IDENT,DESCR,   &
                   B17BIT,PART_TYPE,TYPE,STANDRD,ERR, &
                   TB17BIT5,QCBIT_ARRAY)

!-----------------------------------------------------------------------
!
! PROGRAM       : UAXPAND
!
! PURPOSE       : MASTER PROGRAM FOR UPPER AIR EXPANSION
!
! DESCRIPTION   : FINDS SECTIONS IN REPORT & CALLS EXPANSION PROGRAMS
!
! DATA TYPE(S)  : UPPER AIR (TEMP & PILOT, ALL PARTS)
!
! CALLED BY     : UAEDIT
!
! CALLS         : UAPART, UAHEAD, UATSTDP, UATROP, UAMAXW, UASIGPR,
!                 UAPSTD, UASIGHT, UASONDE, UACLOUD, IDES
!
! ARGUMENTS     : (1) REPORT (STARTING AFTER 'TTAA' OR 'PPBB' OR...)
!                 (2) TT FROM BULLETIN HEADING
!                 (3) ARRAY OF VALUES TO ENCODE - INPUT/OUTPUT
!                 (4) NUMBER OF ELEMENTS IN ARRAY
!                 (5) IDENTIFIER
!                 (6) DESCRIPTOR TO BE PASSED TO ENCODE
!                 (7) BYTE17, BITS5-8 OF INDEX ENTRY - OUTPUT
!                 (8)
!                 (9)
!                 (10)
!                 (11)
!                 (12) INDICATES PART A,B,C OR D - OUTPUT
!                 (13) 1-BIT QC FLAG FOR EACH ELEMENT - INPUT/OUTPUT
!
! REVISION INFO :
!
! $Workfile: uaxpand.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 01/06/2011 15:59:14$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         01/06/2011 15:59:14    Brian Barwell   Add
!       checks for OB subscripts going out of bounds.
!  4    MetDB_Refresh 1.3         04/04/2011 17:17:00    Brian Barwell   Trap
!       for bulletins ending '21212='.
!  3    MetDB_Refresh 1.2         28/03/2011 16:25:30    Brian Barwell
!       QCBIT_ARRAY changed to INOUT and IF block restructured.
!  2    MetDB_Refresh 1.1         26/01/2011 14:22:21    Rosemary Lavery
!       updates post review
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE IDES_mod
USE METDB_COM_mod, ONLY: RMISS
USE UACLOUD_mod
USE UAHEAD_mod
USE UAMAXW_mod
USE UAPART_mod
USE UAPSTD_mod
USE UASIGHT_mod
USE UASIGPR_mod
USE UASONDE_mod
USE UATROP_mod
USE UATSTDP_mod

IMPLICIT NONE

CHARACTER (LEN=*), INTENT(INOUT) :: OB           ! (a1)  report being expanded
CHARACTER (LEN=2), INTENT(IN)    :: TT           ! (a2)  report type ie. fixed land
REAL, INTENT(INOUT)              :: ARRAY(999)   ! (a3)  expanded elements array
INTEGER, INTENT(INOUT)           :: NUM_LEV      ! (a4)
CHARACTER (LEN=10), INTENT(OUT)  :: IDENT        ! (a5)  index station ID
INTEGER, INTENT(OUT)             :: DESCR(:)     ! (a6)  expanded descriptor sequence
CHARACTER (LEN=1), INTENT(OUT)   :: B17BIT       ! (a7)  byte17 index bits 5 - 8
INTEGER, INTENT(OUT)             :: PART_TYPE    ! (a8)
CHARACTER (LEN=2), INTENT(OUT)   :: TYPE         ! (a9)
LOGICAL, INTENT(OUT)             :: STANDRD      ! (a10)  indicates to uasort standrd
LOGICAL, INTENT(OUT)             :: ERR          ! (a11)  Indicates error in decode
INTEGER, INTENT(OUT)             :: TB17BIT5     ! (a12)  Trailer Byte17 Bit 5
REAL, INTENT(INOUT)              :: QCBIT_ARRAY(999)  ! (a13) array of 1 bit QC flags

! Local Variables

INTEGER  :: PTR                     !pointer within report
INTEGER  :: BLOCK                   !wmo block number
INTEGER  :: LENG                    !Length of report
INTEGER  :: STN                     !wmo station number
INTEGER  :: REPS
INTEGER  :: ID
INTEGER  :: I                       !used in loops
INTEGER  :: I3
INTEGER  :: I4
INTEGER  :: IX
INTEGER  :: I5                      ! pointer to 51515
INTEGER  :: I52                     ! pointer to 52525
INTEGER  :: I21212                  !identify this section in rpt
INTEGER  :: STNHT                   !station height
INTEGER  :: BASE                    !displacement in standrd level
INTEGER  :: SIG_BASE                !displacement in sig level

LOGICAL  :: WINDS
LOGICAL  :: KNOTS                   !Indicates wind speed in knots
LOGICAL  :: TSEQOK
LOGICAL  :: WSEQOK
LOGICAL  :: TEMP                    !indicates TEMP report
LOGICAL  :: PILOT                   !indicate PILOT report

CHARACTER (LEN=1)  :: PART          !character part
                                    !or sig level reports
SAVE

! ---------------------------------------------------------------------

!initialize variables
ERR=.FALSE.                         !set error as false
TEMP=.FALSE.                        !report type not yet known
PILOT=.FALSE.                       !report type not yet known
STANDRD=.TRUE.
NUM_LEV=1
PTR=1                               ! point to start of report
LENG=LEN(OB)                        ! length of report
IX=0
IDENT='          '                  ! initialize output variable

!-------------------------------------------------------------------
!Get Report type and Part
!-------------------------------------------------------------------

CALL UAPART(OB,TT,PART,PART_TYPE,STANDRD,TEMP,PILOT)

!----------------------------------------------------------------------
! If pilot part B or D has 21212 section, treat it like TEMP B/D later.
! (but check for 21212 even if TEMP, in case no temperatures)
!----------------------------------------------------------------------

IF (PART == 'B'.OR.PART == 'D') I21212=INDEX(OB,'21212')

!----------------------------------------------------------------------
! call header program to handle groups common to all parts: date/time
! & station number or call sign & position.  the wind indicator in the
! last figure of the date/time group is returned, and part is set.
!----------------------------------------------------------------------

CALL UAHEAD(OB,PTR,LENG,TT,PART,ARRAY,BLOCK,STN,IDENT,  &
       TYPE,ID,KNOTS,ERR,B17BIT,QCBIT_ARRAY)

!----------------------------------------------------------------------
!check the error return from uahead. If it is set true then the decode
!hasnt worked properly and we do not have the minimum required to store
!the report. the rest of the decode will be skiped and the message lost
!----------------------------------------------------------------------

IFBlock1: &
IF (.NOT. ERR) THEN

!**********************************************************************
!
! TEMP part a/c
!
!**********************************************************************

!-----------------------------------------------------------------------
! standard levels   (give up if no 99 (part a) or 70 (part c) at start)
!-----------------------------------------------------------------------

IFBlock2: &
  IF (TEMP.AND.(PART == 'A'.OR.PART == 'C')) THEN
    DESCR(1)=IDES(302197)    ! DESCRIPTOR FOR STANDARD LEVELS
    IX=0

    IF (PTR <= LENG) THEN
      IF (LEN(OB(PTR:)) > 10) THEN
        I3=INDEX(OB(PTR:),' 31313')
        IF (I3 > 0) CALL UASONDE(OB(PTR+I3:LENG),ARRAY,QCBIT_ARRAY)

        IF (PART == 'A') THEN
          IX=INDEX(OB(PTR-1:),' 99') ! TTAA:LOOK FOR SURFACE
        ELSE IF (PART == 'C') THEN
          IX=INDEX(OB(PTR-1:),' 70') ! TTCC: LOOK FOR 70MB
        END IF
      END IF
    END IF

! If no levels found, make sure count is zero (UASONDE may have
! been called & set radiosonde type in that slot! (an) ) before
! abandoning expansion.

    IF (IX == 0) THEN
      ARRAY(14)=0
      RETURN
    END IF

    PTR=PTR+IX-1                   ! POINT TO 99 OR 70

    CALL UATSTDP(OB,PTR,ARRAY,NUM_LEV,PART,ID,KNOTS,BASE,QCBIT_ARRAY)

    ARRAY(14)=NUM_LEV
    QCBIT_ARRAY(14)=99.0

!-----------------------------------------------------------------------
! TROPOPAUSE(S)    (SECTION STARTING 88..., 88999 IF NO DATA)
!-----------------------------------------------------------------------
    IX=0
    IF (PTR < LENG) THEN
      IF (LEN(OB(PTR:)) > 10) IX=INDEX(OB(PTR-1:),' 88')   ! TROPOPAUSE

      IF (IX > 0 .AND. PTR+IX+3 <= LENG) THEN
        IF (OB(PTR+IX+1:PTR+IX+3) /= '999') THEN
          PTR=PTR+IX-1                 ! POINT TO 88

          CALL UATROP(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY)
          ARRAY(14)=NUM_LEV
        END IF                         ! IF NO TROPOPAUSE,
      END IF
    END IF
!-----------------------------------------------------------------------
! MAX WIND(S)      (SECTION STARTING 77... OR 66..., ..999 IF NO DATA)
!-----------------------------------------------------------------------
    IX=0
    IF (PTR < LENG) THEN
      IF (LEN(OB(PTR:)) > 10) THEN
        IX=INDEX(OB(PTR-1:),' 77')   ! MAX WIND
        IF (IX == 0) IX=INDEX(OB(PTR-1:),' 66')
      END IF

      IF (IX > 0 .AND. PTR+IX+3 <= LENG) THEN
        IF (OB(PTR+IX+1:PTR+IX+3) /= '999') THEN
          PTR=PTR+IX-1                 ! POINT TO 77 OR 66
          CALL UAMAXW(OB,PTR,ARRAY,NUM_LEV,PART,KNOTS,QCBIT_ARRAY,LENG)
          ARRAY(14)=NUM_LEV
        END IF                         ! IF NO MAX WIND,
      END IF
    END IF

!**********************************************************************
!
! TEMP PART B/D  (OR PILOT B/D WITH 21212 AND HENCE PRESSURE SENSOR)
!
!**********************************************************************
!
! IF TTBB, FIRST LOOK FOR SECTIONS AT END (31313, 41414, REGIONAL 51515)
! TO COMPLETE INSTRUMENTATION DETAILS AND SURFACE OR LOW-LEVEL DATA.
!
  ELSE IF ((PART == 'B'.OR.PART == 'D') .AND.  &
          (TEMP.OR.(PILOT.AND.I21212 > 0))) THEN
     TB17BIT5=0
    DESCR(1)=IDES(302198)      ! SIGNIFICANT LEVELS (P=COORD)

IFBlock3: &
    IF (LEN(OB(PTR:)) > 10) THEN
      I3=INDEX(OB(PTR:),' 31313')
      IF (I3 > 0) THEN
        CALL UASONDE(OB(PTR+I3:LENG),ARRAY,QCBIT_ARRAY)
      ELSE
        DO I=14,17              ! IF NO 31313, MINUTE OF LAUNCH
          ARRAY(I)=RMISS           ! & SONDE DETAILS ARE MISSING
          QCBIT_ARRAY(I)=0
        END DO
      END IF

! --------------------------------------------------------------------
!look to see if cloud group section present
! --------------------------------------------------------------------

      I4=INDEX(OB(PTR:),' 41414')  ! (WON'T BE FOUND IF PART D)
      IF ((PTR+I4+10) <= LENG) THEN
        CALL UACLOUD(OB(PTR+I4:),ARRAY,QCBIT_ARRAY)
      ELSE
        ARRAY(18)=0                !IF NO CLOUD GROUP, ZERO COUNT
      END IF

! --------------------------------------------------------------------
!using the number of cloud groups replicated we can calculate the     !
!correct base displacement for the other elements in a sig level report
! --------------------------------------------------------------------

      SIG_BASE=13+(4*ARRAY(18))

      NUM_LEV=0

! --------------------------------------------------------------------
! "SEMI-STANDARD" LEVELS: 775MB ETC  (SOMETIMES STILL 925MB)          !
! --------------------------------------------------------------------

      I5=INDEX(OB(PTR:),'51515') ! not found if part D
      I52=INDEX(OB(PTR:),'52525')
      IF (I5 > 0) THEN
        I5=PTR+I5+5
        IF (OB(I5:I5+1) == '77') THEN
          CALL UATSTDP(OB,I5,ARRAY,NUM_LEV,PART,ID,KNOTS,  &
          SIG_BASE,QCBIT_ARRAY)
        END IF
      ELSE IF (I52 > 0) THEN
        I52=PTR+I52+5
        IF (OB(I52:I52+1) == '92') THEN
          CALL UATSTDP(OB,I52,ARRAY,NUM_LEV,PART,ID,KNOTS, &
          SIG_BASE,QCBIT_ARRAY)
        END IF
      END IF

      NUM_LEV=NUM_LEV+1

! SIGNIFICANT TEMPERATURES (1ST LEVEL STARTS 00 IN PART B, 11 IN PART D)
! (make sure 00/11 isn't past 21212, i.e. in wind section!)

      IF (TEMP) THEN
        IF (PTR < LENG) THEN
          IF (PART == 'B') IX=INDEX(OB(PTR-1:),' 00')
          IF (PART == 'D') IX=INDEX(OB(PTR-1:),' 11')
          IF (IX == 0) RETURN
          WINDS=.FALSE.
          IF (PTR+IX < I21212 .OR. I21212 == 0) THEN
            PTR=PTR+IX-1             ! POINT TO 00 OR 11
            CALL UASIGPR(OB,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS, &
               TSEQOK,SIG_BASE,QCBIT_ARRAY)
          END IF
        END IF
      END IF

! SIGNIFICANT WINDS  (FIRST CALL IS FOR WINDS IN EUROPEAN 51515 SECTION,
!                       WHICH GO IN SAME PART OF OUTPUT ARRAY)
      WINDS=.TRUE.
      IF (PART == 'B' .AND. I5 > 0 .AND. I5 < LENG) THEN
        IF (OB(I5:I5+1) == '11') CALL UASIGPR (OB,I5,ARRAY,  &
            NUM_LEV,' ',WINDS,KNOTS,WSEQOK,SIG_BASE,QCBIT_ARRAY)
      END IF
      IX=0
      IF(PTR < LENG) THEN
        IF (LEN(OB(PTR:)) > 10) IX=INDEX(OB(PTR:),'21212')
        IF (IX > 0) THEN
          PTR=PTR+IX+5               ! POINT TO GROUP AFTER 21212
          IF (LENG >= PTR+10) THEN   ! (To catch reports ending 21212=)
            WINDS=.TRUE.
            CALL UASIGPR(OB,PTR,ARRAY,NUM_LEV,PART,WINDS,KNOTS, &
                 WSEQOK,SIG_BASE,QCBIT_ARRAY)
          END IF
        END IF
      END IF
    END IF IFBlock3

!**********************************************************************
!
! PILOT PART A/C
!
!**********************************************************************

! STANDARD WINDS  (LOOK FOR GROUP ENDING 85 OR 70, PRESS FOR 1ST LEVEL)

  ELSE IF (PILOT.AND.(PART == 'A'.OR.PART == 'C')) THEN

    DESCR(1)=IDES(302197)    ! DESCRIPTOR FOR STANDARD LEVELS
    IF (LEN(OB(PTR:)) > 10) THEN
      IF (PART == 'A') THEN
        IX=INDEX(OB(PTR:),'85 ')
        IF (IX == 0) IX=INDEX(OB(PTR:),'70 ')
        IF (IX == 0) IX=INDEX(OB(PTR:),'50 ')
      ELSE IF (PART == 'C') THEN
        IX=INDEX(OB(PTR:),'70 ')
      END IF
    END IF
    IF (IX == 0) RETURN

! THE GROUP ENDING WITH 85 OR 70 MUST START WITH 44 OR 55.  44 MEANS A
! PRESSURE SENSOR WAS USED

    PTR=PTR+IX-4                 ! START OF GROUP ENDING 85 OR 70
    IF (OB(PTR:PTR+1) /= '44' .AND. OB(PTR:PTR+1) /= '55') RETURN
    IF (OB(PTR:PTR+1) == '55') THEN
      TB17BIT5=1
    ELSE IF (OB(PTR:PTR+1) == '44') THEN
      TB17BIT5=0
    END IF
    CALL UAPSTD(OB(:),PTR,ARRAY,NUM_LEV,PART,BLOCK,STN,KNOTS,  &
    QCBIT_ARRAY)

! MAX WIND(S)                 (GROUPS START WITH 6 OR 7, SINGLE FIGURE
!                               IF HEIGHT FOLLOWS, DOUBLE IF PRESSURE
    IX=0
    IF(PTR < LENG) THEN
      IF (LEN(OB(PTR:)) > 10) THEN
        IX=INDEX(OB(PTR-1:),' 7')  ! MAX WIND INDICATOR IS 7 OR 77
        IF (IX == 0) IX=INDEX(OB(PTR-1:),' 6')    ! 6 OR 66 AT TOP
      END IF
      IF (IX > 0 .AND. PTR+IX+3 <= LENG) THEN
        IF (OB(PTR+IX+1:PTR+IX+3) /= '999') THEN
          PTR=PTR+IX-1                 ! POINT TO (FIRST) 6 OR 7
          CALL UAMAXW(OB(:),PTR,ARRAY,NUM_LEV,PART,KNOTS,  &
               QCBIT_ARRAY,LENG)
        END IF                         ! IF NO MAX WIND,
      END IF
    END IF
    ARRAY(14)=NUM_LEV
    QCBIT_ARRAY(14)=99

!**********************************************************************
!
! PILOT PART B/D  (ALREADY TREATED AS TEMP IF 21212 SECTION, SO MUST
!                 HAVE HEIGHT AS VERTICAL COORDINATE IF HANDLED HERE)
!
!**********************************************************************

! SIGNIFICANT WINDS

  ELSE IF (PILOT.AND.(PART == 'B'.OR.PART == 'D')) THEN
    DESCR(1)=IDES(302198)     ! SIGNIFICANT LEVELS (HT=COORD)
    CALL UASIGHT(OB,PTR,ARRAY,NUM_LEV,PART,STNHT,KNOTS,  &
    13,QCBIT_ARRAY,LENG)
    TB17BIT5=1
  END IF IFBlock2

! --------------------------------------------------------------------
!set the number of replicated levels IF SIGNIFICANT LEVELS
! --------------------------------------------------------------------
  IF (PART == 'B' .OR. PART == 'D') THEN
    IF (ARRAY(18) < -99999) ARRAY(18)=0
    REPS=(ARRAY(18)*4)+19
    ARRAY(REPS)=NUM_LEV-1
    QCBIT_ARRAY(REPS)=99
  END IF
ELSE                               !end of error check from uahead
END IF IFBlock1

RETURN
END SUBROUTINE UAXPAND
