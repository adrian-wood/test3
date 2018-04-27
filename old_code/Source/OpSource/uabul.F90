SUBROUTINE UABUL(BULL,PTR,BULEND,TTAAII,CCCC,YYGGGG,CORN,IFT)

!-----------------------------------------------------------------------

! PROGRAM      : UABUL
!
! PURPOSE      : To split upper air bulletin into reports to decode.
!
! DESCRIPTION  : First report in bulletin delimited by MiMiMjMj and
!                equal sign, subsequent reports assumed to start
!                after equal sign and a string of space(s) & CRLFs.
!                Last report accepted without an equal sign.
!
! CALLED BY    : MDBSTOR
!
! CALLS TO     : CCCODE, BULLED, UAEDIT, IVALUE
!
! ARGUMENTS    : (1) BULL    - bulletin
!                (2) PTR     - pointer
!                (3) BULEND  - length of bulletin
!                (4) TTAAII
!                (5) CCCC
!                (6) YYGGGG
!                (7) CORN
!                (8) IFT   (4-8 used elsewhere)
!
! REVISION INFO :
!
!
! $Workfile: uabul.F90$ $Folder: OpSource$
! $Revision: 1$ $Date: 24/01/2011 13:05:29$
!
! CHANGE RECORD :
!
! $Log:
!  1    MetDB_Refresh 1.0         24/01/2011 13:05:29    Rosemary Lavery
!       Initial port
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

USE BULLED_mod
USE CCCODE_mod
USE UAEDIT_mod
USE IVALUE_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT)  :: BULL   ! (a1) bulletin report
INTEGER, INTENT(INOUT)            :: PTR    ! (a2) pointer
INTEGER, INTENT(INOUT)            :: BULEND ! (a3) end of bulletin
CHARACTER (LEN=6), INTENT(IN)     :: TTAAII ! (a4) bulletin id header
CHARACTER (LEN=4), INTENT(IN)     :: CCCC   ! (a5) collecting centre
CHARACTER (LEN=6), INTENT(IN)     :: YYGGGG ! (a6) date/time of bulletin
CHARACTER (LEN=2), INTENT(IN)     :: CORN   ! (a7) report correction number
INTEGER, INTENT(IN)               :: IFT    ! (a8) FT unit number of storage dataset

! Local Variables

INTEGER  :: I
INTEGER  :: ICCCC     ! collecting centre number
INTEGER  :: IXXBB     ! position in message of "XXBB" if present
INTEGER  :: REPEND    ! pointer to last character (before =)
INTEGER  :: RSTART    ! pointer to start of report (MiMiMjMj)

! --------------------------------------------------------------------

SAVE

#if defined (MVS)
PTR=35             !This moves the pointer past the
                   !zczc235 etc to the TTAA/TTBB etc
#else
PTR=PTR-4          !This moves the pointer to the beginning of
                   !the TTAA or TTBB etc group
#endif

!---------------------------------------------------------------------
!Call CCCODE to get integer collecting centre number.
!---------------------------------------------------------------------

CALL CCCODE(287,ICCCC,CCCC)

!---------------------------------------------------------------------
!Point to start of first report in bulletin.
!Bulletin heading must be at least 35 characters: look on from there
! until recognisable start found.
!---------------------------------------------------------------------

DO WHILE (PTR < BULEND .AND.(      &
    (BULL(PTR:PTR+1) /= 'TT' .AND.  &      ! TEMP
     BULL(PTR:PTR+1) /= 'PP' .AND.  &      ! PILOT
     BULL(PTR:PTR+1) /= 'UU' .AND.  &      ! TEMP SHIP
     BULL(PTR:PTR+1) /= 'QQ' .AND.  &      ! PILOT SHIP
     BULL(PTR:PTR+1) /= 'XX' .AND.  &      ! DROPSONDE
     BULL(PTR:PTR+1) /= 'II' .AND.  &      ! TEMP MOBIL
     BULL(PTR:PTR+1) /= 'EE') .OR.  &      ! PILOT MOBIL
  (BULL(PTR+2:PTR+3) /= 'AA' .AND.  &
   BULL(PTR+2:PTR+3) /= 'BB' .AND.  &
   BULL(PTR+2:PTR+3) /= 'CC' .AND.  &
   BULL(PTR+2:PTR+3) /= 'DD')))
  PTR=PTR+1
END DO
IF (PTR >= BULEND) RETURN
RSTART=PTR

!---------------------------------------------------------------------
! If a dropsonde bulletin has an XXAA and an XXBB, and the XXAA
! has no launch time (no 31313 section), skip the XXAA, assuming
! the XXBB has a launch time to the minute and that, with several
! descents in an hour, the A's and B's couldn't be matched.
! No information should be lost by storing only the XXBB.
! If there is no XXBB, then the XXAA will be stored.
!    If the XXAA is to be skipped, take an identifier before it
! (for both parts) & put it after the XXBB, editing in place.
! This isn't necessary for reports with the identifier in a
! 61616 section.
!---------------------------------------------------------------------

IF_Dropsonde: &
IF (TTAAII(1:2) == 'UZ') THEN          ! If dropsonde bulletin

IF_XXBBchk: &
  IF (BULL(PTR:PTR+3) /= 'XXBB') THEN  ! & not XXBB at start,
    IXXBB=INDEX(BULL,'XXBB')           ! is there an XXBB later?

! If there's an XXBB later, and no 31313 before it, hence no
! launch time in the first report (XXAA?), then skip the XXAA.

IF_XXBB: &
    IF (IXXBB > 0 .AND. INDEX(BULL(:IXXBB),'31313 ') == 0) THEN
      RSTART=IXXBB
                                             ! If next group 4 figures
      IF (IVALUE(BULL(RSTART+5:RSTART+8)) >= 0 .AND.  &
        BULL(PTR:PTR+3) == 'XXAA') THEN ! & XXAA at start of bull,
        I=2                            ! delimit group at start
        DO WHILE (BULL(PTR-I:PTR-I) > ' ')
          I=I+1
        END DO
        I=I-1

! Overwrite XXBB with ident, put XXBB before it, and adjust start
! (ignoring anything that was between ident and XXBB)

        IF (BULL(PTR-I:PTR-I) >= 'A' .AND.  &
          BULL(PTR-I:PTR-I) <= 'Z') THEN
          BULL (RSTART-I+5:RSTART+3)=BULL(PTR-I:PTR-2) ! ident
          BULL (RSTART-I:RSTART-I+4)='XXBB ' ! XXBB before ident
          RSTART=RSTART-I              ! & move pointer to XXBB
        END IF
      END IF
    END IF IF_XXBB
  END IF IF_XXBBchk
END IF IF_Dropsonde

!---------------------------------------------------------------------
!Call BULLED to reduce any string of spaces and/or control characters
!to a single space.
!---------------------------------------------------------------------

CALL BULLED(RSTART,BULEND,BULL)

!---------------------------------------------------------------------
!Call UAEDIT for each report in turn, delimiting reports by equal signs
!(or the end of the bulletin).
!---------------------------------------------------------------------

10 CONTINUE

PTR=INDEX(BULL(RSTART:BULEND),'=')
IF (PTR > 0) THEN
  REPEND=RSTART+PTR-2
ELSE
  REPEND=BULEND
END IF

CALL UAEDIT(BULL(RSTART:REPEND),TTAAII,CCCC,ICCCC,YYGGGG,CORN,IFT)

! REPEND points to last figure: go past it, '=' and space to next report

IF (PTR > 0 .AND. REPEND < BULEND-9) THEN
  RSTART=REPEND+3
  GO TO 10
END IF

RETURN
END SUBROUTINE UABUL
