SUBROUTINE UATSTX (STRING, PTR, LEVEL, L, MAXL, ID, ERROR)

!-----------------------------------------------------------------------
!
! PROGRAM       : UATSTX
!
! PURPOSE       : TRY TO RECOVER FROM A SYNTAX ERROR IN A TTAA/TTCC
!
! DESCRIPTION   : THE AIM IS TO CORRECT THE FOLLOWING ERRORS
!                 - A LONG OR SHORT GROUP (SKIP THE LEVEL)
!                 - A CORRUPTED LEVEL INDICATOR (PRESSURE FIGURES)
!                 - A WRONG WIND INDICATOR
!                 A CORRECTION WILL ONLY BE ACCEPTED IF THE REST OF
!                 THE REPORT HAS THE CORRECT SEQUENCE OF LEVELS,
!                 THE ASSUMPTION BEING THAT A CORRECTION IS TOO
!                 RISKY IF THERE IS MORE THAN ONE ERROR.
!
! DATA TYPE(S)  : UPPER AIR TEMP PART A/C
!
! CALLED BY     : UASTDPR
!
! CALLS         : IVALUE (function)
!
! ARGUMENTS     : (1) INPUT STRING, 5-FIG GROUPS SEPARATED BY SPACES
!                     - UNLESS THERE'S AN ERROR!                  (I/O)
!                 (2) INPUT POINTER TO WHERE ERROR WAS RECOGNISED (I/O)
!                 (3) ARRAY OF PRESSURE FIGURES WHICH TAG LEVELS   (I)
!                 (4) SUBSCRIPT OF CURRENT LEVEL IN ABOVE ARRAY   (I/O)
!                 (5) SUBSCRIPT OF LAST LEVEL IN ABOVE ARRAY       (I)
!                 (6) INDICATOR FOR HIGHEST LEVEL WITH WIND (INT) (I/O)
!                 (7) ERROR FLAG SET TO                           (I/O)
!                     0 - first attempt at correction of this report
!                     1 - no safe correction found
!                     2 - long or short group: pointer (and maybe
!                          next level expected) reset
!                     3 - current pressure tag reset (in STRING)
!                     4 - wind indicator (top level with wind) reset
!                          (only in variable ID, not in STRING)
!
! REVISION INFO :
!
! $Workfile: uatstx.f90$ $Folder: OpSource$
! $Revision: 5$ $Date: 12/05/2011 15:37:14$
!
! CHANGE RECORD :
!
! $Log:
!  5    MetDB_Refresh 1.4         12/05/2011 15:37:14    Sheila Needham  syntax
!        error in IF block label
!  4    MetDB_Refresh 1.3         11/05/2011 16:55:52    Brian Barwell
!       Revision of all references to STRING to check for length exceeded.
!  3    MetDB_Refresh 1.2         19/04/2011 16:02:35    Sheila Needham  More
!       end of string tests
!  2    MetDB_Refresh 1.1         04/04/2011 17:14:57    Brian Barwell   Fixes
!       to prevent going off end of STRING.
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

USE IVALUE_mod

IMPLICIT NONE

! Interface Arguments

CHARACTER (LEN=*), INTENT(INOUT)  :: STRING    ! (a1)
INTEGER, INTENT(INOUT)            :: PTR       ! (a2)
CHARACTER (LEN=2), INTENT(IN)     :: LEVEL(:)  ! (a3)
INTEGER, INTENT(INOUT)            :: L         ! (a4)
INTEGER, INTENT(IN)               :: MAXL      ! (a5)
INTEGER, INTENT(INOUT)            :: ID        ! (a6)
INTEGER, INTENT(INOUT)            :: ERROR     ! (a7)

! Local Variables

INTEGER  :: BADID
INTEGER  :: I
INTEGER  :: J
INTEGER  :: LASTRI
INTEGER  :: LASTWO
INTEGER  :: LEND
INTEGER  :: LENSTR      ! Length of STRING
INTEGER  :: NEXTLEV
INTEGER  :: NGROUPS
INTEGER  :: NX
INTEGER  :: PEND
INTEGER  :: THISLEV

LOGICAL  :: BADSEQ
LOGICAL  :: TEST        ! Flag for IF statements

CHARACTER (LEN=1)  :: IND
CHARACTER (LEN=2)  :: WND
CHARACTER (LEN=2)  :: IL
CHARACTER (LEN=2)  :: BADLEV

! ---------------------------------------------------------------------

! Only one attempt at correction: too risky if more than one error.
! because the sequences of pressure tags & wind indicators are not
! decreasing, 1000mb being represented by 0, put a figure in front
! to make it easier to check level against ID.

IF (ERROR > 0) RETURN          ! ONLY ONE ATTEMPT ALLOWED
LENSTR = LEN(STRING)
IF (PTR > LENSTR) RETURN

ERROR=1                        ! SET ERROR IN CASE NO SAFE FIX
WND='0'//CHAR(240+ID)          ! TO COPE WITH ZERO IND FOR 1000MB
IF (WND == '00') WND='10'      ! USE 2-FIG IND IN COMPARISONS

! Find length of groups left in section (look for start of next section)
! & keep pointer to end of report for use later.

LEND=INDEX(STRING(PTR:),' 88')
IF (LEND == 0) LEND=INDEX(STRING(PTR:),' 77')
IF (LEND == 0) LEND=INDEX(STRING(PTR:),' 66')
IF (LEND == 0) LEND=LEN(STRING(PTR:))
PEND=PTR+LEND-1                ! KEEP POINTER TO END OF REPORT

! If the length left implies a bad group (not 5 figures) look first
! for the current level a few figures out, then for the
! next level (space followed by figures) & make the pointer correspond.
! The sequence of levels will be checked from there later.

IFBlock1: &
IF (MOD(LEND,6) /= 0) THEN
  IF (PTR > 5 .AND. PTR+5 <= LENSTR) THEN
    THISLEV=INDEX(STRING(PTR-5:PTR+5),' '//LEVEL(L))
  ELSE
    THISLEV = 0
  END IF

  IF (PTR+25 <= LENSTR) THEN
    NEXTLEV=INDEX(STRING(PTR:PTR+25),' '//LEVEL(L+1))
  ELSE
    NEXTLEV = 0
  END IF

  IF (THISLEV > 0) THEN
    ERROR=2                    ! LONG OR SHORT GROUP
    PTR=PTR-5+THISLEV          ! RESET POINTER
  ELSE IF (NEXTLEV > 0 .AND. MOD(LEND-NEXTLEV,6) == 0) THEN
    ERROR=2                    ! LONG OR SHORT GROUP
    PTR=PTR+NEXTLEV            ! RESET POINTER
    L=L+1                      ! & NEXT LEVEL EXPECTED
  END IF
ELSE

! If the length is such that all the groups could have 5 figures, then
! first look for the next level 2 or 3 groups on, depending on the wind
! indicator: if it is found, then the tag at level L is suspect.

  IL='0'//LEVEL(L)(1:1)
  IF (IL == '00') IL='10'
!                          Set I to location of next level indicator
  I = PTR + 12
  IF (IL >= WND) I = I + 6

  TEST = I < LENSTR
  IF (TEST) TEST = LEVEL(L+1) == STRING(I:I+1)
IFBlock2: &
  IF (TEST) THEN
    ERROR=3                    ! PRESSURE TAG SUSPECT
    BADLEV=STRING(PTR:PTR+1)   ! keep it for error message
    STRING(PTR:PTR+1)=LEVEL(L) ! RESET IT
  ELSE

! If the next level is not where it is expected, either there is more
! than one error or the wind indicator is wrong & we're one group out
! - look for the current missing level (L) one group on or back.

    IL='0'//LEVEL(L-1)(1:1)
    IF (IL == '00') IL='10'

    TEST = PTR > 6
    IF (TEST) TEST = STRING(PTR-6:PTR-5) == LEVEL(L)
IFBlock3: &
    IF (TEST) THEN
      IF (IL >= WND) THEN
        ERROR=4
        PTR=PTR-6
        IND=LEVEL(L-1)(1:1)
      END IF
    ELSE
      TEST = PTR+7 <= LENSTR
      IF (TEST) TEST = STRING(PTR+6:PTR+7) == LEVEL(L)
IFBlock3a: &
      IF (TEST) THEN

IFBlock4: &
        IF (IL < WND) THEN
          PTR=PTR+6

! If the current level started one group back, i.e. for the previous
! level three groups were expected but there were only two, then clearly
! the last level with a wind was the level before that, so the indicator
! can simply be reset as above.  If the current level starts one group
! on, there must be more levels with winds than the indicator shows,
! but we still have to work out how many.
! Look for a level starting 2 groups or 3 from the end.  If a level
! starts 3 groups from the end, then clearly all levels have winds;
! if a level starts 2 groups from the end, then knowing how many groups
! are left and (from the pressure figures) how many levels there are,
! we can work out how many levels have winds.
! If both groups could be starts of levels, i.e. the first two figures
! could be a wind direction (3 groups back) or a temperature (2 groups
! back) instead of a pressure, then there is too much uncertainty for
! a correction to be risked.

          LASTWO = 0
          IF (LENSTR >= 12) THEN
            DO I=L,MAXL
             IF (STRING(PEND-11:PEND-10) == LEVEL(I)) LASTWO=I
            END DO
          END IF

          LASTRI = 0
          IF (LENSTR >= 17) THEN
            DO I=L,MAXL
             IF (STRING(PEND-17:PEND-16) == LEVEL(I)) LASTRI=I
            END DO
          END IF

          NGROUPS=(PEND-PTR+1)/6
          IF (LASTRI > 0 .AND. (LASTRI-L+1)*3 == NGROUPS) THEN
            IF (LASTWO == 0) THEN
              ERROR=4
              IND=STRING(PEND-17:PEND-17)
            END IF
          ELSE IF (LASTWO > 0) THEN
            NX=NGROUPS-(LASTWO-L+1)*2

! The pressure tag 2 groups from the end tells us how many levels to
! expect in between.  NX is the number of groups left minus the number
! of groups which these levels would need if none of them had a wind,
! equal to the number of groups with winds (unless the tag is wrong!).
! We can't reset ID if the last wind is for 250mb or 150mb (without
! inserting ///// for the 200 or 100mb wind)

            IF (NX >= 0 .AND. NX <= MAXL-L) THEN
              IF (LEVEL(L+NX-1) /= '25' .AND.  &
                  LEVEL(L+NX-1) /= '15') THEN
                ERROR=4
                IND=LEVEL(L+NX-1)(1:1)
              END IF
            END IF
          END IF
        END IF IFBlock4
      END IF IFBlock3a
    END IF IFBlock3
  END IF IFBlock2
END IF IFBlock1
!**********************************************************************
!
! We now have a proposed correction - to pressure tag, wind indicator,
! current pointer, next level to be looked for or some combination of
! these.  So finally check levels L+1 to the end with these changes;
! look for start of next level after 2 or 3 groups, depending on wind
! indicator, and return with uncorrectable error if check fails.
!
!**********************************************************************

IFBlock5: &
IF (ERROR >= 2) THEN
  I=1
  J=PTR
  BADSEQ=.FALSE.

  IF (ERROR == 4) THEN
    BADID=ID                   ! keep bad Id for error message
    ID=IVALUE(IND)             ! TO RETURN RESET IND AS INTEGER
    WND='0'//IND               ! RESET 2-FIGURE WND WITH NEW IND
    IF (WND == '00') WND='10'  ! FOR USE IN COMPARISONS
  END IF

 23 CONTINUE

  IL='0'//LEVEL(L+I-1)(1:1)
  IF (IL == '00') IL='10'

  IF (IL >= WND) THEN
    TEST = J+19 <= LENSTR
    IF (TEST) TEST = LEVEL(L+I) == STRING(J+18:J+19)
    IF (TEST) THEN
      J=J+18                   ! NEXT LEVEL 3 GROUPS ON
    ELSE
      BADSEQ=.TRUE.
    END IF
  ELSE IF (IL < WND) THEN
    TEST = J+13 <= LENSTR
    IF (TEST) TEST = LEVEL(L+I) == STRING(J+12:J+13)
    IF (TEST) THEN
      J=J+12                   ! NEXT LEVEL 2 GROUPS ON
    ELSE
      BADSEQ=.TRUE.
    END IF
  END IF

  IF (BADSEQ) THEN
    ERROR=1                    ! NO CORRECTION ATTEMPTED
  ELSE IF (J < PEND .AND. L+I < MAXL) THEN
    I=I+1                      ! LOOK FOR NEXT LEVEL
    GO TO 23
  END IF
END IF IFBlock5

IF (ERROR == 2) PRINT *,'UATSTX: point past bad group to ', &
                        STRING(PTR:PTR+5)
IF (ERROR == 3) PRINT *,'UATSTX: level indicator changed from ', &
                        BADLEV,' to ',LEVEL(L)
IF (ERROR == 4) PRINT *,'UATSTX: Id changed from ',BADID,' to ',ID

RETURN
END SUBROUTINE UATSTX
