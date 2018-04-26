SUBROUTINE MTRWWR(POINT,REPORT,GRPLEN,ELEM,ESTART,WDWD,REWW,ERROR)

!-----------------------------------------------------------------------
!
! SUBROUTINE    : MTRWWR
!
! PURPOSE       : To decode a recent or present weather group (METARs)
!                 or forecast weather (TAFs) into flags as described
!                 in BUFR flag tables 020192, 020193 & 020194.
!
! CALLED BY     : MTREXP, TAFEXP
!
! PARAMETERS    : (1)  POINT   start of group        (input/output)
!                 (2)  REPORT  report                (input)
!                 (3)  GRPLEN  group length          (input,changed)
!                 (4)  ELEM    element count array   (input/output)
!                 (5)  ESTART  position in ELEM      (input)
!                 (6)  WDWD    W'W' array            (output)
!                 (7)  REWW    REWW array            (output)
!                 (8)  ERROR   true if bad letters   (output)
!
! Note: ELEM(ESTART) is for W'W', ELEM(ESTART+1) is for REWW (METARs)
!      (See MTREXP and TAFEXP for layout of ELEM - which is NOT a flag
!       array, as is claimed in various places, but used in subscript
!       calculations!)
!
! REVISION INFO :
!
!
! $Workfile: mtrwwr.f90$ $Folder: OpSource$
! $Revision: 7$ $Date: 07/04/2011 12:28:40$
!
! CHANGE RECORD :
!
! $Log:
!  7    MetDB_Refresh 1.6         07/04/2011 12:28:40    Alison Weir
!       Correct intents and initialisations.
!  6    MetDB_Refresh 1.5         06/04/2011 15:46:11    Alison Weir
!       Initialise intent=out arguments. Check for >3 recent weather groups.
!  5    MetDB_Refresh 1.4         18/11/2010 13:37:42    John Norton
!       Updated as part of merge batch 13.
!  4    MetDB_Refresh 1.3         12/11/2010 17:13:53    Rosemary Lavery remove
!        old header
!  3    MetDB_Refresh 1.2         04/11/2010 15:42:47    Rosemary Lavery
!       removal of HEAD stmt
!  2    MetDB_Refresh 1.1         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  1    MetDB_Refresh 1.0         21/10/2010 16:38:08    Rosemary Lavery
!       Initial port
! $
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Interface Arguments                                                   cked

INTEGER,           INTENT(INOUT) ::  POINT    ! Start of group being checked
CHARACTER (LEN=*), INTENT(IN)    ::  REPORT   ! Report being expanded.
INTEGER,           INTENT(INOUT) ::  GRPLEN   ! Length of group (2-9 characters valid)
INTEGER,           INTENT(INOUT) ::  ELEM(:)  ! Counts for current & recent weather
INTEGER,           INTENT(IN)    ::  ESTART   ! Displacement within element array.
REAL,              INTENT(INOUT) ::  WDWD(:)  ! Expanded weather group.
REAL,              INTENT(INOUT) ::  REWW(:)  ! Expanded recent weather.
LOGICAL,           INTENT(OUT)   ::  ERROR    ! Group cannot be successfully decoded.

! Local Variables                                                       cked

CHARACTER (LEN=58)   ::  TABLE            ! Qualifier/Phenomenon letters
CHARACTER (LEN=2)    ::  PAIR             ! Pair of letters to look up in TABLE

INTEGER              ::  ID               ! Number of descriptor in TABLE
INTEGER              ::  L                ! Loop variable
INTEGER              ::  DESC             ! Weather descriptor.
INTEGER              ::  PHEN             ! Weather phenomenon.
INTEGER              ::  QUAL             ! Intensity or qualifier (table 020192)

LOGICAL              ::  LREWW            ! Flag set if recent weather group (RE)
LOGICAL              ::  FOUND            ! False if reported letters not in table


! Initialise variables.

DESC = 0
PHEN = 0
QUAL = 0
LREWW = .FALSE.
ERROR = .FALSE.

! TABLE lists possible descriptors & phenomena in recent or current
! weather groups (see the columns in code table 4678, though the
! order is as in our local flag tables 020193 & 020194).

TABLE( 1:16)='MIBCDRBLSHTSFZPR'     ! qualifiers
TABLE(17:32)='DZRASNSGICPEGRGS'     ! phenomena: precipitation
TABLE(33:46)='BRFGFUVADUSAHZ'       ! phenomena: obscuration
TABLE(47:58)='POSQFCSSDSUP'         ! other phenomena

! First see if the group is recent weather (starting RE)

IF_BLK1: &
IF (ELEM(ESTART) < 3) THEN

IF_BLK2: &
  IF (GRPLEN >= 4) THEN

IF_BLK3: &
    IF (REPORT(POINT:POINT+1) == 'RE') THEN
      POINT=POINT+2
      GRPLEN=GRPLEN-2
      LREWW=.TRUE.

! Otherwise it's current.  Does the group start VC (in the vicinity) ?

    ELSE IF (REPORT(POINT:POINT+1) == 'VC') THEN
      POINT=POINT+2
      GRPLEN=GRPLEN-2
      QUAL=5

! Old "heavy" intensity qualifier?

    ELSE IF (REPORT(POINT:POINT+1) == 'XX') THEN
      POINT=POINT+2
      GRPLEN=GRPLEN-2
      QUAL=3
    END IF IF_BLK3
  END IF IF_BLK2

! If none of the above, is the group length odd or even?
! Odd groups are only reported in the new code.
! Even groups can be either old or new code.
! If odd, it must start '+' or '-': set intensity to heavy or light.

IF_BLK4: &
  IF (GRPLEN < 4 .OR. (QUAL == 0 .AND. .NOT.LREWW)) THEN

IF_BLK5: &
    IF (MOD(GRPLEN,2) /= 0) THEN
      IF (REPORT(POINT:POINT) == '+') THEN
        QUAL=3
      ELSE IF (REPORT(POINT:POINT) == '-') THEN
        QUAL=1
      ELSE
        ERROR=.TRUE.
        RETURN
      END IF
      POINT=POINT+1
      GRPLEN=GRPLEN-1

! If length is even, and phenomenon not one of those which can't have
! an intensity, set intensity to moderate rather than default zero.

    ELSE
      IF (REPORT(POINT:POINT+1) /= 'MI' .AND. &
          REPORT(POINT:POINT+1) /= 'BC' .AND. &
          REPORT(POINT:POINT+1) /= 'FG' .AND. &
          REPORT(POINT:POINT+1) /= 'PR' .AND. &
          REPORT(POINT:POINT+1) /= 'BR' .AND. &
          REPORT(POINT:POINT+1) /= 'FU' .AND. &
          REPORT(POINT:POINT+1) /= 'HZ') THEN
        QUAL=2
      END IF
    END IF IF_BLK5
  END IF IF_BLK4

! If there are 3 groups already, give up - there's no room in the array

ELSE
  ERROR=.TRUE.
  RETURN
END IF IF_BLK1

! Likewise, if there are 3 recent weather groups, give up

IF (LREWW .AND. ELEM(ESTART+1) >= 3) THEN
  ERROR=.TRUE.
  RETURN
END IF

! The group length at this point must be even.  If it's more than 8
! there's something wrong.

IF (GRPLEN > 8) THEN
  ERROR=.TRUE.
  RETURN
END IF

! Look for pairs of letters in the list of qualifiers & phenomena from
! code tables 020193 & 020194, skipping a pair if consecutive pairs
! are the same.
! The table includes PE for pellets, replaced by PL in 1998 to avoid
! (for political correctness) the combination RAPE; PE must still be
! handled for old data (only expanded when retrieved!).

FOUND=.TRUE.
L=1

DO_BLK1: &
DO WHILE (FOUND .AND. L <= GRPLEN/2)
  PAIR=REPORT(POINT:POINT+1)
  IF (L == 1 .OR. PAIR /= REPORT(POINT-2:POINT-1)) THEN
    IF (PAIR == 'PL') PAIR='PE'
    FOUND=.FALSE.
    ID=0
    DO WHILE (.NOT.FOUND .AND. ID < 29)
      ID=ID+1
      IF (PAIR == TABLE(ID*2-1:ID*2)) THEN
        FOUND=.TRUE.
        IF (ID <= 8) THEN
          DESC=2**(ID-1)+DESC
        ELSE
          PHEN=2**(ID-1-8)+PHEN
        END IF
      END IF
    END DO
  END IF
  POINT=POINT+2
  L=L+1
END DO DO_BLK1

! If any reported characters not found in table, return.

IF (.NOT.FOUND) THEN
  ERROR=.TRUE.
  RETURN
END IF

! Increment output array subscript & return values

IF (LREWW) THEN
  ELEM(ESTART+1)=ELEM(ESTART+1)+1
  IF (DESC /= 0) REWW(1+(ELEM(ESTART+1)-1)*2)=DESC
  IF (PHEN /= 0) REWW(2+(ELEM(ESTART+1)-1)*2)=PHEN
ELSE
  ELEM(ESTART)=ELEM(ESTART)+1
  IF (DESC > 0) WDWD(2+(ELEM(ESTART)-1)*3)=DESC
  IF (PHEN > 0) WDWD(ELEM(ESTART)*3)=PHEN
  IF (QUAL > 0) WDWD(1+(ELEM(ESTART)-1)*3)=QUAL
END IF

RETURN
END SUBROUTINE MTRWWR
