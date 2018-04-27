SUBROUTINE UADUPS(INDEKS,NTRIES,IDENT,ENTRY,IND)

!-----------------------------------------------------------------------
!
! ROUTINE       : UADUPS
!
! PURPOSE       : To find an index entry for the same ascent as this
!                 upper air part.
!
! DESCRIPTION   : The reason for a separate subroutine is that the
!                 minute may or may not be set for a particular part
!                 in the transitional period from launch time only in
!                 part B to launch time in all parts.  Hence the
!                 complicated decision process explained below.
!                    If this part has a launch time (and hence the
!                 minute is set) & the index entry has a nominal
!                 hour (minute missing), the index entry time will
!                 be changed to the launch time - and if the index
!                 entry has a launch time, this will be set in the
!                 trailer for this part.
!                 N.B. If the nominal time is the start of an
!                   index period and the launch time earlier,
!                   different parts of the same ascent will be
!                   indexed in different periods and hence never
!                   combined.  But with index periods starting at
!                   9Z & 21Z this should hardly ever happen.
!
! CALLED BY     : TAFREP (only for upper air)
!
! CALLS         : nothing
!
! ARGUMENTS     : (1) array of index entries                     (i/o)
!                      (nominal hour may be changed to launch
!                       time in matching entry on return)
!                 (2) number of entries in array                  (i)
!                 (3) identifier to match                         (i)
!                 (4) entry to match (with hour & minute set)     (i)
!                 (5) subscript of matching entry (0 if none)     (o)
!
! REVISION INFO :
!
! $Workfile: uadups.f90$ $Folder: OpSource$
! $Revision: 4$ $Date: 03/02/2012 10:37:45$
!
! CHANGE RECORD :
!
! $Log:
!  4    MetDB_Refresh 1.3         03/02/2012 10:37:45    Sheila Needham
!       Removed PRINT* with binary data
!  3    MetDB_Refresh 1.2         07/01/2011 09:46:52    John Norton     Post
!       MDBSTOR batch 4 porting.
!  2    MetDB_Refresh 1.1         07/01/2011 09:42:39    John Norton
!       Original f77 pre-porting version!
!  1    MetDB_Refresh 1.0         10/12/2010 16:42:02    John Norton     After
!       MDBSTOR batch 4 porting.
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

! None

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

INTEGER,           INTENT(IN)    :: NTRIES
CHARACTER(LEN=23), INTENT(INOUT) :: INDEKS(NTRIES)
CHARACTER(LEN=10), INTENT(IN)    :: IDENT
CHARACTER(LEN=23), INTENT(INOUT) :: ENTRY
INTEGER,           INTENT(OUT)   :: IND

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER          ::  PART_HOUR   ! hour of new part
INTEGER          ::  PART_MIN    ! minute of new part
INTEGER          ::  INDEX_HOUR  ! hour of this index entry
INTEGER          ::  INDEX_MIN   ! minute of this index entry
INTEGER          ::  I           ! loop variable
INTEGER          ::  MISSING=255 ! all ones in one byte

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

PART_HOUR=MOD(ICHAR(ENTRY(1:1)),64)
PART_MIN=ICHAR(ENTRY(2:2))
IND=0

! Loop round the index entries, stopping if a suitable entry is found.
! (Check lat/long as well as ident in case there's an error in
! the lat/long groups in a mobile part.)
! If the new part has a time to the minute, then it can be part of an
! ascent for the same hour & minute, obviously, or an ascent with no
! minute set for the same hour or the next.  (The next because e.g.
! British stations & 03953 both launch at, say, 1115Z, but British
! stations set a nominal hour of 11Z, Valentia 12Z.)  A new part
! with no minute can be indexed as part of an ascent for the same hour,
! with or without minutes, or the previous hour, with minutes set.
! N.B. The hours compared are relative to the start of the index
! period, currently (July 2000) 9Z or 21Z.

I=1
DOLABEL1: &
DO WHILE (IND == 0 .AND. I <= NTRIES)
IFLABEL1: &
  IF (INDEKS(I)(3:11) == IDENT(1:9) .AND. &
      INDEKS(I)(13:16) == ENTRY(13:16)) THEN
    INDEX_HOUR=MOD(ICHAR(INDEKS(I)(1:1)),64)
    INDEX_MIN=ICHAR(INDEKS(I)(2:2))
    IF (INDEX_MIN /= MISSING) INDEX_MIN=MOD(INDEX_MIN,64)
IFLABEL2: &
    IF (PART_MIN == MISSING) THEN
      IF (INDEX_HOUR == PART_HOUR .OR. &
         (INDEX_MIN < 60 .AND. INDEX_HOUR+1 == PART_HOUR)) THEN
        IND=I
      END IF
    ELSE
IFLABEL3: &
      IF ((INDEX_HOUR == PART_HOUR .AND. INDEX_MIN == PART_MIN) &
             .OR. (INDEX_MIN == MISSING .AND.               &
                  (INDEX_HOUR == PART_HOUR .OR.             &
                   INDEX_HOUR == PART_HOUR+1))) THEN
        IND=I

! If launch time is set in this part, set it in index too; flag
! minutes to show if launch was in nominal hour or hour before.
! (Flags are as follows: top bit set if previous hour, next bit
! if same hour; flags only set in index entry, not in trailer.)

IFLABEL4: &
        IF (INDEX_MIN == MISSING .AND. PART_MIN < 60) THEN
          INDEKS(I)(1:1)=CHAR(PART_HOUR)
          IF (INDEX_HOUR == PART_HOUR) THEN
            INDEKS(I)(2:2)=CHAR(64+PART_MIN)
          ELSE
            INDEKS(I)(2:2)=CHAR(128+PART_MIN)
          END IF

! If launch time is in index (from another part) but not trailer,
! set trailer time to launch time from index.

        ELSE IF (INDEX_MIN /= MISSING .AND. &
                 PART_MIN == MISSING) THEN
          ENTRY(1:1)=CHAR(INDEX_HOUR)
          ENTRY(2:2)=INDEKS(I)(2:2)
        END IF IFLABEL4
      END IF IFLABEL3
    END IF IFLABEL2
  ELSE IF (INDEKS(I)(3:11) == IDENT(1:9) .AND. &
      INDEKS(I)(13:16) /= ENTRY(13:16)) THEN
    PRINT *,'UADUPS: same ident & time but lat/long diff'
  END IF IFLABEL1
  I=I+1
END DO DOLABEL1
RETURN
END SUBROUTINE UADUPS
