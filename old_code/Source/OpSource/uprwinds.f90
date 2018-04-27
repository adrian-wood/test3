SUBROUTINE UPRWINDS(PROFILE,MXFINELMS,MAXLEV)

!-----------------------------------------------------------------------
!
! subroutine    : UPRWINDS
!
! purpose       : To combine TEMP & PILOT winds
!                  (in retrieval from one or more parts,
!                   not from merged profiles)
!
! description   : The first half of the input array contains winds from
!                 TEMP, the second half winds from PILOT.  These winds
!                 are sorted here into order (of decreasing pressure if
!                 pressure is given, of increasing height otherwise).
!                 Duplicate winds are deleted, and some (but not all)
!                 missing winds.
!
! data type(s)  : UAWINDS
!
! called by     : UPRRET
!
! calls         : SORTR
!
! arguments     : (1) PROFILE  64 values in header & 14 values/level
!                              for TEMP & then PILOT, PILOT starting
!                              half way through PROFILE.
!                 (2) MXFINELMS  dimension of PROFILE (to get start
!                              of PILOT values)
!                 (3) MAXLEV   maximum number of levels in request
!
! REVISION INFO:
!
! $Workfile: uprwinds.f90$ $Folder: OpSource$
! $Revision: 2$ $Date: 17/11/2010 09:58:48$
!
! change record :
!
! $Log:
!  2    MetDB_Refresh 1.1         17/11/2010 09:58:48    John Norton
!       Updated after doing rework for batch 13.
!  1    MetDB_Refresh 1.0         09/11/2010 11:35:01    John Norton     MetDB
!       Refresh created file  
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

! Use statements:
! <Interfaces>

USE sortr_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:

INTEGER,      INTENT(IN)    ::  MXFINELMS
REAL,         INTENT(INOUT) ::  PROFILE(MXFINELMS)
INTEGER,      INTENT(IN)    ::  MAXLEV

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

INTEGER      ::  I,J,N
INTEGER      ::  MASK(14) ! to sort winds into pressure/height order
INTEGER      ::  MASX(14) ! to sort winds into standard/significant order
INTEGER      ::  NWINDT ! number of winds from TEMP (input)
INTEGER      ::  NWINDP ! number of winds from PILOT (input)
INTEGER      ::  NWINDS ! number of winds returned (after any
                        !  deletions, & not more than MAXLEV)

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

! Odd-numbered elements in PROFILE are Q/C (not set), the even elements
! for each level are: kind of level, P, ht, T, Td, dd, ff (subscripts
! 2, 4, 6, 8, 10, 12 & 14 respectively).  T & Td are not set as we're
! dealing with winds.  The masks below sort on elements 2, 4 & 6.

DATA MASX/0,-1, 0,-2, 0,3, 8*0/ ! sort on flags,P,ht in that order
DATA MASK/0,-3, 0,-1, 0,2, 8*0/ ! sort on P,ht,flags in that order

! Get wind counts from headers, PILOT from second half of PROFILE.

NWINDT=PROFILE(64)
NWINDP=PROFILE(MXFINELMS/2+64)
IF (NWINDT < 0) NWINDT=0     ! set missing count to zero
IF (NWINDP < 0) NWINDP=0
NWINDS=NWINDT+NWINDP         ! total number of winds

! Move PILOT winds up to follow TEMP winds, adding a PILOT flag in the
! unused temperature slot (8th in each set of 14 values per level).

DO I=1,NWINDP*14
  PROFILE(64+NWINDT*14+I)=PROFILE(MXFINELMS/2+64+I)
  IF (MOD(I,14) == 8) PROFILE(64+NWINDT*14+I)=256.
END DO

! Sort winds into order of decreasing pressure and increasing height,
! remembering that some values may be missing (-9999999.)
! Levels with pressure will come first, then levels without pressure,
! in height order.
!  Some levels may be duplicated in the PILOT data, so sort on a third
! field too, the kind of level, so that e.g. standard levels will come
! together & one is easily deleted.

CALL SORTR(PROFILE(65),14,NWINDS,MASK)

! Now delete unwanted winds.  If two levels have the same pressure
! (not missing) & wind, and the same kind-of-level figure (apart
! from the TEMP/PILOT flag), then delete the wind from the PILOT.
! (For a standard level the PILOT height may be from a table rather
! than measured.)
! If one of the winds (with the same pressure) is missing, delete it
! (maybe standard winds are in a PILOT & left missing in the TEMP...)
! - but DON'T in general delete missing winds, because two missing
! winds may delimit a part of the ascent where winds weren't measured
! & therefore interpolation between significant winds isn't valid.
! But missing tropopause & standard-level winds can safely be deleted.
!  (Zero kind of level to flag wind for deletion in later loop.)

DOLABEL1: &
DO I=1,NWINDS-1
  J=64+(I-1)*14
IFLABEL1: &
  IF (PROFILE(J+14+4) == PROFILE(J+4) .AND. &   ! if same pressure
      PROFILE(J+4) /= -9999999.) THEN           ! (& not missing)

IFLABEL2: &
    IF (PROFILE(J+14+12) == PROFILE(J+12) .AND. & ! if same dd
        PROFILE(J+14+14) == PROFILE(J+14)) THEN   ! & same ff,
! If flags same, delete whichever level is PILOT
      IF (PROFILE(J+14+2) == PROFILE(J+2)) THEN
        IF (PROFILE(J+8) == 256) PROFILE(J+2)=0 ! flag to delete
        IF (PROFILE(J+14+8) == 256) PROFILE(J+14+2)=0
      END IF

! If one of the two levels has no wind, delete it.
    ELSE IF (PROFILE(J+12) == -9999999. .AND. &
             PROFILE(J+14) == -9999999.) THEN
      PROFILE(J+2)=0
    ELSE IF (PROFILE(J+14+12) == -9999999. .AND. &
             PROFILE(J+14+14) == -9999999.) THEN
      PROFILE(J+14+2)=0
    END IF IFLABEL2
  END IF IFLABEL1

! If a tropopause or standard level has no wind, delete it.
  IF ((PROFILE(J+2) == 16. .OR. &        ! if tropopause
       PROFILE(J+2) == 32. .OR.        & ! or TEMP standard
       PROFILE(J+2) == 32.+256.) .AND. & ! or PILOT standard
      PROFILE(J+12) == -9999999. .AND. & ! & missing dd
      PROFILE(J+14) == -9999999.) THEN   ! & missing ff,
      PROFILE(J+2)=0                     ! flag to delete.
  END IF
END DO DOLABEL1

! Finally delete winds flagged for deletion in the loop above.
! Reset the count of levels (winds).

N=0
DO I=1,NWINDS
  IF (PROFILE(64+(I-1)*14+2) > 0) THEN   ! if not to be deleted
    N=N+1                                ! one more good level
    DO J=1,14
      PROFILE(64+(N-1)*14+J)=PROFILE(64+(I-1)*14+J)
    END DO
  END IF
END DO
NWINDS=N

! If there are too many levels for the user's array, and some winds are
! from PILOTs, sort on flags to put standard levels first, & take first
! MAXLEV levels from that array.
! (MASX sorts on flags in descending order - surface, standard, trop,
! max wind, significant - then pressure (descending) & height.)
! Then sort these MAXLEV levels back into pressure/height order.

IF (NWINDS > MAXLEV) THEN
  CALL SORTR(PROFILE(65),14,NWINDS,MASX)
  CALL SORTR(PROFILE(65),14,MAXLEV,MASK)
  NWINDS=MAXLEV
END IF

PROFILE(64)=NWINDS
RETURN
END SUBROUTINE UPRWINDS
