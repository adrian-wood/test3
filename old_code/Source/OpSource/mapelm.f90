SUBROUTINE MAPELM(USRELM,NUSR,IXELEM,IXNUM,NXELEM, &
                        IREF,NREF,IREPL,LTEST)

!-----------------------------------------------------------------------
!
! subroutine    : MAPELM in MDB
!
! purpose       : Find requested elements in index of retrievable
!                 elements & return numbers referring to array index
!                 or bit index & parallel array of replication counts.
!                    The inputs are a list of element names from
!                 the retrieval request (with numbers attached if
!                 replicated) and an index consisting of names &
!                 numbers.  The output is arrays of numbers &
!                 replication counts corresponding to the user's
!                 element list.
!
! called by     : MDB
!
! calls         : NMSTEM
!
! arguments     (1)  USRELM array of user's element names           (i)
!                     (names as in retrieval documentation, but
!                      maybe with figures on the end - "instances")
!               (2)  NUSR   number of user's elements               (i)
!               (3)  IXELEM array of index element names            (i)
!               (4)  IXNUM  array of index numbers                  (i)
!               (5)  NXELEM number of index elements                (i)
!               (6)  IREF   array of numbers for user's elements    (o)
!               (7)  NREF   number of numbers for user's elements   (o)
!               (8)  IREPL  array of user's replication counts      (o)
!               (9)  LTEST  true if diagnostics are to be printed   (i)
!
! REVISION INFO :
!
!
! $Workfile: mapelm.f90$ $Folder: OpSource$
! $Revision: 8$ $Date: 16/11/2010 14:07:18$
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         16/11/2010 14:07:18    Rosemary Lavery
!       updated
!  7    MetDB_Refresh 1.6         12/11/2010 17:13:17    Rosemary Lavery remove
!        old header
!  6    MetDB_Refresh 1.5         04/11/2010 15:41:00    Rosemary Lavery
!       Corrections after review 
!  5    MetDB_Refresh 1.4         04/11/2010 13:40:32    Rosemary Lavery
!       correction ahead of review
!  4    MetDB_Refresh 1.3         18/10/2010 16:22:49    Rosemary Lavery
!       Further amendments before review
!  3    MetDB_Refresh 1.2         18/10/2010 14:47:20    Rosemary Lavery Corr
!       to Integer declarations having variable dimensions
!  2    MetDB_Refresh 1.1         13/10/2010 13:33:29    Rosemary Lavery F90
!       amendments (before review)
!  1    MetDB_Refresh 1.0         12/10/2010 17:34:46    Rosemary Lavery
!       Initial F90 conversion
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

USE NMSTEM_MOD

IMPLICIT NONE

! Interface argument list

! Element names from request

INTEGER,           INTENT(IN)  :: NUSR       ! num of elements requested
CHARACTER (LEN=*), INTENT(IN)  :: USRELM(:)  ! element names with "instances"

! Index names & numbers (all elements for this data type)
! (The numbers refer to entries in a bit or array index used later.)

CHARACTER (LEN=*), INTENT(IN)  :: IXELEM(:)  ! element name
INTEGER,           INTENT(IN)  :: IXNUM(:)   ! element number
INTEGER,           INTENT(IN)  :: NXELEM     ! number of retrievable elements

! Numbers to be returned, corresponding to element names requested above
! (NREF may be greater than NUSR - there may be entities...)

INTEGER,           INTENT(OUT) :: IREF(:)    ! index numbers for requested elements
INTEGER,           INTENT(OUT) :: NREF       ! number of elements in IREF & IREPL
INTEGER,           INTENT(OUT) :: IREPL(:)   ! instance (or 1 if not replicated)

LOGICAL,           INTENT(IN)  :: LTEST      ! TRUE to print diagnostics

! Local variables

LOGICAL                        :: FOUND      ! set when name is found in index
CHARACTER (LEN=32)             :: USRSTM     ! name without instance(s) on end
CHARACTER (LEN=32)             :: IXSTEM     ! same for name in index
CHARACTER (LEN=6)              :: USRINST    ! number(s) on end of user's element name
CHARACTER (LEN=6)              :: IXINST     ! same for name in index
INTEGER                        :: INST       ! number from USRINST
INTEGER                        :: NFOUND     ! number of elements in entity (or 1)
INTEGER                        :: LM         ! subscript of required element
INTEGER                        :: IX         ! subscript of current index line
INTEGER                        :: I          ! loop variable
INTEGER                        :: L          ! level
INTEGER                        :: N(3)       ! instances at up to 3 levels

! Loop over user's element names, splitting name into stem & instance.
! (Instance starts with underscore followed by figure.)

NREF=0

DO_ELEMRQ: &
DO LM=1,NUSR
  CALL NMSTEM(USRELM(LM),USRSTM,USRINST)

! Look for element in index.  Only the stem needs to be found unless
! the data type is TAFs or METARs, but for these check the instance
! too.  (Recognise TAFs or METARs by ICAO_ID as the first element.)
! (If an element is replicated, but there's no replication in the
!  request, return the first instance.)

  FOUND=.FALSE.
  IX=0                              ! current line number

! Only TAFs & METARs need to check instance in index

IF_ICAO: &
  IF (IXELEM(1) == 'ICAO_ID') THEN
DO_SRCH: &
    DO WHILE (IX < NXELEM .AND. .NOT.FOUND)
      IX=IX+1                       ! next line in index
      CALL NMSTEM(IXELEM(IX),IXSTEM,IXINST)
      IF (USRSTM == IXSTEM) THEN
        IF (USRINST == IXINST) THEN
          FOUND=.TRUE.
        ELSE IF (IXINST == ' ') THEN ! only one instance
          FOUND=.TRUE.
        ELSE IF (USRINST == ' ') THEN ! match with first instance
          IF (IXINST == '_1'.OR.IXINST == '_1_1') FOUND=.TRUE.
        END IF
      END IF
    END DO DO_SRCH

! Any other data type only checks stem against index name

  ELSE
    DO WHILE (IX < NXELEM .AND. .NOT.FOUND)
      IX=IX+1                       ! next line in index
      IF (USRSTM == IXELEM(IX)) FOUND=.TRUE.
    END DO
  END IF IF_ICAO

! If an item has been found, put the following element lines in the
! output table if it's an entity; otherwise keep only the one line.

! But first generate a number to go into IREPL from the instance.

! If there's no underscore except at the start of the instance, it's a
! simple replication.  If there's >1 underscore, there's nesting: keep
! instances at up to 3 levels as (INST3*1000+INST2)*1000+INST1, where
! INSTn refers to an n-level segment number - although nesting may be
! handled by matching (see below) instances expanded out in the index.

! (N.B. If levels are separated by underscores in a *6 string, there
!  can't be more than 3 levels; if there's more than 1 level, no level
!  can have more than 3 figures; and if there's only one level but >3
!  figures, the message structure will show how to interpret INST.
!  (But of course the *6 constraint could be lifted if necessary.))

IF_MATCH: &
  IF (FOUND) THEN
    INST=1

IF_INST: &
    IF (USRINST /= ' ') THEN
      INST=0
      L=1                           ! first level
      N(1)=0
      I=2                           ! start after first underscore

! Loop round characters in USRINST, either underscores or figures:
! build up count in N(L) if figure found, increment L if underscore.

      DO WHILE (I <= 6 .AND. USRINST(I:I) /= ' ')
        IF (USRINST(I:I) >= '0' .AND. USRINST(I:I) <= '9') THEN
          N(L)=N(L)*10+ICHAR(USRINST(I:I))-ICHAR('0')
        ELSE IF (USRINST(I:I) == '_') THEN
          INST=INST+N(L)*1000**(L-1)
          L=L+1                     ! next level
          N(L)=0
        END IF
        I=I+1
      END DO

! Space reached after last (or only!) figure: set final value of INST.
! (which is just N(1) if there's only one level)

      INST=INST+N(L)*1000**(L-1)
    END IF IF_INST
    IF (LTEST) PRINT *,'In MAPELM:',USRSTM,USRINST,INST,'requested'

! A line can now be put in the output table (several if it's an entity)

    NFOUND=1                        ! only one line unless entity
    IF (IXNUM(IX) < 0) THEN         ! negative count if entity
      NFOUND=IABS(IXNUM(IX))        ! set count for loop below
      IX=IX+1                       ! past entity line itself
    END IF

    DO I=1,NFOUND
      NREF=NREF+1
      IREF(NREF)=IXNUM(IX+I-1)
      IREPL(NREF)=INST
      IF (LTEST) PRINT *,'MAPELM found',NREF,IREF(NREF),INST
    END DO

! If element not found, print message & set number to missing.

  ELSE
    PRINT *,'In MAPELM: element not found',USRELM(LM)
    NREF=NREF+1
    IREF(NREF)=-999
  END IF IF_MATCH
END DO DO_ELEMRQ                    ! next requested element
RETURN
END SUBROUTINE MAPELM
