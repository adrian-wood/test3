SUBROUTINE MAPELM(USRELM,NUSR,IXELEM,IXNUM,NXELEM,&
     &IREF,NREF,IREPL,LTEST)

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
! parameters    (1)  USRELM array of user's element names           (i)
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
!Y2K  26.06.1997  MAPELM is Year 2000 compliant.
!
! revision info :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/mapelm.f,v $
!
! change record :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.1  2003/01/06 16:10:58  usmdb
! 20 Jan 2003    C Long
! 2.1  Rewrite with clearer structure, names & comments,
!      avoiding some problems with replications & entities
!      & speeding up a search by calling NMSTEM outside loop.
!
! Revision 2.0  2001/01/08  11:58:50  11:58:50  usmdb (Generic MDB account)
! Removed unused arguments CERR & IFAIL. Added copyright
! and modified header - S.Cox
!
! Revision 1.4  97/08/20  12:24:37  12:24:37  uspm (Pat McCormack)
! Fixes for TAF retrieval - marked !C
!
! Revision 1.3  1997/08/04 13:13:38  uspm
! First revisioned version for COSMOS - with Y2K change
!
! Revision 1.2  1997/02/27 12:11:21  uspm
! Latest version from COSMOS
!
! Revision 1.1  1997/02/11 16:23:03  uspm
! Initial revision
!
! 01-09-97 !C : S.Cox - Nested replications for TAF retrieval not
!             : working correctly following change !A. This change
!             : fixes it.
!
! 25-02-97 !B : S.Cox - Replications of elements within an entity name
!             : was not working correctly. Added a section to fix it.
!
! 27-09-96 !A : S.Cox - MAPELM can now handle replications of up to 9999
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2000 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE
LOGICAL LTEST

! Element names from request & corresponding numbers to be returned
! (NREF may be greater than NUSR - there may be entities...)

INTEGER       NUSR      ! number of elements requested
CHARACTER*(*) USRELM(*) ! element names (with "instances")

INTEGER       NREF      ! number of elements in IREF & IREPL
INTEGER       IREF(*)   ! index numbers for requested elements
INTEGER       IREPL(*)  ! instance (or 1 if not replicated)

! Index names & numbers (all elements for this data type)
! (The numbers refer to entries in a bit or array index used later.)

INTEGER       NXELEM    ! number of retrievable elements
CHARACTER*(*) IXELEM(*) ! element name
INTEGER       IXNUM(*)  ! element number

! Local variables

LOGICAL       FOUND     ! set when name is found in index
CHARACTER*32  USRSTM    ! name without instance(s) on end
CHARACTER*32  IXSTEM    ! same for name in index
CHARACTER*6   USRINST   ! number(s) on end of user's element name
CHARACTER*6   IXINST    ! same for name in index
INTEGER       INST      ! number from USRINST
INTEGER       NFOUND    ! number of elements in entity (or 1)
INTEGER       LM        ! subscript of required element
INTEGER       IX        ! subscript of current index line
INTEGER       I         ! loop variable
INTEGER       L         ! level
INTEGER       N(3)      ! instances at up to 3 levels

CHARACTER*132 HEAD
HEAD='&
&$Source: /home/us0400/mdb/op/lib/source/RCS/mapelm.f,v $&
&'//' $Revision: 1$ $Date: 26/01/2010 10:18:13$ '

! Loop over user's element names, splitting name into stem & instance.
! (Instance starts with underscore followed by figure.)

NREF=0
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

  IF (IXELEM(1).EQ.'ICAO_ID') THEN
    DO WHILE (IX.LT.NXELEM .AND. .NOT.FOUND)
      IX=IX+1                       ! next line in index
      CALL NMSTEM(IXELEM(IX),IXSTEM,IXINST)
      IF (USRSTM.EQ.IXSTEM) THEN
        IF (USRINST.EQ.IXINST) THEN
          FOUND=.TRUE.
        ELSE IF (IXINST.EQ.' ') THEN ! only one instance
          FOUND=.TRUE.
        ELSE IF (USRINST.EQ.' ') THEN ! match with first instance
          IF (IXINST.EQ.'_1'.OR.IXINST.EQ.'_1_1') FOUND=.TRUE.
        ENDIF
      ENDIF
    END DO

! Any other data type only checks stem against index name

  ELSE
    DO WHILE (IX.LT.NXELEM .AND. .NOT.FOUND)
      IX=IX+1                       ! next line in index
      IF (USRSTM.EQ.IXELEM(IX)) FOUND=.TRUE.
    ENDDO
  ENDIF

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

  IF (FOUND) THEN
    INST=1
    IF (USRINST.NE.' ') THEN
      INST=0
      L=1                           ! first level
      N(1)=0
      I=2                           ! start after first underscore

! Loop round characters in USRINST, either underscores or figures:
! build up count in N(L) if figure found, increment L if underscore.

      DO WHILE (I.LE.6 .AND. USRINST(I:I).NE.' ')
        IF (USRINST(I:I).GE.'0' .AND. USRINST(I:I).LE.'9') THEN
          N(L)=N(L)*10+ICHAR(USRINST(I:I))-ICHAR('0')
        ELSE IF (USRINST(I:I).EQ.'_') THEN
          INST=INST+N(L)*1000**(L-1)
          L=L+1                     ! next level
          N(L)=0
        ENDIF
        I=I+1
      ENDDO

! Space reached after last (or only!) figure: set final value of INST.
! (which is just N(1) if there's only one level)

      INST=INST+N(L)*1000**(L-1)
    ENDIF
IF (LTEST) PRINT *,'In MAPELM:',USRSTM,USRINST,INST,'requested'

! A line can now be put in the output table (several if it's an entity)

    NFOUND=1                        ! only one line unless entity
    IF (IXNUM(IX).LT.0) THEN        ! negative count if entity
      NFOUND=IABS(IXNUM(IX))        ! set count for loop below
      IX=IX+1                       ! past entity line itself
    ENDIF

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
  ENDIF
END DO                               ! next requested element
RETURN
END SUBROUTINE MAPELM
