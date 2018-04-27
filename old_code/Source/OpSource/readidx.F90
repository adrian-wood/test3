SUBROUTINE READIDX(SubType,CINDX,Xtype,Match)

!-----------------------------------------------------------------------
! ROUTINE       : READIDX
!
! PURPOSE       : to read the element index for a particular MetDB
!               : subtype from the element index dataset
!
! CALLED BY     : BUFRET, BUSRET, SSMRET, SYNRET, UPRRET,
!                 SYNSEQ, SHPSEQ, MDBBD (BUFR retrieval on HP)     !2.3
!
! CALLS         : RTABLE (to get DSN of element index),INQUIRE
!
! ARGUMENTS     : SubType  (ip) - char*8 member name in element index
!               : CINDX    (io) - string(s) holding element index(es)
!                                  (one string per sequence)
!               : Xtype    (io) - type of index 'A' or 'B'
!               : Match    (io) - TRUE if element index matched
!
! REVISION INFO :
!
! $Revision: 8$
! $Date: 08/04/2011 12:05:10$
! $Source: /data/us0400/mdb/op/lib/source/RCS/readidx.F,v $
!
! CHANGE RECORD :
!
! $Log:
!  8    MetDB_Refresh 1.7         08/04/2011 12:05:10    Alison Weir     Amend
!       intents
!  7    MetDB_Refresh 1.6         22/03/2011 15:13:37    Brian Barwell
!       ACTION='READ' added to OPEN statement.
!  6    MetDB_Refresh 1.5         22/03/2011 14:59:42    Brian Barwell   Ignore
!        - check in failed to pick up new version.
!  5    MetDB_Refresh 1.4         31/01/2011 15:04:40    Sheila Needham
!       Updated OPEN stmt
!  4    MetDB_Refresh 1.3         24/11/2010 12:22:38    Sheila Needham  change
!        CINDX array declaration to ':' to agree with interface
!  3    MetDB_Refresh 1.2         19/11/2010 10:18:04    John Norton
!       Changed inquire_more to inquire_mod
!  2    MetDB_Refresh 1.1         18/11/2010 11:56:41    Sheila Needham  Change
!        OPEN and INQUIRE
!  1    MetDB_Refresh 1.0         01/11/2010 15:48:51    John Norton     MetDB
!       Refresh batch 4.  Files for review
! $
! Revision 2.3  2003/08/05  10:13:01  10:13:01  usmdb (MetDB account c/o usjh)
! 18 Aug 2003     C Long
! 2.3  Only read element index if it's not the last one read in.
!
! Revision 2.2  2003/05/06  08:10:49  08:10:49  usmdb (MetDB account c/o usjh)
! 19 May 2003     C Long
! 2.2  Rewrite incorporating ELIDXNEW to eliminate "machinable" element index.
!      First argument is now member name, not necessarily same as data type.
!
! Revision 2.1  2003/02/03 15:20:48  usmdb
! It is now possible to pass the location of the element index
! in the environment variable METDB_ELEMIDX - S.Cox
!
! Revision 2.0  2001/01/08  11:59:07  11:59:07  usmdb
! Added defaults to the preprocessor statement. Removed
! READONLY from the OPEN statement as this is non-standard
! in f90/f95. Added copyright and modified header - S.Cox
!
! Revision 1.6  98/11/12  11:44:54  11:44:54  usmdb
! 16-11-98 S.Cox
! New element index format - large changes to READIDX to read new format
! element index and return an array of indexes for the subtype rather than
! a single character string.
!
! Revision 1.5  98/01/29  17:13:09  17:13:09  usmdb
! Addition of IBM preprocess directive.
!
! Revision 1.4  97/08/06  08:43:21  08:43:21  uspm
! Ensure characters do not go past column 72
!
! Revision 1.3  1997/08/04 13:25:13  uspm
! First revisioned version for 1  - with Y2K change
!
! Revision 1.2  1997/04/07 12:24:00  uspm
! Insert 'extra' $ after $Source:  to ensure following keyword
! is found by ident in .o, lib
!
! Revision 1.1  1997/02/27 12:13:56  uspm
! Initial revision
!
! Revision 1.4  1997/02/18 14:27:41  uspm
! Correct setting of variable head
!
! Revision 1.3  1997/02/18 14:24:41  uspm
! Correct version of in-line retrieval - version
! 1.2 can be ignored as a duplicate of version 1.1
!
! Revision 1.1  1997/02/12 08:52:26  uspm
! Initial revision
!
! 07-02-97   !B  : S.Cox - Correct WHILE loop. Now NumSubTypes >=
!                : (RecNum-1) rather than NumSubTypes > (RecNum-1)
!
! 12-12-96       : S.Cox - re-write. The element index is now read from
!                : an element index dataset rather than from a storage
!                : dataset. The subtype determines which element to read
!
! 23-07-96   !A  : S.M.Needham - change the argument list so that just
!                : the index type and a character string of the last
!                : record are returned.  Leave setting up the index
!                : arrays until the message type is determined later on.
!
! 18-06-96       : S.Cox - written.
!
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2003 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

! Use statements:
! <Interfaces>

USE rtable_mod
USE inquire_mod

! <Data Modules>

IMPLICIT NONE

! Subroutine arguments:
!<arguments with INTENT(IN) ...>

CHARACTER(8), INTENT(IN)    ::  SubType ! MetDB subtype
CHARACTER(*), INTENT(INOUT) ::  CINDX(:) ! element indexes
CHARACTER(1), INTENT(INOUT) ::  XTYPE ! type of index
LOGICAL,      INTENT(INOUT) ::  MATCH

! Subroutine result:
!<declare the type returned by the Subroutine>
! Local declarations:
!<parameters, derived data types, variables, ...>

LOGICAL      ::  HEADSET = .FALSE.  ! TRUE if HEAD set
LOGICAL      ::  LOCALIDX      ! TRUE if local index library
LOGICAL      ::  STORAGE       ! true if in storage job
LOGICAL      ::  X             ! dummy for DATASETS read

CHARACTER(8)   ::  CDUM        ! dummy argument for RTABLE
CHARACTER(1)   ::  CERR        ! dummy argument for RTABLE
CHARACTER(8)   ::  ELEMIDX     ! Code name for index library
CHARACTER(120) ::  ELEMLIB     ! Library of element index
CHARACTER(120) ::  FILENAME    ! File to open
CHARACTER(40)  ::  FORMAT      ! format for reading DATASETS
CHARACTER(120) ::  LASTFILE    ! File last opened            !2.3
CHARACTER(8)   ::  LASTIDX     ! Code name for last library
CHARACTER(33)  ::  NAME        ! to skip element names
CHARACTER(1)   ::  NESTED      ! 'N' if nested, blank if not
CHARACTER(8)   ::  TYPE        ! data type from DATASETS

INTEGER      ::  I,J,M         ! general loop counters
INTEGER      ::  IDATA(5)      ! dummy argument for RTABLE
INTEGER      ::  IRC           ! return code from OPEN
INTEGER      ::  ITIME(9)      ! dummy argument for RTABLE
INTEGER      ::  K             ! pointer in string
INTEGER      ::  LREC          ! dummy for DATASETS read
INTEGER      ::  LELEMLIB      ! dummy for DATASETS read
INTEGER      ::  LsubType      ! Length of SubType
INTEGER      ::  MS(4)         ! message structure numbers
INTEGER      ::  NMS           ! number of numbers in MS
INTEGER      ::  NSEGS         ! number of segments
INTEGER      ::  NELMS         ! number of lines in index
INTEGER      ::  NUMTAB        ! number of indexes for subtype
INTEGER      ::  TIRM          ! argument 2 for call to rtable

DATA LASTIDX/' '/,LASTFILE/' '/            !2.3

! INTERFACE blocks
!<INCLUDE interface files...>
!<other interface blocks...>
!<other specification statements ...>
!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

SAVE

IFLABEL1: &
IF (.NOT.HEADSET) THEN                                        !2.1
  HEADSET=.TRUE.                                              !2.1

! Get the DSN of ELEMIDX so that a member name can be added.
! When called in storage jobs (for sequence check retrievals)
! READIDX must get the DSN of ELEMIDX as in DSINFO rather than
! from RTABLE.  IF DATASETS is allocated, this is a storage job:
! read DATASETS as in DSINFO till ELEMIDX is found.

#if defined (MVS)
  STORAGE=INQUIRE('DATASETS','DDN')
  IF (STORAGE) THEN
    OPEN (81,FILE='DD:DATASETS',ACTION='READ')
    READ (81,'(//A40////)') FORMAT
    DO WHILE (TYPE /= 'ELEMIDX')
      READ (81,FORMAT) TYPE,X,X,LREC,LELEMLIB,ELEMLIB
    END DO
    CLOSE (81)
  END IF
#else
  STORAGE=.FALSE.
#endif
END IF IFLABEL1                                               !2.1

! Open the appropriate index library if it is different from the
! one already open.

ELEMIDX = 'ELEMIDX '
LOCALIDX = SUBTYPE(1:1) >= '0' .AND. SUBTYPE(1:1) <= '9'
IF (LOCALIDX) ELEMIDX(8:8) = SUBTYPE(1:1)

IFLABEL2: &
IF (ELEMIDX /= LASTIDX .AND. .NOT.STORAGE) THEN
  CDUM=' '
  TIRM=0
  CALL RTABLE(ELEMIDX,TIRM,0,CDUM,.FALSE.,ITIME, &
              ELEMLIB,IDATA,CDUM,IRC,CERR,CDUM,CDUM)
  LELEMLIB=IDATA(1)
  MATCH = IRC <= 8
  IF (.NOT.MATCH) THEN  ! Entry not found
    PRINT *,'Element index library not found in READIDX: ', &
            ELEMIDX
    RETURN
  END IF
  LASTIDX = ELEMIDX
END IF IFLABEL2

! Add a member name to the ELEMIDX DSN found above.
! (N.B. The member name is often not the same as the data type!)

LSubType=INDEX(SubType,' ')-1
IF (LSubType < 0) LSubType = LEN(SubType)

#if defined (MVS)
IF (LOCALIDX) THEN
  FILENAME= "//'" // ELEMLIB(1:LELEMLIB) //                       &
            '(' // SubType(2:LSubType) // ")'"
ELSE
  FILENAME= "//'" // ELEMLIB(1:LELEMLIB) //                       &
            '(' // SubType(1:LSubType) // ")'"
ENDIF
#else
IF (LOCALIDX) THEN
  FILENAME = ELEMLIB(1:LELEMLIB) // '/' // SubType(2:LSubtype)
ELSE
  FILENAME = ELEMLIB(1:LELEMLIB) // '/' // SubType(1:LSubtype)
END IF
#endif

! Only read in the index if it wasn't the last one read.           !2.3

IFLABEL3: &
IF (FILENAME /= LASTFILE) THEN                               !2.3
#if defined (MVS)
  OPEN (UNIT=81,FILE=FILENAME,ACTION='READ',IOSTAT=IRC)
#else
  OPEN (UNIT=81,FILE=FILENAME,IOSTAT=IRC)
#endif
  MATCH = IRC == 0
  IF (.NOT.MATCH) THEN
    PRINT *,SubType,'element index not found in READIDX: ', &
          FILENAME
    RETURN
  END IF

! Read 1st 2 lines; get index type; clear CINDX & keep no. of sequences.

  READ (81,'(2A1)') XTYPE,NESTED ! index type (Array/Bit) & flag
  READ (81,'(I2)') NUMTAB        ! Number of index tables
  DO I=1,NUMTAB
    CINDX(I) = ' '
  END DO
  WRITE (CINDX(1)(1:2),'(I2.2)') NUMTAB

! Loop round the indexes for different sequences, putting each in
! a CINDX string.
! The layout of each CINDX string is:
!  1-2   number of sequences (strings) for data type (1st string only)
!  3-8   sequence descriptor
!  9-12  number of *36 element lines to follow (NELMS)
! 13-48 etc  *36 element lines
! & message structure (number of segments and 2, 3 or 4 numbers
!                      per segment for unformatted read)

DOLABEL1: &
  DO I=1,NUMTAB
    READ (81,'(A6)') CINDX(I)(3:8) ! Sequence descriptor

! Read the element lines in the bit index, skipping the names,
! keeping the numbers as a 36-character substring.
! The assumption is that the index for each sequence will fit
! into a single string, so stop if too many elements.

    NAME=' '
    NELMS=0                      ! To count element lines
    K=12                         ! Point past header
DOLABEL2: &
    DO WHILE (NAME /= 'MESTRUCT_FOLLOWS')
      READ (81,'(A33,A36)') NAME,CINDX(I)(K+1:K+36)
      IF (NAME /= 'MESTRUCT_FOLLOWS') THEN
        NELMS=NELMS+1            ! One more element
        K=K+36                   ! Point past element line
      END IF

      IF (NELMS >= LEN(CINDX(1))/36) THEN
        PRINT *,NELMS,'elements in index - too many!'
        MATCH=.FALSE.
        RETURN
      END IF
    END DO DOLABEL2

    WRITE (CINDX(I)(9:12),'(I4)') NELMS

! Read in message structure (number of segments, then 2, 3 or 4
! numbers per segment, decided by type of index: see NMS)
! Write numbers out separated by commas for * READ, ending with '/'.

    IF (XTYPE == 'A') NMS=3
    IF (XTYPE == 'B') NMS=2
    IF (NESTED == 'N') NMS=NMS+1

    READ (81,*) NSEGS
    WRITE (CINDX(I)(K+1:K+2),'(I2)') NSEGS

    DO J=1,NSEGS
      READ (81,*) (MS(M),M=1,NMS)
      WRITE (CINDX(I)(K+3:K+22),'(4('','',I4))') (MS(M),M=1,NMS)
      K=K+5*NMS
    END DO
    CINDX(I)(K+3:K+3)='/'      ! mark end of numbers (for * read)

    READ (81,*)                ! skip 'index_END' before next seq
  END DO DOLABEL1              ! end of loop round indexes

  CLOSE (81)
  LASTFILE=FILENAME                                          !2.3
END IF IFLABEL3                                             !2.3
RETURN
END SUBROUTINE READIDX
