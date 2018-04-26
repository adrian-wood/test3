SUBROUTINE READCF(INDX,TEXT,NCODES,N)

!-----------------------------------------------------------------------
!
! ROUTINE       : READCF (BUFR)
!
! PURPOSE       : to read CODEFIG dataset into character arrays INDX
!               : and TEXT
!
! DESCRIPTION   : Each description of a code figure (12 characters or
!                 less) is preceded by an 8-bit length, each set of
!                 code figures in a table by a count, mostly in 8 bits
!                 but in 16 bits if the table is sparsely filled.  At
!                 the start an index links a pointer to the descriptor
!                 concerned.  (Number of tables at start).
!
!                 This routine replaces NEWCODES and allows BUFR
!                 to work directly from the editable code/flag table
!                 rather than a machinable version.
!
! Limitations:
!  (Any change of structure must be reflected in CODE & CCCODE)
!
! Before adding new entries note that:
! - Descriptions longer than 12 bytes will be truncated.
!    (i.e. not kept in this machinable version - so make sure that
!     descriptions don't start with the same 12 characters!)
! - The descriptor in the title line must have one space before it.
!    (or two or more if unused code figures skipped - sparse table)!2.2
! - The descriptors on the title lines are essential, the titles
!    themselves & any blank lines between tables are not.
! - The sequence of code figures must start from zero & be increasing,
!    with no gaps unless the title line shows that there are no lines
!    for undefined figures, but can end with the last figure defined.
!      So don't type "reserved" (or "missing data") when adding a
!    table, ignore lines which say reserved from here to the end.
! - The sequence of bits in a flag table MUST start from one.
! - The title line of a flag table must contain '(n-BIT FLAG TABLE)',
!    where n is the number of bits in the Table B entry.
!
! CALLED BY     : CODE, CCCODE
!
! CALLS         : none
!
! ARGUMENTS     : INDX     O    Array of index blocks
!               : TEXT     O    Array of data
!               : NCODES   I/O  Size of INDX (bytes on input, 5-byte
!                               words on output)
!               : N        I/O  Size of TEXT (bytes on input & then
!                               on output)
!
! REVISION INFO :
!
! $Revision: 1$
! $Date: 26/01/2010 10:18:13$
! $Source: /home/us0400/mdb/op/lib/source/RCS/readcf.f,v $
!
! CHANGE RECORD :
!
! $Log:
!  1    Met_DB_Project 1.0         26/01/2010 10:18:13    Richard Weedon  Files
!        ported to F90 for MDB retrieval module build
! $
! Revision 2.2  2004/02/02 11:35:54  usmdb
! 16 Feb 2004     C Long
! 2.2  Remove wrong comments in header: indentation of code
!  figure
!      lines is irrelevant, only header line shows if spars
! e or not.
!
! Revision 2.1  2003/09/04  14:08:41  14:08:41  usmdb (MetDB account c/o usjh)
! 15 Sept 2003    C Long
! 2.1  Correct 2 silly mistakes in code to index backward references.
!
! Revision 2.0  2003/03/06  09:24:39  09:24:39  usmdb (MetDB account c/o usjh)
! *** empty log message ***
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

      IMPLICIT NONE

!-----------------------------------------------------------------------
! Declare variables
!-----------------------------------------------------------------------

INTEGER I         ! short-term loop variable
INTEGER IERROR    ! IOSTAT from IO statements
INTEGER ISEE      ! set if cross-reference found
INTEGER IX        ! displacement of text after KFIG
INTEGER IXX       ! XX in FXXYYY from index entry for search
INTEGER IY        ! YYY in FXXYYY from index entry for search
INTEGER J         ! short-term loop variable
INTEGER JF        ! displacemnet of F=0 on title line
INTEGER L         ! length of description
INTEGER LASTFIG   ! last code figure or bit number
INTEGER LASTL     ! length of last description
INTEGER LEXTRA    ! 2 if code figure explicit, 0 if not
INTEGER LX        ! XX in FXXYYY for backeard reference
INTEGER LY        ! YYY in FXXYYY for backward reference
INTEGER KFIG      ! code figure or bit number
INTEGER MAXNBL    ! size of input TEXT array
INTEGER MAXNXB    ! size of input INDX array
INTEGER N         ! pointer to character array TEXT
INTEGER NCODES    ! number of code tables (elements)
INTEGER NFIGS     ! number of code figures in table
INTEGER NSTART    ! subscript of slot for NFIGS
INTEGER WIDBITS   ! top 2 bits of X field in index entry
INTEGER X,Y       ! XX & YYY in FXXYYY for current descriptor

LOGICAL BACKREF   ! set if entry referred back to is found
LOGICAL ERROR     ! set if code figure sequence wrong
LOGICAL SPARSE    ! set if breaks in code figure sequence

CHARACTER*80  LINE     ! line read from CODEFIG
CHARACTER*132 HEAD     ! revision info
CHARACTER*80  HEADER   ! title line kept for error messages
CHARACTER*1   INDX(*)  ! array to read tables into
CHARACTER*1   TEXT(*)  ! array to read tables into

!-----------------------------------------------------------------------
! Initialise variables. Note: MAXNBL and MAXNXB are not used at present.
! They are there for future use to check that the INDX and TEXT arrays
! don't fill up.
!-----------------------------------------------------------------------

ERROR=.FALSE.
IERROR=0
ISEE=0
LASTL=0
MAXNBL=N        !- set to size of TEXT array
MAXNXB=NCODES   !- set to size of INDX array

N=1
NCODES=0

!-----------------------------------------------------------------------
! Revision info
!-----------------------------------------------------------------------

HEAD='$RCSfile: readcf.f,v $ ' //&
&'$Revision: 1$ $Date: 26/01/2010 10:18:13$'

!-----------------------------------------------------------------------
! Read in a line from the code/flag tables.
!-----------------------------------------------------------------------

DO WHILE (IERROR.EQ.0)
  READ (81,'(A80)',IOSTAT=IERROR) LINE

!-----------------------------------------------------------------------
! If the line starts with one or more spaces and then zero, and it's
! not an indented line for code figure zero (i.e. non-space after 0),
! then assume it's a title line, i.e. the zero is the first figure of
! an element descriptor.
!-----------------------------------------------------------------------

  JF=INDEX(LINE,' 0')
  IF (JF.GT.0 .AND. LINE(1:JF).EQ.' '&
             &.AND. LINE(JF+2:JF+2).NE.' ') THEN
    HEADER=LINE

!-----------------------------------------------------------------------
! The start of a new table marks the end of the last table, so store
! the number of code figures in the last table and make an index entry
! for the last table (descriptor & pointer, plus flags - see below)
! unless its header referred back to a table already defined.
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
! Each index entry covers 5 characters.  The fields are:
!    bits 1-2: width of count field (& code figure fields if sparse
!               (currently fixed at 1 if not sparse, 2 if sparse)
!    bits 3-8 & 9-16: X & Y (i.e element descriptor)
!    bit 17: flag set if table is sparse (code figures in TEXT)
!    bits 18-40: pointer to start (count) in TEXT
!-----------------------------------------------------------------------

    LASTFIG=-9999999     ! to restart figure sequence check
    IF (NCODES.GT.0 .AND. ISEE.EQ.0) THEN

!-----------------------------------------------------------------------
! If sparse, set 2-byte count & 1 in top 2 bits of entry to show that.
!-----------------------------------------------------------------------

      IF (SPARSE) THEN
        TEXT(NSTART)=CHAR(NFIGS/256)
        TEXT(NSTART+1)=CHAR(MOD(NFIGS,256))
        INDX(5*NCODES-4)=CHAR(64+X)
        INDX(5*NCODES-2)=CHAR(128+NSTART/65536)

!-----------------------------------------------------------------------
! If not sparse, set 1-byte count & 0 in top 2 bits of index entry.
!-----------------------------------------------------------------------

      ELSE
        TEXT(NSTART)=CHAR(NFIGS)
        INDX(5*NCODES-4)=CHAR(X)
        INDX(5*NCODES-2)=CHAR(NSTART/65536)
      ENDIF

!-----------------------------------------------------------------------
! The 2nd, 4th & 5th bytes of an entry involve no flags, so are
! set the same way for sparse as non-sparse.
!-----------------------------------------------------------------------

      INDX(5*NCODES-3)=CHAR(Y)
      INDX(5*NCODES-1)=CHAR(MOD(NSTART/256,256))
      INDX(5*NCODES)=CHAR(MOD(NSTART,256))
    ENDIF
    NCODES=NCODES+1

!-----------------------------------------------------------------------
! Convert descriptor for new table to X and Y.
!-----------------------------------------------------------------------

    READ (LINE(JF+2:JF+3),'(I2)',IOSTAT=IERROR) X
    READ (LINE(JF+4:JF+6),'(I3)',IOSTAT=IERROR) Y

!-----------------------------------------------------------------------
! If the title line continues with a reference to a previous table,
! "(SEE FXXYYY)", point the index entry to that copy of the table.
!-----------------------------------------------------------------------

    ISEE=INDEX(LINE,'(SEE')
    IF (ISEE.GT.0) THEN
      READ (LINE(ISEE+6:ISEE+7),'(I2)',IOSTAT=IERROR) LX
      READ (LINE(ISEE+8:ISEE+10),'(I3)',IOSTAT=IERROR) LY

!-----------------------------------------------------------------------
! Loop round the index entries already made, looking for the descriptor
! referred back to.  If it's found, construct the new index entry by
! combining the pointer and flags from the cross-referenced index
! entry with the new descriptor)
!-----------------------------------------------------------------------

      I=1
      BACKREF=.FALSE.
      DO WHILE (I.LT.NCODES .AND. .NOT.BACKREF)
        IXX=MOD(ICHAR(INDX(5*I-4)),64)
        IY=ICHAR(INDX(5*I-3))
        IF (IXX.EQ.LX .AND. IY.EQ.LY) THEN
          BACKREF=.TRUE.
          WIDBITS=ICHAR(INDX(5*I-4))-IXX
          INDX(5*NCODES-4)=CHAR(WIDBITS+X)                   !2.1
          INDX(5*NCODES-3)=CHAR(Y)                           !2.1
          INDX(5*NCODES-2)=INDX(5*I-2)
          INDX(5*NCODES-1)=INDX(5*I-1)
          INDX(5*NCODES)=INDX(5*I)
        ENDIF
        I=I+1
      END DO

!-----------------------------------------------------------------------
! If it's not found, print a warning & reset count of codes.
! (If a local entry is accepted internationally, the table will in
! general be in the same class (smae X, smaller Y), so backward
! reference from local to international works.  If the local table
! is in a different class, with X smaller, just leave it out of
! the machinable version.  Ignore warnings with such references.)
!-----------------------------------------------------------------------

      IF (.NOT.BACKREF) THEN
        NCODES=NCODES-1
        PRINT *,HEADER
        PRINT *,' - Table referred back to not found'
      ENDIF
    ELSE

!-----------------------------------------------------------------------
! If this is a flag table, store a null description for flag zero
! after the slot for the count (keeping its displacement to set the
! count at the end) to keep numbering consistent with code tables.
! (A flag table can't be sparse, so the count MUST be in one byte!)
! Increment N past count and length byte.
!-----------------------------------------------------------------------

      NSTART=N
      IF (INDEX(LINE,'FLAG TABLE').GT.0) THEN
        TEXT(N+1)=CHAR(1)          ! set null length of one
        NFIGS=1                    ! include null in count
        N=N+2                      ! past count slot & length
      ELSE
        NFIGS=0                    ! no code figures yet

!-----------------------------------------------------------------------
! If the descriptor on the title line starts further right, this
! is a sparsely populated table: give it 2 bytes for the count.
!-----------------------------------------------------------------------

        IF (LINE(2:2).EQ.' ') THEN
          SPARSE=.TRUE.
          N=N+2      ! past count slot (2 bytes)
        ELSE
          SPARSE=.FALSE.
          N=N+1      ! past count slot (one byte)
        ENDIF
      ENDIF
    ENDIF            ! end of code to handle header line

!-----------------------------------------------------------------------
! Ignore revision line
!-----------------------------------------------------------------------

  ELSE IF (INDEX(LINE,'$Revision:').GT.0) THEN
    CONTINUE

!-----------------------------------------------------------------------
! If this is not a header or revision line, and not all blank,
! then read the number at the start (delimited by a space) & find
! the start of the text that follows, skipping any further spaces.
! The first DO WHILE looks for a non-space followed by a space,
! the following check fails if the non-space is not a figure.
!-----------------------------------------------------------------------

  ELSE IF (LINE.NE.' ') THEN
    IX=1
    DO WHILE (IX.LT.LEN(LINE) .AND.&
      &.NOT. (LINE(IX:IX).NE.' ' .AND. LINE(IX+1:IX+1).EQ.' '))
      IX=IX+1
    ENDDO                            ! IX --> non-space + space

    IF (IX.LT.LEN(LINE) .AND.&
       &LINE(IX:IX).GE.'0' .AND. LINE(IX:IX).LE.'9') THEN
      READ (LINE(1:IX),*) KFIG
    ELSE
     PRINT *,HEADER
     PRINT *,LINE
     PRINT *,'Line does not start with valid code figure'
     PRINT *,'Previous code figure was',LASTFIG
     RETURN
    ENDIF

!-----------------------------------------------------------------------
! Point IX to the start of the description (to the first non-space
! unless there are 12 spaces, so no text for the machinable version)
!-----------------------------------------------------------------------

    IX=IX+2                          ! past non-space & space
    IF (LINE(IX:IX+11).NE.' ') THEN  ! if next 12 bytes not blank
      DO WHILE (LINE(IX:IX).EQ.' ')
        IX=IX+1
      END DO
    ENDIF

!-----------------------------------------------------------------------
! If the table is not sparse, check the code figures or flag numbers.
! Code figure numbering must start with zero & flags with one.
!-----------------------------------------------------------------------

    IF (.NOT.SPARSE) THEN
      IF (LASTFIG.LT.0) THEN
        IF (INDEX(HEADER,'FLAG TABLE').GT.0) THEN
          IF (KFIG.NE.1) THEN
            ERROR=.TRUE.
            PRINT *,HEADER
            PRINT *,' first bit in flag table must be 1'
          ENDIF
        ELSE
          IF (KFIG.NE.0) THEN
            ERROR=.TRUE.
            PRINT *,HEADER
            PRINT *,' first figure in code table must be 0'
          ENDIF
        ENDIF
      ENDIF
    ENDIF

!-----------------------------------------------------------------------
! The next figure must be one more than the last (if not sparse).
! or at least greater than the last (if sparse).
!-----------------------------------------------------------------------

    IF (LASTFIG.GE.0 .AND. IERROR.EQ.0 .AND.&
     &((.NOT.SPARSE .AND. KFIG.NE.LASTFIG+1) .OR.&
      &(SPARSE .AND. KFIG.LE.LASTFIG))) THEN
      ERROR=.TRUE.
      PRINT *,HEADER
      PRINT *,KFIG,'follows',LASTFIG
    ENDIF

    LASTFIG=KFIG

!-----------------------------------------------------------------------
! Make table entry for code figure or flag:
! work out length of text, store length & text, increment pointers
! (& store code figure itself if it's a sparse table)
!-----------------------------------------------------------------------

    L=12
    DO WHILE (L.GT.0 .AND. LINE(IX+L-1:IX+L-1).EQ.' ')
      L=L-1
    END DO

!-----------------------------------------------------------------------
! Store length of text (12 bytes max, less if spaces at end) plus
! 1 for length byte itself, then text (a character at a time).
!-----------------------------------------------------------------------

    IF (.NOT.SPARSE) THEN
      LEXTRA=0
      TEXT(N)=CHAR(1+L)           ! set one-byte count
    ELSE
      LEXTRA=2
      TEXT(N)=CHAR(1+LEXTRA+L)    ! set one-byte count
      TEXT(N+1)=CHAR(KFIG/256)
      TEXT(N+2)=CHAR(MOD(KFIG,256))
    ENDIF

    DO J=1,L
      TEXT(N+LEXTRA+J)=LINE(IX+J-1:IX+J-1)
    END DO

!-----------------------------------------------------------------------
! If two consecutive 12-character descriptions are the same (& not
! both blank), print a warning (but carry on - it may be unavoidable)
!-----------------------------------------------------------------------

    IF (L.GT.0 .AND. L.EQ.LASTL) THEN
      J=1
      DO WHILE (J.LE.L .AND.&
               &TEXT(N+LEXTRA+J).EQ.TEXT(N-(L+1)+J))
        J=J+1
      END DO

      IF (J.GT.L .AND. LINE(IX+1:IX+L).NE.' ') THEN
        PRINT *,HEADER
        PRINT *,LINE(1:IX+L-1),' same as ',(TEXT(J),J=N-L,N-1)
      ENDIF
    ENDIF
    LASTL=L

!-----------------------------------------------------------------------
! Increment the number of figures & the pointer
!-----------------------------------------------------------------------

    N=N+1+LEXTRA+L
    NFIGS=NFIGS+1
  ENDIF

END DO

!-----------------------------------------------------------------------
! If there are no more tables, store the number of codes in the last
! table (in 2 bytes for sparse table, 1 byte otherwise) and add an
! index entry for it (as above).
!-----------------------------------------------------------------------

IF (.NOT.ERROR) THEN
  IF (SPARSE) THEN
    TEXT(NSTART)=CHAR(NFIGS/256)
    TEXT(NSTART+1)=CHAR(MOD(NFIGS,256))

    INDX(5*NCODES-4)=CHAR(64+X)
    INDX(5*NCODES-2)=CHAR(128+NSTART/65536)
  ELSE
    TEXT(NSTART)=CHAR(NFIGS)

    INDX(5*NCODES-4)=CHAR(X)
    INDX(5*NCODES-2)=CHAR(NSTART/65536)
  ENDIF

  INDX(5*NCODES-3)=CHAR(Y)
  INDX(5*NCODES-1)=CHAR(MOD(NSTART/256,256))
  INDX(5*NCODES)=CHAR(MOD(NSTART,256))

!-----------------------------------------------------------------------
! Output count of tables, index & code figure descriptions.
!-----------------------------------------------------------------------

!       PRINT *,N,'is total length of data'
!       PRINT *,NCODES,'index entries, i.e. code tables'

ELSE
  PRINT *,' *** Could not read CODEFIG: see errors above ***'
ENDIF

RETURN
END SUBROUTINE READCF
