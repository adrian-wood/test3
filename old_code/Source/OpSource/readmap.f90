SUBROUTINE READMAP (NUNIT, MAXSEQ, MAXSEG, MAXREP, MAXLEV, &
           MAXNAME, NDX, NDES, LISTSEQ, LSTART, NSEQS, NDXS, &
           NSEGS, NREPS, NSVALS, NSNEST, NUMREP, NCOUNT, &
           NAME, NTYPE, NREQ, NSEG, NVAL, NAMES, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  READMAP
!
!    To read a new-format element index and store the information.
!
! DESCRIPTION:
!
!    READMAP reads an element index (a data set which maps the data in
!    a BUFR message to the element names used to request specific data
!    items). The element index must be supplied on unit IUNIT and must
!    be in the new format developed in 2010.
!
!    Arguments 2-6 (MAXSEQ to MAXNAME) are set in PARAMETER statements
!    in the calling routine DATAINDX and govern the sizes of some of
!    the arrays used to store data.  Arguments 7-10 (NDX to LSTART)
!    hold the BUFR sequence details, arguments 13-18 (NSEGS to NCOUNT)
!    hold the segment details and replication counts for each index,
!    and arguments 19-24 hold the element names and details of their
!    locations in the BUFR sequences.
!
!    If an error is detected, a non-zero return code is set (see list
!    below) and control is immediately returned to the calling program.
!
!
! USAGE:  CALL READMAP (NUNIT, MAXSEQ, MAXSEG, MAXREP, MAXLEV, MAXNAME,
!              NDX, NDES, LISTSEQ, LSTART, NSEQS, NDXS, NSEGS, NREPS,
!              NSVALS, NSNEST, NUMREP, NCOUNT, NAME, NTYPE, NREQ, NSEG,
!              NVAL, NAMES, ICODE)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    NUNIT    I   I*4    Unit number for element index data set
!    MAXSEQ   I   I*4    Maximum allowed number of BUFR sequences
!    MAXSEG   I   I*4    Maximum allowed number of segments
!    MAXREP   I   I*4    Maximum allowed number of replications
!    MAXLEV   I   I*4    Maximum allowed replication nesting level
!    MAXNAME  I   I*4    Maximum allowed number of element names
!    NDX      O   I*4    Array of index numbers associated with BUFR
!                          sequences read from element index
!    NDES     0   I*4    Array of numbers of descriptors in BUFR seqs.
!    LISTSEQ  O   C*(*)  String holding BUFR sequences in binary form
!    LSTART   O   I*4    Array of locations in LISTSEQ of start of each
!                          BUFR sequence
!    NSEQS    O   I*4    Number of sequences in elements index data set
!    NDXS     O   I*4    Number of indexes in elements index data set
!    NSEGS    O   I*4    Array of numbers of segments in indexes
!    NREPS    O   I*4    Array of numbers of replications in indexes
!    NSVALS   O   I*4    (Array dimensioned (MAXSEG,*))  Numbers of
!                          data values for each segment and index
!    NSNEST   O   I*4    (Array dimensioned (MAXSEG,*))  Numbers of
!                          nesting levels for each segment and index
!    NUMREP   O   I*4    (Array dimensioned (MAXLEV,MAXSEG,*))
!                          Replication numbers for segments & indexes
!    NCOUNT   O   I*4    (Array dimensioned (MAXREP,*))  Replication
!                          counts for each replication and index
!    NAME     O   C*36   Array of element names
!    NTYPE    O   C*1    Code for element type ('C' = 'character)
!    NREQ     O   I*4    Array of NAME subscripts for 16 selected items
!    NSEG     O   I*4    (Array dimensioned (MAXNAME,*))  Segment
!                          numbers for each element name and index
!    NVAL     O   I*4    (Array dimensioned (MAXNAME,*)) Data locations
!                          in segment for each element name and index
!    NAMES    O   I*4    Number of element names in NAME array
!    ICODE    O   I*4    Return code - see below for details
!
! RETURN CODES:
!
!      0  Element index read and data stored with no errors.
!    121  BUFR sequence information not found in index data set.
!    122  Too many BUFR sequences in element index (i.e. > MAXSEQ).
!    123  String LISTSEQ too short to hold all BUFR sequences.
!    124  Too many indexes in element index data set (i.e. > MAXSEQ).
!    125  Unnumbered index in element index data set.
!    126  Too many element names in element index (i.e. > MAXNAME).
!
! CALLS:  READMAP does not call any other routines.
!
! REVISION INFO:
!
!    $Workfile: readmap.f90$ $Folder: OpSource$
!    $Revision: 5$ $Date: 25/06/2012 14:42:25$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         25/06/2012 14:42:25    Brian Barwell   Remove
!        code to read BUFR sequences in binary.
!  4    MetDB_Refresh 1.3         22/08/2011 15:56:49    Brian Barwell   Allow
!       for overflow lines in list of element names.
!  3    MetDB_Refresh 1.2         25/10/2010 17:07:00    Brian Barwell   Made
!       arrays assumed-shape in arguments.
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  NUNIT ! Unit number for input data set
INTEGER, INTENT(IN)             ::  MAXSEQ ! Maximum permitted no. of BUFR sequences
INTEGER, INTENT(IN)             ::  MAXSEG ! Maximum permitted no. of segments
INTEGER, INTENT(IN)             ::  MAXREP ! Maximum permitted no. of replications
INTEGER, INTENT(IN)             ::  MAXLEV ! Maximum permitted nesting level
INTEGER, INTENT(IN)             ::  MAXNAME ! Maximum permitted no. of element names
INTEGER, INTENT(OUT)            ::  NDX(:) ! Index number associated with each seq.
INTEGER, INTENT(OUT)            ::  NDES(:) ! No. of descriptors in BUFR sequences
CHARACTER(*), INTENT(OUT)       ::  LISTSEQ ! String containing BUFR sequences
INTEGER, INTENT(OUT)            ::  LSTART(:) ! Sequence start locations in LISTSEQ
INTEGER, INTENT(OUT)            ::  NSEQS ! Number of sequences in input data set
INTEGER, INTENT(OUT)            ::  NDXS ! Number of indexes in input data set
INTEGER, INTENT(OUT)            ::  NSEGS(:) ! Number of segments for each index
INTEGER, INTENT(OUT)            ::  NREPS(:) ! Number of replications for each index
INTEGER, INTENT(OUT)            ::  NSVALS(:,:) ! No. of values in each seg. & index
INTEGER, INTENT(OUT)            ::  NSNEST(:,:) ! Nesting levels for each seg. & index
INTEGER, INTENT(OUT)            ::  NUMREP(:,:,:) ! Replication numbers
INTEGER, INTENT(OUT)            ::  NCOUNT(:,:) ! Replication counts for rep. & index
CHARACTER(36), INTENT(OUT)      ::  NAME(:) ! Retrieval element names
CHARACTER, INTENT(OUT)          ::  NTYPE(:) ! Code for type of element
INTEGER, INTENT(OUT)            ::  NREQ(16) ! NAME subscripts for selected data items
INTEGER, INTENT(OUT)            ::  NSEG(:,:)! Segment numbers for each name & index
INTEGER, INTENT(OUT)            ::  NVAL(:,:)! Data location in seg. for name & index
INTEGER, INTENT(OUT)            ::  NAMES ! Number of element names found
INTEGER, INTENT(OUT)            ::  ICODE ! Return code

! Local declarations:

INTEGER      ::  I, J  ! Dogsbody variables for local use
INTEGER      ::  ILEV  ! Local variable for nesting level
INTEGER      ::  ILEVS ! Maximum nesting level for index
INTEGER      ::  INDX  ! Local variable for index number
INTEGER      ::  IOS   ! I/O status from READ statement
INTEGER      ::  IREP  ! Local variable for replication number
INTEGER      ::  ISEG  ! Local variable for segment number
INTEGER      ::  I1, I2 ! Locally uses pointers for LISTSEQ
INTEGER      ::  LENSEQ ! Length of string LISTSEQ
INTEGER      ::  NAMELEN ! Element name length in input
INTEGER      ::  NFXX  ! 'FXX' part of BUFR descriptor
INTEGER      ::  NYYY  ! 'YYY' part of BUFR descriptor

CHARACTER(2)  ::  CH2 ! 2-byte character variable for local use
CHARACTER(32) ::  FORMAT ! Format for reading element name lines
CHARACTER(2)  ::  IDREQ(16)! Character IDs for selected data items
CHARACTER(72) ::  TEXT ! Buffer to hold record of input data set

LOGICAL      ::  MORE  ! Flag to control loop termination

DATA IDREQ / 'LX', 'LY', 'T1', 'T2', 'T3', 'T4', 'T5', 'T6', &
             'I1', 'I2', 'I3', 'I4', 'C1', 'C2', '  ', '  ' /

ICODE = 0
LENSEQ = LEN(LISTSEQ)

!-----------------------------------------------------------------------
!                  READ BUFR SEQUENCE INFORMATION
!-----------------------------------------------------------------------
!                           Look for start of BUFR sequence information
IOS = 0
TEXT(1:4) = '    '
DO WHILE (TEXT(1:4) /= 'BUFR' .AND. IOS == 0)
  READ (NUNIT,'(A)',IOSTAT=IOS) TEXT(1:4)
END DO
!                                   Set return code to 121 if not found
IFLABEL1: &
IF (IOS /= 0) THEN
  WRITE (6,'(/T5,A,T15,A,I8)') 'READMAP:', &
           'BUFR SEQUENCES NOT FOUND IN INDEX DATA SET', IOS
  ICODE = 121
  RETURN
END IF IFLABEL1
!                                                       Initialisations
READ (NUNIT,'(1X)')   ! Skip blank line
I1 = 1                ! Start at byte 1 of LISTSEQ
NSEQS = 0             ! No sequences found yet
NDXS = 0              ! No indexes found yet
MORE = .TRUE.         ! To get loop started
!                                              Loop over BUFR sequences
DOLABEL1: &
DO WHILE (MORE)
  READ (NUNIT,'(A)') TEXT
IFLABEL2: &
  IF (TEXT(5:5) /= ' ') THEN ! Next sequence found
    NSEQS = NSEQS + 1
!                                          Set ICODE to 122 if too many
    IF (NSEQS > MAXSEQ) THEN
      WRITE (6,'(/T5,A,T15,A,I8)') 'READMAP:', &
               'TOO MANY BUFR SEQUENCES IN INDEX DATA SET', MAXSEQ
      ICODE = 122
      RETURN
    END IF
!                              Get index number & number of descriptors

    READ (TEXT,'(T2,2I4)') NDX(NSEQS), NDES(NSEQS)

!                               Check that sequence will fit in LISTSEQ
    LSTART(NSEQS) = I1
    I2 = I1 + 2*NDES(NSEQS) - 1
!                                     Set return code to 123 if no room
    IF (I2-1 > LENSEQ) THEN
      WRITE (6,'(/T5,A,T15,A,I8)') 'READMAP:', &
               'LISTSEQ TOO SHORT TO HOLD ALL SEQUENCES', LENSEQ
      ICODE = 123
      RETURN
    END IF
!                                               Read next BUFR sequence
    J = 12  ! Pointer for TEXT byte
!                                                 Loop over descriptors
DOLABEL2: &
    DO I=1,NDES(NSEQS)
!                                           Read next line if necessary
      IF (J > 66) THEN
        READ (NUNIT,'(A)') TEXT  ! Read another line
        J = 12
      END IF
!                                                     Decode descriptor
      READ (TEXT(J:J+6),'(1X,2I3)') NFXX, NYYY
      NFXX = NFXX - 36*(NFXX/100)
!                                             Add descriptor to LISTSEQ
      I2 = I1 + 1
      LISTSEQ(I1:I1) = CHAR(NFXX)
      LISTSEQ(I2:I2) = CHAR(NYYY)
!                                             Reset for next descriptor
      J = J + 7
      I1 = I2 + 1
    END DO DOLABEL2
!                                             Keep highest index number
    NDXS = MAX0(NDXS,NDX(NSEQS))
!                                       Increment pointer for next seq.
    I1 = I2 + 1
  ELSE              ! Blank line marks end of sequences
    MORE = .FALSE.
  END IF IFLABEL2
END DO DOLABEL1

!-----------------------------------------------------------------------
!                    READ DETAILS OF INDEXES
!-----------------------------------------------------------------------
!                                  Set ICODE to 124 if too many indexes
IF (NDXS > MAXSEQ) THEN
  WRITE (6,'(/T5,A,T15,A,I8)') 'READMAP:', &
           'TOO MANY INDEXES IN ELEMENT INDEX DATA SET', MAXSEQ
  ICODE = 124
  RETURN
END IF
!                                                     Loop over indexes
DOLABEL3: &
DO I=1,NDXS
!                                   Read header and decode index number
  READ (NUNIT,'(A)') TEXT
  IF (TEXT(1:5) == 'INDEX') THEN
    READ (TEXT,'(T6,I3)') INDX
  ELSE
!                            Set ICODE to 125 if index number not found
    ICODE = 125
    WRITE (6,'(/T5,A,T15,A)') 'READMAP:', &
             'UNNUMBERED INDEX IN ELEMENT INDEX DATA SET'
    RETURN
  END IF
!                  Read nos. of segments, replications & nesting levels

  READ (NUNIT,'(/T2,3I4/)') NSEGS(INDX), NREPS(INDX), ILEVS

!                                                  Read segment details
  DO ISEG=1,NSEGS(INDX)
    READ (NUNIT,'(T6,16I4)') NSVALS(ISEG,INDX), NSNEST(ISEG,INDX), &
         (NUMREP(ILEV,ISEG,INDX),ILEV=1,ILEVS)
  END DO
!                                        Read replication counts if any

  IF (NREPS(INDX) > 0) READ (NUNIT,'(/(T2,17I4))') &
       (NCOUNT(IREP,INDX),IREP=1,NREPS(INDX))
  READ (NUNIT,'(1X)')   ! Skip blank line
END DO DOLABEL3

!-----------------------------------------------------------------------
!              READ ELEMENT NAMES AND LOCATION DETAILS
!-----------------------------------------------------------------------
!                                       Read heading to get name length
READ (NUNIT,'(///A)') TEXT
NAMELEN = INDEX(TEXT,'>') - 1
!                                           Set format for reading data
J = (66-NAMELEN)/8
I = 2*J
WRITE (FORMAT,'(A,I2.2,A)') '(T2,A,2(1X,A),',I,'I4)'
!                                                       Initialisations
NAMES = 0         ! No names found yet
MORE = .TRUE.     ! To get loop started
DO I=1,16
  NREQ(I) = 0     ! No special subscripts yet
END DO
!                                               Loop over element names
DOLABEL4: &
DO WHILE (MORE)
  READ (NUNIT,'(A)') TEXT
IFLABEL4: &
  IF (TEXT(2:2) /= ' ') THEN ! Next name found
    NAMES = NAMES + 1
!                                    Set ICODE to 126 if too many names
    IF (NAMES > MAXNAME) THEN
      WRITE (6,'(/T5,A,T15,A,I8)') 'READMAP:', &
               'TOO MANY ELEMENT NAMES IN INDEX DATA SET', MAXNAME
      ICODE = 126
      RETURN
    END IF
!                                        Extract name and other details
    NAME(NAMES) = ' '
!                                              First line
    I = MIN0(J,NDXS)
    READ (TEXT,FORMAT) NAME(NAMES)(1:NAMELEN), NTYPE(NAMES), &
         CH2, (NSEG(NAMES,INDX), NVAL(NAMES,INDX), INDX=1,I)

!                                              Overflow lines (if any)
    IF (I.LT.NDXS) READ (NUNIT,'(T8,16I4)')  &
       (NSEG(NAMES,INDX), NVAL(NAMES,INDX), INDX=I+1,NDXS)

!                               Store NAME subscript for selected items
    IF (CH2 /= '  ') THEN
      I = 1
      DO WHILE (I <= 16 .AND. CH2 /= IDREQ(I))
        I = I + 1
      END DO
      IF (I <= 16) NREQ(I) = NAMES
    END IF

  ELSE              ! Blank line marks end of list of names
    MORE = .FALSE.
  END IF IFLABEL4
END DO DOLABEL4
!                                  Close BUFR index data set and RETURN
CLOSE (NUNIT)
RETURN
END SUBROUTINE READMAP
