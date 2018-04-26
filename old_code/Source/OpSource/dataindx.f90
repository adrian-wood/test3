SUBROUTINE DATAINDX (NUNIT, BULSEQ, NUMSEQ, NDSC, NUMDSC, &
                     CREQ, NFIRST, IDISP, NDISP, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  DATAINDX
!
!    To locate the user's requested data in an output array from the
!    BUFR decoder.
!
! DESCRIPTION:
!
!    DATAINDX performs some or all of the operations required to turn
!    a user's retrieval request string into an array of locations in
!    the output array from the BUFR decoder where the data can be
!    found.  The steps involved are listed below and numbered 1-5:
!    most are optional as it will often be the case that not all of
!    them will be need to be done at the same time.
!
!    Using a single subroutine such as DATAINDX to call several others
!    has two advantages, (1) many arrays (mainly output by READMAP)
!    can be treated as local arrays thereby considerably reducing the
!    size of the argument list for the calling routine, and (2) the
!    seven variables initailised in PARAMETER statements can all be
!    kept together. If any of the latter need increasing, all that is
!    needed is to change the PARAMETER statement and recompile.
!
!    1. Read element index
!
!       READMAP is called to read an element index on unit NUNIT. This
!       must be in the new format developed for element indexes 1n 2010.
!
!       This step is bypassed if NUNIT is zero or negative.
!
!    2. Decode user's retrieval request string
!
!       ELEMENTS is called to decode the required elements specified
!       in the user's request string CREQ. Details are put in a local
!       array LREQ for passing to RETPOS. The argument NFIRST should
!       be input as the first byte of CREQ after the word 'ELEMENTS'.
!
!       This step is bypassed if NFIRST is zero or negative in which
!       case CREQ is not used.
!
!    3. Match BUFR sequence in message with one in index
!
!       The BUFR sequence in a BUFR message is matched with one of the
!       sequences in the element index read in at step 1. (An error is
!       generated if step 1 has not been done.) The descriptor sequence
!       in the BUFR message (starting at byte 8 of BUFR section 3) is
!       supplied in BULSEQ and the number of descriptors it contains is
!       specified in NUMSEQ. If the sequence does not match any in the
!       element index, a message is output and an error code of 10 is
!       returned.
!
!       This step is bypassed if NUMSEQ is zero or negative in which
!       case BULSEQ is not used.
!
!    4. Check counts if there's delayed replication
!
!       If the BUFR sequence contains delayed replication, CHECKREPS is
!       called to find out what the counts are for these replications.
!
!       This step is only done if step 3 is also being done but is
!       bypassed if there is no delayed replication in which case NDSC
!       and NUMDSC are not used.
!
!    5. Compute displacements in BUFR decode output array
!
!       RETPOS is called to convert the array LREQ produced in step 2
!       into an array of displacements (IDISP) in the output array from
!       the BUFR decoder for the data items requested. The variable
!       NDISP should be input as the size of the IDISP array: on output
!       it will contain the number of values returned in IDISP. The
!       call to RETPOS is suppressed if a user's request list has not
!       been processed (step 2) and an error is generated if the BUFR
!       sequence has not been matched with one in an element index
!       (steps 1 & 3).
!
!       A special call to RETPOS to generate displacements for 16
!       selected elements (lat/long, date/time etc.) can be made by
!       specifying -1 as the input value of NDISP. In this case NDISP
!       remains unchanged on output.
!
!       This step is bypassed if the input value of NDISP is zero.
!
!    Step 1 needs to be called for each retrieval request and step 2
!    for each list of element names supplied by the user. Step 3 needs
!    to be done for each BUFR bulletin processed at which time DATAINDX
!    will also decide whether it is necessary to do step 4 as well.
!    Step 5 should be called for each retrieval of data from a given
!    bulletin.
!
!    If an error is detected, a non-zero return code is set (see list
!    below). Control is immediately returned to the calling program in
!    this case and also when a positive return code is returned from a
!    routine called by DATAINDX.
!
!    Error messages and non-zero error codes are generated for certain
!    errors as listed below.
!
!    Note on notation: Many arrays used by READMAP to hold information
!    from the element index have names beginning with N (e.g. NSVALS):
!    some of these begin NS or NR depending on whether their elements
!    refer to segments or replications respectively. Other arrays with
!    names beginning with M (or MS or MR) are similar but refer to the
!    BUFR message being processed and may need recalculating for other
!    messages (e.g. MSBFOR, MRVALS).
!
! USAGE:  CALL DATAINDX (NUNIT, BULSEQ, NUMSEQ, NDSC, NUMDSC, CREQ,
!                        NFIRST, IDISP, NDISP, ICODE)
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    NUNIT    I   I*4    Unit number for BUFR index ('0' if not used)
!    BULSEQ   I   C*(*)  BUFR sequence from section 4 of BUFR message
!    NUMSEQ   I   I*4    Number of BUFR descriptors in BULSEQ
!    NDSC     I   I*4    Array of descriptors from decoded BUFR message
!    NUMDSC   I   I*4    Number of descriptors in NDSC array
!    CREQ     I   C*(*)  User's MDB request string
!    NFIRST   I   I*4    Location of first byte in CREQ after 'ELEMENTS'
!    IDISP    O   I*4    Array of displacements for wanted data items
!    NDISP   I/O  I*4    Input:  Dimension of IDISP array
!                        Output: No. of elements of IDISP actually used
!    ICODE    O   I*4    Return code - see below for details
!
! RETURN CODES:
!
!      0  Requested processing done with no errors.
!     10  No matching BUFR sequence in index data set.
!    101  Checking sequence but no BUFR index yet read in.
!    102  Extracting data but no BUFR index yet read in.
!    103  Extracting data but no index number asigned.
!    109  Sequence has delayed replication with variable counts.
!
!    ICODE may also be set by routines called by DATAINDX.  Possible
!    values are 120-129 from READMAP, 140-149 from ELEMENTS, 161-169
!    from CKEKREPS and 181-189 from RETPOS. See documentation of these
!    subroutines for details.
!
! CALLS:  CHEKREPS, ELEMENTS, READMAP, RETPOS.
!
! REVISION INFO:
!
!    $Workfile: dataindx.f90$ $Folder: OpSource$
!    $Revision: 5$ $Date: 26/06/2012 16:51:10$
!
! CHANGE RECORD:
!
! $Log:
!  5    MetDB_Refresh 1.4         26/06/2012 16:51:10    Brian Barwell
!       Argument list changes in calls to RETPOS.
!  4    MetDB_Refresh 1.3         06/12/2010 16:55:49    Brian Barwell
!       Restructure several complicated IF statements.
!  3    MetDB_Refresh 1.2         25/10/2010 17:05:27    Brian Barwell   Made
!       arrays assumed-shape in arguments.
!  2    MetDB_Refresh 1.1         18/10/2010 15:18:22    John Norton
!       Updated after review by Brian.
!  1    MetDB_Refresh 1.0         13/10/2010 16:46:17    John Norton     Ported
!        f90 code
! $
!-----------------------------------------------------------------------
! (C) CROWN COPYRIGHT 2010 - MET OFFICE. All Rights Reserved.
!
! Met Office, United Kingdom.
!
! The use, duplication and disclosure of this code is strictly
! prohibited without the permission of The Meteorological Database
! Team at the above address.
!-----------------------------------------------------------------------
USE chekreps_mod
USE elements_mod
USE readmap_mod
USE retpos_mod

IMPLICIT NONE

! Subroutine arguments:

INTEGER, INTENT(IN)             ::  NUNIT ! Unit number for index data set
CHARACTER(*), INTENT(IN)        ::  BULSEQ ! Sequence from BUFR Section 3
INTEGER, INTENT(IN)             ::  NUMSEQ ! Number of descriptors in BULSEQ
INTEGER, INTENT(IN)             ::  NUMDSC ! Number of descriptors in NDSC array
INTEGER, INTENT(IN)             ::  NDSC(NUMDSC) ! Descriptor array from BUFR decode
CHARACTER(*), INTENT(IN)        ::  CREQ ! User's request string
INTEGER, INTENT(IN)             ::  NFIRST ! First character of CREQ to look at
INTEGER, INTENT(OUT)            ::  IDISP(:) ! Displacements from "RETPOS"
INTEGER, INTENT(INOUT)          ::  NDISP ! No. of values in IDISP array
INTEGER, INTENT(OUT)            ::  ICODE ! Return code

! Local declarations:

INTEGER      ::  LENSEQ  ! Length of string LISTSEQ
INTEGER      ::  MAXLEV  ! Maximum permitted nesting level
INTEGER      ::  MAXNAME ! Maximum permitted no. of element names
INTEGER      ::  MAXREP  ! Maximum permitted no. of replications
INTEGER      ::  MAXREQ  ! Dimension of LREQ array
INTEGER      ::  MAXSEG  ! Maximum permitted no. of segments
INTEGER      ::  MAXSEQ  ! Maximum permitted no. of BUFR sequences

PARAMETER (LENSEQ=2000, MAXREQ=500, MAXSEQ=10)
PARAMETER (MAXLEV=4, MAXNAME=400, MAXREP=30, MAXSEG=80)

INTEGER I              ! Integer variable for local use
INTEGER ILEV           ! Replication nesting level
INTEGER INDX           ! Index number
INTEGER IREP           ! Replication number
INTEGER ISEG           ! Segment number
INTEGER ISEQ           ! BUFR sequence number
INTEGER IVALS          ! Number of data values in segment
INTEGER LASTINDX       ! Index number used in previous call
INTEGER LEV            ! Loop variable for nesting level
INTEGER LNUM           ! Number of elements of LREQ used
INTEGER LREQ(MAXREQ)   ! Integer array describing request string
INTEGER LSTART(MAXSEQ) ! Sequence start locations in LISTSEQ
INTEGER MCOUNT(MAXREP) ! Replication counts for BUFR bulletin
INTEGER MRVALS(MAXREP) ! No. of data values in each replication
INTEGER MSBFOR(MAXSEG) ! No. of data values before each segment
INTEGER NAMES          ! Number of element names found
INTEGER NCOUNT(MAXREP,MAXSEQ) ! Replication counts for indexes
INTEGER ND             ! Number of data values passed so far
INTEGER NDELAY(MAXSEQ) ! Delayed replication indicator flags
                       ! (1 = yes, 0 = no, -1 = don't know yet)
INTEGER NDES(MAXSEQ)   ! Nos. of descriptors in BUFR sequences
INTEGER NDX(MAXSEQ)    ! Index number associated with each seq.
INTEGER NDXS           ! Number of indexes in index data set
INTEGER NREPS(MAXSEQ)  ! Number of replications for each index
INTEGER NREQ(16)       ! NAME subscripts for selected data items
INTEGER NRSEG1(MAXREP,MAXSEQ) ! First segment for each replication
INTEGER NRSEG2(MAXREP,MAXSEQ) ! Last segment for each replication
INTEGER NSEG(MAXNAME,MAXSEQ)  ! Segment numbers for elements
INTEGER NSEGS(MAXSEQ)  ! Number of segments for each index
INTEGER NSEQS          ! Number of sequences in index data set
INTEGER NSNEST(MAXSEG,MAXSEQ) ! Nest levels for each seg. & index
INTEGER NSVALS(MAXSEG,MAXSEQ) ! No. of vals. in each seg. & index
INTEGER NUMREP(MAXLEV,MAXSEG,MAXSEQ) ! Replication numbers
INTEGER NVAL(MAXNAME,MAXSEQ)  ! Segment locations for elements

LOGICAL      ::  NEWSEQ ! Flag for recalculation of seq. details
LOGICAL      ::  TEST   ! Flag for use with complicated IF tests

CHARACTER(LENSEQ) :: LISTSEQ ! String containing BUFR sequences
CHARACTER(36)     :: NAME(MAXNAME) ! Retrieval element names
CHARACTER         :: NTYPE(MAXNAME) ! Code for type of element

!-----------------------------------------------------------------------
!<executable statements ...>
!-----------------------------------------------------------------------

DATA LASTINDX /-1/, INDX /0/, LNUM /0/

SAVE

!=======================================================================
! STEP 1:  READ RETRIEVAL INFORMATION FROM NEW-FORMAT ELEMENT INDEX
!=======================================================================

IF (NUNIT > 0) THEN
  CALL READMAP (NUNIT, MAXSEQ, MAXSEG, MAXREP, MAXLEV, &
                MAXNAME, NDX, NDES, LISTSEQ, LSTART, NSEQS, NDXS, &
                NSEGS, NREPS, NSVALS, NSNEST, NUMREP, NCOUNT, &
                NAME, NTYPE, NREQ, NSEG, NVAL, NAMES, ICODE)
  IF (ICODE /= 0) RETURN
!                                    Set delayed replication indicators
  DO INDX=1,NDXS
    NDELAY(INDX) = -1  ! i.e. not yet determined
  END DO
  LASTINDX = 0         ! No index used from this data set
END IF

!=======================================================================
! STEP 2:  CONVERT USER'S REQUEST STRING TO INTEGER ARRAY "LREQ"
!=======================================================================

IF (NFIRST > 0) THEN
  LNUM = MAXREQ
  CALL ELEMENTS (CREQ, NFIRST, NAME, NAMES, LREQ, LNUM, ICODE)
  IF (ICODE /= 0) RETURN
END IF

!=======================================================================
! STEP 3:  FIND INDEX NUMBER CORRESPONDING TO BUFR SEQUENCE IN MESSAGE
!=======================================================================

IFLABEL1: &
IF (NUMSEQ > 0) THEN
  INDX = 0
!                                 Set ICODE if no data has been read in
  IF (LASTINDX < 0) THEN
    WRITE (6,'(T5,A,T15,A)') 'DATAINDX:', &
             'CHECKING SEQUENCE BUT NO BUFR INDEX YET READ IN'
      ICODE = 101
    RETURN
  END IF
!                            Check BUFR sequences and find index number
  ISEQ = 0
  DO WHILE (INDX == 0 .AND. ISEQ < NSEQS)
    ISEQ = ISEQ + 1
    IF (NDES(ISEQ) == NUMSEQ) THEN
      I = 2*NUMSEQ
      IF (LISTSEQ(LSTART(ISEQ):LSTART(ISEQ)+I-1)  ==  &
          BULSEQ(1:I)) INDX = NDX(ISEQ)
    END IF
  END DO
!                              Set ICODE if matching sequence not found
  IF (INDX == 0) THEN
    I = 2*MIN0(NUMSEQ,15)
    WRITE (6,'(T5,A,T15,A,2X,A)') 'DATAINDX:', &
             'NO MATCH FOR BUFR SEQUENCE:', BULSEQ(1:I)
    ICODE = 10
    RETURN
  END IF

!=======================================================================
! CHECK FOR DELAYED REPLICATION AND SET SEGMENT LIMITS FOR REPLICATIONS
!=======================================================================
!                                                 Check if already done
IFLABEL2: &
  IF (NDELAY(INDX) < 0) THEN   ! Not done
!                                                      Set delayed flag
    NDELAY(INDX) = 0
    DO IREP=1,NREPS(INDX)
      IF (NCOUNT(IREP,INDX) < 0) NDELAY(INDX) = 1
    END DO
!                               Compute segment ranges for replications
    DO ISEG=1,NSEGS(INDX)
      DO ILEV=1,NSNEST(ISEG,INDX)
        IREP = NUMREP(ILEV,ISEG,INDX)
        TEST = .TRUE.
        IF (ISEG > 1) TEST = IREP /= NUMREP(ILEV,ISEG-1,INDX)
        IF (TEST) NRSEG1(IREP,INDX) = ISEG
        TEST = .TRUE.
        IF (ISEG < NSEGS(INDX)) TEST = IREP /= NUMREP(ILEV,ISEG+1,INDX)
        IF (TEST) NRSEG2(IREP,INDX) = ISEG
      END DO
    END DO
  END IF IFLABEL2

!=======================================================================
! STEP 4:  CHECK REPLICATION COUNTS IF SEQUENCE HAS DELAYED REPLICATION
!=======================================================================
!                 Details need recalculating for new index and also for
!                 delayed replication since the counts may be different

  NEWSEQ = INDX /= LASTINDX .OR. NDELAY(INDX) > 0

  LASTINDX = 0  ! In case of premature RETURN

!                                          Copy replication count array
IFLABEL3: &
  IF (NEWSEQ) THEN
    DO IREP=1,NREPS(INDX)
      MCOUNT(IREP) = NCOUNT(IREP,INDX)
    END DO
!                         Fill in counts if delayed replication present

IFLABEL4: &
    IF (NDELAY(INDX) > 0) THEN   ! Delayed replication present

      CALL CHEKREPS (NDSC, NUMDSC, NSEGS(INDX), NRSEG1(1:,INDX), &
           NRSEG2(1:,INDX), NSNEST(1:,INDX), NUMREP(1:,1:,INDX), &
           MAXLEV, NCOUNT(1:,INDX), MCOUNT, NREPS(INDX), ICODE)
      IF (ICODE /= 0) RETURN

!                            Check for replication counts still missing

      DO IREP=1,NREPS(INDX)
        IF (MCOUNT(IREP) < 0) THEN
          WRITE (6,'(T5,A,T15,A,2X,A)') 'DATAINDX:', &
                   'DELAYED REPLICATION WITH VARIABLE COUNTS'
          ICODE = 109
          RETURN
        END IF
      END DO
    END IF IFLABEL4

!=======================================================================
!   COMPUTE ADDITIONAL INFORMATION IF NEW INDEX OR RECALCULATION NEEDED
!=======================================================================

    ND = 0
DOLABEL1: &
    DO ISEG=1,NSEGS(INDX)
      ILEV = NSNEST(ISEG,INDX)
!                                       Check for start of replications
      DO LEV=1,ILEV
        IREP = NUMREP(LEV,ISEG,INDX)
        IF (ISEG == NRSEG1(IREP,INDX)) MRVALS(IREP) = 0
      END DO
!                                           Update count of data values
      MSBFOR(ISEG) = ND
      IF (ND >= 0) ND = ND + NSVALS(ISEG,INDX)

IFLABEL2A: &
      IF (ILEV > 0) THEN
        IF (MRVALS(IREP) >= 0) &
            MRVALS(IREP) = MRVALS(IREP) + NSVALS(ISEG,INDX)

!                                         Check for end of replications
        IREP = NUMREP(ILEV,ISEG,INDX)
DOLABEL2: &
        DO LEV=ILEV,1,-1
IFLABEL5: &
          IF (ISEG == NRSEG2(IREP,INDX)) THEN

            IF (MRVALS(IREP) >= 0 .AND. MCOUNT(IREP) >= 0) THEN
              IVALS = MCOUNT(IREP)*MRVALS(IREP)
              IF (MSBFOR(NRSEG1(IREP,INDX)) >= 0) ND = &
                  MSBFOR(NRSEG1(IREP,INDX)) + IVALS
              ELSE
                IVALS = -1
              ND = -1
            END IF

            IF (LEV > 1) THEN
              IREP = NUMREP(LEV-1,ISEG,INDX)
              IF (IVALS >= 0 .AND. MRVALS(IREP) >= 0) THEN
                MRVALS(IREP) = MRVALS(IREP) + IVALS
              ELSE
                MRVALS(IREP) = -1
              END IF
            END IF
          END IF IFLABEL5
!                                                           Close loops
        END DO DOLABEL2   ! 'LEV' loop
      END IF IFLABEL2A
    END DO DOLABEL1     ! 'ISEG' loop
  END IF IFLABEL3
!                                       Save index number for next call
  LASTINDX = INDX
END IF IFLABEL1

!=======================================================================
! STEP 5:  COMPUTE DISPLACEMENTS IN DATA ARRAY FOR REQUIRED ELEMENTS
!=======================================================================

IFLABEL6: &
IF (NDISP /= 0) THEN
!                                 Set ICODE if no data has been read in
  IF (LASTINDX < 0) THEN
    WRITE (6,'(T5,A,T15,A)') 'DATAINDX:', &
             'EXTRACTING DATA BUT NO BUFR INDEX YET READ IN'
    ICODE = 102
    RETURN
!                                 Set ICODE if no index number assigned
  ELSE IF (INDX == 0) THEN
    WRITE (6,'(T5,A,T15,A)') 'DATAINDX:', &
             'EXTRACTING DATA BUT NO INDEX NUMBER ASSIGNED'
    ICODE = 103
    RETURN
  END IF
!              NDISP=-1: Special call for 16 elements specified by NREQ

  IF (NDISP == -1) THEN
    I = 16
    CALL RETPOS (NTYPE, NSEG(1:,INDX), NVAL(1:,INDX), NREQ, 16, &
         NSNEST(1:,INDX), NUMREP(1:,1:,INDX), NCOUNT(1:,INDX),  &    !5
         MSBFOR, MRVALS, MCOUNT, IDISP, I, ICODE)                    !5

!             NDISP>0: General call for LNUM elements specified by LREQ

  ELSE IF (LNUM > 0) THEN
    CALL RETPOS (NTYPE, NSEG(1:,INDX), NVAL(1:,INDX), LREQ, LNUM, &
         NSNEST(1:,INDX), NUMREP(1:,1:,INDX), NCOUNT(1:,INDX),    &  !5
         MSBFOR, MRVALS, MCOUNT, IDISP, NDISP, ICODE)                !5
  END IF
END IF IFLABEL6

RETURN
END SUBROUTINE DATAINDX
