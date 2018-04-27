SUBROUTINE CHEKREPS (NDSC, NUMDSC, NSEG, NRSEG1, NRSEG2, NSNEST, &
                     NUMREP, MAXLEV, NCOUNT, MCOUNT, NREPS, ICODE)

!-----------------------------------------------------------------------
!
! SUBROUTINE:  CHEKREPS
!
!    To determine the replication counts used for delayed replications
!    in a BUFR message.
!
! DESCRIPTION:
!
!    CHEKREPS is called for BUFR messages with delayed replications to
!    determine the actual counts for these replications.  On input the
!    counts for each replication are given by the NCOUNT array with -1
!    or -2 used to indicate delayed replication.  (-2 is used if the
!    descriptor for the replication count is 031000; -1 otherwise.)
!    The array of descriptors as output from a BUFR decode of the
!    message (NDSC) is used along with various pieces of information
!    from the appropriate element index to find the counts used by the
!    message. These values are put in the MCOUNT array which should be
!    a copy of NCOUNT on input but hopefully will have all the -1's
!    replaced by actual counts on output.
!
!    If there is nested delayed replication in the message and the
!    counts for the inner replication are not the same for all times
!    round the outer replication, an error message is generated as
!    software hasn't been written to cover this eventuality - yet.
!
!    If an error is detected, a non-zero return code is set (see list
!    below) and control is immediately returned to the calling program.
!
! USAGE:  CALL CHEKREPS (NDSC, NUMDSC, NSEG, NRSEG1, NRSEG2, NSNEST,
!                        NUMREP, MAXLEV, NCOUNT, MCOUNT, NREPS, ICODE)
!
! ARGUMENTS:
!
!    Name    I/O  Type      Description
!    ----    ---  ----      -----------
!    NDSC     I   I*4    Array of descriptors from BUFR decoder.
!    NUMDSC   I   I*4    Number of descriptors in NDSC array.
!    NSEG     I   I*4    Number of segments in BUFR sequence.
!    NRSEG1   I   I*4    Array of first segment nos. for replications.
!    NRSEG2   I   I*4    Array of last segment nos. for replications.
!    NSNEST   I   I*4    Array of nesting levels for segments.
!    NUMREP   I   I*4    (Array dimensioned (MAXLEV,:))  Replication
!                          numbers for each segment of BUFR message.
!    MAXLEV   I   I*4    First dimension of NUMREP array.
!    NCOUNT   I   I*4    Array of replication counts with '-1' used to
!                          indicate delayed replication.
!    MCOUNT  I/O  I*4    Input: Copy of NCOUNT.  Output: As input but
!                          delayed counts replaced by actual counts.
!    NREPS    I   I*4    Number of replications in BUFR sequence.
!    ICODE    O   I*4    Return code - see below for details.
!
! RETURN CODES:
!
!      0  Delayed replication counts determined with no errors.
!    161  Too many nesting levels in BUFR sequence.
!    162  Another replication descriptor expected but not found.
!    169  CHEKREPS cannot handle replication with variable count - yet.
!
! CALLS:  CHEKREPS does not call any other routines.
!
! REVISION INFO:
!
!    $Workfile: chekreps.f90$ $Folder: OpSource$
!    $Revision: 8$ $Date: 26/06/2012 16:47:52$
!
! CHANGE RECORD:
!
! $Log:
!  8    MetDB_Refresh 1.7         26/06/2012 16:47:52    Brian Barwell
!       Indicator for variable replication count changed to -3. -2 now used
!       for 031000 descriptors.
!  7    MetDB_Refresh 1.6         06/12/2010 15:51:37    Brian Barwell
!       Correct typing error in last version.
!  6    MetDB_Refresh 1.5         06/12/2010 15:45:10    Brian Barwell
!       Similar to previous change but for a different IF test.
!  5    MetDB_Refresh 1.4         06/12/2010 14:50:15    Brian Barwell
!       Restructure IF test for end of replication.
!  4    MetDB_Refresh 1.3         25/10/2010 17:03:59    Brian Barwell   Made
!       arrays assumed-shape in arguments.
!  3    MetDB_Refresh 1.2         25/10/2010 14:38:53    Brian Barwell
!       Correct spelling errors in argument list in comments.
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

INTEGER, INTENT(IN)          ::  NUMDSC         ! Number of descriptors in NDSC array
INTEGER, INTENT(IN)          ::  NDSC(NUMDSC)   ! Array of descriptors from BUFR decoder
INTEGER, INTENT(IN)          ::  NSEG           ! Number of segments
INTEGER, INTENT(IN)          ::  NRSEG1(:)      ! First segment of each replication
INTEGER, INTENT(IN)          ::  NRSEG2(:)      ! Last segment of each replication
INTEGER, INTENT(IN)          ::  NSNEST(:)      ! Nesting levels for segments
INTEGER, INTENT(IN)          ::  MAXLEV         ! First dimension of NUMREP array
INTEGER, INTENT(IN)          ::  NUMREP(:,:)    ! Replication numbers for levs. & segs.
INTEGER, INTENT(IN)          ::  NREPS          ! Number of replications in sequence
INTEGER, INTENT(IN)          ::  NCOUNT(NREPS)  ! Replication counts from BUFR sequence
INTEGER, INTENT(INOUT)       ::  MCOUNT(NREPS)  ! Replication counts for bulletin
INTEGER, INTENT(OUT)         ::  ICODE          ! Return code

! Local declarations:

INTEGER, PARAMETER ::  MAXNEST = 5 ! Maximum permitted nesting level (Should be at least MAXLEV)

INTEGER      ::  ICOUNT  ! Replication count from descriptor seq.
INTEGER      ::  IDSC    ! Pointer for NDSC array
INTEGER      ::  ILEV    ! Current nesting level
INTEGER      ::  IREP    ! Current replication number
INTEGER      ::  ISEG    ! Current segment number
INTEGER      ::  LEFT(MAXNEST) ! Remaining replications for each level
INTEGER      ::  LASTLEV ! Previous nesting level

LOGICAL      ::  ENDREP  ! Flag for end of replication
LOGICAL      ::  NEXTSEG ! Flag for mid- or no replication
LOGICAL      ::  SEARCH  ! Flag to control loop over descriptors

!=======================================================================
!                    LOOP OVER SEGMENTS/LEVELS
!=======================================================================
!                                                       Initialisations
IDSC = 1       ! Start of NDSC array
ISEG = 1       ! First segment
LASTLEV = 0    ! No last level
ILEV = 0       ! Start from nesting level 0
ICODE = 0      ! No erors yet

DOLABEL1: &
DO WHILE (ISEG <= NSEG)
!                          Check for end of replication (for (1) below)
  ENDREP = .FALSE.
  IF (ILEV > 0) ENDREP = LEFT(ILEV) == 0
!                                                Test used in (3) below
  NEXTSEG = ILEV == 0
  IF (.NOT.NEXTSEG) NEXTSEG = ISEG /= NRSEG2(IREP)

!-----------------------------------------------------------------------
!  (1)  AT END OF LAST TIME ROUND REPLICATION
!-----------------------------------------------------------------------

IFLABEL1: &
  IF (ENDREP) THEN
    LASTLEV = ILEV
    ILEV = ILEV - 1
    IF (ILEV > 0) IREP = NUMREP(ILEV,ISEG)

!-----------------------------------------------------------------------
!  (2)  AT START OF NEW REPLICATION
!-----------------------------------------------------------------------
!                                              Go to next nesting level

  ELSE IF (ILEV >= LASTLEV .AND. ILEV < NSNEST(ISEG)) THEN
    LASTLEV = ILEV
    ILEV = ILEV + 1
!                                   Set ICODE to 161 if too many levels
    IF (ILEV > MAXNEST) THEN
      WRITE (6,'(/T5,A,T15,A,I8)') 'CHEKREPS:', &
               'TOO MANY NESTING LEVELS IN BUFR SEQUENCE', MAXNEST
      ICODE = 161
      RETURN
    END IF
!                                            Get new replication number
    IREP = NUMREP(ILEV,ISEG)

!                     Search descriptors to find next replication count
!                                   (These have the form 16384 + count)
    SEARCH = .TRUE.
    DO WHILE (IDSC < NUMDSC .AND. SEARCH)
      IDSC = IDSC + 1
      IF (NDSC(IDSC) >= 16384 .AND. &
          NDSC(IDSC) < 32768) SEARCH = .FALSE.
    END DO
!                       Set ICODE to 162 if replication count not found
    IF (SEARCH) THEN                                                 !8
      WRITE (6,'(/T5,A,T15,A)') 'CHEKREPS:', &
               'NEXT REPLICATION DESCRIPTOR NOT FOUND'
      ICODE = 162
      RETURN
    END IF
!                                      For delayed replication, compare
!                                         count with those found so far
    IF (NCOUNT(IREP) < 0) THEN
      ICOUNT = NDSC(IDSC) - 16384
      IF (MCOUNT(IREP) == -1 .OR. MCOUNT(IREP) == -2) THEN !         !8
        MCOUNT(IREP) = ICOUNT
      ELSE IF (ICOUNT /= MCOUNT(IREP)) THEN  ! Different count
        MCOUNT(IREP) = -3                                            !8
      END IF
!                                 Otherwise use counts in BUFR sequence
    ELSE
      ICOUNT = NCOUNT(IREP)
    END IF
!                                     Skip replication if count is zero
    LEFT(ILEV) = ICOUNT
    IF (ICOUNT == 0) ISEG = NRSEG2(IREP)

  ELSE
    LASTLEV = ILEV

!-----------------------------------------------------------------------
!  (3)  NOT AT START OR END OF REPLICATION
!-----------------------------------------------------------------------

IFLABEL2: &
    IF (NEXTSEG) THEN
      ISEG = ISEG + 1

!-----------------------------------------------------------------------
!  (4)   AT END OF REPLICATION WITH MORE TO COME
!-----------------------------------------------------------------------
    ELSE
!                                Skip the replication if it is only one
!                               segment and there is no further nesting

      IF (ILEV == NSNEST(ISEG) .AND. ISEG == NRSEG1(IREP)) THEN
        LEFT(ILEV) = 0
      ELSE
!                 Decrement replications and go round again if any left

        LEFT(ILEV) = LEFT(ILEV) - 1
        IF (LEFT(ILEV) > 0) ISEG = NRSEG1(IREP)
      END IF
    END IF IFLABEL2
  END IF IFLABEL1
END DO DOLABEL1

!=======================================================================
!             RESET FLAGS FOR VARIABLE REPLICATION COUNTS
!=======================================================================

DO IREP=1,NREPS
  IF (MCOUNT(IREP) == -3) THEN      ! Counts not all the same        !8
    MCOUNT(IREP) = NCOUNT(IREP)                                      !8
    WRITE (6,'(/T5,A,T15,A)') 'CHEKREPS:', &
             'CANNOT HANDLE REPLICATION WITH VARIABLE COUNT'
    ICODE = 169
    RETURN
  ELSE IF (MCOUNT(IREP) < 0) THEN   ! Replication not used (e.g. if  !8
    MCOUNT(IREP) = 0                ! outer replication had count 0)
  END IF
END DO

RETURN
END SUBROUTINE CHEKREPS
